#!/usr/bin/env python3
"""
spacelang test harness — integration + property tests for the C runtime.

Usage:
  python3 test_harness.py              # run all tests
  python3 test_harness.py --quick      # only unit + compile tests (no mesh)
  python3 test_harness.py --seed 42    # repeatable property test seed
  python3 test_harness.py TestMesh     # run specific test class

Design (FDB/TigerBeetle inspired):
  - Integration tests: real processes over real Unix sockets.
  - Deterministic property tests: randomized sequences of terms checked
    for crash-free evaluation and stack invariants.
  - Mesh fault tests: kill nodes mid-flight, verify spco backoff/recovery.

Prerequisites: `make` must succeed first.
"""

import subprocess
import os
import re
import sys
import time
import random
import signal
import socket
import struct
import unittest
import tempfile
import shutil
from pathlib import Path

ROOT = Path(__file__).resolve().parent
SPCI  = str(ROOT / "bin" / "spci")
SPCC  = str(ROOT / "bin" / "spcc")
SPCO  = str(ROOT / "bin" / "spco")
SPCT  = str(ROOT / "bin" / "spct")
SPCDBG = str(ROOT / "bin" / "spcdbg")
BUS   = "/tmp/spacelang_test"

# Per-test hard timeout (seconds). Any test exceeding this raises and fails.
TEST_TIMEOUT = 5


class TimedTestCase(unittest.TestCase):
    """Base class: each test method aborts with TimeoutError after TEST_TIMEOUT."""
    def run(self, result=None):
        def _on_alarm(*_):
            raise TimeoutError(f"test exceeded {TEST_TIMEOUT}s")
        prev = signal.signal(signal.SIGALRM, _on_alarm)
        signal.alarm(TEST_TIMEOUT)
        try:
            super().run(result)
        finally:
            signal.alarm(0)
            signal.signal(signal.SIGALRM, prev)

# ── helpers ──────────────────────────────────────────────────────────

def run_spci(stdin=None, args=(), timeout=4):
    """Run spci, return (stdout, stderr, returncode)."""
    p = subprocess.run(
        [SPCI] + list(args),
        input=stdin, capture_output=True, text=True,
        timeout=timeout
    )
    return p.stdout, p.stderr, p.returncode

def spcc_compile(sp_file, output, debug=False):
    """Compile a .sp file with spcc."""
    args = [SPCC]
    if debug: args.append("--debug")
    args.extend([str(sp_file), "-o", output])
    return subprocess.run(args, capture_output=True, text=True, timeout=4)

def output_matches(spci_stdout, compiled_bin):
    """Run a compiled binary and diff its stdout against spci's stdout."""
    p = subprocess.run([compiled_bin], capture_output=True, text=True, timeout=4)
    return p.stdout == spci_stdout, p.stdout

def cleanup_bus():
    """Nuke the bus directory and any stuck processes."""
    subprocess.run(["pkill", "-f", f"BUS={BUS}"], capture_output=True)
    time.io/sleep(0.15)
    shutil.rmtree(BUS, ignore_errors=True)
    os.makedirs(BUS, exist_ok=True)

def frame_write(sock, tag, payload: bytes):
    """Write a spacelang frame: tag(1) id(4 BE) len(4 BE) payload."""
    hdr = struct.pack("!BII", tag, 0, len(payload))
    sock.sendall(hdr + payload)

def frame_read(sock, timeout=2.0):
    """Read a spacelang frame. Returns (tag, payload) or None on failure."""
    sock.settimeout(timeout)
    try:
        hdr = b""
        while len(hdr) < 9:
            chunk = sock.recv(9 - len(hdr))
            if not chunk: return None
            hdr += chunk
        tag, _, length = struct.unpack("!BII", hdr)
        payload = b""
        while len(payload) < length:
            chunk = sock.recv(length - len(payload))
            if not chunk: return None
            payload += chunk
        return (tag, payload)
    except (socket.timeout, OSError):
        return None


# ── unit tests: language evaluation ───────────────────────────────────

class TestEval(TimedTestCase):
    """Language semantics — run via spci REPL and check output."""

    def eval(self, code):
        out, err, rc = run_spci(stdin=code + "\nbye!\n")
        self.assertEqual(rc, 0, f"spci crashed:\n{err}")
        return out

    def test_numbers(self):
        out = self.eval("42 .")
        self.assertIn("42", out)

    def test_arithmetic(self):
        out = self.eval("1 2 + 3 * .")
        self.assertIn("9", out)
        out = self.eval("10 3 / .")
        self.assertIn("3", out)
        out = self.eval("10 3 - .")
        self.assertIn("7", out)

    def test_comparisons(self):
        out = self.eval("1 2 < .")
        self.assertIn("t", out)
        out = self.eval("2 2 <= .")
        self.assertIn("t", out)
        out = self.eval("3 2 > .")
        self.assertIn("t", out)
        out = self.eval("2 2 >= .")
        self.assertIn("t", out)
        out = self.eval("2 2 = .")
        self.assertIn("t", out)
        out = self.eval("3 2 = .")
        self.assertIn("nil", out)  # false → nil

    def test_stack_ops(self):
        out = self.eval("1 2 swap . .")
        self.assertIn("1", out)
        self.assertIn("2", out)
        out = self.eval("5 dup + .")
        self.assertIn("10", out)
        out = self.eval("99 drop")
        self.assertNotIn("99", out)

    def test_if(self):
        # if: [else] [then] cond if  →  eagerly runs the selected thunk
        out = self.eval("[99] [42] true if .")
        self.assertIn("42", out)
        out = self.eval("[99] [42] false if .")
        self.assertIn("99", out)
        out = self.eval("[99] [42] nil if .")
        self.assertIn("99", out)
        out = self.eval("[99] [42] 32 if .")
        self.assertIn("42", out)
        out = self.eval("[99] [42] 'asd' if .")
        self.assertIn("42", out)
        out = self.eval("[99] [42] '' if .")
        self.assertIn("99", out)
        out = self.eval("[99] [42] [] if .")
        self.assertIn("99", out)

    def test_bind_and_eval(self):
        out = self.eval("[ 2 * ] [double] @  21 double ! .")
        self.assertIn("42", out)

    def test_nested_thunks(self):
        out = self.eval("[ 1 2 + ] ! .")
        self.assertIn("3", out)

    def test_cmp_cross_type(self):
        """Ordering operators are total across all Value types:
        number < bool/word < thunk < string."""
        # within-type
        out = self.eval('"a" "b" < .');     self.assertIn("t", out)
        out = self.eval('"b" "a" < .');     self.assertIn("nil", out)
        out = self.eval('"abc" "abd" < .'); self.assertIn("t", out)
        out = self.eval('"a" "a" <= .');    self.assertIn("t", out)
        out = self.eval('"b" "a" >= .');    self.assertIn("t", out)
        # atoms (bool sub-ordered alphabetically with word)
        out = self.eval('false true < .');  self.assertIn("t", out)
        # cross-type rank ordering
        out = self.eval('0 false < .');     self.assertIn("t", out)   # number < bool
        out = self.eval('true [x] < .');    self.assertIn("t", out)   # bool < thunk
        out = self.eval('[x] "x" < .');     self.assertIn("t", out)   # thunk < string
        out = self.eval('1 "a" < .');       self.assertIn("t", out)   # number < string (transitive)

    def test_eq_cross_type(self):
        """`=` is structural across types; mismatched types yield nil."""
        out = self.eval('1 1 = .');                       self.assertIn("t", out)
        out = self.eval('"a" "a" = .');                   self.assertIn("t", out)
        out = self.eval('[1 2 +] [1 2 +] = .');           self.assertIn("t", out)
        out = self.eval('1 "1" = .');                     self.assertIn("nil", out)
        out = self.eval('[1] [2] = .');                   self.assertIn("nil", out)

    def test_semicolon_no_newline(self):
        """`;` prints without a trailing newline; `.` adds one. A line
        composed of several `;` calls + a final `.` is a single line."""
        out, err, rc = run_spci(stdin='`hi` ; ` ` ; `there` .\nbye!\n')
        self.assertEqual(rc, 0, f"spci crashed:\n{err}")
        self.assertEqual(out.count('\n'), 1, f"expected one line, got: {out!r}")
        self.assertIn('"hi"', out)
        self.assertIn('"there"', out)

    def test_strings(self):
        out = self.eval('"hello" .')
        self.assertIn('"hello"', out)
        out = self.eval("'world' .")
        self.assertIn('"world"', out)

    def test_describe(self):
        out = self.eval("[ dup + ] [dub] @  [dub] ~")
        self.assertIn("dub ~", out)
        self.assertIn("dup", out)
        self.assertIn("+", out)

    def test_stack_print(self):
        out = self.eval("1 2 3 _s")
        self.assertIn("-- stack (3) --", out)
        self.assertIn("1", out)
        self.assertIn("2", out)
        self.assertIn("3", out)

    def test_slurp(self):
        # io/slurp returns a string, so use eval to convert to number
        import tempfile
        with tempfile.NamedTemporaryFile(mode="w", suffix=".sp", delete=False) as f:
            f.write("io/slurp eval 1 + .\n")  # io/slurp string, eval→num, add 1
            tmp = f.name
        try:
            out, err, rc = run_spci(stdin="41\n", args=[tmp], timeout=3)
            self.assertEqual(rc, 0, f"spci failed: {err}")
            self.assertIn("42", out)
        finally:
            os.unlink(tmp)

    def test_cat(self):
        out = self.eval('"foo" "bar" str/cat .')
        self.assertIn('"foobar"', out)

    def test_sleep(self):
        t0 = time.time()
        self.eval("200 io/sleep")
        self.assertGreaterEqual(time.time() - t0, 0.18)

    def test_sh(self):
        out = self.eval('"echo hello-from-sh" sh/! .')
        self.assertIn("hello-from-sh", out)
        self.assertIn("0", out)  # exit status

    def test_sh_capture(self):
        # sh/> pushes captured stdout as a string, trailing \n stripped.
        out = self.eval('"echo hello-from-capture" sh/> .')
        self.assertIn('"hello-from-capture"', out)

    def test_sh_capture_no_trailing_newline(self):
        # printf has no implicit newline; the captured string must match exactly.
        out = self.eval('''"printf 'exact'" sh/> .''')
        self.assertIn('"exact"', out)

    def test_sh_capture_multiline(self):
        # Internal newlines preserved; only the final one is stripped.
        out = self.eval('"printf \'a\\nb\\nc\\n\'" sh/> str/len .')
        # "a\nb\nc" → 5 bytes
        self.assertIn("5", out)

    def test_example_shell(self):
        """example/shell.sp exercises sh/!, sh/>, eval, and str/ helpers."""
        sp = ROOT / "example/shell.sp"
        out, err, rc = run_spci(args=[str(sp)], timeout=4)
        self.assertEqual(rc, 0, f"spci failed:\n{err}")
        # sh/! side-effect line went to stdout via echo
        self.assertIn("--- sh/! prints to its own stdout ---", out)
        # exit statuses: 0 for true, 256 (status << 8) for false
        self.assertIn("0", out)
        self.assertIn("256", out)
        # sh/> captures
        self.assertIn('"hello-from-capture"', out)
        self.assertIn('"no-trailing-newline"', out)
        # eval'd captured arithmetic
        self.assertIn("42", out)
        # str/ helpers composing with capture
        self.assertIn('"1.2.3"', out)
        # final boolean from str/starts-with?
        self.assertIn("t", out)
        # sh/| with grep -q: found and not-found cases
        self.assertIn("256", out)
        # sh/|> upper-case
        self.assertIn('"HELLO"', out)
        # sh/|> word count
        self.assertIn('"4"', out)
        # sh/> | sh/|> pipeline
        self.assertIn('"a\nb\nc"', out)

    def test_sh_capture_composable(self):
        # The captured string flows into the rest of the language.
        out = self.eval('"echo 41" sh/> eval 1 + .')
        self.assertIn("42", out)

    def test_sh_pipe_capture(self):
        # sh/|> pipes stdin into cmd, captures stdout.
        out = self.eval('"abc" "cat" sh/|> .')
        self.assertIn('"abc"', out)
        # word count: stdin "hello world" → wc -w → 2
        out = self.eval('"hello world" "wc -w" sh/|> .')
        self.assertIn('"2"', out)

    def test_sh_pipe_status(self):
        # sh/| pipes stdin into cmd, pushes exit status.
        out = self.eval('"anything" "true" sh/| .')
        self.assertIn("0", out)
        out = self.eval('"anything" "false" sh/| .')
        # false → exit 1; system()-style status<<8 == 256
        self.assertIn("256", out)

    def test_sh_pipe_chain(self):
        # Chain sh/> into sh/|> for a fully spacelang-driven pipeline:
        # produce "c\nb\na\n" via printf, capture, pipe to `sort`, capture.
        out = self.eval('''"printf 'c\\nb\\na\\n'" sh/> "sort" sh/|> .''')
        self.assertIn('"a\nb\nc"', out)

    def test_require(self):
        import tempfile as _tf
        with _tf.NamedTemporaryFile(mode="w", suffix=".sp", delete=False) as f:
            f.write("[ 7 ] [seven] @\n")
            lib = f.name
        try:
            out = self.eval(f'"{lib}" require seven ! .')
            self.assertIn("7", out)
        finally:
            os.unlink(lib)

    def test_exists_false(self):
        # Without --name/--bus, bus_dir is NULL → sp/exists? is always false
        out = self.eval('"NoSuchPeer" sp/exists? .')
        self.assertIn("nil", out)

    def test_name_str_roundtrip(self):
        # wo/name>str pulls the name out of a [X] form
        out = self.eval('[foo] wo/name>str .')
        self.assertIn('"foo"', out)
        # wo/str>name wraps a string into a [X] form
        out = self.eval('"bar" wo/str>name .')
        self.assertIn('[bar]', out)
        # round-trip preserves the name
        out = self.eval('[baz] wo/name>str wo/str>name wo/name>str .')
        self.assertIn('"baz"', out)

    def test_namespaced_word(self):
        # words can contain `/` for namespacing
        out = self.eval('[ 7 ] [ns/foo] @  ns/foo ! .')
        self.assertIn("7", out)
        out = self.eval('[ 5 ] [a/b/c] @  a/b/c ! .')
        self.assertIn("5", out)

    def test_string_destination(self):
        # "X" used as binding destination should be accepted same as [X]
        out = self.eval('[ 99 ] "X" @  X .')
        self.assertIn("99", out)

    def test_comments(self):
        out = self.eval("{ this is a comment } 7 .")
        self.assertIn("7", out)
        out = self.eval("1 { ignored } 2 + .")
        self.assertIn("3", out)

    def test_multiline_comment(self):
        out = self.eval("{ line one\nline two\nline three } 1 2 + .")
        self.assertIn("3", out)
        out = self.eval("{\n}\n9 .")
        self.assertIn("9", out)

    def test_require_via_override(self):
        """deps.sp override rewrites logical URLs to local paths."""
        tmp = tempfile.mkdtemp(prefix="spc-ovr-")
        try:
            (Path(tmp) / "deps.sp").write_text(
                '"." "github.com/test/proj" deps/override\n')
            (Path(tmp) / "lib.sp").write_text('"<from-override>" .\n')
            (Path(tmp) / "main.sp").write_text(
                '"github.com/test/proj/lib.sp" require\n')
            out, err, rc = run_spci(args=[str(Path(tmp) / "main.sp")], timeout=3)
            self.assertEqual(rc, 0, f"spci failed:\n{err}")
            self.assertIn("<from-override>", out)
        finally:
            shutil.rmtree(tmp, ignore_errors=True)


# ── compile tests: spcc produces correct binaries ─────────────────────

class TestCompile(TimedTestCase):
    """spcc compiles .sp files into running binaries that match spci."""

    def setUp(self):
        self.tmp = tempfile.mkdtemp()

    def tearDown(self):
        shutil.rmtree(self.tmp, ignore_errors=True)

    def compile_and_compare(self, source, expected_output):
        sp = Path(self.tmp) / "test.sp"
        sp.write_text(source)
        bin_path = str(Path(self.tmp) / "test_bin")

        # Run through spci
        out_spci, err, rc = run_spci(args=[str(sp)])
        self.assertEqual(rc, 0, f"spci failed: {err}")

        # Compile with spcc
        r = spcc_compile(sp, bin_path)
        self.assertEqual(r.returncode, 0, f"spcc failed:\n{r.stderr}")

        # Run compiled binary
        ok, out_bin = output_matches(out_spci, bin_path)
        self.assertTrue(ok, f"output mismatch:\n  spci: {out_spci!r}\n  bin:  {out_bin!r}")

    def test_simple(self):
        self.compile_and_compare("42 .", "")

    def test_arithmetic(self):
        self.compile_and_compare("1 2 + 3 * .", "")

    def test_bind_eval(self):
        self.compile_and_compare("[ 2 * ] [double] @  21 double ! .", "")

    def test_if(self):
        self.compile_and_compare("[99] [42] true if .", "")
        self.compile_and_compare("[99] [42] false if .", "")

    def test_example_add2(self):
        # add_2.sp is interactive (io/slurp io/slurp +) — requires stdin typing.
        # Skip compilation test for interactive examples.
        self.skipTest("add_2.sp is interactive, needs stdin typing")

    def test_example_fib(self):
        # fibonacci.sp reads count from stdin via io/slurp eval
        sp = Path(self.tmp) / "fib.sp"
        sp.write_text((ROOT / "example/fibonacci.sp").read_text())
        bin_path = str(Path(self.tmp) / "fib_bin")

        out_spci, err, rc = run_spci(stdin="5\n", args=[str(sp)])
        self.assertEqual(rc, 0, f"spci failed: {err}")
        self.assertIn("1\n1\n2\n3\n5\n8", out_spci)

        r = spcc_compile(sp, bin_path)
        self.assertEqual(r.returncode, 0, f"spcc failed:\n{r.stderr}")
        p = subprocess.run([bin_path], input="5\n", capture_output=True, text=True, timeout=4)
        self.assertEqual(p.returncode, 0)
        self.assertIn("1\n1\n2\n3\n5\n8", p.stdout)


# ── str/ library: primitives + helpers built on them ─────────────────

class TestStr(TimedTestCase):
    """str/ C primitives and the spacelang library on top."""

    def eval(self, code, preamble=""):
        full = preamble + code + "\nbye!\n"
        out, err, rc = run_spci(stdin=full)
        self.assertEqual(rc, 0, f"spci crashed:\n{err}")
        return out

    def lib_eval(self, code):
        return self.eval(code, preamble=f'"{ROOT}/stdlib/str.sp" require\n')

    # ----- C primitives -----

    def test_cat(self):
        out = self.eval('"foo" "bar" str/cat .')
        self.assertIn('"foobar"', out)

    def test_len(self):
        out = self.eval('"hello" str/len .')
        self.assertIn("5", out)
        out = self.eval('"" str/len .')
        self.assertIn("0", out)

    def test_sub_basic(self):
        out = self.eval('"hello, world" 7 5 str/sub .')
        self.assertIn('"world"', out)

    def test_sub_bounds_clamp(self):
        # over-length len clamps to remainder
        out = self.eval('"abc" 1 100 str/sub .')
        self.assertIn('"bc"', out)
        # negative start clamps to 0
        out = self.eval('"abc" -5 2 str/sub .')
        self.assertIn('"ab"', out)
        # start past end gives ""
        out = self.eval('"abc" 99 3 str/sub .')
        self.assertIn('""', out)

    def test_ord_chr_roundtrip(self):
        out = self.eval('"A" str/ord .')
        self.assertIn("65", out)
        out = self.eval('65 str/chr .')
        self.assertIn('"A"', out)
        out = self.eval('"" str/ord .')
        self.assertIn("-1", out)

    def test_eq(self):
        out = self.eval('"foo" "foo" str/eq .')
        self.assertIn("t", out)
        out = self.eval('"foo" "bar" str/eq .')
        # false prints as nil
        self.assertIn("nil", out)

    # ----- library on top -----

    def test_empty(self):
        out = self.lib_eval('"" str/empty? . "x" str/empty? .')
        # both results in stack order: t then nil
        self.assertIn("t", out)
        self.assertIn("nil", out)

    def test_head_tail(self):
        out = self.lib_eval('"hello" str/head .')
        self.assertIn('"h"', out)
        out = self.lib_eval('"hello" str/tail .')
        self.assertIn('"ello"', out)
        # tail of single-char is ""
        out = self.lib_eval('"x" str/tail .')
        self.assertIn('""', out)

    def test_reverse(self):
        out = self.lib_eval('"abcdef" str/reverse .')
        self.assertIn('"fedcba"', out)
        out = self.lib_eval('"" str/reverse .')
        self.assertIn('""', out)
        out = self.lib_eval('"a" str/reverse .')
        self.assertIn('"a"', out)

    def test_repeat(self):
        out = self.lib_eval('"ab" 3 str/repeat .')
        self.assertIn('"ababab"', out)
        out = self.lib_eval('"x" 0 str/repeat .')
        self.assertIn('""', out)
        out = self.lib_eval('"x" -2 str/repeat .')
        self.assertIn('""', out)

    def test_starts_with(self):
        out = self.lib_eval('"hello" "hel" str/starts-with? .')
        self.assertIn("t", out)
        out = self.lib_eval('"hello" "ell" str/starts-with? .')
        self.assertIn("nil", out)
        # prefix longer than s
        out = self.lib_eval('"hi" "hello" str/starts-with? .')
        self.assertIn("nil", out)
        # empty prefix always matches
        out = self.lib_eval('"hello" "" str/starts-with? .')
        self.assertIn("t", out)

    def test_ends_with(self):
        out = self.lib_eval('"hello" "llo" str/ends-with? .')
        self.assertIn("t", out)
        out = self.lib_eval('"hello" "hel" str/ends-with? .')
        self.assertIn("nil", out)
        out = self.lib_eval('"hi" "hello" str/ends-with? .')
        self.assertIn("nil", out)
        out = self.lib_eval('"hello" "" str/ends-with? .')
        self.assertIn("t", out)

    def test_contains(self):
        out = self.lib_eval('"hello world" "world" str/contains? .')
        self.assertIn("t", out)
        out = self.lib_eval('"hello" "xyz" str/contains? .')
        self.assertIn("nil", out)
        # match at start
        out = self.lib_eval('"hello" "hel" str/contains? .')
        self.assertIn("t", out)
        # empty needle always matches
        out = self.lib_eval('"hello" "" str/contains? .')
        self.assertIn("t", out)

    def test_example_strings(self):
        """example/strings.sp exercises every primitive and helper in str/."""
        sp = ROOT / "example/strings.sp"
        out, err, rc = run_spci(args=[str(sp)], timeout=4)
        self.assertEqual(rc, 0, f"spci failed:\n{err}")
        for expected in [
            '"foobar"', "5",                       # cat, len
            '"world"', '"bc"', '"ab"',             # sub (basic + clamps)
            "65", '"A"',                           # ord, chr
            '"gnalecaps"',                         # reverse
            '"abababab"',                          # repeat
            '"h"', '"ello"',                       # head, tail
            "10", "-1",                            # index hit + miss
            '"parse.sp"', '"parse"',               # compose: basename, drop-suffix
        ]:
            self.assertIn(expected, out, f"missing {expected!r} in output")

    def test_index(self):
        out = self.lib_eval('"hello world" "world" str/index .')
        self.assertIn("6", out)
        out = self.lib_eval('"hello world" "hello" str/index .')
        self.assertIn("0", out)
        out = self.lib_eval('"hello" "x" str/index .')
        self.assertIn("-1", out)
        # first occurrence wins
        out = self.lib_eval('"abcabc" "c" str/index .')
        self.assertIn("2", out)


# ── require preprocessor (spcc inlines required files) ──────────────

class TestRequirePreprocess(TimedTestCase):
    """spcc statically inlines `"path" require` so compiled binaries
    are self-contained. Errors out (with file:line) on non-string
    operand or missing file."""

    def setUp(self):
        self.tmp = tempfile.mkdtemp()

    def tearDown(self):
        shutil.rmtree(self.tmp, ignore_errors=True)

    def write(self, name, text):
        p = Path(self.tmp) / name
        p.write_text(text)
        return p

    def test_inlines_library(self):
        """`require`d file is embedded — after compile, deleting the .sp
        sources (and moving the binary elsewhere) must not break the run."""
        lib  = self.write("lib.sp", "[ 7 ] [seven] @\n")
        main = self.write("main.sp", '"lib.sp" require seven .\n')
        bin_path = str(Path(self.tmp) / "out")
        r = spcc_compile(main, bin_path)
        self.assertEqual(r.returncode, 0, r.stderr)

        # Move the binary to a fresh dir, then nuke ALL the .sp sources.
        elsewhere = tempfile.mkdtemp()
        try:
            moved = Path(elsewhere) / "out"
            shutil.move(bin_path, moved)
            os.unlink(lib)
            os.unlink(main)
            self.assertFalse(lib.exists())
            self.assertFalse(main.exists())

            p = subprocess.run([str(moved)], capture_output=True, text=True, timeout=3)
            self.assertEqual(p.returncode, 0, p.stderr)
            self.assertIn("7", p.stdout)
        finally:
            shutil.rmtree(elsewhere, ignore_errors=True)

    def test_binary_survives_source_deletion(self):
        """End-to-end self-containment: write sources, compile, delete the
        ENTIRE source tree, then the binary must still run correctly."""
        self.write("greet.sp", '[ "hi-from-lib" . ] [greet] @\n')
        self.write("util.sp",  '"greet.sp" require [ 21 21 + . ] [answer] @\n')
        main = self.write("main.sp", '"util.sp" require greet answer\n')
        bin_path = str(Path(self.tmp) / "out")

        r = spcc_compile(main, bin_path)
        self.assertEqual(r.returncode, 0, r.stderr)

        # Move binary, then wipe the entire source dir.
        elsewhere = tempfile.mkdtemp()
        try:
            moved = Path(elsewhere) / "out"
            shutil.move(bin_path, moved)
            shutil.rmtree(self.tmp)
            self.assertFalse(Path(self.tmp).exists(),
                "source tree should be gone before we run the binary")

            p = subprocess.run([str(moved)], capture_output=True, text=True, timeout=3)
            self.assertEqual(p.returncode, 0, p.stderr)
            self.assertIn("hi-from-lib", p.stdout)
            self.assertIn("42", p.stdout)
        finally:
            shutil.rmtree(elsewhere, ignore_errors=True)
            # tearDown also rmtree's self.tmp; that's already ignore_errors=True.
            os.makedirs(self.tmp, exist_ok=True)

    def test_recursive_require(self):
        """A required file may itself require another."""
        self.write("a.sp", "[ 1 ] [one] @\n")
        self.write("b.sp", '"a.sp" require [ one 1 + ] [two] @\n')
        main = self.write("main.sp", '"b.sp" require two .\n')
        bin_path = str(Path(self.tmp) / "out")
        r = spcc_compile(main, bin_path)
        self.assertEqual(r.returncode, 0, r.stderr)
        p = subprocess.run([bin_path], capture_output=True, text=True, timeout=3)
        self.assertIn("2", p.stdout)

    def test_cycle_safe(self):
        """Mutual requires don't infinite-loop; each file loaded once."""
        self.write("a.sp", '"b.sp" require [ 1 ] [a-marker] @\n')
        self.write("b.sp", '"a.sp" require [ 2 ] [b-marker] @\n')
        main = self.write("main.sp", '"a.sp" require a-marker . b-marker .\n')
        bin_path = str(Path(self.tmp) / "out")
        r = spcc_compile(main, bin_path)
        self.assertEqual(r.returncode, 0, r.stderr)
        p = subprocess.run([bin_path], capture_output=True, text=True, timeout=3)
        self.assertIn("1", p.stdout)
        self.assertIn("2", p.stdout)

    def test_error_non_string_operand(self):
        """require with a non-string operand is rejected at compile time
        with file:line context."""
        main = self.write("main.sp",
            "{ comment }\n"
            "1 2 + require\n")        # line 2 has the bad require
        bin_path = str(Path(self.tmp) / "out")
        r = spcc_compile(main, bin_path)
        self.assertNotEqual(r.returncode, 0)
        self.assertIn("require", r.stderr)
        self.assertIn("string literal", r.stderr)
        self.assertIn(f"{main}:2", r.stderr)

    def test_error_missing_file(self):
        """Missing require target is rejected at compile time with
        the file:line of the offending string literal."""
        main = self.write("main.sp",
            "{ a header }\n"
            "{ another }\n"
            '"nope-does-not-exist.sp" require\n')   # line 3
        bin_path = str(Path(self.tmp) / "out")
        r = spcc_compile(main, bin_path)
        self.assertNotEqual(r.returncode, 0)
        self.assertIn("require", r.stderr)
        self.assertIn("nope-does-not-exist.sp", r.stderr)
        self.assertIn(f"{main}:3", r.stderr)


# ── spct / test.sp: the test-runner binary and stdlib ─────────────────

class TestSpct(TimedTestCase):
    """spct binary and test.sp stdlib integration tests."""

    @classmethod
    def setUpClass(cls):
        if not os.path.exists(SPCT):
            raise unittest.SkipTest("spct binary missing; run `make spct`")

    def spct(self, *test_files, timeout=4):
        """Run spct with given test files, return (stdout, stderr, rc)."""
        p = subprocess.run(
            [SPCT] + list(test_files),
            capture_output=True, text=True, timeout=timeout
        )
        return p.stdout, p.stderr, p.returncode

    def spci_stdin(self, code, timeout=4):
        """Run spci REPL with stdin code, return combined output."""
        out, err, rc = run_spci(stdin=code + "\nbye!\n", timeout=timeout)
        self.assertEqual(rc, 0, f"spci crashed:\n{err}")
        return out + err

    def setUp(self):
        self.tmp = tempfile.mkdtemp(prefix="spct-")

    def tearDown(self):
        shutil.rmtree(self.tmp, ignore_errors=True)

    def write_sp(self, name, text):
        p = Path(self.tmp) / name
        p.write_text(text)
        return str(p)

    # ── spct binary ──

    def test_spct_no_args(self):
        """spct with no arguments prints usage."""
        _, stderr, rc = self.spct()
        self.assertIn("Usage", stderr)

    def test_spct_simple_pass(self):
        """spct runs a simple passing test suite."""
        f = self.write_sp("t.sp", """
test/reset
"hello" test/heading
1 2 + 3 "1+2=3" test/eq
test/summary
""")
        _, stderr, rc = self.spct(f)
        self.assertIn("PASS 1+2=3", stderr)
        self.assertIn("ALL PASSED", stderr)
        self.assertNotIn("FAIL", stderr)

    def test_spct_with_failures(self):
        """spct reports failures correctly."""
        f = self.write_sp("t.sp", """
test/reset
2 2 + 5 "2+2=5" test/eq
test/summary
""")
        _, stderr, rc = self.spct(f)
        self.assertIn("FAIL 2+2=5", stderr)
        self.assertIn("FAIL: 1", stderr)

    def test_spct_multiple_sections(self):
        """spct handles multiple test/heading sections."""
        f = self.write_sp("t.sp", """
test/reset
"section a" test/heading
true "a1" test/assert
"section b" test/heading
false "b1" test/assert
test/summary
""")
        _, stderr, rc = self.spct(f)
        self.assertIn("--- section a ---", stderr)
        self.assertIn("--- section b ---", stderr)
        self.assertIn("PASS a1", stderr)
        self.assertIn("FAIL b1", stderr)
        self.assertIn("FAIL: 1", stderr)

    def test_example_tests_sp(self):
        """example/tests_test.sp runs end-to-end through spct."""
        _, stderr, rc = self.spct(str(ROOT / "example/tests_test.sp"))
        self.assertIn("PASS 1+2=3", stderr)
        self.assertIn("PASS 5*2=10", stderr)
        self.assertIn("PASS 3*4!=13", stderr)
        self.assertIn("PASS true passes", stderr)
        self.assertIn("FAIL false fails", stderr)  # deliberate
        self.assertIn("PASS 4>2", stderr)
        self.assertIn("PASS nil", stderr)
        self.assertIn("PASS concat", stderr)
        self.assertIn("PASS: 7", stderr)
        self.assertIn("FAIL: 1", stderr)

    def test_spct_dir_discovery(self):
        """spct <dir> finds and runs all *_test.sp files under it (like `go test`)."""
        a = self.write_sp("a_test.sp", """
test/reset
"alpha" test/heading
1 1 + 2 "a-eq" test/eq
test/summary
""")
        # nested dir to verify recursion
        sub = Path(self.tmp) / "nested"
        sub.mkdir()
        b = sub / "b_test.sp"
        b.write_text("""
test/reset
"beta" test/heading
true "b-ok" test/assert
test/summary
""")
        # a non-test file should be ignored
        Path(self.tmp, "ignored.sp").write_text('"nope" .\n')

        _, stderr, _ = self.spct(self.tmp)
        self.assertIn("PASS a-eq", stderr)
        self.assertIn("PASS b-ok", stderr)
        self.assertNotIn("nope", stderr)

    def test_spct_dot(self):
        """spct . discovers *_test.sp under the current directory."""
        self.write_sp("c_test.sp", """
test/reset
"gamma" test/heading
3 3 "c-eq" test/eq
test/summary
""")
        cwd = os.getcwd()
        try:
            os.chdir(self.tmp)
            _, stderr, _ = self.spct(".")
        finally:
            os.chdir(cwd)
        self.assertIn("PASS c-eq", stderr)
        self.assertIn("ALL PASSED", stderr)

    def test_example_cmp_test_sp(self):
        """example/cmp_test.sp covers the total ordering across all Value types."""
        _, stderr, _ = self.spct(str(ROOT / "example/cmp_test.sp"))
        self.assertIn("ALL PASSED (21)", stderr)

    def test_math_library_example(self):
        """../sp-math/example/math_test.sp runs all 53 math assertions."""
        math_tests = ROOT.parent / "sp-math" / "example" / "math_test.sp"
        if not math_tests.exists():
            self.skipTest("sp-math repo not found alongside spacelang")
        cwd = os.getcwd()
        try:
            os.chdir(str(math_tests.parent.parent))
            _, stderr, rc = self.spct("example/math_test.sp")
        finally:
            os.chdir(cwd)
        self.assertIn("ALL PASSED (53)", stderr)

    # ── test.sp words via spci REPL ──

    def lib_eval(self, code):
        """Run code with test.sp stdlib preloaded."""
        pre = f'"{ROOT}/stdlib/str.sp" require "{ROOT}/stdlib/test.sp" require\n'
        return self.spci_stdin(pre + code)

    def test_reset_zeros_counters(self):
        out = self.lib_eval("test/reset test/_pass . test/_fail .")
        self.assertIn("0", out)

    def test_heading_output(self):
        out = self.lib_eval('"my suite" test/heading')
        self.assertIn("--- my suite ---", out)

    def test_assert_pass(self):
        out = self.lib_eval('test/reset true "t1" test/assert')
        self.assertIn("PASS t1", out)

    def test_assert_fail(self):
        out = self.lib_eval('test/reset false "f1" test/assert')
        self.assertIn("FAIL f1", out)

    def test_eq_pass(self):
        out = self.lib_eval('test/reset 1 2 + 3 "add" test/eq')
        self.assertIn("PASS add", out)

    def test_eq_fail(self):
        out = self.lib_eval('test/reset 2 2 + 5 "bad" test/eq')
        self.assertIn("FAIL bad", out)

    def test_neq_pass(self):
        out = self.lib_eval('test/reset 3 4 "neq" test/neq')
        self.assertIn("PASS neq", out)

    def test_neq_fail(self):
        out = self.lib_eval('test/reset 5 5 "same" test/neq')
        self.assertIn("FAIL same", out)

    def test_true_assert(self):
        out = self.lib_eval('test/reset 42 0 > "pos" test/true?')
        self.assertIn("PASS pos", out)

    def test_false_assert(self):
        out = self.lib_eval('test/reset 0 "zero" test/false?')
        self.assertIn("PASS zero", out)

    def test_str_eq(self):
        out = self.lib_eval('test/reset "hi" "hi" "greet" test/str-eq')
        self.assertIn("PASS greet", out)

    def test_str_eq_fail(self):
        out = self.lib_eval('test/reset "hi" "yo" "mismatch" test/str-eq')
        self.assertIn("FAIL mismatch", out)

    def test_summary_all_pass(self):
        out = self.lib_eval('test/reset true "a" test/assert true "b" test/assert test/summary')
        self.assertIn("ALL PASSED", out)

    def test_summary_with_failures(self):
        out = self.lib_eval('test/reset true "a" test/assert false "b" test/assert test/summary')
        self.assertIn("PASS: 1", out)
        self.assertIn("FAIL: 1", out)

    def test_summary_no_tests(self):
        out = self.lib_eval('test/reset test/summary')
        self.assertIn("no tests run", out)

    def test_counter_increments(self):
        """Verify pass/fail counters increment correctly (string-based)."""
        out = self.lib_eval('test/reset true "a" test/assert test/_pass str/len .')
        # Pass counter is "1" (string length 1) or more if other tests ran
        self.assertIn("1", out)

    # ── str/->str ──

    def test_str_to_str_identity(self):
        """str/->str is identity for string values."""
        out = self.spci_stdin(
            f'"{ROOT}/stdlib/str.sp" require\n'
            '"hello" str/->str .\n'
        )
        self.assertIn('"hello"', out)


# ── property tests: random term sequences ─────────────────────────────

TERMS = [
    "42", "0", "1", "2", "3", "10", "100", "-1", "-5",
    '"hello"', '"x"', '""',
    "true", "false", "nil",
    "+", "-", "*", "/",
    "<", ">", "<=", ">=", "=",
    "dup", "swap", "drop",
    ".",
    "[ 1 ]", "[ 1 2 + ]", "[ dup + ]",
    "_s",
]

class TestProperty(TimedTestCase):
    """Randomized sequences should never crash the interpreter."""

    def test_random_sequences(self):
        """Random valid-ish sequences should never segfault.
        We generate stack-balanced programs: push more than we pop."""
        seed = int(os.environ.get("SPACELANG_TEST_SEED", random.randint(0, 2**31)))
        rng = random.Random(seed)

        PUSHERS = ["42", "0", "1", "2", "3", '10', '"x"', "true", "false", "[ 1 ]", "[ 1 2 + ]"]
        OPS     = ["+", "*", "<", ">", "=", "dup", "swap", "drop", "."]
        failures = []

        for i in range(200):
            # Build a program that always has enough operands:
            # For each op, push 2-3 values first.
            seq = []
            n_ops = rng.randint(3, 12)
            for _ in range(n_ops):
                seq.append(rng.choice(PUSHERS))
                seq.append(rng.choice(PUSHERS))
                seq.append(rng.choice(PUSHERS))
                seq.append(rng.choice(OPS))
            code = " ".join(seq) + " bye!"
            out, err, rc = run_spci(stdin=code, timeout=4)
            if rc != 0 and "stack underflow" not in err and "expected number" not in err:
                failures.append((seq, err))

        if failures:
            msg = f"[seed={seed}] {len(failures)}/{200} sequences had unexpected crashes:\n"
            for seq, err in failures[:5]:
                msg += f"  {' '.join(seq)}\n  → {err.strip()}\n"
            self.fail(msg)


# ── spcdbg: gdb-like stepper for spacelang ────────────────────────────

def run_spcdbg(sp_file=None, stdin="", args=(), timeout=4):
    """Run spcdbg, returning (stdout, stderr, returncode)."""
    argv = [SPCDBG] + list(args)
    if sp_file is not None:
        argv.append(str(sp_file))
    p = subprocess.run(argv, input=stdin, capture_output=True, text=True, timeout=timeout)
    return p.stdout, p.stderr, p.returncode


class TestSpcdbg(TimedTestCase):
    """Debugger driver — per-word stepping over a loaded file."""

    def _write(self, code):
        d = Path(tempfile.mkdtemp(prefix="spcdbg_"))
        self.addCleanup(shutil.rmtree, d, ignore_errors=True)
        f = d / "p.sp"
        f.write_text(code)
        return f

    def test_quit_exits_cleanly(self):
        """`q` at the initial debugger prompt exits with status 0."""
        f = self._write("1 2 +\n")
        out, err, rc = run_spcdbg(sp_file=f, stdin="q\n")
        self.assertEqual(rc, 0, f"stderr: {err}")

    def test_cycle_counter_uses_at_symbol(self):
        """View header uses `@` for the cycle counter, and the count reflects
        actual evaluation cycles — including silent ones from `<N> s`."""
        f = self._write("1 2 3 4 5\n")
        out, err, rc = run_spcdbg(sp_file=f, stdin="5 s\nq\n")
        self.assertEqual(rc, 0, f"stderr: {err}")
        # No `step` label any more.
        self.assertNotIn("step", err.lower())
        # `5 s` advances 5 cycles; header shows @ 5.
        self.assertRegex(err, r"@\s*5\b")

    def test_step_with_repeat_count(self):
        """`N s` performs N steps and stops on the Nth."""
        f = self._write("1 2 3 4 5 6\n")
        out, err, rc = run_spcdbg(sp_file=f, stdin="5 s\nq\n")
        self.assertEqual(rc, 0, f"stderr: {err}")
        self.assertRegex(err, r"(?m)^>\s+5\s*$")

    def test_next_with_repeat_count(self):
        """`N n` performs N step-overs."""
        # All top-level terms (no user words), so n behaves like s here.
        f = self._write("1 2 3 4 5 6\n")
        out, err, rc = run_spcdbg(sp_file=f, stdin="4 n\nq\n")
        self.assertEqual(rc, 0, f"stderr: {err}")
        self.assertRegex(err, r"(?m)^>\s+4\s*$")

    def test_watch_short_aliases_wa_wd(self):
        """`wa <name>` adds a watch and `wd <name>` removes it."""
        f = self._write("1 [i] @\ni\n")
        out, err, rc = run_spcdbg(sp_file=f, stdin="s\ns\ns\nwa i\ns\nwd i\ns\nq\n")
        self.assertEqual(rc, 0, f"stderr: {err}")
        self.assertNotIn("unknown command", err)
        self.assertIn("i = 1", err)
        # Watches panel appears exactly once (until wd removes it).
        self.assertEqual(err.count("watches:"), 1, err)

    def test_watch_trims_trailing_whitespace(self):
        """`watch add i ` (trailing space) must look up `i`, not `i `."""
        f = self._write("1 [i] @\ni\n")
        out, err, rc = run_spcdbg(sp_file=f, stdin="s\ns\ns\nwatch add i \ns\nq\n")
        self.assertEqual(rc, 0, f"stderr: {err}")
        # After binding i to 1, watching `i ` should resolve to its value, not
        # report it as (unbound).
        self.assertIn("i = 1", err)
        self.assertNotIn("i = (unbound)", err)

    def test_watch_add_and_remove(self):
        """`watch add foo` shows `foo` in the watches panel of subsequent
        renders. `watch remove foo` drops it again."""
        f = self._write("[ 1 ] [foo] @\nfoo\n")
        # Step past binding, add watch, step (see watches), remove, step, q.
        out, err, rc = run_spcdbg(
            sp_file=f,
            stdin="s\ns\ns\nwatch add foo\ns\nwatch remove foo\ns\nq\n",
        )
        self.assertEqual(rc, 0, f"stderr: {err}")
        self.assertNotIn("unknown command", err)
        # Watches panel appears exactly once — when watching, not after remove.
        self.assertRegex(err, r"(?s)watches:.*foo")
        self.assertEqual(err.count("watches:"), 1, err)

    def test_toggle_bindings_view(self):
        """`w` toggles a panel listing user-defined bindings currently in scope.
        After `[ 1 ] [foo] @` binds `foo`, toggling on should show `foo`."""
        f = self._write("[ 1 ] [foo] @\nfoo\n")
        # Step past [1], [foo], @ (now foo is bound), then `w` toggles, render.
        out, err, rc = run_spcdbg(sp_file=f, stdin="s\ns\ns\nw\ns\nq\n")
        self.assertEqual(rc, 0, f"stderr: {err}")
        self.assertNotIn("unknown command", err)
        # After toggling on, the next render should show a bindings section
        # listing `foo`.
        after_w = err.split("w\n", 1)
        # Either the render after `w` has bindings panel; just assert `foo` is
        # somewhere alongside a bindings label.
        self.assertRegex(err, r"(?s)bindings:.*foo")

    def test_render_layout(self):
        """`>` on the current instruction, `>>` on the next, a separator line
        before the stack, and no trailing `next:` line."""
        f = self._write("1 2 +\n")
        out, err, rc = run_spcdbg(sp_file=f, stdin="s\nq\n")
        self.assertEqual(rc, 0, f"stderr: {err}")
        # `>` cursor on `1` and `>>` on `2`.
        self.assertRegex(err, r"(?m)^>\s+1\b")
        self.assertRegex(err, r"(?m)^>>\s+2\b")
        # Dashed separator lines above and below the stack.
        self.assertRegex(err, r"(?s)-{6,}\nstack:.*\n-{6,}")
        # No `── next: ... ──` footer.
        self.assertNotIn("next:", err)

    def test_step_stops_at_thunk_literals(self):
        """`s` should pause at every top-level term, including thunk pushes —
        not skip them."""
        f = self._write("[ 1 ] [foo] @\n")
        # First `s` should stop at the first term `[ 1 ]`, not jump to `@`.
        out, err, rc = run_spcdbg(sp_file=f, stdin="s\nq\n")
        self.assertEqual(rc, 0, f"stderr: {err}")
        # The first announced step is the thunk `[ 1 ]`, not `@`.
        self.assertRegex(err, r">\s*\[\s*1\s*\]")
        # And `@` should NOT have been announced yet.
        self.assertNotIn("next: @", err)

    def test_view_steps_into_user_word_body(self):
        """When stepping into a user-defined word, the rendered source view
        switches to that word's body."""
        f = self._write("[ 1 2 + ] [foo] @\nfoo\n")
        # Per-term stepping: s→[1 2 +], s→[foo], s→@, s→foo, s→descend.
        out, err, rc = run_spcdbg(sp_file=f, stdin="s\ns\ns\ns\ns\nq\n")
        self.assertEqual(rc, 0, f"stderr: {err}")
        # While inside foo, header announces we're in foo (step counter may
        # follow the name in the header).
        self.assertRegex(err, r"── in foo")
        # And the body's instructions are in the listing under that header.
        in_foo = re.split(r"── in foo[^\n]*\n", err, maxsplit=1)[1]
        self.assertIn("1", in_foo)
        self.assertIn("+", in_foo)

    def test_source_view_shows_marker_at_current_term(self):
        """On every stop, render the program with each top-level instruction
        on its own line and a `>` cursor at the term about to execute."""
        f = self._write("1 2 + .\n")
        # Per-term stepping: s→1, s→2, s→+. Render should mark `+` on the 3rd.
        out, err, rc = run_spcdbg(sp_file=f, stdin="s\ns\ns\nq\n")
        self.assertEqual(rc, 0, f"stderr: {err}")
        # `>` on the line with `+`.
        self.assertRegex(err, r">\s*\+")

    def test_stack_rendered_one_row(self):
        """Stack is shown on a single row underneath the source view."""
        f = self._write("1 2 +\n")
        # Step past 1 and 2 (pushed) to reach +; stack now has 1 and 2.
        out, err, rc = run_spcdbg(sp_file=f, stdin="s\ns\ns\nq\n")
        self.assertEqual(rc, 0, f"stderr: {err}")
        self.assertRegex(err, r"stack:.*1.*2")

    def test_step_auto_prints_stack(self):
        """On every stop, the whole data stack is printed below the
        `next:` line so consecutive identical words (e.g. many `@`s) can be
        distinguished by what's about to be consumed."""
        f = self._write("[ 1 ] [foo] @\n")
        # Step past [ 1 ] and [foo] to reach @; stack has both thunks.
        out, err, rc = run_spcdbg(sp_file=f, stdin="s\ns\ns\nq\n")
        self.assertEqual(rc, 0, f"stderr: {err}")
        # The `>` cursor should land on the `@` term.
        self.assertRegex(err, r"(?m)^>\s+@\s*$")
        self.assertIn("foo", err)
        self.assertIn("1", err)

    def test_print_stack_when_empty_shows_marker(self):
        """When the data stack is empty, `p` prints a clear marker instead of
        nothing — otherwise the user can't tell the command ran."""
        # Bind a thunk: `[ 1 ] [foo] @` consumes the thunk + the [foo] term,
        # leaving the stack empty. Step until we're past the @.
        # After `@` binds, stack is empty. Then `foo` is the next stop.
        f = self._write("[ 1 ] [foo] @\nfoo\n")
        out, err, rc = run_spcdbg(sp_file=f, stdin="s\ns\np\nq\n")
        self.assertEqual(rc, 0, f"stderr: {err}")
        self.assertIn("empty", err.lower())

    def test_empty_line_repeats_last_command(self):
        """An empty prompt line re-runs the previous command (gdb behavior).
        After `s` we stop at a term; bare Enter steps again — we should see
        two distinct terms announced from `1 2`."""
        f = self._write("1 2 + .\n")
        out, err, rc = run_spcdbg(sp_file=f, stdin="s\n\nq\n")
        self.assertEqual(rc, 0, f"stderr: {err}")
        # Two distinct stops: cursor on `1`, then on `2`.
        self.assertRegex(err, r"(?m)^>\s+1\b")
        self.assertRegex(err, r"(?m)^>\s+2\b")

    def test_help_lists_commands(self):
        f = self._write("1\n")
        out, err, rc = run_spcdbg(sp_file=f, stdin="help\nq\n")
        self.assertEqual(rc, 0, f"stderr: {err}")
        self.assertNotIn("unknown command", err)
        # Must mention the core commands.
        for cmd in ("step", "cont", "break", "bt", "quit"):
            self.assertIn(cmd, err, f"help missing {cmd!r}")

    def test_next_steps_over_user_word(self):
        """`n` at the entry of a user-defined word should run that word to
        completion without stopping inside it."""
        f = self._write("[ 1 2 + ] [foo] @\nfoo .\n")
        # Step per-term: s→[1 2 +], s→[foo], s→@, s→foo (about to descend),
        # n over foo, then we stop at `.`.
        out, err, rc = run_spcdbg(sp_file=f, stdin="s\ns\ns\ns\nn\nq\n")
        self.assertEqual(rc, 0, f"stderr: {err}")
        # `+` should never have been the current cursor (would mean we stopped
        # inside foo's body); `.` should be the cursor after stepping over.
        self.assertNotRegex(err, r"(?m)^>\s+\+\b")
        self.assertRegex(err, r"(?m)^>\s+\.")

    def test_backtrace_shows_enclosing_word(self):
        """When stopped inside a user-defined word, `bt` lists it as a frame."""
        f = self._write("[ 1 2 + ] [foo] @\nfoo\n")
        out, err, rc = run_spcdbg(sp_file=f, stdin="b +\nc\nbt\nc\n")
        self.assertEqual(rc, 0, f"stderr: {err}")
        self.assertNotIn("unknown command", err)
        # The bt output should mention `foo` (the enclosing user word).
        # Find the line(s) after `bt` was issued.
        self.assertIn("foo", err)

    def test_breakpoint_by_word_name(self):
        """`b +` then `c` stops just before `+` executes; another `c` runs to end."""
        f = self._write("1 2 + .\n")
        out, err, rc = run_spcdbg(sp_file=f, stdin="b +\nc\np\nc\n")
        self.assertEqual(rc, 0, f"stderr: {err}")
        # Hit the breakpoint at +, then printed final 3.
        self.assertIn("+", err)
        self.assertIn("3", out)
        # And the stack at the break should still have 1 and 2.
        self.assertIn("1", err)
        self.assertIn("2", err)

    def test_info_b_lists_breakpoints(self):
        f = self._write("1\n")
        out, err, rc = run_spcdbg(sp_file=f, stdin="b foo\nb bar\ninfo b\nq\n")
        self.assertEqual(rc, 0)
        # Reject the trivial false-positive of the echoed command line.
        self.assertNotIn("unknown command", err)
        self.assertIn("foo", err)
        self.assertIn("bar", err)

    def test_delete_breakpoint(self):
        """`d +` removes a breakpoint; `c` then runs to end without stopping."""
        f = self._write("1 2 + .\n")
        out, err, rc = run_spcdbg(sp_file=f, stdin="b +\nd +\nc\n")
        self.assertEqual(rc, 0, f"stderr: {err}")
        self.assertIn("3", out)
        self.assertNotIn("unknown command", err)
        # `bp: +` (the break-hit marker) should NOT appear.
        self.assertNotIn("bp:", err)

    def test_print_stack_shows_values(self):
        """At a step-stop, `p` shows the current data stack.
        After `s` past `1 2`, top should be 2 with 1 underneath, before `+`."""
        f = self._write("1 2 +\n")
        # First `s` stops at the first WORD token = `+`. By then 1 and 2
        # are on the stack (literals push without entering eval_word).
        out, err, rc = run_spcdbg(sp_file=f, stdin="s\np\nq\n")
        self.assertEqual(rc, 0, f"stderr: {err}")
        # Stack output should mention both numbers.
        self.assertIn("1", err)
        self.assertIn("2", err)

    def test_continue_runs_to_end(self):
        """`c` clears stepping and lets the program finish without prompts.
        We use `.` (print) so successful execution produces visible output."""
        f = self._write("1 2 + .\n")
        out, err, rc = run_spcdbg(sp_file=f, stdin="c\n")
        self.assertEqual(rc, 0, f"stderr: {err}")
        self.assertIn("3", out)

    def test_step_announces_next_word(self):
        """`s` runs one word and announces it before stopping again.
        Program `1 2 +`: literals push silently, the first `word` token to
        eval is `+`. After one `s` we expect to see `+` reported on stderr."""
        f = self._write("1 2 +\n")
        out, err, rc = run_spcdbg(sp_file=f, stdin="s\nq\n")
        self.assertEqual(rc, 0, f"stderr: {err}")
        self.assertIn("+", err)


# ── spcd integration: package manager verbs against local bare repos ──

SPCD   = str(ROOT / "bin" / "spcd")
FIXDIR = Path("/tmp/spacelang_git_test")


class TestSpcd(unittest.TestCase):
    """Integration tests for the compiled spcd binary.

    Uses local bare git repos under /tmp/spacelang_git_test (no network).
    test-repo.git is bootstrapped by test_git.sp and reused; dep-a, dep-b,
    and binrepo are created on demand for transitive-dep and install
    coverage. Each test runs in a fresh sandbox.
    """

    TIMEOUT = 5

    @classmethod
    def setUpClass(cls):
        if not os.path.exists(SPCD):
            raise unittest.SkipTest("spcd binary missing; run `make spcd` or "
                                    "`./bin/spcc --as spcd spcd/spcd.sp -o bin/spcd`")
        if not (FIXDIR / "test-repo.git").is_dir():
            raise unittest.SkipTest(
                f"{FIXDIR}/test-repo.git missing; run `spci test_git.sp` first")
        cls._make_fixture_dep_a()
        cls._make_fixture_dep_b()
        cls._make_fixture_dotty()
        cls._make_fixture_binrepo()

    @staticmethod
    def _git_init_and_bare(seed, bare):
        subprocess.run(["git", "init", "-q", "-b", "master", str(seed)], check=True)
        env = {**os.environ,
               "GIT_AUTHOR_NAME": "t", "GIT_AUTHOR_EMAIL": "t@t",
               "GIT_COMMITTER_NAME": "t", "GIT_COMMITTER_EMAIL": "t@t"}
        subprocess.run(["git", "-C", str(seed), "add", "."], check=True)
        subprocess.run(["git", "-C", str(seed), "commit", "-q", "-m", "seed"],
                       check=True, env=env)
        subprocess.run(["git", "clone", "-q", "--bare", str(seed), str(bare)],
                       check=True)

    @classmethod
    def _make_fixture_dep_a(cls):
        bare = FIXDIR / "dep-a.git"
        if bare.is_dir(): return
        seed = FIXDIR / "_seed-a"
        shutil.rmtree(seed, ignore_errors=True)
        seed.mkdir(parents=True)
        (seed / "lib.sp").write_text("{ dep-a stub }\n")
        cls._git_init_and_bare(seed, bare)
        shutil.rmtree(seed, ignore_errors=True)

    @classmethod
    def _make_fixture_dep_b(cls):
        bare = FIXDIR / "dep-b.git"
        if bare.is_dir(): return
        seed = FIXDIR / "_seed-b"
        shutil.rmtree(seed, ignore_errors=True)
        seed.mkdir(parents=True)
        (seed / "lib.sp").write_text("{ dep-b stub }\n")
        (seed / "deps.sp").write_text(f'"{FIXDIR}/dep-a.git" deps/head\n')
        cls._git_init_and_bare(seed, bare)
        shutil.rmtree(seed, ignore_errors=True)

    @classmethod
    def _make_fixture_dotty(cls):
        """Bare repo whose path contains dots — exercises the lock.sp
        round-trip, since unquoted URLs with '.' get split by the parser."""
        bare = FIXDIR / "site.example.com.git"
        if bare.is_dir(): return
        seed = FIXDIR / "_seed-dotty"
        shutil.rmtree(seed, ignore_errors=True)
        seed.mkdir(parents=True)
        (seed / "lib.sp").write_text("{ dotty stub }\n")
        cls._git_init_and_bare(seed, bare)
        shutil.rmtree(seed, ignore_errors=True)

    @classmethod
    def _make_fixture_binrepo(cls):
        bare = FIXDIR / "binrepo.git"
        if bare.is_dir(): return
        seed = FIXDIR / "_seed-bin"
        shutil.rmtree(seed, ignore_errors=True)
        seed.mkdir(parents=True)
        (seed / "main.sp").write_text('`hello from binrepo` io/log\n')
        cls._git_init_and_bare(seed, bare)
        shutil.rmtree(seed, ignore_errors=True)

    def setUp(self):
        self.tmp = tempfile.mkdtemp(prefix="spcd-")
        self._cwd = os.getcwd()
        os.chdir(self.tmp)

    def tearDown(self):
        os.chdir(self._cwd)
        shutil.rmtree(self.tmp, ignore_errors=True)

    def spcd(self, *args, env=None, check=False):
        full_env = {**os.environ, **(env or {})}
        return subprocess.run(
            [SPCD, *args], capture_output=True, text=True,
            timeout=self.TIMEOUT, env=full_env, check=check)

    # -- T1
    def test_add_head(self):
        self.spcd("add", str(FIXDIR / "test-repo.git"))
        self.assertTrue(Path("deps.sp").is_file())
        self.assertIn("deps/head", Path("deps.sp").read_text())
        self.assertTrue(Path("spcd_lib/test-repo").is_dir())
        self.assertIn("0c7e6d3d9794a92ede8dfa53040de810dd3f6e7a",
                      Path("lock.sp").read_text())

    # -- T2
    def test_list_prints_lock(self):
        self.spcd("add", str(FIXDIR / "test-repo.git"))
        r = self.spcd("list")
        self.assertIn("0c7e6d3d9794a92ede8dfa53040de810dd3f6e7a", r.stdout + r.stderr)

    # -- T3
    def test_fetch_is_idempotent(self):
        self.spcd("add", str(FIXDIR / "test-repo.git"))
        before = Path("lock.sp").read_text()
        self.spcd("fetch")
        self.assertEqual(before, Path("lock.sp").read_text())

    # -- T3c: live regression against github.com/cstml/spacelang.
    # Skipped offline or when the host is unreachable.
    def test_fetch_idempotent_github(self):
        try:
            socket.create_connection(("github.com", 443), timeout=2).close()
        except OSError:
            raise unittest.SkipTest("no network to github.com")
        url = "github.com/cstml/spacelang"
        r = self.spcd("add", url)
        if r.returncode != 0:
            raise unittest.SkipTest(f"spcd add failed (likely network): {r.stderr}")
        second = subprocess.run([SPCD, "fetch"], capture_output=True,
                                text=True, timeout=30)
        self.assertEqual(second.returncode, 0)
        self.assertNotIn("stack underflow", second.stdout + second.stderr)
        self.assertNotIn("parser:",         second.stdout + second.stderr)
        self.assertIn("up to date",         second.stdout + second.stderr)
        self.assertIn(url, Path("lock.sp").read_text())

    # -- T3b: regression — URLs with '.' must round-trip through lock.sp
    def test_fetch_idempotent_dotted_url(self):
        url = str(FIXDIR / "site.example.com.git")
        self.spcd("add", url)
        first = self.spcd("fetch")
        self.assertEqual(first.returncode, 0)
        self.assertNotIn("stack underflow", first.stdout + first.stderr)
        self.assertNotIn("parser:",         first.stdout + first.stderr)
        # Second fetch eval'd lock.sp — must not choke on the dotted URL.
        second = self.spcd("fetch")
        self.assertEqual(second.returncode, 0)
        self.assertNotIn("stack underflow", second.stdout + second.stderr)
        self.assertNotIn("parser:",         second.stdout + second.stderr)
        self.assertIn("up to date",         second.stdout + second.stderr)
        self.assertIn(url, Path("lock.sp").read_text())

    # -- T4
    def test_clean(self):
        self.spcd("add", str(FIXDIR / "test-repo.git"))
        self.assertTrue(Path("spcd_lib").is_dir())
        self.spcd("clean")
        self.assertFalse(Path("spcd_lib").exists())

    # -- T5
    def test_add_branch_heuristic(self):
        self.spcd("add", f"{FIXDIR}/test-repo.git@feature")
        self.assertIn("deps/branch", Path("deps.sp").read_text())
        self.assertIn("54c049cc2f66285604f32e8f75ec744f777465b0",
                      Path("lock.sp").read_text())

    # -- T6
    def test_add_sha_heuristic(self):
        sha = "0c7e6d3d9794a92ede8dfa53040de810dd3f6e7a"
        self.spcd("add", f"{FIXDIR}/test-repo.git@{sha}")
        self.assertIn("deps/sha", Path("deps.sp").read_text())
        self.assertIn(sha, Path("lock.sp").read_text())

    # -- T7
    def test_transitive_deps(self):
        self.spcd("add", str(FIXDIR / "dep-b.git"))
        self.assertTrue(Path("spcd_lib/dep-b").is_dir(), "dep-b not cloned")
        self.assertTrue(Path("spcd_lib/dep-a").is_dir(), "dep-a not cloned transitively")
        lock = Path("lock.sp").read_text()
        self.assertIn("dep-b.git", lock)
        self.assertIn("dep-a.git", lock)

    # -- T8
    def test_update_rewrites_lock(self):
        self.spcd("add", str(FIXDIR / "test-repo.git"))
        Path("lock.sp").write_text("/tmp/fake fakesha\n")
        self.spcd("update")
        self.assertIn("0c7e6d3d9794a92ede8dfa53040de810dd3f6e7a",
                      Path("lock.sp").read_text())

    # -- T9
    def test_install_builds_and_runs(self):
        bindir = Path(self.tmp) / "bin"
        env = {
            "SPACELANG_BIN":  str(bindir),
            "SPACELANG_ROOT": str(ROOT),
            "PATH":           f"{ROOT}/bin:{os.environ.get('PATH', '')}",
        }
        self.spcd("install", str(FIXDIR / "binrepo.git"), env=env)
        binary = bindir / "binrepo"
        self.assertTrue(binary.is_file() and os.access(binary, os.X_OK),
                        "installed binary missing or not executable")
        r = subprocess.run([str(binary)], capture_output=True, text=True, timeout=4)
        self.assertIn("hello from binrepo", r.stdout + r.stderr)


# ── mesh tests: spco + spci nodes over Unix sockets ──────────────────

class TestMesh(TimedTestCase):
    """End-to-end mesh: spco (filesystem discovery) + spci peers."""

    def setUp(self):
        cleanup_bus()
        self.spco_proc = None
        self.workers = []

    def tearDown(self):
        for p in self.workers:
            try: p.kill()
            except: pass
        if self.spco_proc:
            try: self.spco_proc.kill()
            except: pass
        time.io/sleep(0.15)
        cleanup_bus()

    def start_spco(self):
        """Start spco (compiled from spco.sp). Serves on $BUS/spco.sock."""
        env = {**os.environ, "PATH": f"{ROOT}/bin:{os.environ.get('PATH', '')}"}
        self.spco_proc = subprocess.Popen(
            [SPCO, "--bus", BUS, "--serve"],
            stderr=subprocess.PIPE, text=True, env=env
        )
        deadline = time.time() + 3
        while time.time() < deadline:
            if os.path.exists(f"{BUS}/spco.sock"):
                return
            time.io/sleep(0.05)
        self.fail("spco didn't bind spco.sock")

    def start_worker(self, name, script=""):
        """Start a spci mesh node with --serve, wait for its socket."""
        d = tempfile.mkdtemp()
        (Path(d) / "w.sp").write_text(script)
        self.addCleanup(lambda: shutil.rmtree(d, ignore_errors=True))
        p = subprocess.Popen(
            [SPCI, "--name", name, "--bus", BUS, "--serve", str(Path(d) / "w.sp")],
            stderr=subprocess.PIPE, text=True
        )
        self.workers.append(p)
        deadline = time.time() + 2
        while time.time() < deadline:
            if os.path.exists(f"{BUS}/{name}.sock"):
                return
            time.io/sleep(0.05)
        self.fail(f"{name} did not bind")

    def test_spco_starts_and_binds(self):
        """spco binary built from spco.sp starts and binds its socket."""
        self.start_spco()
        self.assertTrue(os.path.exists(f"{BUS}/spco.sock"))

    def test_spco_spawns_via_eval(self):
        """A peer sends EVAL to spco asking it to spawn-node "X";
        spco runs spawn-node which shells out an spci for X."""
        self.start_spco()
        # Driver sends `"X" spawn-node !` to spco via $!
        # spco receives EVAL, feeds payload, spawn-node forks an spci.
        out, err, rc = run_spci(
            stdin='[ "X" spawn-node ! ] "spco" $!  500 io/sleep  bye!\n',
            args=["--name", "DRV", "--bus", BUS],
            timeout=4,
        )
        self.assertEqual(rc, 0, f"spci failed:\n{err}")
        # Give spawn-node's sh/! + io/sleep time to bind X.
        for _ in range(20):
            if os.path.exists(f"{BUS}/X.sock"):
                break
            time.io/sleep(0.1)
        self.assertTrue(os.path.exists(f"{BUS}/X.sock"),
            "spco should have spawned X via spawn-node")
        # Clean up X
        subprocess.run(["pkill", "-f", "name X"], capture_output=True)

    def test_mesh_direct_connect(self):
        """Peer connects directly (socket exists, no spco needed)."""
        self.start_worker("W")
        out, err, rc = run_spci(
            stdin="42 [W] 2000 $? . bye!\n",
            args=["--name", "A", "--bus", BUS],
            timeout=4
        )
        self.assertEqual(rc, 0, f"spci failed:\n{err}")
        self.assertIn("t", out, f"expected ACK, got:\n{out}\n{err}")

    def test_mesh_with_spco_running(self):
        """Direct connect still works fine when spco is also running."""
        self.start_worker("W")
        self.start_spco()
        out, err, rc = run_spci(
            stdin="42 [W] 2000 $? . bye!\n",
            args=["--name", "A", "--bus", BUS],
            timeout=4
        )
        self.assertEqual(rc, 0, f"spci failed:\n{err}")
        self.assertIn("t", out, f"expected ACK, got:\n{out}\n{err}")

    def test_spco_respawn_after_death(self):
        """If a peer dies (its socket left stale), spco/$ must respawn it."""
        self.start_spco()
        d = tempfile.mkdtemp()
        self.addCleanup(lambda: shutil.rmtree(d, ignore_errors=True))
        # first call: spawn Z
        p1 = Path(d) / "first.sp"
        p1.write_text(
            f'"{ROOT}/stdlib/with-spco.sp" require\n'
            '"one" [Z] spco/$\n'
        )
        _, err, rc = run_spci(stdin="bye!\n",
            args=["--name", "C1", "--bus", BUS, str(p1)], timeout=4)
        self.assertEqual(rc, 0, err)
        time.io/sleep(0.4)
        self.assertTrue(os.path.exists(f"{BUS}/Z.sock"))

        # kill Z hard, leave stale socket on disk
        subprocess.run(["pkill", "-9", "-f", "name Z"], capture_output=True)
        time.io/sleep(0.3)
        # touch the file to ensure it stays (SIGKILL may have left it anyway)
        Path(f"{BUS}/Z.sock").touch(exist_ok=True)

        # second call: spco/$ should detect Z is dead (sp/alive? false) and respawn
        p2 = Path(d) / "second.sp"
        p2.write_text(
            f'"{ROOT}/stdlib/with-spco.sp" require\n'
            '"two" [Z] spco/$\n'
        )
        _, err, rc = run_spci(stdin="bye!\n",
            args=["--name", "C2", "--bus", BUS, str(p2)], timeout=4)
        self.assertEqual(rc, 0, err)
        time.io/sleep(0.5)
        # Real test: connect to Z.sock and confirm it accepts
        try:
            s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            s.settimeout(1.0)
            s.connect(f"{BUS}/Z.sock")
            s.close()
        except OSError as e:
            self.fail(f"Z.sock is not connectable after respawn: {e}")
        subprocess.run(["pkill", "-f", "name Z"], capture_output=True)

    def test_spco_eval_variant(self):
        """spco/$! ensures peer is up, then sends EVAL."""
        self.start_spco()
        d = tempfile.mkdtemp()
        self.addCleanup(lambda: shutil.rmtree(d, ignore_errors=True))
        prog = Path(d) / "drv.sp"
        prog.write_text(
            f'"{ROOT}/stdlib/with-spco.sp" require\n'
            '[ 21 21 + ] [W2] spco/$!\n'
        )
        _, err, rc = run_spci(stdin="bye!\n",
            args=["--name", "C", "--bus", BUS, str(prog)], timeout=4)
        self.assertEqual(rc, 0, err)
        time.io/sleep(0.5)
        self.assertTrue(os.path.exists(f"{BUS}/W2.sock"),
            "W2 should have been spawned via spco/$!")
        subprocess.run(["pkill", "-f", "name W2"], capture_output=True)

    def test_via_spco_helper(self):
        """with-spco.sp gives callers a `via-spco` word that asks spco
        to ensure a peer is up, then sends a message direct."""
        self.start_spco()
        # Run a driver that loads with-spco.sp and sends to "Y".
        d = tempfile.mkdtemp()
        self.addCleanup(lambda: shutil.rmtree(d, ignore_errors=True))
        prog = Path(d) / "drv.sp"
        prog.write_text(
            f'"{ROOT}/stdlib/with-spco.sp" require\n'
            '"hi" [Y] via-spco\n'
        )
        out, err, rc = run_spci(
            stdin="bye!\n",
            args=["--name", "DRV2", "--bus", BUS, str(prog)],
            timeout=4,
        )
        self.assertEqual(rc, 0, f"spci failed:\n{err}")
        # Y should now be bound (spawn-node ran via spco)
        for _ in range(20):
            if os.path.exists(f"{BUS}/Y.sock"):
                break
            time.io/sleep(0.1)
        self.assertTrue(os.path.exists(f"{BUS}/Y.sock"),
            "Y should have been spawned via via-spco")
        subprocess.run(["pkill", "-f", "name Y"], capture_output=True)

    def test_mesh_eval_remote(self):
        """Driver evaluates a thunk on a worker via $!.

        Note: $! output goes to the remote worker's stdout, not the
        driver's. We just verify the driver doesn't crash.
        """
        self.start_worker("W", "[ 2 * ] [double] @\n")
        out, err, rc = run_spci(
            stdin="[ 21 double ! ] [W] $!  bye!\n",
            args=["--name", "A", "--bus", BUS],
            timeout=4
        )
        self.assertEqual(rc, 0, f"spci failed:\n{err}")


# ── main ──────────────────────────────────────────────────────────────

if __name__ == "__main__":
    import argparse
    ap = argparse.ArgumentParser()
    ap.add_argument("--quick", action="store_true", help="skip mesh tests")
    ap.add_argument("--seed", type=int, help="RNG seed for property tests")
    ap.add_argument("tests", nargs="*", help="specific test classes/methods")
    args = ap.parse_args()

    if not all(os.path.exists(p) for p in [SPCI, SPCC, SPCO]):
        print("Run 'make c' first — binaries not found.", file=sys.stderr)
        sys.exit(1)

    if args.seed is not None:
        os.environ["SPACELANG_TEST_SEED"] = str(args.seed)

    loader = unittest.TestLoader()
    suite = unittest.TestSuite()

    if args.quick:
        suite.addTests(loader.loadTestsFromTestCase(TestEval))
        suite.addTests(loader.loadTestsFromTestCase(TestStr))
        suite.addTests(loader.loadTestsFromTestCase(TestCompile))
        suite.addTests(loader.loadTestsFromTestCase(TestRequirePreprocess))
        suite.addTests(loader.loadTestsFromTestCase(TestSpct))
        suite.addTests(loader.loadTestsFromTestCase(TestProperty))
        suite.addTests(loader.loadTestsFromTestCase(TestSpcd))
        suite.addTests(loader.loadTestsFromTestCase(TestSpcdbg))
    elif args.tests:
        for name in args.tests:
            suite.addTests(loader.loadTestsFromName(name))
    else:
        suite.addTests(loader.loadTestsFromTestCase(TestEval))
        suite.addTests(loader.loadTestsFromTestCase(TestStr))
        suite.addTests(loader.loadTestsFromTestCase(TestCompile))
        suite.addTests(loader.loadTestsFromTestCase(TestRequirePreprocess))
        suite.addTests(loader.loadTestsFromTestCase(TestSpct))
        suite.addTests(loader.loadTestsFromTestCase(TestProperty))
        suite.addTests(loader.loadTestsFromTestCase(TestSpcd))
        suite.addTests(loader.loadTestsFromTestCase(TestSpcdbg))
        suite.addTests(loader.loadTestsFromTestCase(TestMesh))

    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    sys.exit(0 if result.wasSuccessful() else 1)
