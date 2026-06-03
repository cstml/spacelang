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
SPCI  = str(ROOT / "spci")
SPCC  = str(ROOT / "spcc")
SPCO  = str(ROOT / "spco")
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
    time.sleep(0.15)
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
        out, err, rc = run_spci(stdin=code + "\n:bye\n")
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
        self.assertIn("0", out)  # false → 0, not nil

    def test_stack_ops(self):
        out = self.eval("1 2 swap . .")
        self.assertIn("1", out)
        self.assertIn("2", out)
        out = self.eval("5 dup + .")
        self.assertIn("10", out)
        out = self.eval("99 drop")
        self.assertNotIn("99", out)

    def test_if(self):
        # if { else then cond → (cond ? then : else) }
        # [else] [then] cond if  →  pushes then if cond truthy
        out = self.eval("[else] [then] true if .")
        self.assertIn("[then]", out)
        out = self.eval("[else] [then] false if .")
        self.assertIn("[else]", out)

    def test_bind_and_eval(self):
        out = self.eval("[ 2 * ] [double] @  21 double ! .")
        self.assertIn("42", out)

    def test_nested_thunks(self):
        out = self.eval("[ 1 2 + ] ! .")
        self.assertIn("3", out)

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
        out = self.eval("1 2 3 :s")
        self.assertIn("-- stack (3) --", out)
        self.assertIn("1", out)
        self.assertIn("2", out)
        self.assertIn("3", out)

    def test_slurp(self):
        # slurp returns a string, so use eval to convert to number
        import tempfile
        with tempfile.NamedTemporaryFile(mode="w", suffix=".sp", delete=False) as f:
            f.write("slurp eval 1 + .\n")  # slurp string, eval→num, add 1
            tmp = f.name
        try:
            out, err, rc = run_spci(stdin="41\n", args=[tmp], timeout=3)
            self.assertEqual(rc, 0, f"spci failed: {err}")
            self.assertIn("42", out)
        finally:
            os.unlink(tmp)

    def test_cat(self):
        out = self.eval('"foo" "bar" cat .')
        self.assertIn('"foobar"', out)

    def test_sleep(self):
        t0 = time.time()
        self.eval("200 :sleep")
        self.assertGreaterEqual(time.time() - t0, 0.18)

    def test_sh(self):
        out = self.eval('"echo hello-from-sh" :sh .')
        self.assertIn("hello-from-sh", out)
        self.assertIn("0", out)  # exit status

    def test_require(self):
        import tempfile as _tf
        with _tf.NamedTemporaryFile(mode="w", suffix=".sp", delete=False) as f:
            f.write("[ 7 ] [seven] @\n")
            lib = f.name
        try:
            out = self.eval(f'"{lib}" :require seven ! .')
            self.assertIn("7", out)
        finally:
            os.unlink(lib)

    def test_exists_false(self):
        # Without --name/--bus, bus_dir is NULL → :exists is always false
        out = self.eval('"NoSuchPeer" :exists .')
        self.assertIn("0", out)

    def test_name_str_roundtrip(self):
        # name>str pulls the name out of a [X] form
        out = self.eval('[foo] name>str .')
        self.assertIn('"foo"', out)
        # str>name wraps a string into a [X] form
        out = self.eval('"bar" str>name .')
        self.assertIn('[bar]', out)
        # round-trip preserves the name
        out = self.eval('[baz] name>str str>name name>str .')
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
        self.compile_and_compare("[yes] [no] true if .", "")

    def test_example_add2(self):
        # add_2.sp is interactive (slurp slurp +) — requires stdin typing.
        # Skip compilation test for interactive examples.
        self.skipTest("add_2.sp is interactive, needs stdin typing")

    def test_example_fib(self):
        # fibonacci.sp reads count from stdin via slurp eval
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


# ── :require preprocessor (spcc inlines required files) ──────────────

class TestRequirePreprocess(TimedTestCase):
    """spcc statically inlines `"path" :require` so compiled binaries
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
        """`:require`d file is embedded — compiled binary has no fs dep."""
        self.write("lib.sp", "[ 7 ] [seven] @\n")
        main = self.write("main.sp", '"lib.sp" :require seven .\n')
        bin_path = str(Path(self.tmp) / "out")
        r = spcc_compile(main, bin_path)
        self.assertEqual(r.returncode, 0, r.stderr)
        # Move the binary AWAY from lib.sp and confirm it still works.
        elsewhere = tempfile.mkdtemp()
        try:
            moved = Path(elsewhere) / "out"
            shutil.move(bin_path, moved)
            p = subprocess.run([str(moved)], capture_output=True, text=True, timeout=3)
            self.assertEqual(p.returncode, 0, p.stderr)
            self.assertIn("7", p.stdout)
        finally:
            shutil.rmtree(elsewhere, ignore_errors=True)

    def test_recursive_require(self):
        """A required file may itself :require another."""
        self.write("a.sp", "[ 1 ] [one] @\n")
        self.write("b.sp", '"a.sp" :require [ one 1 + ] [two] @\n')
        main = self.write("main.sp", '"b.sp" :require two .\n')
        bin_path = str(Path(self.tmp) / "out")
        r = spcc_compile(main, bin_path)
        self.assertEqual(r.returncode, 0, r.stderr)
        p = subprocess.run([bin_path], capture_output=True, text=True, timeout=3)
        self.assertIn("2", p.stdout)

    def test_cycle_safe(self):
        """Mutual requires don't infinite-loop; each file loaded once."""
        self.write("a.sp", '"b.sp" :require [ 1 ] [a-marker] @\n')
        self.write("b.sp", '"a.sp" :require [ 2 ] [b-marker] @\n')
        main = self.write("main.sp", '"a.sp" :require a-marker . b-marker .\n')
        bin_path = str(Path(self.tmp) / "out")
        r = spcc_compile(main, bin_path)
        self.assertEqual(r.returncode, 0, r.stderr)
        p = subprocess.run([bin_path], capture_output=True, text=True, timeout=3)
        self.assertIn("1", p.stdout)
        self.assertIn("2", p.stdout)

    def test_error_non_string_operand(self):
        """:require with a non-string operand is rejected at compile time
        with file:line context."""
        main = self.write("main.sp",
            "{ comment }\n"
            "1 2 + :require\n")        # line 2 has the bad :require
        bin_path = str(Path(self.tmp) / "out")
        r = spcc_compile(main, bin_path)
        self.assertNotEqual(r.returncode, 0)
        self.assertIn(":require", r.stderr)
        self.assertIn("string literal", r.stderr)
        self.assertIn(f"{main}:2", r.stderr)

    def test_error_missing_file(self):
        """Missing :require target is rejected at compile time with
        the file:line of the offending string literal."""
        main = self.write("main.sp",
            "{ a header }\n"
            "{ another }\n"
            '"nope-does-not-exist.sp" :require\n')   # line 3
        bin_path = str(Path(self.tmp) / "out")
        r = spcc_compile(main, bin_path)
        self.assertNotEqual(r.returncode, 0)
        self.assertIn(":require", r.stderr)
        self.assertIn("nope-does-not-exist.sp", r.stderr)
        self.assertIn(f"{main}:3", r.stderr)


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
    ":s",
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
            code = " ".join(seq) + " :bye"
            out, err, rc = run_spci(stdin=code, timeout=4)
            if rc != 0 and "stack underflow" not in err and "expected number" not in err:
                failures.append((seq, err))

        if failures:
            msg = f"[seed={seed}] {len(failures)}/{200} sequences had unexpected crashes:\n"
            for seq, err in failures[:5]:
                msg += f"  {' '.join(seq)}\n  → {err.strip()}\n"
            self.fail(msg)


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
        time.sleep(0.15)
        cleanup_bus()

    def start_spco(self):
        """Start spco (compiled from spco.sp). Serves on $BUS/spco.sock."""
        self.spco_proc = subprocess.Popen(
            [SPCO, "--bus", BUS, "--serve"],
            stderr=subprocess.PIPE, text=True
        )
        deadline = time.time() + 3
        while time.time() < deadline:
            if os.path.exists(f"{BUS}/spco.sock"):
                return
            time.sleep(0.05)
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
            time.sleep(0.05)
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
            stdin='[ "X" spawn-node ! ] "spco" $!  500 :sleep  :bye\n',
            args=["--name", "DRV", "--bus", BUS],
            timeout=4,
        )
        self.assertEqual(rc, 0, f"spci failed:\n{err}")
        # Give spawn-node's :sh + :sleep time to bind X.
        for _ in range(20):
            if os.path.exists(f"{BUS}/X.sock"):
                break
            time.sleep(0.1)
        self.assertTrue(os.path.exists(f"{BUS}/X.sock"),
            "spco should have spawned X via spawn-node")
        # Clean up X
        subprocess.run(["pkill", "-f", "name X"], capture_output=True)

    def test_mesh_direct_connect(self):
        """Peer connects directly (socket exists, no spco needed)."""
        self.start_worker("W")
        out, err, rc = run_spci(
            stdin="42 [W] 2000 $? . :bye\n",
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
            stdin="42 [W] 2000 $? . :bye\n",
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
            f'"{ROOT}/with-spco.sp" :require\n'
            '"one" [Z] spco/$\n'
        )
        _, err, rc = run_spci(stdin=":bye\n",
            args=["--name", "C1", "--bus", BUS, str(p1)], timeout=4)
        self.assertEqual(rc, 0, err)
        time.sleep(0.4)
        self.assertTrue(os.path.exists(f"{BUS}/Z.sock"))

        # kill Z hard, leave stale socket on disk
        subprocess.run(["pkill", "-9", "-f", "name Z"], capture_output=True)
        time.sleep(0.3)
        # touch the file to ensure it stays (SIGKILL may have left it anyway)
        Path(f"{BUS}/Z.sock").touch(exist_ok=True)

        # second call: spco/$ should detect Z is dead (:alive false) and respawn
        p2 = Path(d) / "second.sp"
        p2.write_text(
            f'"{ROOT}/with-spco.sp" :require\n'
            '"two" [Z] spco/$\n'
        )
        _, err, rc = run_spci(stdin=":bye\n",
            args=["--name", "C2", "--bus", BUS, str(p2)], timeout=4)
        self.assertEqual(rc, 0, err)
        time.sleep(0.5)
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
            f'"{ROOT}/with-spco.sp" :require\n'
            '[ 21 21 + ] [W2] spco/$!\n'
        )
        _, err, rc = run_spci(stdin=":bye\n",
            args=["--name", "C", "--bus", BUS, str(prog)], timeout=4)
        self.assertEqual(rc, 0, err)
        time.sleep(0.5)
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
            f'"{ROOT}/with-spco.sp" :require\n'
            '"hi" [Y] via-spco\n'
        )
        out, err, rc = run_spci(
            stdin=":bye\n",
            args=["--name", "DRV2", "--bus", BUS, str(prog)],
            timeout=4,
        )
        self.assertEqual(rc, 0, f"spci failed:\n{err}")
        # Y should now be bound (spawn-node ran via spco)
        for _ in range(20):
            if os.path.exists(f"{BUS}/Y.sock"):
                break
            time.sleep(0.1)
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
            stdin="[ 21 double ! ] [W] $!  :bye\n",
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
        suite.addTests(loader.loadTestsFromTestCase(TestCompile))
        suite.addTests(loader.loadTestsFromTestCase(TestRequirePreprocess))
        suite.addTests(loader.loadTestsFromTestCase(TestProperty))
    elif args.tests:
        for name in args.tests:
            suite.addTests(loader.loadTestsFromName(name))
    else:
        suite.addTests(loader.loadTestsFromTestCase(TestEval))
        suite.addTests(loader.loadTestsFromTestCase(TestCompile))
        suite.addTests(loader.loadTestsFromTestCase(TestRequirePreprocess))
        suite.addTests(loader.loadTestsFromTestCase(TestProperty))
        suite.addTests(loader.loadTestsFromTestCase(TestMesh))

    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    sys.exit(0 if result.wasSuccessful() else 1)
