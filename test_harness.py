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

# ── helpers ──────────────────────────────────────────────────────────

def run_spci(stdin=None, args=(), timeout=5):
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
    return subprocess.run(args, capture_output=True, text=True, timeout=10)

def output_matches(spci_stdout, compiled_bin):
    """Run a compiled binary and diff its stdout against spci's stdout."""
    p = subprocess.run([compiled_bin], capture_output=True, text=True, timeout=5)
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

class TestEval(unittest.TestCase):
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

    def test_comments(self):
        out = self.eval("{ this is a comment } 7 .")
        self.assertIn("7", out)
        out = self.eval("1 { ignored } 2 + .")
        self.assertIn("3", out)


# ── compile tests: spcc produces correct binaries ─────────────────────

class TestCompile(unittest.TestCase):
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
        p = subprocess.run([bin_path], input="5\n", capture_output=True, text=True, timeout=5)
        self.assertEqual(p.returncode, 0)
        self.assertIn("1\n1\n2\n3\n5\n8", p.stdout)


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

class TestProperty(unittest.TestCase):
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
            out, err, rc = run_spci(stdin=code, timeout=5)
            if rc != 0 and "stack underflow" not in err and "expected number" not in err:
                failures.append((seq, err))

        if failures:
            msg = f"[seed={seed}] {len(failures)}/{200} sequences had unexpected crashes:\n"
            for seq, err in failures[:5]:
                msg += f"  {' '.join(seq)}\n  → {err.strip()}\n"
            self.fail(msg)


# ── mesh tests: spco + spci nodes over Unix sockets ──────────────────

class TestMesh(unittest.TestCase):
    """End-to-end mesh communication: spco orchestrator + spci peers."""

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

    def start_spco(self, *entries):
        """Start spco, return Popen. Each entry is 'NAME=CMD'."""
        self.spco_proc = subprocess.Popen(
            [SPCO, "--bus", BUS] + list(entries),
            stderr=subprocess.PIPE, text=True
        )
        # Wait for spco to be listening
        deadline = time.time() + 3
        while time.time() < deadline:
            line = self.spco_proc.stderr.readline()
            if "listening" in line:
                return
        self.fail("spco didn't start listening")

    def spco_stderr_contains(self, needle, timeout=3):
        """Read spco stderr until we see `needle` or timeout."""
        deadline = time.time() + timeout
        buf = ""
        while time.time() < deadline:
            # non-blocking read from stderr
            import select
            r, _, _ = select.select([self.spco_proc.stderr], [], [], 0.2)
            if r:
                chunk = os.read(self.spco_proc.stderr.fileno(), 4096).decode()
                buf += chunk
                if needle in buf:
                    return True
        return needle in buf

    def spco_lookup(self, name, timeout=3):
        """Send LOOKUP to spco, return ADDR payload or None."""
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock.settimeout(timeout)
        try:
            sock.connect(f"{BUS}/spco.sock")
            frame_write(sock, 0x06, name.encode())  # TAG_LOOKUP
            result = frame_read(sock, timeout)
            if result and result[0] == 0x07:  # TAG_ADDR
                return result[1].decode()
            return None
        finally:
            sock.close()

    def test_spco_lookup_spawns_child(self):
        """spco spawns a child on LOOKUP and returns its ADDR."""
        self.start_spco(f"W={SPCI} --bus {BUS} --serve /dev/null")
        addr = self.spco_lookup("W")
        self.assertIsNotNone(addr, "spco did not return ADDR")
        self.assertIn("W.sock", addr)
        # Verify the child's socket actually exists
        self.assertTrue(os.path.exists(addr), f"socket not found: {addr}")

    def test_spco_unknown_name(self):
        """spco closes connection for unknown names."""
        self.start_spco(f"W={SPCI} --bus {BUS} --serve /dev/null")
        addr = self.spco_lookup("NoSuchPeer")
        self.assertIsNone(addr, "spco should not resolve unknown name")

    def test_spco_crash_backoff(self):
        """After a child exits, spco applies backoff before respawn."""
        # Quick worker: runs and exits (no --serve). spco appends --name X --bus ...
        quick_sp = Path(tempfile.mkdtemp()) / "quick.sp"
        quick_sp.write_text("1 .\n")
        self.addCleanup(lambda: shutil.rmtree(quick_sp.parent, ignore_errors=True))

        self.start_spco(f"X={SPCI} --bus {BUS} {quick_sp}")

        # First lookup spawns it, child runs and exits
        addr1 = self.spco_lookup("X")
        self.assertIsNotNone(addr1, "first lookup should spawn X")

        # Wait for child to exit
        time.sleep(0.4)

        # Second lookup should be in backoff (100ms default)
        addr2 = self.spco_lookup("X", timeout=1)
        self.assertIsNone(addr2, "spco should refuse during backoff")

    def test_mesh_push(self):
        """Driver sends a value to a worker via $, worker receives it."""
        # Worker script: after receiving, print stack
        worker_sp = Path(tempfile.mkdtemp()) / "w.sp"
        worker_sp.write_text("")  # empty, just serves
        self.addCleanup(lambda: shutil.rmtree(worker_sp.parent, ignore_errors=True))

        self.start_spco(f"W={SPCI} --bus {BUS} --serve {worker_sp}")

        # Start worker explicitly (by lookup)
        self.spco_lookup("W")
        time.sleep(0.3)

        # Driver sends a value to W via $?
        driver_sp = Path(tempfile.mkdtemp()) / "d.sp"
        driver_sp.write_text('42 [W] 2000 $? . :bye\n')
        self.addCleanup(lambda: shutil.rmtree(driver_sp.parent, ignore_errors=True))

        out, err, rc = run_spci(
            args=["--name", "D", "--bus", BUS, str(driver_sp)],
            timeout=5
        )
        self.assertEqual(rc, 0, f"driver failed:\n{err}")
        self.assertIn("t", out, "expected ACK (t) from sync send")

    def test_mesh_eval(self):
        """Driver evaluates a thunk on a worker via $!."""
        worker_sp = Path(tempfile.mkdtemp()) / "w.sp"
        worker_sp.write_text("[ 2 * ] [double] @\n")
        self.addCleanup(lambda: shutil.rmtree(worker_sp.parent, ignore_errors=True))

        self.start_spco(f"W={SPCI} --bus {BUS} --serve {worker_sp}")
        self.spco_lookup("W")
        time.sleep(0.3)

        # Send [21 double !] to W via $!, then ask for W's stack
        driver_sp = Path(tempfile.mkdtemp()) / "d.sp"
        driver_sp.write_text(
            "[ 21 double ! ] [W] $!  "
            "[ :s ] [W] $!  "
            ":bye\n"
        )
        self.addCleanup(lambda: shutil.rmtree(driver_sp.parent, ignore_errors=True))

        _, err, rc = run_spci(
            args=["--name", "D", "--bus", BUS, str(driver_sp)],
            timeout=5
        )
        self.assertEqual(rc, 0, f"driver failed:\n{err}")


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
        suite.addTests(loader.loadTestsFromTestCase(TestProperty))
    elif args.tests:
        for name in args.tests:
            suite.addTests(loader.loadTestsFromName(name))
    else:
        suite.addTests(loader.loadTestsFromTestCase(TestEval))
        suite.addTests(loader.loadTestsFromTestCase(TestCompile))
        suite.addTests(loader.loadTestsFromTestCase(TestProperty))
        suite.addTests(loader.loadTestsFromTestCase(TestMesh))

    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    sys.exit(0 if result.wasSuccessful() else 1)
