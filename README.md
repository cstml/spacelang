# spacelang

A concatenative, stack-based language with mesh transport.
Written in C. Ships as three binaries plus a runtime library:

- **`spci`** — interactive interpreter (REPL + file mode + mesh node).
- **`spcc`** — compiler: turns a `.sp` file into a standalone native binary.
- **`spco`** — discovery broker: answers LOOKUP by checking `$BUS/<name>.sock` on disk.
- **`libspci.a`** + **`spci.h`** — the runtime, linked into programs `spcc` produces.

## Build

```sh
make              # builds spci, spcc, spco, libspci.a
make clean        # remove build artifacts
make test         # run test suite
make test-quick   # unit + compile + property tests only
```

Requires a C compiler (`cc`) and `ar`. Tests require Python 3.

## Files

| File           | Role                                                                          |
|----------------|-------------------------------------------------------------------------------|
| `spci.h`       | Public surface of the runtime: mesh state, peer table, `feed()`, mesh I/O.    |
| `spci.c`       | Definitions: values, stack, dict, parser, builtins, frame I/O, mesh polling.  |
| `spci_main.c`  | Driver for the interactive `spci` binary: argv parsing + REPL/event loop.     |
| `spcc.c`       | The compiler: emits a small `.c` and links it against `libspci.a`.            |
| `spco.c`       | Discovery broker: answers LOOKUP by checking the filesystem for .sock files.  |
| `test_harness.py` | Test suite: unit, compile, property, and mesh integration tests.            |
| `libspci.a`    | Archive of `spci.o`; what `spcc`-produced binaries link against.              |

## Language reference

### Values

Numbers (`42`, `-7`), strings (`"hi"` or `'hi'`), words (`foo`), booleans
(`true`, `false`, `nil`), and thunks (`[ ... ]`). Comments are
`{ like this }`.

### Builtins

**Arithmetic / comparison** — pop two, push result:
```
+  -  *  /         { numeric }
<  >  <=  >=  =    { → t / nil }
```

**Stack:**
```
dup   { x → x x }
swap  { x y → y x }
drop  { x → }
```

**Control:**
```
if    { else then cond → (cond ? then : else) }
```

**Binding & eval:**
```
@     { value [name] → bind name = value }
!     { thunk → evaluate it }
~     { [name] → print "name ~ <value>" }
```

**I/O:**
```
.     { x → print x with newline }
,     { x → format and print (same as . for now) }
slurp { → read a line from stdin, push as string (newline stripped) }
eval  { string → feed it to the interpreter }
```

`slurp eval` is the dynamic-load idiom — read a line of spacelang
source and execute it.

**Mesh (only meaningful with `--name`/`SPACELANG_NAME` set):**
```
$   { value [peer] → send PUSH, fire-and-forget }
$!  { value [peer] → send EVAL, fire-and-forget }
$?  { value [peer] timeout-ms → send + wait; push t on ack, nil on timeout }
```

**Keywords:**
```
:s    { print the stack }
:bye  { exit 0 }
```

### Idioms

```
[ dup + ] [double] @       { bind a function }
21 double ! .              { → 42 }

[ 1 + ] [inc] @
0 inc ! inc ! inc ! .      { → 3 }

slurp eval                 { one REPL step }
[ slurp eval ] [step] @
step ! step ! step !       { read & execute 3 lines from stdin }
```

## Running `spci`

```sh
./spci                      # REPL
./spci program.sp           # batch mode (run file, exit)
./spci --name A --bus /tmp/spacelang             # mesh node, REPL on stdin
./spci --name A --bus /tmp/spacelang program.sp  # mesh node, run file, exit
./spci --name A --bus /tmp/spacelang --serve program.sp  # run file, stay alive
```

In mesh mode, `spci` binds `$BUS/<name>.sock` and lazily connects to peers
at `$BUS/<peer>.sock` on first send. Without `--serve`, the process exits
after the script finishes (same as compiled binaries). With `--serve`, it
keeps serving mesh requests. See `docs/inter-machine.html` for
protocol details.

## Compiling with `spcc`

```sh
./spcc program.sp -o program           # compile to ./program
./spcc program.sp                      # default output: same name minus .sp
./spcc --emit-c program.sp             # print generated C to stdout
./spcc --keep-c program.sp -o program  # also write program.c
./spcc --debug program.sp -o program   # -g -O0, implies --keep-c (for gdb)
./spcc --cc gcc program.sp -o program  # override compiler (default: $CC or cc)
./spcc --root DIR program.sp -o program  # override runtime lookup
```

### How `spcc` finds the runtime

`spcc` needs `spci.h` + `libspci.a` to link your program against. It looks
for them in:

1. `$SPACELANG_ROOT` if set
2. The directory of the `spcc` binary itself (via `/proc/self/exe`)

The first directory containing **both** files wins. In the dev tree they
sit in the project root alongside `spcc`, so no env var is needed.

### Generated binaries

The compiled program is fully self-contained — the runtime is statically
linked in. It honors the same mesh contract as `spci`:

```sh
./program                                # single-process, run and exit
SPACELANG_NAME=A ./program               # mesh node, run program, exit
SPACELANG_NAME=A ./program --serve       # mesh node, run program, stay alive
./program --name A --bus /tmp/spacelang  # equivalent without env
```

The `--serve` flag is what you want when the program is a service (other
peers will send to it after `feed()` returns). Without it, the binary
exits after the script finishes.

## Distribution shape

Conceptually like `gcc` + `libgcc.a` + headers, or Go's `$GOROOT`:

```
<install-dir>/
    spcc           # the compiler (must be in PATH or invoked directly)
    spci.h         # runtime header
    libspci.a      # runtime archive
    spci           # interactive interpreter (independent of spcc)
```

To install elsewhere, copy those four files into a single directory and
either run `spcc` from there or set `SPACELANG_ROOT` to point at it. The
binaries `spcc` produces don't need any of this — they're standalone.

## Debugging compiled programs

```sh
./spcc --debug program.sp -o program     # produces ./program + ./program.c with -g
gdb ./program
(gdb) break feed       # entry to the interpreter loop
(gdb) break eval_word  # per-token dispatch
(gdb) break mesh_poll  # mesh I/O
(gdb) run
```

Stepping into the runtime works because `program.c` `#include`s `spci.h`
and the linker uses `libspci.a` built from `spci.c`; line numbers map
back to the runtime source.

## Testing

```sh
make
make test              # all tests (unit + compile + mesh + property)
make test-quick        # skip mesh tests (faster)
python3 test_harness.py --seed 42    # repeatable property test seed
```

Four test classes:

| Class           | What it covers                                    |
|-----------------|---------------------------------------------------|
| `TestEval`      | Language semantics via spci REPL                  |
| `TestCompile`   | spcc binary matches spci output byte-for-byte     |
| `TestProperty`  | 200 random balanced programs → no segfaults       |
| `TestMesh`      | spco + spci nodes over real Unix sockets          |

## Mesh discovery with `spco`

`spco` is a stateless discovery broker. It answers LOOKUP by checking
whether `$BUS/<name>.sock` exists on the filesystem. No spawning, no
lifecycle, no child management — just filesystem checks.

### How it works

1. A sender tries `$BUS/<peer>.sock` directly.
2. If that fails, it sends `LOOKUP "peer"` to `spco`.
3. `spco` does `stat($BUS/peer.sock)` and returns `ADDR` if it exists.
4. The sender connects directly to the peer — `spco` stays out of the data path.

### CLI

```sh
./spco [--bus DIR]
```

No other arguments. Peers are started independently (by hand, systemd, docker,
etc.) and bind their sockets under the bus directory.

### End-to-end example

```sh
# Terminal 1: start the discovery broker
./spco --bus /tmp/spacelang

# Terminal 2: start worker W (could be on another machine via socat forwarding)
./spci --name W --bus /tmp/spacelang --serve worker.sp

# Terminal 3: driver sends to W
./spci --name DRIVER --bus /tmp/spacelang
> 42 [W] 2000 $? .
t
```

The design rationale is in
`docs/superpowers/specs/2026-06-02-spcc-c-compiler-design.md`.
