# spacelang in C

A C implementation of spacelang: a concatenative, stack-based language with
mesh transport. Ships as three binaries plus a runtime library:

- **`spci`** — interactive interpreter (REPL + file mode + mesh node).
- **`spcc`** — compiler: turns a `.sp` file into a standalone native binary.
- **`spco`** — orchestrator: name-resolution broker, lazy-spawns peers on demand.
- **`libspci.a`** + **`spci.h`** — the runtime, linked into programs `spcc` produces.

## Build

```sh
make c           # builds spci, spcc, spco, libspci.a
make c-clean
```

Requires a C compiler (`cc`) and `ar`. No other deps.

## Files

| File           | Role                                                                          |
|----------------|-------------------------------------------------------------------------------|
| `spci.h`       | Public surface of the runtime: mesh state, peer table, `feed()`, mesh I/O.    |
| `spci.c`       | Definitions: values, stack, dict, parser, builtins, frame I/O, mesh polling.  |
| `spci_main.c`  | Driver for the interactive `spci` binary: argv parsing + REPL/event loop.     |
| `spcc.c`       | The compiler: emits a small `.c` and links it against `libspci.a`.            |
| `spco.c`       | The orchestrator: LOOKUP/ADDR broker, lazy-spawns children, crash backoff.    |
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
./c/spci                      # REPL
./c/spci program.sp           # batch mode (run file, exit)
./c/spci --name A --bus /tmp/spacelang             # mesh node, REPL on stdin
./c/spci --name A --bus /tmp/spacelang program.sp  # mesh node, run file, exit
./c/spci --name A --bus /tmp/spacelang --serve program.sp  # run file, stay alive
```

In mesh mode, `spci` binds `$BUS/<name>.sock` and lazily connects to peers
at `$BUS/<peer>.sock` on first send. Without `--serve`, the process exits
after the script finishes (same as compiled binaries). With `--serve`, it
keeps serving mesh requests. See `../docs/inter-machine.html` for
protocol details.

## Compiling with `spcc`

```sh
./c/spcc program.sp -o program           # compile to ./program
./c/spcc program.sp                      # default output: same name minus .sp
./c/spcc --emit-c program.sp             # print generated C to stdout
./c/spcc --keep-c program.sp -o program  # also write program.c
./c/spcc --debug program.sp -o program   # -g -O0, implies --keep-c (for gdb)
./c/spcc --cc gcc program.sp -o program  # override compiler (default: $CC or cc)
./c/spcc --root DIR program.sp -o program  # override runtime lookup
```

### How `spcc` finds the runtime

`spcc` needs `spci.h` + `libspci.a` to link your program against. It looks
for them in:

1. `$SPACELANG_ROOT` if set
2. The directory of the `spcc` binary itself (via `/proc/self/exe`)

The first directory containing **both** files wins. In the dev tree they
sit next to `spcc` under `c/`, so no env var is needed.

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
./c/spcc --debug program.sp -o program     # produces ./program + ./program.c with -g
gdb ./program
(gdb) break feed       # entry to the interpreter loop
(gdb) break eval_word  # per-token dispatch
(gdb) break mesh_poll  # mesh I/O
(gdb) run
```

Stepping into the runtime works because `program.c` `#include`s `spci.h`
and the linker uses `libspci.a` built from `spci.c`; line numbers map
back to the runtime source.

## Mesh orchestration with `spco`

`spco` is a name-resolution broker. It listens on `$BUS/spco.sock` and
handles LOOKUP requests from peers that can't find a direct socket.

### How it works

1. A sender tries `$BUS/<peer>.sock` directly.
2. If that fails, it sends `LOOKUP "peer"` to `spco`.
3. `spco` spawns the peer (if not already running) and returns `ADDR /path/to/peer.sock`.
4. The sender connects directly to the peer — `spco` stays out of the data path.

### CLI

```sh
spco [-v|--verbose] [--bus DIR] NAME=CMD ...
```

Each `NAME=CMD` declares a peer: `NAME` is the mesh identity, `CMD` is the
command to spawn (split on whitespace). `spco` appends `--name NAME --bus DIR`
and sets `SPACELANG_NAME`/`SPACELANG_BUS` in the child's environment.

### Examples

```sh
# Two workers defined by source files (need --serve to stay alive)
./c/spco -v --bus /tmp/spacelang A='./c/spci --serve a.sp' B='./c/spci --serve b.sp'

# Using compiled binaries with --serve
./c/spco --bus /tmp/spacelang \
  A='./a_compiled --serve' \
  B='./b_compiled --serve'
```

### Child lifecycle

- **Lazy spawn** — children start on first LOOKUP, not at boot.
- **Crash backoff** — on exit, restart on next LOOKUP after exponential
  backoff (100ms → 200ms → 400ms … capped at 5s).
- **Graceful shutdown** — on SIGINT/SIGTERM, `spco` sends SIGTERM to all
  children, waits 100ms, then unlinks sockets and exits.

### Verbose mode

`-v` or `--verbose` prints a per-request trace:

```
[spco] listening on /tmp/spacelang/spco.sock, 2 entries
[spco]   A → ./c/spci
[spco]   B → ./c/spci
[spco] client connected
[spco] LOOKUP "A"
[spco] lazy-spawning A
[spco] spawn A pid=12345 cmd=./c/spci
[spco] A bound at /tmp/spacelang/A.sock
[spco] → ADDR /tmp/spacelang/A.sock
```

Without `-v`, only essential messages (startup, errors, shutdown) are shown.

### End-to-end example

```sh
# Terminal 1: start the orchestrator
./c/spco -v --bus /tmp/spacelang W='./c/spci --bus /tmp/spacelang worker.sp'

# Terminal 2: send to W — spco spawns it on demand
./c/spci --name DRIVER --bus /tmp/spacelang
> 42 [W] 2000 $? .
t
```

The design rationale is in
`../docs/superpowers/specs/2026-06-02-spcc-c-compiler-design.md`.
