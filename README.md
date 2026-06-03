# spaceforth

A small **Forth** with mesh primitives. Stack machine, words, quotations
(`[ ... ]`) — straight Forth lineage; the novel part is that
**identity and peer messaging are baked into the kernel**: any node
can bind a name, address another by `[Name]`, and send values over
Unix domain sockets with `$ / $! / $?`. Discovery is a stateless
broker (`spco`) that just checks the filesystem.

The project name in source is still `spacelang` (env vars, bus dir, `.sp`
extension); only the framing here is updated. Treat "spaceforth" as the
honest description of what it is and "spacelang" as the historical name
used in code paths and tooling.

Written in C. Ships as three binaries plus a runtime library:

- **`spci`** — interactive interpreter (REPL + file mode + mesh node).
- **`spcc`** — compiler: turns a `.sp` source into a standalone native binary.
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

| File              | Role                                                                          |
|-------------------|-------------------------------------------------------------------------------|
| `spci.h`          | Public surface of the runtime: mesh state, peer table, `feed()`, mesh I/O.    |
| `spci.c`          | Definitions: values, stack, dict, parser, builtins, frame I/O, mesh polling.  |
| `spci_main.c`     | Driver for the interactive `spci` binary: argv parsing + REPL/event loop.     |
| `spcc.c`          | The compiler: emits a small `.c` and links it against `libspci.a`.            |
| `spco.sp`         | Discovery broker, written in spaceforth itself and compiled with `spcc`.      |
| `with-spco.sp`    | Helper lib: spco-aware variants of `$ / $! / $?`.                             |
| `str/str.sp`      | String library built on the `str/` C primitives.                              |
| `test_harness.py` | Test suite: unit, compile, property, and mesh integration tests.              |
| `libspci.a`       | Archive of `spci.o`; what `spcc`-produced binaries link against.              |

## Language reference

### Lineage

Standard Forth shape with one common modern extension: **quotations**
(`[ ... ]`), the same construct Factor calls quotations and gforth
spells `[: ... ;]`. Binding form is `value [name] @` (rather than
`: name value ;`). Words bound to thunks **auto-evaluate** on reference
— you don't write `name !` to invoke them, just `name`. `!` is reserved
for explicitly evaluating a thunk that's on the stack.

If you know Forth, the only things you have to learn are the binding
syntax, the auto-eval rule, and the mesh ops.

### Values

Numbers (`42`, `-7`), strings (`"hi"` or `'hi'`), words (`foo`), booleans
(`true`, `false`), and thunks (`[ ... ]`). `nil` is an alias for the
falsy num `0`. Comments are `{ like this }` (non-nesting — a `{` inside
a comment closes it early).

### Builtins

**Arithmetic / comparison** — pop two, push result:
```
+  -  *  /         { numeric }
<  >  <=  >=  =    { → t / 0 }
```

**Stack:**
```
dup   { x → x x }
swap  { x y → y x }
drop  { x → }
rot   { a b c → b c a }
```

**Control:**
```
if    { [else] [then] cond → pushes [then] if cond is truthy, else [else] }
```

After `if` you typically write `!` to evaluate the chosen thunk:
`[else-body] [then-body] cond if !`.

**Binding & eval:**
```
@     { value [name] → bind name = value }
!     { thunk → run it; non-thunk → no-op, leave as-is }
~     { [name] → print "name ~ <value>" }
```

Auto-eval rule: referencing a name bound to a thunk runs the thunk
right away. To push a thunk as data, bind it double-wrapped:
`[ [ body ] ] [name] @`, then `name` pushes `[ body ]`.

**I/O:**
```
.     { x → print x with newline }
,     { x → format and print (same as . for now) }
slurp { → read a line from stdin, push as string (newline stripped) }
eval  { string → feed it to the interpreter }
```

`slurp eval` is the dynamic-load idiom — read a line of source and run it.

**Mesh (only meaningful with `--name`/`SPACELANG_NAME` set):**
```
$   { value [peer] → send PUSH, fire-and-forget }
$!  { value [peer] → send EVAL, fire-and-forget (usually a thunk) }
$?  { value [peer] timeout-ms → send + wait; push t on ack, 0 on timeout }
```

Sender renders the value, receiver parses it back. For `$!`, send a
thunk so the receiver's `!` step runs it. `with-spco.sp` provides
`spco/$ / spco/$! / spco/$?` variants that first ask `spco` to ensure
the peer is up.

**Shell-out:**
```
sh/!   { "cmd" → run via /bin/sh -c, push exit status (stdout passes through) }
sh/>   { "cmd" → run, push captured stdout (single trailing \n stripped) }
sh/|   { "input" "cmd" → pipe input into stdin, push exit status }
sh/|>  { "input" "cmd" → pipe input into stdin, push captured stdout }
```

**Strings** (C primitives; the `str/` library adds helpers on top):
```
str/cat str/len str/sub str/ord str/chr str/eq
```

After `"str/str.sp" :require` you also get:
```
str/empty?  str/head  str/tail
str/reverse str/repeat
str/starts-with?  str/ends-with?
str/contains?  str/index
```

**Keywords:**
```
:s        { print the stack }
:bye      { exit 0 }
:sleep    { n → sleep n milliseconds }
:require  { "path" → load and feed file at path }
:exists   { "name" → is there a mesh peer with that name? }
:bus      { → push current bus dir }
:log      { x → print to stderr }
```

### Idioms

```
[ dup + ] [double] @       { bind a function }
21 double .                { → 42  (auto-eval; no ! needed) }

[ 1 + ] [inc] @
0 inc inc inc .            { → 3 }

slurp eval                 { one REPL step }
[ slurp eval ] [step] @
step step step             { read & execute 3 lines from stdin }
```

`if`-driven branching uses `!` because `if` selects a thunk:

```
[ "no" . ] [ "yes" . ] true if !    { → yes }
```

## Running `spci`

```sh
./spci                                           # REPL
./spci program.sp                                # batch mode (run file, exit)
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
./spcc --as NAME program.sp -o NAME    # bake NAME as default --name
```

`spcc` statically inlines `:require`d files at compile time, so the
output binary has no runtime dependency on the `.sp` library files —
you can delete them after building. Cycle-safe (each file is inlined
at most once).

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
make test              # all tests (unit + compile + str + mesh + property)
make test-quick        # skip mesh tests (faster)
python3 test_harness.py --seed 42    # repeatable property test seed
```

Test classes:

| Class                   | What it covers                                              |
|-------------------------|-------------------------------------------------------------|
| `TestEval`              | Language semantics + shell-out via `spci` REPL              |
| `TestStr`               | `str/` primitives and the spaceforth library on top         |
| `TestCompile`           | `spcc` binary matches `spci` output byte-for-byte           |
| `TestRequirePreprocess` | `spcc` statically inlines `:require`d files                 |
| `TestProperty`          | 200 random balanced programs → no segfaults                 |
| `TestMesh`              | `spco` + `spci` nodes over real Unix sockets                |

## Mesh discovery with `spco`

`spco` is a stateless discovery broker. It answers LOOKUP by checking
whether `$BUS/<name>.sock` exists on the filesystem. No spawning, no
lifecycle, no child management — just filesystem checks. It is itself
written in spaceforth (`spco.sp`) and compiled with `spcc`.

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

## Relation to other Forths

The stack-language core is conventional Forth. Quotations (`[ ... ]`) are
the Factor/gforth-`[:...;]` idiom. The `:require`-with-static-inline trick
is closest to **lbForth** (source-to-C, single binary). `spcc`'s
distribution model — compiler + headers + runtime archive sitting next to
each other — is the gcc shape.

What's not from Forth: the mesh primitives (`$ / $! / $?`), the
filesystem-based discovery in `spco`, the `with-spco.sp` ensure-then-send
helper, and the on-demand `spawn-node` flow. If you ported the project
to **Factor**, the kernel work would shrink dramatically and the mesh
layer would still be the interesting part.

The design rationale for `spcc` is in
`docs/superpowers/specs/2026-06-02-spcc-c-compiler-design.md`.
