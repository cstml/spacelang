<!-- AGENTS.md — spacelang project guidelines and learnings.              -->
<!-- Reloaded each session; update as patterns emerge.                    -->

# spacelang

A concatenative, stack-based language. Single-threaded C interpreter (`spci`),
compiler (`spcc`), mesh transport over Unix sockets, dependency manager (`spcd`).

## Language rules (hard-won)

**Arguments before function, always.** `"title" test/heading`, never
`test/heading "title"`. In a concatenative language values are pushed then
consumed. Stack underflows have no line numbers — you trace by dead reckoning.

**`if` order: `[else] [then] condition if`.** The runtime pops condition first,
then then, then else. When the condition boolean is produced by an operation
(`<`, `=`, etc.) that executes *before* the thunks are pushed, the bool ends up
underneath the thunk pair. Fix: push the thunks first, then compute the
condition, so the bool is on top when `if` fires. Or use `rot` to float it up.

**Tokens are not what you guess.** `2dup` is parsed as number `2` then word
`dup`. `x_{n+1}` inside a `{comment}` prematurely closes it — braces do not
nest. The parser is simple; read `runtime.c` before assuming.

**No `num>str` built-in.** String concatenation (`str/cat`) needs two strings,
and there is no way to convert a number to its decimal representation without
shell. Workaround: store counters as strings and increment via `expr N + 1`.

**`sh/!` forks a child process.** `exec` redirects inside that child only —
it cannot change the parent's stdout. Don't try to capture `.` output by
shelling out with `exec 1>file`.

**`spcc --as <name>` enables mesh mode** — the compiled binary listens on
`/tmp/spacelang/<name>.sock`. Use it for mesh services (spco, spcd). For CLI
tools (spct), compile without `--as`.

**Nothing is `true` or `false` — truthiness is value-dependent.** Numbers are
truthy if non-zero. Strings are truthy if non-empty. Thunks are truthy if
non-empty. Booleans (`V_BOOL`) are their own type, printed as `t` or `nil`.
`=` compares numbers only; use `str/eq` for strings.

**`*` on booleans fails** because `n_of()` expects `V_NUM`, but booleans are
`V_BOOL`. No built-in `and`/`or`/`not` — build them with `if`.

## Project conventions

**Commits.** No `Co-Authored-By` trailers. Messages are one-line imperative.

**Dependencies.** URL-style paths in deps.sp (e.g.
`github.com/cstml/spacelang`). The resolver walks up to find `deps.sp`, then
maps URLs via `deps/override` or `lock.sp` entries.

**Documentation.** `docs/` files are numbered. British English in prose,
comments, and docs. Code identifiers keep the spelling of the surrounding code.

**Testing.** Python harness (`test_harness.py`) with unittest classes. Each
new binary or stdlib module gets a test class. Run with `python3
test_harness.py --quick`.

**New binaries** follow the spcd/spco pattern: a `foobin/foobin.sp` source,
a `bin/foobin` make target, and a `TestFoobin` class in the harness.

**Mesh transport.** Full mesh over Unix domain sockets in `/tmp/spacelang/`.
Three operators: `$` (fire-and-forget PUSH), `$!` (fire-and-forget EVAL),
`$?` (sync send with timeout). Prefer distinct operators over one
parameterized operator with flags.

## Common tasks

Build all: `make`. Quick test: `python3 test_harness.py --quick`.
Compile a standalone binary: `./bin/spcc src.sp -o out`.
Run through interpreter: `echo 'code' | ./bin/spci`.
