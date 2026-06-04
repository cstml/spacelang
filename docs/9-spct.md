# spct — spacelang test runner

A compiled binary that loads the `test.sp` stdlib and runs test files through
the interpreter. Test files manage their own lifecycle (`test/reset` and
`test/summary`); spct is a thin loader.

## What it does

`spct` takes one or more `.sp` test files as arguments. It loads `test.sp`
(and transitively `str.sp`), then evaluates each test file in order. The test
files call `test/reset` before their assertions and `test/summary` at the end.

`spct` itself does **not** call `test/reset` or `test/summary` — it delegates
that to the test author so a single invocation can run multiple independent
suites.

## CLI

```
spct test1.sp test2.sp ...     run test files
spct                           print usage
```

| Arg | Effect |
|-----|--------|
| `test1.sp …` | Load and evaluate each file in order |
| (no args) | Print usage to stderr and exit |

## Examples

### Single test file

```bash
$ cat my_test.sp
test/reset
"math" test/heading
1 2 + 3 "1+2=3" test/eq
test/summary

$ spct my_test.sp
--- math ---
  PASS 1+2=3
=========================
  ALL PASSED (1)
=========================
```

### Multiple test files

```bash
$ spct unit.sp integration.sp edge_cases.sp
--- unit tests ---
  PASS foo
  PASS bar
=========================
  ALL PASSED (2)
=========================
...
```

### Exit codes

Currently `spct` always exits 0. The test output must be inspected to determine
pass/fail. A future version may exit 1 when `test/_fail` is non-zero.

## How it works

`spct` is compiled from `spct/spct.sp`:

```
spcc spct.sp -o spct
```

It does three things:

1. `require`s `str.sp` and `test.sp` (inlined at compile time)
2. Checks `io/argc` — if zero, prints usage
3. If arguments are present, loops over `io/argv` indices, `cat`s each file
   through `sh/>`, and `eval`s the result

Since `spcc` cannot inline runtime `require` calls, dynamic file loading uses
`cat <file>` piped to `eval`.

## Dependencies

- `stdlib/str.sp` — string primitives
- `stdlib/test.sp` — the test library

## Comparison with spci

| | `spci` | `spct` |
|---|---|---|
| Mode | REPL / batch / mesh | batch test runner |
| Requires explicit `require` | yes | no — test.sp preloaded |
| Auto `test/reset` | no | no — files control lifecycle |
| Auto `test/summary` | no | no — files control lifecycle |
| Multiple files | run each sequentially | run each sequentially |

You can achieve the same result with `spci`:

```bash
$ spci -c '"stdlib/test.sp" require test/reset ... test/summary'
```

`spct` is a convenience — it bundles the require and provides a cleaner CLI.

## Implementation notes

- **No `--as` flag.** `spct` is compiled without `--as`, so it does not start
  in mesh mode (unlike `spco` and `spcd`).
- **`str/->str`** is available from `str.sp` but is currently an identity
  function. Counters in `test.sp` are stored as strings so `str/cat` works
  directly with them.
- **Counter arithmetic** uses shell `expr` (e.g. `expr 4 + 1`), since spacelang
  has no built-in number→string conversion.

## See also

- `docs/8-test.sp.md` — test.sp library reference
- `spct/spct.sp` — source code for the runner
