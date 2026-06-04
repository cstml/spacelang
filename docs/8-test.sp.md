# test.sp — spacelang test library

Minimal test framework built entirely in spacelang. Tracks pass/fail counts,
prints results to stderr, and reports a summary.

## Quick start

```sp
"github.com/cstml/spacelang/stdlib/test.sp" require

test/reset

"arithmetic" test/heading
1 2 + 3 "1+2=3" test/eq
5 2 * 11 "5*2=11" test/neq

"strings" test/heading
"hello" "hello" "same" test/str-eq

test/summary
```

Output (on stderr):

```
--- arithmetic ---
  PASS 1+2=3
  PASS 5*2=11

--- strings ---
  PASS same

=========================
  ALL PASSED (3)
=========================
```

## Concepts

### Counters

The library keeps two global scratch bindings — `test/_pass` and `test/_fail` —
as **string counters** (e.g. `"0"`, `"1"`, `"5"`). They are incremented via shell
`expr`. This avoids the need for a number→string built-in and makes the summary
formatting straightforward.

Counters are **not reentrant** — don't nest calls to test words from within
themselves.

### Lifecycle

A typical test file calls:

1. `test/reset` — zero both counters
2. `"section name" test/heading` — print a labeled section
3. Assertions (`test/eq`, `test/assert`, …) — check conditions
4. `test/summary` — print pass/fail totals

---

## Words

All output goes to stderr via the `log` built-in.

### `test/reset`

`--`

Zeros the pass and fail counters (sets both to the string `"0"`).

```
> test/reset
```

### `test/heading`

`"title" --`

Prints a section heading with a blank line above:

```
> "my tests" test/heading

--- my tests ---
```

### `test/assert`

`bool "name" --`

The fundamental assertion. Pops a boolean and a label. If the bool is truthy,
increments the pass counter and prints `  PASS name`. Otherwise increments the
fail counter and prints `  FAIL name`.

```
> true "always passes" test/assert
  PASS always passes
> false "always fails" test/assert
  FAIL always fails
```

### `test/eq`

`actual expected "name" --`

Compare two values with `=` (numeric equality). Passes if they are equal.

```
> 1 2 + 3 "1+2 vs 3" test/eq
  PASS 1+2 vs 3
```

### `test/neq`

`actual expected "name" --`

Compare two values with `=` (numeric equality). Passes if they are NOT equal.

```
> 3 4 "distinct" test/neq
  PASS distinct
> 5 5 "same-fails" test/neq
  FAIL same-fails
```

### `test/true?`

`bool "name" --`

Assert that a value is truthy.

```
> 42 0 > "positive" test/true?
  PASS positive
```

### `test/false?`

`bool "name" --`

Assert that a value is falsey.

```
> 0 "zero is falsey" test/false?
  PASS zero is falsey
```

### `test/num-eq`

`actual expected "name" --`

Alias for `test/eq` — asserts numeric equality via `=`.

### `test/str-eq`

`actual expected "name" --`

Assert two strings are equal using `str/eq`.

```
> "hello" "world" str/cat "hello" "world" str/cat
  "helloworld" "concat" test/str-eq
  PASS concat
```

### `test/summary`

`--`

Prints a summary banner. Three possible outputs:

- **All passed:** `ALL PASSED (N)`
- **Failures:** `PASS: N  FAIL: M`
- **No tests run:** `(no tests run)`

```
> test/summary
=========================
  ALL PASSED (3)
=========================
```

---

## Dependencies

- `str.sp` — for `str/cat` and `str/eq`

---

## Limitations

- **Counters are strings** — comparisons in `test/summary` use `str/eq` and
  `test/_not`. The string representation is always the decimal form (e.g.
  `"0"`, `"5"`), which is fine for pass/fail counts.

- **Non-reentrant** — scratch bindings (`test/_r-ok`, `test/_r-label`, etc.)
  prevent nesting. Don't trigger one test assertion from inside another.

- **No `test/str-neq`** — use `test/str-eq` with swapped branches or check
  `str/eq` and negate manually.

- **`test/neq` is numeric** — it uses `=`, not `str/eq`. For string
  inequality, use `test/str-eq` and negate the expectation.
