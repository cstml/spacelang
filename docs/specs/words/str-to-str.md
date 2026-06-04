# `str/->str`

**Category:** Strings (C)
**Stack:** `v -- s`
**Source:** runtime (C)

Converts any value to its string representation. The result is the same
form that `.`, `;`, `$`, `$!`, and `$?` use when rendering values for
output or mesh transport.

## Examples
```
> 42 str/->str .
"42"

> true str/->str .
"true"

> `hello` str/->str .
"hello"

> [1 2 +] str/->str .
"[1 2 +]"
```

## Errors / notes
- accepts any value type (number, bool, string, word, thunk)
- strings are quoted; numbers, bools, and words are not
- `nil` (the number 0) renders as `"0"`
