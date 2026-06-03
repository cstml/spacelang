# `str/sub`

**Category:** Strings (C)  
**Stack:** `s start len -- sub`  
**Source:** runtime (C)

Returns `len` bytes of `s` starting at `start`. Bounds are clamped: a negative `start` is clamped to 0; over-length ranges are truncated.

## Examples
```
> `hello, world` 7 5 str/sub .
"world"
```

## Errors / notes
- s must be a string; start and len must be numbers
