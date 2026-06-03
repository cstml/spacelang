# `str/chr`

**Category:** Strings (C)  
**Stack:** `n -- s`  
**Source:** runtime (C)

Builds a one-byte string from the low 8 bits of `n`.

## Examples
```
> 65 str/chr .
"A"
```

## Errors / notes
- operand must be a number
