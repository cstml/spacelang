# `str/ord`

**Category:** Strings (C)  
**Stack:** `s -- n`  
**Source:** runtime (C)

Pushes the unsigned byte value (0–255) of the first character of `s`, or `-1` for the empty string.

## Examples
```
> `A` str/ord .
65
```

## Errors / notes
- operand must be a string
