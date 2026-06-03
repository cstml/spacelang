# `str/repeat`

**Category:** Strings (lib)  
**Stack:** `s n -- s*n`  
**Source:** stdlib/str.sp

Concatenates `s` with itself `n` times. Non-positive `n` returns `""`.

## Examples
```
> `ab` 4 str/repeat .
"abababab"
```
