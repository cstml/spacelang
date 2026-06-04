# `str/reverse`

**Category:** Strings (lib)  
**Stack:** `s -- r`  
**Source:** stdlib/str.sp

Byte-reverses `s`. Not codepoint-aware (multi-byte UTF-8 will scramble).

## Examples
```
> `spacelang` str/reverse .
"gnalecaps"
```
