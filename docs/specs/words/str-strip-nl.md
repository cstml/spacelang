# `str/strip-nl`

**Category:** Strings (lib)  
**Stack:** `s -- s'`  
**Source:** stdlib/str.sp

Removes a single trailing `\n` from `s`, if present.

## Examples
```
> `hello\n` str/strip-nl .
"hello"
```
