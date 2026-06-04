# `str/ends-with?`

**Category:** Strings (lib)  
**Stack:** `s suffix -- bool`  
**Source:** stdlib/str.sp

`t` iff `s` ends with `suffix`. Empty suffix always matches.

## Examples
```
> `hello.sp` `.sp` str/ends-with? .
t
```
