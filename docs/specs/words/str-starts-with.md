# `str/starts-with?`

**Category:** Strings (lib)  
**Stack:** `s prefix -- bool`  
**Source:** stdlib/str.sp

`t` iff `s` begins with `prefix`. Empty prefix always matches.

## Examples
```
> `hello.sp` `he` str/starts-with? .
t
```
