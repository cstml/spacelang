# `str/contains?`

**Category:** Strings (lib)  
**Stack:** `s needle -- bool`  
**Source:** stdlib/str.sp

`t` iff `needle` appears anywhere in `s`. Empty needle always matches.

## Examples
```
> `quick brown fox` `brown` str/contains? .
t
```
