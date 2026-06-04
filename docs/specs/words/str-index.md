# `str/index`

**Category:** Strings (lib)  
**Stack:** `s needle -- n`  
**Source:** stdlib/str.sp

0-based byte index of the first occurrence of `needle` in `s`, or `-1` if absent.

## Examples
```
> `quick brown fox` `brown` str/index .
6
```
