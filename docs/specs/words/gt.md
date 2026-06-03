# `>`

**Category:** Comparison  
**Stack:** `a b -- bool`  
**Source:** runtime (C)

Pops two values, pushes `t` if `a` is greater than `b`, else `nil`. On numbers, ordering is numeric; on strings, lexicographic byte order.

## Examples
```
> 4 3 > .
t
```

## Errors / notes
- stack underflow with fewer than 2 items
