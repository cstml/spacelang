# `=`

**Category:** Comparison  
**Stack:** `a b -- bool`  
**Source:** runtime (C)

Pops two values, pushes `t` if `a` is equal `b`, else `nil`. On numbers, ordering is numeric; on strings, lexicographic byte order.

## Examples
```
> 5 5 = .
t
```

## Errors / notes
- stack underflow with fewer than 2 items
