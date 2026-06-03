# `swap`

**Category:** Stack  
**Stack:** `x y -- y x`  
**Source:** runtime (C)

Swaps the top two values.

## Examples
```
> 1 2 swap . .
1
2
```

## Errors / notes
- stack underflow with fewer than 2 items
