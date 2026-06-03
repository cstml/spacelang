# `+`

**Category:** Arithmetic  
**Stack:** `a b -- a+b`  
**Source:** runtime (C)

Pops two numbers, pushes their integer sum.

## Examples
```
> 3 4 + .
7
```

## Errors / notes
- stack underflow with fewer than 2 items
- expected number if either operand is not a number
