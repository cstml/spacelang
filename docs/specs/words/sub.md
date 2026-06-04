# `-`

**Category:** Arithmetic  
**Stack:** `a b -- a-b`  
**Source:** runtime (C)

Pops two numbers, pushes their integer difference.

## Examples
```
> 10 4 - .
6
```

## Errors / notes
- stack underflow with fewer than 2 items
- expected number if either operand is not a number
