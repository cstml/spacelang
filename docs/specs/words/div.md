# `/`

**Category:** Arithmetic  
**Stack:** `a b -- a/b`  
**Source:** runtime (C)

Pops two numbers, pushes their integer quotient (truncated toward zero).

## Examples
```
> 10 3 / .
3
```

## Errors / notes
- stack underflow with fewer than 2 items
- expected number if either operand is not a number
- division by zero is undefined (runtime aborts)
