# `rot`

**Category:** Stack  
**Stack:** `a b c -- b c a`  
**Source:** runtime (C)

Rotates the top three values: the third-from-top moves to the top.

## Examples
```
> 1 2 3 rot . . .
1
3
2
```

## Errors / notes
- stack underflow with fewer than 3 items
