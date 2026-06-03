# `dup`

**Category:** Stack  
**Stack:** `x -- x x`  
**Source:** runtime (C)

Duplicates the top-of-stack value.

## Examples
```
> 5 dup + .
10
```

## Errors / notes
- stack underflow on empty stack
