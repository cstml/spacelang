# `:argv`

**Category:** Keywords  
**Stack:** `i -- s`  
**Source:** runtime (C)

Pushes the `i`-th user argument as a string, or `""` if out of bounds.

## Examples
```
> 0 :argv .
"a"
```

## Errors / notes
- operand must be a number
