# `wo/name>str`

**Category:** Keywords  
**Stack:** `[name] -- s`  
**Source:** runtime (C)

Extracts a name from a one-word thunk binding form and pushes it as a string.

## Examples
```
> [my-peer] wo/name>str .
"my-peer"
```

## Errors / notes
- operand must be a name-thunk or a string
