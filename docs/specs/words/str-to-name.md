# `wo/str>name`

**Category:** Keywords  
**Stack:** `s -- [name]`  
**Source:** runtime (C)

Wraps `s` as a one-word thunk usable as a binding destination.

## Examples
```
> `worker1` wo/str>name .
[worker1]
```

## Errors / notes
- operand must be a string
