# `:exists`

**Category:** Keywords  
**Stack:** `name -- bool`  
**Source:** runtime (C)

Pushes `t` iff `$BUS/<name>.sock` exists and is a socket — a pure filesystem check, no connect. Use `:alive` for a real liveness probe.

## Examples
```
> `W1` :exists .
t
```

## Errors / notes
- operand must be a string
- always `nil` outside mesh mode
