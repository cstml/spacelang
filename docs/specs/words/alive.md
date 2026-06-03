# `:alive`

**Category:** Keywords  
**Stack:** `[name] -- bool`  
**Source:** runtime (C)

Attempts a real connect to `$BUS/<name>.sock` and immediately closes it. Pushes `t` iff something is actually listening.

## Examples
```
> [worker] :alive .
t
```
