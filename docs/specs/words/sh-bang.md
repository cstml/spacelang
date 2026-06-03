# `sh/!`

**Category:** Shell  
**Stack:** `cmd -- status`  
**Source:** runtime (C)

Runs `cmd` via `/bin/sh -c`. The child's stdout and stderr inherit the parent's. Pushes the `system()` exit status (`0` on success, `signal<<8 | exit` otherwise).

## Examples
```
> `echo hi` sh/! .
hi
0
```

## Errors / notes
- cmd must be a string
