# `sh/>`

**Category:** Shell  
**Stack:** `cmd -- stdout`  
**Source:** runtime (C)

Runs `cmd` via `/bin/sh -c`, captures its stdout as a string with the single trailing newline stripped. Stderr still passes through.

## Examples
```
> `echo 41` sh/> eval 1 + .
42
```

## Errors / notes
- cmd must be a string
