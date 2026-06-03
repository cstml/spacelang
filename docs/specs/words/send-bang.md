# `$!`

**Category:** Mesh  
**Stack:** `value [name] -- `  
**Source:** runtime (C)

Fire-and-forget EVAL: like `$`, but the receiver runs `!` on the value after pushing it. Sending a thunk causes the receiver to execute it.

## Examples
```
> [ 1 2 + . ] [W2] $!
```

## Errors / notes
- silently no-ops if the peer cannot be reached
