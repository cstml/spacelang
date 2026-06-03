# `spco/$!`

**Category:** Mesh  
**Stack:** `value [name] -- `  
**Source:** stdlib/with-spco.sp

Same as `$!`, but first asks `spco` to ensure the peer is up.

## Examples
```
> [ 21 21 + ] [W] spco/$!
```

## Errors / notes
- fails if no spco broker is reachable on the bus
