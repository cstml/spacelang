# `spco/$`

**Category:** Mesh  
**Stack:** `value [name] -- `  
**Source:** stdlib/with-spco.sp

Same as `$`, but first asks the `spco` discovery broker to ensure the peer is up (spawning it if necessary). Requires `"stdlib/with-spco.sp" :require`.

## Examples
```
> `hello` [B] spco/$
```

## Errors / notes
- fails if no spco broker is reachable on the bus
