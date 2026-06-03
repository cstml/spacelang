# `spco/$?`

**Category:** Mesh  
**Stack:** `value [name] timeout-ms -- bool`  
**Source:** stdlib/with-spco.sp

Same as `$?`, but first asks `spco` to ensure the peer is up.

## Examples
```
> `ping` [W] 2000 spco/$? .
t
```
