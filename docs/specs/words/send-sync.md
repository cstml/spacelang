# `$?`

**Category:** Mesh  
**Stack:** `value [name] timeout-ms -- bool`  
**Source:** runtime (C)

Sync send with timeout: sends `value`, waits up to `timeout-ms` milliseconds for an ACK frame. Pushes `t` on success, `nil` on timeout or connect failure.

## Examples
```
> `ping` [W1] 2000 $? .
t
```

## Errors / notes
- requires the node to be running under `--name` and `--bus`
