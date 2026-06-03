# `$`

**Category:** Mesh  
**Stack:** `value [name] -- `  
**Source:** runtime (C)

Fire-and-forget PUSH: serialises `value` and sends it to the peer named `name` (connecting to `$BUS/<name>.sock` if not already connected). The receiver pushes the value onto its stack. No reply is awaited. Requires the node to be running under `--name` and `--bus`.

## Examples
```
> `hello` [W1] $
```

## Errors / notes
- silently no-ops if the peer cannot be reached
