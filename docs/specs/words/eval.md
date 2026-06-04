# `eval`

**Category:** I/O  
**Stack:** `s -- ...`  
**Source:** runtime (C)

Pops a string and evaluates it as Spaceforth source. The classic step idiom is `slurp eval`.

## Examples
```
> `1 2 +` eval .
3
```

## Errors / notes
- the operand must be a string
- parse / runtime errors inside the string propagate
