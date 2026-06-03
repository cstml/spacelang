# `if`

**Category:** Control  
**Stack:** `[else] [then] cond -- `  
**Source:** runtime (C)

Pops a condition and two thunks. Runs the **then** thunk (pushed second, top of stack at the time of `if`) when the condition is truthy; otherwise runs **else** (pushed first). Falsy values: `nil` / `false` / `0` / `""` / `[]`. Everything else is truthy.

## Examples
```
> [ `no` :log ] [ `yes` :log ] true if
yes
> [ `no` :log ] [ `yes` :log ] 0 if
no
```

## Errors / notes
- stack underflow if fewer than three operands present
- the two operands below `cond` must be thunks
