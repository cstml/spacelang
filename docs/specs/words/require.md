# `require`

**Category:** Keywords  
**Stack:** `path -- `  
**Source:** runtime (C)

Loads and evaluates the `.sp` source file at `path`. Paths are resolved relative to the directory of the currently-loading file. Each file is loaded at most once (cycle-safe). `spcc` inlines `require` at compile time when the operand is a string literal, making compiled binaries self-contained.

## Examples
```
> `stdlib/str.sp` require
```

## Errors / notes
- operand must be a string literal (at compile time)
- missing file is reported with file:line
