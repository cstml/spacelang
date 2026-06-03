# `:env`

**Category:** Keywords  
**Stack:** `name -- value`  
**Source:** runtime (C)

Pushes the value of environment variable `name`, or `""` if unset.

## Examples
```
> `HOME` :env .
"/home/cstml"
```

## Errors / notes
- operand must be a string
