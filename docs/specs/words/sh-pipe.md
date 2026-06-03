# `sh/|`

**Category:** Shell  
**Stack:** `input cmd -- status`  
**Source:** runtime (C)

Pipes `input` to `cmd`'s stdin, then returns the exit status (cmd's stdout passes through). Useful with predicates like `grep -q`.

## Examples
```
> `hay` `grep -q hay` sh/| .
0
```
