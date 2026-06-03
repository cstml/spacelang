# `:argc`

**Category:** Keywords  
**Stack:** ` -- n`  
**Source:** runtime (C)

Pushes the count of user arguments (those after `--` on the command line, or everything after the script path for a compiled binary).

## Examples
```
$ ./prog a b c
> :argc .
3
```
