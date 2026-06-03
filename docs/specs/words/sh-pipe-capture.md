# `sh/|>`

**Category:** Shell  
**Stack:** `input cmd -- stdout`  
**Source:** runtime (C)

Pipes `input` to `cmd`'s stdin and captures stdout (trailing newline stripped).

## Examples
```
> `hello world` `wc -w` sh/|> .
"2"
```
