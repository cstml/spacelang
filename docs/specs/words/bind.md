# `@`

**Category:** Binding  
**Stack:** `value [name] -- `  
**Source:** runtime (C)

Binds `value` to a global word. The name destination may be a bare-word thunk (`[name]`) or a string (`"name"`). When the bound value is itself a thunk, later mentions of the word auto-evaluate the thunk's body; to bind a thunk as pure data, double-wrap it.

## Examples
```
> 42 [answer] @  answer .
42
> [ dup + ] [double] @  21 double .
42
```

## Errors / notes
- the destination must be a one-word thunk or a string
