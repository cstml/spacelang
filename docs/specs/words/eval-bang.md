# `!`

**Category:** Evaluation  
**Stack:** `x -- ...`  
**Source:** runtime (C)

If `x` is a thunk, runs its body. For any other value, `!` is a no-op (the value stays on the stack). Used after `if` to actually execute the chosen branch in the REPL idiom `[else][then] cond if !`.

## Examples
```
> [ 1 2 + ] ! .
3
> 99 ! .
99
```
