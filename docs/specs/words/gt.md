# `>`

**Category:** Comparison  
**Stack:** `a b -- bool`  
**Source:** runtime (C)

Pops two values, pushes `t` if `a` is greater than `b`, else `nil`. Total order across all Value types. **Rank: number < bool/word < thunk < string** (bool and word share a rank, compared by name). Within a rank: numbers numerically; strings lex byte order; bool/word by name; thunks by FNV-1a hash of their serialised form (deterministic but not semantic).

## Examples
```
> 4 3 > .
t
```

## Errors / notes
- stack underflow with fewer than 2 items
