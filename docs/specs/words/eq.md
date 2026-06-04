# `=`

**Category:** Comparison  
**Stack:** `a b -- bool`  
**Source:** runtime (C)

Pops two values, pushes `t` iff `a` and `b` are structurally equal. Cross-type comparisons are `nil` (different ranks in the total order: **number < bool/word < thunk < string**). Within a rank: numbers numerically; strings byte-equal; bool/word by name (so the bool `true` equals a word of the same spelling); thunks equal iff their FNV-1a hashes match.

## Examples
```
> 5 5 = .
t
> "a" "a" = .
t
> [1 2 +] [1 2 +] = .
t
> 1 "1" = .
nil
```

## Errors / notes
- stack underflow with fewer than 2 items
