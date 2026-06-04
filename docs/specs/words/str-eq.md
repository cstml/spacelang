# `str/eq`

**Category:** Strings (C)  
**Stack:** `a b -- bool`  
**Source:** runtime (C)

Byte-equal string compare. Pushes `t` or `nil`.

## Examples
```
> `foo` `foo` str/eq .
t
```

## Errors / notes
- both operands must be strings
