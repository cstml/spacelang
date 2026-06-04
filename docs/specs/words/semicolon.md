# `;`

**Category:** I/O  
**Stack:** `x -- `  
**Source:** runtime (C)

Pretty-prints the top of stack to stdout **without** a trailing newline. The
companion of `.`, which always appends `\n`. Use `;` to compose pieces of a
single output line:

```
"hello, " ; "world" .
```

produces `hello, world` (one line).

## Examples
```
> 42 ;
42> "x" ; "y" ; "z" .
xyz
> `>` ; 1 2 + ; ` ` ; 3 . `<` .
>3 3<
```

## Errors / notes
- stack underflow on empty stack
- Output is identical to `.` apart from the missing newline; the same
  pretty-printer is shared.
