# Spaceforth Builtins Reference

Every built-in word in Spaceforth, with a short description and a runnable example.
All examples assume you're at the `spci` REPL (`./spci`).

---

## Arithmetic / Comparison

Arithmetic (`+ - * /`) pops two numbers, pushes the result.

Comparison (`< > <= >= =`) is total across every Value type. Rank order:

```
number  <  bool/word  <  thunk  <  string
```

`bool` and `word` share a rank and are compared by name (so the bool `true`
equals a word of the same spelling). Within a rank: numbers numerically;
strings lex byte order; thunks by FNV-1a hash of their serialised form
(deterministic but not semantic). `=` is structural — cross-type
comparisons return `nil`.

| Word | Stack effect | Description |
|------|-------------|-------------|
| `+`  | `a b -- a+b` | Add (numbers) |
| `-`  | `a b -- a-b` | Subtract (numbers) |
| `*`  | `a b -- a*b` | Multiply (numbers) |
| `/`  | `a b -- a/b` | Integer divide (numbers) |
| `<`  | `a b -- bool` | Less than (total order) |
| `>`  | `a b -- bool` | Greater than (total order) |
| `<=` | `a b -- bool` | Less or equal (total order) |
| `>=` | `a b -- bool` | Greater or equal (total order) |
| `=`  | `a b -- bool` | Structural equal (cross-type → nil) |

**Example:**
```
> 3 4 + .
7
> 5 5 = .
t
> "abc" "abd" < .
t
> 0 false < .              { NUM < atom }
t
> [1 2 +] [1 2 +] = .       { thunks compared by hash }
t
> 1 "1" = .                 { cross-type }
nil
```

Per-word specs: [`docs/specs/words/`](specs/words.md).

---

## Stack Manipulation

| Word   | Stack effect    | Description |
|--------|-----------------|-------------|
| `dup`  | `x -- x x`      | Duplicate the top item |
| `swap` | `x y -- y x`    | Swap the top two items |
| `drop` | `x --`          | Discard the top item |
| `rot`  | `a b c -- b c a` | Rotate the top three |

**Example:**
```
> 1 2 dup
> _s
-- stack (3) --
  [0] 1
  [1] 2
  [2] 2
> drop
> _s
-- stack (2) --
  [0] 1
  [1] 2
> swap
> _s
-- stack (2) --
  [0] 2
  [1] 1
> 3 rot
> _s
-- stack (3) --
  [0] 1
  [1] 3
  [2] 2
```

---

## Control Flow

### `if`

`[else-body] [then-body] cond if`

Pops the condition, then the "then" thunk, then the "else" thunk (order: else, then,
cond). Pushes the chosen thunk. Typically followed by `!` to evaluate it.

```
> [ "no" . ] [ "yes" . ] true if !
yes
> [ "no" . ] [ "yes" . ] false if !
no
```

Without `!`, the selected thunk is left on the stack as data:

```
> [1 2 +] [3 4 +] true if
> _s
-- stack (1) --
  [0] [3 4 +]
```

---

## Binding & Evaluation

### `@` — bind

`value [name] @`

Binds a value to a name in the global word table. The name must be given as a
single-word thunk (`[name]`) or a string (`"name"`).

```
> 42 [answer] @
> answer .
42
```

Functions are thunks bound to words — they auto-evaluate on reference:

```
> [ dup + ] [double] @
> 21 double .
42
```

To push a raw thunk as data instead, double-wrap it:

```
> [ [ 1 2 + ] ] [data-thunk] @
> data-thunk _s
-- stack (1) --
  [0] [1 2 +]
```

### `!` — evaluate

`thunk !`

If the top of stack is a thunk, run its body. Non-thunks are left as-is — `!` is
a no-op for them.

```
> [ 1 2 + ] ! .
3
> 99 ! .
99
```

### `~` — describe

`[name] ~`

Prints the name and its current binding.

```
> 42 [answer] @
> [answer] ~
answer ~ 42
> [double] ~
double ~ [dup +]
```

---

## I/O

### `.` — print

`x .`

Print the value with a trailing newline.

```
> 42 .
42
> "hello" .
"hello"
> [1 2 +] .
[1 2 +]
```

### `;` — print without newline

`x ;`

Pretty-prints the top of stack without the trailing `\n` that `.` adds. Use it
to compose pieces of a single output line.

```
> "hi" ; " " ; "there" .
"hi" "there"
```

### `io/slurp` — read line

`io/slurp`

Reads one line from stdin (newline stripped), pushes it as a string.

```
> io/slurp .
hello world          { typed by user }
"hello world"
```

### `eval` — evaluate string

`string eval`

Feeds a string into the interpreter as source code.

```
> "1 2 +" eval
> _s
-- stack (1) --
  [0] 3
```

The classic REPL-step idiom:

```
> io/slurp eval
1 2 + .             { typed by user }
3
```

Make it a reusable word:

```
> [ io/slurp eval ] [step] @
> step
40 2 + .             { typed }
42
```

---

## Mesh Primitives

Only meaningful when running with `--name` and `--bus`. Each node listens on
`$BUS/<name>.sock` and lazily connects to peers on first send.

### `$` — fire-and-forget PUSH

`value [name] $`

Sends `value` to the peer. The receiver pushes it onto its stack. No response.

```
> "hello from driver" [W1] $
```

### `$!` — fire-and-forget EVAL

`value [name] $!`

Sends `value` to the peer, then runs `!` on the receiver side. If you send a
thunk, the receiver evaluates it.

```
> [ 1 2 + . ] [W2] $!        { W2 prints "3" on its own terminal }
> [_s] [W1] $!                { asks W1 to print its stack }
```

### `$?` — sync send with timeout

`value [name] timeout-ms $?`

Sends `value` to the peer and waits for an ACK. Pushes `t` on success, `0`
(nil) on timeout or unreachable peer.

```
> "ping" [W1] 2000 $? .
t                             { W1 acked within 2 seconds }
> "ping" [Nobody] 200 $? .
0                             { no such peer, connect failed }
```

### spco-aware variants (requires `"with-spco.sp" require`)

These ask the `spco` discovery broker to ensure the peer is up before sending:

| Word       | Like |
|-----------|------|
| `spco/$`  | `$`  |
| `spco/$!` | `$!` |
| `spco/$?` | `$?` |

```
> "with-spco.sp" require
> "hello" [B] spco/$
```

---

## Shell-Out

### `sh/!` — run command, exit status

`"cmd" sh/!`

Runs the command via `/bin/sh -c`. Stdout passes through to the terminal.
Pushes the exit status (as returned by `system()`).

```
> "echo hello" sh/! drop
hello
> "true" sh/! .
0
> "false" sh/! .
256
```

### `sh/>` — run command, capture stdout

`"cmd" sh/>`

Runs the command, captures its stdout as a string (trailing newline stripped).
Stderr passes through.

```
> "echo hello" sh/> .
"hello"
> "whoami" sh/> .
"cstml"
> "echo 41" sh/> eval 1 + .
42
```

### `sh/|` — pipe stdin, exit status

`"input" "cmd" sh/|`

Pipes the input string to the command's stdin. Pushes the exit status. The
command's stdout passes through.

```
> "needle\nhay" "grep needle" sh/| .
0                              { found }
> "hey" "grep needle" sh/| .
256                            { not found }
```

### `sh/|>` — pipe stdin, capture stdout

`"input" "cmd" sh/|>`

Pipes the input string to the command's stdin, captures stdout.

```
> "hello world" "wc -w" sh/|> .
"2"
> "abc" "tr a-z A-Z" sh/|> .
"ABC"
```

---

## String Primitives (C level)

These live in the runtime. An optional higher-level library (`str/str.sp`) builds
on top of them.

### `str/cat` — concatenate

`"a" "b" str/cat`

```
> "foo" "bar" str/cat .
"foobar"
```

### `str/len` — byte length

`"s" str/len`

```
> "hello" str/len .
5
> "" str/len .
0
```

### `str/sub` — substring

`"s" start len str/sub`

Bounds-clamped: negative/oversized start and len are clamped to valid ranges.

```
> "hello, world" 7 5 str/sub .
"world"
> "abc" 1 100 str/sub .
"bc"
> "abc" -5 2 str/sub .
"ab"
```

### `str/ord` — first byte as number

`"s" str/ord`

Returns the unsigned byte value of the first character (0–255), or `-1` for an
empty string.

```
> "A" str/ord .
65
> "" str/ord .
-1
```

### `str/chr` — number to single-byte string

`n str/chr`

```
> 65 str/chr .
"A"
> 97 str/chr .
"a"
```

### `str/eq` — string equality

`"a" "b" str/eq`

```
> "foo" "foo" str/eq .
t
> "foo" "bar" str/eq .
0
```

---

## String Library (after `"str/str.sp" require`)

### `str/empty?`

`"s" str/empty?`

```
> "" str/empty? .
t
> "x" str/empty? .
0
```

### `str/head`

`"s" str/head`

First character as a string. Empty string if input is empty.

```
> "hello" str/head .
"h"
> "" str/head .
""
```

### `str/tail`

`"s" str/tail`

Everything after the first character.

```
> "hello" str/tail .
"ello"
> "a" str/tail .
""
```

### `str/reverse`

`"s" str/reverse`

```
> "spacelang" str/reverse .
"gnalecaps"
```

### `str/repeat`

`"s" n str/repeat`

```
> "ab" 4 str/repeat .
"abababab"
> "ha" 3 str/repeat .
"hahaha"
```

### `str/starts-with?`

`"s" "prefix" str/starts-with?`

```
> "hello.sp" "he" str/starts-with? .
t
> "hi" "hello" str/starts-with? .
0
```

### `str/ends-with?`

`"s" "suffix" str/ends-with?`

```
> "hello.sp" ".sp" str/ends-with? .
t
> "main.c" ".sp" str/ends-with? .
0
```

### `str/contains?`

`"s" "sub" str/contains?`

```
> "the quick brown fox" "brown" str/contains? .
t
> "the quick brown fox" "purple" str/contains? .
0
```

### `str/index`

`"s" "sub" str/index`

Returns the 0-based index of the first occurrence, or `-1` if not found.

```
> "the quick brown fox" "brown" str/index .
10
> "the quick brown fox" "purple" str/index .
-1
```

### `str/strip-nl`

`"s" str/strip-nl`

Drops a single trailing newline. Safe on empty strings.

```
> "hello\n" str/strip-nl .
"hello"
> "hello\n\n" str/strip-nl .
"hello\n"
```

---

## Keywords

### `_s` — print stack

Prints the current stack contents (without modifying it).

```
> 1 2 "hi" [3 4 +]
> _s
-- stack (4) --
  [0] 1
  [1] 2
  [2] "hi"
  [3] [3 4 +]
```

### `bye!` — exit

Exits the interpreter immediately with status 0.

```
> bye!
```

### `io/sleep` — io/sleep

`n io/sleep`

Sleeps for `n` milliseconds.

```
> "about to io/sleep..." . 1000 io/sleep "woke up" .
"about to io/sleep..."
"woke up"
```

### `require` — load file

`"path" require`

Loads and evaluates a `.sp` source file. Resolves relative paths against the
directory of the currently-loading file, not CWD. Cycle-safe (each file is
inlined at most once).

```
> "stdlib/str.sp" require
> "hello" str/empty? .
0
```

### `io/log` — io/log to stderr

`"msg" io/log`

Writes the string to stderr (with a newline). Useful for debug output that
doesn't contaminate stack values.

```
> "processing..." io/log
processing...                    { on stderr }
```

### `sp/bus` — current bus directory

`sp/bus`

Pushes the current bus directory as a string (or `""` if not in mesh mode).

```
> sp/bus .
"/tmp/spacelang"
```

### `io/env` — environment variable

`"NAME" io/env`

Pushes the value of the environment variable, or `""` if unset.

```
> "HOME" io/env .
"/home/cstml"
> "NONEXISTENT" io/env .
""
```

### `io/argc` — user argument count

`io/argc`

Pushes the number of arguments passed after `--` on the command line.

```bash
$ ./spci --name N --bus /tmp/s -- myscript.sp data.txt
```
```
> io/argc .
2
```

### `io/argv` — nth user argument

`n io/argv`

Pushes the nth argument passed after `--`, or `""` if out of bounds.

```
> 0 io/argv .
"myscript.sp"
> 1 io/argv .
"data.txt"
> 5 io/argv .
""
```

### `sp/exists?` — socket existence check

`"name" sp/exists?`

Pushes `t` if `$BUS/<name>.sock` exists and is a socket (filesystem check only).

```
> "W1" sp/exists? .
t
```

### `sp/alive?` — real connect test

`[name] sp/alive?`

Pushes `t` if a peer is actually listening on `$BUS/<name>.sock` (opens a real
connection, then closes it). More reliable than `sp/exists?`.

```
> [worker] sp/alive? .
t
```

### `wo/name>str` — name to string

`[name] wo/name>str`

Extracts the name from a binding form and pushes it as a string.

```
> [my-peer] wo/name>str .
"my-peer"
> "my-peer" wo/name>str .
"my-peer"
```

### `wo/str>name` — string to name

`"s" wo/str>name`

Converts a string into a binding-form thunk `[name]`.

```
> "worker1" wo/str>name .
[worker1]
```

---

## Literal Values

| Word    | Stack effect | Description |
|---------|-------------|-------------|
| `true`  | `-- true`   | Boolean true (prints as `t`) |
| `false` | `-- false`  | Boolean false (prints as `0`) |
| `nil`   | `-- 0`      | Alias for the falsy number 0 |

```
> true .
t
> false .
0
> nil .
0
> 42 nil = .
0
```

---

## Quotations (Thunks)

`[ ... ]`

A quotation (thunk) captures a sequence of terms as a first-class value.
It does not execute when parsed — it sits on the stack as data.

```
> [ 1 2 + ] _s
-- stack (1) --
  [0] [1 2 +]
```

Thunks bound to words **auto-evaluate** on reference:

```
> [ dup + ] [double] @
> 5 double .
10
```

To push a thunk as data through a binding, double-wrap it:

```
> [ [ 1 2 + ] ] [my-data] @
> my-data _s
-- stack (1) --
  [0] [1 2 +]
```

---

## Comments

`{ this is a comment }`

Comments start with `{` and end with `}`. They do not nest — a `{` inside a
comment closes it early. Strings (`"`, `'`, `` ` ``) shield their contents from
comment parsing.

```
> 1 2 + { add them } .
3
> { this
.. spans
.. lines
.. } "ok" .
"ok"
```

---

## Quick Reference by Category

```
Arithmetic      +  -  *  /  <  >  <=  >=  =
Stack           dup  swap  drop  rot
Control         if
Binding         @  !  ~
I/O             .  ,  io/slurp  eval
Mesh            $  $!  $?
Shell           sh/!  sh/>  sh/|  sh/|>
Strings (C)     str/cat  str/len  str/sub  str/ord  str/chr  str/eq
Keywords        _s  bye!  io/sleep  require  sp/exists?  sp/alive?  sp/bus  io/log
                io/env  io/argc  io/argv  wo/name>str  wo/str>name
Literals        true  false  nil
Syntax          [ ... ]  { ... }  "..."  '...'  `...`
```

### After `"stdlib/str.sp" require`

```
str/empty?  str/head  str/tail  str/reverse  str/repeat
str/starts-with?  str/ends-with?  str/contains?  str/index  str/strip-nl
```

### After `"stdlib/with-spco.sp" require`

```
spco/$  spco/$!  spco/$?
```
