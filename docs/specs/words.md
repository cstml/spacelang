# Spaceforth words — specifications

One file per built-in word. Each spec covers stack effect, semantics, an example, and known errors. The companion overview is [`docs/7-builtins.md`](../7-builtins.md).

## Arithmetic

- [`+`](./words/add.md)
- [`-`](./words/sub.md)
- [`*`](./words/mul.md)
- [`/`](./words/div.md)

## Comparison

- [`<`](words/lt.md)
- [`>`](words/gt.md)
- [`<=`](words/le.md)
- [`>=`](words/ge.md)
- [`=`](words/eq.md)

## Stack

- [`dup`](words/dup.md)
- [`swap`](words/swap.md)
- [`drop`](words/drop.md)
- [`rot`](words/rot.md)

## Control

- [`if`](words/if.md)

## Binding

- [`@`](words/bind.md)

## Evaluation

- [`!`](words/eval-bang.md)

## Introspection

- [`~`](words/describe.md)

## I/O

- [`.`](words/print.md)
- [`,`](words/comma.md)
- [`io/slurp`](words/slurp.md)
- [`eval`](words/eval.md)

## Mesh

- [`$`](words/send.md)
- [`$!`](words/send-bang.md)
- [`$?`](words/send-sync.md)
- [`sp/bus`](words/bus.md)
- [`sp/exists?`](words/exists.md)
- [`sp/alive?`](words/alive.md)

## Shell

- [`sh/!`](words/sh-bang.md)
- [`sh/>`](words/sh-capture.md)
- [`sh/|`](words/sh-pipe.md)
- [`sh/|>`](words/sh-pipe-capture.md)

## Strings (C)

- [`str/cat`](words/str-cat.md)
- [`str/len`](words/str-len.md)
- [`str/sub`](words/str-sub.md)
- [`str/ord`](words/str-ord.md)
- [`str/chr`](words/str-chr.md)
- [`str/eq`](words/str-eq.md)

## System

- [`_s`](words/stack.md)
- [`bye!`](words/bye.md)
- [`io/sleep`](words/sleep.md)
- [`require`](words/require.md)
- [`io/log`](words/log.md)

## Process (io/)

- [`io/env`](words/env.md)
- [`io/argc`](words/argc.md)
- [`io/argv`](words/argv.md)

## Words (wo/)

- [`wo/name>str`](words/name-to-str.md)
- [`wo/str>name`](words/str-to-name.md)

## Literals

- [`true`](words/true.md)
- [`false`](words/false.md)
- [`nil`](words/nil.md)

