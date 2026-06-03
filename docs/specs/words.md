# Spaceforth words — specifications

One file per built-in word. Each spec covers stack effect, semantics, an example, and known errors. The companion overview is [`docs/7-builtins.md`](../7-builtins.md).

## Arithmetic

- [`+`](words/add.md)
- [`-`](words/sub.md)
- [`*`](words/mul.md)
- [`/`](words/div.md)

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
- [`slurp`](words/slurp.md)
- [`eval`](words/eval.md)

## Mesh

- [`$`](words/send.md)
- [`$!`](words/send-bang.md)
- [`$?`](words/send-sync.md)
- [`spco/$`](words/spco-send.md)
- [`spco/$!`](words/spco-send-bang.md)
- [`spco/$?`](words/spco-send-sync.md)

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

## Strings (lib)

- [`str/empty?`](words/str-empty.md)
- [`str/head`](words/str-head.md)
- [`str/tail`](words/str-tail.md)
- [`str/reverse`](words/str-reverse.md)
- [`str/repeat`](words/str-repeat.md)
- [`str/starts-with?`](words/str-starts-with.md)
- [`str/ends-with?`](words/str-ends-with.md)
- [`str/contains?`](words/str-contains.md)
- [`str/index`](words/str-index.md)
- [`str/strip-nl`](words/str-strip-nl.md)

## Keywords

- [`:s`](words/stack.md)
- [`:bye`](words/bye.md)
- [`:sleep`](words/sleep.md)
- [`:require`](words/require.md)
- [`:log`](words/log.md)
- [`:bus`](words/bus.md)
- [`:env`](words/env.md)
- [`:argc`](words/argc.md)
- [`:argv`](words/argv.md)
- [`:exists`](words/exists.md)
- [`:alive`](words/alive.md)
- [`name>str`](words/name-to-str.md)
- [`str>name`](words/str-to-name.md)

## Literals

- [`true`](words/true.md)
- [`false`](words/false.md)
- [`nil`](words/nil.md)

