# spci — spacelang interpreter

The interactive interpreter and runtime engine. Runs `.sp` source files, provides a REPL, and powers the mesh transport layer. Importantly spci is single threaded.

## What it does

`spci` is the heart of spacelang. It parses and evaluates spacelang source — numbers, strings, words, thunks, and all built-in operators. It can run as:

- **Batch mode.** `spci hello.sp` — loads and evaluates the file, then exits.
- **REPL.** `spci` (no args) — drops you into a `>` prompt. Type spacelang, get results.
- **Mesh node.** `spci --name X --bus DIR` — becomes a named peer on a mesh bus, listening for `$`/`$!`/`$?` frames from other machines while optionally running source or a REPL.

## CLI

```
spci [flags] [file.sp ...]
```

| Flag | Effect |
|---|---|
| `file.sp` | Load and evaluate each file in order |
| `--name X` | Mesh identity (requires `--bus`) |
| `--bus DIR` | Unix socket directory for the mesh |
| `--serve` | Keep mesh alive after files processed (no REPL) |
| `--version`, `-V` | Print version and exit |
| `--` | Everything after becomes `io/argv` for the program |

### Modes at a glance

```
spci hello.sp                           batch: run file, exit
spci                                    REPL: prompt on stdin
spci --name W1 --bus /tmp/spacelang     mesh node with REPL
spci hello.sp --name W --bus DIR --serve   mesh node, run file, stay alive
```

## Mesh transport

When run with `--name` and `--bus`, `spci` listens on `$BUS/<name>.sock` and accepts connections from peers. The first `$` to a peer lazily `connect()`s and caches the fd. Three send operators:

All I/O is single-threaded `poll()` — no concurrency bugs.
