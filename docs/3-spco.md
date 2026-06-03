# spco — spacelang orchestrator

A long-running daemon that manages worker processes on a mesh bus. Send it a name, it makes sure an `spci` is running for that name. Idempotent — no double-spawns.

## What it does

`spco` sits on the same mesh bus as your workers and driver. You send it `"name" spawn-node` via `$!`. It checks if a machine with that name is already alive on the bus. If not, it spawns one. If yes, it does nothing.

```
   D (driver)                           spco
   ──                                     ──
   "W1" spawn-node [spco] $! ──EVAL──►   W1 alive? → no
                                         spawn: spci --name W1 --bus DIR --serve &
                                         
   "W1" spawn-node [spco] $! ──EVAL──►   W1 alive? → yes (skip)
```

## Why it exists

Without `spco`, you start workers by hand — opening terminals, typing `spci --name W1 --bus DIR`, keeping track of which are up, restarting them when they crash. This doesn't scale past two or three machines.

`spco` takes that manual loop and turns it into a single send:

- **One command to spawn.** No terminals, no shell juggling. Any peer on the mesh can ask for a worker.
- **Idempotent.** You can fire `spawn-node` for the same name a hundred times — only the first one spawns. Startup scripts become safe to re-run.
- **Self-healing.** Worker crashed? Send `spawn-node` again. `spco` notices the socket is gone and respawns it. No need to know which workers died or why.
- **Dogfoods the mesh.** `spco` isn't a privileged supervisor process — it's just another peer receiving `$!` frames. The same protocol you use to talk to workers is how you talk to the orchestrator.

## Usage

Start the orchestrator alongside your workers:

```bash
./spco --bus /tmp/spacelang --serve
```

### Raw (direct `$!` to spco)

From your driver, spawn workers on demand:

```
> "W1" spawn-node [spco] $!
> "W2" spawn-node [spco] $!
```

Now `W1` and `W2` are running (check with `ls /tmp/spacelang/`). You can use them immediately:

```
> "hello" [W1] $
> [:s] [W1] $!
```

If a worker crashes, send `spawn-node` again — `spco` will notice it's gone and respawn.

### With `with-spco.sp` (automatic ensure)

Requiring `stdlib/with-spco.sp` gives you `spco/$`, `spco/$!`, and `spco/$?`. These wrap the standard send operators with an automatic spawn-node check — no need to manually call `spawn-node` before sending.

```
> "with-spco.sp" :require
> "hello" [W1] spco/$        { W1 auto-spawned if missing, then message delivered }
> [1 2 + .] [W2] spco/$!      { W2 auto-spawned, thunk evaluated }
```

All three have the same observed stack effect as the originals:

```
msg  [name] spco/$               { same as $ }
term [name] spco/$!              { same as $! }
term [name] timeout-ms spco/$?   { same as $? }
```

`via-spco` is a backward-compatible alias for `spco/$`.

## How it works

`spco` itself is written in `spacelang`, packaged by the `spcc` compiler, and is distributed with it.

## Why a separate binary?

You could type `spawn-node` in the driver REPL directly. `spco` exists because:

- **Automation.** Scripts and other machines can ask for workers without a human at the keyboard.
- **Idempotent bootstrap.** A startup script can fire off `"*" spawn-node [spco] $!` for every expected worker — duplicates are no-ops.
- **Supervisor lite.** Restart `spco` after a crash and it re-spawns missing workers.

## Relationship to the mesh

`spco` is just another peer on the bus. It has no special privileges. The `spawn-node` word is bound in its own word table — it's not a built-in, just spacelang code that happens to use `sh/!` and `:alive`.
