# spacelang mesh · workflow examples

How to actually use a running mesh: a driver REPL talking to worker machines.

## The mental model

You have a **bus** (a directory of Unix sockets) and **machines** (processes). Some machines are long-running **workers** sitting in their event loop. One is your **driver** — the one you type into.

```
┌─────────────────────┐  ┌─────────────────────┐
│ W1 (worker)         │  │ W2 (worker)         │
│ spci --name W1      │  │ spci --name W2      │
│     --bus DIR       │  │     --bus DIR       │
└─────────────────────┘  └─────────────────────┘
┌─────────────────────┐  ┌─────────────────────┐
│ W3 (worker)         │  │ D (driver)          │
│ spci --name W3      │  │ spci --name D       │
│     --bus DIR       │  │     --bus DIR       │
└─────────────────────┘  │ → REPL on stdin     │
                         └─────────────────────┘
```

All four are `spci --name X --bus DIR`. The driver is special only because its stdin is a TTY — the interpreter detects that and gives you a `>` prompt while still pumping mesh frames in the background.

## Setup

```bash
mkdir -p /tmp/spacelang

# in three separate terminals (or tmux panes):
./spci --name W1 --bus /tmp/spacelang
./spci --name W2 --bus /tmp/spacelang
./spci --name W3 --bus /tmp/spacelang

# in a fourth, the driver:
./spci --name D  --bus /tmp/spacelang
```

Each worker prints `[Wn] listening on /tmp/spacelang/Wn.sock` and `[Wn] ready`, then waits. The driver prints the same plus a `>` prompt.

## Examples

### 1. Local sanity check

```
> 1 2 + .
3
```

Driver still runs spacelang locally — nothing was sent.

### 2. Push a value onto W1's stack

```
> "hello from D" [W1] $
```

Nothing visible. The frame went to W1; W1 pushed the string and is now silent again. To inspect:

```
> [_s] [W1] $!
```

W1's terminal now prints its stack:

```
-- stack (1) --
  [0] "hello from D"
```

### 3. Run a computation on W2

```
> [1 2 + 10 * .] [W2] $!
```

W2's terminal prints:

```
30
```

The thunk `[1 2 + 10 * .]` is evaluated *on W2* — the `.` at the end prints on W2's stdout, not yours.

### 4. Fan-out to all three workers

```
> "ping" [W1] $   "ping" [W2] $   "ping" [W3] $
> [_s] [W1] $!   [_s] [W2] $!   [_s] [W3] $!
```

Each worker prints its stack with `"ping"` at the top.

### 5. Sync send — happy path

```
> "sync hi" [W1] 1000 $? .
t
```

`$?` sent a PUSH-with-ack frame and waited up to 1000ms. W1 acked, so the driver got `t` on its stack and printed it.

### 6. Sync send — timeout / unreachable

```
> "no one home" [Nobody] 300 $? .
0
```

No listener at `/tmp/spacelang/Nobody.sock`, so connect fails fast and `$?` pushes `0` (nil). For a real timeout — peer up but slow to ack — same result: `0`.

### 7. Define a helper on a worker, then call it

```
> [[1 2 +] [adder] @] [W3] $!     { teach W3 the word 'adder' }
> [adder !] [W3] $!               { ask W3 to run it }
> [_s] [W3] $!                    { show W3's stack }
```

W3's stack now has `3` on top. The word `adder` lives on W3, not on the driver — closures don't cross processes (see caveats below).

### 8. Distributed conditional with `$?`

```
> [work] [W1] 1000 $?
> [W1-failed] [W1-ok] if
```

If W1 acked, `t` is on the stack → `if` takes the "then" branch and `W1-ok` is left on top. Otherwise `W1-failed`. Build retries and fallbacks from this primitive.

## Caveats you'll hit

> ⚠️ **Words don't travel.** If you bind `adder` on the driver and then send `[adder !]` to W1, W1 has no `adder` and errors with `unknown word: adder`. Fix: define the helper on the worker first (see example 7), or inline the code in what you send.

> ⚠️ **Worker output goes to the worker's terminal.** When you send `[. print whatever]` to W2, the output lands on W2's stdout, not yours. The driver only sees output from things you ran locally or from `$?` results.

> ⚠️ **Peer disconnects are sticky for now.** If you Ctrl-c a worker and restart it, the driver's cached connection to it is dead. Next `$` to that name will fail. Workaround: restart the driver too, or wait for the planned auto-reconnect.

> ⚠️ **Lazy connect with retry.** If you send to a worker that hasn't started yet, `$` retries the connect for up to ~500ms before giving up. Useful in scripts that launch peers in parallel.

## Cheat sheet

| What you want | How |
|---|---|
| Push value `X` to W1's stack | `X [W1] $` |
| Run thunk `T` on W1 | `T [W1] $!` |
| Same, but wait for ack (timeout 500ms) | `T [W1] 500 $?` |
| Show W1's stack (on W1's terminal) | `[_s] [W1] $!` |
| Bind word `w` on W1 | `[V [w] @] [W1] $!` |
| Describe word `w` on W1 | `[[w] ~] [W1] $!` |
| Quit driver only | `bye!` |
| Discover live workers | `ls /tmp/spacelang/` (from shell) |

## tmux convenience

If you want all four in one window:

```bash
tmux new-session -d -s spacelang
tmux split-window -h
tmux split-window -v -t spacelang:0.0
tmux split-window -v -t spacelang:0.1

# then send each command into its pane:
tmux send-keys -t spacelang:0.0 './spci --name W1 --bus /tmp/spacelang' C-m
tmux send-keys -t spacelang:0.1 './spci --name W2 --bus /tmp/spacelang' C-m
tmux send-keys -t spacelang:0.2 './spci --name W3 --bus /tmp/spacelang' C-m
tmux send-keys -t spacelang:0.3 './spci --name D  --bus /tmp/spacelang' C-m
tmux select-pane -t spacelang:0.3
tmux attach -t spacelang
```

Layout ends up:

```
┌──────────┐  ┌──────────┐
│    W1    │  │    W2    │
└──────────┘  └──────────┘
┌──────────┐  ┌──────────┐
│    W3    │  │ D (focus)│
└──────────┘  └──────────┘
```

Navigation: `Ctrl-b` + arrows to move between panes. `Ctrl-b z` zooms a pane fullscreen. `Ctrl-b d` detaches; reattach with `tmux attach -t spacelang`.

## Teardown

```bash
tmux kill-session -t spacelang
rm -rf /tmp/spacelang
```
