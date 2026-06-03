# spacelang × Unix — inter-machine communication

Using Unix domain sockets for inter-machine communication, so the kernel is the scheduler.

## 1. The core idea

A spacelang **machine** becomes a Unix **process**. The `$` (send) and `$!` (send-eval) operators become `write()` calls into another process's socket. The kernel handles blocking, scheduling, and backpressure — you write no event loop.

```
         stdin (fd 0)                   stdout (fd 1)
    incoming frames ──┐              ┌── outgoing frames
                       ▼              ▲
   ┌───────────────────────────────────────┐
   │    spacelang machine "A"              │
   │    (one Unix process)                 │
   │                                       │
   │   ┌────────┐   ┌──────────────────┐   │
   │   │ stack  │   │ word table       │   │
   │   │        │   │ name → thunk     │   │
   │   └────────┘   └──────────────────┘   │
   │                                       │
   └───────────────────────────────────────┘
                       │
              stderr (fd 2): logs
```

A machine reads frames from its socket, writes frames to peer sockets, logs to stderr.

## 2. Topology: full mesh, no supervisor

We considered three shapes — **star** (one supervisor routes everything), **broadcast** (everyone hears everyone), and **mesh** (every pair has a direct connection). We picked **mesh**: it matches the language's semantic model (machines as independent universes, not children of a hub), has no single point of failure, and the protocol stays symmetric.

Discovery uses the filesystem. Each machine listens on a Unix domain socket at `/tmp/spacelang/<name>.sock`. Adding a peer is just starting one; discovering peers is `ls /tmp/spacelang/`. Connections are lazy: the first time A sends to B, A calls `connect()` and caches the fd.

```
                ┌─────────────────┐
                │    machine A    │
                │ A.sock          │
                └───┬───────┬─────┘
                    │       │
           ┌────────┘       └────────┐
           ▼                         ▼
   ┌─────────────────┐     ┌─────────────────┐
   │    machine B    │◄───►│    machine C    │
   │ B.sock          │     │ C.sock          │
   └─────────────────┘     └─────────────────┘
            ▲                       ▲
            └───────────────────────┘

   discovery: ls /tmp/spacelang/
   connect() is lazy + cached
```

Every pair has a direct Unix-socket connection. No supervisor.

## 3. The wire frame

Every message is length-prefixed so the receiver knows where one frame ends and the next begins. The payload is just spacelang source text — your existing parser handles it.

```
┌──────┬────────────┬───────────────────────────────────────────┐
│ TAG  │  LENGTH    │  PAYLOAD (utf-8 spacelang source)        │
│ 1 B  │  4 B (u32) │  N bytes — e.g. "hello" or [1 2 +]      │
└──────┴────────────┴───────────────────────────────────────────┘
```

5-byte header, variable payload. The payload is plain source — thunks ship as their bracketed text.

## 4. Three send operators: `$`, `$!`, `$?`

Different tag, different semantics:

- **PUSH** (`$`): fire-and-forget. Receiver parses and *pushes*. Sender doesn't wait.
- **EVAL** (`$!`): fire-and-forget. Receiver parses and *evaluates*.
- **SYNC** (`$?`): send-and-wait. Pops `term name timeout-ms`, blocks until the receiver acks or the timeout expires. Pushes `t` on ack, `nil` on timeout.

Why two fire-and-forget operators (`$` and `$!`) instead of one with a flag? The semantics are genuinely different — pushing vs evaluating affects the receiver's behavior. Concatenative languages prefer many small named operators over one configurable one.

```
   A                                          B
   ──                                         ──
   [1 2 +] [B] $ ────── PUSH "[1 2 +]" ────► stack ← [1 2 +]
   
   [1 2 +] [B] $! ───── EVAL "[1 2 +]" ────► stack ← 3
   
   [1 2 +] [B] 500 $? ── PUSH+id "[1 2 +]" ─► stack ← [1 2 +]
                       ◄── ACK id ──────────── (or timeout → nil)
```

Same wire, three tags. `$?` adds a frame id and waits for the ack frame back.

### The four levels of "send completed"

`$?` exists because `$` only guarantees the kernel accepted the bytes — not that the receiver processed them. Pick the level you need:

```
  1. Queued          2. Delivered        3. Parsed + Acked    4. Evaluated
  ┌──────────┐      ┌──────────┐        ┌──────────────┐    ┌──────────────┐
  │write()   │      │receiver  │        │term on       │    │eval returned │
  │returned  │ ───► │read()    │ ────►  │receiver's    │ ──►│(RPC,         │
  │          │      │(back-    │        │stack         │    │ deadlocks)   │
  │→ $       │      │ pressure)│        │→ $?          │    │              │
  └──────────┘      └──────────┘        └──────────────┘    └──────────────┘
  ◄──────────────────────────────────────────────────────────────────────►
  more guarantee · more cost · more deadlock risk
```

`$` sits at level 1, `$?` at level 3. Level 4 (synchronous RPC) is intentionally not a primitive — users can build it from `$?` + a return channel.

## 5. Why this is cheap to build

- **No scheduler.** `read()` blocks; the OS parks the process; another machine runs.
- **Backpressure for free.** A full pipe buffer makes the sender's `write()` block.
- **Crash isolation.** One machine segfaults; the others keep running.
- **Observable.** `strace -f` shows every send as a real `write()` with bytes inline.
- **Prototype before coding.** `socat` can wire two machines together in one shell line.

### Prototype it with socat first

```bash
socat -v EXEC:"./spci --name A" EXEC:"./spci --name B"
```

`-v` dumps every byte to stderr so you can watch your frames flow before writing any C supervisor code.

## 6. Caveats

> ⚠️ **Closures don't cross processes.** If A sends a thunk `[foo bar]` and `foo` is only bound in A's word table, B has no idea what `foo` means. Three ways out:
> - Declare it a feature — sent code must be self-contained. *(Spacelang-ish.)*
> - Inline known bindings before sending (a tiny "linker" pass).
> - Ship a bytecode with embedded constants.

> ⚠️ **Per-pair ordering only.** A→B is FIFO. But A→B and C→B arriving at B have no global order. Same as any actor system.

> ⚠️ **Process spawn is milliseconds.** Fine for long-lived machines. Don't `fork+exec` per `$!`; keep machines persistent.
