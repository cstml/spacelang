/*
 * spci — spacelang interpreter in C, with mesh transport.
 *
 * Single-process subset:
 *   numbers, strings, words, thunks [...], booleans
 *   + - * / < > <= >= =
 *   dup swap drop if
 *   @ (bind)   ! (eval)   . (print)   , (format)   ~ (describe)
 *   slurp (read line from stdin → string)   eval (pop string → feed)
 *   :s (print stack)   :bye (exit)   { comments }
 *
 * Inter-machine (with --name X --bus DIR):
 *   $  (send PUSH, fire-and-forget)
 *   $! (send EVAL, fire-and-forget)
 *   $? (send PUSH with timeout-ms; pushes t on ack, nil on timeout)
 *
 * Mesh topology: each machine listens on $BUS/<name>.sock and lazily
 * connect()s to peers on first send. No supervisor.
 *
 * Build: cc -O2 -Wall -Wextra -o spci spci.c
 * Run  : ./spci file.sp                            # batch
 *        ./spci                                    # REPL
 *        ./spci --name A --bus /tmp/spacelang      # mesh node (+ optional file)
 */

#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <time.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/stat.h>

#include "spci.h"

/* ---------- values ---------- */

typedef enum { V_NUM, V_STR, V_WORD, V_THUNK, V_BOOL } VType;

typedef struct Value {
    VType type;
    int refs;
    union {
        long num;
        char *str;
        char *word;
        int boolean;
        struct { struct Value **items; size_t len, cap; } thunk;
    } as;
} Value;

static Value *v_new(VType t) {
    Value *v = calloc(1, sizeof *v);
    v->type = t; v->refs = 1; return v;
}
static Value *v_ref(Value *v) { if (v) v->refs++; return v; }
static void   v_unref(Value *v);

static Value *v_num(long n)   { Value *v = v_new(V_NUM);  v->as.num = n; return v; }
static Value *v_bool(int b)   { Value *v = v_new(V_BOOL); v->as.boolean = b; return v; }
static Value *v_str(const char *s) {
    Value *v = v_new(V_STR); v->as.str = strdup(s); return v;
}
static Value *v_word(const char *s) {
    Value *v = v_new(V_WORD); v->as.word = strdup(s); return v;
}
static Value *v_thunk(void) {
    Value *v = v_new(V_THUNK);
    v->as.thunk.items = NULL; v->as.thunk.len = 0; v->as.thunk.cap = 0;
    return v;
}
static void thunk_push(Value *t, Value *item) {
    if (t->as.thunk.len == t->as.thunk.cap) {
        t->as.thunk.cap = t->as.thunk.cap ? t->as.thunk.cap * 2 : 4;
        t->as.thunk.items = realloc(t->as.thunk.items,
                                    t->as.thunk.cap * sizeof(Value *));
    }
    t->as.thunk.items[t->as.thunk.len++] = item;
}
static void v_unref(Value *v) {
    if (!v) return;
    if (--v->refs > 0) return;
    switch (v->type) {
        case V_STR:   free(v->as.str); break;
        case V_WORD:  free(v->as.word); break;
        case V_THUNK:
            for (size_t i = 0; i < v->as.thunk.len; i++)
                v_unref(v->as.thunk.items[i]);
            free(v->as.thunk.items);
            break;
        default: break;
    }
    free(v);
}

/* deep-ish copy: numbers/bools share, strings/words/thunks dup so
 * mutating one bound thunk doesn't reach back through callers. */
static Value *v_clone(Value *v) {
    if (!v) return NULL;
    switch (v->type) {
        case V_NUM:   return v_num(v->as.num);
        case V_BOOL:  return v_bool(v->as.boolean);
        case V_STR:   return v_str(v->as.str);
        case V_WORD:  return v_word(v->as.word);
        case V_THUNK: {
            Value *t = v_thunk();
            for (size_t i = 0; i < v->as.thunk.len; i++)
                thunk_push(t, v_clone(v->as.thunk.items[i]));
            return t;
        }
    }
    return NULL;
}

/* ---------- stack ---------- */

typedef struct { Value **items; size_t len, cap; } Stack;
static Stack S;

static void push(Value *v) {
    if (S.len == S.cap) {
        S.cap = S.cap ? S.cap * 2 : 64;
        S.items = realloc(S.items, S.cap * sizeof(Value *));
    }
    S.items[S.len++] = v;
}
static Value *pop(void) {
    if (S.len == 0) { fprintf(stderr, "stack underflow\n"); exit(1); }
    return S.items[--S.len];
}

/* ---------- word table (flat, fine for v1) ---------- */

typedef struct { char *name; Value *val; } Binding;
static Binding *W = NULL;
static size_t W_len = 0, W_cap = 0;

static void word_set(const char *name, Value *val) {
    for (size_t i = 0; i < W_len; i++) {
        if (strcmp(W[i].name, name) == 0) {
            v_unref(W[i].val);
            W[i].val = val;
            return;
        }
    }
    if (W_len == W_cap) {
        W_cap = W_cap ? W_cap * 2 : 32;
        W = realloc(W, W_cap * sizeof(Binding));
    }
    W[W_len].name = strdup(name);
    W[W_len].val  = val;
    W_len++;
}
static Value *word_get(const char *name) {
    for (size_t i = 0; i < W_len; i++)
        if (strcmp(W[i].name, name) == 0) return W[i].val;
    return NULL;
}

/* ---------- printing ---------- */

static void pretty(Value *v) {
    if (!v) { printf("nil"); return; }
    switch (v->type) {
        case V_NUM:  printf("%ld", v->as.num); break;
        case V_BOOL: printf(v->as.boolean ? "t" : "nil"); break;
        case V_STR:  printf("\"%s\"", v->as.str); break;
        case V_WORD: printf("%s", v->as.word); break;
        case V_THUNK:
            printf("[");
            for (size_t i = 0; i < v->as.thunk.len; i++) {
                if (i) printf(" ");
                pretty(v->as.thunk.items[i]);
            }
            printf("]");
            break;
    }
}
static void print_stack(void) {
    printf("-- stack (%zu) --\n", S.len);
    for (size_t i = 0; i < S.len; i++) {
        printf("  [%zu] ", i);
        pretty(S.items[i]);
        printf("\n");
    }
}

/* ---------- value → source string ---------- */

typedef struct { char *buf; size_t len, cap; } SBuf;
static void sb_putc(SBuf *s, char c) {
    if (s->len + 2 > s->cap) {
        s->cap = s->cap ? s->cap * 2 : 64;
        s->buf = realloc(s->buf, s->cap);
    }
    s->buf[s->len++] = c;
    s->buf[s->len] = 0;
}
static void sb_puts(SBuf *s, const char *str) {
    while (*str) sb_putc(s, *str++);
}
static void sb_printf(SBuf *s, const char *fmt, ...) {
    char tmp[64];
    va_list ap; va_start(ap, fmt);
    vsnprintf(tmp, sizeof tmp, fmt, ap);
    va_end(ap);
    sb_puts(s, tmp);
}
static void format_value(SBuf *s, Value *v) {
    if (!v) { sb_puts(s, "nil"); return; }
    switch (v->type) {
        case V_NUM:  sb_printf(s, "%ld", v->as.num); break;
        case V_BOOL: sb_puts(s, v->as.boolean ? "true" : "false"); break;
        case V_STR:  sb_putc(s, '"'); sb_puts(s, v->as.str); sb_putc(s, '"'); break;
        case V_WORD: sb_puts(s, v->as.word); break;
        case V_THUNK:
            sb_putc(s, '[');
            for (size_t i = 0; i < v->as.thunk.len; i++) {
                if (i) sb_putc(s, ' ');
                format_value(s, v->as.thunk.items[i]);
            }
            sb_putc(s, ']');
            break;
    }
}

/* ---------- frame I/O ---------- (TAG_* defined in spci.h) */

static int read_n(int fd, void *buf, size_t n) {
    size_t got = 0;
    while (got < n) {
        ssize_t r = read(fd, (char*)buf + got, n - got);
        if (r == 0) return -1;
        if (r < 0) { if (errno == EINTR) continue; return -1; }
        got += r;
    }
    return 0;
}
static int write_all(int fd, const void *buf, size_t n) {
    size_t sent = 0;
    while (sent < n) {
        ssize_t w = write(fd, (const char*)buf + sent, n - sent);
        if (w < 0) { if (errno == EINTR) continue; return -1; }
        sent += w;
    }
    return 0;
}
int frame_write(int fd, uint8_t tag, uint32_t id, const char *payload, uint32_t len) {
    uint8_t hdr[9];
    hdr[0] = tag;
    hdr[1] = id >> 24; hdr[2] = id >> 16; hdr[3] = id >> 8; hdr[4] = id;
    hdr[5] = len >> 24; hdr[6] = len >> 16; hdr[7] = len >> 8; hdr[8] = len;
    if (write_all(fd, hdr, 9) < 0) return -1;
    if (len && write_all(fd, payload, len) < 0) return -1;
    return 0;
}
int frame_read(int fd, uint8_t *tag, uint32_t *id, char **payload, uint32_t *len) {
    uint8_t hdr[9];
    if (read_n(fd, hdr, 9) < 0) return -1;
    *tag = hdr[0];
    *id  = ((uint32_t)hdr[1]<<24)|((uint32_t)hdr[2]<<16)|((uint32_t)hdr[3]<<8)|hdr[4];
    *len = ((uint32_t)hdr[5]<<24)|((uint32_t)hdr[6]<<16)|((uint32_t)hdr[7]<<8)|hdr[8];
    if (*len > 16 * 1024 * 1024) return -1;  /* sanity cap: 16 MiB */
    if (*len) {
        *payload = malloc(*len + 1);
        if (read_n(fd, *payload, *len) < 0) { free(*payload); return -1; }
        (*payload)[*len] = 0;
    } else {
        *payload = NULL;
    }
    return 0;
}

/* ---------- mesh state ---------- */

char  *my_name = NULL;
char  *bus_dir = NULL;
int    listen_fd = -1;

/* Peer typedef now in spci.h */

Peer  *peers = NULL;
size_t peers_len = 0;
static size_t peers_cap = 0;

Peer *peer_add(int fd, const char *name, int outgoing) {
    if (peers_len == peers_cap) {
        peers_cap = peers_cap ? peers_cap * 2 : 8;
        peers = realloc(peers, peers_cap * sizeof(Peer));
    }
    peers[peers_len] = (Peer){ name ? strdup(name) : NULL, fd, outgoing };
    return &peers[peers_len++];
}
void peer_drop(size_t i) {
    if (peers[i].fd >= 0) close(peers[i].fd);
    free(peers[i].name);
    peers[i] = peers[peers_len - 1];
    peers_len--;
}
static Peer *peer_find(const char *name) {
    if (!name) return NULL;
    for (size_t i = 0; i < peers_len; i++)
        if (peers[i].name && strcmp(peers[i].name, name) == 0) return &peers[i];
    return NULL;
}

/* connect_to_path: open a stream socket and connect, with retry. */
static int connect_to_path(const char *path, int max_tries) {
    int fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (fd < 0) return -1;
    struct sockaddr_un addr = {0};
    addr.sun_family = AF_UNIX;
    snprintf(addr.sun_path, sizeof addr.sun_path, "%s", path);
    int tries = 0;
    while (connect(fd, (struct sockaddr*)&addr, sizeof addr) < 0) {
        if (++tries > max_tries) { close(fd); return -1; }
        struct timespec ts = { 0, 25 * 1000 * 1000 };
        nanosleep(&ts, NULL);
    }
    return fd;
}

/* Ask spco at $BUS/spco.sock to resolve `name`. Returns connected fd to
 * the resolved socket, or -1 if spco is absent / refuses / spawn fails. */
static int spco_lookup_and_connect(const char *name) {
    char spath[104];
    snprintf(spath, sizeof spath, "%s/spco.sock", bus_dir);
    int sfd = connect_to_path(spath, 0);
    if (sfd < 0) return -1;
    if (frame_write(sfd, TAG_LOOKUP, 0, name, strlen(name)) < 0) {
        close(sfd); return -1;
    }
    uint8_t tag; uint32_t id, len; char *payload = NULL;
    if (frame_read(sfd, &tag, &id, &payload, &len) < 0) { close(sfd); return -1; }
    close(sfd);
    if (tag != TAG_ADDR || !payload) { free(payload); return -1; }
    int dfd = connect_to_path(payload, 20);
    free(payload);
    return dfd;
}

/* connect to /tmp/spacelang/<name>.sock, send HELLO, return new peer or NULL.
 * If direct connect fails, fall back to asking spco at $BUS/spco.sock. */
static Peer *peer_connect(const char *name) {
    if (!bus_dir || !my_name) return NULL;
    char path[104];
    snprintf(path, sizeof path, "%s/%s.sock", bus_dir, name);
    int fd = connect_to_path(path, 20);  /* try direct first */
    if (fd < 0) fd = spco_lookup_and_connect(name);
    if (fd < 0) return NULL;
    if (frame_write(fd, TAG_HELLO, 0, my_name, strlen(my_name)) < 0) {
        close(fd); return NULL;
    }
    return peer_add(fd, name, 1);
}

/* ---------- pending acks for $? ---------- */

typedef struct { uint32_t id; int status; /* 0 pending, 1 acked */ } Pending;
static Pending *pendings = NULL;
static size_t pendings_len = 0, pendings_cap = 0;
static uint32_t next_frame_id = 1;

static void pending_add(uint32_t id) {
    if (pendings_len == pendings_cap) {
        pendings_cap = pendings_cap ? pendings_cap * 2 : 16;
        pendings = realloc(pendings, pendings_cap * sizeof(Pending));
    }
    pendings[pendings_len++] = (Pending){ id, 0 };
}
static int pending_status(uint32_t id) {
    for (size_t i = 0; i < pendings_len; i++)
        if (pendings[i].id == id) return pendings[i].status;
    return -1;
}
static void pending_ack(uint32_t id) {
    for (size_t i = 0; i < pendings_len; i++)
        if (pendings[i].id == id) { pendings[i].status = 1; return; }
}
static void pending_remove(uint32_t id) {
    for (size_t i = 0; i < pendings_len; i++)
        if (pendings[i].id == id) {
            pendings[i] = pendings[pendings_len - 1];
            pendings_len--;
            return;
        }
}

/* forward decl: process one frame's payload through the interpreter */
/* feed declared in spci.h */

/* dispatch a frame we just received */
void on_frame(Peer *p, uint8_t tag, uint32_t id, char *payload, uint32_t len) {
    switch (tag) {
        case TAG_HELLO:
            free(p->name);
            p->name = payload ? strndup(payload, len) : strdup("?");
            break;
        case TAG_PUSH:
        case TAG_EVAL:
            if (payload) feed(payload);
            if (tag == TAG_EVAL) feed("!");
            break;
        case TAG_SYNC:
            if (payload) feed(payload);
            /* ack on the same connection */
            frame_write(p->fd, TAG_ACK, id, NULL, 0);
            break;
        case TAG_ACK:
            pending_ack(id);
            break;
    }
    free(payload);
}

/* pump readable peer fds + accept new connections; non-blocking poll w/ timeout_ms */
int mesh_poll(int timeout_ms) {
    if (listen_fd < 0) return 0;
    struct pollfd pfds[64];
    size_t n = 0;
    pfds[n].fd = listen_fd; pfds[n].events = POLLIN; n++;
    for (size_t i = 0; i < peers_len && n < 64; i++) {
        pfds[n].fd = peers[i].fd; pfds[n].events = POLLIN; n++;
    }
    int r = poll(pfds, n, timeout_ms);
    if (r <= 0) return r;

    /* accept */
    if (pfds[0].revents & POLLIN) {
        int cfd = accept(listen_fd, NULL, NULL);
        if (cfd >= 0) peer_add(cfd, NULL, 0);
    }
    /* iterate carefully — peer indices may change on disconnect */
    for (size_t pi = 1; pi < n; pi++) {
        if (!(pfds[pi].revents & (POLLIN|POLLHUP|POLLERR))) continue;
        /* find the peer by fd (index may have shifted) */
        size_t found = (size_t)-1;
        for (size_t j = 0; j < peers_len; j++)
            if (peers[j].fd == pfds[pi].fd) { found = j; break; }
        if (found == (size_t)-1) continue;

        uint8_t tag; uint32_t id, len; char *payload = NULL;
        if (frame_read(peers[found].fd, &tag, &id, &payload, &len) < 0) {
            peer_drop(found);
            continue;
        }
        on_frame(&peers[found], tag, id, payload, len);
    }
    return r;
}

/* set up the listen socket at $BUS/$NAME.sock */
int mesh_listen(void) {
    mkdir(bus_dir, 0700);
    char path[104];
    snprintf(path, sizeof path, "%s/%s.sock", bus_dir, my_name);
    unlink(path);  /* clean stale */
    int fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (fd < 0) { perror("socket"); return -1; }
    struct sockaddr_un addr = {0};
    addr.sun_family = AF_UNIX;
    snprintf(addr.sun_path, sizeof addr.sun_path, "%s", path);
    if (bind(fd, (struct sockaddr*)&addr, sizeof addr) < 0) { perror("bind"); close(fd); return -1; }
    if (listen(fd, 16) < 0) { perror("listen"); close(fd); return -1; }
    listen_fd = fd;
    fprintf(stderr, "[%s] listening on %s\n", my_name, path);
    return 0;
}

/* ---------- tokenizer / parser ---------- */

typedef struct { const char *src; size_t i, n; } Lex;

static void skip_ws(Lex *L) {
    for (;;) {
        while (L->i < L->n && isspace((unsigned char)L->src[L->i])) L->i++;
        if (L->i < L->n && L->src[L->i] == '{') {
            while (L->i < L->n && L->src[L->i] != '}') L->i++;
            if (L->i < L->n) L->i++;
            continue;
        }
        break;
    }
}

static Value *parse_term(Lex *L);

static Value *parse_thunk(Lex *L) {
    Value *t = v_thunk();
    L->i++; /* consume [ */
    for (;;) {
        skip_ws(L);
        if (L->i >= L->n) {
            fprintf(stderr, "unterminated thunk\n"); exit(1);
        }
        if (L->src[L->i] == ']') { L->i++; return t; }
        Value *child = parse_term(L);
        if (child) thunk_push(t, child);
    }
}

static Value *parse_string(Lex *L, char quote) {
    L->i++; /* opener */
    size_t start = L->i;
    while (L->i < L->n && L->src[L->i] != quote) L->i++;
    size_t end = L->i;
    if (L->i < L->n) L->i++; /* closer */
    char *buf = malloc(end - start + 1);
    memcpy(buf, L->src + start, end - start);
    buf[end - start] = 0;
    Value *v = v_str(buf);
    free(buf);
    return v;
}

static Value *parse_term(Lex *L) {
    skip_ws(L);
    if (L->i >= L->n) return NULL;
    char c = L->src[L->i];

    if (c == '[') return parse_thunk(L);
    if (c == '"' || c == '\'') return parse_string(L, c);

    if (isdigit((unsigned char)c) ||
        (c == '-' && L->i + 1 < L->n && isdigit((unsigned char)L->src[L->i+1]))) {
        size_t start = L->i;
        if (c == '-') L->i++;
        while (L->i < L->n && isdigit((unsigned char)L->src[L->i])) L->i++;
        char *end;
        long n = strtol(L->src + start, &end, 10);
        return v_num(n);
    }

    /* multi-char operators */
    if ((c == '<' || c == '>') && L->i + 1 < L->n && L->src[L->i+1] == '=') {
        char buf[3] = { c, '=', 0 };
        L->i += 2;
        return v_word(buf);
    }
    if (c == '$' && L->i + 1 < L->n && L->src[L->i+1] == '!') {
        L->i += 2; return v_word("$!");
    }
    if (c == '$' && L->i + 1 < L->n && L->src[L->i+1] == '?') {
        L->i += 2; return v_word("$?");
    }

    /* :keyword */
    if (c == ':' && L->i + 1 < L->n &&
        (isalpha((unsigned char)L->src[L->i+1]) || L->src[L->i+1] == '_')) {
        size_t start = L->i;
        L->i++;
        while (L->i < L->n &&
               (isalnum((unsigned char)L->src[L->i]) || L->src[L->i] == '_'))
            L->i++;
        char *buf = malloc(L->i - start + 1);
        memcpy(buf, L->src + start, L->i - start);
        buf[L->i - start] = 0;
        Value *v = v_word(buf);
        free(buf);
        return v;
    }

    /* single-char ops / punctuation become words too */
    if (strchr("+-*/<>=@!.,~$:]", c)) {
        char buf[2] = { c, 0 };
        L->i++;
        return v_word(buf);
    }

    /* identifier */
    if (isalpha((unsigned char)c) || c == '_') {
        size_t start = L->i;
        while (L->i < L->n &&
               (isalnum((unsigned char)L->src[L->i]) || L->src[L->i] == '_'))
            L->i++;
        char *buf = malloc(L->i - start + 1);
        memcpy(buf, L->src + start, L->i - start);
        buf[L->i - start] = 0;
        Value *v = v_word(buf);
        free(buf);
        return v;
    }

    fprintf(stderr, "parser: unknown char '%c' at %zu\n", c, L->i);
    L->i++;
    return NULL;
}

/* ---------- eval ---------- */

static int truthy(Value *v) {
    if (!v) return 0;
    switch (v->type) {
        case V_NUM:   return v->as.num != 0;
        case V_BOOL:  return v->as.boolean;
        case V_STR:   return v->as.str[0] != 0;
        case V_WORD:  return 1;
        case V_THUNK: return v->as.thunk.len != 0;
    }
    return 0;
}

static void eval(Value *t);

/* run every subterm of a thunk in order (the spacelang `!` semantics
 * for evaluating a cons). */
static void run_thunk(Value *t) {
    for (size_t i = 0; i < t->as.thunk.len; i++) {
        eval(v_clone(t->as.thunk.items[i]));
    }
}

/* binary numeric op */
static long n_of(Value *v) {
    if (v->type != V_NUM) { fprintf(stderr, "expected number\n"); exit(1); }
    return v->as.num;
}
static void bin_num(long (*f)(long,long)) {
    Value *b = pop(), *a = pop();
    Value *r = v_num(f(n_of(a), n_of(b)));
    v_unref(a); v_unref(b);
    push(r);
}
static void bin_cmp(int (*f)(long,long)) {
    Value *b = pop(), *a = pop();
    Value *r = f(n_of(a), n_of(b)) ? v_bool(1) : v_num(0);
    v_unref(a); v_unref(b);
    push(r);
}
static long add(long a,long b){return a+b;}
static long sub(long a,long b){return a-b;}
static long mul(long a,long b){return a*b;}
static long divv(long a,long b){return a/b;}
static int  lt(long a,long b){return a<b;}
static int  gt(long a,long b){return a>b;}
static int  le(long a,long b){return a<=b;}
static int  ge(long a,long b){return a>=b;}
static int  eq(long a,long b){return a==b;}

/* a "binding form" is a thunk of length 1 whose single element is a word —
 * spacelang's [foo] @ idiom. Return the word name or NULL. */
static const char *binding_name(Value *v) {
    if (!v || v->type != V_THUNK || v->as.thunk.len != 1) return NULL;
    Value *inner = v->as.thunk.items[0];
    if (!inner || inner->type != V_WORD) return NULL;
    return inner->as.word;
}

static void eval_word(const char *w) {
    /* arithmetic / comparison */
    if (!strcmp(w,"+")) { bin_num(add); return; }
    if (!strcmp(w,"-")) { bin_num(sub); return; }
    if (!strcmp(w,"*")) { bin_num(mul); return; }
    if (!strcmp(w,"/")) { bin_num(divv); return; }
    if (!strcmp(w,"<")) { bin_cmp(lt); return; }
    if (!strcmp(w,">")) { bin_cmp(gt); return; }
    if (!strcmp(w,"<=")){ bin_cmp(le); return; }
    if (!strcmp(w,">=")){ bin_cmp(ge); return; }
    if (!strcmp(w,"=")) { bin_cmp(eq); return; }

    /* stack */
    if (!strcmp(w,"dup"))  { Value *x = pop(); push(x); push(v_ref(x)); return; }
    if (!strcmp(w,"swap")) { Value *a = pop(), *b = pop(); push(a); push(b); return; }
    if (!strcmp(w,"drop")) { v_unref(pop()); return; }

    /* if: pops cond, then-branch, else-branch (top to bottom) */
    if (!strcmp(w,"if")) {
        Value *c = pop(), *th = pop(), *el = pop();
        push(truthy(c) ? th : el);
        v_unref(truthy(c) ? el : th);
        v_unref(c);
        return;
    }

    /* @ bind: pop [name], pop term, set */
    if (!strcmp(w,"@")) {
        Value *binder = pop();
        Value *term   = pop();
        const char *name = binding_name(binder);
        if (!name) {
            fprintf(stderr, "@ expects [name] on top, got: ");
            pretty(binder); fprintf(stderr, "\n");
            v_unref(binder); v_unref(term);
            return;
        }
        word_set(name, term);
        v_unref(binder);
        return;
    }

    /* ! eval: pop top, evaluate it */
    if (!strcmp(w,"!")) {
        Value *t = pop();
        if (t && t->type == V_THUNK) {
            run_thunk(t); v_unref(t);
        } else {
            eval(t);
        }
        return;
    }

    /* slurp: read one line from stdin (sans trailing newline), push as string */
    if (!strcmp(w, "slurp")) {
        char line[4096];
        if (fgets(line, sizeof line, stdin)) {
            size_t n = strlen(line);
            if (n && line[n-1] == '\n') line[--n] = 0;
            push(v_str(line));
        } else {
            push(v_str(""));
        }
        return;
    }
    /* eval: pop a string, feed it to the interpreter */
    if (!strcmp(w, "eval")) {
        Value *t = pop();
        if (t && t->type == V_STR) feed(t->as.str);
        else fprintf(stderr, "eval: expected string\n");
        v_unref(t);
        return;
    }

    /* . print */
    if (!strcmp(w,".")) {
        Value *t = pop(); pretty(t); printf("\n"); v_unref(t); return;
    }
    /* , format — same as print for our value repr */
    if (!strcmp(w,",")) {
        Value *t = pop(); pretty(t); printf("\n"); v_unref(t); return;
    }
    /* ~ describe */
    if (!strcmp(w,"~")) {
        Value *binder = pop();
        const char *name = binding_name(binder);
        if (name) {
            Value *bound = word_get(name);
            printf("%s ~ ", name);
            if (bound) pretty(bound); else printf("<unbound>");
            printf("\n");
        }
        v_unref(binder);
        return;
    }

    /* $ $! $? — only meaningful in --name mode */
    if (!strcmp(w,"$") || !strcmp(w,"$!") || !strcmp(w,"$?")) {
        int is_sync = !strcmp(w, "$?");
        int is_eval = !strcmp(w, "$!");
        long timeout_ms = 0;
        if (is_sync) {
            Value *t = pop();
            timeout_ms = n_of(t); v_unref(t);
        }
        Value *binder = pop();
        Value *term   = pop();
        const char *name = binding_name(binder);
        if (!name) {
            fprintf(stderr, "%s expects [name] as destination\n", w);
            v_unref(binder); v_unref(term);
            if (is_sync) push(v_num(0));
            return;
        }
        if (!my_name) {
            fprintf(stderr, "%s: not in --name mode\n", w);
            v_unref(binder); v_unref(term);
            if (is_sync) push(v_num(0));
            return;
        }
        Peer *p = peer_find(name);
        if (!p) p = peer_connect(name);
        if (!p) {
            fprintf(stderr, "%s: cannot reach peer '%s'\n", w, name);
            v_unref(binder); v_unref(term);
            if (is_sync) push(v_num(0));
            return;
        }

        SBuf sb = {0};
        format_value(&sb, term);
        v_unref(binder); v_unref(term);

        uint8_t tag = is_sync ? TAG_SYNC : (is_eval ? TAG_EVAL : TAG_PUSH);
        uint32_t id = is_sync ? next_frame_id++ : 0;
        int rc = frame_write(p->fd, tag, id, sb.buf ? sb.buf : "", sb.len);
        free(sb.buf);

        if (!is_sync) { if (rc < 0) fprintf(stderr, "%s: write failed\n", w); return; }

        pending_add(id);
        struct timespec t0; clock_gettime(CLOCK_MONOTONIC, &t0);
        for (;;) {
            int s = pending_status(id);
            if (s == 1) { push(v_bool(1)); pending_remove(id); return; }
            struct timespec now; clock_gettime(CLOCK_MONOTONIC, &now);
            long elapsed = (now.tv_sec - t0.tv_sec) * 1000
                         + (now.tv_nsec - t0.tv_nsec) / 1000000;
            long remaining = timeout_ms - elapsed;
            if (remaining <= 0) { push(v_num(0)); pending_remove(id); return; }
            mesh_poll(remaining > 50 ? 50 : (int)remaining);
        }
    }

    /* :s :bye */
    if (!strcmp(w,":")) {
        /* should never get here as bare token — keywords are parsed as : then word.
         * Drop silently. */
        return;
    }

    /* literal keywords / commands */
    if (!strcmp(w,"true"))  { push(v_bool(1)); return; }
    if (!strcmp(w,"false")) { push(v_num(0));  return; }
    if (!strcmp(w,"nil"))   { push(v_num(0));  return; }

    /* otherwise: look up and push the binding */
    Value *bound = word_get(w);
    if (bound) { push(v_clone(bound)); return; }

    fprintf(stderr, "unknown word: %s\n", w);
}

static void eval(Value *t) {
    if (!t) return;
    switch (t->type) {
        case V_NUM:
        case V_STR:
        case V_BOOL:
        case V_THUNK:
            push(t);
            return;
        case V_WORD: {
            /* handle :foo keyword form: parser gives us ":" then word.
             * Easier: if word starts with ':', treat the rest as a command. */
            char *w = t->as.word;
            if (w[0] == ':') {
                if (!strcmp(w, ":s"))   { print_stack(); v_unref(t); return; }
                if (!strcmp(w, ":bye")) { v_unref(t); exit(0); }
                /* unknown keyword: push as word for now */
                push(t); return;
            }
            char *copy = strdup(w);
            v_unref(t);
            eval_word(copy);
            free(copy);
            return;
        }
    }
}

/* a parsed top-level word like ":s" arrives as two tokens (":" then "s")
 * because of our single-char op rule. Fuse them at the source feeder. */
void feed(const char *src) {
    Lex L = { src, 0, strlen(src) };
    for (;;) {
        skip_ws(&L);
        if (L.i >= L.n) break;

        /* keyword fusion: ':' followed immediately by an identifier */
        if (L.src[L.i] == ':') {
            size_t j = L.i + 1;
            if (j < L.n && (isalpha((unsigned char)L.src[j]) || L.src[j] == '_')) {
                size_t start = L.i;
                L.i = j;
                while (L.i < L.n &&
                       (isalnum((unsigned char)L.src[L.i]) || L.src[L.i] == '_'))
                    L.i++;
                char *buf = malloc(L.i - start + 1);
                memcpy(buf, L.src + start, L.i - start);
                buf[L.i - start] = 0;
                eval(v_word(buf));
                free(buf);
                continue;
            }
        }

        Value *t = parse_term(&L);
        if (t) eval(t);
    }
}

