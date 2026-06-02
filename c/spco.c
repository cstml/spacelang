/*
 * spco — spacelang orchestrator.
 *
 * High-level design (name-resolution broker, NSQ-lookupd-style + lifecycle)
 * ------------------------------------------------------------------------
 * spco listens on $BUS/spco.sock. The spacelang runtime falls back to it
 * when a direct peer connect fails. Protocol:
 *
 *   sender → spco : LOOKUP "name"
 *   spco   → sender: ADDR  "/path/to/name.sock"     (success)
 *   spco   → sender: <close without reply>          (unknown name / spawn failure)
 *
 * spco is OUT of the data path: it only mediates discovery and lifecycle.
 * After getting an ADDR, the sender connects direct to that socket and
 * peer-to-peer traffic flows over the existing protocol unchanged.
 *
 * Manifest is positional argv: each NAME=CMD declares that `NAME` should
 * be brought up by exec'ing `CMD` (with `--name NAME --bus $BUS` appended
 * and SPACELANG_NAME/SPACELANG_BUS env vars set).
 *
 *   spco [--bus DIR] NAME=CMD ...
 *
 * Examples
 *   spco A='./c/spci --serve a.sp'   B='./c/spci --serve b.sp'
 *   spco A='./a_compiled --serve'    B='./b_compiled --serve'
 *
 * Child supervision:
 *   - Lazy spawn — children start when the first LOOKUP arrives, not at boot.
 *   - On crash, restart on next LOOKUP after exponential backoff
 *     (100ms, 200ms, 400ms, … capped at 5s).
 *   - On SIGINT/SIGTERM: SIGTERM all children, unlink sockets, exit.
 */

#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <signal.h>
#include <poll.h>
#include <time.h>
#include <errno.h>
#include <stdint.h>

#include "spci.h"   /* TAG_LOOKUP / TAG_ADDR + frame_read/frame_write */

typedef struct {
    char  *name;
    char **argv;          /* NULL-terminated; argv[0] = command */
    pid_t  pid;           /* 0 if not running */
    int    crashes;
    long   next_spawn_ms; /* monotonic ms; 0 = ok to spawn now */
} Entry;

static Entry  *entries = NULL;
static size_t  entries_len = 0, entries_cap = 0;
static char   *o_bus = "/tmp/spacelang";
static char    o_listen_path[108];  /* sizeof(sockaddr_un.sun_path) */
static int     o_listen = -1;
static int     verbose = 0;

static long now_ms(void) {
    struct timespec ts; clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}

static Entry *find_entry(const char *name) {
    for (size_t i = 0; i < entries_len; i++)
        if (!strcmp(entries[i].name, name)) return &entries[i];
    return NULL;
}

/* split `cmd` on whitespace into argv tokens. Returns malloc'd argv array,
 * each entry malloc'd. Appends --name NAME --bus BUS and NULL terminator. */
static char **build_argv(const char *name, const char *cmd) {
    size_t cap = 8, n = 0;
    char **argv = malloc(cap * sizeof(char*));
    const char *p = cmd;
    while (*p) {
        while (*p && isspace((unsigned char)*p)) p++;
        if (!*p) break;
        const char *start = p;
        while (*p && !isspace((unsigned char)*p)) p++;
        size_t len = p - start;
        if (n + 6 > cap) { cap *= 2; argv = realloc(argv, cap * sizeof(char*)); }
        argv[n] = malloc(len + 1);
        memcpy(argv[n], start, len);
        argv[n][len] = 0;
        n++;
    }
    if (n + 5 > cap) { cap = n + 5; argv = realloc(argv, cap * sizeof(char*)); }
    argv[n++] = strdup("--name");
    argv[n++] = strdup(name);
    argv[n++] = strdup("--bus");
    argv[n++] = strdup(o_bus);
    argv[n] = NULL;
    return argv;
}

static void add_entry(const char *name, const char *cmd) {
    if (entries_len == entries_cap) {
        entries_cap = entries_cap ? entries_cap * 2 : 8;
        entries = realloc(entries, entries_cap * sizeof(Entry));
    }
    Entry *e = &entries[entries_len++];
    e->name = strdup(name);
    e->argv = build_argv(name, cmd);
    e->pid = 0;
    e->crashes = 0;
    e->next_spawn_ms = 0;
}

static void sock_path_for(const char *name, char *out, size_t outsz) {
    snprintf(out, outsz, "%s/%s.sock", o_bus, name);
}
static int sock_exists(const char *path) {
    struct stat st;
    return stat(path, &st) == 0 && S_ISSOCK(st.st_mode);
}

static int spawn(Entry *e) {
    /* clean stale socket so child can bind fresh */
    char path[108]; sock_path_for(e->name, path, sizeof path);
    unlink(path);

    pid_t pid = fork();
    if (pid < 0) { perror("fork"); return -1; }
    if (pid == 0) {
        setenv("SPACELANG_NAME", e->name, 1);
        setenv("SPACELANG_BUS", o_bus, 1);
        execvp(e->argv[0], e->argv);
        perror(e->argv[0]);
        _exit(127);
    }
    e->pid = pid;
    if (verbose) {
        fprintf(stderr, "[spco] spawn %s pid=%d cmd=%s\n",
                e->name, pid, e->argv[0]);
    }
    return 0;
}

/* Spawn (if needed) and wait briefly for child to bind. */
static int ensure_ready(Entry *e) {
    char path[108]; sock_path_for(e->name, path, sizeof path);

    if (e->pid == 0) {
        long now = now_ms();
        if (e->next_spawn_ms && now < e->next_spawn_ms) {
            fprintf(stderr, "[spco] %s in backoff (%ldms left)\n",
                    e->name, e->next_spawn_ms - now);
            return 0;
        }
        if (verbose) fprintf(stderr, "[spco] lazy-spawning %s\n", e->name);
        if (spawn(e) < 0) return 0;
    } else {
        if (verbose) fprintf(stderr, "[spco] %s already running (pid=%d)\n", e->name, e->pid);
    }
    /* wait up to ~1s for the bind */
    for (int i = 0; i < 40; i++) {
        if (sock_exists(path)) {
            if (verbose) fprintf(stderr, "[spco] %s bound at %s\n", e->name, path);
            return 1;
        }
        struct timespec ts = { 0, 25 * 1000 * 1000 };
        nanosleep(&ts, NULL);
    }
    fprintf(stderr, "[spco] %s did not bind within 1s\n", e->name);
    return 0;
}

static void handle_client(int cfd) {
    if (verbose) fprintf(stderr, "[spco] client connected\n");
    uint8_t tag; uint32_t id, len; char *payload = NULL;
    if (frame_read(cfd, &tag, &id, &payload, &len) < 0) {
        if (verbose) fprintf(stderr, "[spco] client read failed, closing\n");
        close(cfd); return;
    }
    if (tag != TAG_LOOKUP || !payload) {
        fprintf(stderr, "[spco] bad frame tag=0x%02x, closing\n", tag);
        free(payload); close(cfd); return;
    }

    if (verbose) fprintf(stderr, "[spco] LOOKUP \"%s\"\n", payload);
    Entry *e = find_entry(payload);
    if (!e) {
        fprintf(stderr, "[spco] unknown name \"%s\"\n", payload);
        free(payload); close(cfd); return;
    }
    free(payload);

    if (!ensure_ready(e)) {
        fprintf(stderr, "[spco] cannot bring up %s\n", e->name);
        close(cfd); return;
    }

    char path[108]; sock_path_for(e->name, path, sizeof path);
    if (verbose) fprintf(stderr, "[spco] → ADDR %s\n", path);
    frame_write(cfd, TAG_ADDR, 0, path, strlen(path));
    close(cfd);
}

static void reap_children(void) {
    int status; pid_t pid;
    while ((pid = waitpid(-1, &status, WNOHANG)) > 0) {
        for (size_t i = 0; i < entries_len; i++) {
            if (entries[i].pid == pid) {
                entries[i].pid = 0;
                entries[i].crashes++;
                long backoff = 100;
                for (int k = 1; k < entries[i].crashes && backoff < 5000; k++)
                    backoff *= 2;
                entries[i].next_spawn_ms = now_ms() + backoff;
                if (verbose) {
                    int ex = WIFEXITED(status) ? WEXITSTATUS(status) : -1;
                    fprintf(stderr,
                        "[spco] %s exited (code=%d crashes=%d backoff=%ldms)\n",
                        entries[i].name, ex, entries[i].crashes, backoff);
                }
                break;
            }
        }
    }
}

static volatile sig_atomic_t stop = 0;
static void on_signal(int sig) { (void)sig; stop = 1; }

static void cleanup_and_exit(int code) {
    fprintf(stderr, "[spco] shutting down\n");
    for (size_t i = 0; i < entries_len; i++) {
        if (entries[i].pid > 0) kill(entries[i].pid, SIGTERM);
    }
    /* short wait for clean exit */
    struct timespec ts = { 0, 100 * 1000 * 1000 };
    nanosleep(&ts, NULL);
    for (size_t i = 0; i < entries_len; i++) {
        char p[108]; sock_path_for(entries[i].name, p, sizeof p);
        unlink(p);
    }
    if (o_listen >= 0) close(o_listen);
    unlink(o_listen_path);
    exit(code);
}

static void usage(void) {
    fputs(
        "usage: spco [-v|--verbose] [--bus DIR] NAME=CMD ...\n"
        "  -v, --verbose   show per-request trace (spawn, LOOKUP, ADDR)\n"
        "  --bus DIR       bus directory (default: /tmp/spacelang)\n"
        "  NAME=CMD        peer name and command to spawn on LOOKUP\n",
        stderr);
}

int main(int argc, char **argv) {
    setvbuf(stderr, NULL, _IOLBF, 0);

    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "-v") || !strcmp(argv[i], "--verbose")) {
            verbose = 1;
        } else if (!strcmp(argv[i], "--bus") && i+1 < argc) o_bus = argv[++i];
        else if (!strcmp(argv[i], "-h") || !strcmp(argv[i], "--help")) {
            usage(); return 0;
        }
        else if (strchr(argv[i], '=')) {
            char *eq = strchr(argv[i], '=');
            *eq = 0;
            add_entry(argv[i], eq + 1);
            *eq = '=';  /* restore for ps clarity */
        } else {
            fprintf(stderr, "spco: bad arg: %s\n", argv[i]);
            usage(); return 1;
        }
    }
    if (entries_len == 0) {
        fprintf(stderr, "spco: at least one NAME=CMD required\n");
        usage(); return 1;
    }

    mkdir(o_bus, 0700);
    snprintf(o_listen_path, sizeof o_listen_path, "%s/spco.sock", o_bus);
    unlink(o_listen_path);

    o_listen = socket(AF_UNIX, SOCK_STREAM, 0);
    if (o_listen < 0) { perror("socket"); return 1; }
    struct sockaddr_un addr = {0};
    addr.sun_family = AF_UNIX;
    snprintf(addr.sun_path, sizeof addr.sun_path, "%s", o_listen_path);
    if (bind(o_listen, (struct sockaddr*)&addr, sizeof addr) < 0) {
        perror("bind"); return 1;
    }
    if (listen(o_listen, 16) < 0) { perror("listen"); return 1; }

    signal(SIGINT,  on_signal);
    signal(SIGTERM, on_signal);
    signal(SIGPIPE, SIG_IGN);

    fprintf(stderr, "[spco] listening on %s, %zu entries\n",
            o_listen_path, entries_len);
    if (verbose)
        for (size_t i = 0; i < entries_len; i++)
            fprintf(stderr, "[spco]   %s → %s\n", entries[i].name, entries[i].argv[0]);

    while (!stop) {
        struct pollfd pfd = { o_listen, POLLIN, 0 };
        int r = poll(&pfd, 1, 500);
        reap_children();
        if (r > 0 && (pfd.revents & POLLIN)) {
            int cfd = accept(o_listen, NULL, NULL);
            if (cfd >= 0) handle_client(cfd);
        }
    }
    cleanup_and_exit(0);
    return 0;
}
