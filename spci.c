/*
 * spci.c — driver for the interactive spci interpreter.
 *
 * All the heavy lifting (parser, evaluator, mesh transport) lives in
 * runtime.c and is reached through spci.h. This file is just argv
 * parsing + the event loop that multiplexes stdin and mesh sockets.
 */

#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <poll.h>
#include <sys/socket.h>
#include <stdint.h>
#include <libgen.h>

#include "spci.h"

/* Returns updated comment depth after scanning `s`. `{ ... }` comments do not
 * nest in the parser, but we track depth as a plain counter; the parser will
 * treat the first `}` as the closer regardless. Strings (", ', `) shield their
 * contents so `{` inside a literal does not start a comment. */
static int scan_comment_depth(const char *s, int depth) {
    char in_str = 0;
    for (; *s; s++) {
        char c = *s;
        if (depth > 0) { if (c == '}') depth--; continue; }
        if (in_str)   { if (c == in_str) in_str = 0; continue; }
        if (c == '"' || c == '\'' || c == '`') in_str = c;
        else if (c == '{') depth++;
    }
    return depth;
}

/* Read one logical chunk from stdin: keeps reading lines while inside an open
 * `{ ... }` comment so multiline comments work in the REPL. Returns malloc'd
 * buffer, or NULL on EOF with nothing buffered. */
static char *read_chunk(int is_tty) {
    char line[4096];
    char *buf = NULL; size_t len = 0;
    int depth = 0;
    for (;;) {
        if (!fgets(line, sizeof line, stdin)) {
            if (buf) return buf;
            return NULL;
        }
        size_t llen = strlen(line);
        char *nb = realloc(buf, len + llen + 1);
        if (!nb) { free(buf); return NULL; }
        buf = nb; memcpy(buf + len, line, llen + 1); len += llen;
        depth = scan_comment_depth(line, depth);
        if (depth <= 0) return buf;
        if (is_tty) { printf("..  "); fflush(stdout); }
    }
}

static char *slurp_file(const char *path) {
    FILE *f = fopen(path, "rb");
    if (!f) { perror(path); exit(1); }
    fseek(f, 0, SEEK_END);
    long n = ftell(f);
    fseek(f, 0, SEEK_SET);
    char *buf = malloc(n + 1);
    fread(buf, 1, n, f);
    buf[n] = 0;
    fclose(f);
    return buf;
}

int main(int argc, char **argv) {
    setvbuf(stdout, NULL, _IOLBF, 0);
    setvbuf(stderr, NULL, _IOLBF, 0);
    const char *files[32]; int nfiles = 0;
    int serve = 0;
    /* Args after a literal `--` become the user-visible argv (read from
     * spaceforth via :argc / :argv). */
    int dd = argc;
    for (int i = 1; i < argc; i++) if (!strcmp(argv[i], "--")) { dd = i; break; }
    for (int i = 1; i < dd; i++) {
        if (!strcmp(argv[i], "--version") || !strcmp(argv[i], "-V")) {
            printf("spci %s\n", SPC_VERSION); return 0;
        }
        else if (!strcmp(argv[i], "--name") && i + 1 < dd) { my_name = argv[++i]; }
        else if (!strcmp(argv[i], "--bus") && i + 1 < dd) { bus_dir = argv[++i]; }
        else if (!strcmp(argv[i], "--serve")) { serve = 1; }
        else if (argv[i][0] != '-') {
            if (nfiles < 32) files[nfiles++] = argv[i];
        }
        else { fprintf(stderr, "unknown arg: %s\n", argv[i]); return 1; }
    }
    if (dd < argc) {
        user_argc = argc - (dd + 1);
        user_argv = (char *const *)&argv[dd + 1];
    }
    if ((my_name && !bus_dir) || (!my_name && bus_dir)) {
        fprintf(stderr, "--name and --bus must be used together\n"); return 1;
    }

    if (my_name) {
        if (mesh_listen() < 0) return 1;
    }

    if (nfiles > 0) {
        for (int i = 0; i < nfiles; i++) {
            char *src = slurp_file(files[i]);
            char *path_dup = strdup(files[i]);
            const char *prev = spc_source_dir;
            spc_source_dir = dirname(path_dup);
            feed(src);
            spc_source_dir = prev;
            free(path_dup);
            free(src);
        }
        if (!my_name) return 0;
        if (!serve) {
            fprintf(stderr, "[%s] done (use --serve to keep alive)\n", my_name);
            return 0;
        }
    }

    if (my_name) {
        int is_tty = isatty(STDIN_FILENO);
        int stdin_open = 1;  /* cleared on EOF; mesh keeps running */
        fprintf(stderr, "[%s] ready%s\n", my_name, is_tty ? " — REPL on stdin" : "");
        if (is_tty) { printf("> "); fflush(stdout); }
        for (;;) {
            struct pollfd pfds[64];
            size_t n = 0;
            size_t stdin_slot = (size_t)-1;
            if (stdin_open) {
                stdin_slot = n;
                pfds[n].fd = STDIN_FILENO; pfds[n].events = POLLIN; n++;
            }
            size_t listen_slot = n;
            pfds[n].fd = listen_fd; pfds[n].events = POLLIN; n++;
            for (size_t i = 0; i < peers_len && n < 64; i++) {
                pfds[n].fd = peers[i].fd; pfds[n].events = POLLIN; n++;
            }
            if (poll(pfds, n, 1000) <= 0) continue;

            if (stdin_slot != (size_t)-1 && (pfds[stdin_slot].revents & POLLIN)) {
                char *chunk = read_chunk(is_tty);
                if (!chunk) {
                    if (is_tty) return 0;        /* Ctrl-D → exit */
                    stdin_open = 0;               /* pipe closed, keep serving */
                } else {
                    feed(chunk);
                    free(chunk);
                    if (is_tty) { printf("> "); fflush(stdout); }
                }
            }
            if (pfds[listen_slot].revents & POLLIN) {
                int cfd = accept(listen_fd, NULL, NULL);
                if (cfd >= 0) peer_add(cfd, NULL, 0);
            }
            for (size_t pi = listen_slot + 1; pi < n; pi++) {
                if (!(pfds[pi].revents & (POLLIN|POLLHUP|POLLERR))) continue;
                size_t found = (size_t)-1;
                for (size_t j = 0; j < peers_len; j++)
                    if (peers[j].fd == pfds[pi].fd) { found = j; break; }
                if (found == (size_t)-1) continue;
                uint8_t tag; uint32_t id, len; char *payload = NULL;
                if (frame_read(peers[found].fd, &tag, &id, &payload, &len) < 0) {
                    peer_drop(found); continue;
                }
                on_frame(&peers[found], tag, id, payload, len);
            }
        }
    }

    /* REPL (no mesh) */
    int is_tty = isatty(STDIN_FILENO);
    if (is_tty) printf("spci · spacelang in C · bye! to quit\n");
    for (;;) {
        if (is_tty) { printf("> "); fflush(stdout); }
        char *chunk = read_chunk(is_tty);
        if (!chunk) { printf("\n"); break; }
        feed(chunk);
        free(chunk);
    }
    return 0;
}
