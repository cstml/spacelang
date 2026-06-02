/*
 * spci_main.c — driver for the interactive spci interpreter.
 *
 * All the heavy lifting (parser, evaluator, mesh transport) lives in spci.c
 * and is reached through spci.h. This file is just argv parsing + the
 * event loop that multiplexes stdin and mesh sockets.
 */

#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <poll.h>
#include <sys/socket.h>
#include <stdint.h>

#include "spci.h"

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
    const char *file_arg = NULL;
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--name") && i + 1 < argc) { my_name = argv[++i]; }
        else if (!strcmp(argv[i], "--bus") && i + 1 < argc) { bus_dir = argv[++i]; }
        else if (argv[i][0] != '-') { file_arg = argv[i]; }
        else { fprintf(stderr, "unknown arg: %s\n", argv[i]); return 1; }
    }
    if ((my_name && !bus_dir) || (!my_name && bus_dir)) {
        fprintf(stderr, "--name and --bus must be used together\n"); return 1;
    }

    if (my_name) {
        if (mesh_listen() < 0) return 1;
    }

    if (file_arg) {
        char *src = slurp_file(file_arg);
        feed(src);
        free(src);
        if (!my_name) return 0;
    }

    if (my_name) {
        int is_tty = isatty(STDIN_FILENO);
        fprintf(stderr, "[%s] ready%s\n", my_name, is_tty ? " — REPL on stdin" : "");
        if (is_tty) { printf("> "); fflush(stdout); }
        for (;;) {
            struct pollfd pfds[64];
            size_t n = 0;
            pfds[n].fd = STDIN_FILENO; pfds[n].events = POLLIN; n++;
            pfds[n].fd = listen_fd;    pfds[n].events = POLLIN; n++;
            for (size_t i = 0; i < peers_len && n < 64; i++) {
                pfds[n].fd = peers[i].fd; pfds[n].events = POLLIN; n++;
            }
            if (poll(pfds, n, 1000) <= 0) continue;

            if (pfds[0].revents & POLLIN) {
                char line[4096];
                if (!fgets(line, sizeof line, stdin)) return 0;
                feed(line);
                if (is_tty) { printf("> "); fflush(stdout); }
            }
            if (pfds[1].revents & POLLIN) {
                int cfd = accept(listen_fd, NULL, NULL);
                if (cfd >= 0) peer_add(cfd, NULL, 0);
            }
            for (size_t pi = 2; pi < n; pi++) {
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
    char line[4096];
    printf("spci · spacelang in C · :bye to quit\n");
    for (;;) {
        printf("> "); fflush(stdout);
        if (!fgets(line, sizeof line, stdin)) { printf("\n"); break; }
        feed(line);
    }
    return 0;
}
