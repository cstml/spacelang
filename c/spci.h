/*
 * spci.h — public surface of the spacelang C runtime.
 *
 * Consumers:
 *   - spci_main.c          (the interactive interpreter binary)
 *   - generated programs   (emitted by spcc, see c/spcc.c)
 *
 * Everything declared here is defined in spci.c. Internal helpers
 * (Value type, stack, dict, parser, builtins, frame I/O, peer table) stay
 * `static` inside spci.c — only what the driver/main needs to call is
 * exposed below.
 */

#ifndef SPCI_H
#define SPCI_H

#include <stddef.h>
#include <stdint.h>

/* --- mesh state ---
 * Driver sets these before calling mesh_listen(). NULL my_name means
 * single-process mode (no mesh). */
extern char *my_name;
extern char *bus_dir;
extern int   listen_fd;

/* --- peer table (exposed so the driver can run the event loop) --- */
typedef struct {
    char *name;
    int   fd;
    int   outgoing;
} Peer;

extern Peer  *peers;
extern size_t peers_len;

Peer *peer_add(int fd, const char *name, int outgoing);
void  peer_drop(size_t i);

/* --- framing --- */
int  frame_read(int fd, uint8_t *tag, uint32_t *id,
                char **payload, uint32_t *len);
void on_frame(Peer *p, uint8_t tag, uint32_t id,
              char *payload, uint32_t len);

/* --- mesh lifecycle --- */
int  mesh_listen(void);          /* bind $BUS/$NAME.sock; needs my_name+bus_dir */
int  mesh_poll(int timeout_ms);  /* pump accept + peer fds */

/* --- evaluator entry point --- */
void feed(const char *src);

#endif /* SPCI_H */
