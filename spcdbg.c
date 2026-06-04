/*
 * spcdbg — gdb-like stepper for spacelang.
 *
 * Design:
 *   - Links the runtime built with -DSPC_DEBUG, which calls a single
 *     hook (spc_dbg_on_word) at the top of eval_word(). The hook may
 *     enter an interactive prompt loop that reads commands from a
 *     "debugger fd" (stdin by default) until the user resumes.
 *   - Commands: s/step, c/cont, b/break <word>, d <word>, info b,
 *     p/stack, bt, q/quit.
 *   - Without the runtime hook, this file alone only supports the
 *     pre-execution prompt (enough to satisfy basic load+quit).
 */

#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <libgen.h>

#include "spci.h"

static char *slurp(const char *path) {
    FILE *f = fopen(path, "rb");
    if (!f) { perror(path); exit(1); }
    fseek(f, 0, SEEK_END);
    long n = ftell(f);
    fseek(f, 0, SEEK_SET);
    char *buf = malloc(n + 1);
    fread(buf, 1, n, f); buf[n] = 0;
    fclose(f);
    return buf;
}

/* Provided by runtime.c when built with -DSPC_DEBUG. */
void spc_dbg_show_stack(FILE *fp);

/* Local forward declarations. */
static void dbg_prompt(void);
static void dbg_render(const char *event, const char *word);
static int  step_should_stop(void);
#define DBG_MAX_WATCH 64
extern char *watches[];
extern int   watches_len;

/* Debugger state shared with runtime hooks. */
enum { DBG_RUN, DBG_STEP, DBG_NEXT };
int   spc_dbg_mode = DBG_STEP;   /* start stopped */
int   spc_dbg_quit = 0;
int   spc_dbg_next_depth = 0;    /* max frame depth at which DBG_NEXT stops */
int   spc_dbg_show_bindings_panel = 0;  /* toggled by `w` */
int   spc_dbg_step_remaining = 0;       /* extra silent steps before stopping */
#include <stdint.h>
static uint64_t spc_dbg_step_count = 0;

/* Source view: pre-tokenized list of top-level instructions for the loaded
 * file, plus an instruction-pointer maintained by the runtime hook. */
#define DBG_MAX_TERMS 4096
#define DBG_MAX_VIEWS 256

typedef struct {
    char **terms;       /* owned */
    int    nterms;
    int    ip;
    char  *header;      /* owned: "in foo" or path */
} View;

static View views[DBG_MAX_VIEWS];
static int  nviews = 0;

#define CUR (views[nviews - 1])

static void view_push(char **t, int n, const char *header) {
    if (nviews >= DBG_MAX_VIEWS) return;
    views[nviews].terms  = t;
    views[nviews].nterms = n;
    views[nviews].ip     = -1;
    views[nviews].header = strdup(header);
    nviews++;
}
static void view_pop(void) {
    if (nviews == 0) return;
    nviews--;
    for (int i = 0; i < views[nviews].nterms; i++) free(views[nviews].terms[i]);
    free(views[nviews].terms);
    free(views[nviews].header);
}

static int is_ws(char c)   { return c == ' ' || c == '\t' || c == '\n' || c == '\r'; }
static int is_qte(char c)  { return c == '"' || c == '\'' || c == '`'; }

/* Tiny pre-tokenizer that mirrors the runtime parser's notion of a
 * top-level term: skips `{ ... }` comments, walks past balanced brackets,
 * walks past quoted strings, otherwise reads a whitespace-delimited word. */
static void prescan(const char *src, const char *path) {
    char **tmp = malloc(sizeof(char*) * DBG_MAX_TERMS);
    int   ntmp = 0;
    size_t i = 0, n = strlen(src);
    while (i < n && ntmp < DBG_MAX_TERMS) {
        /* skip whitespace and {} comments */
        while (i < n) {
            if (is_ws(src[i])) { i++; continue; }
            if (src[i] == '{') {
                int d = 1; i++;
                while (i < n && d) { if (src[i] == '}') d--; i++; }
                continue;
            }
            break;
        }
        if (i >= n) break;
        size_t start = i;
        char c = src[i];
        if (c == '[') {
            int d = 1; i++;
            while (i < n && d) {
                if (src[i] == '[')      d++;
                else if (src[i] == ']') { d--; i++; continue; }
                else if (is_qte(src[i])) {
                    char q = src[i++];
                    while (i < n && src[i] != q) i++;
                    if (i < n) i++;
                    continue;
                }
                else if (src[i] == '{') {
                    int e = 1; i++;
                    while (i < n && e) { if (src[i] == '}') e--; i++; }
                    continue;
                }
                i++;
            }
        } else if (is_qte(c)) {
            char q = c; i++;
            while (i < n && src[i] != q) i++;
            if (i < n) i++;
        } else {
            while (i < n && !is_ws(src[i]) && src[i] != '[' && src[i] != ']') i++;
        }
        size_t len = i - start;
        char *s = malloc(len + 1);
        memcpy(s, src + start, len);
        s[len] = 0;
        tmp[ntmp++] = s;
    }
    view_push(tmp, ntmp, path);
}

void spc_dbg_on_term(void) {
    /* Outermost feed advances the top-of-stack view's ip. Then, if we are
     * single-stepping, stop here too so the user sees literal/thunk pushes
     * (which never enter eval_word and would otherwise be skipped). */
    if (nviews == 0) return;
    views[0].ip++;
    if (spc_dbg_step_count < UINT64_MAX) spc_dbg_step_count++;
    if (spc_dbg_quit) return;
    if (!step_should_stop()) return;
    int i = views[0].ip;
    const char *w = (i >= 0 && i < views[0].nterms) ? views[0].terms[i] : "?";
    dbg_render("next", w);
    dbg_prompt();
    if (spc_dbg_quit) exit(0);
}

/* Render the source listing + one-row stack + a divider. Stderr so the
 * program's stdout stays clean. */
static void dbg_render(const char *event, const char *word) {
    (void)event; (void)word;
    if (nviews == 0) return;
    fprintf(stderr, "── %s @ %llu ──\n",
            CUR.header ? CUR.header : "",
            (unsigned long long)spc_dbg_step_count);
    for (int i = 0; i < CUR.nterms; i++) {
        const char *marker = "  ";
        if (i == CUR.ip)          marker = "> ";
        else if (i == CUR.ip + 1) marker = ">>";
        fprintf(stderr, "%s %s\n", marker, CUR.terms[i]);
    }
    extern void spc_dbg_show_stack_row(FILE *fp);
    fputs("------------------------------\n", stderr);
    fputs("stack:", stderr);
    spc_dbg_show_stack_row(stderr);
    if (spc_dbg_show_bindings_panel) {
        extern void spc_dbg_show_bindings(FILE *fp);
        fputs("bindings:\n", stderr);
        spc_dbg_show_bindings(stderr);
    }
    if (watches_len > 0) {
        extern void spc_dbg_format_binding(const char *name, char *out, size_t cap);
        fputs("watches:\n", stderr);
        for (int i = 0; i < watches_len; i++) {
            char buf[256];
            spc_dbg_format_binding(watches[i], buf, sizeof buf);
            fprintf(stderr, "  %s = %s\n", watches[i], buf);
        }
    }
    fputs("------------------------------\n", stderr);
}

/* Call-frame stack: names of user-defined words currently executing. */
#define DBG_MAX_FRAMES 256
static const char *frames[DBG_MAX_FRAMES];
static int frames_len = 0;

void spc_dbg_enter(const char *w, const char **items, int n) {
    if (frames_len < DBG_MAX_FRAMES) frames[frames_len++] = w;
    /* Copy items into an owned terms[] array and push a new view. */
    char **t = malloc(sizeof(char*) * (n ? n : 1));
    for (int i = 0; i < n; i++) t[i] = strdup(items[i]);
    char hdr[256];
    snprintf(hdr, sizeof hdr, "in %s", w);
    view_push(t, n, hdr);
}
void spc_dbg_leave(void) {
    if (frames_len > 0) frames_len--;
    view_pop();
}
void spc_dbg_on_body_item(int i) {
    if (nviews == 0) return;
    CUR.ip = i;
    if (spc_dbg_step_count < UINT64_MAX) spc_dbg_step_count++;
    if (!step_should_stop()) return;
    const char *w = (i >= 0 && i < CUR.nterms) ? CUR.terms[i] : "?";
    dbg_render("next", w);
    dbg_prompt();
    if (spc_dbg_quit) exit(0);
}

/* Watches: names whose current bound value is shown in every render.
 * Declared without `static` because dbg_render (defined earlier) needs to
 * reference them via forward `extern` declarations. */
char *watches[DBG_MAX_WATCH];
int   watches_len = 0;

static int watch_find(const char *w) {
    for (int i = 0; i < watches_len; i++)
        if (!strcmp(watches[i], w)) return i;
    return -1;
}
static void watch_add(const char *w) {
    if (watch_find(w) >= 0) return;
    if (watches_len >= DBG_MAX_WATCH) { fprintf(stderr, "watch table full\n"); return; }
    watches[watches_len++] = strdup(w);
}
static void watch_remove(const char *w) {
    int i = watch_find(w);
    if (i < 0) { fprintf(stderr, "no such watch: %s\n", w); return; }
    free(watches[i]);
    watches[i] = watches[--watches_len];
}

/* Breakpoints: linear array of names. v1 size is fine. */
#define DBG_MAX_BP 256
static char *bp_names[DBG_MAX_BP];
static int   bp_len = 0;

static int bp_find(const char *w) {
    for (int i = 0; i < bp_len; i++)
        if (!strcmp(bp_names[i], w)) return i;
    return -1;
}
static void bp_add(const char *w) {
    if (bp_find(w) >= 0) return;
    if (bp_len >= DBG_MAX_BP) { fprintf(stderr, "bp table full\n"); return; }
    bp_names[bp_len++] = strdup(w);
}
static void bp_del(const char *w) {
    int i = bp_find(w);
    if (i < 0) { fprintf(stderr, "no such bp: %s\n", w); return; }
    free(bp_names[i]);
    bp_names[i] = bp_names[--bp_len];
}
static void bp_list(FILE *fp) {
    if (bp_len == 0) { fprintf(fp, "(no breakpoints)\n"); return; }
    for (int i = 0; i < bp_len; i++)
        fprintf(fp, "  %d: %s\n", i + 1, bp_names[i]);
}

/* Debugger reads commands from /dev/tty when available (so the program's
 * stdin stays free for I/O like io/slurp). Falls back to stdin otherwise —
 * which is what the test harness uses. */
static FILE *dbg_in = NULL;
static int   dbg_is_tty = 0;
static void dbg_in_init(void) {
    if (dbg_in) return;
    FILE *tty = fopen("/dev/tty", "r");
    if (tty) { dbg_in = tty; dbg_is_tty = 1; }
    else     { dbg_in = stdin; dbg_is_tty = isatty(STDIN_FILENO); }
}

static char *dbg_readline(void) {
    static char buf[1024];
    dbg_in_init();
    if (dbg_is_tty) { fputs("(spcdbg) ", stderr); fflush(stderr); }
    if (!fgets(buf, sizeof buf, dbg_in)) return NULL;
    size_t n = strlen(buf);
    while (n && (buf[n-1] == '\n' || buf[n-1] == '\r')) buf[--n] = 0;
    return strdup(buf);
}

/* Dispatch one command. Returns 1 to resume execution, 0 to keep prompting. */
static int dbg_dispatch(const char *line) {
    while (*line == ' ' || *line == '\t') line++;
    if (!*line) return 0;
    /* `<N> <cmd>` — leading repeat count for stepping commands. */
    if (line[0] >= '0' && line[0] <= '9') {
        long n = 0;
        const char *p = line;
        while (*p >= '0' && *p <= '9') { n = n * 10 + (*p++ - '0'); }
        while (*p == ' ' || *p == '\t') p++;
        if (n > 0 && (!strcmp(p, "s") || !strcmp(p, "step") ||
                      !strcmp(p, "n") || !strcmp(p, "next"))) {
            spc_dbg_step_remaining = (int)(n - 1);
            line = p;
        }
    }
    if (!strcmp(line, "q") || !strcmp(line, "quit")) {
        spc_dbg_quit = 1;
        return 1;
    }
    if (!strcmp(line, "s") || !strcmp(line, "step")) {
        spc_dbg_mode = DBG_STEP;
        return 1;
    }
    if (!strcmp(line, "n") || !strcmp(line, "next")) {
        spc_dbg_mode = DBG_NEXT;
        spc_dbg_next_depth = frames_len;
        return 1;
    }
    if (!strcmp(line, "c") || !strcmp(line, "cont") || !strcmp(line, "continue")) {
        spc_dbg_mode = DBG_RUN;
        return 1;
    }
    if (!strncmp(line, "watch add ", 10) || !strncmp(line, "watch remove ", 13) ||
        !strncmp(line, "watch rm ", 9) ||
        !strncmp(line, "wa ", 3)        || !strncmp(line, "wd ", 3)) {
        int is_add;
        const char *arg;
        if      (!strncmp(line, "wa ", 3)) { is_add = 1; arg = line + 3; }
        else if (!strncmp(line, "wd ", 3)) { is_add = 0; arg = line + 3; }
        else if (!strncmp(line, "watch add ", 10))   { is_add = 1; arg = line + 10; }
        else if (!strncmp(line, "watch remove ", 13)){ is_add = 0; arg = line + 13; }
        else                                          { is_add = 0; arg = line + 9; }
        /* trim both ends */
        while (*arg == ' ' || *arg == '\t') arg++;
        size_t end = strlen(arg);
        while (end > 0 && (arg[end-1] == ' ' || arg[end-1] == '\t')) end--;
        if (end == 0) { fprintf(stderr, "usage: watch %s <name>\n", is_add ? "add" : "remove"); return 0; }
        char name[256];
        if (end >= sizeof name) end = sizeof name - 1;
        memcpy(name, arg, end); name[end] = 0;
        if (is_add) watch_add(name);
        else        watch_remove(name);
        return 0;
    }
    if (!strcmp(line, "watch") || !strcmp(line, "watch list")) {
        if (watches_len == 0) fprintf(stderr, "(no watches)\n");
        else for (int i = 0; i < watches_len; i++)
            fprintf(stderr, "  %s\n", watches[i]);
        return 0;
    }
    if (!strcmp(line, "w") || !strcmp(line, "bindings")) {
        spc_dbg_show_bindings_panel = !spc_dbg_show_bindings_panel;
        fprintf(stderr, "bindings panel: %s\n",
                spc_dbg_show_bindings_panel ? "on" : "off");
        return 0;
    }
    if (!strcmp(line, "h") || !strcmp(line, "help") || !strcmp(line, "?")) {
        fprintf(stderr,
            "commands:\n"
            "  s, step          run one word, then stop\n"
            "  n, next          step, but skip over inner user-word frames\n"
            "  c, cont          continue until next breakpoint\n"
            "  b, break <word>  set breakpoint by word name\n"
            "  d, delete <word> remove breakpoint\n"
            "  info b           list breakpoints\n"
            "  p, stack         print data stack\n"
            "  bt, backtrace    print call frames\n"
            "  w, bindings      toggle bindings panel in the view\n"
            "  wa <name> / watch add <name>     — track a binding\n"
            "  wd <name> / watch remove <name>  — stop tracking a binding\n"
            "  h, help          show this help\n"
            "  q, quit          exit spcdbg\n");
        return 0;
    }
    if (!strcmp(line, "p") || !strcmp(line, "stack")) {
        spc_dbg_show_stack(stderr);
        return 0;
    }
    if (!strncmp(line, "b ", 2) || !strncmp(line, "break ", 6)) {
        const char *arg = line + (line[1] == ' ' ? 2 : 6);
        while (*arg == ' ') arg++;
        if (*arg) bp_add(arg);
        else fprintf(stderr, "usage: b <word>\n");
        return 0;
    }
    if (!strncmp(line, "d ", 2) || !strncmp(line, "delete ", 7)) {
        const char *arg = line + (line[1] == ' ' ? 2 : 7);
        while (*arg == ' ') arg++;
        if (*arg) bp_del(arg);
        else fprintf(stderr, "usage: d <word>\n");
        return 0;
    }
    if (!strcmp(line, "info b") || !strcmp(line, "info break")) {
        bp_list(stderr);
        return 0;
    }
    if (!strcmp(line, "bt") || !strcmp(line, "backtrace")) {
        if (frames_len == 0) fprintf(stderr, "  (top level)\n");
        for (int i = frames_len - 1; i >= 0; i--)
            fprintf(stderr, "  #%d %s\n", frames_len - 1 - i, frames[i]);
        return 0;
    }
    fprintf(stderr, "unknown command: %s\n", line);
    return 0;
}

/* Called from runtime eval_word() under -DSPC_DEBUG, once per word token. */
/* Step stops happen at the term/body-item granularity now. on_word only
 * fires for breakpoint matches (any user/builtin word with a bp hits). */
void spc_dbg_on_word(const char *w) {
    if (spc_dbg_quit) exit(0);
    if (bp_find(w) < 0) return;
    dbg_render("bp", w);
    dbg_prompt();
    if (spc_dbg_quit) exit(0);
}

static int step_should_stop(void) {
    int qualifies = (spc_dbg_mode == DBG_STEP)
                 || (spc_dbg_mode == DBG_NEXT && frames_len <= spc_dbg_next_depth);
    if (!qualifies) return 0;
    if (spc_dbg_step_remaining > 0) { spc_dbg_step_remaining--; return 0; }
    return 1;
}

/* Interactive prompt loop. Returns when user resumes execution or quits.
 * Empty input repeats the previous command (gdb behavior). */
static void dbg_prompt(void) {
    static char last[1024] = "";
    for (;;) {
        char *line = dbg_readline();
        if (!line) { spc_dbg_quit = 1; return; }
        const char *cmd = line;
        while (*cmd == ' ' || *cmd == '\t') cmd++;
        if (!*cmd && *last) cmd = last;
        else if (*cmd) { strncpy(last, cmd, sizeof last - 1); last[sizeof last - 1] = 0; }
        int resume = dbg_dispatch(cmd);
        free(line);
        if (resume) return;
    }
}

int main(int argc, char **argv) {
    setvbuf(stdout, NULL, _IOLBF, 0);
    setvbuf(stderr, NULL, _IOLBF, 0);
    const char *files[32]; int nfiles = 0;
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--version") || !strcmp(argv[i], "-V")) {
            printf("spcdbg %s\n", SPC_VERSION); return 0;
        } else if (argv[i][0] != '-') {
            if (nfiles < 32) files[nfiles++] = argv[i];
        } else {
            fprintf(stderr, "unknown arg: %s\n", argv[i]); return 1;
        }
    }

    /* Initial prompt: debugger is stopped before any execution. */
    dbg_prompt();
    if (spc_dbg_quit) return 0;

    /* Resume: feed the file(s). Hooks (when wired) will re-enter dbg_prompt. */
    for (int i = 0; i < nfiles; i++) {
        char *src = slurp(files[i]);
        char *path_dup = strdup(files[i]);
        const char *prev = spc_source_dir;
        spc_source_dir = dirname(path_dup);
        prescan(src, files[i]);
        feed(src);
        spc_source_dir = prev;
        free(path_dup); free(src);
        if (spc_dbg_quit) break;
    }
    return 0;
}
