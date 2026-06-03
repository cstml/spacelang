/*
 * spcc — spacelang compiler.
 *
 * High-level design (compiler+lib distribution, gcc-style)
 * --------------------------------------------------------
 * spcc compiles a .sp source into a standalone native executable by
 * emitting a small C file and shelling out to a C compiler that links it
 * against a pre-built runtime archive (libspci.a) and the runtime header
 * (spci.h).
 *
 * Distribution model: spcc + spci.h + libspci.a ship together in a known
 * directory (`SPACELANG_ROOT`), conceptually like gcc + libgcc.a + headers
 * under $GOROOT-style discoverable layout. The compiled output binary is
 * fully self-contained — the archive is statically linked in.
 *
 * Runtime lookup, in order:
 *   1. $SPACELANG_ROOT (if set)
 *   2. dirname(realpath(argv[0]))   — handy in dev when spcc sits next
 *                                     to spci.h / libspci.a in c/
 * The first location containing both spci.h and libspci.a wins.
 *
 * Generated file is small (~1 KB): just `#include "spci.h"`, the user's
 * .sp source as a byte array, and a generated main() that calls
 * feed(SP_SOURCE). cc compiles + links it against libspci.a.
 *
 * Mesh contract: the generated binary speaks the existing spci mesh
 * protocol. Identity from SPACELANG_NAME / SPACELANG_BUS or --name/--bus
 * on the generated binary. --serve keeps it alive after feed() returns.
 *
 * CLI
 *   spcc input.sp -o output           compile to ./output
 *   spcc --emit-c input.sp            print generated C to stdout
 *   spcc --keep-c input.sp -o output  also write output.c next to binary
 *   spcc --cc gcc input.sp -o output  override $CC (default: cc)
 *   spcc --debug input.sp -o output   compile with -g -O0, implies --keep-c
 *   spcc --root DIR input.sp ...      override runtime-source lookup
 */

#define _POSIX_C_SOURCE 200809L
#define _DEFAULT_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <libgen.h>
#include <errno.h>
#include <limits.h>
#include <ctype.h>

static void die(const char *msg) {
    fprintf(stderr, "spcc: %s\n", msg);
    exit(1);
}

static char *slurp(const char *path, size_t *out_len) {
    FILE *f = fopen(path, "rb");
    if (!f) { perror(path); exit(1); }
    fseek(f, 0, SEEK_END);
    long n = ftell(f);
    fseek(f, 0, SEEK_SET);
    char *buf = malloc(n + 1);
    if (!buf) die("oom");
    if (fread(buf, 1, n, f) != (size_t)n) die("short read");
    buf[n] = 0;
    fclose(f);
    if (out_len) *out_len = (size_t)n;
    return buf;
}

/* ---------- :require preprocessor ----------
 *
 * Resolves `"path" :require` statically: opens the referenced file,
 * preprocesses it recursively, and inlines its contents. So compiled
 * binaries are self-contained — no filesystem dependency at runtime.
 *
 * Errors out (with file:line) if :require's operand isn't a string
 * literal, or if the referenced file can't be opened.
 *
 * Cycle-safe: each realpath is loaded at most once.
 */

typedef struct { char *data; size_t len, cap; } Buf;

static void buf_grow(Buf *b, size_t need) {
    if (b->len + need <= b->cap) return;
    size_t c = b->cap ? b->cap : 256;
    while (c < b->len + need) c *= 2;
    b->data = realloc(b->data, c);
    if (!b->data) die("oom");
    b->cap = c;
}
static void buf_put(Buf *b, const char *s, size_t n) {
    buf_grow(b, n + 1);
    memcpy(b->data + b->len, s, n);
    b->len += n;
    b->data[b->len] = 0;
}

typedef struct { char **v; size_t n, cap; } VSet;
static int vset_has(VSet *s, const char *p) {
    for (size_t i = 0; i < s->n; i++) if (!strcmp(s->v[i], p)) return 1;
    return 0;
}
static void vset_add(VSet *s, const char *p) {
    if (s->n == s->cap) { s->cap = s->cap ? s->cap*2 : 8; s->v = realloc(s->v, s->cap * sizeof *s->v); }
    s->v[s->n++] = strdup(p);
}

static char *join_path(const char *dir, const char *rel) {
    if (rel[0] == '/') return strdup(rel);
    size_t dn = strlen(dir), rn = strlen(rel);
    char *r = malloc(dn + 1 + rn + 1);
    memcpy(r, dir, dn);
    r[dn] = '/';
    memcpy(r + dn + 1, rel, rn + 1);
    return r;
}

static void preprocess(const char *path, VSet *visited, Buf *out);

/* Process src buffer from `path` into `out`. dir = directory of path (for relative requires). */
static void preprocess_buf(const char *path, const char *dir,
                           const char *src, size_t n,
                           VSet *visited, Buf *out) {
    size_t i = 0, line = 1;

    /* Tokens that can precede :require. We only care about the last one. */
    int   last_was_string = 0;
    char *last_string     = NULL;
    size_t last_string_line = 0;
    size_t last_string_out_start = 0;  /* where we wrote it in `out` */

    while (i < n) {
        unsigned char c = (unsigned char)src[i];

        /* whitespace */
        if (isspace(c)) {
            if (c == '\n') line++;
            buf_put(out, (char*)&c, 1);
            i++;
            continue;
        }
        /* comment { ... } (non-nesting, matches spci tokenizer) */
        if (c == '{') {
            size_t start = i;
            while (i < n && src[i] != '}') { if (src[i] == '\n') line++; i++; }
            if (i < n) i++;
            buf_put(out, src + start, i - start);
            continue;
        }
        /* strings */
        if (c == '"' || c == '\'' || c == '`') {
            char q = c;
            size_t start = i++;
            size_t str_line = line;
            while (i < n && src[i] != q) { if (src[i] == '\n') line++; i++; }
            if (i >= n) {
                fprintf(stderr, "spcc: %s:%zu: unterminated string\n", path, str_line);
                exit(1);
            }
            size_t end = i;
            i++; /* closer */
            free(last_string);
            last_string = malloc(end - (start + 1) + 1);
            memcpy(last_string, src + start + 1, end - (start + 1));
            last_string[end - (start + 1)] = 0;
            last_was_string = 1;
            last_string_line = str_line;
            last_string_out_start = out->len;
            buf_put(out, src + start, i - start);
            continue;
        }
        /* single-char structural */
        if (c == '[' || c == ']') {
            buf_put(out, (char*)&c, 1);
            i++;
            last_was_string = 0;
            continue;
        }
        /* word: read until whitespace or structural */
        size_t start = i;
        while (i < n) {
            unsigned char wc = (unsigned char)src[i];
            if (isspace(wc) || wc == '[' || wc == ']' || wc == '{' || wc == '"' || wc == '\'' || wc == '`') break;
            i++;
        }
        size_t wlen = i - start;

        if (wlen == 8 && !memcmp(src + start, ":require", 8)) {
            if (!last_was_string) {
                fprintf(stderr,
                    "spcc: %s:%zu: :require expects a string literal as its operand\n",
                    path, line);
                exit(1);
            }
            char *resolved = join_path(dir, last_string);
            char real[PATH_MAX];
            if (!realpath(resolved, real)) {
                fprintf(stderr, "spcc: %s:%zu: :require cannot open '%s': %s\n",
                        path, last_string_line, last_string, strerror(errno));
                exit(1);
            }
            free(resolved);
            /* Drop the string literal we emitted; replace with file contents. */
            out->len = last_string_out_start;
            out->data[out->len] = 0;
            buf_put(out, "\n", 1);
            if (!vset_has(visited, real)) {
                vset_add(visited, real);
                preprocess(real, visited, out);
            }
            buf_put(out, "\n", 1);
            last_was_string = 0;
            free(last_string); last_string = NULL;
            continue;
        }

        buf_put(out, src + start, wlen);
        last_was_string = 0;
    }
    free(last_string);
    (void)last_string_line;
}

static void preprocess(const char *path, VSet *visited, Buf *out) {
    size_t n = 0;
    char *src = slurp(path, &n);
    char *dup = strdup(path);
    char *dir = strdup(dirname(dup));
    free(dup);
    preprocess_buf(path, dir, src, n, visited, out);
    free(dir);
    free(src);
}

static int file_exists(const char *path) {
    struct stat st;
    return stat(path, &st) == 0 && S_ISREG(st.st_mode);
}

static int root_has_runtime(const char *root) {
    char p[PATH_MAX];
    snprintf(p, sizeof p, "%s/spci.h",    root); if (!file_exists(p)) return 0;
    snprintf(p, sizeof p, "%s/libspci.a", root); if (!file_exists(p)) return 0;
    return 1;
}

/* find SPACELANG_ROOT — env override, then next to the spcc binary. */
static char *find_root(void) {
    const char *env = getenv("SPACELANG_ROOT");
    if (env && *env && root_has_runtime(env)) return strdup(env);

    char exe[PATH_MAX];
    ssize_t n = readlink("/proc/self/exe", exe, sizeof exe - 1);
    if (n > 0) {
        exe[n] = 0;
        char *dup = strdup(exe);
        char *d = dirname(dup);
        if (root_has_runtime(d)) { char *r = strdup(d); free(dup); return r; }
        free(dup);
    }
    return NULL;
}

static void emit(FILE *out, const char *sp_src, size_t sp_len, const char *bake_name) {
    fputs("/* generated by spcc - do not edit by hand */\n", out);
    fputs("#define _POSIX_C_SOURCE 200809L\n", out);
    fputs("#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n", out);
    fputs("#include \"spci.h\"\n", out);

    fputs("\nstatic const char SP_SOURCE[] = {\n", out);
    for (size_t i = 0; i < sp_len; i++) {
        fprintf(out, "0x%02x,", (unsigned char)sp_src[i]);
        if ((i & 15) == 15) fputc('\n', out);
    }
    fputs("0x00};\n", out);

    fputs(
        "\n"
        "int main(int argc, char **argv) {\n"
        "    setvbuf(stdout, NULL, _IOLBF, 0);\n"
        "    setvbuf(stderr, NULL, _IOLBF, 0);\n"
        "    const char *env_name = getenv(\"SPACELANG_NAME\");\n"
        "    const char *env_bus  = getenv(\"SPACELANG_BUS\");\n",
        out);
    if (bake_name) fprintf(out, "    my_name = strdup(\"%s\");\n", bake_name);
    fputs(
        "    if (env_name) my_name = strdup(env_name);\n"
        "    if (env_bus)  bus_dir = strdup(env_bus);\n"
        "    int keep_alive = 0;\n"
        "    /* Collect non-runtime args into user_argv so spaceforth can\n"
        "     * read them via :argc / :argv. Runtime flags --name/--bus/\n"
        "     * --serve are consumed here and excluded. */\n"
        "    char **uargv = malloc(sizeof(char*) * (argc + 1));\n"
        "    int uargc = 0;\n"
        "    for (int i = 1; i < argc; i++) {\n"
        "        if (!strcmp(argv[i], \"--name\") && i+1<argc) my_name = argv[++i];\n"
        "        else if (!strcmp(argv[i], \"--bus\") && i+1<argc) bus_dir = argv[++i];\n"
        "        else if (!strcmp(argv[i], \"--serve\")) keep_alive = 1;\n"
        "        else uargv[uargc++] = argv[i];\n"
        "    }\n"
        "    uargv[uargc] = NULL;\n"
        "    user_argc = uargc;\n"
        "    user_argv = (char *const *)uargv;\n"
        "    if (my_name && !bus_dir) bus_dir = (char*)\"/tmp/spacelang\";\n"
        "    if (my_name) { if (mesh_listen() < 0) return 1; }\n"
        "    feed(SP_SOURCE);\n"
        "    if (my_name && keep_alive) { for (;;) mesh_poll(1000); }\n"
        "    return 0;\n"
        "}\n",
        out);
}

static int run_cc(const char *cc, const char *root,
                  const char *cfile, const char *output, int debug) {
    char lib[PATH_MAX], inc[PATH_MAX + 2];
    snprintf(lib, sizeof lib, "%s/libspci.a", root);
    snprintf(inc, sizeof inc, "-I%s", root);

    pid_t pid = fork();
    if (pid < 0) { perror("fork"); return -1; }
    if (pid == 0) {
        if (debug)
            execlp(cc, cc, "-g", "-O0", "-w", inc, cfile, lib, "-o", output, (char*)NULL);
        else
            execlp(cc, cc, "-O2",       "-w", inc, cfile, lib, "-o", output, (char*)NULL);
        perror(cc);
        _exit(127);
    }
    int status = 0;
    if (waitpid(pid, &status, 0) < 0) { perror("waitpid"); return -1; }
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) return -1;
    return 0;
}

static void usage(void) {
    fputs(
        "usage: spcc [--emit-c] [--keep-c] [--debug] [--cc CC] [--root DIR]\n"
        "            input.sp [-o output]\n",
        stderr);
}

int main(int argc, char **argv) {
    const char *input = NULL;
    const char *output = NULL;
    const char *cc = getenv("CC");
    if (!cc || !*cc) cc = "cc";
    const char *root_override = NULL;
    const char *bake_name = NULL;
    int emit_c_only = 0;
    int keep_c = 0;
    int debug = 0;

    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--emit-c")) emit_c_only = 1;
        else if (!strcmp(argv[i], "--keep-c")) keep_c = 1;
        else if (!strcmp(argv[i], "--debug") || !strcmp(argv[i], "-g")) { debug = 1; keep_c = 1; }
        else if (!strcmp(argv[i], "--cc") && i+1 < argc) cc = argv[++i];
        else if (!strcmp(argv[i], "--root") && i+1 < argc) root_override = argv[++i];
        else if (!strcmp(argv[i], "--as") && i+1 < argc) bake_name = argv[++i];
        else if (!strcmp(argv[i], "-o") && i+1 < argc) output = argv[++i];
        else if (!strcmp(argv[i], "-h") || !strcmp(argv[i], "--help")) { usage(); return 0; }
        else if (argv[i][0] == '-') {
            fprintf(stderr, "spcc: unknown flag: %s\n", argv[i]);
            usage(); return 1;
        }
        else input = argv[i];
    }
    if (!input) { usage(); return 1; }
    if (!emit_c_only && !output) {
        size_t n = strlen(input);
        char *o = malloc(n + 5);
        memcpy(o, input, n + 1);
        if (n > 3 && !strcmp(o + n - 3, ".sp")) o[n-3] = 0;
        else strcat(o, ".out");
        output = o;
    }

    char *root = NULL;
    if (root_override) {
        if (!root_has_runtime(root_override)) {
            fprintf(stderr, "spcc: --root %s: missing spci.h/libspci.a\n", root_override);
            return 1;
        }
        root = strdup(root_override);
    } else {
        root = find_root();
        if (!root) {
            fprintf(stderr,
                "spcc: cannot locate runtime (spci.h, libspci.a).\n"
                "      Set SPACELANG_ROOT or pass --root DIR.\n");
            return 1;
        }
    }

    VSet visited = {0};
    Buf pp = {0};
    char real_in[PATH_MAX];
    if (!realpath(input, real_in)) {
        fprintf(stderr, "spcc: cannot open '%s': %s\n", input, strerror(errno));
        return 1;
    }
    vset_add(&visited, real_in);
    preprocess(real_in, &visited, &pp);
    char *sp_src = pp.data ? pp.data : strdup("");
    size_t sp_len = pp.len;
    for (size_t i = 0; i < visited.n; i++) free(visited.v[i]);
    free(visited.v);

    if (emit_c_only) {
        emit(stdout, sp_src, sp_len, bake_name);
        free(sp_src); free(root);
        return 0;
    }

    char cpath[PATH_MAX];
    if (keep_c) snprintf(cpath, sizeof cpath, "%s.c", output);
    else        snprintf(cpath, sizeof cpath, "/tmp/spcc-%d.c", (int)getpid());

    FILE *cf = fopen(cpath, "wb");
    if (!cf) { perror(cpath); return 1; }
    emit(cf, sp_src, sp_len, bake_name);
    fclose(cf);
    free(sp_src);

    int rc = run_cc(cc, root, cpath, output, debug);
    if (!keep_c) unlink(cpath);
    free(root);
    if (rc != 0) { fprintf(stderr, "spcc: %s failed\n", cc); return 3; }
    return 0;
}
