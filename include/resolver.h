/* resolver.h — shared :require path resolution for spcc + spci runtime.
 *
 * Header-only on purpose: both translation units have their own small needs,
 * and the resolver is mechanical enough that one .h is cheaper than another
 * link unit.
 *
 * Resolution order, per call site:
 *   1. Walk up from `source_dir` to find the nearest `deps.sp` (the module
 *      root). Parse its `"local" "url" deps/override` lines.
 *   2. If `spec` starts with any declared override `url` (followed by `/`
 *      or end-of-string), rewrite: drop the prefix, prepend the override's
 *      `local` (joined with the deps.sp dir if local is relative).
 *   3. Otherwise return NULL — caller falls back to its own scheme
 *      (sibling-relative for spcc, sibling+CWD for runtime).
 *
 * Cycles: irrelevant here — we don't recursively resolve overrides; one
 * pass, returns a candidate path. Caller fopens/realpaths it.
 */
#ifndef SPC_RESOLVER_H
#define SPC_RESOLVER_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/stat.h>
#include <limits.h>

typedef struct { char *local; char *url; } SpcOverride;
typedef struct {
    SpcOverride *items;
    size_t       n;
    char        *root_dir;   /* dir containing deps.sp; NULL if no manifest */
} SpcOverrides;

static int spc_file_exists_(const char *path) {
    struct stat st;
    return stat(path, &st) == 0 && S_ISREG(st.st_mode);
}

/* Walk up from start_dir looking for deps.sp. Writes dir into out
 * (size >= PATH_MAX) and returns 1 on success, 0 if not found. */
static int spc_find_module_root(const char *start_dir, char *out) {
    char buf[PATH_MAX];
    if (!start_dir || !*start_dir) return 0;
    if (start_dir[0] != '/') {
        /* relative — make it absolute relative to CWD */
        if (!getcwd(buf, sizeof buf)) return 0;
        size_t bl = strlen(buf);
        if (bl + 1 + strlen(start_dir) + 1 >= sizeof buf) return 0;
        buf[bl++] = '/';
        strcpy(buf + bl, start_dir);
    } else {
        snprintf(buf, sizeof buf, "%s", start_dir);
    }
    for (;;) {
        char cand[PATH_MAX];
        snprintf(cand, sizeof cand, "%s/deps.sp", buf);
        if (spc_file_exists_(cand)) {
            snprintf(out, PATH_MAX, "%s", buf);
            return 1;
        }
        char *slash = strrchr(buf, '/');
        if (!slash) return 0;
        if (slash == buf) { buf[1] = 0; break; }  /* root */
        *slash = 0;
    }
    char cand[PATH_MAX];
    snprintf(cand, sizeof cand, "%s/deps.sp", buf);
    if (spc_file_exists_(cand)) { snprintf(out, PATH_MAX, "%s", buf); return 1; }
    return 0;
}

/* Tokenize deps.sp and pick out `"local" "url" deps/override` triples. */
static void spc_load_overrides(const char *root_dir, SpcOverrides *o) {
    o->items = NULL; o->n = 0; o->root_dir = root_dir ? strdup(root_dir) : NULL;
    if (!root_dir) return;
    char path[PATH_MAX];
    snprintf(path, sizeof path, "%s/deps.sp", root_dir);
    FILE *f = fopen(path, "rb"); if (!f) return;
    fseek(f, 0, SEEK_END); long n = ftell(f); fseek(f, 0, SEEK_SET);
    char *src = malloc(n + 1);
    if (fread(src, 1, n, f) != (size_t)n) { /* tolerate short read */ }
    src[n] = 0; fclose(f);

    char *strs[2] = { NULL, NULL };
    size_t cap = 0;
    long i = 0;
    while (i < n) {
        char c = src[i];
        if (c == '{') {  /* comment */
            while (i < n && src[i] != '}') i++;
            if (i < n) i++;
            continue;
        }
        if (c == '"' || c == '\'' || c == '`') {
            char q = c; long start = ++i;
            while (i < n && src[i] != q) i++;
            char *s = malloc(i - start + 1);
            memcpy(s, src + start, i - start); s[i - start] = 0;
            free(strs[0]); strs[0] = strs[1]; strs[1] = s;
            if (i < n) i++;
            continue;
        }
        if (isspace((unsigned char)c) || c == '[' || c == ']') { i++; continue; }
        /* word */
        long start = i;
        while (i < n) {
            unsigned char wc = src[i];
            if (isspace(wc) || wc == '[' || wc == ']' || wc == '{'
                || wc == '"' || wc == '\'' || wc == '`') break;
            i++;
        }
        long wlen = i - start;
        if (wlen == 13 && !memcmp(src + start, "deps/override", 13)
            && strs[0] && strs[1]) {
            if (o->n == cap) {
                cap = cap ? cap * 2 : 4;
                o->items = realloc(o->items, cap * sizeof *o->items);
            }
            o->items[o->n].local = strs[0]; strs[0] = NULL;
            o->items[o->n].url   = strs[1]; strs[1] = NULL;
            o->n++;
        }
    }
    free(strs[0]); free(strs[1]);
    free(src);
}

static void spc_free_overrides(SpcOverrides *o) {
    for (size_t i = 0; i < o->n; i++) {
        free(o->items[i].local); free(o->items[i].url);
    }
    free(o->items); free(o->root_dir);
    o->items = NULL; o->n = 0; o->root_dir = NULL;
}

/* If `spec` matches any override URL (as a path prefix), return the
 * rewritten absolute-ish path (malloc'd). Otherwise return NULL. */
static char *spc_resolve_via_overrides(const char *spec, const SpcOverrides *o) {
    if (!o || !o->root_dir) return NULL;
    for (size_t i = 0; i < o->n; i++) {
        const char *url = o->items[i].url;
        size_t ul = strlen(url);
        if (strncmp(spec, url, ul) != 0) continue;
        if (spec[ul] != '/' && spec[ul] != 0) continue;  /* prefix must end on /  */
        const char *rest = spec + ul + (spec[ul] == '/' ? 1 : 0);
        const char *local = o->items[i].local;
        char base[PATH_MAX];
        if (local[0] == '/') {
            snprintf(base, sizeof base, "%s", local);
        } else {
            snprintf(base, sizeof base, "%s/%s", o->root_dir, local);
        }
        size_t outsz = strlen(base) + 1 + strlen(rest) + 1;
        char *out = malloc(outsz);
        if (*rest) snprintf(out, outsz, "%s/%s", base, rest);
        else       snprintf(out, outsz, "%s", base);
        return out;
    }
    return NULL;
}

#endif /* SPC_RESOLVER_H */
