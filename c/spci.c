/*
 * spci — single-process spacelang interpreter in C.
 *
 * Mirrors src/evaluator.lisp semantics for the subset:
 *   numbers, strings, words, thunks [...], booleans
 *   + - * / < > <= >= =
 *   dup swap drop if
 *   @ (bind)   ! (eval)   . (print)   , (format)   ~ (describe)
 *   :s (print stack)   :bye (exit)   { comments }
 *
 * Out of scope for v1: $ $! slurp cons dictionary-stack debug.
 * Those land once the supervisor / pipe transport exists.
 *
 * Build: cc -O2 -Wall -Wextra -o spci c/spci.c
 * REPL : ./spci         File: ./spci path.sp
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>

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
static void feed(const char *src) {
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

/* ---------- driver ---------- */

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
    if (argc >= 2) {
        char *src = slurp_file(argv[1]);
        feed(src);
        free(src);
        return 0;
    }

    /* REPL */
    char line[4096];
    printf("spci · spacelang in C · :bye to quit\n");
    for (;;) {
        printf("> "); fflush(stdout);
        if (!fgets(line, sizeof line, stdin)) { printf("\n"); break; }
        feed(line);
    }
    return 0;
}
