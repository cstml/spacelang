#ifndef term_c
#define term_c
typedef enum TermT_E {
  NOOP,
  KEYWORD,
  SYMBOL,
  INTEGER,
  DOUBLE,
  THUNK_CLOSE,
  THUNK_OPEN,
  EVAL,
  BIND,
  THUNK,
}TermT;

struct Term_T;

typedef struct Keyword_T {
  char *s;
} Keyword;

typedef struct Symbol_T {
  char *s;
}Symbol;

typedef struct Integer_T {
  long long v;
}Integer;

typedef struct Double_T {
  long double v;
}Double;

typedef struct Thunk_T {
  struct Term_T* ts;
  unsigned long long term_count;
}Thunk;

typedef struct Term_T {
  TermT ty;
  union { // value
    Keyword kw;
    Symbol sy;
    Integer i;
    Double d;
    Thunk t;
  };
} Term;

#endif // term_c
