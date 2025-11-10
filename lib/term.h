#ifndef TERM_H_SEEN
#define TERM_H_SEEN

typedef struct Location_S {
    char *fileName;
    unsigned int row;
    unsigned int column;
} Location;

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
  unsigned int len;
} Keyword;

typedef struct Symbol_T {
  char *s;
  unsigned int len;
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
  Location l;
  TermT ty;
  union { // value
    Keyword kw;
    Symbol sy;
    Integer i;
    Double d;
    Thunk t;
  };
} Term;

// Pretty print functions
void term_print(const Term *term, int indent, int show_location);
void term_println(const Term *term);
void term_println_with_location(const Term *term);

#endif // term_c
