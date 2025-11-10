#ifndef TERM_C
#define TERM_C

#include "term.c"
#include <stdlib.h>

typedef struct Binding_T {
  char *name;
  Term term;
} Binding;

typedef struct Memory_T {
  Term stack[2048];
  unsigned int stack_ix;

  Term accumulator[2048];
  unsigned int acc_ix;
  int thunk_nesting_ix;

  Binding bindings[2048];
  unsigned int binding_count;
} Memory;

Memory* MkMemory(){
  Memory *m = malloc(sizeof(Memory));
  m->stack_ix = 0;
  m->binding_count = 0;
  m->thunk_nesting_ix = 0;
  return m;
}
#endif
