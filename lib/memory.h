#ifndef MEMORY_H_SEEN
#define MEMORY_H_SEEN

#include "term.h"

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

Memory* MkMemory();

// Pretty print functions
void memory_print(const Memory *m);
void memory_print_stack(const Memory *m);
void memory_print_accumulator(const Memory *m);
void memory_print_bindings(const Memory *m);
void memory_free(Memory *m);

#endif // MEMORY_H_SEEN
