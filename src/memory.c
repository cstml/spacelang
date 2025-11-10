#include "memory.h"
#include "term.h"
#include <stdio.h>
#include <stdlib.h>

void memory_print_stack(const Memory *m) {
  printf("┌─ Stack (depth: %u) ─┐\n", m->stack_ix);
  if (m->stack_ix == 0) {
    printf("│ <empty>            │\n");
  } else {
    for (int i = m->stack_ix - 1; i >= 0; i--) {
      printf("│ [%d] ", i);
      // Print term inline without newline
      const Term *t = &m->stack[i];
      switch (t->ty) {
        case NOOP:
          printf("NOOP\n");
          break;
        case KEYWORD:
          printf("KEYWORD: \"%.*s\"\n", t->kw.len, t->kw.s);
          break;
        case SYMBOL:
          printf("SYMBOL: \"%.*s\"\n", t->sy.len, t->sy.s);
          break;
        case INTEGER:
          printf("INTEGER: %lld\n", t->i.v);
          break;
        case DOUBLE:
          printf("DOUBLE: %Lf\n", t->d.v);
          break;
        case THUNK_CLOSE:
          printf("THUNK_CLOSE\n");
          break;
        case THUNK_OPEN:
          printf("THUNK_OPEN\n");
          break;
        case EVAL:
          printf("EVAL\n");
          break;
        case BIND:
          printf("BIND\n");
          break;
        case THUNK:
          printf("THUNK (%llu terms)\n", t->t.term_count);
          break;
        default:
          printf("UNKNOWN\n");
          break;
      }
    }
  }
  printf("└────────────────────┘\n");
}

void memory_print_accumulator(const Memory *m) {
  printf("┌─ Accumulator (depth: %u) ─┐\n", m->acc_ix);
  if (m->acc_ix == 0) {
    printf("│ <empty>                   │\n");
  } else {
    for (int i = m->acc_ix - 1; i >= 0; i--) {
      printf("│ [%d] ", i);
      const Term *t = &m->accumulator[i];
      switch (t->ty) {
        case NOOP:
          printf("NOOP\n");
          break;
        case KEYWORD:
          printf("KEYWORD: \"%.*s\"\n", t->kw.len, t->kw.s);
          break;
        case SYMBOL:
          printf("SYMBOL: \"%.*s\"\n", t->sy.len, t->sy.s);
          break;
        case INTEGER:
          printf("INTEGER: %lld\n", t->i.v);
          break;
        case DOUBLE:
          printf("DOUBLE: %Lf\n", t->d.v);
          break;
        case THUNK_CLOSE:
          printf("THUNK_CLOSE\n");
          break;
        case THUNK_OPEN:
          printf("THUNK_OPEN\n");
          break;
        case EVAL:
          printf("EVAL\n");
          break;
        case BIND:
          printf("BIND\n");
          break;
        case THUNK:
          printf("THUNK (%llu terms)\n", t->t.term_count);
          break;
        default:
          printf("UNKNOWN\n");
          break;
      }
    }
  }
  printf("└───────────────────────────┘\n");
}

void memory_print_bindings(const Memory *m) {
  printf("┌─ Bindings (count: %u) ─┐\n", m->binding_count);
  if (m->binding_count == 0) {
    printf("│ <none>                │\n");
  } else {
    for (unsigned int i = 0; i < m->binding_count; i++) {
      printf("│ \"%s\" → ", m->bindings[i].name);
      const Term *t = &m->bindings[i].term;
      switch (t->ty) {
        case NOOP:
          printf("NOOP\n");
          break;
        case KEYWORD:
          printf("KEYWORD: \"%.*s\"\n", t->kw.len, t->kw.s);
          break;
        case SYMBOL:
          printf("SYMBOL: \"%.*s\"\n", t->sy.len, t->sy.s);
          break;
        case INTEGER:
          printf("INTEGER: %lld\n", t->i.v);
          break;
        case DOUBLE:
          printf("DOUBLE: %Lf\n", t->d.v);
          break;
        case THUNK_CLOSE:
          printf("THUNK_CLOSE\n");
          break;
        case THUNK_OPEN:
          printf("THUNK_OPEN\n");
          break;
        case EVAL:
          printf("EVAL\n");
          break;
        case BIND:
          printf("BIND\n");
          break;
        case THUNK:
          printf("THUNK (%llu terms)\n", t->t.term_count);
          break;
        default:
          printf("UNKNOWN\n");
          break;
      }
    }
  }
  printf("└───────────────────────┘\n");
}

void memory_print(const Memory *m) {
  if (!m) {
    printf("<NULL Memory>\n");
    return;
  }
  
  printf("\n╔═══════════════════════════════════════╗\n");
  printf("║         MEMORY STATE                  ║\n");
  printf("╠═══════════════════════════════════════╣\n");
  printf("║ Thunk Nesting Level: %d               \n", m->thunk_nesting_ix);
  printf("╚═══════════════════════════════════════╝\n\n");
  
  memory_print_stack(m);
  printf("\n");
  memory_print_accumulator(m);
  printf("\n");
  memory_print_bindings(m);
  printf("\n");
}

void memory_free(Memory *m) {
  if (m) {
    // TODO: Free any dynamically allocated strings in bindings
    free(m);
  }
}

Memory* MkMemory(){
  Memory *m = malloc(sizeof(Memory));
  m->stack_ix = 0;
  m->binding_count = 0;
  m->thunk_nesting_ix = 0;
  return m;
}
