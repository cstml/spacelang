#include "term.c"
#include "memory.c"
#include <stdio.h>
#include <string.h>

int push(Memory *m, Term *t) {
  m->stack[m->stack_ix] = *t;
  m->stack_ix+=1;
  return 0;
}

int pop(Memory *m, Term *t) {
  if (m->stack_ix == 0) {
    fprintf(stderr,"stack underflow\n");
    return -1;
  }
  memcpy(t, &m->stack[m->stack_ix - 1], sizeof(Term));
  m->stack[m->stack_ix - 1].ty = NOOP;
  m->stack_ix -= 1;
  return 0;
}

// like pop but ignores the term
int pop_(Memory *m) {
  if (m->stack_ix == 0) {
    fprintf(stderr,"stack underflow\n");
    return -1;
  }
  m->stack[m->stack_ix - 1].ty = NOOP;
  m->stack_ix -= 1;
  return 0;
}
/** Copies the current binding to bound_term. Returns the index of the current
 ** position of the binding.
 **/
int get_binding (Memory *m, char *b, Term *bound_term) {
  for (int i = 0; i < m->binding_count; i++) {
    if(strcmp(m->bindings[i].name,b) == 0){
      memcpy(bound_term, &m->bindings[i].term, sizeof(Term));
      return i;
    }
  }
  return -1;
}

int upsert_binding(Memory *m, char *b, Term *bound_term) {
  Term current_term;
  int ix = get_binding(m, b, &current_term);
  if (ix != -1) {
    m->bindings[ix].term = *bound_term;
  } else {
    m->bindings[m->binding_count].term = *bound_term;
    int size = strlen(b) + 1; // doesn't count the \0
    m->bindings[m->binding_count].name = malloc(size * sizeof(char));
    strcpy(m->bindings[m->binding_count].name,b);
    m->binding_count++;
  }
  return 0;
}

int bind(Memory *m, Term *binder_term, Term *bound_term) {
  if (binder_term->ty != THUNK) {
    fprintf(stderr,"binder must be a keyword in a thunk");
    return -1;
  }

  if (binder_term->t.term_count != 1) {
    fprintf(stderr,"thunk must have exactly 1 element");
    return -1;
  }

  if (binder_term->t.ts[0].ty != SYMBOL) {
    fprintf(stderr,"thunk must contain a symbol");
    return -1;
  }

  return upsert_binding(m,binder_term->t.ts[0].sy.s,bound_term);
}

int evaluate(Memory *m, Term *t) {
  Term bound_term;
  Term t1,t2;
  switch (t->ty){

  case KEYWORD:
    push(m,t);
    break;

  case BIND:
    if (pop(m, &t1) != 0) {
      return -1;
    }
    if (pop(m, &t2) != 0) {
      return -1;
    }
    return bind(m,&t1,&t2);

  case SYMBOL:
    if (0 != get_binding(m, t->sy.s, &bound_term)) {
      fprintf(stderr,"unbound term %s\n", t->sy.s);
      return -1;
    }
    return evaluate(m,&bound_term);

  case THUNK:
    push(m,t);

  default:
    break;
  }
  return 0;
}
