#ifndef EVALUATOR_H_SEEN
#define EVALUATOR_H_SEEN
#include "term.h"
#include "memory.h"

int push(Memory *m, Term *t);
int pop(Memory *m, Term *t);
int pop_(Memory *m);
int get_binding (Memory *m, char *b, Term *bound_term);
int upsert_binding(Memory *m, char *b, Term *bound_term);
int bind(Memory *m, Term *binder_term, Term *bound_term);
int evaluate(Memory *m, Term *t);

#endif //EVALUATOR_H_SEEN