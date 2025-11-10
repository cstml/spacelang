#include <CUnit/Basic.h>
#include <CUnit/CUnit.h>
#include "evaluator.c"
#include "term.c"
#include "memory.c"
#include "parser.c"


void test_parse() {
  cursor = "1234";
  Term* x = PTerm();
  CU_ASSERT(x->ty == INTEGER);
  CU_ASSERT(x->i.v == 1234);
}
