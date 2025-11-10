#include <CUnit/Basic.h>
#include <CUnit/CUnit.h>
#include "evaluator.c"
#include "term.c"
#include "memory.c"
#include "parser.c"
#include "parser_test.c"

void test_push() {
  Memory* m = MkMemory();
  Term t1,t2,t3;
  t1.ty = THUNK_OPEN;
  t2.ty = DOUBLE;
  t2.d.v = 100.2;
  push(m,&t1);
  push(m,&t2);
  pop(m,&t3);
  CU_ASSERT(m->stack[0].ty ==  t1.ty);
  CU_ASSERT(m->stack[1].ty ==  t2.ty);
  CU_ASSERT(m->stack[1].d.v ==  t2.d.v);
  CU_ASSERT(memcmp(&m->stack[1],&t2,sizeof(Term)) == 0);
  CU_ASSERT(memcmp(&t2,&t3,sizeof(Term)) == 0);
}

void test_pop() {
  Memory* m = MkMemory();
  Term t1,t2;
  t1.ty = DOUBLE;
  t1.d.v = 100.2;
  push(m,&t1);
  pop(m,&t2);
  CU_ASSERT(memcmp(&t1,&t2,sizeof(Term)) == 0);
  free(m);
}

void test_bind() {
    Memory* m = MkMemory();
    char binder[40] = "hello";
    Term thunk[1] = {{.ty = SYMBOL, .sy = binder}};
    Term t1 = {
      .ty = THUNK,
      .t = { .ts = thunk, .term_count = 1 },
    };

    Term t2 = { .ty = INTEGER, .i = 3};
    push(m,&t2);
    push(m,&t1);

    Term t3 = {.ty = BIND};

    evaluate(m,&t3);

    Term t4;
    get_binding(m,binder,&t4);

    CU_ASSERT(memcmp(&t2,&t4,sizeof(Term)) == 0);
    free(m);
}


int main() {
    CU_initialize_registry();
    CU_pSuite suite = CU_add_suite("Evaluator Tests", 0, 0);
    CU_add_test(suite, "test push", test_push);
    CU_add_test(suite, "test pop", test_pop);
    CU_add_test(suite, "test bind", test_bind);
    // Parser Tests
    CU_pSuite suite2 = CU_add_suite("Parser Tests", 0, 0);
    CU_add_test(suite2, "test parse", test_parse);
    CU_basic_run_tests();
    CU_cleanup_registry();
    return 0;
}
