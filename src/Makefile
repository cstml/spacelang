spc: main.c term.c
	gcc -g main.c term.c -o spc

test.out: evaluate_test.c
	gcc -g evaluate_test.c -lcunit -o test.out

.PHONY: run-test
run-test: test.out
	./test
