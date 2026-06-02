.PHONY: all clean test DockerRun

all: spci spcc spco libspci.a

spci.o: spci.c spci.h
	cc -O2 -Wall -Wextra -c -o $@ spci.c

libspci.a: spci.o
	ar rcs $@ $<

spci: spci.o spci_main.c spci.h
	cc -O2 -Wall -Wextra -o $@ spci_main.c spci.o

spcc: spcc.c
	cc -O2 -Wall -Wextra -o $@ spcc.c

spco: spco.c spci.h libspci.a
	cc -O2 -Wall -Wextra -o $@ spco.c libspci.a

clean:
	rm -f spci spcc spco spci.o libspci.a

test:
	python3 test_harness.py

test-quick:
	python3 test_harness.py --quick

DockerRun:
	docker build . -t spacelang
	docker run -it spacelang
