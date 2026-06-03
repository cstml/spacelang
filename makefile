.PHONY: all clean test DockerRun install uninstall

PREFIX ?= $(HOME)/.local
BINDIR  = $(PREFIX)/bin
LIBDIR  = $(PREFIX)/lib
INCDIR  = $(PREFIX)/include

all: spci spcc spco/spco spcd/spcd libspci.a

spci.o: spci.c spci.h
	cc -O2 -Wall -Wextra -c -o $@ spci.c

libspci.a: spci.o
	ar rcs $@ $<

spci: spci.o spci_main.c spci.h
	cc -O2 -Wall -Wextra -o $@ spci_main.c spci.o

spcc: spcc.c
	cc -O2 -Wall -Wextra -o $@ spcc.c

spco/spco: spco/spco.sp spcc libspci.a spci.h
	./spcc --as spco spco/spco.sp -o $@

spcd/spcd: spcd/spcd.sp spcd/dep.sp lib/str.sp lib/log.sp lib/git.sp lib/fset.sp lib/fmap.sp spcc libspci.a spci.h
	./spcc --as spcd spcd/spcd.sp -o $@

install: all
	install -d $(DESTDIR)$(BINDIR) $(DESTDIR)$(LIBDIR) $(DESTDIR)$(INCDIR)
	install -m 755 spci spcc spco/spco spcd/spcd $(DESTDIR)$(BINDIR)
	install -m 644 libspci.a $(DESTDIR)$(LIBDIR)
	install -m 644 spci.h $(DESTDIR)$(INCDIR)

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/spci $(DESTDIR)$(BINDIR)/spcc \
	      $(DESTDIR)$(BINDIR)/spco $(DESTDIR)$(BINDIR)/spcd \
	      $(DESTDIR)$(LIBDIR)/libspci.a $(DESTDIR)$(INCDIR)/spci.h

clean:
	rm -f spci spcc spco/spco spcd/spcd spci.o libspci.a

test:
	python3 test_harness.py

test-quick:
	python3 test_harness.py --quick

DockerRun:
	docker build . -t spacelang
	docker run -it spacelang
