.PHONY: all clean test test-quick DockerRun install uninstall

PREFIX ?= $(HOME)/.local
BINDIR  = $(PREFIX)/bin
LIBDIR  = $(PREFIX)/lib
INCDIR  = $(PREFIX)/include

CFLAGS = -O2 -Wall -Wextra -Iinclude

all: bin/spci bin/spcc bin/spco bin/spcd lib/libspci.a

bin lib build:
	mkdir -p $@

build/runtime.o: runtime.c include/spci.h | build
	cc $(CFLAGS) -c -o $@ runtime.c

lib/libspci.a: build/runtime.o | lib
	ar rcs $@ $<

bin/spci: build/runtime.o spci.c include/spci.h | bin
	cc $(CFLAGS) -o $@ spci.c build/runtime.o

bin/spcc: spcc.c | bin
	cc $(CFLAGS) -o $@ spcc.c

bin/spco: spco/spco.sp bin/spcc lib/libspci.a include/spci.h | bin
	./bin/spcc --as spco spco/spco.sp -o $@

bin/spcd: spcd/spcd.sp spcd/dep.sp \
          stdlib/str.sp stdlib/log.sp stdlib/git.sp stdlib/fset.sp stdlib/fmap.sp \
          bin/spcc lib/libspci.a include/spci.h | bin
	./bin/spcc --as spcd spcd/spcd.sp -o $@

install: all
	install -d $(DESTDIR)$(BINDIR) $(DESTDIR)$(LIBDIR) $(DESTDIR)$(INCDIR)
	install -m 755 bin/spci bin/spcc bin/spco bin/spcd $(DESTDIR)$(BINDIR)
	install -m 644 lib/libspci.a $(DESTDIR)$(LIBDIR)
	install -m 644 include/spci.h $(DESTDIR)$(INCDIR)

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/spci $(DESTDIR)$(BINDIR)/spcc \
	      $(DESTDIR)$(BINDIR)/spco $(DESTDIR)$(BINDIR)/spcd \
	      $(DESTDIR)$(LIBDIR)/libspci.a $(DESTDIR)$(INCDIR)/spci.h

clean:
	rm -rf bin/spci bin/spcc bin/spco bin/spcd build/runtime.o lib/libspci.a

test:
	python3 test_harness.py

test-quick:
	python3 test_harness.py --quick

DockerRun:
	docker build . -t spacelang
	docker run -it spacelang
