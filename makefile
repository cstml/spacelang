.PHONY: all clean test test-quick DockerRun install uninstall

PREFIX ?= $(HOME)/.local
BINDIR  = $(PREFIX)/bin
LIBDIR  = $(PREFIX)/lib
INCDIR  = $(PREFIX)/include

SPC_VERSION = v0.0.1
CFLAGS = -O2 -Wall -Wextra -Iinclude -DSPC_VERSION=\"$(SPC_VERSION)\"

all: bin/spci bin/spcc bin/spco bin/spcd bin/spct lib/libspci.a

bin lib build:
	mkdir -p $@

build/runtime.o: runtime.c include/spci.h include/resolver.h makefile | build
	cc $(CFLAGS) -c -o $@ runtime.c

lib/libspci.a: build/runtime.o | lib
	ar rcs $@ $<

bin/spci: build/runtime.o spci.c include/spci.h makefile | bin
	cc $(CFLAGS) -o $@ spci.c build/runtime.o

bin/spcc: spcc.c include/resolver.h makefile | bin
	cc $(CFLAGS) -o $@ spcc.c

bin/spco: spco/spco.sp bin/spcc lib/libspci.a include/spci.h | bin
	./bin/spcc --as spco spco/spco.sp -o $@

bin/spcd: spcd/spcd.sp spcd/dep.sp \
          stdlib/str.sp stdlib/log.sp stdlib/git.sp stdlib/fset.sp stdlib/fmap.sp \
          bin/spcc lib/libspci.a include/spci.h | bin
	./bin/spcc --as spcd spcd/spcd.sp -o $@

bin/spct: spct/spct.sp stdlib/test.sp stdlib/str.sp \
          bin/spcc lib/libspci.a include/spci.h | bin
	./bin/spcc spct/spct.sp -o $@

install: all
	install -d $(DESTDIR)$(BINDIR) $(DESTDIR)$(LIBDIR) $(DESTDIR)$(INCDIR)
	install -m 755 bin/spci bin/spcc bin/spco bin/spcd bin/spct $(DESTDIR)$(BINDIR)
	install -m 644 lib/libspci.a $(DESTDIR)$(LIBDIR)
	install -m 644 include/spci.h $(DESTDIR)$(INCDIR)

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/spci $(DESTDIR)$(BINDIR)/spcc \
	      $(DESTDIR)$(BINDIR)/spco $(DESTDIR)$(BINDIR)/spcd \
	      $(DESTDIR)$(BINDIR)/spct \
	      $(DESTDIR)$(LIBDIR)/libspci.a $(DESTDIR)$(INCDIR)/spci.h

clean:
	rm -rf bin/spci bin/spcc bin/spco bin/spcd bin/spct build/runtime.o lib/libspci.a

test:
	python3 test_harness.py

test-quick:
	python3 test_harness.py --quick

DockerRun:
	docker build . -t spacelang
	docker run -it spacelang
