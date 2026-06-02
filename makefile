lisp_files := $(wildcard *.lisp)
asd_files := $(wildcard *.asd)

bin/spci: bin/ $(lisp_files) $(asd_files)
	sbcl --disable-debugger \
	     --eval '(ql:quickload "spacelang")' \
	     --eval '(asdf:load-system "spacelang")' \
	     --eval '(use-package :spacelang)' \
	     --eval "(sb-ext:save-lisp-and-die #p\"bin/spci\" :toplevel #'space! :executable t)"

bin/:
	mkdir bin

.PHONY: c c-clean DockerRun

c: c/spci c/spcc

c/spci: c/spci.c c/spci.h c/spci_main.c
	cc -O2 -Wall -Wextra -o $@ c/spci.c c/spci_main.c

c/spci_blob.h: c/spci.c c/spci.h
	cd c && xxd -i spci.h >  spci_blob.h && xxd -i spci.c >> spci_blob.h

c/spcc: c/spcc.c c/spci_blob.h
	cc -O2 -Wall -Wextra -o $@ c/spcc.c

c-clean:
	rm -f c/spci c/spcc c/spci_blob.h


DockerRun:
	docker build . -t spacelang
	docker run -it spacelang

compiler:
	sbcl \
    --disable-debugger \
	  --eval '(ql:quickload "spacelang")' \
	  --eval '(asdf:load-system "spacelang")' \
	  --eval '(use-package :spacelang.compiler)' \
    --eval '(compiler)'

bake:
	sbcl --disable-debugger \
	     --eval '(ql:quickload "spacelang")' \
	     --eval '(asdf:load-system "spacelang")' \
	     --eval '(use-package :spacelang.compiler)' \
       --eval '(bake-binary\
									 #p "/home/cstml/.quicklisp/local-projects/spacelang/bin/baked"\
									 #p"/home/cstml/.quicklisp/local-projects/spacelang/example/add_2.sp")'
