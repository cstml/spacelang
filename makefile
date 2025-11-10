src_files := $(wildcard *.c)
lib_files := $(wildcard *.h)

bin/spc:
	sbcl --disable-debugger \
	     --eval '(ql:quickload "spacelang")' \
	     --eval '(asdf:load-system "spacelang")' \
	     --eval '(use-package :spacelang)' \
			 --eval "(declaim (optimize (speed 3) (debug 0) (safety 0)))" \
	     --eval "(sb-ext:save-lisp-and-die #p\"bin/spci\" :toplevel #'space! :executable t)"

bin/:
	mkdir bin

.PHONY: DockerRun

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
