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

.PHONY: DockerRun

DockerRun:
	docker build . -t spacelang
	docker run -it spacelang
