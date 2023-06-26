build:
	sbcl --eval '(asdf:load-system "spacelang")' \
			 --eval '(use-package :spacelang)' \
			 --eval "(sb-ext:save-lisp-and-die #p\"spacelang\" :toplevel #'space! :executable t)"
