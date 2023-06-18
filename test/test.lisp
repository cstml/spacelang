;;;; test.lisp
(in-package #:spacelang/test)

(require :fiveam)

(test refl
  (is (= 1 1)))

(format t "Hello from spacelang/test.~%")

(fiveam:run!)
