;;;; package.lisp

(defpackage #:spacelang/test
  (:use #:cl
        #:alexandria
        #:fiveam))

;; enable ^() synyax for lambdas
(cl-punch:enable-punch-syntax)
