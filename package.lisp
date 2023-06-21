;;;; package.lisp

(defpackage #:spacelang
  (:use #:cl
        #:smug
        #:str
        #:alexandria)
  (:shadowing-import-from :alexandria "EMPTYP")
  (:export #:space!))
