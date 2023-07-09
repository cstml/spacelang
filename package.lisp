;;;; package.lisp

(defpackage #:spacelang
  (:use #:cl
        #:smug
        #:str
        #:alexandria
        #:bt-semaphore)
  (:shadowing-import-from :alexandria "EMPTYP")
  (:export #:space!))
