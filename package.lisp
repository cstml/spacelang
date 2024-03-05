;;;; package.lisp

(defpackage #:spacelang
  (:use #:cl
        #:smug
        #:str
        #:alexandria
        #:bt-semaphore)
  (:shadowing-import-from :alexandria "EMPTYP")
  (:export #:space!
           ;; Globals
           #:*universe*
           #:*trace-mode*))

;; Optimize to type-check
;; (declaim (optimize (safety 3)))
