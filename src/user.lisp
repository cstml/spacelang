(defpackage #:spacelang.user
  (:use #:cl
        #:spacelang.parser
        #:spacelang.memory
        #:spacelang.term
        #:spacelang.evaluator
        #:spacelang.universe)
  (:local-nicknames (:a :alexandria))
  (:shadowing-import-from :alexandria "EMPTYP")
  (:import-from :alexandria #:read-file-into-string))
(in-package #:spacelang.user)

(defvar *home* (get-memory :home *universe*))

(defun run! (term)
  (spacelang.evaluator:evaluate *home* term))

(setf *debug-mode* t)
(setf *trace-mode* t)

;; Evaluating a function in a different machine and sending it back to this one.
(run! '(1 2 3 + + (:home) :send))
(run! '(bye))
(run! :send-bang)
(print-universe!)
