;;;; memory.lisp
(defpackage #:spacelang.memory
  (:use #:cl
        #:spacelang.term)
  (:import-from #:alexandria #:copy-hash-table #:hash-table-alist #:hash-table-values)
  (:import-from #:str #:repeat)
  (:export #:space-memory
           #:stack
           #:dictionary
           #:dictionary-level
           #:name
           #:parent-universe
           #:reset-memory
           #:reset-stack
           #:push-dictionary!
           #:pop-dictionary!
           #:print-stack!
           #:print-dictionary!
           #:print-name!
           #:print-memory!
           #:get-stack!
           #:set-stack!
           #:pop!
           #:get-word!
           #:get-word!?
           #:set-word!
           #:push!))
(in-package #:spacelang.memory)

(defparameter *silent-mode* t)
(defparameter *silent-rebind-mode* t)

(defclass space-memory ()
  ((stack
    :initarg :stack
    :initform '()
    :accessor stack)

   (dictionary
    :initarg :dictionary
    :initform (make-hash-table :test 'equalp)
    :accessor dictionary)

   (dictionary-level
    :initarg :dictionary-level
    :initform '()
    :accessor dictionary-level)

   (name
    :initarg :name
    :initform (error "Must set memory name!")
    :accessor name)

   (parent-universe
    :initarg :parent-universe
    :initform (error "Must set universe.")
    :accessor parent-universe)))

(defun reset-memory (memory)
  (setf memory (make-instance 'space-memory
                              :name (name memory)
                              :parent-universe (parent-universe memory))))

(defun reset-stack (memory)
  (setf (stack memory) '()))

(defun push-dictionary! (memory)
  (when (not *silent-mode*) (format t "Pushing dictionary!~%"))
  (push (dictionary memory) (dictionary-level memory))
  (setf (dictionary memory) (copy-hash-table (dictionary memory))))

(defun pop-dictionary! (memory)
  (when (not *silent-mode*) (format t "Popping dictionary!~%"))
  (let ((old-dictionary (pop (dictionary-level memory))))
    (if old-dictionary
        (setf (dictionary memory) old-dictionary)
        (progn (format t "Last dictionary popped!~%")
               (format t "Resetting dictionary!~%")
               (setf (dictionary memory) (make-hash-table :test 'equalp))))))

(defun print-stack! (memory)
  "Prints the current stack."
  (format t "stack: ~%")
  (loop :for term :in (reverse (stack memory))
        :do (format t "~a" (pretty-term term)))
  (format t "~%~%"))

(defun print-dictionary! (memory)
  "Prints the current dictionary."
  (format t "dicty: ~%")
  (loop :for x :in (hash-table-alist (dictionary memory))
        :do (let ((binder (car x))
                  (term (cdr x)))
              (format t "~a ~~ ~a ~%" binder (pretty-term term)))))

(defun print-name! (memory)
  "Prints the current memory name."
  (format t "memory: ~s. ~%" (name memory)))

(defun print-memory! (memory)
  "Prints the current memory."
  (labels ((print-sep! () (format t "~s~%" (repeat 80 "-")))
           (newline! () (format t "~%")))
    (newline!)
    (print-sep!)
    (print-stack! memory)
    (print-dictionary! memory)
    (print-sep!)))

;; Stack
(defun get-stack! (memory)
  (stack memory))

(defun set-stack! (memory s)
  (setf (stack memory) s))

(defun push! (memory term)
  (push term (stack memory)))

(defun pop! (memory)
  (if (/= 0  (length (stack memory)))
      (pop (stack memory))
      (error "Stack underflow!")))

;; Dictionary
(defun get-word! (memory s-word)
  (multiple-value-bind (result exists) (gethash s-word (dictionary memory))
    (if exists
        result
        (error "Unbound Word: \"~s\" " s-word))))

;; Dictionary
(defun get-word!? (memory s-word)
  (gethash s-word (dictionary memory)))

(defun set-word! (memory s-word term)
  (let ((exists (gethash s-word (dictionary memory))))
    (if exists
        (progn
          (when (not *silent-rebind-mode*)
            (format t "Rebinding word \"~s\".~%" s-word))
          (setf (gethash s-word (dictionary memory)) term))
        (setf (gethash s-word (dictionary memory)) term))))

(defmethod evaluate ((memory space-memory)
                     (term (eql :r)))
  (reset-memory memory)
  (when (not *silent-mode*) (format t "Memory reset.~%")))

(defmethod evaluate ((memory space-memory)
                     (term (eql :rs)))
  (reset-stack memory)
  (when (not *silent-mode*) (format t "Stack reset.~%")))
