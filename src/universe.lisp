(defpackage #:spacelang.universe
  (:use #:cl
        #:spacelang.memory
        #:spacelang.term
        #:alexandria)
  (:import-from #:spacelang.memory)
  (:import-from #:str #:repeat)
  (:export
   #:universe
   #:space-instances
   #:get-memory
   #:*universe*
   #:print-universe!))
(in-package #:spacelang.universe)

(defclass universe ()
    ((space-instances
      :initarg :space-instances
      :initform (make-hash-table :test 'equalp)
      :accessor space-instances)))

(defun init-universe ()
  (let ((universe (make-instance 'universe)))
    (setf (gethash :home (space-instances universe))
          (make-instance 'space-memory :name :home :parent-universe universe))
    universe))

(defun add-machine (universe name machine)
  (setf (gethash name (space-instances universe)) machine))

(defun remove-machine (universe name)
  (setf (gethash name (space-instances universe)) nil))


(defun initialise-machine (universe new-machine-name)
  (setf (gethash new-machine-name (space-instances universe))
        (make-instance 'space-memory :name new-machine-name
                                     :parent-universe universe))
  (start-machine-thread! universe new-machine-name))

(defun get-memory (memory-name universe)
  (gethash memory-name
           (space-instances universe)))

(defun print-universe! ()
  "Prints the current universe."
  (labels ((print-sep! () (format t "~A~%" (repeat 80 "=")))
           (newline! () (format t "~%")))
    (newline!)
    (print-sep!)
    (loop :for memory :in (hash-table-values (space-instances *universe*))
          :do (progn
                (print-name! memory)
                (print-stack! memory)
                (print-dictionary! memory)
                (print-sep!)))))

(defparameter *universe* (init-universe))
