(defpackage #:spacelang.term
  (:use #:cl)
  (:import-from #:alexandria #:copy-hash-table)
  (:import-from #:str #:repeat)
  (:export
   #:pretty-term
   #:format-term))
(in-package #:spacelang.term)

(defun pretty-term (term)
  (cond
    ((stringp term) (format nil "\"~a\"" term))
    ((consp term) (str:concat
                   (format nil "[")
                   (apply #'str:concat  (loop :for x :in term :collect (pretty-term x)))
                   (format nil "]")))
    (t (format nil " ~a "term))))

(defun format-term (term)
  (cond
    ((consp term) (format nil "[~s]"
                          (apply #'str:concat  (loop :for x :in term :collect (pretty-term x)))))
    (t (format nil "~a" term))))
