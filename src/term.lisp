(defpackage #:spacelang.term
  (:use #:cl)
  (:import-from #:alexandria #:copy-hash-table)
  (:import-from #:str #:repeat)
  (:export
   #:pretty-term
   #:format-term
   #:s-terms
   #:s-term
   #:s-instruction
   #:s-word
   #:s-number
   #:s-keyword
   #:s-send
   #:s-eval
   #:s-bind
   #:s-thunk-open
   #:s-thunk-close
   #:s-dictionary-up
   #:s-dictionary-down
   #:s-noop
   #:s-read
   #:s-symbol))
(in-package #:spacelang.term)

;;;
;;; Terms  = Term Space Term*
;;;
;;; Space = [ \n\t;]+
;;;       | Comment
;;;
;;;
;;; Comment = #| .* |#
;;;
;;;
;;; Term = Word
;;;      | Instruction
;;;
;;; DictionaryUp = "("
;;;
;;; DictionaryDown = ")"
;;;
;;; Instruction = Send
;;;             | Eval
;;;             | Bind
;;;             | Scoop ;; like cons at a location
;;;             | ThunkOpen
;;;             | ThunkClose
;;;             | ReverseThunkOpen
;;;             | ReverseThunkClose
;;;             | DictionaryUp
;;;             | DictionaryDown
;;;             | NoOp
;;;
;;; Starts appendign to a thunk
;;;
;;; ThunkOpen = "["
;;;
;;;
;;; Closes the thunk
;;;
;;; ThunkClose = "]"
;;;
;;;
;;; Starts appending to a thunk, that gets reversed/flipped  upon closing.
;;;
;;; ReverseThunkOpen = "[|"
;;;
;;;
;;; Closes a reverse thunk.
;;;
;;; ReverseThunkClose = "|]"
;;;
;;;
;;;
;;; Send = "$"
;;;      | "send"
;;;
;;; Bind = "@"
;;;      | "bind"
;;;
;;; Eval = "!"
;;;      | "eval"
;;;
;;; Word = Number
;;;      | Symbol
;;;      | Keyword
;;;      | Text
;;;      | Character
;;;      | Boolean
;;;
;;; Number = Float
;;;        | Integer
;;;
;;; Symbol = [a-zA-Z*-+_/=]
;;;

(defclass s-terms ()
  ((s-terms :initarg :terms :accessor s-terms)))

(defclass s-term () ())

(defclass s-instruction (s-term) ())

(defclass s-send (s-instruction) ())
(defclass s-eval (s-instruction) ())
(defclass s-bind (s-instruction) ())
(defclass s-thunk-open (s-instruction) ())
(defclass s-thunk-close (s-instruction) ())
(defclass s-dictionary-up (s-instruction) ())
(defclass s-dictionary-down (s-instruction) ())
(defclass s-noop (s-instruction) ())
(defclass s-read (s-instruction) ())

(defclass s-word (s-term) ())

(defclass s-symbol (s-word)
  ((s-symbol :initarg :s-symbol :accessor s-symbol)))

(defclass s-number (s-word)
  ((s-number :initarg :s-number :accessor s-number)))

(defclass s-keyword (s-word)
  ((s-keyword :initarg :s-keyword )))

(defclass s-text (s-word)
  ((s-text :initarg :s-text )))

(defclass s-char (s-word)
  ((s-char :initarg :s-char )))

(defclass s-bool (s-word)
  ((s-bool :initarg :s-bool )))

(defun pretty-term (term)
  (cond
    ((stringp term) (format nil "\"~s\"" term))
    ((consp term) (str:concat
                   (format nil "[")
                   (apply #'str:concat  (loop :for x :in term :collect (pretty-term x)))
                   (format nil "]")))
    (t (format nil " ~s "term))))

(defun format-term (term)
  (cond
    ((consp term) (format nil "[~s]"
                          (apply #'str:concat  (loop :for x :in term :collect (pretty-term x)))))
    (t (format nil "~a" term))))
