(defpackage #:spacelang.term
  (:use #:cl)
  (:import-from #:alexandria #:copy-hash-table)
  (:import-from #:str #:repeat)
  (:export
   #:pretty-term
   #:format-term))
(in-package #:spacelang.term)

#|
Terms  = Term Space Term*

Space = [ \n\t;]+
      | Comment


Comment = #| .* |#


Term = Word
     | LocatedWord
     | ThunkOpen
     | ThunkClose
     | Instruction
     | ReverseThunkOpen
     | ReverseThunkClose
     | DictionaryUp
     | DictionaryDown


Located Word means this word from this machine.

LocatedWord = "@" Word "." Word


Starts appendign to a thunk

ThunkOpen = "["


Closes the thunk

ThunkClose = "]"


Starts appending to a thunk, that gets reversed/flipped  upon closing.

ReverseThunkOpen = "[|"


Closes a reverse thunk.

ReverseThunkClose = "|]"



DictionaryUp = "("

DictionaryDown = ")"

Instruction = Send
            | Eval
            | Bind
            | Scoop ;; like cons at a location


Send = "$"
     | "send"

Bind = "@"
     | "bind"

Eval = "!"
     | "eval"

Word = Number
     | Symbol
     | Keyword
     | Text
     | Character
     | Boolean

Number = Float
       | Integer

Symbol = [a-zA-Z*-+_/=]


|#
(defclass s-terms ()
  ((s-terms
    :initarg :terms
    :initform '()
    :accessor terms)))

(defclass s-term ()
  ())

(defclass s-word (s-term)
  ((s-word
    :initarg :s-word
    :initform (error "must be defined")
    :accessor s-word)))

(defclass s-number (term)
  ((s-number
    :initarg :s-number
    :initform (error "must be defined")
    :accessor s-number)))

(defclass s-keyword)

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
