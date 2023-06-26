(in-package :spacelang)

(defun .comment ()
  (.let*
      ((_ (.string-equal "{"))
       (c (.map 'string (.is #'cl:characterp)))
       (_ (.string-equal "}")))
    (.identity c)))

(defun .whitespace ()
  (.map 'string
        (.or
         (.char= #\	)
         (.char= #\)
         (.char= #\ ))))

(defun .read (f)
  (.prog2 (.optional (.whitespace))
          f
          (.optional (.whitespace))))

(defun .number ()
  (.let* ((n (.map 'string (.is #'cl:digit-char-p))))
    (.identity (read-from-string n))))

(defun .word ()
  (labels ((.chars () (.or (.is #'cl:upper-case-p)
                           (.is #'cl:lower-case-p))))
    (.let* ((n (.map 'string (.chars))))
      (.identity `,(read-from-string n)))))

(defun .opp ()
  (.let* ((o (.map 'string
                   (.or (.char= #\+)
                        (.char= #\-)
                        (.char= #\<)
                        (.char= #\>)
                        (.char= #\*)
                        (.char= #\/)))))
    (.identity (read-from-string (str:concat ":" o)))))

(defun .bind-term ()
  (.let* ((_ (.map 'string (.char= #\^))))
    (.identity :bind-term)))

(defun .eval-term ()
  (.let* ((_ (.map 'string (.char= #\!))))
    (.identity :eval-term)))

(defun .cons ()
  (.let* ((_ (.char= #\:))
          (_ (.char= #\:)))
    (.identity :cons)))

(defun .dict-up ()
  (.let* ((_ (.char= #\()))
    (.identity :dictionary-up)))

(defun .dict-down ()
  (.let* ((_ (.char= #\))))
    (.identity :dictionary-down)))

(defun .reader-delay ()
  (.let* ((_ (.char= #\[)))
    (.identity :reader-delay)))

(defun .reader-un-delay ()
  (.let* ((_ (.char= #\])))
    (.identity :reader-un-delay)))

(defun .keyword ()
  (.let* ((n (.and
              (.char= #\:)
              (.map 'string (.or (.is #'cl:upper-case-p)
                                 (.is #'cl:lower-case-p))))))
    (.identity (read-from-string (str:concat ":" n)))))

(defun .string ()
  (.let* ((_ (.read (.char= #\")))
          (ts (.map 'string (.is #'cl:characterp)))
          (_ (.read (.char= #\"))))
    (.identity ts)))

(defun .print ()
  (.let* ((_ (.char= #\.)))
    (.identity 'print)))

(defun .unknown ()
  (.let* ((nxt (.item)))
    (error (format nil "Don't know how to parse: \"~a\"." nxt))))

(defun .term ()
  (.or
   (.read (.word))
   (.read (.keyword))
   (.read (.number))
   (.read (.opp))
   (.read (.dict-up))
   (.read (.dict-down))
   (.read (.cons))
   (.read (.bind-term))
   (.read (.eval-term))
   (.read (.string))
   (.read (.reader-delay))
   (.read (.reader-un-delay))
   (.read (.print))))

(defun .terms ()
  (.map 'list (.term)))

(defun parse-terms (str)
  (handler-case
      (smug:parse (.or (.terms) (.unknown)) str)
    (error (err)
      (format t "Parser Error! ~%")
      (format t "~A ~%" err))))
