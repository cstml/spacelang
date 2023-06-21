;;;; spacelang.lisp
(in-package #:spacelang)

;; Memory related
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
    :accessor dictionary-level)))

(defun reset-memory ()
  (setf *memory* (make-instance 'space-memory)))

(defun reset-stack ()
  (setf (stack *memory*) '()))

(defun push-dictionary! ()
  (format t "Pushing dictionary!~%")
  (push (dictionary *memory*) (dictionary-level *memory*))
  (setf (dictionary *memory*) (copy-hash-table (dictionary *memory*))))

(defun pop-dictionary! ()
  (format t "Popping dictionary!~%")
  (let ((old-dictionary (pop (dictionary-level *memory*))))
    (if old-dictionary
        (setf (dictionary *memory*) old-dictionary)
        (progn (format t "Last dictionary popped!~%")
               (format t "Resetting dictionary!~%")
               (setf (dictionary *memory*) (make-hash-table :test 'equalp))))))

(defparameter *memory*
  (make-instance 'space-memory))

(defvar *debug-mode* nil)


;; Printing
(defun print-stack! ()
  "Prints the current stack."
  (format t "stack> ~s ~%" (reverse (stack *memory*))))

(defun print-dictionary! ()
  "Prints the current dictionary."
  (format t "dicty> ~A ~%" (hash-table-plist (dictionary *memory*))))

(defun print-memory! ()
  "Prints the current memory."
  (print-stack!)
  (print-dictionary!))

;; Stack
(defun get-stack! ()
  (stack *memory*))

(defun set-stack! (s)
  (setf (stack *memory*) s))

(defun push! (term)
  (push term (stack *memory*)))

(defun pop! ()
  (let ((term (pop (stack *memory*))))
    (if term term
        (error "Stack underflow!"))))

;; Dictionary
(defun get-word! (s-word)
  (multiple-value-bind (result exists) (gethash s-word (dictionary *memory*))
    (if exists
        result
        (error "Unbound Word: \"~s\" " s-word))))

;; Dictionary
(defun get-word!? (s-word)
  (gethash s-word (dictionary *memory*)))

(defun set-word! (s-word term)
  (multiple-value-bind (_ exists) (gethash s-word (dictionary *memory*))
    (if exists
        (progn
          (format t "Rebinding word \"~s\".~%" s-word)
          (setf (gethash s-word (dictionary *memory*)) term))
        (setf (gethash s-word (dictionary *memory*)) term))))

;; Evaluation
(defun space-nilp (term)
  (case term
    ('() t)
    (0 t)
    (t nil)))

(defgeneric 1-evaluate (term)
  (:documentation "Evaluate a spacelang term."))

(defmethod 1-evaluate ((term cons))
  (mapcar #'evaluate term))

(defmethod 1-evaluate (term)
  (evaluate term))

(defgeneric evaluate (term)
  (:documentation "Evaluate a spacelang term."))

(defmethod evaluate ((term number))
  (push! term))

(defmethod evaluate ((term cons))
  (push! term))

(defmethod evaluate ((term string))
  (push! term))

(defmethod evaluate ((term symbol))
  (labels ((f2 (f) (let ((t1 (pop!))
                         (t2 (pop!)))
                     (push! (funcall f t1 t2))))

           (f1 (f) (let ((t1 (pop!)))
                     (funcall f t1)))

           (space-if () (let ((t1 (pop!))
                              (t2 (pop!))
                              (t3 (pop!)))
                          (if (space-nilp t1)
                              (push! t3)
                              (push! t2)))))
    (case term
      (-  (f2 #'-))
      (+  (f2 #'+))
      (*  (f2 #'*))
      (/  (f2 #'/))
      (!  (f1 #'1-EVALUATE))
      (du (push-dictionary!))
      (dd (pop-dictionary!))
      (bind-term (evaluate '^))
      (eval-term (evaluate '!))
      (^  (let ((binding (pop!))
                (term (pop!)))
            (if (and (= 1 (length binding))
                     (eql 'symbol (type-of (car binding))))
                (set-word! (car binding) term)
                (error "Cannot use term \" ~a \" as a binder." binding))))
      (if (space-if))
      (cons (let ((binding (pop!))
                  (term (pop!)))
              (if (and (= 1 (length binding))
                       (eql 'symbol (type-of (car binding))))
                  (set-word! (car binding) (cons term (get-word!? (car binding)))))))
      (t  (evaluate (get-word! term))))))

(defmethod evaluate ((term (eql :r)))
  (reset-memory)
  (format t "Memory reset.~%"))

(defmethod evaluate ((term (eql :rs)))
  (reset-memory)
  (format t "Memory reset.~%"))

(defmethod evaluate ((term (eql :m)))
  (print-memory!))

(defmethod evaluate ((term (eql :s)))
  (print-stack!))

(defmethod evaluate ((term (eql :d)))
  (print-dictionary!))

(defmethod evaluate ((term (eql :debug)))
  (if *debug-mode* (setf *debug-mode* nil)
      (setf *debug-mode* t)))

(defmethod evaluate ((term (eql :load)))
  (let ((location (pop!)))
    (read-file-into-string location)))

(defmethod evaluate ((term (eql :help)))
  (format t "
:help - for help.
:r - to reset memory.
:load - to load a file memory.
:rs - to reset stack.
:m - to print the memory.
:s - to print the stack.
:d - to print the dictionary.
:debug - to switch debug mode on/off ~~ print memory after each read.
"))

(defun handle-err (err)
  (case (type-of err)
    ('SB-PCL::NO-APPLICABLE-METHOD-ERROR (format t "Sorry, don't know what to do with that!~%"))))

(defun e (&rest terms)
  "Main evaluating function."
  (handler-case
      (mapcar #'evaluate terms)
    (error (err)
      (handle-err err)
      (format t "Error Type: ~s.~%" (type-of err))
      (format t "Error: ~A.~%" err)
      (reset-stack)
      (format t "Stack reset.~%"))))

(defun .whitespace ()
  (.map 'string (.char= #\ )))

(defun .read (f)
  (.prog2 (.optional (.whitespace))
          f
          (.optional (.whitespace))))

(defun .number ()
  (.let* ((n (.map 'string (.is #'cl:digit-char-p))))
    (.identity (read-from-string n))))

(defun .word ()
  (.let* ((n (.map 'string (.or (.is #'cl:upper-case-p)
                                (.is #'cl:lower-case-p)))))
    (.identity `,(read-from-string n))))

(defun .opp ()
  (.let* ((o (.map 'string
                   (.or (.char= #\+)
                        (.char= #\-)
                        (.char= #\*)
                        (.char= #\/)))))
    (.identity (read-from-string o))))

(defun .bind-term ()
  (.let* ((_ (.map 'string (.char= #\^))))
    (.identity 'bind-term)))

(defun .eval-term ()
  (.let* ((_ (.map 'string (.char= #\!))))
    (.identity 'eval-term)))

(defun .cons ()
  (.let* ((_ (.char= #\:))
          (_ (.char= #\:)))
    (.identity 'cons)))

(defun .dict-up ()
  (.let* ((_ (.char= #\()))
    (.identity 'du)))

(defun .dict-down ()
  (.let* ((_ (.char= #\))))
    (.identity 'dd)))

(defun .vector ()
  (.let* ((_ (.read (.char= #\[ )))
          (ts (.first (.map 'list (.term))))
          (_ (.read (.char= #\]))))
    (.identity ts)))

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

(defun .unknown ()
  (.let* ((nxt (.item)))
    (error (format nil "Don't know how to parse: \"~a\"." nxt))))

(defun .term ()
  (.or
   (.read (.word))
   (.read (.keyword))
   (.read (.number))
   (.read (.opp))
   (.read (.vector))
   (.read (.dict-up))
   (.read (.dict-down))
   (.read (.cons))
   (.read (.bind-term))
   (.read (.eval-term))
   (.read (.string))))

(defun .terms ()
  (.map 'list (.term)))

(defun parse-terms (str)
  (handler-case
      (smug:parse (.or (.terms) (.unknown)) str)
    (error (err)
      (format t "Parser Error! ~%")
      (format t "~A ~%" err))))

(defun space! ()
  (format t "~% > ")
  (let* ((str (read-line))
         (read-terms (parse-terms str)))
    (apply #'e read-terms)
    (when *DEBUG-MODE* (print-memory!))
    (space!)))
