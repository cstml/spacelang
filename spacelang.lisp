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

(defparameter *trace-mode* nil)

(defparameter *silent-rebind-mode* t)

(defvar *debug-mode* nil)

(defvar *REPL-MODE* nil)
;; Printing
(defun pretty-term (term)
  (cond
    ((consp term)
     (str:concat
      (format nil "[")
      (apply #'str:concat  (loop :for x :in term :collect (pretty-term x)))
      (format nil "]")))

    (t
     (format nil " ~a "term))))

(defun print-stack! ()
  "Prints the current stack."
  (format t "stack: ~%")
  (loop :for term :in (reverse (stack *memory*))
        :do (format t "~a" (pretty-term term)))
  (format t "~%~%"))

(defun print-dictionary! ()
  "Prints the current dictionary."
  (format t "dicty: ~%")
  (loop :for x :in (hash-table-alist (dictionary *memory*))
        :do (let ((binder (car x))
                  (term (cdr x)))
              (format t "~A ~~ ~A ~%" binder (pretty-term term)))))

(defun print-memory! ()
  "Prints the current memory."
  (labels ((print-sep! () (format t "~A~%" (repeat 80 "=")))
           (newline! () (format t "~%")))
    (newline!)
    (print-sep!)
    (print-stack!)
    (print-dictionary!)
    (print-sep!)))

;; Stack
(defun get-stack! ()
  (stack *memory*))

(defun set-stack! (s)
  (setf (stack *memory*) s))

(defun push! (term)
  (push term (stack *memory*)))

(defun pop! ()
  (if (/= 0  (length (stack *memory*)))
      (pop (stack *memory*))
      (error "Stack underflow!")))

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
          (when (not *silent-rebind-mode*)
            (format t "Rebinding word \"~s\".~%" s-word))
          (setf (gethash s-word (dictionary *memory*)) term))
        (setf (gethash s-word (dictionary *memory*)) term))))

;; Evaluation
(defun space-nilp (term)
  (case term
    ('() t)
    (0 t)
    (nil t)
    ;; everything else is a truthy value
    (t nil)))

(defun enter-collect-then (more-terms after-fn)
  (funcall after-fn (collect-then (funcall more-terms) more-terms)))

(defun collect-then (term more-terms)
  (case term
        (1 '(1))
        (t (cons term (collect-then (funcall more-terms) more-terms)))))

(defgeneric 1-evaluate (term)
  (:documentation "Evaluate a spacelang term."))

(defmethod 1-evaluate ((term cons))
  (mapcar #'evaluate term))

(defmethod 1-evaluate (term)
  (evaluate term))

(defgeneric evaluate (term)
  (:documentation "Evaluate a spacelang term."))

(defmethod evaluate :before (term)
  (when *trace-mode* (format t "~~ ~s ~%" term)))

(defmethod evaluate ((term number))
  (push! term))

(defmethod evaluate ((term cons))
  (push! term))

(defmethod evaluate ((term string))
  (push! term))

(defun f2 (f)
  (let ((t1 (pop!))
        (t2 (pop!)))
    (push! (funcall f t1 t2))))

(defmethod evaluate ((term (eql :+)))
  (f2 #'+))

(defmethod evaluate ((term (eql :*)))
  (f2 #'*))

(defmethod evaluate ((term (eql :-)))
  (f2 #'-))

(defmethod evaluate ((term (eql :/)))
  (f2 #'/))

(defmethod evaluate ((term (eql :/)))
  (f2 #'/))

(defmethod evaluate ((term (eql :<)))
  (f2 #'<))

(defmethod evaluate ((term (eql :>)))
  (f2 #'>))

(defmethod evaluate ((term (eql :<=)))
  (f2 #'<=))

(defmethod evaluate ((term (eql :>=)))
  (f2 #'>=))

(defmethod evaluate ((term (eql :=)))
  (f2 #'eql))

(defmethod evaluate ((term (eql :bind-term)))
  (let ((binding (pop!))
               (term (pop!)))
           (if (and (= 1 (length binding))
                    (eql 'symbol (type-of (car binding))))
               (set-word! (car binding) term)
               (error "Cannot use term \" ~a \" as a binder." binding))))

(defmethod evaluate ((term (eql :if)))
  (let ((t1 (pop!))
               (t2 (pop!))
               (t3 (pop!)))
           (if (space-nilp t1)
               (push! t3)
               (push! t2))))

(defun f1 (f)
  (let ((t1 (pop!)))
    (funcall f t1)))

(defmethod evaluate ((term (eql :eval-term)))
  (f1 #'1-EVALUATE))

(defun describe-term (binding)
  (labels ((sep! () (format t "~a~%" (repeat 80 "="))))
    (sep!)
    (format t "Describing:~%")
    (format t
            "~a ~~ ~a ~%"
            (car binding)
            (pretty-term (get-word! (car binding))))
    (sep!)))

(defmethod evaluate ((term (eql :describe)))
  (let ((binding (pop!)))
    (if (and (= 1 (length binding))
             (eql 'symbol (type-of (car binding))))
        (describe-term binding)
        (error "Cannot use term \" ~a \" as a binder." (pretty-term binding)))))

(defmethod evaluate ((term (eql :slurp)))
  (let ((terms (parse-terms (read-line))))
    (mapcar #'evaluate terms)))

(defmethod evaluate ((term symbol))
  (labels
      ((f2 (f)
         (let ((t1 (pop!))
               (t2 (pop!)))
           (push! (funcall f t1 t2))))

       (cons-terms ()
         (let ((binding (pop!))
               (term (pop!)))
           (if (and (= 1 (length binding))
                    (eql 'symbol (type-of (car binding))))
               (set-word! (car binding) (cons term (get-word!? (car binding)))))))

       (print-last-term ()
         (format t "~A~%" (pop!))))

    (case term
      ;; math opps
      (-  (f2 #'-))
      (+  (f2 #'+))
      (*  (f2 #'*))
      (/  (f2 #'/))
      (>  (f2 #'>))
      (<  (f2 #'<))
      ;; eval

      (eval-term (evaluate '!))
      ;; dictionary
      (du (push-dictionary!))
      (dd (pop-dictionary!))
      ;; binding
      ;; io
      (slurp (read-terms))
      (print (print-last-term))
      ;; cons
      (cons (cons-terms))
      (t  (evaluate (get-word! term))))))

(defmethod evaluate ((term (eql :r)))
  (reset-memory)
  (format t "Memory reset.~%"))

(defmethod evaluate ((term (eql :rs)))
  (reset-stack)
  (format t "Stack reset.~%"))

(defmethod evaluate ((term (eql :m)))
  (print-memory!))

(defmethod evaluate ((term (eql :s)))
  (print-stack!))

(defmethod evaluate ((term (eql :d)))
  (print-dictionary!))

(defmethod evaluate ((term (eql :debug)))
  (if *debug-mode* (setf *debug-mode* nil)
      (setf *debug-mode* t)))

(defmethod evaluate ((term (eql :bye)))
  (sb-ext:exit))

(defmethod evaluate ((term (eql :noop)))
  nil)

(defmethod evaluate ((term (eql :load)))
  (let ((location (pop!)))
    (run-reader! #'read-line
                 (read-file-into-string location))))

(defun read-file (location)
  (read-file-into-string location))

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
:bye - to exit the program at any time.
"))

(defun handle-err (err)
  (case (type-of err)
    ('SB-PCL::NO-APPLICABLE-METHOD-ERROR
     (format t "Sorry, don't know what to do with that!~%"))
    ('END-OF-FILE (evaluate :bye))))

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

(defun collect-terms (slurp-fn remaining acc)
  (labels
      ((proceed (term remaining acc)
         (progn
           (cond
             ((eql :READER-UN-DELAY term)
              (values (reverse  acc) remaining))

             ((eql :READER-DELAY term)
              (multiple-value-bind (term-2 remaining-2)
                  (collect-terms slurp-fn remaining '())
                (collect-terms slurp-fn remaining-2 (cons term-2 acc))))

             (t
              (collect-terms slurp-fn remaining (cons term acc)))))))
   (if (emptyp remaining)
       (multiple-value-bind (term remaining) (parse (.term) (funcall slurp-fn))
         (proceed term remaining acc))
       (multiple-value-bind (term remaining) (parse (.term) remaining)
         (proceed term remaining acc)))))

(defun prompt ()
  (when *DEBUG-MODE* (print-memory!))
  (when *REPL-MODE* (format t "~% ~a > " (length (stack *memory*))))
  (finish-output))

(defun run-reader! (slurp-fn remaining)
  (labels
      ((eval-fn (slurp-fn remaining)
         (multiple-value-bind
               (term remaining) (parse (.term) remaining)
           (progn
             (cond
               ((eql :reader-delay term)
                (multiple-value-bind (term remaining)
                    (collect-terms slurp-fn remaining '())
                  (progn
                    (evaluate term)
                    (continue-eval slurp-fn remaining))))

               ((eql :reader-un-delay term)
                (error "Cannot undelay further."))

               (t (progn
                    (evaluate term)
                    (continue-eval slurp-fn remaining)))))))

       (continue-eval (slurp-fn remaining)
         (if (emptyp remaining)
             (progn
               (prompt)
               (eval-fn slurp-fn (funcall slurp-fn)))
             (eval-fn slurp-fn remaining))))

    (continue-eval slurp-fn remaining)))

(defun space! ()
  (let ((args (uiop:command-line-arguments)))
    (when args
        (progn (evaluate (first args))
               (evaluate :load))))
  (handler-case
      (run-reader! #'read-line "")
    (error (err)
      (handle-err err)
      (format t "Error Type: ~s.~%" (type-of err))
      (format t "Error: ~A.~%" err)
      (reset-stack)
      (format t "Stack reset.~%")
      (space!))))
