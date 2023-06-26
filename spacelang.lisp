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

(defmethod evaluate ((term (eql :bind-term)))
  (let ((binding (pop!))
               (term (pop!)))
           (if (and (= 1 (length binding))
                    (eql 'symbol (type-of (car binding))))
               (set-word! (car binding) term)
               (error "Cannot use term \" ~a \" as a binder." binding))))

(defmethod evaluate ((term symbol))
  (labels
      ((f2 (f)
         (let ((t1 (pop!))
               (t2 (pop!)))
           (push! (funcall f t1 t2))))

       (f1 (f)
         (let ((t1 (pop!)))
           (funcall f t1)))

       (bind-term ()
         )

       (space-if ()
         (let ((t1 (pop!))
               (t2 (pop!))
               (t3 (pop!)))
           (if (space-nilp t1)
               (push! t3)
               (push! t2))))

       (read-terms ()
           (let ((terms (parse-terms (read-line))))
             (mapcar #'evaluate terms)))

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
      (<  (f2 #'<))
      ;; eval
      (!  (f1 #'1-EVALUATE))
      (eval-term (evaluate '!))
      ;; dictionary
      (du (push-dictionary!))
      (dd (pop-dictionary!))
      ;; binding
      (bind-term (evaluate '^))
      (^  (bind-term))
      ;; io
      (slurp (read-terms))
      (print (print-last-term))
      ;; if
      (if (space-if))
      ;; cons
      (cons (cons-terms))
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

(defmethod evaluate ((term (eql :bye)))
  (sb-ext:exit))

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
:bye - to exit the program at any time.
"))

(defun handle-err (err)
  (case (type-of err)
    ('SB-PCL::NO-APPLICABLE-METHOD-ERROR
     (format t "Sorry, don't know what to do with that!~%"))))

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
           (print term)
           (print remaining)
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
  (format t "~% > "))

(defun run-reader! (slurp-fn remaining)
  (labels
      ((eval-fn (slurp-fn remaining)
         (multiple-value-bind (term remaining)
             (parse (.term) remaining)
           (progn
             (print  `(:term ,term))
             (cond
               ((eql :Reader-delay term) (multiple-value-bind (term remaining)
                                             (collect-terms slurp-fn remaining '())
                                           (progn
                                             (evaluate term)
                                             (continue-eval slurp-fn remaining))))
               ((eql :reader-un-delay term)  (error "Cannot undelay further."))
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
  (handler-case
      (run-reader! #'read-line "")
    (error (err)
      (handle-err err)
      (format t "Error Type: ~s.~%" (type-of err))
      (format t "Error: ~A.~%" err)
      (reset-stack)
      (format t "Stack reset.~%")
      (space!))))
