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
    :accessor dictionary-level)

   (name
    :initarg :name
    :initform (error "Must set memory name!")
    :accessor name)

   (inbox
    :initarg :inbox
    :initform '()
    :accessor inbox)

   (parent-universe
    :initarg :parent-universe
    :initform (error "Must set universe.")
    :accessor parent-universe)))

(defvar *universe-lock* (bt:make-lock))

(defparameter *trace-mode* nil)

(defparameter *silent-rebind-mode* t)

(defparameter *silent-mode* t)

(defvar *debug-mode* nil)

(defvar *REPL-MODE* nil)

(defun reset-memory (memory)
  (setf memory (make-instance 'space-memory
                              :name (name memory)
                              :parent-universe (parent-universe memory))))

(defun reset-stack (memory)
  (setf (stack memory) '()))

(defun reset-inbox (memory)
  (setf (inbox memory) '()))

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

;; Printing
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
              (format t "~A ~~ ~A ~%" binder (pretty-term term)))))

(defun print-name! (memory)
  "Prints the current memory name."
  (format t "memory: ~a. ~%" (name memory)))

(defun print-message! (message)
  (format t "(@~a) ~a ~%" (sender message) (pretty-term (contents message))))

(defun print-inbox! (memory)
  "Prints the current memory name."
  (format t "inbox: ~%" )
  (loop :for msg :in (inbox memory)
        :do (print-message! msg)))

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
                (print-inbox! memory)
                (print-sep!)))))

(defun print-memory! (memory)
  "Prints the current memory."
  (labels ((print-sep! () (format t "~A~%" (repeat 80 "=")))
           (newline! () (format t "~%")))
    (newline!)
    (print-sep!)
    (print-stack! memory)
    (print-dictionary! memory)
    (print-sep!)))

;; Stack
(defun get-stack! (memory)
  (bt:with-lock-held (*universe-lock*)
    (stack memory)))

(defun set-stack! (memory s)
  (bt:with-lock-held (*universe-lock*)
    (setf (stack memory) s)))

(defun push! (memory term)
  (bt:with-lock-held (*universe-lock*)
    (push term (stack memory))))

(defun pop! (memory)
  (bt:with-lock-held (*universe-lock*)
    (if (/= 0  (length (stack memory)))
        (pop (stack memory))
        (error "Stack underflow!"))))

;; Dictionary
(defun get-word! (memory s-word)
  (bt:with-lock-held (*universe-lock*)
    (multiple-value-bind (result exists) (gethash s-word (dictionary memory))
      (if exists
          result
          (error "Unbound Word: \"~s\" " s-word)))))

;; Dictionary
(defun get-word!? (memory s-word)
  (bt:with-lock-held (*universe-lock*)
    (gethash s-word (dictionary memory))))

(defun set-word! (memory s-word term)
  (bt:with-lock-held (*universe-lock*)
    (multiple-value-bind (_ exists) (gethash s-word (dictionary memory))
      (if exists
          (progn
            (when (not *silent-rebind-mode*)
              (format t "Rebinding word \"~s\".~%" s-word))
            (setf (gethash s-word (dictionary memory)) term))
          (setf (gethash s-word (dictionary memory)) term)))))

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

(defgeneric 1-evaluate (memory term)
  (:documentation "Evaluate a spacelang term."))

(defmethod 1-evaluate (memory (term cons))
  (mapcar (lambda (sub-term) (evaluate memory sub-term)) term))

(defmethod 1-evaluate (memory term)
  (evaluate memory term))

(defgeneric evaluate (memory term)
  (:documentation "Evaluate a spacelang term."))

(defmethod evaluate :before (memory term)
  (when *trace-mode* (format t "~~ ~s ~%" term)))

(defmethod evaluate :after (memory term)
  (when *trace-mode* (format t "~~ ~s ~%" term)))

(defmethod evaluate ((memory space-memory) (term number))
  "Pushes a term to the stack."
  (push! memory term))

(defmethod evaluate ((memory space-memory) (term cons))
  "Pushes a thunk to the stack."
  (push! memory term))

(defmethod evaluate ((memory space-memory) (term string))
  "Pushes the string to the stack."
  (push! memory term))

(defun f2 (memory f)
  "Pops 2 terms from the stack, applies a 2 arrity function, evaluates the
resulting term."
  (let ((t1 (pop! memory))
        (t2 (pop! memory)))
    (evaluate memory (funcall f t1 t2))))

(defun f2p (memory f)
  "Pops 2 terms from the stack, applies a 2 arrity function, evaluates to t if
true 0 if false."
  (let ((t1 (pop! memory))
        (t2 (pop! memory)))
    (if (funcall f t1 t2)
        (evaluate memory t)
        (evaluate memory 0))))

(defmethod evaluate ((memory space-memory) (term (eql :+)))
  "Pops two terms, sums them, evaluates the result."
  (f2 memory #'+))

(defmethod evaluate ((memory space-memory) (term (eql :*)))
  "Pops two terms, multiplies them, evaluates the result."
  (f2 memory #'*))

(defmethod evaluate ((memory space-memory) (term (eql :-)))
  "Pops two terms, subtracts them, evaluates the result."
  (f2 memory #'-))

(defmethod evaluate ((memory space-memory)
                     (term (eql :/)))
  (f2 memory #'/))

(defmethod evaluate ((memory space-memory)
                     (term (eql :<)))
  (f2p memory #'<))

(defmethod evaluate ((memory space-memory)
                     (term (eql :>)))
  (f2p memory #'>))

(defmethod evaluate ((memory space-memory)
                     (term (eql :<=)))
  (f2p memory #'<=))

(defmethod evaluate ((memory space-memory)
                     (term (eql :>=)))
  (f2p memory #'>=))

(defmethod evaluate ((memory space-memory)
                     (term (eql :=)))
  (f2p memory #'eql))

(defmethod evaluate ((memory space-memory)
                     (term (eql :cons)))
  (let ((binding (pop! memory))
        (term (pop! memory)))
    (if (and (= 1 (length binding))
             (eql 'symbol (type-of (car binding))))
        (set-word! memory
                   (car binding)
                   (cons term (get-word!? memory (car binding)))))))

(defmethod evaluate ((memory space-memory)
                     (term (eql :bind-term)))
  (let ((binding (pop! memory))
        (term (pop! memory)))
    (if (and (= 1 (length binding))
             (eql 'symbol (type-of (car binding))))
        (set-word! memory (car binding) term)
        (error "Cannot use term \" ~a \" as a binder." binding))))

(defmethod evaluate ((memory space-memory)
                     (term (eql :send)))
  (let ((machine-name (pop! memory))
        (term (pop! memory)))
    (labels ((send-1-term (term)
               (send-to-inbox (name memory) (car machine-name) term (parent-universe memory))))
      (if (and (= 1 (length machine-name))
               (eql 'symbol (type-of (car machine-name))))
          (cond
            ;; If the term is a thunk ~ evaluate it.
            ((consp term) (mapcar (lambda (term-1) (send-1-term term-1)) term))
            ;; if the term is a value ~ send it.
            (t (send-1-term term)))
          (error "Cannot use term \" ~a \" as a machine name." machine-name)))))

(defmethod evaluate ((memory space-memory)
                     (term (eql :if)))
  (let ((t1 (pop! memory))
        (t2 (pop! memory))
        (t3 (pop! memory)))
    (if (space-nilp t1)
        (push! memory t3)
        (push! memory t2))))

(defun f1 (memory f)
  "Pop 1 term, apply function to it."
  (let ((t1 (pop! memory)))
    (funcall f t1)))

(defmethod evaluate ((memory space-memory)
                     (term (eql :eval-term)))
  (f1 memory (lambda (term) (1-EVALUATE memory term))))

(defun describe-term (memory binding)
  (labels ((sep! () (format t "~a~%" (repeat 80 "="))))
    (sep!)
    (format t "Describing:~%")
    (format t "~a ~~ ~a ~%" (car binding) (pretty-term (get-word! memory (car binding))))
    (sep!)))

(defmethod evaluate ((memory space-memory)
                     (term (eql :describe)))
  (let ((binding (pop! memory)))
    (if (and (= 1 (length binding))
             (eql 'symbol (type-of (car binding))))
        (describe-term memory binding)
        (error "Cannot use term \" ~a \" as a binder." (pretty-term binding)))))

(defmethod evaluate ((memory space-memory)
                     (term symbol))
  (labels
      ((cons-terms ()
         (let ((binding (pop! memory))
               (term (pop! memory)))
           (if (and (= 1 (length binding))
                    (eql 'symbol (type-of (car binding))))
               (set-word! memory (car binding) (cons term (get-word!? memory (car binding)))))))

       (print-last-term ()
         (format t "~A~%" (pretty-term (pop! memory)))))

    (case term
      ;; math opps
      (-  (f2 memory #'-))
      (+  (f2 memory #'+))
      (*  (f2 memory #'*))
      (/  (f2 memory #'/))
      (>  (f2 memory #'>))
      (<  (f2 memory #'<))
      ;; eval

      (eval-term (evaluate memory '!))
      ;; dictionary
      (du (push-dictionary! memory))
      (dd (pop-dictionary! memory))
      ;; binding
      (print (print-last-term))
      ;; cons
      (cons (cons-terms))
      ;; t
      ('nil (push! memory 0))
      ('t (push! memory t))

      (otherwise (push! memory (get-word! memory term))))))


(defmethod evaluate ((memory space-memory)
                     (term (eql :print)))
  (let ((last-term (pop! memory)))
    (format t "~a~%" (pretty-term last-term))))

(defmethod evaluate ((memory space-memory)
                     (term (eql :format)))
  (let ((last-term (pop! memory)))
    (format t "~a~%" (format-term last-term))))

(defmethod evaluate ((memory space-memory)
                     (term (eql :slurp)))
  (let ((terms (parse-terms (read-line))))
    (mapcar (lambda (term) (evaluate memory term)) terms)))

(defmethod evaluate ((memory space-memory)
                     (_term (eql :dictionary-up)))
  (push-dictionary! memory))

(defmethod evaluate ((memory space-memory)
                     (_term (eql :dictionary-down)))
  (pop-dictionary! memory))

(defmethod evaluate ((memory space-memory)
                     (term (eql :r)))
  (reset-memory memory)
  (when (not *silent-mode*) (format t "Memory reset.~%")))

(defmethod evaluate ((memory space-memory)
                     (term (eql :rs)))
  (reset-stack memory)
  (when (not *silent-mode*) (format t "Stack reset.~%")))

(defmethod evaluate ((memory space-memory)
                     (term (eql :m)))
  (print-memory! memory))

(defmethod evaluate ((memory space-memory)
                     (term (eql :s)))
  (print-stack! memory))

(defmethod evaluate ((memory space-memory)
                     (term (eql :d)))
  (print-dictionary! memory))

(defmethod evaluate ((_memory space-memory)
                     (_term (eql :debug)))
  (if *debug-mode* (setf *debug-mode* nil)
      (setf *debug-mode* t)))

(defmethod evaluate ((_memory space-memory)
                     (term (eql :bye)))
  (sb-ext:exit))

(defmethod evaluate ((_memory space-memory)
                     (term (eql :noop)))
  "Do nothing."
  nil)

(defmethod evaluate ((memory space-memory)
                     (term (eql nil)))
  "Do nothing."
  nil)

(defmethod evaluate ((memory space-memory)
                     (term (eql :load)))
  "Read a file, and evaluate it."
  (let ((location (pop! memory)))
    (run-reader! memory
                 #'read-line
                 (read-file-into-string location))))

(defmethod evaluate ((memory space-memory)
                     (_term (eql :h)))
  "Returns the help string."
  (evaluate memory :help))

(defmethod evaluate ((_memory space-memory)
                     (_term (eql :help)))
  "Returns the help string."
  (format t "
:help - for help.
:r - to reset memory.
:load - to load a file memory.
:rs - to reset stack.
:m - to print the memory.
:s - to print the stack.
:d - to print the dictionary.
:debug - to switch debug mode on/off ~~ print memory after each read.
:bye - to exit the program at any time.~%"))

(defun handle-err (err)
  (case (type-of err)
    (SB-PCL::NO-APPLICABLE-METHOD-ERROR
     (format t "Sorry, don't know what to do with that!~%"))
    (END-OF-FILE (evaluate nil :bye))))

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

(defun prompt (memory)
  (when *DEBUG-MODE* (print-universe!))
  (when *REPL-MODE* (format t "~% ~a > " (length (stack memory))))
  (finish-output))

(defun run-reader! (memory slurp-fn remaining)
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
                    (evaluate memory term)
                    (continue-eval slurp-fn remaining))))

               ((eql :reader-un-delay term)
                (error "Cannot undelay further."))

               (t (progn
                    (evaluate memory term)
                    (continue-eval slurp-fn remaining)))))))

       (continue-eval (slurp-fn remaining)
         (if (emptyp remaining)
             (progn
               (prompt memory)
               (eval-fn slurp-fn (funcall slurp-fn)))
             (eval-fn slurp-fn remaining))))

    (continue-eval slurp-fn remaining)))

(defun collect-terms-now (pop-fn)
  (let ((term (funcall pop-fn)))
   (cond
     ((eql :reader-un-delay term)
      '())

     ((eql :reader-delay term)
      (let ((term-acc (collect-terms-now pop-fn)))
        (concat term-acc (collect-terms-now pop-fn))))

     (t
      (concat term (collect-terms-now pop-fn))))))

(defun run-reader-term! (memory pop-fn)
  (labels
      ((eval-fn (pop-fn)
         (let ((term (funcall pop-fn)))
           (progn
             (cond
               ((eql :reader-delay term)
                (collect-terms-now pop-fn)
                (progn
                  (evaluate memory term)
                  (continue-eval pop-fn)))

               ((eql :reader-un-delay term)
                (error "Cannot undelay further."))

               (t (progn
                    (evaluate memory term)
                    (continue-eval pop-fn)))))))

       (continue-eval (pop-fn)
         (eval-fn pop-fn)))

    (continue-eval pop-fn)))

;; Universe related
(defclass universe ()
    ((space-instances
      :initarg :space-instances
      :initform (make-hash-table :test 'equalp)
      :accessor space-instances)))

(defclass message ()
  ((sender
    :initarg :sender
    :initform :not-set
    :accessor sender)
   (contents
    :initarg :contents
    :initform nil
    :accessor contents)))

(defun init-universe ()
  (let ((universe (make-instance 'universe)))
    (setf (gethash :home (space-instances universe))
          (make-instance 'space-memory
                         :name :home
                         :parent-universe universe))
    universe))

(defun add-machine (universe name machine)
  (setf (gethash name (space-instances universe)) machine))

(defun remove-machine (universe name)
  (setf (gethash name (space-instances universe)) nil))

(defun start-machine-thread! (universe machine-name)
  (let ((top-level *standard-output*)
        (memory (gethash machine-name (space-instances universe))))
    (bt:make-thread
     (lambda ()
       (handler-case
           (run-reader-term! memory (lambda () (pop-inbox memory)))
         (error (err)
           (handle-err err)
           (format top-level "In Machine: ~a.~%" machine-name)
           (format top-level "Error Type: ~s.~%" (type-of err))
           (format top-level "Error: ~A.~%" err)
           (reset-stack memory)
           (format top-level "Stack reset.~%")
           (reset-inbox memory)
           (format top-level "Inbox reset.~%")
           (start-machine-thread! universe machine-name)))))))

(defun initialise-machine (universe new-machine-name)
  (setf (gethash new-machine-name (space-instances universe))
        (make-instance 'space-memory :name new-machine-name :parent-universe universe))
  (start-machine-thread! universe new-machine-name))

(defparameter *universe* (init-universe))

(defun get-memory (memory-name universe)
  (gethash memory-name
           (space-instances universe)))

(defun send-to-inbox (from-name to-name message universe)
  "Sends message from to machine."
  (let ((msg (make-instance 'message :sender from-name :contents message)))
    (bt:with-lock-held (*universe-lock*)
      ;; The machine at the address hasn't been initialised/started.
      (when (not (get-memory to-name universe)) (initialise-machine universe to-name))
      (setf (inbox (gethash to-name (space-instances universe)))
            (concatenate 'list
                         (inbox (gethash to-name (space-instances universe)))
                         (list msg))))))

(defun pop-inbox (memory)
  "Pops a term from the inbox."
  (bt:with-lock-held (*universe-lock*)
    (let ((next-term (pop (inbox memory))))
      (if next-term
          (contents next-term)
          :noop))))

(defun space! ()
  "Starts the spacelang repl."
  (let ((args (uiop:command-line-arguments))
        (home-memory (get-memory :home *universe*)))
    (when args
      (progn (evaluate home-memory (first args))
             (evaluate home-memory :load)))
    (handler-case
        (run-reader! home-memory #'read-line "")
      (error (err)
        (handle-err err)
        (format t "Error Type: ~s.~%" (type-of err))
        (format t "Error: ~A.~%" err)
        (reset-stack home-memory)
        (format t "Stack reset.~%")
        (space!)))))
