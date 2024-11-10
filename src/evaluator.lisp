(defpackage #:spacelang.evaluator
  (:use #:cl
        #:spacelang.memory
        #:spacelang.universe
        #:spacelang.parser
        #:spacelang.term)
  (:import-from #:spacelang.memory)
  (:import-from #:str #:repeat #:emptyp)
  (:export #:*silent-rebind-mode*
           #:*trace-mode*
           #:*debug-mode*
           #:*REPL-MODE*
           #:evaluate))
(in-package #:spacelang.evaluator)

(defparameter *trace-mode* nil)
(defvar *debug-mode* nil)
(defvar *REPL-MODE* nil)

(defun space-nilp (term)
  (case term
    ('() t)
    (0 t)
    (nil t)
    ;; everything else is a truthy value
    (t nil)))

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
  (push! memory term))

(defmethod evaluate ((memory space-memory) (term cons))
  (push! memory term))

(defmethod evaluate ((memory space-memory) (term string))
  (push! memory term))

(defmethod evaluate ((memory space-memory) (term (eql :+)))
  (f2 memory #'+))

(defmethod evaluate ((memory space-memory) (term (eql :*)))
  (f2 memory #'*))

(defmethod evaluate ((memory space-memory) (term (eql :-)))
  (f2 memory #'-))

(defmethod evaluate ((memory space-memory) (term (eql :/)))
  (f2 memory #'/))

(defmethod evaluate ((memory space-memory) (term (eql :<)))
  (f2p memory #'<))

(defmethod evaluate ((memory space-memory) (term (eql :>)))
  (f2p memory #'>))

(defmethod evaluate ((memory space-memory) (term (eql :<=)))
  (f2p memory #'<=))

(defmethod evaluate ((memory space-memory) (term (eql :>=)))
  (f2p memory #'>=))

(defmethod evaluate ((memory space-memory) (term (eql :=)))
  (f2p memory #'eql))

(defmethod evaluate ((memory space-memory) (term (eql :cons)))
  (let ((binding (pop! memory))
        (term (pop! memory)))
    (if (and (= 1 (length binding))
             (eql 'symbol (type-of (car binding))))
        (set-word! memory
                   (car binding)
                   (cons term (get-word!? memory (car binding)))))))

(defun binding? (term)
  (if (and (= 1 (length term))
           (eql 'symbol (type-of (car term))))
      (car term)
      nil))

(defmethod evaluate ((memory space-memory) (term (eql :bind-term)))
  (let ((binding (binding? (pop! memory)))
        (term (pop! memory)))
    (if binding
        (set-word! memory binding term)
        (error "Cannot use term \" ~a \" as a binder." binding))))

(defun get-name (term)
  (and
   (= 1 (length term))
   (car term)))

(defmethod evaluate ((memory space-memory)
                     (term (eql :send)))
  (let* ((machine-name (get-name (pop! memory)))
         (term (pop! memory))
         (other-memory (get-memory machine-name *universe*)))
    (evaluate other-memory term)))

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
