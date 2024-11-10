(defpackage #:spacelang
  (:use #:cl
        #:spacelang.parser
        #:spacelang.memory
        #:spacelang.term
        #:spacelang.evaluator
        #:spacelang.universe)
  (:local-nicknames (:a :alexandria))
  (:shadowing-import-from :alexandria "EMPTYP")
  (:import-from :alexandria #:read-file-into-string)
  (:export #:space!
           ;; Globals
           #:*universe*
           #:*trace-mode*))
(in-package #:spacelang)

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
        (multiple-value-bind (term remaining)
            (spacelang.parser::parse (spacelang.parser::.term) (funcall slurp-fn))
          (proceed term remaining acc))
        (multiple-value-bind (term remaining)
            (spacelang.parser::parse (spacelang.parser::.term) remaining)
          (proceed term remaining acc)))))

(defun prompt (memory)
  (when *DEBUG-MODE* (print-universe!))
  (when *REPL-MODE* (format t "~% ~a > " (length (stack memory))))
  (finish-output))

(defun run-reader! (memory slurp-fn remaining)
  (labels
      ((eval-fn (slurp-fn remaining)
         (multiple-value-bind (term remaining)
             (spacelang.parser::parse (spacelang.parser::.term) remaining)
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
        (concatenate 'list term-acc (collect-terms-now pop-fn))))

     (t
      (concatenate 'list term (collect-terms-now pop-fn))))))

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

(defmethod evaluate ((memory space-memory)
                     (term (eql :load)))
  "Read a file, and evaluate it."
  (let ((location (pop! memory)))
    (run-reader! memory
                 #'read-line
                 (read-file-into-string location))))
(defun space! ()
  "Starts the spacelang repl."
  (let ((args (uiop:command-line-arguments))
        (home-memory (get-memory :home *universe*)))
    (setf lparallel:*kernel* (lparallel:make-kernel 10))
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
