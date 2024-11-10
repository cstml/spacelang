(defpackage #:spacelang.compiler
  (:use #:cl
        #:spacelang.parser
        #:spacelang.memory
        #:spacelang.term
        #:spacelang.evaluator
        #:spacelang.universe
        #:arrows)
  (:local-nicknames (:a :alexandria))
  (:shadowing-import-from :alexandria "EMPTYP")
  (:import-from :alexandria #:read-file-into-string)
  (:export #:compiler))
(in-package #:spacelang.compiler)

(defvar *terms* nil
  "Terms used by the compiled binary to run")

(defun ->compile-file (file-path)
  "Reads a file, and bakes it into the binary."
  (->> (read-file-into-string file-path)
       (parse-terms)
       (setf *terms*)))

(defun get-args ()
  (let* ((args (uiop:command-line-arguments))
         (dest (cadr (member "-dest" args :test #'equal)))
         (file-path (cadr (member "-c" args :test #'equal))))
    (cond
      ((emptyp dest)
       (progn (format t "Destination required -dest")
              (error "Bad")))
      ((emptyp file-path)
       (progn
         (format t "Compile required -c")
         (error "Bad 2")))
      (t (list dest file-path)))))

(defun run-binary ()
  (let ((home-memory (get-memory :home *universe*)))
    (loop :for term :in *terms*
          :do (evaluate home-memory term))))

(defun bake-binary (destination file-path)
  (->compile-file file-path)
  (sb-ext:save-lisp-and-die destination
                            :toplevel #'run-binary
                            :executable t))

(defun compile-this ()
  (let ((args (get-args)))
    (apply #'bake-binary args)))

(defun compiler ()
  (sb-ext:save-lisp-and-die
   #p"/home/cstml/.quicklisp/local-projects/spacelang/bin/compiler"
   :toplevel #'compile-this
   :executable t))
