;;;; spacelang.lisp

(in-package #:spacelang)

;; enable ^() synyax for lambdas
;; (^(+ _ _) 3 4)
(cl-punch:enable-punch-syntax)

(defclass space-memory ()
  ((stacks
    :initarg :stacks
    :initform (error ":stacks are required.")
    :accessor stacks)))

(defclass space-hash-table-memory (space-memory)
  ()
  (:default-initargs
   :stacks (make-hash-table :test 'equalp)))

(defun init-memory ()
  (make-instance 'space-hash-table-memory))

(defvar *counting-from* 0)

(defvar *memory* (init-memory))

(defparameter *evaluating-mode* '(:evaluating))

(defparameter thunk-mode '(:thunk 1))

(defvar *mode* evaluating-mode)

(defun reset-memory ()
  (setf *memory* (init-memory))
  (setf *mode* evaluating-mode))

(defun print-memory ()
  (pprint (hash-table-plist (stacks *memory*))))

(defgeneric get-stack (memory location)
  (:documentation "Reads the current memory at a location."))

(defmethod get-stack ((memory space-memory) location)
  (ensure-gethash location (stacks memory) '()))

(defgeneric set-stack (memory location stack)
  (:documentation "Sets the stack to a specific value."))

(defmethod set-stack ((memory space-memory) location stack)
  (setf (gethash location (stacks memory)) stack))

(defun get-location (location)
  (get-stack *memory* location))

(defun push-term (term location)
  (let ((current-stack (get-stack *memory* location)))
    (set-stack *memory* location (cons term current-stack))))

(defun pop-term (location)
  (let ((current-stack (get-stack *memory* location)))
    (set-stack *memory* location (cdr current-stack))
    (car current-stack)))

(defun peek-term (n l)
  (subseq (get-stack *memory* l) 0 n))

;; peek
;; term: location number peek

(defgeneric evaluate (term)
  (:documentation "Evaluate a spacelang term."))

(defmethod evaluate ((term number))
  (case (car *evaluating-mode*)
    (:evaluating (push-term term :default))
    (:thunk term)))

(defmethod evaluate ((term string))
  (push-term term :default))

(defmethod evaluate ((term (eql :peek)))
  (let* ((n (pop-term :default))
         (l (pop-term :default))
         (ts (peek-term n l)))
    (loop :for x :in ts
          :do (push-term x :default))))

(defmethod evaluate ((term (eql :pop)))
  (let* ((n (pop-term :default))
         (l (pop-term :default))
         (terms (loop :for x :from 1 :upto n
                      :collect (pop-term l))))
    (loop :for term :in (reverse terms)
          :do (push-term term :default))))

(defmethod evaluate ((term (eql '+)))
  (let ((t1 (pop-term :default))
        (t2 (pop-term :default)))
    (evaluate (+ t1 t2))))

(defmethod evaluate ((term (eql :+)))
  (evaluate '+))

(defmethod evaluate ((term (eql #'+)))
  (let ((t1 (pop-term :default))
        (t2 (pop-term :default)))
    (evaluate (+ t1 t2))))

(defmethod evaluate ((term (eql '-)))
  (let ((t1 (pop-term :default))
        (t2 (pop-term :default)))
    (evaluate (- t1 t2))))

(defmethod evaluate ((term (eql '*)))
  (let ((t1 (pop-term :default))
        (t2 (pop-term :default)))
    (evaluate (* t1 t2))))

(defmethod evaluate ((term (eql :push)))
  (let ((number (pop-term :default))
        (location (pop-term :default)))
    (loop :for x :from 1 :upto number
          :do (push-term
               (pop-term :default)
               location))))

(defmethod evaluate ((term (eql :!)))
  (let* ((evaluated-term (pop-term :default))
         (terms-at-location (get-location evaluated-term)))
    (loop :for term :in terms-at-location
          :do (evaluate term))))

(defmethod evaluate ((term (eql :r)))
  (reset-memory)
  (format t "Memory reset.~%"))

(defmethod evaluate ((term (eql :r)))
  (reset-memory)
  (format t "Memory reset.~%"))

(defmethod evaluate ((term (eql :[)))
  (format t "Memory reset.~%"))

(reduce (lambda (acc x) (evaluate x)) '(1 2 3))

(defmacro ex (x &rest xs)
  (case (car (type-of x))
    ('integer 2)))

(defun handle-err (err)
  (case (type-of err)
    ('SB-PCL::NO-APPLICABLE-METHOD-ERROR (format t "Sorry, don't know what to do with that!~%"))))

(defun e (&rest terms)
  (handler-case
      (progn
        (mapcar #'evaluate terms)
        (print-memory))
    (error (err)
      (handle-err err)
      (format t "Error Type: ~s.~%" (type-of err))
      (format t "Error: ~A.~%" err)
      (format t "Memory reset.~%")
      (reset-memory)
      )))

(defun space! ()
  (let ((term (read)))
    ))
