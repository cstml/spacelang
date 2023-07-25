;;;; test.lisp
(in-package #:spacelang/test)

(require :fiveam)

(def-suite* spacelang-tests)

(defmacro universe-fixture (x)
  "Initialises the test with a new universe and binds the home machine memory."
  `(progn
     (let* ((*universe (spacelang::init-universe))
            (*home-machine (spacelang::get-memory :home *universe)))
       ,x)))

(defmacro stack-ends-as (actions-before stack-expected)
  "Macro to compare the stack at the end of an evaluation. To be used inside a
universe-fixture."
  `(progn
     ,actions-before
     (equal (spacelang::stack *home-machine)
            ,stack-expected)))

(defmacro all-stacks-end-as (actions-before stacks-expected)
  "Macro to compare the stack at the end of an evaluation.To be used inside a
universe-fixture."
  `(progn
     ,actions-before
     (equal (set-exclusive-or (mapcar (lambda (x) (cons (car x) (spacelang::stack (cdr x))))
                                      (hash-table-alist (spacelang::space-instances *universe)))
                              ,stacks-expected)
            nil)))

(defun evaluate-sequence (memory sequence)
  (mapcar (lambda (term) (spacelang::evaluate memory term)) sequence))

(test eval-+
  (and
   (universe-fixture
    (is (stack-ends-as (evaluate-sequence *home-machine '(1 2 :+))
                       '(3))))
   (universe-fixture
    (is (stack-ends-as (evaluate-sequence *home-machine '(1 2 3 :+ :+))
                       '(6))))

   (for-all ((a (gen-integer))
             (b (gen-integer)))
     (universe-fixture
      (is (stack-ends-as (evaluate-sequence *home-machine (list a b :+))
                         (list (+ a b))))))

   (for-all ((a (gen-integer))
             (b (gen-integer))
             (c (gen-integer)))
     (universe-fixture
      (is (stack-ends-as (evaluate-sequence *home-machine (list a b c :+ :+))
                         (list (+ a b c))))))))

(test eval--
  (and
   (universe-fixture
    (is (stack-ends-as (evaluate-sequence *home-machine '(1 2 :-))
                       '(1))))
   (universe-fixture
    (is (stack-ends-as (evaluate-sequence *home-machine '(1 2 3 :- :-))
                       '(0))))))

(test eval-*
  (and
   (universe-fixture
    (is (stack-ends-as (evaluate-sequence *home-machine '(1 2 :*))
                       '(2))))
   (universe-fixture
    (is (stack-ends-as (evaluate-sequence *home-machine '(1 2 3 :* :*))
                       '(6))))))

(test evaluate-send
  (universe-fixture
   (is (all-stacks-end-as (evaluate-sequence *home-machine '(1 (a) :send))
                          '((:home . '())
                            (:a   . '(1)))))))

(fiveam:run!)
