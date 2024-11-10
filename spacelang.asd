;;;; spacelang.asd
(asdf:defsystem #:spacelang
  :description "Spacelang is a stack programming language."
  :author "Vlad P. Luchian <cstmlcodes@gmail.com>"
  :license "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:alexandria
               :str
               :arrows
               :cl-punch
               :bt-semaphore
               :lparallel
               :smug)
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "parser")
                 (:file "term")
                 (:file "memory")
                 (:file "universe")
                 (:file "evaluator")
                 (:file "spacelang")
                 (:file "compiler")))))

(asdf:defsystem #:spacelang/test
  :description "Test suite for spacelang."
  :author "Vlad P. Luchian <cstmlcodes@gmail.com>"
  :license "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:fiveam
               :spacelang)
  ; :pathname "test/"
  :components ((:module "test"
                :components ((:file "package")
                             (:file "test"))))
  :perform (test-op (op c) (symbol-call :fiveam :run-tests c)))
