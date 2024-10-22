;;;; spacelang.asd

(asdf:defsystem #:spacelang
  :description "Spacelang is a stack programming language."
  :author "Vlad P. Luchian <cstmlcodes@gmail.com>"
  :license "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:alexandria
               :str
               :fiveam
               :cl-punch
               :smug
               :bt-semaphore)
  :components ((:file "package")
               (:file "parser")
               (:file "spacelang")))

(asdf:defsystem #:spacelang/test
  :description "Test suite for spacelang."
  :author "Vlad P. Luchian <cstmlcodes@gmail.com>"
  :license "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:alexandria
               :fiveam
               :cl-punch
               :for
               :spacelang)
  :pathname "test/"
  :components ((:file "package")
               (:file "test")))
