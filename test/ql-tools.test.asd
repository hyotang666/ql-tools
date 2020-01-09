; vim: ft=lisp et
(in-package :asdf)
(defsystem "ql-tools.test"
  :version "0.1.0"
  :depends-on
  (
   "ql-tools.utility"
   )
  :components ((:file "test")))
