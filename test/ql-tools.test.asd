; vim: ft=lisp et
(in-package :asdf)
(defsystem "ql-tools.test"
  :version "0.2.1"
  :depends-on
  (
   "ql-tools.utility"
   )
  :components ((:file "test")))
