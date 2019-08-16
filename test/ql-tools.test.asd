; vim: ft=lisp et
(in-package :asdf)
(defsystem "ql-tools.test"
  :version "0.0.3"
  :depends-on
  (
   "ql-tools.utility"
   )
  :components ((:file "test")))
