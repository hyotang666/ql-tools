; vim: ft=lisp et
(in-package :asdf)
(defsystem :ql-tools.load-system
  :version "0.0.1"
  :depends-on
  (
   "prompt-for"         ; type safe user input.
   "ql-tools.utility"
   )
  :components((:file "load-system")))
