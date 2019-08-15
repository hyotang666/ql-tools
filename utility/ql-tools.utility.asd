; vim: ft=lisp et
(in-package :asdf)
(defsystem :ql-tools.utility
  :version "0.1.0"
  :depends-on
  (
   "prompt-for"         ; type safe user input.
   )
  :components((:file "utility")))
