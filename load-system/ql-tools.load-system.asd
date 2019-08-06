; vim: ft=lisp et
(in-package :asdf)
(defsystem :ql-tools.load-system
  :version "0.0.0"
  :depends-on
  (
   "prompt-for"         ; type safe user input.
   "ql-tools.utility"
   "with-package"       ; using package temporarily.
   )
  :components((:file "load-system")))
