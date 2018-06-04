; vim: ft=lisp et
(in-package :asdf)
(defsystem :ql-tools.load-system
  :depends-on
  (
   "prompt-for"         ; type safe user input.
   "musam"              ; #` dispatch macro.
   "ql-tools.utility"
   "with-package"       ; using package temporarily.
   )
  :components((:file "load-system")))
