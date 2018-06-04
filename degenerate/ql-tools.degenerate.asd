; vim: ft=lisp et
(in-package :asdf)
(defsystem :ql-tools.degenerate
  :depends-on
  (
   "ql-tools.utility"
   "with-package"       ; using package temporarily.
   )
  :components((:file "degenerate")))
