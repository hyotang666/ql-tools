; vim: ft=lisp et
(in-package :asdf)
(defsystem :ql-tools.diffs
  :depends-on
  (
   "ql-tools.utility"
   )
  :components((:file "diffs")))
