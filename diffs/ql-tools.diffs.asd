; vim: ft=lisp et
(in-package :asdf)
(defsystem :ql-tools.diffs
  :version "0.0.0"
  :depends-on
  (
   "ql-tools.utility"
   )
  :components((:file "diffs")))
