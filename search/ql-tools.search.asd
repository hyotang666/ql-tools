; vim: ft=lisp et
(in-package :asdf)
(defsystem "ql-tools.search"
  :depends-on
  (
   :cl-ppcre	; Portable perl conpatible regular expression.
   )
  :components
  ((:file "search")
   )
  )
