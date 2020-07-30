; vim: ft=lisp et
(in-package :asdf)
(defsystem "ql-tools.search"
  :version "0.0.1"
  :depends-on
  (
   :cl-ppcre	; Portable perl conpatible regular expression.
   )
  :components
  ((:file "search")
   )
  )
