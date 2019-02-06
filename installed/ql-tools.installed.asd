; vim: ft=lisp
(in-package :asdf)
(defsystem #:ql-tools.installed
  :depends-on
  (
   "cl-ppcre" ; Portable perl compatible regular expressions.
   )
  :components
  ((:file "installed")))
