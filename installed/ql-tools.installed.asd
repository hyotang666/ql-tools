; vim: ft=lisp
(in-package :asdf)
(defsystem #:ql-tools.installed
  :version "0.0.0"
  :depends-on
  (
   "cl-ppcre" ; Portable perl compatible regular expressions.
   )
  :components
  ((:file "installed")))
