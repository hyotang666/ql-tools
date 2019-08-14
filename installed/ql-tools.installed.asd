; vim: ft=lisp
(in-package :asdf)
(defsystem #:ql-tools.installed
  :version "0.0.1"
  :depends-on
  (
   "cl-ppcre" ; Portable perl compatible regular expressions.
   )
  :components
  ((:file "installed")))
