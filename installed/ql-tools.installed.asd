; vim: ft=lisp
(in-package :asdf)
(defsystem #:ql-tools.installed
  :version "0.2.0"
  :depends-on
  (
   "ql-tools.utility"
   "cl-ppcre" ; Portable perl compatible regular expressions.
   )
  :components
  ((:file "installed")))
