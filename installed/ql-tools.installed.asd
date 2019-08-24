; vim: ft=lisp
(in-package :asdf)
(defsystem #:ql-tools.installed
  :version "0.2.2"
  :depends-on
  (
   "cl-ansi-text" ; Colorinzing text.
   "ql-tools.utility"
   "cl-ppcre" ; Portable perl compatible regular expressions.
   )
  :components
  ((:file "installed")))
