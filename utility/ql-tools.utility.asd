; vim: ft=lisp et
(in-package :asdf)
(defsystem :ql-tools.utility
  :version "0.0.1"
  :depends-on
  (
   "type-ext"           ; type extensions.
   "prompt-for"         ; type safe user input.
   )
  :components((:file "utility")))
