; vim: ft=lisp et
(in-package :asdf)
(defsystem :ql-tools.utility
  :version "0.0.0"
  :depends-on
  (
   "type-ext"           ; type extensions.
   "with-package"       ; using package temporarily.
   "prompt-for"         ; type safe user input.
   )
  :components((:file "utility")))
