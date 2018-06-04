; vim: ft=lisp et
(in-package :asdf)
(defsystem :ql-tools.utility
  :depends-on
  (
   "type-ext"           ; type extensions.
   "with-package"       ; using package temporarily.
   "musam"              ; #` dispatch macro.
   "prompt-for"         ; type safe user input.
   )
  :components((:file "utility")))
