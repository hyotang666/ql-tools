; vim: ft=lisp et
(in-package :asdf)
(defsystem :ql-tools.utility
  :depends-on(:nitch-util)
  :components((:file "utility")))
