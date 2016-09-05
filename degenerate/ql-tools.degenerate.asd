; vim: ft=lisp et
(in-package :asdf)
(defsystem :ql-tools.degenerate
  :depends-on (:musam :ql-tools.utility :nitch-util :named-readtables)
  :components((:file "degenerate")))
