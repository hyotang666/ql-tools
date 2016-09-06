; vim: ft=lisp et
(in-package :asdf)
(defsystem :ql-tools.degenerate
  :depends-on (:ql-tools.utility :with-package)
  :components((:file "degenerate")))
