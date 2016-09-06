; vim: ft=lisp et
(in-package :asdf)
(defsystem :ql-tools.load-system
  :depends-on (:prompt-for :musam :ql-tools.utility :with-package)
  :components((:file "load-system")))
