; vim: ft=lisp et
(in-package :asdf)
(defsystem :ql-tools.load-system
  :depends-on (:with-package)
  :components((:file "load-system")))
