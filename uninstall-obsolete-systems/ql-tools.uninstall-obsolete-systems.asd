; vim: ft=lisp et
(defsystem :ql-tools.uninstall-obsolete-systems
  :depends-on (:nitch-util)
  :components ((:file "uninstall-obsolete-systems")))
