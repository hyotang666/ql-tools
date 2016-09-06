; vim: ft=lisp et
(in-package :asdf)
(defsystem :ql-tools.utility
  :depends-on(:nitch-util :with-package)
  :components((:file "utility")))
