; vim: ft=lisp et
(in-package :asdf)
(defsystem :ql-tools.diffs
  :depends-on (:nitch-util :ql-tools.utility :with-package)
  :components((:file "diffs")))
