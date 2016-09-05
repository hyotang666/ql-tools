; vim: ft=lisp et
(in-package :asdf)
(defsystem :ql-tools
  :depends-on(:nitch-util :ql-tools.utility :ql-tools.diffs :ql-tools.uninstall-obsolete-systems :ql-tools.degenerate)
  :components ((:file "interface")))
