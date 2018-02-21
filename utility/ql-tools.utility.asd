; vim: ft=lisp et
(in-package :asdf)
(defsystem :ql-tools.utility
  :depends-on(:type-ext :with-package :musam :prompt-for)
  :components((:file "utility")))
