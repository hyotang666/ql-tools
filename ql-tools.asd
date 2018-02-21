; vim: ft=lisp et
(in-package :asdf)
(eval-when(:compile-toplevel :load-toplevel :execute)
(defvar *ql-tools.modules* '(:ql-tools.diffs
                              :ql-tools.degenerate
                              :ql-tools.load-system
                              :ql-tools.installed
                              :ql-tools.update
                              ))
) ; eval-when
(defsystem :ql-tools
  :defsystem-depends-on #.*ql-tools.modules*)

(macrolet((defs(systems)
            `(UIOP:DEFINE-PACKAGE :QL-TOOLS (:USE ,@systems)
                                  (:NICKNAMES "QLT")
                                  (:REEXPORT ,@systems))))
  (defs #.*ql-tools.modules*))
