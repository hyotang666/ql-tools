(defpackage :ql-tools.load-system(:use :cl)
  (:export
    #:load-system
    ))
(in-package :ql-tools.load-system)
(named-readtables:in-readtable with-package:syntax)

#@(:asdf #:*system-definition-search-functions*)

(defun load-system(system)
  (let((*system-definition-search-functions*(list* (make-searcher system)
						   *system-definition-search-functions*)))
    (asdf:load-system system)))
