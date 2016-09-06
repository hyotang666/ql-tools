(defpackage :ql-tools.load-system(:use :cl :ql-tools.utility :prompt-for)
  (:export
    #:load-system
    ))
(in-package :ql-tools.load-system)
(named-readtables:in-readtable with-package:syntax)
(musam:enable)

#@(:asdf #:*system-definition-search-functions*)

(defun load-system(system)
  (let((*system-definition-search-functions*(list* (make-searcher system)
						   *system-definition-search-functions*)))
    (asdf:load-system system)))

(defun make-searcher(system)
  (let*((systems(find-systems system))
	(length(list-length systems)))
    (constantly (if(not(< 1 length))
		  (car systems)
		  (nth (prompt-for #`(typep $user-input `(mod ,length))
				   (prompt systems))
		       systems)))))

