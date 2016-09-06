(defpackage :ql-tools.load-system(:use :cl :ql-tools.utility :prompt-for)
  (:export
    #:load-system
    ))
(in-package :ql-tools.load-system)
(named-readtables:in-readtable with-package:syntax)
(musam:enable)

;;;; develop utility
(define-symbol-macro u (asdf:load-system :ql-tools.load-system))

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
		  (nth (prompt-for `(mod ,length)
				   (prompt systems))
		       systems)))))

(defun prompt(systems)
  (with-output-to-string(*standard-output*)
    (loop :for n :upfrom 0
	  :for system :in (mismatch-pathnames systems)
	  :do (format t "~%~3D: ~A" n system)
	  :finally (format t "~&Which system do you install?~%Type number >> "))))


