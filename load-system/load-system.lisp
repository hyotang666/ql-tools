(defpackage :ql-tools.load-system(:use :cl :prompt-for)
  (:import-from :ql-tools.utility
		#:System-source-files
		#:System-name=
		#:Any-version-of
		#:Mismatch-pathnames
		#:Prompt
		)
  (:export
    #:load-system
    ))
(in-package :ql-tools.load-system)
(named-readtables:in-readtable with-package:syntax)
(musam:enable)

;;;; develop utility
(define-symbol-macro u (asdf:load-system :ql-tools.load-system))

#@(:asdf #:*System-definition-search-functions*)

(defun load-system(system)
  (let((*System-definition-search-functions*(cons (make-searcher)
						  *System-definition-search-functions*)))
    (asdf:load-system system)))

#@(:asdf #:System-source-file #:System-registered-p)

;;;; We treat sub system as monolithic.
;;;; E.g. mcclim has many sub system as module.(mcclim-core, clim, etc...)
(let((monolith (make-hash-table :test #'equal)))
  (defun make-searcher()
    (lambda(system)
      (labels((LOG-IT(arg)
		(print arg)
		(force-output))
	      (ALREADY-REGISTERED-ONE(registered-p)
		(when registered-p
		  (System-source-file(cdr registered-p))))
	      (DO-SEARCH(systems)
		(let((length(list-length systems)))
		  (when systems
		    (RETURN-IT (if(HAVE-ONLY-ONE? length)
				 (car systems)
				 (ASK-USER `(MOD ,length) systems))))))
	      (HAVE-ONLY-ONE?(length)
		(not(< 1 length)))
	      (RETURN-IT(directory)
		(loop :for asd :in (System-source-files directory)
		      :if (System-name= system asd)
		      :collect asd :into result
		      :do (setf(gethash(pathname-name asd)monolith)asd)
		      :finally (return (car result))))
	      (ASK-USER(type systems)
		(nth (Prompt-for type (Prompt (Mismatch-pathnames systems)
					      "~&Which version do you load?~%Type number >> "))
		     systems))
	      )
	;; one .asd file may define some systems.
	;; in such case, we should treat it as atomic.
	(LOG-IT system)
	(or (ALREADY-REGISTERED-ONE(System-registered-p system))
	    (gethash(string-downcase system)monolith)
	    (DO-SEARCH(Any-version-of system)))))))
