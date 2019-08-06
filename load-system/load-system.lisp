(defpackage :ql-tools.load-system(:use :cl)
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

;;;; develop utility
(define-symbol-macro u (asdf:load-system :ql-tools.load-system))


(defun load-system(system)
  (let((asdf:*system-definition-search-functions*(cons (make-searcher)
						  asdf:*system-definition-search-functions*)))
    (asdf:load-system system)))

;;;; We treat sub system as monolithic.
;;;; E.g. mcclim has many sub system as module.(mcclim-core, clim, etc...)
(let((monolith (make-hash-table :test #'equal)))
  (defun make-searcher()
    (lambda(system)
      (labels((LOG-IT(arg)
		(print arg)
		(force-output))
	      (ALREADY-REGISTERED-ONE(system)
		(when system
		  (asdf:system-source-file system)))
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
		(nth (prompt-for:prompt-for
		       type
		       (Prompt (Mismatch-pathnames systems)
			       "~&Which version do you load?~%Type number >> "))
		     systems))
	      )
	;; one .asd file may define some systems.
	;; in such case, we should treat it as atomic.
	(LOG-IT system)
	(or (ALREADY-REGISTERED-ONE(asdf:registered-system system))
	    (gethash(string-downcase system)monolith)
	    (DO-SEARCH(Any-version-of system)))))))
