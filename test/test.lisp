(defpackage :ql-tools.test(:use :cl)
  (:export
    #:test
    )
  )
(in-package :ql-tools.test)

(defmacro with-package-clean(()&body body)
  `(let*((oldies
           (copy-list(list-all-packages))))
     (unwind-protect(progn ,@body)
       (let((diff
              (set-difference (list-all-packages)
                              oldies)))
         (if(null diff)
           (warn "~&No packages are made.")
           (handler-bind((package-error
                           (lambda(condition)
                             (let((restart
                                    (find-restart 'continue condition)))
                               (when restart
                                 (invoke-restart restart))))))
             (format *trace-output* "~&Deleting packages ~:S" diff)
             (mapc #'delete-package diff)))))))

(defun test()
  (labels((restarter(restart)
	    (lambda(c)
	      (let((restart(find-restart restart c)))
		(when restart
		  (invoke-restart restart)))))
	  (require-loader(jumper)
	    (lambda(c)
	      (handler-bind((asdf:missing-component
			      (restarter 'asdf:clear-configuration-and-retry)))
		(ql:quickload(asdf/find-component:missing-requires c)))
	      (funcall jumper)))
	  )
    (let(giveups)
      (dolist(release (ql-tools.utility:all-releases)giveups)
	:try
	(restart-case(handler-bind((asdf:missing-dependency
				     (require-loader(lambda()(go :try)))))
		       (let((target(or (find "test" (ql-dist:system-files release)
					     :test #'search)
				       (ql-dist:name release))))
                         (with-package-clean()
                           (asdf:test-system (uiop:split-name-type target)))
			 (format t "~&~S on ~S test finished."
				 target
				 (ql-dist:name(ql-dist:dist release))))
		       (force-output)
		       (ql-util:press-enter-to-continue))
	  (giveup()
	    :report
	    (lambda(s)(format s "Give up to test ~S"(ql-dist:name release)))
	    (push release giveups)))))))
