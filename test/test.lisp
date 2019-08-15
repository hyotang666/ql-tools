(defpackage :ql-tools.test(:use :cl)
  (:export
    #:test
    )
  )
(in-package :ql-tools.test)

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
  (dolist(release (ql-tools.utility:all-releases))
    :try
    (restart-case(handler-bind((asdf:missing-dependency
				 (require-loader(lambda()(go :try)))))
		   (asdf:test-system(ql-dist:name release))
		   (format t "~&~S on ~S test finished."
			   (ql-dist:name release)
			   (ql-dist:name(ql-dist:dist release)))
		   (force-output)
		   (ql-util:press-enter-to-continue))
	(giveup()
	  :report
	  (lambda(s)(format s "Give up to test ~S"(ql-dist:name release))))))
    ))
