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
  (let((*system-definition-search-functions*(cons (make-searcher)
						  *system-definition-search-functions*)))
    (asdf:load-system system)))

(defun make-searcher()
  (let((memo(make-hash-table :test #'equal)))
    (lambda(system)
      (or (gethash(string-downcase system)memo)
	  (setf(gethash(string-downcase system)memo)
	    (let*((systems(ignore-errors(find-systems system)))
		  (length(list-length systems)))
	      (and systems
		   (if(not(< 1 length))
		     (merge-to-source-file (car systems)
					   (search-asd system (car systems)))
		     (let((target(nth (prompt-for `(mod ,length)
						  (prompt systems))
				      systems)))
		       (merge-to-source-file target (search-asd system target)))))))))))

(defun search-asd (system root)
  (flet((asd-file-searcher(directory)
	  (let((asd(uiop:directory-files directory
					 (format nil "~(~A~).asd" system))))
	    (when asd (return-from search-asd (car asd))))))
    (uiop:collect-sub*directories root t t #'asd-file-searcher)))

(defun merge-to-source-file(system-root-directory system-source-file)
  (labels((rec(root-components source-components)
	    (if(endp root-components)
	      (do-return source-components)
	      (rec(cdr root-components)(cdr source-components))))
	  (do-return(rest-components)
	    (make-pathname :directory (appended-directory rest-components)
			   :name (pathname-name system-source-file)
			   :type (pathname-type system-source-file)))
	  (appended-directory(rest-components)
	    (append (pathname-directory system-root-directory)
		    rest-components)))
    (rec (pathname-directory system-root-directory)
	 (pathname-directory system-source-file))))

(defun prompt(systems)
  (with-output-to-string(*standard-output*)
    (loop :for n :upfrom 0
	  :for system :in (mismatch-pathnames systems)
	  :do (format t "~%~3D: ~A" n system)
	  :finally (format t "~&Which system do you install?~%Type number >> "))))

(defun mismatch-pathnames(pathnames)
  (labels((rec(components)
	    (if(same-directory-p components)
	      (rec (map-into components #'cdr components))
	      (do-return components)))
	  (same-directory-p(components)
	    (loop :with elt = (caar components)
		  :for component :in (cdr components)
		  :always (string= elt (car component))))
	  (do-return(components)
	    (map-into components
		      #`(make-pathname :directory (cons :relative $components))
		      components)))
    (rec (mapcar #'pathname-directory pathnames))))
