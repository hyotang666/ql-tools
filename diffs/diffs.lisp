(defpackage :ql-tools.diffs(:use :ql-tools.utility :cl)
  (:import-from :nitch-util.type #:prototype)
  (:export
    #:diff
    ))
(in-package :ql-tools.diffs)
(named-readtables:in-readtable with-package:syntax)

#@(:ql-dist #:All-dists #:Dist)
#@(:uiop #:Merge-pathnames*)

(defun diff(&key (dists(All-dists))(if-exists :supersede))
  (unless(listp dists) ; as canonicalize.
    (setf dists (list(Dist dists))))
  (let((*default-pathname-defaults*(Merge-pathnames* "ql-diffs/"(user-homedir-pathname))))
    (ensure-directories-exist *default-pathname-defaults*)
    (dolist(dist dists)
      (%diff dist if-exists))))

#@(:uiop #:Run-program)

(defun %diff(dist if-exists)
  (dolist(system(target-systems dist))
    (with-open-file(*standard-output* (diff-filename-of system)
				      :direction :output
				      :if-does-not-exist :create
				      :if-exists if-exists)
      (Run-program (underlying-diff system)
		   :output t
		   :ignore-error-status t)
      (terpri))))

(defun target-systems(dist)
  (labels((SETUP(table)
	    (dolist(pathname(Installed-systems dist))
	      (push pathname (gethash (Coerce-name pathname)table)))
	    (DO-RETURN table))
	  (DO-RETURN(table)
	    (loop :for systems :being :each :hash-value :of table
		  :when (AT-LEAST-2-SYSTEMS-P systems)
		  :collect(TARGETS systems)))
	  (AT-LEAST-2-SYSTEMS-P(systems)
	    (<= 2 (list-length systems)))
	  (TARGETS(systems)
	    (last (sort systems #'< :key #'Version<=pathname)
		  2))
	  )
    (SETUP(make-hash-table :test #'equal))))

(defun diff-filename-of(system)
  (let((main(Bottom-directory-namestring(first system))))
    (subseq main
	    0
	    (mismatch main (Bottom-directory-namestring(second system))))))

(defun underlying-diff(system)
  (format nil "diff -r ~{~A~^ ~}"system))
