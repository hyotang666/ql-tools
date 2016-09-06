(defpackage :ql-tools.degenerate(:use :cl :ql-tools.utility)
  (:export
    #:degenerate
    ))
(in-package :ql-tools.degenerate)

;;;; develop utility.
(define-symbol-macro u (asdf:load-system :ql-tools.degenerate))

;;;; readtable setup.
(with-package:enable) ; #@

#@(:ql-dist #:install-metadata-file #:find-system)

(defun degenerate(system)
  (let((previous-system(previous-system system)))
    (backup system)
    (with-open-file(s (install-metadata-file(find-system system))
		      :direction :output
		      :if-exists :supersede)
      (write-line (relative-namestring previous-system)))))

#@(:ql-dist #:short-description #:release)

(defun previous-system(system)
  (let((systems(find-systems system)))
    (or (nth (1+ (position (short-description(release system))
			   systems
			   :key #'bottom-directory-namestring
			   :test #'string=))
	     systems)
	(error "No previous systems of ~S" system))))

(defun backup(system)
  (with-open-file(*standard-output* (backup-file)
				    :direction :output
				    :if-does-not-exist :create
				    :if-exists :append)
    (write-line (relative-namestring(asdf:system-source-file system )))))

(defun backup-file()
  (uiop:merge-pathnames* "backup.txt" (asdf:system-source-directory :ql-tools.degenerate)))

(defun relative-namestring(pathname)
  (flet((under-dists-components(components)
	  (cons :relative
		(nthcdr (position "dists" components :test #'string=)
			components))))
    (let((directory(pathname-directory pathname)))
      (namestring(make-pathname :directory(under-dists-components directory) 
				:name (pathname-name pathname)
				:type (pathname-type pathname))))))
