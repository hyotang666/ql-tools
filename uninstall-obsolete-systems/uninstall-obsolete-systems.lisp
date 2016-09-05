(defpackage :ql-tools.uninstall-obsolete-systems
  (:use :nitch-util.type :ql-tools.utility :cl)
  (:export
    #:uninstall-obsolete-systems
    ))
(in-package :ql-tools.uninstall-obsolete-systems)

(defun uninstall-obsolete-systems(&optional(dists(ql-dist:all-dists))
				   (validate(constantly t))
				   debug)
  (unless(listp dists) ; as canonicalize.
    (setf dists (list(ql-dist:dist dists))))
  (dolist(dist dists)
    (%uninstall-obsolete-systems dist validate debug)))

(defun %uninstall-obsolete-systems(dist &optional(validate(constantly t))debug)
  (dolist(obsolete-system(obsolete-systems dist))
    (when debug (format *debug-io* "~%Try to uninstall ~S"obsolete-system))
    (uiop:delete-directory-tree obsolete-system :validate validate)
    (delete-file(old-archive-file obsolete-system dist))))

(define-simple-type(dists (:element-type ql-dist:dist)
			  (:element-predicate (lambda(x)(typep x 'ql-dist:dist)))))

(prototype obsolete-systems(ql-dist:dist)pathnames)
(defun obsolete-systems(dist)
  (set-difference (installed-systems dist)
		  (latest-systems dist)
		  :key #'pathname-directory :test #'equal))

(prototype latest-systems(ql-dist:dist)pathnames)
(defun latest-systems(dist)
  (delete-duplicates(systems<=dist dist) :test #'equal))

(prototype systems<=dist(ql-dist:dist)pathnames)
(defun systems<=dist(dist)
  (loop :for system :in (ql-dist:installed-systems dist)
	:collect (ql-dist:base-directory(ql-dist:release system))))

(defun old-archive-file(pathname dist)
  (uiop:merge-pathnames*
    (make-pathname :directory '(:relative "archives")
		   :name(car(last(pathname-directory pathname)))
		   :type "tgz")
    (ql-dist:base-directory dist)))
