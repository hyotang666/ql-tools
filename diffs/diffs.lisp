(defpackage :ql-tools.diffs
  (:use :cl)
  (:import-from :ql-tools.utility
                #:installed-systems
                #:coerce-name
                #:bottom-directory-namestring
                #:version<=pathname)
  (:export #:diff))

(in-package :ql-tools.diffs)

(defun diff (&key (dists (ql-dist:all-dists)) (if-exists :supersede))
  (unless (listp dists) ; as canonicalize.
    (setf dists (list (ql-dist:dist dists))))
  (let ((*default-pathname-defaults*
         (uiop:merge-pathnames* "ql-diffs/" (user-homedir-pathname))))
    (ensure-directories-exist *default-pathname-defaults*)
    (dolist (dist dists) (%diff dist if-exists))))

(defun %diff (dist if-exists)
  (dolist (system (target-systems dist))
    (with-open-file (*standard-output* (diff-filename-of system) :direction :output
                     :if-does-not-exist :create
                     :if-exists if-exists)
      (uiop:run-program (underlying-diff system)
                        :output t
                        :ignore-error-status t)
      (terpri))))

(defun target-systems (dist)
  (labels ((setup (table)
             (dolist (pathname (installed-systems dist))
               (restart-case (push pathname
                                   (gethash (coerce-name pathname) table))
                 (skip ()
                     :report (lambda (stream)
                               (format stream "Skip to get diff of ~S"
                                       (bottom-directory-namestring pathname)))
                     :test (lambda (condition)
                             (typep condition 'ql-tools.utility:not-resolve))
                   #|Do nothing|#)))
             (do-return table))
           (do-return (table)
             (loop :for systems :being :each :hash-value :of table
                   :when (at-least-2-systems-p systems)
                     :collect (targets systems)))
           (at-least-2-systems-p (systems)
             (<= 2 (list-length systems)))
           (targets (systems)
             (last (sort systems #'< :key #'version<=pathname) 2)))
    (setup (make-hash-table :test #'equal))))

(defun diff-filename-of (system)
  (let ((main (bottom-directory-namestring (first system))))
    (subseq main 0
            (mismatch main (bottom-directory-namestring (second system))))))

(defun underlying-diff (system) (format nil "diff -r ~{~A~^ ~}" system))
