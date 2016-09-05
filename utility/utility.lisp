(defpackage :ql-tools.utility(:use :cl :nitch-util.type)
  (:export
    #:installed-systems
    #:coerce-name
    #:version<=pathname
    #:bottom-directory-namestring
    ;; type
    #:pathnames
    #:system-designator
    ))
(in-package :ql-tools.utility)

(define-simple-type(pathnames (:element-type pathname)
			      (:element-predicate pathnamep)))
(deftype system-designator ()
  '(or keyword string))

(prototype installed-systems(ql-dist:dist)pathnames)
(defun installed-systems(dist)
  (uiop:subdirectories(repository dist)))

(prototype repository(ql-dist:dist)pathname)
(defun repository(dist)
  (ql-dist:relative-to dist
    (make-pathname :directory (list :relative "software"))))

(prototype version<=pathname(pathname)integer)
(defun version<=pathname(pathname)
  (parse-integer(retrieve-if #'digit-char-p
			     (bottom-directory-namestring pathname))))

(defun retrieve-if(predicate string)
  (remove-if (complement predicate) string))

(prototype bottom-directory-namestring(pathname)string)
(defun bottom-directory-namestring(pathname)
  (car(last(pathname-directory pathname))))

(defun coerce-name(thing)
  (retrieve-if #'alpha-char-p (etypecase thing
				(pathname (bottom-directory-namestring thing))
				(string thing)
				(symbol (string-downcase thing)))))
