(defpackage :ql-tools.utility(:use :cl :nitch-util.type)
  (:export
    #:installed-systems
    #:coerce-name
    #:version<=pathname
    #:bottom-directory-namestring
    #:find-systems
    ;; type
    #:pathnames
    #:system-designator
    ))
(in-package :ql-tools.utility)
(named-readtables:in-readtable with-package:syntax)

(Define-simple-type(pathnames (:element-type pathname)
			      (:element-predicate pathnamep)))
(deftype system-designator ()
  '(OR KEYWORD STRING))

(Prototype installed-systems(ql-dist:dist)pathnames)
(defun installed-systems(dist)
  "Return \"dists/DIST/software/*\""
  (uiop:subdirectories(repository dist)))

(Prototype repository(ql-dist:dist)pathname)
(defun repository(dist)
  (ql-dist:relative-to dist
    (make-pathname :directory (list :relative "software"))))

(deftype pathname-designator()
  `(OR PATHNAME STRING))

(Prototype version<=pathname(pathname-designator)(or null integer))
(defun version<=pathname(pathname)
  (labels((RETRIEVE-IF(predicate string)
	    (remove-if (complement predicate) string))
	  )
    (values ; to discard second value.
      (parse-integer(RETRIEVE-IF #'digit-char-p
				 (bottom-directory-namestring pathname))))))

(Prototype bottom-directory-namestring(pathname-designator)string)
(defun bottom-directory-namestring(pathname)
  (car(last(pathname-directory pathname))))

(defun coerce-name(thing)
  (retrieve-if #'alpha-char-p (etypecase thing
				(pathname (bottom-directory-namestring thing))
				(string thing)
				(symbol (string-downcase thing)))))

(prototype find-systems(system-designator)pathnames)
#@(:ql-dist #:find-system #:dist #:release #:short-description)

(defun find-systems(system)
  (let*((release(release(find-system system)))
	(sd(short-description release)))
    (loop :for pathname :in (installed-systems(dist release))
	  :when(system-name= sd pathname)
	  :collect pathname :into pathnames
	  :finally (return(sort pathnames(complement #'<)
				:key #'version<=pathname)))))

(defun system-name=(thing1 thing2)
  (string=(coerce-name thing1)(coerce-name thing2)))

