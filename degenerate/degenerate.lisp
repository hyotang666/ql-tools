(defpackage :ql-tools.degenerate(:use :cl :ql-tools.utility)
  (:export
    #:degenerate
    ))
(in-package :ql-tools.degenerate)
(named-readtables:in-readtable with-package:syntax)

(defvar *degenerated-directory* (uiop:merge-pathnames* "degenerated/"
						       (asdf:system-source-directory :ql-tools.degenerate)))

(macrolet((mkdir(subdir)
	    `(ENSURE-DIRECTORIES-EXIST(UIOP:MERGE-PATHNAMES* (CONCATENATE 'STRING ,subdir "/")
							     *DEGENERATED-DIRECTORY*))))
  (mkdir "systems")
  (mkdir "releases"))

(defun output(data pathname &key(if-does-not-exist :error)
		   (if-exists :supersede))
  (with-open-file(*standard-output* pathname
				    :direction :output
				    :if-does-not-exist if-does-not-exist
				    :if-exists if-exists)
    (write-line data)))

(defun setup()
  (labels((DIVERGE()
	    (if(uiop:featurep :ros.init)
	      (MAY-SET #'SET-TO-.ROSWELL)
	      (MAY-SET #'SET-TO-IMPLE)))
	  (MAY-SET(setter)
	    (unless(uiop:featurep :qlt-degenerate)
	      (funcall setter)
	      (pushnew :qlt-degenerate *features*)))
	  (SET-TO-.ROSWELL()
	    (OUTPUT-LOAD-FORM(uiop:merge-pathnames* ".roswell/init.lisp"
						    (user-homedir-pathname))))
	  (OUTPUT-LOAD-FORM(to)
	    (output (THE-LOAD-FORM)
		    to
		    :if-exists :append
		    :if-does-not-exist :create))
	  (THE-LOAD-FORM()
	    (format nil ";;;; This form is generated by QL-TOOLS.DEGENERATE~%~S"
		    `(PROGN (QL:QUICKLOAD :QL-TOOLS.DEGENERATE :SILENT T)
			    (PUSHNEW :QLT-DEGENERATE *FEATURES*))))
	  (SET-TO-IMPLE()
	    (if(not(uiop:featurep :quicklisp))
	      (error "Missing quicklisp.~&QL-TOOLS heavily depends on it.")
	      (let((init-file(ql-impl-util::init-file-name)))
		(if init-file
		  (OUTPUT-LOAD-FORM (uiop:merge-pathnames* init-file (user-homedir-pathname)))
		  (error "Don't know how to add to init file for your implementation.")))))
	  )
    (unless(uiop:featurep :qlt-degenerate)
      (DIVERGE))))

#@(:ql-dist #:Find-system)

(defun degenerate(system)
  (setup)
  (labels(
	  (DIVERGE(release versions)
	    (if(probe-file release)
	      (MAIN release(OLDER-SYSTEM release versions))
	      (MAIN release versions)))
	  (OLDER-SYSTEM(release older-systems)
	    (nth (1+(position (Bottom-directory-namestring(uiop:read-file-line release))
			      older-systems
			      :test #'string=
			      :key #'Bottom-directory-namestring))
		 older-systems))
	  (MAIN(release versions)
	    (let((older-system(second versions)))
	      (if older-system
		(%MAIN older-system release)
		(HANDLE-RESTART release versions))))
	  (%MAIN(older-system release)
	    (DEGENERATE-RELEASE older-system release)
	    (DEGENERATE-SYSTEMS older-system))
	  (HANDLE-RESTART(release versions)
	    (if versions
	      (progn (cerror "Force to do."
			     "Only one version for ~S."system)
		     (%MAIN (car versions) release))
	      (warn "System named ~S is not found."system)))
	  (DEGENERATE-RELEASE(older-system release)
	    (%DEGENERATE (namestring(system-homedir-pathname older-system))
			 release))
	  (%DEGENERATE(data to)
	    (output data to :if-does-not-exist :create :if-exists :append))
	  (DEGENERATE-SYSTEMS(older-system)
	    (dolist(asd(System-source-files older-system))
	      (%DEGENERATE (relative-namestring asd)
			   (degenerated-pathname (pathname-name asd) "systems"))))
	  )
    (DIVERGE (degenerated-pathname system "releases")
	     (Any-version-of system))))

(defun system-homedir-pathname(thing) ; separated for easy testing.
  (labels((UPPER-DIRECTORIES(pathname)
	    (make-pathname :directory(REC(pathname-directory pathname))))
	  (REC(components &optional acc)
	    (if(endp components)
	      (error "There is no \"software\" directory in pathname.~%so can not handle it. ~S" thing)
	      (COLLECT (car components)(cdr components) acc)))
	  (COLLECT(component rest acc)
	    (if(string= "software" component)
	      (nreconc acc (list component (car rest)))
	      (REC rest (push component acc))))
	  (ASDF(system)
	    (asdf:system-source-directory system))
	  (QUICKLISP(system)
	    (ql-dist:base-directory(ql-dist:release system)))
	  )
    (UPPER-DIRECTORIES(etypecase thing
			(asdf:system (ASDF thing))
			((or ql-dist:system ql-dist:release)
			 (QUICKLISP thing))
			((or pathname string)
			 thing)))))

(defun degenerated-pathname(system subdir)
  (pathname(format nil "~A~@[~A~]/~(~A~).txt"
		   (namestring *degenerated-directory*)
		   subdir
		   (string-downcase system))))

#@(:ql-dist #:Base-directory #:Dist)

(defun degenerated-definition-searcher(system)
  (labels((DEGENERATED-DEFINITION-PATHNAME(pathname)
	    (uiop:merge-pathnames* (car(last(uiop:read-file-lines pathname)))
				   (Base-directory(Dist :quicklisp))))
	  )
    (let((pathname(degenerated-pathname system "systems")))
      (when(probe-file pathname)
	(DEGENERATED-DEFINITION-PATHNAME pathname)))))

(push #'degenerated-definition-searcher asdf:*system-definition-search-functions*)

(defun relative-namestring(pathname) ; separeted for easy testing.
  (flet((under-dists-components(components)
	  (cons :relative
		(nthcdr (position "software" components :test #'string=)
			components))))
    (let((directory(pathname-directory pathname)))
      (namestring(make-pathname :directory(under-dists-components directory) 
				:name (pathname-name pathname)
				:type (pathname-type pathname))))))
