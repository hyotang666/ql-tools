(defpackage :ql-tools.utility(:use :cl :nitch-util.type)
  (:export
    #:installed-systems
    #:coerce-name
    #:version<=pathname
    #:bottom-directory-namestring
    ;;;; system
    #:any-version-of
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

(prototype coerce-name(t)string)
(let((cache(make-hash-table :test #'equal)))
  (defun coerce-name(thing)
    (labels(
	    (DO-PATHNAME(pathname)
	      (if(asd-p pathname)
		(pathname-name pathname)
		(ENSURE-CACHE pathname)))
	    (ENSURE-CACHE(directory)
	      (or (gethash directory cache)
		  (setf (gethash directory cache)(DO-DIRECTORY directory))))
	    (DO-DIRECTORY(directory)
	      (pathname-name(system-source-file directory)))
	    )
      (etypecase thing
	((OR SYMBOL STRING) (string-downcase thing))
	(ASDF:SYSTEM (asdf:coerce-name thing))
	(QL-DIST:SYSTEM (ql-dist:name thing))
	(PATHNAME (DO-PATHNAME thing))))))

(Prototype system-source-file(T)pathname)
(let((cache(make-hash-table :test #'equal)))
  (defun system-source-file(thing)
    (labels(
	    (DO-PATHNAME(pathname)
	      (if(asd-p pathname)
		pathname
		(DO-DIRECTORY pathname
			      (bottom-directory-namestring pathname))))
	    (DO-DIRECTORY(directory bottom)
	      (or (gethash bottom cache)
		  (setf(gethash bottom cache)(DO-SEARCH directory bottom))))
	    (DO-SEARCH(root bottom)
	      (let((result(gensym "RESULT")))
		(catch result
		  (let((asds(MAY-COLLECT-ASD root bottom result)))
		    (if(ONLY-ONE-ELEMENT? asds)
		      (throw result(car asds))
		      (FINALLY asds))))))
	    (MAY-COLLECT-ASD(root bottom result)
	      (let(acc)
		(flet((SEARCHER(directory)
			(dolist(asd(uiop:directory-files directory "*.asd"))
			  (if (search(pathname-name asd)bottom)
			    (throw result asd) ; stop to collect.
			    (push asd acc)))))
		  (uiop:collect-sub*directories root t t #'SEARCHER)
		  acc)))
	    (ONLY-ONE-ELEMENT?(list)
	      (typep list `(CONS T NULL)))
	    (FINALLY(asds)
	      (flet((READER()
		      `(,(Prompt-for `(MOD ,(list-length asds))
				     (Prompt (mismatch-pathnames asds)
					     "~%Type number.>> ")))))
		(restart-case(error "Can not resolve asd file for ~S"thing)
		  (select(number)
		    :report "Select target asd file."
		    :interactive READER
		    (nth number asds)))))
	    )
    (etypecase thing
      ((OR SYMBOL STRING)
       (system-source-file(asdf:find-system(string-downcase thing))))
      (ASDF:SYSTEM (asdf:system-source-file thing))
      (QL-DIST:SYSTEM (system-source-file(ql-dist:name thing)))
      (PATHNAME (DO-PATHNAME thing))))))

(Prototype asd-p(pathname)boolean)
(defun asd-p(pathname)
  (string= "asd" (pathname-type pathname)))

(Prototype any-version-of(system-designator)pathnames)
#@(:ql-dist #:Find-system #:Dist)
(let((cache(make-hash-table :test #'equal)))
  (defun any-version-of(system)
    (setf system (the string (string-downcase system))) ; as canonicalize.
    (labels((DIVERGE(system)
	      ;; Newer to older order.
	      (sort (if system
		      (DO-MAIN system)
		      (FALLBACK))
		    (complement #'<)
		    :key #'version<=pathname))
	    (DO-MAIN(system)
	      (loop :for pathname :in (installed-systems(Dist system))
		    :when(system-name= system pathname)
		    :collect pathname))
	    (FALLBACK()
	      (or (copy-list(gethash system cache))
		  (SEARCH-SYSTEM)))
	    (SEARCH-SYSTEM()
	      (WARN-TO-USER)
	      (uiop:while-collecting(ACC)
		(dolist(root(installed-systems(Dist :quicklisp)))
		  (dolist(asd(system-source-files root))
		    (CACHE-IT asd)
		    (when(system-name= system asd)
		      (ACC asd))))))
	    (WARN-TO-USER()
	      (warn "~S: fall back to search ~S.~%This may take minutes."
		    'any-version-of system))
	    (CACHE-IT(asd)
	      (pushnew asd(gethash(pathname-name asd)cache)
		       :test #'equal))
	    )
      (or (uiop:ensure-list(ql:local-projects-searcher system))
	  (DIVERGE(Find-system system))))))

(Prototype system-source-files(T)pathnames)
(defun system-source-files(thing)
  (etypecase thing
    (QL-DIST:SYSTEM
      (let((system-root(ql-dist:base-directory(ql-dist:release thing))))
	(loop :for subpath :in (ql-dist:system-files(ql-dist:release thing))
	      :collect (uiop:merge-pathnames* subpath system-root))))
    (SYMBOL (system-source-files(ql-dist:find-system thing)))
    (PATHNAME
      (uiop:while-collecting(asd)
	(uiop:collect-sub*directories thing t t
          (lambda(directory)
	    (dolist(file(uiop:directory-files directory "*.asd"))
	      (asd file))))))))

(Prototype system-name=(T T)boolean)
(defun system-name=(thing1 thing2)
  (string=(coerce-name thing1)(coerce-name thing2)))

