(defpackage :ql-tools.utility(:use :cl :type-ext :prompt-for)
  (:export
    ;;;; dist
    #:installed-systems
    ;;;; generics
    #:coerce-name
    #:system-name=
    #:system-source-files
    ;;;; pathname
    #:version<=pathname
    #:bottom-directory-namestring
    ;;;; pathnames
    #:prompt
    #:mismatch-pathnames
    ;;;; system
    #:any-version-of
    ;;;; type name
    #:pathnames
    #:system-designator
    ))
(in-package :ql-tools.utility)

(Define-simple-type(pathnames (:element-type pathname)
			      (:element-predicate pathnamep)))
(deftype system-designator ()
  '(OR KEYWORD STRING))

(Prototype installed-systems(ql-dist:dist)pathnames)
(defun installed-systems(dist)
  "Return \"dists/DIST/software/*\""
  (uiop:subdirectories(ql-dist:relative-to dist "software/")))

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
	      (loop :for pathname :in (installed-systems(ql-dist:dist system))
		    :when(system-name= system pathname)
		    :collect pathname))
	    (FALLBACK()
	      (or (copy-list(gethash system cache))
		  (SEARCH-SYSTEM)))
	    (SEARCH-SYSTEM()
	      (WARN-TO-USER)
	      (uiop:while-collecting(ACC)
		(dolist(root(installed-systems(ql-dist:dist :quicklisp)))
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
	  (DIVERGE(ql-dist:find-system system))))))

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

(Prototype mismatch-pathnames(pathnames)pathnames)
(macrolet((assertion(form)
	    `(ASSERT ,form()
		     'SIMPLE-ERROR
		     :FORMAT-CONTROL "~S: argument PATHNAMES must have at least 2 elements, but ~S."
		     :FORMAT-ARGUMENTS `(MISMATCH-PATHNAMES ,pathnames)))
	  (!(tag form)
	    `(HANDLER-CASE,form
	       ,(ecase tag
		  (0 `(ERROR()
			(ERROR 'SIMPLE-TYPE-ERROR
			       :FORMAT-CONTROL "~S: argument PATHNAMES must proper list, but ~S."
			       :FORMAT-ARGUMENTS (LIST 'MISMATCH-PATHNAMES PATHNAMES))))
		  (1 `(ERROR()
			(LET((DATUM(FIND-IF-NOT #'PATHNAMEP PATHNAMES)))
			  (ERROR 'SIMPLE-TYPE-ERROR
				 :FORMAT-CONTROL "~S: argument PATHNAMES's every element must pathname, but ~S.~%Especially ~S."
				 :FORMAT-ARGUMENTS (LIST 'MISMATCH-PATHNAMES PATHNAMES DATUM)
				 :EXPECTED-TYPE 'PATHNAME
				 :DATUM DATUM))))))))

  (defun mismatch-pathnames(pathnames)
    (assertion(<=  2 (! 0(list-length pathnames))))
    (labels((REC(components)
	      (if(SAME-DIRECTORY-P components)
		(REC (map-into components #'cdr components))
		(DO-RETURN components)))
	    (SAME-DIRECTORY-P(components)
	      (let((elt(caar components)))
		(when elt
		  (loop :for component :in (cdr components)
			:always (equal elt (car component))))))
	    (DO-RETURN(components)
	      (map-into components
			(lambda($component $pathname)
			  (make-pathname :directory(CANONICALIZE $component)
					 :name (pathname-name $pathname)
					 :type (pathname-type $pathname)))
			components
			pathnames))
	    (CANONICALIZE(component)
	      (if(symbolp (car component))
		component
		(cons :relative component)))
	    )
      (REC (mapcar (lambda($pathname)
		       (! 1 (check-type $pathname pathname))
		       (pathname-directory $pathname))
		   pathnames)))))

(Prototype prompt(list &rest T)string)
(defun prompt(choices &rest format-args)
  (with-output-to-string(*standard-output*)
    (loop :for n :upfrom 0
	  :for choice :in choices
	  :do (format t "~%~3D: ~A" n choice)
	  :finally (apply #'format t format-args))))
