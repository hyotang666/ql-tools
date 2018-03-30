(defpackage :ql-tools.installed(:use :cl :ql-dist)
  (:export #:installed))
(in-package :ql-tools.installed)

(defvar *alt-descriptions* (make-hash-table :test #'equal))

(defun installed()
  (map nil #'funcall (printers (installed-releases(dist "quicklisp")))))

(defun printers(releases)
  (mapcar (lambda(release)
	    (let*((*standard-output*(make-broadcast-stream))
		  (*error-output*(make-broadcast-stream))
		  (name(name release))
		  (system(asdf:find-system name nil)))
	      (lambda()
		(format t "~&~A - ~A"
			name
			(if(not system)
			  "Not found"
			  ;; To override useless description, GETHASH first.
			  (or (gethash name *alt-descriptions*)
			      (asdf::component-description system)
			      "No description"))))))
	  releases))

;;; Some systems does not have its own asdf::description.
;;; Here, we retreave dessctions from its README or elsewhere.

(defmacro def (key description)
  `(setf(gethash ,key *alt-descriptions*)
     ,description))

(def "access" "Unifying data-strucure accessing")
(def "also-alsa" "Basic ALSA bindings. PCM stream.")
(def "cerberus" "Kerberos v5 auehntication protocol implementation")
(def "chunga" "Portable chunked streams")
(def "cl-dbi" "Database independent interface")
(def "cl-interpol" "String interpolation")
(def "cl-marshal" "Lisp data structure serializer / deserializer.")
(def "cl-parser-combinators" "Parsec inspired parser combinator.")
(def "cl-markup" "Modern markup generation library")
(def "cl-tasukete" "Collecting debug information and output somewhere")
(def "cl-utilities" "Collection of Common Lisp utilities")
(def "css-lite" "Generating CSS from an S-exp based syntax")
(def "cxml" "XML parser")
(def "external-program" "Running programs outside the Lisp process")
(def "flexichain" "Protocol to dynamically modify sequence (or chain)")
(def "fxml" "XML parser, fork of CXML")
(def "prove" "Yet another unit testing framework")
(def "quicklisp-slime-helper" "Making it easy to use SLIME from Quicklisp")
(def "slime" "Superior Lisp Interactioin Mode for Emacs")
(def "spatial-trees" "Set of dynamic index data structures")
(def "trivial-utf-8" "UTF-8 encoder / decoder")
(def "closure-html" "HTML parser")
(def "cl-tga" "Facility to load .tga files into OpenGL programs")
(def "cxml-stp" "A data structure for well-formed XML documents")
(def "femlisp" "Finite Element Method framework")
(def "kmrcl" "Collection of utility functions")
(def "js" "Compiler (translator) aimed to enable scripting software written in CL with JS.")
