(defpackage :ql-tools.installed
  (:use :cl)
  (:export #:installed))

(in-package :ql-tools.installed)

(defvar *alt-descriptions* (make-hash-table :test #'equal))

(defun installed (&optional regex)
  (loop :for (name author description) :in (informations)
        :when (or (and regex (ppcre:scan regex description)) (null regex))
          :do (format t "~&~%~A~@[ by ~A~]~%~A"
                      (cl-ansi-text:green (string-upcase name)) author
                      description)))

(defun informations ()
  (loop :for release :in (ql-tools.utility:all-releases)
        :collect (information release)))

(declaim
 (ftype (function (ql-dist:release)
         (values (cons string (cons (or null cons string) (cons string null)))
                 &optional))
        information))

(defun information (release)
  (let ((system (system release)))
    (if system
        (list (asdf:coerce-name system)
              (ignore-errors (asdf:system-author system))
              (or (ignore-errors (asdf:system-description system))
                  "No description"))
        (list (ql-dist:name release) nil
              ";; WARNING: Could not detect correct system."))))

(declaim
 (ftype (function (ql-dist:release) (values (or null asdf:system) &optional))
        system))

(defun system (release)
  (or (asdf:find-system (ql-dist:name release) nil)
      (let ((system-files
             (remove "test" (ql-dist:system-files release) :test #'search)))
        (when (and system-files (null (cdr system-files)))
          (asdf:find-system (uiop:split-name-type (car system-files)) nil)))))

;;; Some systems does not have its own asdf::description.
;;; Here, we retreave dessctions from its README or elsewhere.

(defmacro def (key description)
  `(setf (gethash ,key *alt-descriptions*) ,description))

#|
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
(def "cl-html-diff" "Generating human readable diffs of HTML documents, using HTML.")
(def "rfc2109" "Includes in whole RFC2109, and in part RFC2608 and the Netscape cookie spec")
(def "rfc2388" "Internet Media Type, multipart/form-data.")
(def "rfc2388-binary" "Parsing multipart/form-data data streams.")
(def "rucksack" "Fairly transparent persistence mechanism for conses, vectors and CLOS objects.")
(def "atdoc" "generates documentation for Common Lisp packages")
(def "conspack" "Inspired by MessagePack, and by the general lack of features among prominent serial/wire formats.")
(def "cl-gss" "Interface to GSSAPI which provides a standard API to authentication services")
(def "frpc" "EXtensible Data Representation (XDR) serializer and a flexible Remote Procedure Call (RPC) framework")
(def "generic-comparability" "Interface for the EQUALS function")
(def "lfarm" "Distributing work across machines using the lparallel API.")
(def "xmls" "Small, Simple, non-validating xml parser.")
|#

