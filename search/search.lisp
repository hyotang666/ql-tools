(defpackage :ql-tools.search
  (:use :cl)
  (:shadow search)
  (:export #:search))

(in-package :ql-tools.search)

(defstruct system name description)

;;;; SEARCH

(defun search (rexp)
  (dolist (system (matched-systems rexp))
    (format t "~&~:@(~A~) : ~A" (system-name system)
            (system-description system))))

(defun matched-systems (rexp)
  (loop :for system :in (all-dists-systems)
        :for description = (match-description system rexp)
        :when description
          :collect (make-system :name (ql-dist:name system)
                                :description description)))

(defun all-dists-systems ()
  (loop :for dist :in (ql-dist:all-dists)
        :append (ql-dist:provided-systems dist)))

(defun match-description (system rexp)
  (if (ql-dist:installedp system)
      (match-asdf-description system rexp)
      (scan system #'ql-dist:name rexp "not installed")))

(defun match-asdf-description (system rexp)
  (let ((system
         (ignore-errors ; e.g. missing-dependencies.
                        (asdf:find-system (ql-dist:name system) nil))))
    (when system
      (or (scan system #'asdf:system-description rexp)
          (scan system #'asdf:system-long-description rexp)
          (and (scan system #'asdf:coerce-name rexp)
               (asdf:system-description system))))))

(defun scan (object string-generator rexp &optional alter)
  (let ((string (funcall string-generator object)))
    (when string
      (with-input-from-string (in string)
        (loop :for line = (read-line in nil)
              :while line
              :when (ppcre:scan rexp line)
                :return (or alter line))))))
