(defpackage #:ql-tools.show
  (:use :cl)
  (:export #:show))

(in-package #:ql-tools.show)

;;;; SHOW

(defun show (name) (format t "~{~&~:(~A~) : ~A~}" (system-properties name)))

(defun system-properties (name)
  (let ((system (find-system name)))
    (when system
      `(:system ,(system system) :version ,(version system) :state
        ,(state system) :licence ,(licence system) :author ,(author system)
        :maintainer ,(maintainer system) :archive-size ,(size system)
        :depends-on ,(depends-on system) :description ,(description system)
        :homepage ,(homepage system)))))

(defun find-system (name)
  (or (ql-dist:find-system name) (asdf:find-system name nil)))

(defmethod ql-dist:name ((system asdf:system)) (asdf:coerce-name system))

(defun system (system) (ql-dist:name system))

(defun state (system)
  (etypecase system
    (ql-dist:system
     (format nil "~@[not ~]installed" (not (ql-dist:installedp system))))
    (asdf:system "Local project")))

(defun with-call-asdf-accessor (system slot)
  (let ((system (asdf:find-system (ql-dist:name system) nil)))
    (when system
      (ignore-errors (funcall slot system)))))

(defun version (system)
  (with-call-asdf-accessor system #'asdf:component-version))

(defun maintainer (system)
  (with-call-asdf-accessor system #'asdf:system-maintainer))

(defun size (system) (ql-dist:archive-size (ql-dist:release system)))

(defmethod ql-dist:release ((system asdf:system)) system)

(defmethod ql-dist:archive-size ((system asdf:system)) "Not archived")

(defun depends-on (system)
  (with-call-asdf-accessor system #'asdf:system-depends-on))

(defun homepage (system)
  (with-call-asdf-accessor system #'asdf:system-homepage))

(defun description (system)
  (let ((short (with-call-asdf-accessor system #'asdf:system-description))
        (long (with-call-asdf-accessor system #'asdf:system-long-description)))
    (if (null short)
        (if (null long)
            nil
            long)
        (if long
            (format nil "~A~2%~A" short long)
            short))))

(defun author (system) (with-call-asdf-accessor system #'asdf:system-author))

(defun licence (system) (with-call-asdf-accessor system #'asdf:system-licence))
