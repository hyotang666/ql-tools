(defpackage :ql-tools.update
  (:use :cl :ql-dist)
  (:export #:update))

(in-package :ql-tools.update)

(defun update ()
  (let* ((dist (find-dist "quicklisp")) (new (available-update dist)))
    (cond
     (new (show-update-report dist new)
      (when (ql-util:press-enter-to-continue)
        (update-in-place dist new))
      (install-dependencies dist))
     ((not (subscribedp dist))
      (format t "~&You are not subscribed to ~S." (name dist)))
     (t
      (format t "~&You already have the latest version of ~S: ~A.~%"
              (name dist) (version dist))))))

(defun install-dependencies (dist)
  (map nil #'install-required-systems (installed-systems dist)))

(defun install-required-systems (system)
  (dolist (required (required-systems system))
    (let ((s (system required)))
      (when (and s (not (asdf:find-system (name s) nil)))
        (format t "~&~A require ~A.~%" (name system) required)
        (handler-bind ((error
                         (lambda (c)
                           (declare (ignore c))
                           (when (find-restart 'asdf/action:accept)
                             (invoke-restart 'asdf/action:accept)))))
          (ql:quickload required))))))
