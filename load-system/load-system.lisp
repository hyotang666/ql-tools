(defpackage :ql-tools.load-system
  (:use :cl)
  (:import-from :ql-tools.utility
                #:system-source-files
                #:system-name=
                #:any-version-of
                #:mismatch-pathnames
                #:prompt)
  (:export #:load-system))

(in-package :ql-tools.load-system)

;;;; develop utility

(define-symbol-macro u (asdf:load-system :ql-tools.load-system))

(defun load-system (system)
  (let ((asdf:*system-definition-search-functions*
         (cons (make-searcher) asdf:*system-definition-search-functions*)))
    (asdf:load-system system)))

;;;; We treat sub system as monolithic.
;;;; E.g. mcclim has many sub system as module.(mcclim-core, clim, etc...)

(let ((monolith (make-hash-table :test #'equal)))
  (defun make-searcher ()
    (lambda (system)
      (labels ((log-it (arg)
                 (print arg)
                 (force-output))
               (already-registered-one (system)
                 (when system
                   (asdf:system-source-file system)))
               (do-search (systems)
                 (let ((length (list-length systems)))
                   (when systems
                     (return-it
                       (if (have-only-one? length)
                           (car systems)
                           (ask-user `(mod ,length) systems))))))
               (have-only-one? (length)
                 (not (< 1 length)))
               (return-it (directory)
                 (loop :for asd :in (system-source-files directory)
                       :if (system-name= system asd)
                         :collect asd :into result
                       :do (setf (gethash (pathname-name asd) monolith) asd)
                       :finally (return (car result))))
               (ask-user (type systems)
                 (nth
                   (prompt-for:prompt-for type
                                          (prompt (mismatch-pathnames systems)
                                                  "~&Which version do you load?~%Type number >> "))
                   systems)))
        ;; one .asd file may define some systems.
        ;; in such case, we should treat it as atomic.
        (log-it system)
        (or (already-registered-one (asdf:registered-system system))
            (gethash (string-downcase system) monolith)
            (do-search (any-version-of system)))))))
