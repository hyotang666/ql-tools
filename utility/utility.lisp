(defpackage :ql-tools.utility
  (:use :cl :prompt-for)
  (:export ;;;; dist
           #:installed-systems
           ;;;; releases
           #:all-releases
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
           ;;;; condition name/function
           #:not-resolve))

(in-package :ql-tools.utility)

(deftype pathnames () "List of pathnames" 'list)

(deftype system-designator () '(or keyword string))

(declaim
 (ftype (function (ql-dist:dist) (values pathnames &optional))
        installed-systems))

(defun installed-systems (dist)
  "Return \"dists/DIST/software/*\""
  (uiop:subdirectories (ql-dist:relative-to dist "software/")))

(deftype pathname-designator () `(or pathname string))

(declaim
 (ftype (function (pathname-designator) (values (or null integer) &optional))
        version<=pathname))

(defun version<=pathname (pathname)
  (labels ((retrieve-if (predicate string)
             (remove-if (complement predicate) string)))
    (values ; to discard second value.
            (parse-integer
              (retrieve-if #'digit-char-p
                           (bottom-directory-namestring pathname))))))

(declaim
 (ftype (function (pathname-designator) (values string &optional))
        bottom-directory-namestring))

(defun bottom-directory-namestring (pathname)
  (car (last (pathname-directory pathname))))

(declaim (ftype (function (*) (values string &optional)) coerce-name))

(let ((cache (make-hash-table :test #'equal)))
  (defun coerce-name (thing)
    (labels ((do-pathname (pathname)
               (if (asd-p pathname)
                   (pathname-name pathname)
                   (ensure-cache pathname)))
             (ensure-cache (directory)
               (or (gethash directory cache)
                   (setf (gethash directory cache) (do-directory directory))))
             (do-directory (directory)
               (pathname-name (system-source-file directory))))
      (etypecase thing
        ((or symbol string) (string-downcase thing))
        (asdf:system (asdf:coerce-name thing))
        (ql-dist:system (ql-dist:name thing))
        (pathname (do-pathname thing))))))

(declaim (ftype (function (*) (values pathname &optional)) system-source-file))

(define-condition not-resolve (cell-error) ()
  (:report
   (lambda (condition stream)
     (format stream "Can not resolve asd file for ~S."
             (cell-error-name condition)))))

(defun not-resolve (name) (error 'not-resolve :name name))

(let ((cache (make-hash-table :test #'equal)))
  (defun system-source-file (thing)
    (labels ((do-pathname (pathname)
               (if (asd-p pathname)
                   pathname
                   (do-directory pathname
                                 (bottom-directory-namestring pathname))))
             (do-directory (directory bottom)
               (or (gethash bottom cache)
                   (setf (gethash bottom cache) (do-search directory bottom))))
             (do-search (root bottom)
               (let ((result (gensym "RESULT")))
                 (catch result
                   (let ((asds (may-collect-asd root bottom result)))
                     (if (only-one-element? asds)
                         (throw result (car asds))
                         (finally asds))))))
             (may-collect-asd (root bottom result)
               (let (acc)
                 (flet ((searcher (directory)
                          (dolist
                              (asd (uiop:directory-files directory "*.asd"))
                            (if (search (pathname-name asd) bottom)
                                (throw result asd) ; stop to collect.
                                (push asd acc)))))
                   (uiop:collect-sub*directories root t t #'searcher)
                   acc)))
             (only-one-element? (list)
               (typep list `(cons t null)))
             (finally (asds)
               (flet ((reader ()
                        `(,(prompt-for `(mod ,(list-length asds))
                                       (prompt (mismatch-pathnames asds)
                                               "~%Type number.>> ")))))
                 (restart-case (not-resolve thing)
                   (select (number)
                       :report "Select target asd file."
                       :interactive reader
                     (nth number asds))))))
      (etypecase thing
        ((or symbol string)
         (system-source-file (asdf:find-system (string-downcase thing))))
        (asdf:system (asdf:system-source-file thing))
        (ql-dist:system (system-source-file (ql-dist:name thing)))
        (pathname (do-pathname thing))))))

(declaim (ftype (function (pathname) (values boolean &optional)) asd-p))

(defun asd-p (pathname) (string= "asd" (pathname-type pathname)))

(declaim
 (ftype (function (system-designator) (values pathnames &optional))
        any-version-of))

(let ((cache (make-hash-table :test #'equal)))
  (defun any-version-of (system)
    (let ((system (string-downcase system)))
      (labels ((diverge (system)
                 ;; Newer to older order.
                 (sort
                   (if system
                       (do-main system)
                       (fallback))
                   (complement #'<)
                   :key #'version<=pathname))
               (do-main (system)
                 (loop :for pathname
                            :in (installed-systems (ql-dist:dist system))
                       :when (system-name= system pathname)
                         :collect pathname))
               (fallback ()
                 (or (copy-list (gethash system cache)) (search-system)))
               (search-system ()
                 (warn-to-user)
                 (uiop:while-collecting (acc)
                   (dolist (root (installed-systems (ql-dist:dist :quicklisp)))
                     (dolist (asd (system-source-files root))
                       (cache-it asd)
                       (when (system-name= system asd)
                         (acc asd))))))
               (warn-to-user ()
                 (warn "~S: fall back to search ~S.~%This may take minutes."
                       'any-version-of system))
               (cache-it (asd)
                 (pushnew asd (gethash (pathname-name asd) cache) :test
                          #'equal)))
        (or (uiop:ensure-list (ql:local-projects-searcher system))
            (diverge (ql-dist:find-system system)))))))

(declaim
 (ftype (function (*) (values pathnames &optional)) system-source-files))

(defun system-source-files (thing)
  (etypecase thing
    (ql-dist:system
     (let ((system-root (ql-dist:base-directory (ql-dist:release thing))))
       (loop :for subpath :in (ql-dist:system-files (ql-dist:release thing))
             :collect (uiop:merge-pathnames* subpath system-root))))
    (symbol (system-source-files (ql-dist:find-system thing)))
    (pathname
     (uiop:while-collecting (asd)
       (uiop:collect-sub*directories thing t t
                                     (lambda (directory)
                                       (dolist
                                           (file
                                            (uiop:directory-files directory
                                                                  "*.asd"))
                                         (asd file))))))))

(declaim (ftype (function (* *) (values boolean &optional)) system-name=))

(defun system-name= (thing1 thing2)
  (string= (coerce-name thing1) (coerce-name thing2)))

(declaim
 (ftype (function (pathnames) (values pathnames &optional)) mismatch-pathnames))

(macrolet ((assertion (form)
             `(assert ,form ()
                'simple-error :format-control "~S: argument PATHNAMES must have at least 2 elements, but ~S."
                              :format-arguments `(mismatch-pathnames
                                                   ,pathnames)))
           (! (tag form)
             `(handler-case ,form
                ,(ecase tag
                   (0
                    `(error nil
                            (error 'simple-type-error
                                   :format-control "~S: argument PATHNAMES must proper list, but ~S."
                                   :format-arguments (list 'mismatch-pathnames
                                                           pathnames))))
                   (1
                    `(error nil
                            (let ((datum (find-if-not #'pathnamep pathnames)))
                              (error 'simple-type-error
                                     :format-control "~S: argument PATHNAMES's every element must pathname, but ~S.~%Especially ~S."
                                     :format-arguments (list
                                                         'mismatch-pathnames
                                                         pathnames datum)
                                     :expected-type 'pathname
                                     :datum datum))))))))
  (defun mismatch-pathnames (pathnames)
    (assertion (<= 2 (! 0 (list-length pathnames))))
    (labels ((rec (components)
               (if (same-directory-p components)
                   (rec (map-into components #'cdr components))
                   (do-return components)))
             (same-directory-p (components)
               (let ((elt (caar components)))
                 (when elt
                   (loop :for component :in (cdr components)
                         :always (equal elt (car component))))))
             (do-return (components)
               (map-into components
                         (lambda ($component $pathname)
                           (make-pathname :directory (canonicalize $component)
                                          :name (pathname-name $pathname)
                                          :type (pathname-type $pathname)))
                         components pathnames))
             (canonicalize (component)
               (if (symbolp (car component))
                   component
                   (cons :relative component))))
      (rec
        (mapcar
          (lambda ($pathname)
            (! 1 (check-type $pathname pathname))
            (pathname-directory $pathname))
          pathnames)))))

(declaim (ftype (function (list &rest t) (values string &optional)) prompt))

(defun prompt (choices &rest format-args)
  (with-output-to-string (*standard-output*)
    (loop :for n :upfrom 0
          :for choice :in choices
          :do (format t "~%~3D: ~A" n choice)
          :finally (apply #'format t format-args))))

(declaim (ftype (function nil (values list &optional)) all-releases))

(defun all-releases ()
  (loop :for dist :in (ql-dist:all-dists)
        :append (ql-dist:installed-releases dist)))
