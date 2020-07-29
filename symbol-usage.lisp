(defpackage :symbol-usage
  (:use :cl)
  (:export #:analyze))

(in-package :symbol-usage)

;;;; In order to debug, return value is hash-table.

(defun analyze (&optional (system :cl))
  (labels ((file ()
             (uiop:merge-pathnames* (format nil "~(~A~)-symbol-usage" system)
                                    (user-homedir-pathname)))
           (analyzed (table)
             (let ((*package* (ensure-package (ensure-system system))))
               (dolist (s (target-systems system) table)
                 (analyze-system s table))))
           (ensure-system (system)
             (if (eq :cl system)
                 :cl-user
                 system))
           (output (table file)
             (with-open-file (*standard-output* file :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
               (print-result table))
             table))
    (output (analyzed (table-of system)) (file))))

(declaim (ftype (function (keyword) (values list &optional)) target-systems))

(defun target-systems (system)
  (labels ((installed-systems ()
             (delete-test
               (ql-dist:installed-systems (ql-dist:dist :quicklisp))))
           (delete-test (systems &optional acc)
             (if (endp systems)
                 acc ; ignore order.
                 (body (ql-dist:name (car systems)) (cdr systems) acc)))
           (body (system rest acc)
             (if (test-system-p system)
                 (delete-test rest acc)
                 (delete-test rest (push system acc))))
           (test-system-p (system)
             (search "test" system)))
    (if (eq :cl system)
        (installed-systems)
        (remove-if #'test-system-p
                   (ql:who-depends-on (asdf:coerce-name system))))))

(defun ensure-package (system)
  ;; FIXME: some system has different package-name.
  (or (find-package system)
      (find-package (car (ql:quickload system)))
      (error
        "Package ~S is not found.~%Typo? or system ~:*~S may have different package name."
        system)))

;;;; In order to debug `ANALYZE-SYSTEM` and `PRINT-RESULT`, this is global.

(defun table-of (&optional (system :cl))
  (let ((table (make-hash-table :test #'eq)) (package (ensure-package system)))
    (do-external-symbols (s package table) (setf (gethash s table) 0))))

(declaim
 (ftype (function (system-name hash-table) (values hash-table &optional))
        analyze-system))

(defun analyze-system (system table)
  (labels ((system-components (system)
             (handler-case (asdf:required-components (asdf:find-system system))
               ((or asdf:missing-component asdf:missing-dependency) (c)
                 (ensure-warn c))
               (named-readtables:readtable-does-not-exist ())
               (error (c)
                 (let ((restart (find-restart 'skip)))
                   (if restart
                       (invoke-restart restart)
                       (ensure-warn c))))))
           (ensure-warn (thing)
             (handler-case (warn (princ-to-string thing))
               (error ()
                 (warn (prin1-to-string thing)))))
           (cl-source-file-p (component)
             (typep component 'asdf:cl-source-file)))
    (dolist (component (system-components system) table)
      (when (cl-source-file-p component)
        (let ((null-package:*only-junk-p* t))
          (analyze-file (asdf:component-pathname component) table))))))

(defparameter *debug* nil)

(defparameter *verbose* nil)

(defun analyze-file (pathname table)
  (print pathname)
  (labels ((read-sexp (sexp-notation)
             (setf *debug* sexp-notation)
             (handler-case
                 (let ((*read-eval* nil))
                   (let ((sexp (read-from-string sexp-notation nil)))
                     #+sbcl
                     (setf sexp
                             (trestrul:nmapleaf
                               (lambda (x)
                                 (if (sb-int:comma-p x)
                                     (sb-int:comma-expr x)
                                     x))
                               sexp))
                     sexp))
               (reader-error (c)
                 (ensure-cerror c))
               (package-error ()
                 (re-read sexp-notation))
               (error (c)
                 (format *error-output* "~A~&~S" c pathname))))
           (ensure-cerror (c)
             (let ((format-control
                    (handler-case (format nil (princ-to-string c))
                      (error ()
                        (prin1-to-string c)))))
               (when *verbose*
                 (cerror format-control "Treat as NIL."))))
           (re-read (string)
             (with-input-from-string (s string)
               (handler-case (null-package:read-with-null-package s)
                 (error (c)
                   (setf *debug* string) (ensure-cerror c)))))
           (analyze-sexp (sexp)
             ;; FIXME: Currently we never found `NIL`, because it is node.
             (when (listp sexp)
               (trestrul:dotree (leaf sexp)
                 (when (gethash leaf table)
                   (incf (gethash leaf table)))))))
    (with-open-file (s pathname :if-does-not-exist nil)
      (when s
        (loop :for sexp
                   = (let ((read-as-string:*muffle-reader-error* t))
                       (read-as-string:read-as-string s nil))
              :while sexp
              :when (string= "" sexp)
                :do (loop-finish)
              :else
                :do (analyze-sexp (read-sexp sexp))))
      table)))

(defun print-result (table)
  (labels ((rec (list temp rank)
             (if (endp list)
                 (epilogue temp rank)
                 (body (car list) (cdr list) temp rank)))
           (epilogue (temp rank)
             (when temp
               (format t "~3@S | ~4@S |~{~A~^ ~}~%" rank (cdar temp)
                       (mapcar #'car temp))))
           (body (first rest temp rank)
             (if (same-rank-p first (car rest))
                 (rec rest (push first temp) rank)
                 (do-print rank first temp rest)))
           (same-rank-p (cons1 cons2)
             (eql (cdr cons1) (cdr cons2)))
           (do-print (rank first temp rest)
             (format t "~3@S | ~4@S |~{~A~^ ~}~%" rank (cdr first)
                     (mapcar #'car (push first temp)))
             (when (zerop (mod rank 10))
               (format t "~80,,,'-A~%" #\-))
             (rec rest nil (1+ rank)))
           (sort-result (table)
             (sort (alexandria:hash-table-alist table) #'> :key #'cdr)))
    (rec (sort-result table) nil 1)))

;;;; for dubug use

#++
(defun system-source-files (system)
  (labels ((rec (components &optional acc)
             (if (endp components)
                 acc
                 (body (car components) (cdr components) acc)))
           (body (component rest acc)
             (if (typep component 'asdf:cl-source-file)
                 (rec rest (push (asdf:component-pathname component) acc))
                 (rec rest acc))))
    (rec (asdf:required-components (asdf:find-system system)))))
