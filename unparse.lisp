(in-package #:first-class-lambda-lists)

(defgeneric fcll:unparse (object))

(defmethod print-object ((parameter parameter) stream)
  (print-unreadable-object (parameter stream :type t)
    (prin1 (fcll:unparse parameter) stream)))

(defmethod print-object ((section fcll:standard-lambda-list-section) stream)
  (print-unreadable-object (section stream :type t)
    (format stream "~S~{ ~S~}"
            (defsys:name (fcll:lambda-list-keyword section))
            (mapcar #'fcll:unparse (parameters section)))))


;;; Parameters

(defun %unparse-recursable-variable (variable-or-lambda-list)
  (etypecase variable-or-lambda-list
    (symbol variable-or-lambda-list)
    (fcll:lambda-list (fcll:unparse variable-or-lambda-list))))

(defmethod fcll:unparse ((parameter simple-parameter))
  (variable parameter))

(defmethod fcll:unparse ((parameter required-parameter))
  (%unparse-recursable-variable (variable parameter)))

(defmethod fcll:unparse ((parameter specializable-parameter))
  (let ((variable (variable parameter))
        (specializer (specializer parameter)))
    (if (and (eq specializer t) (not (typep variable 'fcll:lambda-list)))
        variable
        (list (%unparse-recursable-variable variable) specializer))))

(defmethod fcll:unparse ((parameter optional-parameter))
  (let* ((variable (variable parameter))
         (initform (initform parameter))
         (initform-not-default-p (not (%default-initform initform)))
         (suppliedp-variable (suppliedp-variable parameter))
         (initform-or-suppliedp (or initform-not-default-p suppliedp-variable)))
    (if (or initform-or-suppliedp (typep variable 'fcll:lambda-list))
        `(,(%unparse-recursable-variable variable)
          ,@(when initform-or-suppliedp (list initform))
          ,@(when suppliedp-variable (list suppliedp-variable)))
        variable)))

(defmethod fcll:unparse ((parameter optional-no-defaulting-parameter))
  (variable parameter))

(defmethod fcll:unparse ((parameter key-parameter))
  (let ((variable (variable parameter))
        (keyword-name (keyword-name parameter))
        (initform (initform parameter))
        (suppliedp-variable (suppliedp-variable parameter)))
    (let ((custom-keyword-name-p (or (not (keywordp keyword-name))
                                     (string/= (symbol-name keyword-name)
                                               (symbol-name variable))))
          (initform-not-default-p (not (%default-initform initform)))
          (recursivep :untested))
      ;; Do the "expensive" TYPEP only once, and only as a last resort.
      (if (or custom-keyword-name-p initform-not-default-p suppliedp-variable
              (setf recursivep (typep variable 'fcll:lambda-list)))
          `(,(if (or custom-keyword-name-p (if (eq recursivep :untested)
                                               (typep variable 'fcll:lambda-list)
                                               recursivep))
                 (list keyword-name (%unparse-recursable-variable variable))
                 variable)
             ,@(when (or initform-not-default-p suppliedp-variable) (list initform))
             ,@(when suppliedp-variable (list suppliedp-variable)))
          variable))))

(defmethod fcll:unparse ((parameter key-no-defaulting-parameter))
  (let ((variable (variable parameter))
        (keyword-name (keyword-name parameter)))
    (let ((custom-keyword-name-p (or (not (keywordp keyword-name))
                                     (string/= (symbol-name keyword-name)
                                               (symbol-name variable)))))
      (if custom-keyword-name-p
          `((,keyword-name ,variable))
          variable))))

(defmethod fcll:unparse ((parameter aux-parameter))
  (let ((variable (variable parameter))
        (initform (initform parameter)))
    (if initform
        `(,variable ,initform)
        variable)))


;;; Sections

(defmethod fcll:unparse ((section fcll:standard-lambda-list-section))
  (let ((introducer (introducer (fcll:lambda-list-keyword section)))
        (parameters (mapcar #'fcll:unparse (parameters section))))
    (if introducer
        (cons introducer parameters)
        parameters)))

(defmethod fcll:unparse ((section standard-&key-section))
  (if (allow-other-keys-p section)
      (append (call-next-method) '(&allow-other-keys))
      (call-next-method)))


;;; Lambda lists

(defmethod fcll:unparse ((lambda-list fcll:standard-lambda-list))
  (%call-with-root-lambda-list-setup
   lambda-list
   (lambda ()
     (reduce #'append
             (%sections lambda-list)
             :from-end t
             :key #'fcll:unparse
             :initial-value nil))))
