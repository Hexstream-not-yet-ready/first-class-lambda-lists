(in-package #:first-class-lambda-lists)

(defclass expansion-environment ()
  ())

(defgeneric expand (object expansion-env form))

(defun %expand-lambda-list (lambda-list expression form)
  (let ((expansion-env (make-instance 'standard-expansion-environment)))
    `(let ((,(tail-var expansion-env) ,expression))
       ,(expand lambda-list expansion-env form))))


(defclass parameter () ())

(defgeneric fcll:unparse (object))

(defmethod print-object ((parameter parameter) stream)
  (print-unreadable-object (parameter stream :type t)
    (prin1 (fcll:unparse parameter) stream)))

(defclass parameter-variable-mixin ()
  ((%variable :initarg :variable
              :reader variable)))

(defclass parameter-simple-variable-mixin (parameter-variable-mixin)
  ((%variable :type symbol)))

(defclass parameter-recursable-variable-mixin (parameter-variable-mixin)
  ((%variable :type (or symbol fcll:lambda-list))))

(defvar *parse-recursable-variable*)

(defparameter *default-initform* nil)

(defun %default-initform (&optional (to-test nil testp))
  (if testp
      (equal to-test *default-initform*)
      *default-initform*))

(defun %parse-recursable-variable (variable)
  (etypecase variable
    (list (funcall *parse-recursable-variable* variable))
    (symbol variable)))

(defun %unparse-recursable-variable (variable-or-lambda-list)
  (etypecase variable-or-lambda-list
    (symbol variable-or-lambda-list)
    (fcll:lambda-list (unparse variable-or-lambda-list))))

(defclass parameter-initform-mixin ()
  ((%initform :initarg :initform
              :reader initform
              :type t)))

(defclass parameter-suppliedp-variable-mixin ()
  ((%suppliedp-variable :initarg :suppliedp-variable
                        :reader suppliedp-variable
                        :type symbol
                        :initform nil)))

(defclass simple-parameter (parameter parameter-simple-variable-mixin)
  ())

(defun %parse-simple-parameter (parameter)
  (check-type parameter symbol)
  (make-instance 'simple-parameter :variable parameter))

(defun %parse-simple-recursable-parameter (parameter)
  (make-instance 'simple-parameter :variable (%parse-recursable-variable parameter)))

(defmethod fcll:unparse ((parameter simple-parameter))
  (variable parameter))

(defclass required-parameter (parameter parameter-recursable-variable-mixin)
  ())

(defun %parse-required-parameter (parameter)
  (make-instance 'required-parameter :variable (%parse-recursable-variable parameter)))

(defmethod fcll:unparse ((parameter required-parameter))
  (%unparse-recursable-variable (variable parameter)))

(defmethod expand ((parameter required-parameter) (expansion-env expansion-environment) form)
  (let ((tail-var (tail-var expansion-env))
        (var-or-lambda-list (variable parameter)))
    (etypecase var-or-lambda-list
      (symbol (let ((variable var-or-lambda-list))
                `(let ((,variable (prog1 (car ,tail-var)
                                    (setf ,tail-var (cdr ,tail-var)))))
                   ,form)))
      (fcll:lambda-list (let ((lambda-list var-or-lambda-list))
                          (%expand-lambda-list lambda-list
                                               `(prog1 (car ,tail-var)
                                                  (setf ,tail-var (cdr ,tail-var)))
                                               form))))))

(defclass specializable-parameter (required-parameter)
  ((%specializer :initarg :specializer
                 :reader specializer
                 :initform t)))

(defun %parse-specializable-parameter (parameter)
  (multiple-value-bind (variable specializer)
      (if (symbolp parameter)
          (values (or parameter (%parse-recursable-variable parameter))
                  t)
          (destructuring-bind (variable &optional (specializer t))
              parameter
            (values (%parse-recursable-variable variable) specializer)))
    (make-instance 'specializable-parameter
                   :variable variable
                   :specializer specializer)))

(defmethod fcll:unparse ((parameter specializable-parameter))
  (let ((variable (variable parameter))
        (specializer (specializer parameter)))
    (if (and (eq specializer t) (not (typep variable 'fcll:lambda-list)))
        variable
        (list (%unparse-recursable-variable variable) specializer))))

(defclass optional-parameter (parameter parameter-recursable-variable-mixin parameter-initform-mixin parameter-suppliedp-variable-mixin)
  ())

(defun %parse-optional-parameter (parameter)
  (multiple-value-bind (variable initform suppliedp-variable)
      (if (symbolp parameter)
          (values (or parameter (%parse-recursable-variable parameter))
                  (%default-initform)
                  nil)
          (destructuring-bind (variable &optional (initform (%default-initform)) suppliedp-variable)
              parameter
            (values (%parse-recursable-variable variable)
                    initform
                    suppliedp-variable)))
    (make-instance 'optional-parameter
                   :variable variable
                   :initform initform
                   :suppliedp-variable suppliedp-variable)))

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

(defmethod expand ((parameter optional-parameter) (expansion-env expansion-environment) form)
  (let ((tail-var (tail-var expansion-env))
        (var-or-lambda-list (variable parameter))
        (initform (initform parameter))
        (suppliedp-variable (suppliedp-variable parameter)))
    (etypecase var-or-lambda-list
      (symbol (let ((variable var-or-lambda-list))
                `(let (,@(when suppliedp-variable
                               (list `(,suppliedp-variable (not (null ,tail-var)))))
                       (,variable (if ,tail-var
                                      (prog1 (car ,tail-var)
                                        (setf ,tail-var (cdr ,tail-var)))
                                      ,initform)))
                   ,form)))
      (fcll:lambda-list
       (let ((lambda-list var-or-lambda-list))
         (%expand-lambda-list lambda-list
                              `(if ,tail-var
                                   ,(if suppliedp-variable
                                        `(car ,tail-var)
                                        `(prog1 (car ,tail-var)
                                           (setf ,tail-var (cdr ,tail-var))))
                                   ,initform)
                              (if suppliedp-variable
                                  `(let ((,suppliedp-variable (prog1 (not (null ,tail-var))
                                                                (setf ,tail-var (cdr ,tail-var)))))
                                     ,form)
                                  form)))))))

(defclass optional-no-defaulting-parameter (parameter parameter-simple-variable-mixin)
  ())

(defun %parse-optional-no-defaulting-parameter (parameter)
  (make-instance 'optional-no-defaulting-parameter
                 :variable (etypecase parameter
                             (symbol parameter)
                             ((cons symbol null)
                              (car parameter)))))

(defmethod fcll:unparse ((parameter optional-no-defaulting-parameter))
  (variable parameter))

(defclass parameter-keyword-name-mixin ()
  ((%keyword-name :initarg :keyword-name
                  :reader keyword-name
                  :type symbol)))

(defmethod shared-initialize :after ((instance parameter-keyword-name-mixin) slot-names &key)
  (declare (ignore slot-names))
  (unless (slot-boundp instance '%keyword-name)
    (setf (slot-value instance '%keyword-name)
          (%keywordize (variable instance)))))

(defclass key-parameter (parameter parameter-recursable-variable-mixin parameter-keyword-name-mixin
                                   parameter-initform-mixin parameter-suppliedp-variable-mixin)
  ())

(defun %keywordize (symbol)
  (intern (symbol-name symbol) '#:keyword))

(defun %parse-key-parameter (parameter)
  (multiple-value-bind (variable initform suppliedp-variable keyword-name)
      (if (symbolp parameter)
          (values (or parameter (%parse-recursable-variable parameter))
                  (%default-initform)
                  nil
                  (%keywordize parameter))
          (destructuring-bind (variable-and/or-keyword-name &optional (initform (%default-initform)) suppliedp-variable)
              parameter
            (multiple-value-bind (variable keyword-name)
                (if (symbolp variable-and/or-keyword-name)
                    (values (or variable-and/or-keyword-name
                                (%parse-recursable-variable variable-and/or-keyword-name))
                            (%keywordize variable-and/or-keyword-name))
                    (destructuring-bind (keyword-name variable) variable-and/or-keyword-name
                      (values (%parse-recursable-variable variable) keyword-name)))
              (values variable initform suppliedp-variable keyword-name))))
    (make-instance 'key-parameter
                   :variable variable
                   :initform initform
                   :suppliedp-variable suppliedp-variable
                   :keyword-name keyword-name)))

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

(defclass key-no-defaulting-parameter (parameter parameter-simple-variable-mixin parameter-keyword-name-mixin)
  ())

(defun %parse-key-no-defaulting-parameter (parameter)
  (multiple-value-bind (variable keyword-name)
      (if (symbolp parameter)
          (values parameter (%keywordize parameter))
          (destructuring-bind (variable-and/or-keyword-name)
              parameter
            (multiple-value-bind (variable keyword-name)
                (if (symbolp variable-and/or-keyword-name)
                    (values variable-and/or-keyword-name
                            (%keywordize variable-and/or-keyword-name))
                    (destructuring-bind (keyword-name variable) variable-and/or-keyword-name
                      (values variable keyword-name)))
              (values variable keyword-name))))
    (make-instance 'key-no-defaulting-parameter
                   :variable variable
                   :keyword-name keyword-name)))

(defmethod fcll:unparse ((parameter key-no-defaulting-parameter))
  (let ((variable (variable parameter))
        (keyword-name (keyword-name parameter)))
    (let ((custom-keyword-name-p (or (not (keywordp keyword-name))
                                     (string/= (symbol-name keyword-name)
                                               (symbol-name variable)))))
      (if custom-keyword-name-p
          `((,keyword-name ,variable))
          variable))))

(defclass aux-parameter (parameter parameter-simple-variable-mixin parameter-initform-mixin)
  ())

(defun %parse-aux-parameter (parameter)
  (multiple-value-bind (variable initform)
      (if (symbolp parameter)
          (values parameter nil)
          (destructuring-bind (variable &optional initform)
              parameter
            (values variable initform)))
    (make-instance 'aux-parameter
                   :variable variable
                   :initform initform)))

(defmethod fcll:unparse ((parameter aux-parameter))
  (let ((variable (variable parameter))
        (initform (initform parameter)))
    (if initform
        `(,variable ,initform)
        variable)))

(defmethod expand ((parameter aux-parameter) (expansion-env expansion-environment) form)
  (let ((variable (variable parameter))
        (initform (initform parameter)))
    `(let ((,variable ,initform))
       ,form)))
