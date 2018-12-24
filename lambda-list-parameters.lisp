(in-package #:first-class-lambda-lists)

(defclass parameter () ())

(defgeneric fcll:unparse (object))

(defmethod print-object ((parameter parameter) stream)
  (print-unreadable-object (parameter stream :type t)
    (prin1 (fcll:unparse parameter) stream)))

(defclass parameter-variable-mixin ()
  ((%variable :initarg :variable
              :reader variable
              :type symbol)))

(defclass parameter-initform-mixin ()
  ((%initform :initarg :initform
              :reader initform
              :type t)))

(defclass parameter-suppliedp-variable-mixin ()
  ((%suppliedp-variable :initarg :suppliedp-variable
                        :reader suppliedp-variable
                        :type symbol
                        :initform nil)))

(defclass simple-parameter (parameter parameter-variable-mixin)
  ())

(defun %parse-simple-parameter (parameter)
  (check-type parameter symbol)
  (make-instance 'simple-parameter :variable parameter))

(defmethod fcll:unparse ((parameter simple-parameter))
  (variable parameter))

(defclass required-parameter (parameter parameter-variable-mixin)
  ())

(defun %parse-required-parameter (parameter)
  (check-type parameter symbol)
  (make-instance 'required-parameter :variable parameter))

(defmethod fcll:unparse ((parameter required-parameter))
  (variable parameter))

(defclass specializable-parameter (required-parameter)
  ((%specializer :initarg :specializer
                 :reader specializer
                 :initform t)))

(defun %parse-specializable-parameter (parameter)
  (multiple-value-bind (variable specializer)
      (if (symbolp parameter)
          (values parameter t)
          (destructuring-bind (variable &optional (specializer t))
              parameter
            (values variable specializer)))
    (make-instance 'specializable-parameter
                   :variable variable
                   :specializer specializer)))

(defmethod fcll:unparse ((parameter specializable-parameter))
  (let ((variable (variable parameter))
        (specializer (specializer parameter)))
    (if (eq specializer t)
        variable
        (list variable specializer))))

(defclass optional-parameter (parameter parameter-variable-mixin parameter-initform-mixin parameter-suppliedp-variable-mixin)
  ())

(defun %parse-optional-parameter (parameter)
  (multiple-value-bind (variable initform suppliedp-variable)
      (if (symbolp parameter)
          (values parameter nil nil)
          (destructuring-bind (variable &optional initform suppliedp-variable)
              parameter
            (values variable initform suppliedp-variable)))
    (make-instance 'optional-parameter
                   :variable variable
                   :initform initform
                   :suppliedp-variable suppliedp-variable)))

(defmethod fcll:unparse ((parameter optional-parameter))
  (let ((variable (variable parameter))
        (initform (initform parameter))
        (suppliedp-variable (suppliedp-variable parameter)))
    (if (or initform suppliedp-variable)
        `(,variable ,initform ,@(when suppliedp-variable (list suppliedp-variable)))
        variable)))

(defclass optional-no-defaulting-parameter (parameter parameter-variable-mixin)
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
  (unless (slot-boundp instance '%keyword-name)
    (setf (slot-value instance '%keyword-name)
          (%keywordize (variable instance)))))

(defclass key-parameter (parameter parameter-variable-mixin parameter-keyword-name-mixin
                                   parameter-initform-mixin parameter-suppliedp-variable-mixin)
  ())

(defun %keywordize (symbol)
  (intern (symbol-name symbol) '#:keyword))

(defun %parse-key-parameter (parameter)
  (multiple-value-bind (variable initform suppliedp-variable keyword-name)
      (if (symbolp parameter)
          (values parameter nil nil (%keywordize parameter))
          (destructuring-bind (variable-and/or-keyword-name &optional initform suppliedp-variable)
              parameter
            (multiple-value-bind (variable keyword-name)
                (if (symbolp variable-and/or-keyword-name)
                    (values variable-and/or-keyword-name
                            (%keywordize variable-and/or-keyword-name))
                    (destructuring-bind (variable keyword-name) variable-and/or-keyword-name
                      (values variable keyword-name)))
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
                                               (symbol-name variable)))))
      (if (or custom-keyword-name-p initform suppliedp-variable)
          `(,(if custom-keyword-name-p
                 (list keyword-name variable)
                 variable)
             ,initform
             ,@(when suppliedp-variable (list suppliedp-variable)))
          variable))))

(defclass key-no-defaulting-parameter (parameter parameter-variable-mixin parameter-keyword-name-mixin)
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
                    (destructuring-bind (variable keyword-name) variable-and/or-keyword-name
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

(defclass aux-parameter (parameter parameter-variable-mixin parameter-initform-mixin)
  ())

(defun %parse-aux-parameter (parameter)
  (multiple-value-bind (variable initform)
      (if (symbolp parameter)
          (values parameter nil)
          (destructuring-bind (variable &optional initform)
              parameter
            (values variable initform)))
    (make-instance 'optional-parameter
                   :variable variable
                   :initform initform)))

(defmethod fcll:unparse ((parameter aux-parameter))
  (let ((variable (variable parameter))
        (initform (initform parameter)))
    (if initform
        `(,variable ,initform)
        variable)))
