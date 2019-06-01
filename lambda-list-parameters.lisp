(in-package #:first-class-lambda-lists)

(defclass parameter () ())

(defclass parameter-variable-mixin ()
  ((%variable :initarg :variable
              :reader variable)))

(defclass parameter-simple-variable-mixin (parameter-variable-mixin)
  ((%variable :type symbol)))

(defclass parameter-recursable-variable-mixin (parameter-variable-mixin)
  ((%variable :type (or symbol fcll:lambda-list))))

(defparameter *default-initform* nil)

(defun %default-initform (&optional (to-test nil testp))
  (if testp
      (equal to-test *default-initform*)
      *default-initform*))

(defun %parse-recursable-variable (variable)
  (etypecase variable
    (list (funcall *parse-recursable-variable* variable))
    (symbol variable)))

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

(defclass required-parameter (parameter parameter-recursable-variable-mixin)
  ())

(defun %parse-required-parameter (parameter)
  (make-instance 'required-parameter :variable (%parse-recursable-variable parameter)))

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

(defclass optional-no-defaulting-parameter (parameter parameter-simple-variable-mixin)
  ())

(defun %parse-optional-no-defaulting-parameter (parameter)
  (make-instance 'optional-no-defaulting-parameter
                 :variable (etypecase parameter
                             (symbol parameter)
                             ((cons symbol null)
                              (car parameter)))))

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
