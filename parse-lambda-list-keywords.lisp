(in-package #:first-class-lambda-lists)

(defclass parameter () ())

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
  ((%variable :initarg :variable
              :reader variable
              :type symbol)))

(defun %parse-simple-parameter (parameter)
  (check-type parameter symbol)
  (make-instance 'simple-parameter :variable parameter))

(defclass required-parameter (parameter parameter-variable-mixin)
  ())

(defun %parse-required-parameter (parameter)
  (check-type parameter symbol)
  (make-instance 'required-parameter :variable parameter))

(defclass specializable-parameter (required-parameter)
  ((%specializer :initarg :specializer
                 :reader specializer
                 :initform nil)))

(defun %parse-specializable-parameter (parameter)
  (multiple-value-bind (variable specializer)
      (if (symbolp parameter)
          (values parameter nil)
          (destructuring-bind (variable &optional specializer)
              parameter
            (values variable specializer)))
    (make-instance 'specializable-parameter
                   :variable variable
                   :specializer specializer)))

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

(defclass key-parameter (parameter parameter-variable-mixin parameter-initform-mixin parameter-suppliedp-variable-mixin)
  ((%keyword-name :initarg :keyword-name
                  :reader keyword-name
                  :type symbol)))

(defun %keywordize (symbol)
  (intern (symbol-name symbol) '#:keyword))

(defmethod shared-initialize :after ((instance key-parameter) slot-names &key)
  (unless (slot-boundp instance '%keyword-name)
    (setf (slot-value instance '%keyword-name)
          (%keywordize (variable instance)))))

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
    (make-instance 'optional-parameter
                   :variable variable
                   :initform initform
                   :suppliedp-variable suppliedp-variable
                   :keyword-name keyword-name)))
