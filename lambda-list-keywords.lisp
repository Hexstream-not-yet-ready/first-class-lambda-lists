(in-package #:first-class-lambda-lists)

(defclass fcll:lambda-list-keyword () ())

(defclass fcll:standard-lambda-list-keyword ()
  ((%name :initarg :name
          :reader name)
   (%arity :initarg :arity
           :reader arity)
   (%introducer :initarg :introducer)
   (%parameter-parser :initarg :parameter-parser
                      :reader parameter-parser
                      :type (or function symbol))))

(defmethod shared-initialize :after ((instance fcll:standard-lambda-list-keyword) slot-names &key)
  (unless (slot-boundp instance '%introducer)
    (setf (slot-value instance '%introducer) (name instance)))
  (unless (slot-boundp instance '%parameter-parser)
    (setf (slot-value instance '%parameter-parser)
          (let ((arity (arity instance)))
            (ecase arity
              (0 nil)
              (1 #'%parse-simple-parameter)
              (t (error "Must supply a parameter-parser for arity ~S." arity)))))))

(defclass parameter () ())

(defclass parameter-initform-mixin ()
  ((%initform :initarg :initform
              :reader initform
              :type t)))

(defclass parameter-suppliedp-variable-mixin ()
  ((%suppliedp-variable :initarg :suppliedp-variable
                        :reader suppliedp-variable
                        :type symbol
                        :initform nil)))

(defclass simple-parameter ()
  ((%variable :initarg :variable
              :reader variable
              :type symbol)))

(defun %parse-simple-parameter (parameter)
  (make-instance 'simple-parameter :variable parameter))

(defclass specializable-parameter (simple-parameter)
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

(defclass optional-parameter (parameter-initform-mixin parameter-suppliedp-variable-mixin)
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

(defclass aux-parameter (parameter-initform-mixin)
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

(defclass key-parameter (parameter-initform-mixin parameter-suppliedp-variable-mixin)
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

(make-instance 'fcll:standard-lambda-list-keyword
               :name '&whole
               :arity 1)

(make-instance 'fcll:standard-lambda-list-keyword
               :name '&environment
               :arity 1)

(make-instance 'fcll:standard-lambda-list-keyword
               :name :required
               :arity t
               :introducer nil
               :parameter-parser #'%parse-simple-parameter)

(make-instance 'fcll:standard-lambda-list-keyword
               :name :required-specializable
               :arity t
               :introducer nil
               :parameter-parser #'%parse-specializable-parameter)

(make-instance 'fcll:standard-lambda-list-keyword
               :name '&optional
               :arity t
               :parameter-parser #'%parse-optional-parameter)

(make-instance 'fcll:standard-lambda-list-keyword
               :name '&rest
               :arity 1)

(make-instance 'fcll:standard-lambda-list-keyword
               :name '&body
               :arity 1)

(make-instance 'fcll:standard-lambda-list-keyword
               :name '&key
               :arity t
               :parameter-parser #'%parse-key-parameter)

(make-instance 'fcll:standard-lambda-list-keyword
               :name '&allow-other-keys
               :arity 0)

(make-instance 'fcll:standard-lambda-list-keyword
               :name '&aux
               :arity t
               :parameter-parser #'%parse-aux-parameter)
