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
