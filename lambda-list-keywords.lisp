(in-package #:first-class-lambda-lists)

(defclass fcll:lambda-list-keyword () ())

(defclass fcll:standard-lambda-list-keyword ()
  ((%name :initarg :name
          :reader name)
   (%arity :initarg :arity
           :reader arity)
   (%introducer :initarg :introducer)))

(defmethod shared-initialize :after ((instance fcll:standard-lambda-list-keyword) slot-names &key)
  (unless (slot-boundp instance '%introducer)
    (setf (slot-value instance '%introducer) (name instance))))

(make-instance 'fcll:standard-lambda-list-keyword
               :name '&whole
               :arity 1)

(make-instance 'fcll:standard-lambda-list-keyword
               :name '&environment
               :arity 1)

(make-instance 'fcll:standard-lambda-list-keyword
               :name :required
               :arity t
               :introducer nil)

(make-instance 'fcll:standard-lambda-list-keyword
               :name :required-specializable
               :arity t
               :introducer nil)

(make-instance 'fcll:standard-lambda-list-keyword
               :name '&optional
               :arity t)

(make-instance 'fcll:standard-lambda-list-keyword
               :name '&rest
               :arity 1)

(make-instance 'fcll:standard-lambda-list-keyword
               :name '&body
               :arity 1)

(make-instance 'fcll:standard-lambda-list-keyword
               :name '&key
               :arity t)

(make-instance 'fcll:standard-lambda-list-keyword
               :name '&allow-other-keys
               :arity 0)

(make-instance 'fcll:standard-lambda-list-keyword
               :name '&aux
               :arity t)
