(in-package #:first-class-lambda-lists)

(defclass fcll:lambda-list-keyword () ())

(defclass fcll:standard-lambda-list-keyword ()
  ((%name :initarg :name
          :reader name)
   (%arity :initarg :arity
           :reader arity)))

(make-instance 'fcll:standard-lambda-list-keyword
               :name '&whole
               :arity 1)

(make-instance 'fcll:standard-lambda-list-keyword
               :name '&environment
               :arity 1)

(make-instance 'fcll:standard-lambda-list-keyword
               :name :required
               :arity t)

(make-instance 'fcll:standard-lambda-list-keyword
               :name :required-specializable
               :arity t)

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
