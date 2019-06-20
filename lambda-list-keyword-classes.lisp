(in-package #:first-class-lambda-lists)

(defclass fcll:lambda-list-keyword-class () ())

(defgeneric fcll:lambda-list-keyword-class (object))

(defmethod fcll:lambda-list-keyword-class ((lambda-list-keyword-class fcll:lambda-list-keyword-class))
  lambda-list-keyword-class)

(defmethod fcll:lambda-list-keyword-class ((name symbol))
  (defsys:locate 'fcll:lambda-list-keyword-class name))

(defmethod defsys:locate ((system lambda-list-keyword-class-definitions) (name fcll:lambda-list-keyword-class) &rest keys)
  (declare (ignore keys))
  name)

(defclass fcll:standard-lambda-list-keyword-class (fcll:lambda-list-keyword-class defsys:name-mixin)
  ())
