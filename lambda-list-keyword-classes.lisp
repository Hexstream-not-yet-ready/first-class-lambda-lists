(in-package #:first-class-lambda-lists)

(eval-when t

  (defclass lambda-list-keyword-class-definitions (defsys:standard-system)
    ())

  (defvar *lambda-list-keyword-class-definitions*
    (make-instance 'lambda-list-keyword-class-definitions :name 'fcll:lambda-list-keyword-class))

  (setf (defsys:locate (defsys:root-system) 'fcll:lambda-list-keyword-class)
        *lambda-list-keyword-class-definitions*)

  (defun %ensure-lambda-list-keyword-class (name &rest initargs)
    (apply #'%ensure-definition *lambda-list-keyword-class-definitions* name
           'fcll:standard-lambda-list-keyword-class
           initargs))

  (defmethod defsys:expand-definition ((system lambda-list-keyword-class-definitions) name environment args &key)
    (declare (ignore environment))
    `(%ensure-lambda-list-keyword-class ',name ,@args)))


(defclass fcll:lambda-list-keyword-class () ())

(defgeneric fcll:lambda-list-keyword-class (object))

(defmethod fcll:lambda-list-keyword-class ((lambda-list-keyword-class fcll:lambda-list-keyword-class))
  lambda-list-keyword-class)

(defmethod fcll:lambda-list-keyword-class ((name symbol))
  (defsys:locate *lambda-list-keyword-class-definitions* name))

(defmethod defsys:locate ((system lambda-list-keyword-class-definitions) (name fcll:lambda-list-keyword-class) &rest keys)
  (declare (ignore keys))
  name)

(defclass fcll:standard-lambda-list-keyword-class (fcll:lambda-list-keyword-class defsys:name-mixin)
  ())
