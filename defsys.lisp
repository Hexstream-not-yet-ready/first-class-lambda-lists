(in-package #:first-class-lambda-lists)

(defclass lambda-list-keyword-definitions (defsys:standard-system)
  ())

(defvar *lambda-list-keyword-definitions*
  (make-instance 'lambda-list-keyword-definitions :name 'fcll:lambda-list-keyword))

(setf (defsys:locate (defsys:root-system) 'fcll:lambda-list-keyword)
      *lambda-list-keyword-definitions*)

(defun %ensure-lambda-list-keyword (name arity &rest initargs)
  (setf (defsys:locate *lambda-list-keyword-definitions* name)
        (apply #'make-instance 'fcll:standard-lambda-list-keyword
               :name name :arity arity initargs)))

(defmethod defsys:expand-definition ((system lambda-list-keyword-definitions) name environment args &rest options)
  (destructuring-bind (arity) options
    `(%ensure-lambda-list-keyword ',name ,arity ,@args)))
