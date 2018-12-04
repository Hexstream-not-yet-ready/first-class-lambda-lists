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

(defmethod defsys:expand-definition ((system lambda-list-keyword-definitions) name environment arity-then-args &key)
  (destructuring-bind (arity &rest args) arity-then-args
    `(%ensure-lambda-list-keyword ',name ,arity ,@args)))


(defclass lambda-list-kind-definitions (defsys:standard-system)
  ())

(defvar *lambda-list-kind-definitions*
  (make-instance 'lambda-list-kind-definitions :name 'fcll:lambda-list-kind))

(setf (defsys:locate (defsys:root-system) 'fcll:lambda-list-kind)
      *lambda-list-kind-definitions*)

(defun %ensure-lambda-list-kind (name operator keywords &rest initargs)
  (setf (defsys:locate *lambda-list-kind-definitions* name)
        (apply #'make-instance 'fcll:standard-lambda-list-kind
               :name name :operator operator :keywords keywords
               initargs)))

(defmethod defsys:expand-definition ((system lambda-list-kind-definitions) name environment args &key)
  (destructuring-bind (operator keywords &rest args) args
    (let ((keywords-expansion
           (etypecase keywords
             ((cons (eql :derive) list)
              `(%derive-keywords-list ,@(mapcan (let ((processp t))
                                                  (lambda (key value)
                                                    (prog1 (when processp
                                                             (list key `',value))
                                                      (setf processp (not processp)))))
                                                (cdr keywords)
                                                (cddr keywords))))
             (list `',keywords))))
      `(%ensure-lambda-list-kind ',name ',operator ,keywords-expansion ,@args))))
