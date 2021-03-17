(in-package #:first-class-lambda-lists)

;;; Lambda list keyword classes

(defclass lambda-list-keyword-class-definitions (defsys:standard-system)
  ())

(define (defsys:system fcll:lambda-list-keyword-class)
  lambda-list-keyword-class-definitions)

(defmethod defsys:expand-definition ((system lambda-list-keyword-class-definitions) name environment args &key)
  (declare (ignore environment))
  `(defsys:ensure ',(defsys:name system) ',name 'fcll:standard-lambda-list-keyword-class ,@args))


;;; Lambda list keywords

(defclass lambda-list-keyword-definitions (defsys:standard-system)
  ())

(define (defsys:system fcll:lambda-list-keyword)
  lambda-list-keyword-definitions)

(defmethod defsys:expand-definition ((system lambda-list-keyword-definitions) name environment arity-then-args &key)
  (declare (ignore environment))
  (destructuring-bind (arity &rest args) arity-then-args
    `(defsys:ensure ',(defsys:name system) ',name 'fcll:standard-lambda-list-keyword :arity ,arity ,@args)))


;;; Lambda list keyword order

(defclass lambda-list-keyword-order-definitions (defsys:standard-system)
  ())

(define (defsys:system fcll:lambda-list-keyword-order)
  lambda-list-keyword-order-definitions)

(defmethod defsys:expand-definition ((system lambda-list-keyword-order-definitions) name environment args &key)
  (declare (ignore environment))
  (destructuring-bind (specification) args
    `(defsys:ensure ',(defsys:name system) ',name 'fcll:standard-lambda-list-keyword-order :specification ',specification)))


;;; Lambda list keyword conflicts

(defclass lambda-list-keyword-conflicts-definitions (defsys:standard-system)
  ())

(define (defsys:system fcll:lambda-list-keyword-conflicts)
  lambda-list-keyword-conflicts-definitions)

(defmethod defsys:expand-definition ((system lambda-list-keyword-conflicts-definitions) name environment args &key)
  (declare (ignore environment))
  (destructuring-bind (specification) args
    `(defsys:ensure ',(defsys:name system) ',name 'fcll:standard-lambda-list-keyword-conflicts :specification ',specification)))


;;; Lambda list kinds

(defclass lambda-list-kind-definitions (defsys:standard-system)
  ())

(define (defsys:system fcll:lambda-list-kind)
  lambda-list-kind-definitions)

(defun %mappcon (function plist)
  (mapcan (let ((processp t))
            (lambda (key value)
              (prog1 (when processp
                       (funcall function key value))
                (setf processp (not processp)))))
          (cdr plist)
          (cddr plist)))

(defmethod defsys:expand-definition ((system lambda-list-kind-definitions) name environment args &key)
  (declare (ignore environment))
  (destructuring-bind (operator core &rest args) args
    (let ((core
           (if (typep core '(cons (eql :derive) list))
               `(make-instance 'derived-raw-lambda-list-core
                               ,@(%mappcon (lambda (key value)
                                             (list key (case key
                                                         ((:add :remove :replace) `',value)
                                                         (t value))))
                                           core))
               core)))
      `(defsys:ensure ',(defsys:name system) ',name 'fcll:standard-lambda-list-kind
                      :operator ',operator :raw-core ,core ,@args))))
