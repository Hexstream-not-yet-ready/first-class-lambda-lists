(in-package #:first-class-lambda-lists)

;;; Lambda list keyword classes

(defclass lambda-list-keyword-class-definitions (defsys:standard-system)
  ())

(defvar *lambda-list-keyword-class-definitions*
  (make-instance 'lambda-list-keyword-class-definitions :name 'fcll:lambda-list-keyword-class))

(setf (defsys:locate (defsys:root-system) 'fcll:lambda-list-keyword-class)
      *lambda-list-keyword-class-definitions*)

(defmethod defsys:expand-definition ((system lambda-list-keyword-class-definitions) name environment args &key)
  (declare (ignore environment))
  `(defsys:ensure ',(defsys:name system) ',name 'fcll:standard-lambda-list-keyword-class ,@args))


;;; Lambda list keywords

(defclass lambda-list-keyword-definitions (defsys:standard-system)
  ())

(defvar *lambda-list-keyword-definitions*
  (make-instance 'lambda-list-keyword-definitions :name 'fcll:lambda-list-keyword))

(setf (defsys:locate (defsys:root-system) 'fcll:lambda-list-keyword)
      *lambda-list-keyword-definitions*)

(defmethod defsys:expand-definition ((system lambda-list-keyword-definitions) name environment arity-then-args &key)
  (declare (ignore environment))
  (destructuring-bind (arity &rest args) arity-then-args
    `(defsys:ensure ',(defsys:name system) ',name 'fcll:standard-lambda-list-keyword :arity ,arity ,@args)))


;;; Lambda list keyword order

(defclass lambda-list-keyword-order-definitions (defsys:standard-system)
  ())

(defvar *lambda-list-keyword-order-definitions*
  (make-instance 'lambda-list-keyword-order-definitions :name 'fcll:lambda-list-keyword-order))

(setf (defsys:locate (defsys:root-system) 'fcll:lambda-list-keyword-order)
      *lambda-list-keyword-order-definitions*)

(defmethod defsys:expand-definition ((system lambda-list-keyword-order-definitions) name environment args &key)
  (declare (ignore environment))
  (destructuring-bind (specification) args
    `(defsys:ensure ',(defsys:name system) ',name 'fcll:standard-lambda-list-keyword-order :specification ',specification)))


;;; Lambda list keyword conflicts

(defclass lambda-list-keyword-conflicts-definitions (defsys:standard-system)
  ())

(defvar *lambda-list-keyword-conflicts-definitions*
  (make-instance 'lambda-list-keyword-conflicts-definitions :name 'fcll:lambda-list-keyword-conflicts))

(setf (defsys:locate (defsys:root-system) 'fcll:lambda-list-keyword-conflicts)
      *lambda-list-keyword-conflicts-definitions*)

(defmethod defsys:expand-definition ((system lambda-list-keyword-conflicts-definitions) name environment args &key)
  (declare (ignore environment))
  (destructuring-bind (specification) args
    `(defsys:ensure ',(defsys:name system) ',name 'fcll:standard-lambda-list-keyword-conflicts :specification ',specification)))


;;; Lambda list kinds

(defclass lambda-list-kind-definitions (defsys:standard-system)
  ())

(defvar *lambda-list-kind-definitions*
  (make-instance 'lambda-list-kind-definitions :name 'fcll:lambda-list-kind))

(setf (defsys:locate (defsys:root-system) 'fcll:lambda-list-kind)
      *lambda-list-kind-definitions*)

(defun %derive-keywords-list (&key (from :ordinary) add remove replace)
  (let ((from (and from (raw-keywords-list (lambda-list-kind from)))))
    (multiple-value-call #'make-instance 'standard-raw-lambda-list-keywords-list
                         :keywords-set (make-instance 'fcll:derived-lambda-list-keywords-set
                                                      :keywords-set (and from (keywords-set from))
                                                      :add add
                                                      :remove remove
                                                      :replace replace)
                         (if from
                             (values :keyword-order (keyword-order from)
                                     :keyword-conflicts (keyword-conflicts from))
                             (values)))))

(defmethod defsys:expand-definition ((system lambda-list-kind-definitions) name environment args &key)
  (declare (ignore environment))
  (destructuring-bind (operator keywords &rest args) args
    (let ((keywords-list
           (etypecase keywords
             ((cons (eql :derive) list)
              `(%derive-keywords-list ,@(mapcan (let ((processp t))
                                                  (lambda (key value)
                                                    (prog1 (when processp
                                                             (list key `',value))
                                                      (setf processp (not processp)))))
                                                (cdr keywords)
                                                (cddr keywords))))
             (list
              `(%derive-keywords-list :from nil :add ',keywords)))))
      `(defsys:ensure ',(defsys:name system) ',name 'fcll:standard-lambda-list-kind
                      :operator ',operator :raw-keywords-list ,keywords-list ,@args))))
