(in-package #:first-class-lambda-lists)

(defclass lambda-list-keywords-list () ())


(defclass raw-lambda-list-keywords-list (lambda-list-keywords-list)
  ())

(defclass lambda-list-keywords-list-slots-mixin ()
  ((%keywords-set :reader keywords-set
                  :type fcll:lambda-list-keywords-set)
   (%keyword-order :reader keyword-order
                   :type fcll:lambda-list-keyword-order)
   (%keyword-conflicts :reader keyword-conflicts
                       :type fcll:lambda-list-keyword-conflicts)))

(defclass keywords-list-mixin ()
  ((%keywords-list :initarg :keywords-list
                   :reader keywords-list
                   :type lambda-list-keywords-list
                   :initform (error ":keywords-list argument is required."))))

(defclass standard-raw-lambda-list-keywords-list (raw-lambda-list-keywords-list
                                                  lambda-list-keywords-list-slots-mixin)
  ((%keywords-set :initarg :keywords-set)
   (%keyword-order :initarg :keyword-order
                   :initform (defsys:locate 'fcll:lambda-list-keyword-order
                                            :standard))
   (%keyword-conflicts :initarg :keyword-conflicts
                       :initform (defsys:locate 'fcll:lambda-list-keyword-conflicts
                                                :standard))))


(defclass coherent-lambda-list-keywords-list (lambda-list-keywords-list)
  ())

(defgeneric coherent-lambda-list-keywords-list (lambda-list-keywords-list))

(defmethod coherent-lambda-list-keywords-list ((keywords-list raw-lambda-list-keywords-list))
  (make-instance 'standard-coherent-lambda-list-keywords-list
                 :keywords-list keywords-list))

(defmethod coherent-lambda-list-keywords-list ((keywords-list coherent-lambda-list-keywords-list))
  keywords-list)

(defclass standard-coherent-lambda-list-keywords-list (coherent-lambda-list-keywords-list
                                                       lambda-list-keywords-list-slots-mixin
                                                       keywords-list-mixin)
  ())

(defmethod shared-initialize :after ((instance standard-coherent-lambda-list-keywords-list) slot-names
                                     &key (keywords-list nil keywords-list-p))
  (declare (ignore slot-names))
  (when keywords-list-p
    (check-type keywords-list lambda-list-keywords-list)
    (let ((keywords-set (keywords-set keywords-list)))
      (setf (slot-value instance '%keywords-set)
            keywords-set
            (slot-value instance '%keyword-order)
            (make-instance 'scoped-lambda-list-keyword-order
                           :keyword-order (keyword-order keywords-list)
                           :keywords-set keywords-set)
            (slot-value instance '%keyword-conflicts)
            (make-instance 'scoped-lambda-list-keyword-conflicts
                           :keyword-conflicts (keyword-conflicts keywords-list)
                           :keywords-set keywords-set)))))

(defclass mapped-lambda-list-keywords-list (coherent-lambda-list-keywords-list
                                            lambda-list-keywords-list-slots-mixin
                                            keywords-list-mixin
                                            mapper-mixin)
  ((%keywords-list :type coherent-lambda-list-keywords-list)))

(defmethod shared-initialize :after ((instance mapped-lambda-list-keywords-list) slot-names
                                     &key (keywords-list nil keywords-list-p))
  (declare (ignore slot-names))
  (when keywords-list-p
    (check-type keywords-list coherent-lambda-list-keywords-list)
    (let* ((keywords-set (keywords-set keywords-list))
           (mapped-lambda-list-keywords-set
            (make-instance 'mapped-lambda-list-keywords-set
                           :keywords-set keywords-set
                           :mapper (mapper instance)))
           (mapper (let ((mappings (mappings mapped-lambda-list-keywords-set)))
                     (lambda (lambda-list-keyword)
                       (or (cdr (assoc lambda-list-keyword mappings :test #'eq))
                           (error "Mapping error."))))))
      (setf (slot-value instance '%keywords-set)
            mapped-lambda-list-keywords-set
            (slot-value instance '%keyword-order)
            (make-instance 'mapped-lambda-list-keyword-order
                           :keyword-order (keyword-order keywords-list)
                           :mapper mapper)
            (slot-value instance '%keyword-conflicts)
            (make-instance 'mapped-lambda-list-keyword-conflicts
                           :keyword-conflicts (keyword-conflicts keywords-list)
                           :mapper mapper)))))
