(in-package #:first-class-lambda-lists)

(defclass lambda-list-core () ())


(defclass raw-lambda-list-core (lambda-list-core)
  ())

(defgeneric raw-lambda-list-core (object))

(defmethod raw-lambda-list-core ((core raw-lambda-list-core))
  core)

(defclass lambda-list-core-slots-mixin ()
  ((%keywords-set :reader keywords-set
                  :type fcll:lambda-list-keywords-set)
   (%keyword-order :reader keyword-order
                   :type fcll:lambda-list-keyword-order)
   (%keyword-conflicts :reader keyword-conflicts
                       :type fcll:lambda-list-keyword-conflicts)))

(defmethod print-object ((core lambda-list-core-slots-mixin) stream)
  (print-unreadable-object (core stream :type t)
    (prin1 (lambda-list-keywords (keywords-set core)) stream)))

(defclass lambda-list-core-mixin ()
  ((%core :initarg :core
          :reader core
          :type lambda-list-core
          :initform (error ":core argument is required."))))

(defclass standard-raw-lambda-list-core (raw-lambda-list-core
                                         lambda-list-core-slots-mixin)
  ((%keywords-set :initarg :keywords-set)
   (%keyword-order :initarg :keyword-order)
   (%keyword-conflicts :initarg :keyword-conflicts)))


(defclass derived-raw-lambda-list-core (raw-lambda-list-core
                                        lambda-list-core-slots-mixin)
  ((%from :initarg :from
          :reader from
          :type raw-lambda-list-core)
   (%keyword-order :initarg :keyword-order)
   (%keyword-conflicts :initarg :keyword-conflicts)))

(defmethod shared-initialize :around ((instance derived-raw-lambda-list-core) slot-names &rest initargs &key (from :ordinary from-p) add remove replace)
  (declare (ignore add remove replace))
  (if (or from-p (eq slot-names t) (member '%from slot-names))
      (apply #'call-next-method instance slot-names :from (raw-lambda-list-core from) initargs)
      (call-next-method)))

(defmethod shared-initialize :after ((instance derived-raw-lambda-list-core) slot-names &rest initargs)
  (let ((from (from instance)))
    (setf (slot-value instance '%keywords-set)
          (apply #'make-instance 'derived-lambda-list-keywords-set
                 :keywords-set (keywords-set from)
                 (%mappcon (lambda (key value)
                             (when (member key '(:add :remove :replace))
                               (list key value)))
                           initargs)))
    (unless (slot-boundp instance '%keyword-order)
      (setf (slot-value instance '%keyword-order)
            (keyword-order from)))
    (unless (slot-boundp instance '%keyword-conflicts)
      (setf (slot-value instance '%keyword-conflicts)
            (keyword-conflicts from)))))


(defclass coherent-lambda-list-core (lambda-list-core)
  ())

(defgeneric coherent-lambda-list-core (lambda-list-core))

(defmethod coherent-lambda-list-core ((core raw-lambda-list-core))
  (make-instance 'standard-coherent-lambda-list-core
                 :core core))

(defmethod coherent-lambda-list-core ((core coherent-lambda-list-core))
  core)

(defclass standard-coherent-lambda-list-core (coherent-lambda-list-core
                                              lambda-list-core-slots-mixin
                                              lambda-list-core-mixin)
  ())

(defmethod shared-initialize :after ((instance standard-coherent-lambda-list-core) slot-names
                                     &key (core nil core-supplied-p))
  (declare (ignore slot-names))
  (when core-supplied-p
    (check-type core lambda-list-core)
    (let ((keywords-set (keywords-set core)))
      (setf (slot-value instance '%keywords-set)
            keywords-set
            (slot-value instance '%keyword-order)
            (make-instance 'scoped-lambda-list-keyword-order
                           :keyword-order (keyword-order core)
                           :keywords-set keywords-set)
            (slot-value instance '%keyword-conflicts)
            (make-instance 'scoped-lambda-list-keyword-conflicts
                           :keyword-conflicts (keyword-conflicts core)
                           :keywords-set keywords-set)))))

(defclass mapped-lambda-list-core (coherent-lambda-list-core
                                   lambda-list-core-slots-mixin
                                   lambda-list-core-mixin
                                   mapper-mixin)
  ((%core :type coherent-lambda-list-core)))

(defmethod shared-initialize :after ((instance mapped-lambda-list-core) slot-names
                                     &key (core nil core-supplied-p))
  (declare (ignore slot-names))
  (when core-supplied-p
    (check-type core coherent-lambda-list-core)
    (let* ((keywords-set (keywords-set core))
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
                           :keyword-order (keyword-order core)
                           :mapper mapper)
            (slot-value instance '%keyword-conflicts)
            (make-instance 'mapped-lambda-list-keyword-conflicts
                           :keyword-conflicts (keyword-conflicts core)
                           :mapper mapper)))))
