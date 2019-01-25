(in-package #:first-class-lambda-lists)

(defclass standard-inheritable-slots-class (standard-class)
  ((%inheritable-slots :reader inheritable-slots
                       :type list)
   (%non-inheritable-slots :reader non-inheritable-slots
                           :type list)))

(defclass inherit-mixin ()
  ((%inherit :initarg :inherit
             :reader inherit
             :initform nil)))

(defclass inheritable-direct-slot-definition (c2mop:standard-direct-slot-definition inherit-mixin)
  ())

(defmethod c2mop:direct-slot-definition-class ((class standard-inheritable-slots-class) &key (inherit nil inherit-supplied-p) &allow-other-keys)
  (declare (ignore inherit))
  (find-class (if inherit-supplied-p
                  'inheritable-direct-slot-definition
                  'c2mop:standard-direct-slot-definition)))

(defclass inheritable-effective-slot-definition (c2mop:standard-effective-slot-definition inherit-mixin)
  ())

(defmethod c2mop:effective-slot-definition-class ((class standard-inheritable-slots-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'inheritable-effective-slot-definition))

(defmethod c2mop:compute-effective-slot-definition ((class standard-inheritable-slots-class) name direct-slot-definitions)
  (declare (ignore name))
  (let ((slot-definition (call-next-method)))
    (setf (slot-value slot-definition '%inherit)
          (let ((found (find-if (lambda (direct-slot-definition)
                                  (typep direct-slot-definition 'inheritable-direct-slot-definition))
                                direct-slot-definitions)))
            (and found (inherit found))))
    slot-definition))


(defmethod c2mop:finalize-inheritance :after ((class standard-inheritable-slots-class))
  (setf (values (slot-value class '%inheritable-slots)
                (slot-value class '%non-inheritable-slots))
        (let (inheritable non-inheritable)
          (dolist (effective-slot (c2mop:class-slots class))
            (if (and (typep effective-slot 'inheritable-effective-slot-definition)
                     (inherit effective-slot))
                (push effective-slot inheritable)
                (push effective-slot non-inheritable)))
          (values (nreverse inheritable) (nreverse non-inheritable)))))

(defmethod c2mop:validate-superclass ((class standard-inheritable-slots-class) (superclass standard-class))
  t)


(defclass standard-inheritable-slots-object (standard-object)
  ())

(defmethod c2mop:compute-class-precedence-list ((class standard-inheritable-slots-class))
  (let ((class-precedence-list (call-next-method))
        (target (find-class 'standard-inheritable-slots-object)))
    (if (member target class-precedence-list)
        class-precedence-list
        (let ((tail (member (find-class 'standard-object) class-precedence-list)))
          (if tail
              (let ((head (ldiff class-precedence-list tail)))
                (nconc head (list target) tail))
              (error "Neither ~S nor ~S found in the class precedence list."
                     'standard-inheritable-slots-object
                     'standard-object))))))

(defun slot-inherited-value (object slot-name)
  (check-type slot-name symbol)
  (let* ((class (class-of object))
         (slot (find slot-name (c2mop:class-slots class)
                     :key #'c2mop:slot-definition-name
                     :test #'eq)))
    (if slot
        (slot-inherited-value-using-class class object slot)
        (slot-missing class object slot-name 'slot-inherited-value))))

(defmethod slot-missing ((class standard-inheritable-slots-class) instance slot-name operation &optional new-value)
  (declare (ignore new-value))
  (if (eq operation 'slot-inherited-value)
      (error "The slot ~S was missing when calling ~S on ~S." slot-name operation instance)
      (call-next-method)))

(defgeneric slot-inherited-value-using-class (class object slot))

(defmethod shared-initialize ((object standard-inheritable-slots-object) slot-names &rest initargs)
  (let* ((class (class-of object))
         (inheritable-slots (inheritable-slots class)))
    (multiple-value-bind (maybe-inherit passthrough)
        (if (eq slot-names t)
            (values inheritable-slots
                    (mapcar #'c2mop:slot-definition-name (non-inheritable-slots class)))
            (let* ((all-slots (c2mop:class-slots class))
                   (named-slots (mapcar (lambda (slot-name)
                                          (or (find slot-name all-slots
                                                    :key #'c2mop:slot-definition-name
                                                    :test #'eq)
                                              (error "Slot named ~S not found in object ~S."
                                                     slot-name object)))
                                        slot-names))
                   (inheritable (intersection inheritable-slots named-slots :test #'eq)))
              (values inheritable
                      (mapcar #'c2mop:slot-definition-name
                              (set-difference named-slots inheritable :test #'eq)))))
      (apply #'call-next-method object passthrough initargs)
      (let (repassthrough)
        (dolist (slot maybe-inherit)
          (unless (c2mop:slot-boundp-using-class class object slot)
            (multiple-value-bind (inherited-value inherited-value-p)
                (slot-inherited-value-using-class class object slot)
              (if inherited-value-p
                  (setf (c2mop:slot-value-using-class class object slot)
                        inherited-value)
                  (push (c2mop:slot-definition-name slot) repassthrough)))))
        (call-next-method object (nreverse repassthrough))))))
