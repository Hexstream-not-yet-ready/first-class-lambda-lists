(in-package #:first-class-lambda-lists)

(eval-when t

  (defclass lambda-list-keyword-order-definitions (defsys:standard-system)
    ())

  (defvar *lambda-list-keyword-order-definitions*
    (make-instance 'lambda-list-keyword-order-definitions :name 'fcll:lambda-list-keyword-order))

  (setf (defsys:locate (defsys:root-system) 'fcll:lambda-list-keyword-order)
        *lambda-list-keyword-order-definitions*)

  (defun %ensure-lambda-list-keyword-order (name specification)
    (%ensure-definition *lambda-list-keyword-order-definitions* name
                        'fcll:standard-lambda-list-keyword-order
                        :specification specification))

  (defmethod defsys:expand-definition ((system lambda-list-keyword-order-definitions) name environment args &key)
    (declare (ignore environment))
    (destructuring-bind (specification) args
      `(%ensure-lambda-list-keyword-order ',name ',specification))))


(defclass tree-mixin ()
  ((%tree :reader tree)))

(defgeneric %compute-tree (object))

(defmethod shared-initialize :after ((instance tree-mixin) slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (setf (slot-value instance '%tree) (%compute-tree instance)))

(defun %transform-keyword-order-specification (specification process-atom)
  (labels ((recurse (spec)
             (etypecase spec
               (cons (destructuring-bind (operator &rest args) spec
                       (check-type operator (member list or))
                       (let ((args (mapcan (let ((to-splice `(cons (cons (eql ,operator) list)
                                                                   null)))
                                             (lambda (arg)
                                               (let ((result (recurse arg)))
                                                 (if (typep result to-splice)
                                                     (rest (first result))
                                                     result))))
                                           args)))
                         (case (length args)
                           (0 nil)
                           (1 args)
                           (t (list (cons operator args)))))))
               (atom (funcall process-atom spec)))))
    (first (recurse specification))))

(defclass keyword-order-mixin ()
  ((%keyword-order :initarg :keyword-order
                   :reader keyword-order
                   :type fcll:lambda-list-keyword-order
                   :initform (error "Must supply a :keyword-order."))))

(defclass keywords-set-mixin ()
  ((%keywords-set :initarg :keywords-set
                  :reader keywords-set
                  :type fcll:lambda-list-keywords-set
                  :initform (error "Must supply a :keywords-set."))))


(defclass fcll:lambda-list-keyword-order () ())

(defclass fcll:standard-lambda-list-keyword-order (fcll:lambda-list-keyword-order tree-mixin defsys:name-mixin)
  ((%specification :initarg :specification
                   :reader specification
                   :type cons))
  (:default-initargs :name nil))

(defmethod %compute-tree ((instance fcll:standard-lambda-list-keyword-order))
  (%transform-keyword-order-specification (specification instance)
                                          (lambda (spec)
                                            (list (lambda-list-keyword spec)))))

(defclass scoped-lambda-list-keyword-order (fcll:lambda-list-keyword-order keyword-order-mixin keywords-set-mixin tree-mixin)
  ())

(defmethod %compute-tree ((instance scoped-lambda-list-keyword-order))
  (let ((keywords (lambda-list-keywords (keywords-set instance))))
    (%transform-keyword-order-specification (tree (keyword-order instance))
                                            (lambda (keyword)
                                              (when (member (defsys:name keyword) keywords
                                                            :key #'defsys:name :test #'eq)
                                                (list keyword))))))

(defclass mapper-mixin ()
  ((%mapper :initarg :mapper
            :reader mapper
            :type (or function symbol)
            :initform (error "Must supply a :mapper."))))

(defclass mapped-lambda-list-keyword-order (fcll:lambda-list-keyword-order keyword-order-mixin mapper-mixin tree-mixin)
  ())

(defmethod %compute-tree ((instance mapped-lambda-list-keyword-order))
  (%transform-keyword-order-specification (tree (keyword-order instance))
                                          (let ((mapper (mapper instance)))
                                            (lambda (keyword)
                                              (list (funcall mapper keyword))))))

(define (fcll:lambda-list-keyword-order :standard)
  (list &whole
        (or (list (or :required :required-specializable)
                  (or &optional :&optional-no-defaulting)
                  (or &rest &body)
                  (or &key :&key-no-defaulting)
                  &aux
                  :&environment-last)
            :&environment-not-before-&whole)))
