(in-package #:first-class-lambda-lists)

(eval-when t

  (defclass lambda-list-keyword-conflicts-definitions (defsys:standard-system)
    ())

  (defvar *lambda-list-keyword-conflicts-definitions*
    (make-instance 'lambda-list-keyword-conflicts-definitions :name 'fcll:lambda-list-keyword-conflicts))

  (setf (defsys:locate (defsys:root-system) 'fcll:lambda-list-keyword-conflicts)
        *lambda-list-keyword-conflicts-definitions*)

  (defun %ensure-lambda-list-keyword-conflicts (name specification)
    (%ensure-definition *lambda-list-keyword-conflicts-definitions* name
                        'fcll:standard-lambda-list-keyword-conflicts
                        :specification specification))

  (defmethod defsys:expand-definition ((system lambda-list-keyword-conflicts-definitions) name environment args &key)
    (declare (ignore environment))
    (destructuring-bind (specification) args
      `(%ensure-lambda-list-keyword-conflicts ',name ',specification))))


(defmethod %transform-keyword-conflicts-specification (specification process-atom)
  (labels ((recurse (spec)
             (etypecase spec
               (cons (destructuring-bind (operator &rest args) spec
                       (check-type operator (member or and))
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
                           (1 (if (eq operator 'and)
                                  nil
                                  args))
                           (t (list (cons operator args)))))))
               (atom (funcall process-atom spec)))))
    (when specification
      (first (recurse specification)))))

(defclass keyword-conflicts-mixin ()
  ((%keyword-conflicts :initarg :keyword-conflicts
                       :reader keyword-conflicts
                       :type fcll:lambda-list-keyword-conflicts
                       :initform (error "Must supply a :keyword-conflicts."))))


(defclass fcll:lambda-list-keyword-conflicts () ())

(defclass fcll:standard-lambda-list-keyword-conflicts (fcll:lambda-list-keyword-conflicts tree-mixin defsys:name-mixin)
  ((%specification :initarg :specification
                   :reader specification
                   :type list)))

(defmethod %compute-tree ((instance fcll:standard-lambda-list-keyword-conflicts))
  (%transform-keyword-conflicts-specification (specification instance)
                                              (lambda (spec)
                                                (list (lambda-list-keyword spec)))))


(defclass scoped-lambda-list-keyword-conflicts (fcll:lambda-list-keyword-conflicts keyword-conflicts-mixin keywords-set-mixin tree-mixin)
  ())

(defmethod %compute-tree ((instance scoped-lambda-list-keyword-conflicts))
  (let ((keywords (lambda-list-keywords (keywords-set instance))))
    (%transform-keyword-conflicts-specification (tree (keyword-conflicts instance))
                                                (lambda (keyword)
                                                  (when (member (defsys:name keyword) keywords
                                                                :key #'defsys:name :test #'eq)
                                                    (list keyword))))))


(defclass mapped-lambda-list-keyword-conflicts (fcll:lambda-list-keyword-order keyword-conflicts-mixin mapper-mixin tree-mixin)
  ())

(defmethod %compute-tree ((instance mapped-lambda-list-keyword-conflicts))
  (%transform-keyword-conflicts-specification (tree (keyword-conflicts instance))
                                              (let ((mapper (mapper instance)))
                                                (lambda (keyword)
                                                  (list (funcall mapper keyword))))))


(define (fcll:lambda-list-keyword-conflicts :standard)
  (and &rest &body))
