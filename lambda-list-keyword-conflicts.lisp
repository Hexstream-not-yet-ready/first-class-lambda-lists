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
    (destructuring-bind (specification) args
      `(%ensure-lambda-list-keyword-conflicts ',name ',specification))))


(defclass fcll:lambda-list-keyword-conflicts () ())

(defclass fcll:standard-lambda-list-keyword-conflicts (fcll:lambda-list-keyword-conflicts defsys:name-mixin)
  ((%specification :initarg :specification
                   :reader specification
                   :type cons)
   (%tree :reader tree)))

(defun %transform-keyword-conflicts (conflicts process-atom)
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
    (first (recurse conflicts))))

(defun %lambda-list-keyword-conflicts-specification-to-tree (specification)
  (%transform-keyword-conflicts specification (lambda (spec)
                                                (check-type spec symbol)
                                                (list (defsys:locate *lambda-list-keyword-definitions* spec)))))

(defmethod shared-initialize :after ((instance fcll:standard-lambda-list-keyword-conflicts) slot-names &key)
  (setf (slot-value instance '%tree)
        (%lambda-list-keyword-conflicts-specification-to-tree (specification instance))))

(define (fcll:lambda-list-keyword-conflicts :standard)
  (and &rest &body))
