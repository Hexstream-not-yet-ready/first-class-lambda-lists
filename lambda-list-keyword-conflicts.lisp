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


(defclass fcll:lambda-list-keyword-conflicts () ())

(defclass fcll:standard-lambda-list-keyword-conflicts (fcll:lambda-list-keyword-conflicts defsys:name-mixin)
  ((%specification :initarg :specification
                   :reader specification
                   :type list)
   (%tree :reader tree)))

(defgeneric %transform-keyword-conflicts (keyword-conflicts process-atom))

(defmethod %transform-keyword-conflicts ((keyword-conflicts fcll:standard-lambda-list-keyword-conflicts) process-atom)
  (make-instance 'fcll:standard-lambda-list-keyword-conflicts
                 :specification (%transform-keyword-conflicts (tree keyword-conflicts) process-atom)))

(defmethod %transform-keyword-conflicts (specification process-atom)
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

(defmethod shared-initialize :after ((instance fcll:standard-lambda-list-keyword-conflicts) slot-names &key)
  (declare (ignore slot-names))
  (setf (slot-value instance '%tree)
        (%transform-keyword-conflicts (specification instance)
                                      (lambda (spec)
                                        (list (lambda-list-keyword spec))))))

(define (fcll:lambda-list-keyword-conflicts :standard)
  (and &rest &body))
