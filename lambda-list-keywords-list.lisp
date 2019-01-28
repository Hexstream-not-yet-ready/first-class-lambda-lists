(in-package #:first-class-lambda-lists)

(defclass lambda-list-keywords-list () ())

(defclass standard-lambda-list-keywords-list (lambda-list-keywords-list)
  ((%keywords-set :initarg :keywords-set ;Canonicalized in (shared-initialize :around).
                  :reader keywords-set
                  :reader lambda-list-keywords-set
                  :type lambda-list-keywords-set)
   (%keyword-order :reader keyword-order
                   :type fcll:lambda-list-keyword-order)
   (%keyword-conflicts :reader keyword-conflicts
                       :type fcll:lambda-list-keyword-conflicts)))

(defmethod shared-initialize :after ((keywords-list standard-lambda-list-keywords-list) slot-names &key)
  (declare (ignore slot-names))
  (let ((keywords-set (keywords-set keywords-list)))
    (setf (slot-value keywords-list '%keyword-order)
          (make-instance 'scoped-lambda-list-keyword-order
                         :keyword-order (defsys:locate 'fcll:lambda-list-keyword-order
                                                       :standard)
                         :keywords-set keywords-set)
          (slot-value keywords-list '%keyword-conflicts)
          (make-instance 'scoped-lambda-list-keyword-conflicts
                         :keyword-conflicts (defsys:locate 'fcll:lambda-list-keyword-conflicts
                                                           :standard)
                         :keywords-set keywords-set))))
