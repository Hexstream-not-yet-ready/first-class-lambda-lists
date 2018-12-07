(in-package #:first-class-lambda-lists)

(defclass fcll:lambda-list-keyword-order () ())

(defclass fcll:standard-lambda-list-keyword-order (fcll:lambda-list-keyword-order defsys:name-mixin)
  ((%order-chains :initarg :order-chains
                  :reader order-chains
                  :type list)
   (%expanded :reader expanded
              :type list)))

(defun %expand-order (multiplexed)
  (mapcan (lambda (before after)
            (flet ((extract-possibilities (possibilities)
                     (etypecase possibilities
                       (symbol (list possibilities))
                       ((cons (eql or) (cons symbol list))
                        (mapcar (lambda (possibility)
                                  (check-type possibility symbol))
                                (cddr possibilities))
                        (cdr possibilities)))))
              (cartesian-product (extract-possibilities before)
                                 (extract-possibilities after))))
          multiplexed
          (cdr multiplexed)))

(defmethod shared-initialize :after ((instance fcll:standard-lambda-list-keyword-order) slot-names &key)
  (setf (slot-value instance '%expanded)
        (mapcan #'%expand-order (slot-value instance '%order-chains))))

(define (fcll:lambda-list-keyword-order :standard)
  (&whole
   (or :required :required-specializable)
   (or &optional :&optional-no-defaulting)
   (or &rest &body)
   (or &key :&key-no-defaulting)
   &aux
   :&environment-last)
  (&whole :&environment-not-before-&whole))
