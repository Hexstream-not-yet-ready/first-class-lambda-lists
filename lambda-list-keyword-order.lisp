(in-package #:first-class-lambda-lists)

(defclass fcll:lambda-list-keyword-order () ())

(defclass fcll:standard-lambda-list-keyword-order (fcll:lambda-list-keyword-order defsys:name-mixin)
  ((%specification :initarg :specification
                   :reader specification
                   :type cons)))

(define (fcll:lambda-list-keyword-order :standard)
  (list &whole
        (or (list (or :required :required-specializable)
                  (or &optional :&optional-no-defaulting)
                  (or &rest &body)
                  (or &key :&key-no-defaulting)
                  &aux
                  :&environment-last)
            :&environment-not-before-&whole)))
