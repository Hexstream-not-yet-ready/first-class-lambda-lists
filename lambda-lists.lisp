(in-package #:first-class-lambda-lists)

(defclass fcll:lambda-list () ())

(defclass fcll:standard-lambda-list (fcll:lambda-list)
  ((%kind :initarg :kind
          :reader kind
          :type fcll:lambda-list-kind
          :initform (error "Must supply a lambda list kind."))
   (%sections :reader %sections
              :type simple-vector)))
