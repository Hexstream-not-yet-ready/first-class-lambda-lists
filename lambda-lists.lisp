(in-package #:first-class-lambda-lists)

(defclass fcll:lambda-list () ())

(defclass fcll:standard-lambda-list (fcll:lambda-list)
  ((%kind :initarg :kind
          :reader kind
          :type fcll:lambda-list-kind
          :initform (error "Must supply a lambda list kind."))
   (%sections :reader %sections
              :type list
              :initform nil)))

(defgeneric reset (object))

(defmethod reset ((lambda-list fcll:standard-lambda-list))
  (setf (slot-value lambda-list '%sections) nil))


(defgeneric parse-lambda-list (lambda-list specification))

(defmethod parse-lambda-list ((lambda-list fcll:standard-lambda-list) specification)
  (setf (slot-value lambda-list '%sections)
        (funcall (parser-maker (kind lambda-list)) specification)))
