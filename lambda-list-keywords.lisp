(in-package #:first-class-lambda-lists)

(defclass fcll:lambda-list-keyword () ())

(defclass fcll:standard-lambda-list-keyword (fcll:lambda-list-keyword defsys:name-mixin)
  ((%arity :initarg :arity
           :reader arity)
   (%introducer :initarg :introducer)
   (%parameter-parser :initarg :parameter-parser
                      :reader parameter-parser
                      :type (or function symbol))))

(defmethod shared-initialize :after ((instance fcll:standard-lambda-list-keyword) slot-names &key)
  (unless (slot-boundp instance '%introducer)
    (setf (slot-value instance '%introducer) (defsys:name instance)))
  (unless (slot-boundp instance '%parameter-parser)
    (setf (slot-value instance '%parameter-parser)
          (let ((arity (arity instance)))
            (ecase arity
              (0 nil)
              (1 #'%parse-simple-parameter)
              (t (error "Must supply a parameter-parser for arity ~S." arity)))))))

(define (fcll:lambda-list-keyword &whole 1))

(define (fcll:lambda-list-keyword &environment 1))

(define (fcll:lambda-list-keyword :required t)
  :introducer nil
  :parameter-parser #'%parse-simple-parameter)

(define (fcll:lambda-list-keyword :required-specializable t)
  :introducer nil
  :parameter-parser #'%parse-specializable-parameter)

(define (fcll:lambda-list-keyword &optional t)
  :parameter-parser #'%parse-optional-parameter)

(define (fcll:lambda-list-keyword &rest 1))

(define (fcll:lambda-list-keyword &body 1))

(define (fcll:lambda-list-keyword &key t)
  :parameter-parser #'%parse-key-parameter)

(define (fcll:lambda-list-keyword &allow-other-keys 0))

(define (fcll:lambda-list-keyword &aux t)
  :parameter-parser #'%parse-aux-parameter)
