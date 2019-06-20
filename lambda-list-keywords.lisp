(in-package #:first-class-lambda-lists)

(defclass fcll:lambda-list-keyword () ())

(defgeneric fcll:lambda-list-keyword (object))

(defmethod fcll:lambda-list-keyword ((lambda-list-keyword fcll:lambda-list-keyword))
  lambda-list-keyword)

(defmethod fcll:lambda-list-keyword ((name symbol))
  (defsys:locate 'fcll:lambda-list-keyword name))

(defmethod defsys:locate ((system lambda-list-keyword-definitions) (name fcll:lambda-list-keyword) &rest keys)
  (declare (ignore keys))
  name)

(defgeneric section-class (object))

(defclass fcll:standard-lambda-list-keyword (fcll:lambda-list-keyword parent-mixin defsys:name-mixin standard-inheritable-slots-object)
  ((%arity :initarg :arity
           :reader arity
           :inherit t)
   (%introducer :initarg :introducer
                :reader introducer
                :inherit t)
   (%parameter-parser :initarg :parameter-parser
                      :reader parameter-parser
                      :type (or function symbol)
                      :inherit t)
   (%recursablep :initarg :recursablep
                 :reader recursablep
                 :type boolean
                 :initform nil
                 :inherit t)
   (%parser-maker :initarg :parser-maker
                  :reader %parser-maker
                  :type (or function symbol)
                  :initform '%make-lambda-list-keyword-parser
                  :inherit t)
   (%section-class :initarg :section-class
                   :reader section-class
                   :type class
                   :inherit t)
   (%parser :reader parser
            :type function))
  (:metaclass standard-inheritable-slots-class))

(defmethod shared-initialize :around ((instance fcll:standard-lambda-list-keyword) slot-names &rest initargs &key parent &allow-other-keys)
  (if parent
      (apply #'call-next-method instance slot-names :parent (lambda-list-keyword parent) initargs)
      (call-next-method)))

(defmethod shared-initialize :after ((instance fcll:standard-lambda-list-keyword) slot-names &key)
  (declare (ignore slot-names))
  (unless (slot-boundp instance '%introducer)
    (setf (slot-value instance '%introducer) (defsys:name instance)))
  (unless (slot-boundp instance '%parameter-parser)
    (setf (slot-value instance '%parameter-parser)
          (let ((arity (arity instance)))
            (ecase arity
              (0 nil)
              (1 (if (recursablep instance)
                     #'%parse-simple-recursable-parameter
                     #'%parse-simple-parameter))
              (t (error "Must supply a parameter-parser for arity ~S." arity))))))
  (setf (slot-value instance '%parser)
        (funcall (%parser-maker instance) instance)))
