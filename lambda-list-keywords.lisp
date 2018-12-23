(in-package #:first-class-lambda-lists)

(defclass fcll:lambda-list-keyword () ())

(defmethod defsys:locate ((system lambda-list-keyword-definitions) (name fcll:lambda-list-keyword) &rest keys)
  (declare (ignore keys))
  name)

(defclass fcll:standard-lambda-list-keyword (fcll:lambda-list-keyword defsys:name-mixin)
  ((%arity :initarg :arity
           :reader arity)
   (%introducer :initarg :introducer
                :reader introducer)
   (%parameter-parser :initarg :parameter-parser
                      :reader parameter-parser
                      :type (or function symbol))
   (%parser :reader parser
            :type function)))

(defclass fcll:lambda-list-section () ())

(defclass fcll:standard-lambda-list-section ()
  ((%lambda-list-keyword :initarg :lambda-list-keyword
                         :reader fcll:lambda-list-keyword
                         :type fcll:lambda-list-keyword
                         :initform (error "Must supply lambda list keyword."))
   (%parameters :initarg :parameters
                :reader parameters
                :type list
                :initform nil)))

(defun %apparent-lambda-list-keyword-p (object)
  (let ((symbol-name (and (symbolp object) (symbol-name object))))
    (and (plusp (length symbol-name)) (char= (char symbol-name 0) #\&))))

(defun %make-introducer-parser (introducer parameters-parser)
  (if introducer
      (lambda (tail)
        (check-type tail cons)
        (if (eq (first tail) introducer)
            (funcall parameters-parser (rest tail))
            (values tail nil)))
      (lambda (tail)
        (check-type tail cons)
        (let ((first (first tail)))
          (if (not (%apparent-lambda-list-keyword-p first))
              (funcall parameters-parser (rest tail))
              (values tail nil))))))

(defun %make-parameters-parser (lambda-list-keyword)
  (let ((arity (arity lambda-list-keyword))
        (parameter-parser (parameter-parser lambda-list-keyword)))
    (ecase arity
      (0 (lambda (tail)
           (values tail
                   (list (make-instance 'fcll:standard-lambda-list-section
                                        :lambda-list-keyword lambda-list-keyword)))))
      (1 (lambda (tail)
           (if tail
               (values (rest tail)
                       (list (make-instance 'fcll:standard-lambda-list-section
                                            :lambda-list-keyword lambda-list-keyword
                                            :parameters (list (funcall parameter-parser (first tail))))))
               (error "Lambda list keyword ~S expected an argument." lambda-list-keyword))))
      ((t) (lambda (tail)
             (let* ((end (member-if #'%apparent-lambda-list-keyword-p tail))
                    (head (ldiff tail end)))
               (values end (make-instance 'fcll:standard-lambda-list-section
                                          :lambda-list-keyword lambda-list-keyword
                                          :parameters (map-into head parameter-parser head)))))))))

(defun %make-lambda-list-keyword-parser (lambda-list-keyword)
  (%make-introducer-parser (introducer lambda-list-keyword)
                           (%make-parameters-parser lambda-list-keyword)))

(defmethod shared-initialize :after ((instance fcll:standard-lambda-list-keyword) slot-names &key)
  (unless (slot-boundp instance '%introducer)
    (setf (slot-value instance '%introducer) (defsys:name instance)))
  (unless (slot-boundp instance '%parameter-parser)
    (setf (slot-value instance '%parameter-parser)
          (let ((arity (arity instance)))
            (ecase arity
              (0 nil)
              (1 #'%parse-simple-parameter)
              (t (error "Must supply a parameter-parser for arity ~S." arity))))))
  (setf (slot-value instance '%parser)
        (%make-lambda-list-keyword-parser instance)))

(defun %make-keyword-canonicalizer ()
  (let ((defs *lambda-list-keyword-definitions*))
    (lambda (designator)
      (defsys:locate defs designator))))

(defun %keyword= (a b)
  (check-type a fcll:lambda-list-keyword)
  (check-type b fcll:lambda-list-keyword)
  (eq (defsys:name a) (defsys:name b)))

(define (fcll:lambda-list-keyword &whole) 1)

(define (fcll:lambda-list-keyword :&environment-not-before-&whole) 1
  :introducer '&environment)

(define (fcll:lambda-list-keyword :&environment-last) 1
  :introducer '&environment)

(define (fcll:lambda-list-keyword :required) t
  :introducer nil
  :parameter-parser #'%parse-required-parameter)

(define (fcll:lambda-list-keyword :required-specializable) t
  :introducer nil
  :parameter-parser #'%parse-specializable-parameter)

(define (fcll:lambda-list-keyword &optional) t
  :parameter-parser #'%parse-optional-parameter)

(define (fcll:lambda-list-keyword :&optional-no-defaulting) t
  :introducer '&optional
  :parameter-parser #'%parse-optional-no-defaulting-parameter)

(define (fcll:lambda-list-keyword &rest) 1)

(define (fcll:lambda-list-keyword &body) 1)

(define (fcll:lambda-list-keyword &key) t
  :parameter-parser #'%parse-key-parameter)

(define (fcll:lambda-list-keyword :&key-no-defaulting) t
  :introducer '&key
  :parameter-parser #'%parse-key-no-defaulting-parameter)

(define (fcll:lambda-list-keyword &allow-other-keys) 0)

(define (fcll:lambda-list-keyword &aux) t
  :parameter-parser #'%parse-aux-parameter)
