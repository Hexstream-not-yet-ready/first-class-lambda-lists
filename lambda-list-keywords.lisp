(in-package #:first-class-lambda-lists)

(eval-when t

  (defclass lambda-list-keyword-definitions (defsys:standard-system)
    ())

  (defvar *lambda-list-keyword-definitions*
    (make-instance 'lambda-list-keyword-definitions :name 'fcll:lambda-list-keyword))

  (setf (defsys:locate (defsys:root-system) 'fcll:lambda-list-keyword)
        *lambda-list-keyword-definitions*)

  (defun %ensure-definition (definitions-system name definition-class &rest initargs)
    (let ((existing (defsys:locate definitions-system name :errorp nil)))
      (if existing
          (apply #'reinitialize-instance existing initargs)
          (setf (defsys:locate definitions-system name)
                (apply #'make-instance definition-class
                       :name name initargs)))))

  (defun %ensure-lambda-list-keyword (name arity &rest initargs)
    (apply #'%ensure-definition *lambda-list-keyword-definitions* name
           'fcll:standard-lambda-list-keyword
           :arity arity initargs))

  (defmethod defsys:expand-definition ((system lambda-list-keyword-definitions) name environment arity-then-args &key)
    (declare (ignore environment))
    (destructuring-bind (arity &rest args) arity-then-args
      `(%ensure-lambda-list-keyword ',name ,arity ,@args))))


(defclass fcll:lambda-list-keyword () ())

(defgeneric fcll:lambda-list-keyword (object))

(defmethod fcll:lambda-list-keyword ((lambda-list-keyword fcll:lambda-list-keyword))
  lambda-list-keyword)

(defmethod fcll:lambda-list-keyword ((name symbol))
  (defsys:locate *lambda-list-keyword-definitions* name))

(defmethod defsys:locate ((system lambda-list-keyword-definitions) (name fcll:lambda-list-keyword) &rest keys)
  (declare (ignore keys))
  name)

(defclass fcll:standard-lambda-list-keyword (fcll:lambda-list-keyword defsys:name-mixin)
  ((%parent :initarg :parent
            :reader parent
            :initform nil)
   (%arity :initarg :arity
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
   (%parser :reader parser
            :type function))
  (:metaclass standard-inheritable-slots-class))

(defmethod slot-inherited-value-using-class ((class standard-inheritable-slots-class)
                                             (object fcll:standard-lambda-list-keyword)
                                             slot)
  (let ((parent (parent object)))
    (if parent
        (values (slot-value parent (c2mop:slot-definition-name slot))
                t)
        (values nil nil))))

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

(defmethod fcll:unparse ((section fcll:standard-lambda-list-section))
  (let ((introducer (introducer (fcll:lambda-list-keyword section)))
        (parameters (mapcar #'fcll:unparse (parameters section))))
    (if introducer
        (cons introducer parameters)
        parameters)))

(defmethod print-object ((section fcll:standard-lambda-list-section) stream)
  (print-unreadable-object (section stream :type t)
    (format stream "~S~{ ~S~}"
            (defsys:name (fcll:lambda-list-keyword section))
            (mapcar #'fcll:unparse (parameters section)))))

(defclass standard-&key-section (fcll:standard-lambda-list-section)
  ((%allow-other-keys-p :initarg :allow-other-keys-p
                        :reader allow-other-keys-p
                        :type boolean
                        :initform nil)))

(defmethod fcll:unparse ((section standard-&key-section))
  (if (allow-other-keys-p section)
      (append (call-next-method) '(&allow-other-keys))
      (call-next-method)))

(defun %apparent-lambda-list-keyword-p (object)
  (let ((symbol-name (and (symbolp object) (symbol-name object))))
    (and (plusp (length symbol-name)) (char= (char symbol-name 0) #\&))))

(defun %make-introducer-parser (introducer parameters-parser)
  (if introducer
      (lambda (tail)
        (check-type tail cons)
        (if (eq (first tail) introducer)
            (funcall parameters-parser (rest tail))
            tail))
      (lambda (tail)
        (check-type tail cons)
        (let ((first (first tail)))
          (if (not (%apparent-lambda-list-keyword-p first))
              (funcall parameters-parser tail)
              tail)))))

(defvar *sections*)

(defun %add-section (section)
  (push section *sections*))

(define-condition fcll:malformed-lambda-list (error)
  ((%root-lambda-list :initarg :root-lambda-list
                      :reader root-lambda-list)
   (%specification :initarg :specification
                   :reader specification)
   (%tail :initarg :tail
          :reader tail)))

(define-condition simple-malformed-lambda-list-error (fcll:malformed-lambda-list simple-error)
  ())

(defvar *%malformed-lambda-list*)

(defun %malformed-lambda-list (error-type &rest args)
  (apply *%malformed-lambda-list* error-type args))

(defun %make-parameters-parser (lambda-list-keyword)
  (let ((arity (arity lambda-list-keyword))
        (parameter-parser (parameter-parser lambda-list-keyword)))
    (ecase arity
      (0 (lambda (tail)
           (%add-section (make-instance 'fcll:standard-lambda-list-section
                                        :lambda-list-keyword lambda-list-keyword))
           tail))
      (1 (lambda (tail)
           (if tail
               (progn (%add-section
                       (make-instance 'fcll:standard-lambda-list-section
                                      :lambda-list-keyword lambda-list-keyword
                                      :parameters (list (funcall parameter-parser (first tail)))))
                      (rest tail))
               (%malformed-lambda-list 'simple-malformed-lambda-list-error
                                       :tail tail
                                       :format-control "Lambda list keyword ~S expected an argument."
                                       :format-arguments (list lambda-list-keyword)))))
      ((t) (lambda (tail)
             (let* ((end (member-if #'%apparent-lambda-list-keyword-p tail))
                    (head (ldiff tail end)))
               (%add-section (make-instance 'fcll:standard-lambda-list-section
                                            :lambda-list-keyword lambda-list-keyword
                                            :parameters (map-into head parameter-parser head)))
               end))))))

(defun %make-lambda-list-keyword-parser (lambda-list-keyword)
  (%make-introducer-parser (introducer lambda-list-keyword)
                           (%make-parameters-parser lambda-list-keyword)))

(defun %make-&key-parser (lambda-list-keyword)
  (%make-introducer-parser
   (introducer lambda-list-keyword)
   (let ((parameter-parser (parameter-parser lambda-list-keyword)))
     (lambda (tail)
       (let* ((end (member-if #'%apparent-lambda-list-keyword-p tail))
              (head (ldiff tail end))
              (allow-other-keys-p (when (eq (first end) '&allow-other-keys)
                                    (setf end (rest end))
                                    t)))
         (%add-section (make-instance 'standard-&key-section
                                      :lambda-list-keyword lambda-list-keyword
                                      :parameters (map-into head parameter-parser head)
                                      :allow-other-keys-p allow-other-keys-p))
         end)))))

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


(define (fcll:lambda-list-keyword &whole) 1)

(define (fcll:lambda-list-keyword :&environment-not-before-&whole) 1
  :introducer '&environment)

(define (fcll:lambda-list-keyword :&environment-last) 1
  :introducer '&environment)

(define (fcll:lambda-list-keyword :required) t
  :introducer nil
  :parameter-parser #'%parse-required-parameter
  :recursablep t)

(define (fcll:lambda-list-keyword :required-specializable) t
  :parent :required
  :parameter-parser #'%parse-specializable-parameter)

(define (fcll:lambda-list-keyword &optional) t
  :parameter-parser #'%parse-optional-parameter
  :recursablep t)

(define (fcll:lambda-list-keyword :&optional-no-defaulting) t
  :parent '&optional
  :parameter-parser #'%parse-optional-no-defaulting-parameter)

(define (fcll:lambda-list-keyword &rest) 1
  :recursablep t)

(define (fcll:lambda-list-keyword &body) 1
  :recursablep t)

(define (fcll:lambda-list-keyword &key) t
  :parameter-parser #'%parse-key-parameter
  :recursablep t
  :parser-maker '%make-&key-parser)

(define (fcll:lambda-list-keyword :&key-no-defaulting) t
  :parent '&key
  :parser-maker '%make-lambda-list-keyword-parser
  :parameter-parser #'%parse-key-no-defaulting-parameter)

(define (fcll:lambda-list-keyword &allow-other-keys) 0)

(define (fcll:lambda-list-keyword &aux) t
  :parameter-parser #'%parse-aux-parameter)
