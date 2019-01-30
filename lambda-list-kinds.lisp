(in-package #:first-class-lambda-lists)

(eval-when t

  (defclass lambda-list-kind-definitions (defsys:standard-system)
    ())

  (defvar *lambda-list-kind-definitions*
    (make-instance 'lambda-list-kind-definitions :name 'fcll:lambda-list-kind))

  (setf (defsys:locate (defsys:root-system) 'fcll:lambda-list-kind)
        *lambda-list-kind-definitions*)

  (defun %ensure-lambda-list-kind (name operator keywords-list &rest initargs)
    (apply #'%ensure-definition *lambda-list-kind-definitions* name
           'fcll:standard-lambda-list-kind
           :operator operator :keywords-list keywords-list initargs))

  (defun %derive-keywords-set (&key (from :ordinary) add remove replace)
    (make-instance 'fcll:derived-lambda-list-keywords-set
                   :keywords-set (and from (keywords-set (keywords-list (lambda-list-kind from))))
                   :add add
                   :remove remove
                   :replace replace))

  (defmethod defsys:expand-definition ((system lambda-list-kind-definitions) name environment args &key)
    (declare (ignore environment))
    (destructuring-bind (operator keywords &rest args) args
      (let ((keywords-expansion
             (etypecase keywords
               ((cons (eql :derive) list)
                `(%derive-keywords-set ,@(mapcan (let ((processp t))
                                                   (lambda (key value)
                                                     (prog1 (when processp
                                                              (list key `',value))
                                                       (setf processp (not processp)))))
                                                 (cdr keywords)
                                                 (cddr keywords))))
               (list
                `(%derive-keywords-set :from nil :add ',keywords)))))
        `(%ensure-lambda-list-kind ',name ',operator
                                   (make-instance 'standard-coherent-lambda-list-keywords-list
                                                  :keywords-list
                                                  (make-instance 'standard-raw-lambda-list-keywords-list
                                                                 :keywords-set ,keywords-expansion))
                                   ,@args)))))


(defclass fcll:lambda-list-kind () ())

(defgeneric fcll:lambda-list-kind (object))

(defmethod fcll:lambda-list-kind ((kind fcll:lambda-list-kind))
  kind)

(defmethod fcll:lambda-list-kind ((name symbol))
  (defsys:locate *lambda-list-kind-definitions* name))

(defmethod defsys:locate ((system lambda-list-kind-definitions) (name fcll:lambda-list-kind) &rest keys)
  (declare (ignore keys))
  name)

(defclass fcll:standard-lambda-list-kind (fcll:lambda-list-kind defsys:name-mixin)
  ((%operator :initarg :operator
              :reader operator)
   (%keywords-list :initarg :keywords-list
                   :reader keywords-list
                   :reader lambda-list-keywords-list
                   :type lambda-list-keywords-list)
   (%recurse :initarg :recurse ;Canonicalized in (shared-initialize :around).
             :reader recurse
             :type (or null fcll:lambda-list-kind)
             :initform nil)
   (%default-initform :initarg :default-initform
                      :reader default-initform
                      :initform nil)
   (%parser :reader parser)))

(defun %make-list-processor-maker (recurse args)
  (let ((arg-processor-makers (mapcar recurse args)))
    (lambda ()
      (let ((arg-processors (mapcar #'funcall arg-processor-makers)))
        (lambda (tail)
          (when tail
            (block nil
              (mapl (lambda (processors)
                      (let ((new-tail (funcall (first processors) tail)))
                        (unless new-tail
                          (setf tail new-tail)
                          (return))
                        (unless (eq new-tail tail)
                          (setf arg-processors (rest processors)
                                tail new-tail))))
                    arg-processors)))
          (values tail (not arg-processors)))))))

(defun %make-or-processor-dispenser (list)
  (let* ((elements (copy-list list))
         (previous-cons (last elements))
         (elements-count (length elements))
         (unproductive-streak 0))
    (nconc elements elements)
    (lambda (tail)
      (block nil
        (multiple-value-bind (new-tail donep)
            (funcall (first elements) tail)
          (let ((next (cdr elements)))
            (when donep
              (when (eq next elements)
                (return (values new-tail t)))
              (setf (rest previous-cons) next
                    unproductive-streak 0)
              (decf elements-count))
            (shiftf previous-cons elements next)
            (when (eq new-tail tail)
              (incf unproductive-streak)
              (when (= unproductive-streak elements-count)
                (return (values new-tail :stuck))))
            (values new-tail nil)))))))

(defun %make-or-processor-maker (recurse args)
  (let ((arg-processor-makers (mapcar (lambda (arg)
                                        (funcall recurse arg t))
                                      args)))
    (lambda ()
      (let ((arg-processor-dispenser (%make-or-processor-dispenser
                                      (mapcar #'funcall arg-processor-makers))))
        (lambda (tail)
          (if tail
              (let ((donep (block nil
                             (loop
                                (multiple-value-bind (new-tail donep)
                                    (funcall arg-processor-dispenser tail)
                                  (setf tail new-tail)
                                  (when (or donep (not tail))
                                    (return donep)))))))
                (values tail (not (eq donep :stuck))))
              (values tail nil t)))))))

(defparameter *root-lambda-list* nil)

(define-condition fcll:lambda-list-keywords-conflict (fcll:malformed-lambda-list)
  ((%keywords :initarg :lambda-list-keywords
              :reader lambda-list-keywords))
  (:report (lambda (condition stream)
             (format stream "The following lambda list keywords conflict:~%~S"
                     (lambda-list-keywords condition)))))

(defun %make-keyword-processor-maker (lambda-list-keyword keyword-conflicts backtrackp)
  (let ((keyword-conflicts (tree keyword-conflicts))
        (inner
         (let ((parser (parser lambda-list-keyword)))
           (if backtrackp
               (lambda (tail)
                 (let ((new-tail (funcall parser tail)))
                   (values new-tail (not (eq new-tail tail)))))
               parser))))
    (if keyword-conflicts
        ;; TODO: Optimize according to lambda list keyword order.
        (let* ((conflicts-with
                (labels ((recurse (spec)
                           (destructuring-bind (operator &rest args) spec
                             (ecase operator
                               (or (delete-duplicates (mapcan #'recurse args)
                                                      :test #'eq))
                               (and (when (member lambda-list-keyword args :test #'eq)
                                      (delete lambda-list-keyword (copy-list args) :test #'eq)))))))
                  (recurse keyword-conflicts)))
               (inner (if conflicts-with
                          ;; TODO: Don't parse introducer twice.
                          (let ((introducer (introducer lambda-list-keyword)))
                            (%make-introducer-parser
                             introducer
                             (lambda (tail)
                               (let ((conflicts (remove-if-not (lambda (lambda-list-keyword)
                                                                 (member lambda-list-keyword
                                                                         conflicts-with
                                                                         :test #'eq))
                                                               *sections*
                                                               :key #'fcll:lambda-list-keyword)))
                                 (if conflicts
                                     (%malformed-lambda-list 'fcll:lambda-list-keywords-conflict
                                                             :lambda-list-keywords conflicts)
                                     (funcall inner (cons introducer tail)))))))
                          inner)))
          (lambda ()
            inner))
        (lambda ()
          inner))))

;;; ↑ WORST. CODE. EVER! ↓

(defun %make-parser (keyword-order keyword-conflicts recursive-lambda-list-kind)
  (labels ((recurse (spec &optional backtrackp)
             (etypecase spec
               (cons (destructuring-bind (operator &rest args) spec
                       (check-type args cons)
                       (ecase operator
                         (list (%make-list-processor-maker #'recurse args))
                         (or (%make-or-processor-maker #'recurse args)))))
               (fcll:lambda-list-keyword
                (%make-keyword-processor-maker spec keyword-conflicts backtrackp)))))
    (let ((parser-maker (recurse (tree keyword-order))))
      (lambda (tail)
        (let ((*sections* nil)
              (*%malformed-lambda-list*
               (lambda (error-type &rest args)
                 (apply #'error error-type
                        :root-lambda-list *root-lambda-list*
                        :specification tail
                        args))))
          (multiple-value-bind (new-tail donep)
              (let* ((parser (funcall parser-maker))
                     (*parse-recursable-variable*
                      (if recursive-lambda-list-kind
                          (lambda (variable)
                            (make-instance 'standard-lambda-list
                                           :kind recursive-lambda-list-kind
                                           :parse variable))
                          (lambda (variable)
                            (declare (ignore variable))
                            (error "Tried to parse a recursable variable in a non-recursive context.")))))
                (funcall parser tail))
            (let ((sections (nreverse *sections*)))
              (if new-tail
                  (%malformed-lambda-list 'simple-malformed-lambda-list-error
                                          :tail new-tail
                                          :format-control "Could not completely parse lambda list.~@
                                                           donep: ~S~%sections: ~S"
                                          :format-arguments (list donep sections))
                  sections))))))))

(defmethod shared-initialize :around ((kind fcll:standard-lambda-list-kind) slot-names
                                      &rest initargs &key (recurse nil recurse-p))
  (let ((canonicalized
         (when (and recurse-p
                    (not (typep recurse '(or null fcll:lambda-list-kind))))
           (list :recurse (if (eq recurse t)
                              kind
                              (fcll:lambda-list-kind recurse))))))
    (if canonicalized
        (apply #'call-next-method kind slot-names (nconc canonicalized initargs))
        (call-next-method))))

(defmethod shared-initialize :after ((kind fcll:standard-lambda-list-kind) slot-names &key)
  (declare (ignore slot-names))
  (setf (slot-value kind '%parser)
        (let ((keywords-list (keywords-list kind)))
          (%make-parser (keyword-order keywords-list)
                        (keyword-conflicts keywords-list)
                        (recurse kind)))))

(define (fcll:lambda-list-kind :ordinary) defun
  (:required &optional &rest &key &aux)) ;&allow-other-keys is subordinate to &key, so implied by it.

(define (fcll:lambda-list-kind :generic-function) defgeneric
  (:derive :replace ((&optional :&optional-no-defaulting)
                     (&key :&key-no-defaulting))
           :remove &aux))

(define (fcll:lambda-list-kind :specialized) defmethod
  (:derive :replace ((:required :required-specializable))))

(define (fcll:lambda-list-kind :destructuring) destructuring-bind
  (:derive :add (&whole &body))
  :recurse t)

(define (fcll:lambda-list-kind :macro) defmacro
  (:derive :from :destructuring :add :&environment-not-before-&whole)
  :recurse :destructuring)

(define (fcll:lambda-list-kind :boa) defstruct
  (:derive))

(define (fcll:lambda-list-kind :defsetf) defsetf
  (:derive :add :&environment-last :remove &aux))

(define (fcll:lambda-list-kind :deftype) deftype
  (:derive :from :macro)
  :recurse :destructuring
  :default-initform ''*)

(define (fcll:lambda-list-kind :define-modify-macro) define-modify-macro
  (:derive :remove (&key &aux)))

(define (fcll:lambda-list-kind :define-method-combination-arguments) define-method-combination
  (:derive :add &whole))
