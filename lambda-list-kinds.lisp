(in-package #:first-class-lambda-lists)

(defclass fcll:lambda-list-kind () ())

(defgeneric fcll:lambda-list-kind (object))

(defmethod fcll:lambda-list-kind ((kind fcll:lambda-list-kind))
  kind)

(defmethod fcll:lambda-list-kind ((name symbol))
  (defsys:locate 'fcll:lambda-list-kind name))

(defmethod defsys:locate ((system lambda-list-kind-definitions) (name fcll:lambda-list-kind) &rest keys)
  (declare (ignore keys))
  name)

(defclass fcll:standard-lambda-list-kind (fcll:lambda-list-kind defsys:name-mixin)
  ((%operator :initarg :operator
              :reader operator)
   (%raw-core :initarg :raw-core
              :reader raw-core
              :type raw-lambda-list-core)
   (%core :initarg :core
          :reader core
          :type coherent-lambda-list-core)
   (%recurse :initarg :recurse ;Canonicalized in (shared-initialize :around).
             :reader recurse
             :type (or null fcll:lambda-list-kind)
             :initform nil)
   (%default-initform :initarg :default-initform
                      :reader default-initform
                      :initform nil)
   (%parser :reader parser)))

(defmethod raw-lambda-list-core ((lambda-list-kind fcll:standard-lambda-list-kind))
  (raw-core lambda-list-kind))

(defmethod raw-lambda-list-core ((lambda-list-kind-name symbol))
  (raw-lambda-list-core (fcll:lambda-list-kind lambda-list-kind-name)))

(defclass effective-lambda-list-core (coherent-lambda-list-core)
  ())

(defclass standard-effective-lambda-list-core (effective-lambda-list-core
                                               mapped-lambda-list-core)
  ())

(defmethod shared-initialize :after ((core standard-effective-lambda-list-core) slot-names &key)
  (declare (ignore slot-names))
  (let ((keyword-conflicts (tree (keyword-conflicts core))))
    (dolist (lambda-list-keyword (lambda-list-keywords (keywords-set core)))
      (setf (slot-value lambda-list-keyword '%conflicts-with)
            (and keyword-conflicts
                 (labels ((recurse (spec)
                            (destructuring-bind (operator &rest args) spec
                              (ecase operator
                                (or (delete-duplicates (mapcan #'recurse args) :test #'eq))
                                (and (when (member lambda-list-keyword args :test #'eq)
                                       (delete lambda-list-keyword (copy-list args) :test #'eq)))))))
                   (recurse keyword-conflicts)))))))

(defclass effective-lambda-list-keyword (fcll:lambda-list-keyword)
  ())

(defclass standard-effective-lambda-list-keyword (fcll:standard-lambda-list-keyword
                                                  effective-lambda-list-keyword)
  ((%conflicts-with :reader conflicts-with))
  (:metaclass standard-inheritable-slots-class))

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


(define-condition fcll:lambda-list-keywords-conflict (fcll:malformed-lambda-list)
  ((%keywords :initarg :lambda-list-keywords
              :reader lambda-list-keywords))
  (:report (lambda (condition stream)
             (format stream "The following lambda list keywords conflict:~%~S"
                     (lambda-list-keywords condition)))))

(defvar *sections*)

(defun %add-section (section)
  (push section *sections*))

(defun %make-keyword-processor-maker (lambda-list-keyword backtrackp)
  (let ((inner
         (let ((parser (parser lambda-list-keyword)))
           (if backtrackp
               (lambda (tail)
                 (let ((new-tail (funcall parser tail)))
                   (values new-tail (not (eq new-tail tail)))))
               parser))))
    ;; TODO: Optimize according to lambda list keyword order.
    (let* ((conflicts-with (conflicts-with lambda-list-keyword))
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
        inner))))

;;; ↑ WORST. CODE. EVER! ↓

(defvar *parse-recursable-variable*)

(defun %make-parser (core parse-recursable-variable)
  (let ((parser-maker
         (labels ((recurse (spec &optional backtrackp)
                    (etypecase spec
                      (cons (destructuring-bind (operator &rest args) spec
                              (check-type args cons)
                              (ecase operator
                                (list (%make-list-processor-maker #'recurse args))
                                (or (%make-or-processor-maker #'recurse args)))))
                      (fcll:lambda-list-keyword
                       (%make-keyword-processor-maker spec backtrackp)))))
           (recurse (tree (keyword-order core))))))
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
                   (*parse-recursable-variable* parse-recursable-variable))
              (funcall parser tail))
          (let ((sections (nreverse *sections*)))
            (if new-tail
                (%malformed-lambda-list 'simple-malformed-lambda-list-error
                                        :tail new-tail
                                        :format-control "Could not completely parse lambda list.~@
                                                         donep: ~S~%sections: ~S~%tail: ~S"
                                        :format-arguments (list donep sections new-tail))
                sections)))))))

(defmethod shared-initialize :around ((kind fcll:standard-lambda-list-kind) slot-names
                                      &rest initargs &key
                                                       (raw-core nil raw-core-p)
                                                       (recurse nil recurse-p))
  (let ((canonicalized
         (nconc
          (when raw-core-p
            (list :core
                  (make-instance 'standard-effective-lambda-list-core
                                 :core (coherent-lambda-list-core raw-core)
                                 :mapper (lambda (parent)
                                           (make-instance 'standard-effective-lambda-list-keyword
                                                          :name (defsys:name parent)
                                                          :parent parent)))))
          (when (and recurse-p
                     (not (typep recurse '(or null fcll:lambda-list-kind))))
            (list :recurse (if (eq recurse t)
                               kind
                               (fcll:lambda-list-kind recurse)))))))
    (if canonicalized
        (apply #'call-next-method kind slot-names (nconc canonicalized initargs))
        (call-next-method))))

(defmethod shared-initialize :after ((kind fcll:standard-lambda-list-kind) slot-names &key)
  (declare (ignore slot-names))
  (setf (slot-value kind '%parser)
        (let ((core (core kind)))
          (%make-parser core
                        (let ((recursive-lambda-list-kind (recurse kind)))
                          (if recursive-lambda-list-kind
                              (lambda (variable)
                                (make-instance 'standard-lambda-list
                                               :kind recursive-lambda-list-kind
                                               :parse variable))
                              (lambda (variable)
                                (declare (ignore variable))
                                (error "Tried to parse a recursable variable in a non-recursive context."))))))))
