(in-package #:first-class-lambda-lists)

(eval-when t

  (defclass lambda-list-kind-definitions (defsys:standard-system)
    ())

  (defvar *lambda-list-kind-definitions*
    (make-instance 'lambda-list-kind-definitions :name 'fcll:lambda-list-kind))

  (setf (defsys:locate (defsys:root-system) 'fcll:lambda-list-kind)
        *lambda-list-kind-definitions*)

  (defun %ensure-lambda-list-kind (name operator raw-keywords-list &rest initargs)
    (apply #'%ensure-definition *lambda-list-kind-definitions* name
           'fcll:standard-lambda-list-kind
           :operator operator :raw-keywords-list raw-keywords-list initargs))

  (defun %derive-keywords-list (&key (from :ordinary) add remove replace)
    (let ((from (and from (raw-keywords-list (lambda-list-kind from)))))
      (multiple-value-call #'make-instance 'standard-raw-lambda-list-keywords-list
                           :keywords-set (make-instance 'fcll:derived-lambda-list-keywords-set
                                                        :keywords-set (and from (keywords-set from))
                                                        :add add
                                                        :remove remove
                                                        :replace replace)
                           (if from
                               (values :keyword-order (keyword-order from)
                                       :keyword-conflicts (keyword-conflicts from))
                               (values)))))

  (defmethod defsys:expand-definition ((system lambda-list-kind-definitions) name environment args &key)
    (declare (ignore environment))
    (destructuring-bind (operator keywords &rest args) args
      (let ((keywords-list
             (etypecase keywords
               ((cons (eql :derive) list)
                `(%derive-keywords-list ,@(mapcan (let ((processp t))
                                                    (lambda (key value)
                                                      (prog1 (when processp
                                                               (list key `',value))
                                                        (setf processp (not processp)))))
                                                  (cdr keywords)
                                                  (cddr keywords))))
               (list
                `(%derive-keywords-list :from nil :add ',keywords)))))
        `(%ensure-lambda-list-kind ',name ',operator ,keywords-list ,@args)))))


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
   (%raw-keywords-list :initarg :raw-keywords-list
                       :reader raw-keywords-list
                       :type raw-lambda-list-keywords-list)
   (%keywords-list :initarg :keywords-list
                   :reader keywords-list
                   :type coherent-lambda-list-keywords-list)
   (%recurse :initarg :recurse ;Canonicalized in (shared-initialize :around).
             :reader recurse
             :type (or null fcll:lambda-list-kind)
             :initform nil)
   (%default-initform :initarg :default-initform
                      :reader default-initform
                      :initform nil)
   (%parser :reader parser)))

(defclass effective-lambda-list-keywords-list (coherent-lambda-list-keywords-list)
  ())

(defclass standard-effective-lambda-list-keywords-list (effective-lambda-list-keywords-list
                                                        mapped-lambda-list-keywords-list)
  ())

(defmethod shared-initialize :after ((keywords-list standard-effective-lambda-list-keywords-list) slot-names &key)
  (declare (ignore slot-names))
  (let ((keyword-conflicts (tree (keyword-conflicts keywords-list))))
    (dolist (lambda-list-keyword (lambda-list-keywords (keywords-set keywords-list)))
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

(define-condition fcll:lambda-list-keywords-conflict (fcll:malformed-lambda-list)
  ((%keywords :initarg :lambda-list-keywords
              :reader lambda-list-keywords))
  (:report (lambda (condition stream)
             (format stream "The following lambda list keywords conflict:~%~S"
                     (lambda-list-keywords condition)))))

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

(defun %make-parser (keywords-list parse-recursable-variable)
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
           (recurse (tree (keyword-order keywords-list))))))
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
                                                         donep: ~S~%sections: ~S"
                                        :format-arguments (list donep sections))
                sections)))))))

(defmethod shared-initialize :around ((kind fcll:standard-lambda-list-kind) slot-names
                                      &rest initargs &key
                                                       (raw-keywords-list nil raw-keywords-list-p)
                                                       (recurse nil recurse-p))
  (let ((canonicalized
         (nconc
          (when raw-keywords-list-p
            (list :keywords-list
                  (make-instance 'standard-effective-lambda-list-keywords-list
                                 :keywords-list (coherent-lambda-list-keywords-list raw-keywords-list)
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
        (let ((keywords-list (keywords-list kind)))
          (%make-parser keywords-list
                        (let ((recursive-lambda-list-kind (recurse kind)))
                          (if recursive-lambda-list-kind
                              (lambda (variable)
                                (make-instance 'standard-lambda-list
                                               :kind recursive-lambda-list-kind
                                               :parse variable))
                              (lambda (variable)
                                (declare (ignore variable))
                                (error "Tried to parse a recursable variable in a non-recursive context."))))))))
