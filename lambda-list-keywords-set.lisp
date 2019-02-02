(in-package #:first-class-lambda-lists)

(defclass fcll:lambda-list-keywords-set () ())

(defgeneric fcll:lambda-list-keywords-set (object))

(defmethod fcll:lambda-list-keywords-set ((keywords-set fcll:lambda-list-keywords-set))
  keywords-set)

(defmethod print-object ((keywords-set fcll:lambda-list-keywords-set) stream)
  (print-unreadable-object (keywords-set stream :type t)
    (prin1 (lambda-list-keywords keywords-set) stream)))

(defgeneric lambda-list-keywords (keywords-set))

(defclass lambda-list-keywords-mixin ()
  ((%lambda-list-keywords :reader lambda-list-keywords
                          :type list)))

(defgeneric %compute-lambda-list-keywords (object))

(defmethod shared-initialize :after ((instance lambda-list-keywords-mixin) slot-names &key)
  (declare (ignore slot-names))
  (setf (slot-value instance '%lambda-list-keywords)
        (%compute-lambda-list-keywords instance)))

(defclass fcll:derived-lambda-list-keywords-set (lambda-list-keywords-set lambda-list-keywords-mixin)
  ((%keywords-set :initarg :keywords-set
                  :reader keywords-set
                  :type (or null fcll:lambda-list-keywords-set)
                  :initform nil)
   (%add :initarg :add
         :reader add
         :type list
         :initform nil)
   (%remove :initarg :remove
            :reader remove
            :type list
            :initform nil)
   (%replace :initarg :replace
             :reader replace
             :type list
             :initform nil)))

(defmethod shared-initialize :around ((keywords-set fcll:derived-lambda-list-keywords-set) slot-names &rest initargs
                                      &key (add nil addp) (remove nil removep) (replace nil replacep))
  (let ((canonicalized
         (flet ((listify (object)
                  (if (listp object)
                      object
                      (list object))))
           (nconc (when addp
                    (list :add (mapcar #'lambda-list-keyword (listify add))))
                  (when removep
                    (list :remove (mapcar #'lambda-list-keyword (listify remove))))
                  (when replacep
                    (list :replace (mapcar (lambda (to-replace)
                                             (destructuring-bind (remove add) to-replace
                                               (list (lambda-list-keyword remove)
                                                     (lambda-list-keyword add))))
                                           replace)))))))
    (if canonicalized
        (apply #'call-next-method keywords-set slot-names
               (nconc canonicalized initargs))
        (call-next-method))))

(defmethod %compute-lambda-list-keywords ((keywords-set fcll:derived-lambda-list-keywords-set))
  (let ((inherited (let ((base (keywords-set keywords-set)))
                     (and base (lambda-list-keywords base)))))
    (multiple-value-bind (add remove)
        (let ((replace (replace keywords-set)))
          (values (append (add keywords-set) (mapcar #'second replace))
                  (append (remove keywords-set) (mapcar #'first replace))))
      (let ((shared (intersection add remove :test #'eq)))
        (when shared
          (error "Cannot both add and remove the same lambda list keywords: ~S" shared)))
      (let ((overadd (intersection inherited add :test #'eq)))
        (when overadd
          (warn "Tried to add already inherited lambda list keywords: ~S" overadd)))
      (let ((overremove (set-difference remove inherited :test #'eq)))
        (when overremove
          (warn "Tried to remove already not inherited lambda list keywords: ~S" overremove)))
      (union (set-difference inherited remove :test #'eq) add :test #'eq))))
