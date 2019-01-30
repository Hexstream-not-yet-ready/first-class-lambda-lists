(in-package #:first-class-lambda-lists)

(defclass fcll:lambda-list-keywords-set () ())

(defgeneric fcll:lambda-list-keywords-set (object))

(defmethod fcll:lambda-list-keywords-set ((keywords-set fcll:lambda-list-keywords-set))
  keywords-set)

(defmethod print-object ((keywords-set fcll:lambda-list-keywords-set) stream)
  (print-unreadable-object (keywords-set stream :type t)
    (prin1 (lambda-list-keywords keywords-set) stream)))

(defgeneric lambda-list-keywords (keywords-set))

(defclass fcll:derived-lambda-list-keywords-set (lambda-list-keywords-set)
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
             :initform nil)
   (%lambda-list-keywords :reader lambda-list-keywords
                          :type list)))

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

(defmethod shared-initialize :after ((keywords-set fcll:derived-lambda-list-keywords-set) slot-names &key)
  (declare (ignore slot-names))
  (setf (slot-value keywords-set '%lambda-list-keywords)
        (%compute-lambda-list-keywords (keywords-set keywords-set)
                                       (add keywords-set)
                                       (remove keywords-set)
                                       (replace keywords-set))))

(defun %compute-lambda-list-keywords (base add remove replace)
  (let ((inherited (and base (lambda-list-keywords base)))
        (add (append add (mapcar #'second replace)))
        (remove (append remove (mapcar #'first replace))))
    (let ((shared (intersection add remove :test #'eq)))
      (when shared
        (error "Cannot both add and remove the same lambda list keywords: ~S" shared)))
    (let ((overadd (intersection inherited add :test #'eq)))
      (when overadd
        (warn "Tried to add already inherited lambda list keywords: ~S" overadd)))
    (let ((overremove (set-difference remove inherited :test #'eq)))
      (when overremove
        (warn "Tried to remove already not inherited lambda list keywords: ~S" overremove)))
    (union (set-difference inherited remove :test #'eq) add :test #'eq)))
