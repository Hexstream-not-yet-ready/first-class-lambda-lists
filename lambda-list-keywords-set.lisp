(in-package #:first-class-lambda-lists)

(defclass lambda-list-keywords-set () ())

(defgeneric lambda-list-keywords-set (object))

(defmethod lambda-list-keywords-set ((keywords-set lambda-list-keywords-set))
  keywords-set)

(defmethod print-object ((keywords-set lambda-list-keywords-set) stream)
  (print-unreadable-object (keywords-set stream :type t)
    (prin1 (lambda-list-keywords keywords-set) stream)))

(defclass derived-lambda-list-keywords-set (lambda-list-keywords-set)
  ((%keywords-set :initarg :keywords-set
                  :reader keywords-set
                  :type (or null lambda-list-keywords-set)
                  :initform nil)
   (%add :reader add
         :type list
         :initform nil)
   (%remove :reader remove
            :type list
            :initform nil)
   (%replace :reader replace
             :type list
             :initform nil)
   (%lambda-list-keywords :reader lambda-list-keywords
                          :type list)))

(defmethod shared-initialize :after ((keywords-set derived-lambda-list-keywords-set) slot-names &key add remove replace)
  (flet ((listify (object)
           (if (listp object)
               object
               (list object))))
    (let ((add (mapcar #'lambda-list-keyword (listify add)))
          (remove (mapcar #'lambda-list-keyword (listify remove)))
          (replace (mapcar (lambda (to-replace)
                             (destructuring-bind (remove add) to-replace
                               (list (lambda-list-keyword remove)
                                     (lambda-list-keyword add))))
                           replace)))
      (setf (slot-value keywords-set '%add)
            add
            (slot-value keywords-set '%remove)
            remove
            (slot-value keywords-set '%replace)
            replace
            (slot-value keywords-set '%lambda-list-keywords)
            (%compute-lambda-list-keywords (keywords-set keywords-set) add remove replace)))))

(defun %compute-lambda-list-keywords (base add remove replace)
  (let ((inherited (and base (print (lambda-list-keywords base))))
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

(defclass subordinate-lambda-list-keywords-set (lambda-list-keywords-set)
  ((%owner :initarg :owner
           :reader owner
           :initform nil)
   (%keywords-set :initarg :keywords-set
                  :reader keywords-set
                  :type lambda-list-keywords-set
                  :initform (error "Must supply :keywords-set argument."))))

(defmethod lambda-list-keywords ((keywords-set subordinate-lambda-list-keywords-set))
  (lambda-list-keywords (keywords-set keywords-set)))
