(in-package #:first-class-lambda-lists)

(defclass fcll:lambda-list-kind () ())

(defclass fcll:standard-lambda-list-kind (fcll:lambda-list-kind defsys:name-mixin)
  ((%operator :initarg :operator
              :reader operator)
   (%keywords :initarg :keywords
              :reader keywords
              :type list)
   (%keyword-order :reader keyword-order)
   (%recurse :initarg :recurse
             :reader recurse
             :initform nil)
   (%default :initarg :default
             :reader default
             :initform nil)))

(defun %compute-keyword-order (keywords keyword-order)
  (labels ((recurse (spec)
             (etypecase spec
               (cons (destructuring-bind (operator &rest args) spec
                       (check-type operator (member list or))
                       (let ((args (mapcan (let ((to-splice `(cons (cons (eql ,operator) list)
                                                                   null)))
                                             (lambda (arg)
                                               (let ((result (recurse arg)))
                                                 (if (typep result to-splice)
                                                     (rest (first result))
                                                     result))))
                                           args)))
                         (case (length args)
                           (0 nil)
                           (1 args)
                           (t (list (cons operator args)))))))
               (symbol (when (member spec keywords :key #'defsys:name :test #'eq)
                         (list spec))))))
    (first (recurse keyword-order))))

(defmethod shared-initialize :after ((kind fcll:standard-lambda-list-kind) slot-names &key)
  (let ((keywords (mapcar (%make-keyword-canonicalizer) (slot-value kind '%keywords))))
    (setf (slot-value kind '%keywords)
          keywords
          (slot-value kind '%keyword-order)
          (%compute-keyword-order
           keywords
           (specification (defsys:locate 'fcll:lambda-list-keyword-order :standard))))))

(defun %derive-keywords-list (&key (from :ordinary) add remove replace)
  (let ((canonicalize (%make-keyword-canonicalizer)))
    (flet ((listify (object)
             (if (listp object)
                 object
                 (list object))))
      (multiple-value-bind (replace-add replace-remove)
          (let ((add nil)
                (remove nil))
            (dolist (cons replace)
              (push (funcall canonicalize (first cons)) remove)
              (push (funcall canonicalize (second cons)) add))
            (values (nreverse add) (nreverse remove)))
        (let ((inherited (keywords (defsys:locate *lambda-list-kind-definitions* from)))
              (add (nconc (mapcar canonicalize (listify add)) replace-add))
              (remove (nconc (mapcar canonicalize (listify remove)) replace-remove)))
          (let ((shared (intersection add remove :test #'%keyword=)))
            (when shared
              (error "Cannot both add and remove the same lambda list keywords: ~S" shared)))
          (let ((overadd (intersection inherited add :test #'%keyword=)))
            (when overadd
              (warn "Tried to add already inherited lambda list keywords: ~S" overadd)))
          (let ((overremove (set-difference remove inherited :test #'%keyword=)))
            (when overremove
              (warn "Tried to remove already not inherited lambda list keywords: ~S" overremove)))
          (union (set-difference inherited remove :test #'%keyword=) add :test #'%keyword=))))))

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
  :recurse :self)

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
  :default '*)

(define (fcll:lambda-list-kind :define-modify-macro) define-modify-macro
  (:derive :remove (&key &aux)))

(define (fcll:lambda-list-kind :define-method-combination-arguments) define-method-combination
  (:derive :add &whole))
