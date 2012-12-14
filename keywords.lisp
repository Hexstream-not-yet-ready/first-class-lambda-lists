(in-package #:first-class-lambda-keywords)

(defclass fc-lk:keyword () ())


(defgeneric fc-lk:name (lambda-keyword))

(defclass fc-lk:name-mixin ()
  ((%name :initarg :name
          :reader fc-lk:name
          :type (and symbol (not null)))))


(defgeneric fc-lk:arity (lambda-keyword))

(deftype %arity ()
  '(cons (integer 0) (cons (or (integer 0) null) null)))

(defclass fc-lk:arity-mixin ()
  ((%arity :type %arity
           :initform (list 0 nil))))

(defmethod fc-lk:arity ((mixin fc-lk:arity-mixin))
  (destructuring-bind (min max) (slot-value mixin '%arity)
    (values min max)))

(defmethod shared-initialize :after ((mixin fc-lk:arity-mixin) slot-names
                                     &key (arity nil arityp))
  (when arityp
    (setf (slot-value mixin '%arity)
          (etypecase arity
            ((integer 0)
             (list arity arity))
            (%arity arity)))))


(defgeneric fc-lk:introducer (lambda-keyword)
  (:method ((lambda-keyword fc-lk:keyword))
    (fc-lk:name lambda-keyword)))

(defclass fc-lk:introducer-mixin ()
  ((%introducer :initarg :introducer
                :type symbol)))

(defmethod fc-lk:introducer ((mixin fc-lk:introducer-mixin))
  (if (slot-boundp mixin '%introducer)
      (slot-value mixin '%introducer)
      (call-next-method)))


(defgeneric fc-lk:specializerp (lambda-keyword))

(defclass fc-lk:specializerp-mixin ()
  ((%specializerp :initarg :specializerp
                  :reader fc-lk:specializerp)))


(defgeneric fc-lk:default (lambda-keyword))

(defun fc-lk:defaultp (lambda-keyword)
  (nth-value 1 (fc-lk:default lambda-keyword)))

(defclass fc-lk:default-mixin ()
  ((%default :initarg :default)))

(defmethod fc-lk:default ((mixin fc-lk:default-mixin))
  (if (slot-boundp mixin '%default)
      (values (slot-value mixin '%default) t)
      (values nil nil)))

(defmethod shared-initialize :after ((mixin fc-lk:default-mixin) slot-names
                                     &key (defaultp nil defaultpp))
  (when defaultpp
    (if defaultp
        (unless (slot-boundp mixin '%default)
          (setf (slot-value mixin '%default) nil))
        (slot-makunbound mixin '%default))))

(defgeneric fc-lk:suppliedp (lambda-keyword)
  (:method ((lambda-keyword fc-lk:keyword))
    (fc-lk:defaultp lambda-keyword)))


(defgeneric fc-lk:keyword-name-p (lambda-keyword)
  (:method ((lambda-keyword fc-lk:keyword))
    nil))

(defgeneric fc-lk:conflicts (lambda-keyword)
  (:method ((lambda-keyword fc-lk:keyword))
    nil))
(defgeneric fc-lk:order (lambda-keyword)
  (:method ((lambda-keyword fc-lk:keyword))
    (values nil nil)))
(defun fc-lk:before (lambda-keyword)
  (identity (fc-lk:order lambda-keyword)))
(defun fc-lk:after (lambda-keyword)
  (nth-value 1 (fc-lk:order lambda-keyword)))
(defgeneric fc-lk:modifiers (lambda-keyword)
  (:method ((lambda-keyword fc-lk:keyword))
    (values nil nil)))
(defun fc-lk:modified-by (lambda-keyword)
  (nth-value 1 (fc-lk:modifiers lambda-keyword)))


(defclass standard-keyword (fc-lk:name-mixin
                            fc-lk:arity-mixin
                            fc-lk:introducer-mixin
                            fc-lk:specializerp-mixin
                            fc-lk:default-mixin)
  ())


(defmacro fc-lk:define (name &body options)
  (declare (ignore name options)))

(defmacro fc-lk:define-conflicts ((&rest lambda-keywords))
  (declare (ignore lambda-keywords)))

(defmacro fc-lk:define-order ((&rest lambda-keywords))
  (declare (ignore lambda-keywords)))

(defmacro fc-lk:define-modifies ((flag-lambda-keyword modified-lambda-keyword))
  (declare (ignore flag-lambda-keyword modified-lambda-keyword)))


;;; Standard lambda list keywords.

(fc-lk:define &whole
  (:arity 1))

(fc-lk:define &environment
  (:arity 1))

(fc-lk:define :required
  (:introducer nil))

(fc-lk:define &optional
  (:default nil))

(fc-lk:define &rest
  (:arity 1))
(fc-lk:define &body
  (:alias-for &rest))

(fc-lk:define &key
  (:default nil))
(fc-lk:define &allow-other-keys
  (:arity 0)
  (:top-level-p nil))
(fc-lk:define-modifies (&allow-other-keys &key))

(fc-lk:define &aux
  (:default nil))

;; Other options: (:defaultp :specializerp :recurse)


(fc-lk:define-order (&whole :required &optional &rest &key &aux))
(fc-lk:define-order (&whole &environment))


;;; "Extended" lambda list keywords

(fc-lk:define &transform)

(fc-lk:define &doc)
(fc-lk:define &decl)
(fc-lk:define-order ((or &doc &decl) &rest (:after &rest)))

(fc-lk:define &rest+)
(fc-lk:define &body+
  (:alias-for &rest+))
(fc-lk:define-conflicts (&rest+ &rest))

(fc-lk:define &head)
(fc-lk:define &tail)

(fc-lk:define &destructure)
