(in-package #:first-class-lambda-keywords)

(defclass fc-lk:keyword () ())

(defgeneric fc-lk:name (lambda-keyword))

(defgeneric fc-lk:arity (lambda-keyword)
  (:method ((lambda-keyword fc-lk:keyword))
    (values 0 nil)))

(defgeneric fc-lk:introducer (lambda-keyword)
  (:method ((lambda-keyword fc-lk:keyword))
    (fc-lk:name lambda-keyword)))

(defgeneric fc-lk:specializerp (lambda-keyword))

(defgeneric fc-lk:default (lambda-keyword)
  (:method ((lambda-keyword fc-lk:keyword))
    (values nil nil)))
(defun fc-lk:defaultp (lambda-keyword)
  (nth-value 1 (fc-lk:default lambda-keyword)))

(defgeneric fc-lk:suppliedp (lambda-keyword)
  (:method ((lambda-keyword fc-lk:keyword))
    (fc-lk:defaultp lambda-keyword)))

(defgeneric fc-lk:keyword-name-p (lambda-keyword)
  (:method ((lambda-keyword fc-lk:keyword))
    nil))

(defgeneric fc-lk:conflicts (lambda-keyword))
(defgeneric fc-lk:order (lambda-keyword))
(defun fc-lk:precedes (lambda-keyword)
  (identity (fc-lk:order lambda-keyword)))
(defun fc-lk:follows (lambda-keyword)
  (nth-value 1 (fc-lk:order lambda-keyword)))
(defgeneric fc-lk:modifies (lambda-keyword))


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


(fc-lk:define-order (&whole :required &optional &rest &key &aux))
(fc-lk:define-order (&whole &environment))


;;; "Extended" lambda list keywords

(fc-lk:define &transform)

(fc-lk:define &doc)
(fc-lk:define &decl)
(fc-lk:define-order ((or &doc &decl) &rest (:followers &rest)))

(fc-lk:define &rest+)
(fc-lk:define &body+
  (:alias-for &rest+))
(fc-lk:define-conflicts (&rest+ &rest))

(fc-lk:define &head)
(fc-lk:define &tail)

(fc-lk:define &destructure)
