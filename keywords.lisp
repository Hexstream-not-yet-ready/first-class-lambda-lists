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


(defmacro fc-lk:define (name &body options)
  (declare (ignore name options)))

(defmacro fc-lk:conflicts ((&rest lambda-keywords))
  (declare (ignore lambda-keywords)))

(defmacro fc-lk:precedes ((first-lambda-keyword second-lambda-keyword))
  (declare (ignore first-lambda-keyword second-lambda-keyword)))

(defmacro fc-lk:modifies ((flag-lambda-keyword modified-lambda-keyword))
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
  (:arity 1))
(fc-lk:conflicts (&rest &body))

(fc-lk:define &key
  (:default nil))
(fc-lk:define &allow-other-keys
  (:arity 0)
  (:top-level-p nil))
(fc-lk:modifies (&allow-other-keys &key))

(fc-lk:define &aux
  (:default nil))


(fc-lk:precedes (&whole &environment))
(fc-lk:precedes (&whole :required))
(fc-lk:precedes (:required &optional))
(fc-lk:precedes (&optional &rest))
(fc-lk:precedes (&optional &body))
(fc-lk:precedes (&rest &key))
(fc-lk:precedes (&body &key))
(fc-lk:precedes (&key &aux))


;;; "Extended" lambda list keywords

(fc-lk:define &transform)

(fc-lk:define &doc)
(fc-lk:define &decl)

(fc-lk:define &rest+)
(fc-lk:define &body+)
(fc-lk:conflicts (&rest+ &body+ &rest &body))

(fc-lk:define &head)
(fc-lk:define &tail)

(fc-lk:define &destructure)
