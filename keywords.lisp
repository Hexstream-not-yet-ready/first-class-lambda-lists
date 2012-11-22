(in-package #:first-class-lambda-keywords)

(defmacro fc-lk:define (name &body options)
  (declare (ignore name options)))

(defmacro fc-lk:conflicts ((first-lambda-keyword second-lambda-keyword))
  (declare (ignore first-lambda-keyword second-lambda-keyword)))

(defmacro fc-lk:precedes ((first-lambda-keyword second-lambda-keyword))
  (declare (ignore first-lambda-keyword second-lambda-keyword)))

(defmacro fc-lk:modifies ((flag-lambda-keyword modified-lambda-keyword))
  (declare (ignore flag-lambda-keyword modified-lambda-keyword)))


(fc-lk:define &whole
  (:min-arguments 1)
  (:max-arguments 1))

(fc-lk:define &environment
  (:min-arguments 1)
  (:max-arguments 1))

(fc-lk:define :required
  (:introducer nil)
  (:min-clusters 1))

(fc-lk:define &optional
  (:defaultp t)
  (:suppliedp t))

(fc-lk:define &rest
  (:min-arguments 1)
  (:max-arguments 1))
(fc-lk:define &body
  (:min-arguments 1)
  (:max-arguments 1))
(fc-lk:conflicts (&rest &body))

(fc-lk:define &key ()
  (:defaultp t)
  (:suppliedp t))
(fc-lk:define &allow-other-keys
  (:top-level-p nil))
(fc-lk:modifies (&allow-other-keys &key))

(fc-lk:define &aux
  )


(fc-lk:precedes (&whole &environment))
(fc-lk:precedes (&whole :required))
(fc-lk:precedes (:required &optional))
(fc-lk:precedes (&optional &rest))
(fc-lk:precedes (&optional &body))
(fc-lk:precedes (&rest &key))
(fc-lk:precedes (&body &key))
(fc-lk:precedes (&key &aux))
