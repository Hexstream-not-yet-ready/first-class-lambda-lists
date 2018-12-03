(in-package #:first-class-lambda-lists)

(defclass fcll:lambda-list-kind () ())

(defclass fcll:standard-lambda-list-kind (fcll:lambda-list-kind)
  ((%name :initarg :name
          :reader name)
   (%operator :initarg :operator
              :reader operator)
   (%keywords :initarg :keywords
              :reader keywords)
   (%recurse :initarg :recurse
             :reader recurse
             :initform nil)
   (%default :initarg :default
             :reader default
             :initform nil)))

(defun %derive-keywords-list (&key (base :ordinary) add remove replace)
  (declare (ignore base add remove replace))
  nil)

(defun %no-defaulting (base)
  (declare (ignore base))
  nil)

(make-instance 'fcll:standard-lambda-list-kind
               :name :ordinary
               :operator 'lambda
               :keywords '(:required &optional &rest &key &aux)) ;&allow-other-keys is subordinate to &key, so implied by it.

(make-instance 'fcll:standard-lambda-list-kind
               :name :generic-function
               :operator 'defgeneric
               :keywords (%derive-keywords-list :replace `((&optional ,(%no-defaulting '&optional))
                                                           (&key ,(%no-defaulting '&key)))
                                                :remove '(&aux)))

(make-instance 'fcll:standard-lambda-list-kind
               :name :specialized
               :operator 'defmethod
               :keywords (%derive-keywords-list :replace '((:required :required-specializable))))

(make-instance 'fcll:standard-lambda-list-kind
               :name :destructuring
               :operator 'destructuring-bind
               :keywords (%derive-keywords-list :add '(&whole &body))
               :recurse :self)

(make-instance 'fcll:standard-lambda-list-kind
               :name :macro
               :operator 'defmacro
               :keywords (%derive-keywords-list :base :destructuring :add '(&environment))
               :recurse :destructuring)

;; Boa lambda lists are syntactically the same as ordinary lambda lists.

(make-instance 'fcll:standard-lambda-list-kind
               :name :defsetf
               :operator 'defsetf
               :keywords (%derive-keywords-list :add '(&environment) :remove '(&aux)))

(make-instance 'fcll:standard-lambda-list-kind
               :name :deftype
               :operator 'deftype
               :keywords (%derive-keywords-list :base :macro)
               :recurse :destructuring
               :default '*)

(make-instance 'fcll:standard-lambda-list-kind
               :name :define-modify-macro
               :operator 'define-modify-macro
               :keywords (%derive-keywords-list :remove '(&key &aux)))

(make-instance 'fcll:standard-lambda-list-kind
               :name :define-method-combination-arguments
               :operator 'define-method-combination
               :keywords (%derive-keywords-list :add '(&whole)))
