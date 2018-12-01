(in-package #:first-class-lambda-lists)

(defclass fcll:lambda-list-kind () ())

(defclass fcll:standard-lambda-list-kind (fcll:lambda-list-kind)
  ((%name :initarg :name
          :reader name)
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
               :keywords '(:required &optional &rest &key &aux)) ;&allow-other-keys is subordinate to &key, so implied by it.

(make-instance 'fcll:standard-lambda-list-kind
               :name :generic-function
               :keywords (%derive-keywords-list :replace `((&optional ,(%no-defaulting '&optional))
                                                           (&key ,(%no-defaulting '&key)))
                                                :remove '(&aux)))

(make-instance 'fcll:standard-lambda-list-kind
               :name :specialized
               :keywords (%derive-keywords-list :replace '((:required :required-specializable))))

(make-instance 'fcll:standard-lambda-list-kind
               :name :destructuring
               :keywords (%derive-keywords-list :add '(&whole &body))
               :recurse :self)

(make-instance 'fcll:standard-lambda-list-kind
               :name :macro
               :keywords (%derive-keywords-list :base :destructuring :add '(&environment))
               :recurse :destructuring)

;; Boa lambda lists are syntactically the same as ordinary lambda lists.

(make-instance 'fcll:standard-lambda-list-kind
               :name :defsetf
               :keywords (%derive-keywords-list :add '(&environment) :remove '(&aux)))

(make-instance 'fcll:standard-lambda-list-kind
               :name :deftype
               :keywords (%derive-keywords-list :base :macro)
               :recurse :destructuring
               :default '*)

(make-instance 'fcll:standard-lambda-list-kind
               :name :define-modify-macro
               :keywords (%derive-keywords-list :remove '(&key &aux)))

(make-instance 'fcll:standard-lambda-list-kind
               :name :define-method-combination-arguments
               :keywords (%derive-keywords-list :add '(&whole)))
