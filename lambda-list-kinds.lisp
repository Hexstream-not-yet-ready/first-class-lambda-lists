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

(define (fcll:lambda-list-kind :ordinary) defun
  '(:required &optional &rest &key &aux)) ;&allow-other-keys is subordinate to &key, so implied by it.

(define (fcll:lambda-list-kind :generic-function) defgeneric
  (%derive-keywords-list :replace '((&optional :&optional-no-defaulting)
                                    (&key :&key-no-defaulting))
                         :remove '(&aux)))

(define (fcll:lambda-list-kind :specialized) defmethod
  (%derive-keywords-list :replace '((:required :required-specializable))))

(define (fcll:lambda-list-kind :destructuring) destructuring-bind
  (%derive-keywords-list :add '(&whole &body))
  :recurse :self)

(define (fcll:lambda-list-kind :macro) defmacro
  (%derive-keywords-list :base :destructuring :add '(&environment))
  :recurse :destructuring)

(define (fcll:lambda-list-kind :boa) defstruct
  (%derive-keywords-list))

(define (fcll:lambda-list-kind :defsetf) defsetf
  (%derive-keywords-list :add '(&environment) :remove '(&aux)))

(define (fcll:lambda-list-kind :deftype) deftype
  (%derive-keywords-list :base :macro)
  :recurse :destructuring
  :default '*)

(define (fcll:lambda-list-kind :define-modify-macro) define-modify-macro
  (%derive-keywords-list :remove '(&key &aux)))

(define (fcll:lambda-list-kind :define-method-combination-arguments) define-method-combination
  (%derive-keywords-list :add '(&whole)))
