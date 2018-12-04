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

(defun %derive-keywords-list (&key (from :ordinary) add remove replace)
  (declare (ignore from add remove replace))
  nil)

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
  (:derive :from :destructuring :add &environment)
  :recurse :destructuring)

(define (fcll:lambda-list-kind :boa) defstruct
  (:derive))

(define (fcll:lambda-list-kind :defsetf) defsetf
  (:derive :add &environment :remove &aux))

(define (fcll:lambda-list-kind :deftype) deftype
  (:derive :from :macro)
  :recurse :destructuring
  :default '*)

(define (fcll:lambda-list-kind :define-modify-macro) define-modify-macro
  (:derive :remove (&key &aux)))

(define (fcll:lambda-list-kind :define-method-combination-arguments) define-method-combination
  (:derive :add &whole))
