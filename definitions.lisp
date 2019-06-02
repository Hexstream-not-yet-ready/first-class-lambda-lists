(in-package #:first-class-lambda-lists)

;;; Lambda list keywords

(define (fcll:lambda-list-keyword &whole) 1
        :section-class (find-class 'standard-&whole-section))

(define (fcll:lambda-list-keyword :&environment-not-before-&whole) 1
  :introducer '&environment
  :section-class (find-class 'standard-&environment-section))

(define (fcll:lambda-list-keyword :&environment-last) 1
  :introducer '&environment
  :section-class (find-class 'standard-&environment-section))

(define (fcll:lambda-list-keyword :required) t
  :introducer nil
  :section-class (find-class 'standard-required-section)
  :parameter-parser #'%parse-required-parameter
  :recursablep t)

(define (fcll:lambda-list-keyword :required-specializable) t
  :parent :required
  :parameter-parser #'%parse-specializable-parameter)

(define (fcll:lambda-list-keyword &optional) t
        :section-class (find-class 'standard-&optional-section)
        :parameter-parser #'%parse-optional-parameter
        :recursablep t)

(define (fcll:lambda-list-keyword :&optional-no-defaulting) t
  :parent '&optional
  :parameter-parser #'%parse-optional-no-defaulting-parameter)

(define (fcll:lambda-list-keyword &rest) 1
        :section-class (find-class 'standard-&rest-section)
        :recursablep t)

(define (fcll:lambda-list-keyword &body) 1
        :section-class (find-class 'standard-&rest-section)
        :recursablep t)

(define (fcll:lambda-list-keyword &key) t
        :section-class (find-class 'standard-&key-section)
        :parameter-parser #'%parse-key-parameter
        :recursablep t
        :parser-maker '%make-&key-parser)

(define (fcll:lambda-list-keyword :&key-no-defaulting) t
  :parent '&key
  :parser-maker '%make-lambda-list-keyword-parser
  :parameter-parser #'%parse-key-no-defaulting-parameter)

(define (fcll:lambda-list-keyword &allow-other-keys) 0
        :section-class (find-class 'subordinate-section))

(define (fcll:lambda-list-keyword &aux) t
        :section-class (find-class 'standard-&aux-section)
        :parameter-parser #'%parse-aux-parameter)



;;; Lambda list keyword order

(define (fcll:lambda-list-keyword-order :standard)
  (list &whole
        (or (list (or :required :required-specializable)
                  (or &optional :&optional-no-defaulting)
                  (or &rest &body)
                  (or &key :&key-no-defaulting)
                  &aux
                  :&environment-last)
            :&environment-not-before-&whole)))

;;; Lambda list keyword conflicts

(define (fcll:lambda-list-keyword-conflicts :standard)
  (and &rest &body))


;;; Lambda list kinds

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
  :recurse t)

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
  :default-initform ''*)

(define (fcll:lambda-list-kind :define-modify-macro) define-modify-macro
  (:derive :remove (&key &aux)))

(define (fcll:lambda-list-kind :define-method-combination-arguments) define-method-combination
  (:derive :add &whole))
