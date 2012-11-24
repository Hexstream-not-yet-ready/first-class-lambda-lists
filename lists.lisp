(in-package #:first-class-lambda-lists)

(defmacro fc-ll:define (name lambda-keywords-config &body options)
  (declare (ignore name lambda-keywords-config options)))


;;; Standard lambda list kinds, as specified in CLHS 3.1:
;;; http://www.lispworks.com/documentation/HyperSpec/Body/03_d.htm

;; &key implies &allow-other-keys by default.
(fc-ll:define lambda (:required &optional &rest &key &aux)
  (:namestring "ordinary"))

(fc-ll:define generic-function ((:overriding ((t :defaultp nil))
                                             (:include lambda (:except &aux))))
  (:namestring "generic function"))

(fc-ll:define method ((:overriding ((:required :specializerp t))
                                   (:include lambda)))
  (:namestring "specialized"))

(fc-ll:define destructuring-bind ((:overriding ((t :recurse t))
                                               (:include lambda))
                                  &whole &body)
  (:namestring "destructuring"))

;; &environment doesn't propagate beyond top-level by default.
(fc-ll:define macro ((:overriding ((t :recurse destructuring-bind))
                                  (:include destructuring-bind))
                     &environment))

(fc-ll:define defstruct ((:include lambda))
  (:namestring "boa"))

(fc-ll:define defsetf ((:include lambda (:except &aux))
                       &environment))

(fc-ll:define deftype ((:overriding ((t :default '*))
                                    (:include macro))))

(fc-ll:define define-modify-macro (&optional &rest))

(fc-ll:define define-method-combination ((:include lambda) &whole)
  (:namestring "define-method-combination arguments"))
