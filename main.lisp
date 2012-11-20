(in-package #:first-class-lambda-lists)

#+nil
(&whole &optional &rest &body &key &allow-other-keys &aux &environment)

#+nil
(&doc &decl &rest+ &body+ &head &tail &destructure)

#+nil
((%define lambda "ordinary"
          (:required &optional &rest &key &allow-other-keys &aux))
 (%define generic-function "generic function"
          (:required (&optional :defaultp nil) &rest (&key :defaultp nil) &allow-other-keys))
 (%define method "specialized"
          (:required :specializerp t) &optional &rest &key &allow-other-keys &aux)
 (%define macro "macro"
          (&whole &environment :required &optional &rest &body &key &allow-other-keys &aux))
 (%define defstruct "boa"
          (:required &optional &rest &key &allow-other-keys &aux))
 (%define defsetf "defsetf"
          (:required &optional &rest &key &allow-other-keys &environment))
 (%define deftype "deftype"
          (&whole &environment :required &optional &rest &body &key &allow-other-keys &aux)
          (:default-initform '*))
 (%define define-modify-macro "define-modify-macro"
          (&optional &rest))
 (%define define-method-combination "define-method-combination arguments"
          (&whole :required &optional &rest &key &allow-other-keys &aux)))
