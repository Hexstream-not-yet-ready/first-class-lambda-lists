(ikeywords:defpackage #:first-class-lambda-keywords.extended
  (:nicknames #:fc-lk.extended)
  (:export #:&doc
           #:&decl
           #:&rest+
           #:&body+))

(cl:defpackage #:first-class-lambda-keywords
  (:nicknames #:fc-lk #:fc-lambda-keywords #:fc-lambda-keyword)
  (:use #:cl
        #:fc-lk.extended)
  (:shadow #:keyword)
  (:export #:keyword
           #:name
           #:name-mixin
           #:arity
           #:arity-mixin
           #:introducer
           #:introducer-mixin
           #:specializerp
           #:specializerp-mixin
           #:default
           #:defaultp
           #:default-mixin
           #:suppliedp
           #:keyword-name-p
           #:keyword-name-p-mixin
           #:conflicts
           #:conflicts-mixin

           #:define
           #:define-conflicts
           #:define-order
           #:define-modifies

           #:conflicts
           #:order
           #:before
           #:after
           #:modifiers
           #:modified-by))

(cl:defpackage #:first-class-lambda-lists
  (:nicknames #:fc-ll #:fc-lambda-lists)
  (:use #:cl)
  (:shadow #:list)
  (:export #:list
           #:define))
