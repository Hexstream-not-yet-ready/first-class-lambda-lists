(cl:defpackage #:first-class-lambda-keywords
  (:nicknames #:fc-lk #:fc-lambda-keywords #:fc-lambda-keyword)
  (:use #:cl)
  (:shadow #:keyword)
  (:export #:keyword
           #:name
           #:arity
           #:introducer
           #:specializerp
           #:default
           #:defaultp
           #:suppliedp
           #:keyword-name-p

           #:define
           #:conflicts
           #:precedes
           #:modifies))

(ikeywords:defpackage #:first-class-lambda-keywords.extended
  (:nicknames #:fc-ll.extended)
  (:export #:&transform
           #:&doc
           #:&decl
           #:&rest+
           #:&body+
           #:&head
           #:&tail
           #:&destructure))

(cl:defpackage #:first-class-lambda-lists
  (:nicknames #:fc-ll #:fc-lambda-lists)
  (:use #:cl)
  (:shadow #:list)
  (:export #:list
           #:define))
