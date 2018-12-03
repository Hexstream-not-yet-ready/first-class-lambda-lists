(cl:defpackage #:first-class-lambda-lists
  (:nicknames #:fcll)
  (:use #:cl)
  (:shadow #:variable)
  (:export #:lambda-list-keyword
           #:standard-lambda-list-keyword

           #:lambda-list-kind
           #:standard-lambda-list-kind))
