(cl:defpackage #:first-class-lambda-lists
  (:nicknames #:fcll)
  (:use #:cl)
  (:shadow #:variable)
  (:import-from #:definitions-systems #:define)
  (:import-from #:bubble-operator-upwards #:cartesian-product)
  (:export #:lambda-list-keyword
           #:standard-lambda-list-keyword

           #:lambda-list-kind
           #:standard-lambda-list-kind

           #:lambda-list
           #:standard-lambda-list))
