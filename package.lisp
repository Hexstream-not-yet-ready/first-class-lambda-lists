(cl:defpackage #:fcll
  (:nicknames #:first-class-lambda-lists)
  (:use #:cl)
  (:shadow #:variable)
  (:import-from #:definitions-systems #:define)
  (:import-from #:bubble-operator-upwards #:cartesian-product)
  (:export #:lambda-list-keyword
           #:standard-lambda-list-keyword
           #:lambda-list-section
           #:standard-lambda-list-section

           #:lambda-list-keyword-order
           #:standard-lambda-list-keyword-order

           #:lambda-list-keyword-conflicts
           #:standard-lambda-list-keyword-conflicts

           #:lambda-list-kind
           #:standard-lambda-list-kind

           #:lambda-list
           #:standard-lambda-list
           #:parse
           #:unparse))
