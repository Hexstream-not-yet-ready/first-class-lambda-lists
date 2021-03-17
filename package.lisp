(cl:defpackage #:fcll
  (:nicknames #:first-class-lambda-lists)
  (:use #:cl)
  (:shadow #:variable #:lambda-list-keywords #:remove #:replace)
  (:import-from #:definitions-systems #:define)
  (:shadowing-import-from #:enhanced-eval-when #:eval-when)
  (:export #:lambda-list-keyword-class
           #:standard-lambda-list-keyword-class

           #:lambda-list-keyword
           #:standard-lambda-list-keyword

           #:lambda-list-section
           #:standard-lambda-list-section

           #:lambda-list-keywords-set
           #:derived-lambda-list-keywords-set

           #:lambda-list-keyword-order
           #:standard-lambda-list-keyword-order

           #:lambda-list-keyword-conflicts
           #:standard-lambda-list-keyword-conflicts

           #:lambda-list-keywords-list

           #:lambda-list-kind
           #:standard-lambda-list-kind

           #:lambda-list
           #:standard-lambda-list

           #:parse
           #:unparse
           #:expand
           #:bind

           #:malformed-lambda-list
           #:lambda-list-keywords-conflict))
