(asdf:defsystem #:first-class-lambda-lists

  :author "Jean-Philippe Paradis <hexstream@hexstreamsoft.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "To be described."

  :depends-on ("definitions-systems"
               "bubble-operator-upwards"
               "enhanced-eval-when")

  :version "0.1"
  :serial cl:t
  :components ((:file "package")
               (:file "lambda-list-parameters")
               (:file "lambda-list-keywords")
               (:file "lambda-list-keyword-order")
               (:file "lambda-list-keyword-conflicts")
               (:file "lambda-list-kinds")
               (:file "lambda-lists"))

  :in-order-to ((asdf:test-op (asdf:test-op #:first-class-lambda-lists_tests))))
