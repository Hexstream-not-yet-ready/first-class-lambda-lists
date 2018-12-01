(asdf:defsystem #:first-class-lambda-lists

  :author "Jean-Philippe Paradis <hexstream@hexstreamsoft.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "To be described."

  :depends-on ()

  :version "0.1"
  :serial cl:t
  :components ((:file "package")
               (:file "lambda-list-keywords")
               (:file "lambda-list-kinds"))

  :in-order-to ((asdf:test-op (asdf:test-op #:first-class-lambda-lists_tests))))
