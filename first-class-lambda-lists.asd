(asdf:defsystem #:first-class-lambda-lists

  :author "Jean-Philippe Paradis <hexstream@hexstreamsoft.com>"

  :license "Unlicense"

  :description "Provides a simple unified extensible way of processing lambda lists."

  :depends-on ("definitions-systems"
               "enhanced-eval-when")

  :version "0.1"
  :serial cl:t
  :components ((:file "package")

               (:file "defsys")

               (:file "lambda-list-keyword-classes")
               (:file "lambda-list-keywords")
               (:file "lambda-list-keywords-set")
               (:file "lambda-list-keyword-order")
               (:file "lambda-list-keyword-conflicts")
               (:file "lambda-list-cores")
               (:file "lambda-list-kinds")

               (:file "lambda-list-parameters")
               (:file "lambda-list-sections")
               (:file "lambda-lists")

               (:file "unparse")
               (:file "expand")

               (:file "definitions"))

  :in-order-to ((asdf:test-op (asdf:test-op #:first-class-lambda-lists_tests))))
