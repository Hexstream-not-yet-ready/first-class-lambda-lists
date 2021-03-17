(asdf:defsystem #:first-class-lambda-lists_tests

  :author "Jean-Philippe Paradis <hexstream@hexstreamsoft.com>"

  :license "Unlicense"

  :description "first-class-lambda-lists unit tests."

  :depends-on ("first-class-lambda-lists"
               "parachute")

  :serial cl:t
  :components ((:file "tests"))

  :perform (asdf:test-op (op c) (uiop:symbol-call '#:parachute '#:test '#:first-class-lambda-lists_tests)))
