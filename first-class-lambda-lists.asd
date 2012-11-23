(asdf:defsystem #:first-class-lambda-lists

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "To be described."

  :depends-on (#:incognito-keywords)

  :version "1.0"
  :serial cl:t
  :components ((:file "package")
               (:file "keywords")
               (:file "lists")))
