(cl:defpackage #:first-class-lambda-lists_tests
  (:use #:cl #:parachute))

(cl:in-package #:first-class-lambda-lists_tests)

(defmacro are (comp expected form &optional description &rest format-args)
  `(is ,comp ,expected (multiple-value-list ,form) ,description ,@format-args))

(defun round-trip (kind specification &optional (result specification))
  (is equal result (fcll:unparse (make-instance 'fcll:standard-lambda-list :kind kind :parse specification))))

(define-test "main"
  (round-trip :ordinary
              '(required1
                &optional optional1 (optional2) (optional3 nil) (optional4 nil nil) (optional5 t) (optional6 nil optional6-supplied-p)
                &rest rest
                &key key1 key2 ((:key3 key3)) (key4 t) (key5 nil key5-supplied-p) ((custom-key6 key6) nil key6-supplied-p) ((:custom-key7 key7)) ((key8 key8))
                &aux aux1 (aux2) (aux3 nil) (aux4 t))
              '(required1
                &optional optional1 optional2 optional3 optional4 (optional5 t) (optional6 nil optional6-supplied-p)
                &rest rest
                &key key1 key2 key3 (key4 t) (key5 nil key5-supplied-p) ((custom-key6 key6) nil key6-supplied-p) ((:custom-key7 key7)) ((key8 key8))
                &aux aux1 aux2 aux3 (aux4 t)))
  (round-trip :generic-function
              '(required1
                &optional optional1 (optional2)
                &rest rest
                &key key1 key2 ((:key3 key3)) ((:custom-key4 key4)) ((key5 key5)))
              '(required1
                &optional optional1 optional2
                &rest rest
                &key key1 key2 key3 ((:custom-key4 key4)) ((key5 key5))))
  (round-trip :specialized
              '(required1 (required2) (required3 t) (required4 integer)
                &optional optional1 (optional2) (optional3 nil) (optional4 nil nil) (optional5 t) (optional6 nil optional6-supplied-p)
                &rest rest
                &key key1 key2 ((:key3 key3)) (key4 t) (key5 nil key5-supplied-p) ((custom-key6 key6) nil key6-supplied-p) ((:custom-key7 key7)) ((key8 key8))
                &aux aux1 (aux2) (aux3 nil) (aux4 t))
              '(required1 required2 required3 (required4 integer)
                &optional optional1 optional2 optional3 optional4 (optional5 t) (optional6 nil optional6-supplied-p)
                &rest rest
                &key key1 key2 key3 (key4 t) (key5 nil key5-supplied-p) ((custom-key6 key6) nil key6-supplied-p) ((:custom-key7 key7)) ((key8 key8))
                &aux aux1 aux2 aux3 (aux4 t))))
