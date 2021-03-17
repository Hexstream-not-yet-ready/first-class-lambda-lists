(cl:defpackage #:first-class-lambda-lists_tests
  (:use #:cl #:parachute))

(cl:in-package #:first-class-lambda-lists_tests)

(defmacro are (comp expected form &optional description &rest format-args)
  `(is ,comp ,expected (multiple-value-list ,form) ,description ,@format-args))

(defun round-trip (kind specification &optional (result specification))
  (is equal result (fcll:unparse (make-instance 'fcll:standard-lambda-list :kind kind :parse specification))))

(defmacro fails (kind specification &optional (error-type 'error))
  `(fail (make-instance 'fcll:standard-lambda-list :kind ,kind :parse ,specification) ,error-type))

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
                &aux aux1 aux2 aux3 (aux4 t)))
  ;; "i-" as in "inner-"
  (round-trip :destructuring
              '(&whole whole
                required1
                (&whole i-whole
                 i-required1
                 &optional i-optional1 (i-optional2) (i-optional3 nil) (i-optional4 nil nil) (i-optional5 t) (i-optional6 nil i-optional6-supplied-p)
                 &body i-rest
                 &key i-key1 i-key2 ((:i-key3 i-key3)) (i-key4 t) (i-key5 nil i-key5-supplied-p) ((i-custom-key6 i-key6) nil i-key6-supplied-p) ((:i-custom-key7 i-key7)) ((i-key8 i-key8))
                 &aux i-aux1 (i-aux2) (i-aux3 nil) (i-aux4 t))
                &optional optional1 (optional2) (optional3 nil) (optional4 nil nil) (optional5 t) (optional6 nil optional6-supplied-p)
                &rest rest
                &key key1 key2 ((:key3 key3)) (key4 t) (key5 nil key5-supplied-p) ((custom-key6 key6) nil key6-supplied-p) ((:custom-key7 key7)) ((key8 key8))
                &aux aux1 (aux2) (aux3 nil) (aux4 t))
              '(&whole whole
                required1
                (&whole i-whole
                 i-required1
                 &optional i-optional1 i-optional2 i-optional3 i-optional4 (i-optional5 t) (i-optional6 nil i-optional6-supplied-p)
                 &body i-rest
                 &key i-key1 i-key2 i-key3 (i-key4 t) (i-key5 nil i-key5-supplied-p) ((i-custom-key6 i-key6) nil i-key6-supplied-p) ((:i-custom-key7 i-key7)) ((i-key8 i-key8))
                 &aux i-aux1 i-aux2 i-aux3 (i-aux4 t))
                &optional optional1 optional2 optional3 optional4 (optional5 t) (optional6 nil optional6-supplied-p)
                &rest rest
                &key key1 key2 key3 (key4 t) (key5 nil key5-supplied-p) ((custom-key6 key6) nil key6-supplied-p) ((:custom-key7 key7)) ((key8 key8))
                &aux aux1 aux2 aux3 (aux4 t)))
  (round-trip :macro
              '(&whole whole
                &environment env
                required1
                (&whole i-whole
                 i-required1
                 &optional i-optional1 (i-optional2) (i-optional3 nil) (i-optional4 nil nil) (i-optional5 t) (i-optional6 nil i-optional6-supplied-p)
                 &body i-rest
                 &key i-key1 i-key2 ((:i-key3 i-key3)) (i-key4 t) (i-key5 nil i-key5-supplied-p) ((i-custom-key6 i-key6) nil i-key6-supplied-p) ((:i-custom-key7 i-key7)) ((i-key8 i-key8)) &allow-other-keys
                 &aux i-aux1 (i-aux2) (i-aux3 nil) (i-aux4 t))
                &optional optional1 (optional2) (optional3 nil) (optional4 nil nil) (optional5 t) (optional6 nil optional6-supplied-p)
                &rest rest
                &key key1 key2 ((:key3 key3)) (key4 t) (key5 nil key5-supplied-p) ((custom-key6 key6) nil key6-supplied-p) ((:custom-key7 key7)) ((key8 key8))
                &aux aux1 (aux2) (aux3 nil) (aux4 t))
              '(&whole whole
                &environment env
                required1
                (&whole i-whole
                 i-required1
                 &optional i-optional1 i-optional2 i-optional3 i-optional4 (i-optional5 t) (i-optional6 nil i-optional6-supplied-p)
                 &body i-rest
                 &key i-key1 i-key2 i-key3 (i-key4 t) (i-key5 nil i-key5-supplied-p) ((i-custom-key6 i-key6) nil i-key6-supplied-p) ((:i-custom-key7 i-key7)) ((i-key8 i-key8)) &allow-other-keys
                 &aux i-aux1 i-aux2 i-aux3 (i-aux4 t))
                &optional optional1 optional2 optional3 optional4 (optional5 t) (optional6 nil optional6-supplied-p)
                &rest rest
                &key key1 key2 key3 (key4 t) (key5 nil key5-supplied-p) ((custom-key6 key6) nil key6-supplied-p) ((:custom-key7 key7)) ((key8 key8))
                &aux aux1 aux2 aux3 (aux4 t)))
  (round-trip :macro
              '(&whole whole
                required1
                &optional optional1 (optional2) (optional3 nil) (optional4 nil nil) (optional5 t) (optional6 nil optional6-supplied-p)
                &rest rest
                &key key1 key2 ((:key3 key3)) (key4 t) (key5 nil key5-supplied-p) ((custom-key6 key6) nil key6-supplied-p) ((:custom-key7 key7)) ((key8 key8)) &allow-other-keys
                &aux aux1 (aux2) (aux3 nil) (aux4 t)
                &environment env)
              '(&whole whole
                required1
                &optional optional1 optional2 optional3 optional4 (optional5 t) (optional6 nil optional6-supplied-p)
                &rest rest
                &key key1 key2 key3 (key4 t) (key5 nil key5-supplied-p) ((custom-key6 key6) nil key6-supplied-p) ((:custom-key7 key7)) ((key8 key8)) &allow-other-keys
                &aux aux1 aux2 aux3 (aux4 t)
                &environment env))
  (round-trip :boa
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
  (round-trip :defsetf
              '(required1
                &optional optional1 (optional2) (optional3 nil) (optional4 nil nil) (optional5 t) (optional6 nil optional6-supplied-p)
                &rest rest
                &key key1 key2 ((:key3 key3)) (key4 t) (key5 nil key5-supplied-p) ((custom-key6 key6) nil key6-supplied-p) ((:custom-key7 key7)) ((key8 key8)))
              '(required1
                &optional optional1 optional2 optional3 optional4 (optional5 t) (optional6 nil optional6-supplied-p)
                &rest rest
                &key key1 key2 key3 (key4 t) (key5 nil key5-supplied-p) ((custom-key6 key6) nil key6-supplied-p) ((:custom-key7 key7)) ((key8 key8))))
  (round-trip :defsetf
              '(required1
                &optional optional1 (optional2) (optional3 nil) (optional4 nil nil) (optional5 t) (optional6 nil optional6-supplied-p)
                &rest rest
                &key key1 key2 ((:key3 key3)) (key4 t) (key5 nil key5-supplied-p) ((custom-key6 key6) nil key6-supplied-p) ((:custom-key7 key7)) ((key8 key8))
                &environment env)
              '(required1
                &optional optional1 optional2 optional3 optional4 (optional5 t) (optional6 nil optional6-supplied-p)
                &rest rest
                &key key1 key2 key3 (key4 t) (key5 nil key5-supplied-p) ((custom-key6 key6) nil key6-supplied-p) ((:custom-key7 key7)) ((key8 key8))
                &environment env))
  (round-trip :deftype
              '(&whole whole
                &environment env
                required1
                (&whole i-whole
                 i-required1
                 &optional i-optional1 (i-optional2) (i-optional3 '*) (i-optional4 '* nil) (i-optional5 t) (i-optional6 '* i-optional6-supplied-p)
                 &body i-rest
                 &key i-key1 i-key2 ((:i-key3 i-key3)) (i-key4 t) (i-key5 '* i-key5-supplied-p) ((i-custom-key6 i-key6) '* i-key6-supplied-p) ((:i-custom-key7 i-key7)) ((i-key8 i-key8))
                 &aux i-aux1 (i-aux2) (i-aux3 nil) (i-aux4 t))
                &optional optional1 (optional2) (optional3 '*) (optional4 '* nil) (optional5 t) (optional6 '* optional6-supplied-p)
                &rest rest
                &key key1 key2 ((:key3 key3)) (key4 t) (key5 '* key5-supplied-p) ((custom-key6 key6) '* key6-supplied-p) ((:custom-key7 key7)) ((key8 key8))
                &aux aux1 (aux2) (aux3 nil) (aux4 t))
              '(&whole whole
                &environment env
                required1
                (&whole i-whole
                 i-required1
                 &optional i-optional1 i-optional2 i-optional3 i-optional4 (i-optional5 t) (i-optional6 '* i-optional6-supplied-p)
                 &body i-rest
                 &key i-key1 i-key2 i-key3 (i-key4 t) (i-key5 '* i-key5-supplied-p) ((i-custom-key6 i-key6) '* i-key6-supplied-p) ((:i-custom-key7 i-key7)) ((i-key8 i-key8))
                 &aux i-aux1 i-aux2 i-aux3 (i-aux4 t))
                &optional optional1 optional2 optional3 optional4 (optional5 t) (optional6 '* optional6-supplied-p)
                &rest rest
                &key key1 key2 key3 (key4 t) (key5 '* key5-supplied-p) ((custom-key6 key6) '* key6-supplied-p) ((:custom-key7 key7)) ((key8 key8))
                &aux aux1 aux2 aux3 (aux4 t)))
  (round-trip :deftype
              '(&whole whole
                required1
                &optional optional1 (optional2) (optional3 '*) (optional4 '* nil) (optional5 t) (optional6 '* optional6-supplied-p)
                &rest rest
                &key key1 key2 ((:key3 key3)) (key4 t) (key5 '* key5-supplied-p) ((custom-key6 key6) '* key6-supplied-p) ((:custom-key7 key7)) ((key8 key8))
                &aux aux1 (aux2) (aux3 nil) (aux4 t)
                &environment env)
              '(&whole whole
                required1
                &optional optional1 optional2 optional3 optional4 (optional5 t) (optional6 '* optional6-supplied-p)
                &rest rest
                &key key1 key2 key3 (key4 t) (key5 '* key5-supplied-p) ((custom-key6 key6) '* key6-supplied-p) ((:custom-key7 key7)) ((key8 key8))
                &aux aux1 aux2 aux3 (aux4 t)
                &environment env))
  (round-trip :define-modify-macro
              '(required1
                &optional optional1 (optional2) (optional3 nil) (optional4 nil nil) (optional5 t) (optional6 nil optional6-supplied-p)
                &rest rest)
              '(required1
                &optional optional1 optional2 optional3 optional4 (optional5 t) (optional6 nil optional6-supplied-p)
                &rest rest))
  (round-trip :define-method-combination-arguments
              '(&whole whole
                required1
                &optional optional1 (optional2) (optional3 nil) (optional4 nil nil) (optional5 t) (optional6 nil optional6-supplied-p)
                &rest rest
                &key key1 key2 ((:key3 key3)) (key4 t) (key5 nil key5-supplied-p) ((custom-key6 key6) nil key6-supplied-p) ((:custom-key7 key7)) ((key8 key8))
                &aux aux1 (aux2) (aux3 nil) (aux4 t))
              '(&whole whole
                required1
                &optional optional1 optional2 optional3 optional4 (optional5 t) (optional6 nil optional6-supplied-p)
                &rest rest
                &key key1 key2 key3 (key4 t) (key5 nil key5-supplied-p) ((custom-key6 key6) nil key6-supplied-p) ((:custom-key7 key7)) ((key8 key8))
                &aux aux1 aux2 aux3 (aux4 t)))
  (fails :ordinary '(&optional optional1 &optional optional2) 'fcll:malformed-lambda-list)
  (fails :ordinary '(&rest) 'fcll:malformed-lambda-list)
  (fails :ordinary '(&rest foo bar) 'fcll:malformed-lambda-list)
  (fails :ordinary '(&fake) 'fcll:malformed-lambda-list)
  (fails :ordinary '(&rest rest &optional) 'fcll:malformed-lambda-list)
  (fails :ordinary '(&environment env) 'fcll:malformed-lambda-list)
  (fails :macro '((&environment env)) 'fcll:malformed-lambda-list)
  (fails :macro '(&rest rest &body body) 'fcll:lambda-list-keywords-conflict)
  (fails :macro '(&body body &rest rest) 'fcll:lambda-list-keywords-conflict)
  (fails :defsetf '(&environment env foo) 'fcll:malformed-lambda-list)
  (fcll:bind :ordinary (required1 required2 &optional optional1 optional2 (optional3 'initform1) (optional4 'initform2 optional4-supplied-p) &aux aux1 (aux2 t))
      '(value1 value2 value3)
    (is eq 'value1 required1)
    (is eq 'value2 required2)
    (is eq 'value3 optional1)
    (is eq nil optional2)
    (is eq 'initform1 optional3)
    (is eq 'initform2 optional4)
    (is eq nil optional4-supplied-p)
    (is eq nil aux1)
    (is eq t aux2))
  (fcll:bind :ordinary (required1 &optional optional1 &rest rest1)
      '(value1 value2 value3 value4 value5)
    (is eq 'value1 required1)
    (is eq 'value2 optional1)
    (is equal '(value3 value4 value5) rest1))
  (fcll:bind :ordinary (required1 &optional optional1 &rest rest1)
      '(value1)
    (is eq 'value1 required1)
    (is eq nil optional1)
    (is eq nil rest1))
  (fcll:bind :ordinary (&key key1 key2 (key3 'initform1) (key4 'initform2 key4-supplied-p) &allow-other-keys)
      '(:key5 value1 :key1 value2)
    (is eq 'value2 key1)
    (is eq nil key2)
    (is eq 'initform1 key3)
    (is eq 'initform2 key4)
    (is eq nil key4-supplied-p)))
