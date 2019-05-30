(in-package #:first-class-lambda-lists)

(defclass fcll:lambda-list-section () ())

(defclass fcll:standard-lambda-list-section ()
  ((%lambda-list-keyword :initarg :lambda-list-keyword
                         :reader fcll:lambda-list-keyword
                         :type fcll:lambda-list-keyword
                         :initform (error "Must supply lambda list keyword."))
   (%parameters :initarg :parameters
                :reader parameters
                :type list
                :initform nil)))

(defmethod fcll:unparse ((section fcll:standard-lambda-list-section))
  (let ((introducer (introducer (fcll:lambda-list-keyword section)))
        (parameters (mapcar #'fcll:unparse (parameters section))))
    (if introducer
        (cons introducer parameters)
        parameters)))

(defmethod print-object ((section fcll:standard-lambda-list-section) stream)
  (print-unreadable-object (section stream :type t)
    (format stream "~S~{ ~S~}"
            (defsys:name (fcll:lambda-list-keyword section))
            (mapcar #'fcll:unparse (parameters section)))))

(defmethod expand ((section fcll:standard-lambda-list-section) (expansion-env expansion-environment) form)
  (reduce (lambda (parameter form)
            (expand parameter expansion-env form))
          (parameters section)
          :from-end t
          :initial-value form))

(defclass standard-&key-section (fcll:standard-lambda-list-section)
  ((%allow-other-keys-p :initarg :allow-other-keys-p
                        :reader allow-other-keys-p
                        :type boolean
                        :initform nil)))

(defmethod fcll:unparse ((section standard-&key-section))
  (if (allow-other-keys-p section)
      (append (call-next-method) '(&allow-other-keys))
      (call-next-method)))


(defvar *sections*)

(defun %add-section (section)
  (push section *sections*))


(defun %apparent-lambda-list-keyword-p (object)
  (let ((symbol-name (and (symbolp object) (symbol-name object))))
    (and (plusp (length symbol-name)) (char= (char symbol-name 0) #\&))))

(defun %make-introducer-parser (introducer parameters-parser)
  (if introducer
      (lambda (tail)
        (check-type tail cons)
        (if (eq (first tail) introducer)
            (funcall parameters-parser (rest tail))
            tail))
      (lambda (tail)
        (check-type tail cons)
        (let ((first (first tail)))
          (if (not (%apparent-lambda-list-keyword-p first))
              (funcall parameters-parser tail)
              tail)))))

(defun %make-parameters-parser (lambda-list-keyword)
  (let ((arity (arity lambda-list-keyword))
        (parameter-parser (parameter-parser lambda-list-keyword)))
    (ecase arity
      (0 (lambda (tail)
           (%add-section (make-instance 'fcll:standard-lambda-list-section
                                        :lambda-list-keyword lambda-list-keyword))
           tail))
      (1 (lambda (tail)
           (if tail
               (progn (%add-section
                       (make-instance 'fcll:standard-lambda-list-section
                                      :lambda-list-keyword lambda-list-keyword
                                      :parameters (list (funcall parameter-parser (first tail)))))
                      (rest tail))
               (%malformed-lambda-list 'simple-malformed-lambda-list-error
                                       :tail tail
                                       :format-control "Lambda list keyword ~S expected an argument."
                                       :format-arguments (list lambda-list-keyword)))))
      ((t) (lambda (tail)
             (let* ((end (member-if #'%apparent-lambda-list-keyword-p tail))
                    (head (ldiff tail end)))
               (%add-section (make-instance 'fcll:standard-lambda-list-section
                                            :lambda-list-keyword lambda-list-keyword
                                            :parameters (map-into head parameter-parser head)))
               end))))))

(defun %make-lambda-list-keyword-parser (lambda-list-keyword)
  (%make-introducer-parser (introducer lambda-list-keyword)
                           (%make-parameters-parser lambda-list-keyword)))

(defun %make-&key-parser (lambda-list-keyword)
  (%make-introducer-parser
   (introducer lambda-list-keyword)
   (let ((parameter-parser (parameter-parser lambda-list-keyword)))
     (lambda (tail)
       (let* ((end (member-if #'%apparent-lambda-list-keyword-p tail))
              (head (ldiff tail end))
              (allow-other-keys-p (when (eq (first end) '&allow-other-keys)
                                    (setf end (rest end))
                                    t)))
         (%add-section (make-instance 'standard-&key-section
                                      :lambda-list-keyword lambda-list-keyword
                                      :parameters (map-into head parameter-parser head)
                                      :allow-other-keys-p allow-other-keys-p))
         end)))))


(define-condition fcll:malformed-lambda-list (error)
  ((%root-lambda-list :initarg :root-lambda-list
                      :reader root-lambda-list)
   (%specification :initarg :specification
                   :reader specification)
   (%tail :initarg :tail
          :reader tail)))

(define-condition simple-malformed-lambda-list-error (fcll:malformed-lambda-list simple-error)
  ())

(defvar *%malformed-lambda-list*)

(defun %malformed-lambda-list (error-type &rest args)
  (apply *%malformed-lambda-list* error-type args))
