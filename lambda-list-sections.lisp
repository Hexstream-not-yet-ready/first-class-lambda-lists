(in-package #:first-class-lambda-lists)

(defclass fcll:lambda-list-section () ())

(defclass &whole-section (fcll:lambda-list-section) ())
(defclass &environment-section (fcll:lambda-list-section) ())
(defclass required-section (fcll:lambda-list-section) ())
(defclass &optional-section (fcll:lambda-list-section) ())
(defclass &rest-section (fcll:lambda-list-section) ())
(defclass &key-section (fcll:lambda-list-section) ())
(defclass &aux-section (fcll:lambda-list-section) ())
(defclass subordinate-section (fcll:lambda-list-section) ())

(defclass fcll:standard-lambda-list-section (fcll:lambda-list-section)
  ((%lambda-list-keyword :initarg :lambda-list-keyword
                         :reader fcll:lambda-list-keyword
                         :type fcll:lambda-list-keyword
                         :initform (error "Must supply lambda list keyword."))
   (%parameters :initarg :parameters
                :reader parameters
                :type list
                :initform nil)))

(defclass standard-&whole-section (fcll:standard-lambda-list-section &whole-section) ())
(defclass standard-&environment-section (fcll:standard-lambda-list-section &environment-section) ())
(defclass standard-required-section (fcll:standard-lambda-list-section required-section) ())
(defclass standard-&optional-section (fcll:standard-lambda-list-section &optional-section) ())
(defclass standard-&rest-section (fcll:standard-lambda-list-section &rest-section) ())
(defclass standard-&aux-section (fcll:standard-lambda-list-section &aux-section) ())

(defclass standard-&key-section (fcll:standard-lambda-list-section &key-section)
  ((%allow-other-keys-p :initarg :allow-other-keys-p
                        :reader allow-other-keys-p
                        :type boolean
                        :initform nil)))


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
        (parameter-parser (parameter-parser lambda-list-keyword))
        (section-class (section-class lambda-list-keyword)))
    (ecase arity
      (0 (lambda (tail)
           (%add-section (make-instance section-class
                                        :lambda-list-keyword lambda-list-keyword))
           tail))
      (1 (lambda (tail)
           (if tail
               (progn (%add-section
                       (make-instance section-class
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
               (%add-section (make-instance section-class
                                            :lambda-list-keyword lambda-list-keyword
                                            :parameters (map-into head parameter-parser head)))
               end))))))

(defun %make-lambda-list-keyword-parser (lambda-list-keyword)
  (%make-introducer-parser (introducer lambda-list-keyword)
                           (%make-parameters-parser lambda-list-keyword)))

(defun %make-&key-parser (lambda-list-keyword)
  (%make-introducer-parser
   (introducer lambda-list-keyword)
   (let ((parameter-parser (parameter-parser lambda-list-keyword))
         (section-class (section-class lambda-list-keyword)))
     (lambda (tail)
       (let* ((end (member-if #'%apparent-lambda-list-keyword-p tail))
              (head (ldiff tail end))
              (allow-other-keys-p (when (eq (first end) '&allow-other-keys)
                                    (setf end (rest end))
                                    t)))
         (%add-section (make-instance section-class
                                      :lambda-list-keyword lambda-list-keyword
                                      :parameters (map-into head parameter-parser head)
                                      :allow-other-keys-p allow-other-keys-p))
         end)))))
