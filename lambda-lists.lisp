(in-package #:first-class-lambda-lists)

(defclass fcll:lambda-list () ())

(defclass fcll:standard-lambda-list (fcll:lambda-list)
  ((%kind :reader kind
          :type fcll:lambda-list-kind)
   (%sections :reader %sections
              :type list
              :initform nil)))

(defmethod shared-initialize :after ((instance fcll:standard-lambda-list) slot-names
                                     &key kind (parse nil parse-supplied-p))
  (declare (ignore slot-names))
  (setf (slot-value instance '%kind)
        (defsys:locate 'fcll:lambda-list-kind kind))
  (when parse-supplied-p
    (fcll:parse instance parse)))

(defun %call-with-root-lambda-list-setup (lambda-list function)
  (let* ((root-lambda-list (or *root-lambda-list* lambda-list))
         (*default-initform* (default-initform (kind root-lambda-list)))
         (*root-lambda-list* root-lambda-list))
    (funcall function)))

(defmethod print-object ((lambda-list fcll:standard-lambda-list) stream)
  (print-unreadable-object (lambda-list stream :type t)
    (format stream "~S ~S"
            (defsys:name (kind lambda-list))
            (%sections lambda-list))))


(defgeneric fcll:parse (lambda-list specification))

(defmethod fcll:parse ((lambda-list fcll:standard-lambda-list) specification)
  (setf (slot-value lambda-list '%sections)
        (%call-with-root-lambda-list-setup
         lambda-list
         (lambda ()
           (funcall (parser (kind lambda-list)) specification)))))

(defmethod fcll:parse ((kind symbol) specification)
  (fcll:parse (defsys:locate 'fcll:lambda-list-kind kind)
              specification))

(defmethod fcll:parse ((kind fcll:lambda-list-kind) specification)
  (make-instance 'fcll:standard-lambda-list :kind kind :parse specification))

(defclass standard-expansion-environment (expansion-environment)
  ((%tail-var :initarg :tail-var
              :reader tail-var
              :initform (gensym (string '#:tail)))))
