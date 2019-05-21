(in-package #:first-class-lambda-lists)

;;;; INCOMPLETE.

(defun %keyword-order-to-nodes (keyword-order)
  (flet ((recurse (spec)
           (etypecase spec
             (cons (destructuring-bind (operator &rest args) spec
                     (check-type args cons)
                     (make-instance (ecase operator
                                      (list 'list-node)
                                      (or 'or-node))
                                    :args (mapcar #'recurse args))))
             (fcll:lambda-list-keyword
              (make-instance 'lambda-list-keyword-node :lambda-list-keyword spec)))))
    (make-instance 'root-node :child (recurse (tree keyword-order)))))

(defvar *backtrackp*)

(defgeneric transform-node (transformer node))

(defclass standard-compiler () ())

(defclass node () ())

(defclass root-node (node)
  ((%child :initarg :child
           :reader child
           :type node
           :initform (error ":child initarg is required."))))

(defmethod transform-node ((compiler standard-compiler) (node root-node))
  (transform-node compiler (child node))
  (lambda (tail)
    (let ((*sections* nil)
          (*%malformed-lambda-list*
           (lambda (error-type &rest args)
             (apply #'error error-type
                    :root-lambda-list *root-lambda-list*
                    :specification tail
                    args))))
      (multiple-value-bind (new-tail donep)
          (let* ((parser (funcall parser-maker))
                 (*parse-recursable-variable* parse-recursable-variable))
            (funcall parser tail))
        (let ((sections (nreverse *sections*)))
          (if new-tail
              (%malformed-lambda-list 'simple-malformed-lambda-list-error
                                      :tail new-tail
                                      :format-control "Could not completely parse lambda list.~@
                                                       donep: ~S~%sections: ~S"
                                      :format-arguments (list donep sections))
              sections))))))

(defclass args-mixin ()
  ((%args :initarg :args
          :reader args
          :type list)))

(defclass list-node (node args-mixin)
  ())

(defun %transform-args (compiler args)
  (mapcar (lambda (arg)
            (transform-node compiler arg))
          args))

(defmethod transform-node ((compiler standard-compiler) (node list-node))
  (let ((arg-processors (%transform-args compiler (args node))))
    (lambda (tail)
      (when tail
        (block nil
          (mapl (lambda (processors)
                  (let ((new-tail (funcall (first processors) tail)))
                    (unless new-tail
                      (setf tail new-tail)
                      (return))
                    (unless (eq new-tail tail)
                      (setf arg-processors (rest processors)
                            tail new-tail))))
                arg-processors)))
      (values tail (not arg-processors)))))

(defclass or-node (node args-mixin)
  ())

(defmethod transform-node ((compiler standard-compiler) (node or-node))
  (let ((arg-processor-dispenser (%make-or-processor-dispenser
                                  (let ((*backtrackp* t))
                                    (%transform-args compiler (args node))))))
    (lambda (tail)
      (if tail
          (let ((donep (block nil
                         (loop
                            (multiple-value-bind (new-tail donep)
                                (funcall arg-processor-dispenser tail)
                              (setf tail new-tail)
                              (when (or donep (not tail))
                                (return donep)))))))
            (values tail (not (eq donep :stuck))))
          (values tail nil t)))))

(defclass lambda-list-keyword-node (node)
  ((%lambda-list-keyword :initarg :lambda-list-keyword
                         :reader fcll:lambda-list-keyword
                         :type fcll:lambda-list-keyword)))
