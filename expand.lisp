(in-package #:first-class-lambda-lists)

(defclass expansion-environment ()
  ())

(defgeneric fcll:expand (object expansion-env form))

(defun %expand-lambda-list (lambda-list expression form)
  (let ((expansion-env (make-instance 'standard-expansion-environment)))
    `(let ((,(tail-var expansion-env) ,expression))
       ,(fcll:expand lambda-list expansion-env form))))

(defmacro bind (lambda-list-kind lambda-list expression &body body)
  (let ((lambda-list (make-instance 'fcll:standard-lambda-list :kind lambda-list-kind :parse lambda-list)))
    (%expand-lambda-list lambda-list expression (if (= (length body) 1)
                                                    (car body)
                                                    `(progn ,@body)))))


;;; Sections

(defun %expand-parameters (expand-parameter section form)
  (reduce expand-parameter (parameters section) :from-end t :initial-value form))

(defmethod fcll:expand ((section standard-required-section) (expansion-env expansion-environment) form)
  (%expand-parameters
   (lambda (parameter form)
     (let ((tail-var (tail-var expansion-env))
           (var-or-lambda-list (variable parameter)))
       (etypecase var-or-lambda-list
         (symbol (let ((variable var-or-lambda-list))
                   `(let ((,variable (prog1 (car ,tail-var)
                                       (setf ,tail-var (cdr ,tail-var)))))
                      ,form)))
         (fcll:lambda-list (let ((lambda-list var-or-lambda-list))
                             (%expand-lambda-list lambda-list
                                                  `(prog1 (car ,tail-var)
                                                     (setf ,tail-var (cdr ,tail-var)))
                                                  form))))))
   section
   form))

(defmethod fcll:expand ((section standard-&optional-section) (expansion-env expansion-environment) form)
  (%expand-parameters
   (lambda (parameter form)
     (let ((tail-var (tail-var expansion-env))
           (var-or-lambda-list (variable parameter))
           (initform (initform parameter))
           (suppliedp-variable (suppliedp-variable parameter)))
       (etypecase var-or-lambda-list
         (symbol (let ((variable var-or-lambda-list))
                   `(let (,@(when suppliedp-variable
                                  (list `(,suppliedp-variable (not (null ,tail-var)))))
                          (,variable (if ,tail-var
                                         (prog1 (car ,tail-var)
                                           (setf ,tail-var (cdr ,tail-var)))
                                         ,initform)))
                      ,form)))
         (fcll:lambda-list
          (let ((lambda-list var-or-lambda-list))
            (%expand-lambda-list lambda-list
                                 `(if ,tail-var
                                      ,(if suppliedp-variable
                                           `(car ,tail-var)
                                           `(prog1 (car ,tail-var)
                                              (setf ,tail-var (cdr ,tail-var))))
                                      ,initform)
                                 (if suppliedp-variable
                                     `(let ((,suppliedp-variable (prog1 (not (null ,tail-var))
                                                                   (setf ,tail-var (cdr ,tail-var)))))
                                        ,form)
                                     form)))))))
   section
   form))

(defmethod fcll:expand ((section standard-&aux-section) (expansion-env expansion-environment) form)
  (%expand-parameters
   (lambda (parameter form)
     (let ((variable (variable parameter))
           (initform (initform parameter)))
       `(let ((,variable ,initform))
          ,form)))
   section
   form))


;;; Lambda lists

(defmethod fcll:expand ((lambda-list fcll:standard-lambda-list) (expansion-env expansion-environment) body)
  (reduce (lambda (section body)
            (fcll:expand section expansion-env body))
          (%sections lambda-list)
          :from-end t
          :initial-value body))
