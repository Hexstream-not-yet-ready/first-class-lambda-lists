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
                   `(let ((,variable (pop ,tail-var)))
                      ,form)))
         (fcll:lambda-list (let ((lambda-list var-or-lambda-list))
                             (%expand-lambda-list lambda-list `(pop ,tail-var) form))))))
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
                                         (pop ,tail-var)
                                         ,initform)))
                      ,form)))
         (fcll:lambda-list
          (let ((lambda-list var-or-lambda-list))
            (%expand-lambda-list lambda-list
                                 `(if ,tail-var
                                      ,(if suppliedp-variable
                                           `(car ,tail-var)
                                           `(pop ,tail-var))
                                      ,initform)
                                 (if suppliedp-variable
                                     `(let ((,suppliedp-variable (not (null (pop ,tail-var)))))
                                        ,form)
                                     form)))))))
   section
   form))

(defmethod fcll:expand ((section standard-&rest-section) (expansion-env expansion-environment) form)
  (%expand-parameters
   (lambda (parameter form)
     (let ((tail-var (tail-var expansion-env))
           (variable (variable parameter)))
       `(let ((,variable ,tail-var))
          ,form)))
   section
   form))

(defmacro while (condition &body body)
  (let ((loop-tag (gensym (string '#:loop))))
    `(block nil
       (tagbody ,loop-tag
          (unless ,condition
            (return))
          (progn ,@body)
          (go ,loop-tag)))))

(defun %process-keyword-args-strictly (plist values wanted)
  (let ((also-accepted nil))
    (declare (ignore also-accepted))
    ;; TODO.
    (%process-keyword-args-loosely plist values wanted)))

(defun %pop-wanted-keyword (keyword wanted)
  (let ((previous-cons wanted)
        (current-cons (cdr wanted)))
    (while current-cons
      (let ((current (car current-cons))
            (next-cons (cdr current-cons)))
        (when (eq (car current) keyword)
          (setf previous-cons next-cons)
          (return (cdr current)))
        (setf previous-cons current-cons
              current-cons next-cons)))))

(defun %process-keyword-args-loosely (plist values wanted)
  (while (and plist (cdr wanted))
    (let ((next-cons (cdr plist)))
      (let ((indicator (car plist))
            (value (car next-cons)))
        (let ((index (%pop-wanted-keyword indicator wanted)))
          (when index
            (setf (svref values index) value))))
      (setf plist (cdr next-cons)))))

(defmethod fcll:expand ((section standard-&key-section) (expansion-env expansion-environment) form)
  (let ((parameters (parameters section))
        (tail-var (tail-var expansion-env))
        (values-var (gensym (string '#:values_)))
        (absent (gensym (string '#:absent_)))
        (absentp-var nil)
        (wanted-var (gensym (string '#:wanted_)))
        (value-var (gensym (string '#:value_)))
        (process-keyword-args (if (allow-other-keys-p section)
                                  '%process-keyword-args-loosely
                                  '%process-keyword-args-strictly)))
    `(let ((,values-var (make-array ,(length parameters) :initial-element ',absent))
           (,wanted-var (list :wanted ,@(mapcar (let ((i -1))
                                                  (lambda (parameter)
                                                    `'(,(keyword-name parameter) . ,(incf i))))
                                                parameters))))
       (declare (dynamic-extent ,values-var ,wanted-var))
       (,process-keyword-args ,tail-var ,values-var ,wanted-var)
       (let* (,@(mapcan (let ((i -1))
                          (lambda (parameter)
                            (let* ((suppliedp-variable (suppliedp-variable parameter))
                                   (is-value-absent `(eq ,value-var ',absent))
                                   (value-or-initform
                                    `(if ,(if suppliedp-variable
                                              `(setf ,(or absentp-var
                                                          (setf absentp-var
                                                                (gensym (string '#:absentp_))))
                                                     ,is-value-absent)
                                              is-value-absent)
                                         ,(initform parameter)
                                         ,value-var))
                                   (get-value `(,value-var (svref ,values-var ,(incf i))))
                                   (variable-binding
                                    `(,(variable parameter)
                                       ,(if suppliedp-variable
                                            value-or-initform
                                            `(let (,get-value)
                                               ,value-or-initform)))))
                              (if suppliedp-variable
                                  `(,get-value
                                    (,absentp-var ,is-value-absent)
                                    ,variable-binding
                                    (,suppliedp-variable (not ,absentp-var)))
                                  (list variable-binding)))))
                        parameters))
         ,form))))

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
