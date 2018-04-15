(in-package :clunit)

(define-condition assertion-condition () ())

(define-condition assertion-passed (assertion-condition) ())

(define-condition assertion-error (assertion-condition)
  ((message
    :initarg  :message
    :initform ""
    :documentation "this is a special case, if an unexpected condition
    is  signalled  outside  an  assertion test,  store  the  condition
    description here.")))

(define-condition assertion-failed (assertion-condition)
  ((expression
    :initarg  :expression
    :initform nil
    :documentation "the expression that was tested.")
   (forms
    :initarg  :forms
    :initform nil
    :documentation  "holds a  list of  form-value pairs,  see function
    expand-forms for more detail.")
   (expected
    :initarg  :expected
    :initform nil
    :documentation "the value the expression was expected to return.")
   (returned
    :initarg :returned
    :initform nil
    :documentation "the result that was returned from evaluating the expression.")))

(define-condition assertion-fail-forced (assertion-condition)
  ((format-string
    :initarg
    :format-string
    :initform ""
    :documentation "the format string argument passed to format.")
   (args
    :initarg  :args
    :initform ""
    :documentation "the format string arguments.")))
