(in-package :clunit)

(define-condition test-suite-failure-condition () ())

(define-condition test-suite-failure (test-suite-failure-condition)
  ((test-errors
    :initarg  :test-errors
    :initform nil
    :documentation "The number of tests with errors.")
   (test-failed
    :initarg  :test-fails
    :initform nil
    :documentation  "The number of tests that failed.")
   (total-tests
    :initarg  :total-tests
    :initform nil
    :documentation "The total number of tests.")))
