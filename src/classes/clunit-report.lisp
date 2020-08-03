(in-package :clunit)

(defclass clunit-report ()
  ((errors
    :initform 0
    :initarg  :errors
    :documentation  "The  number  of  tests that  signalled  an  error
    condition during test executions.")
   (failed
    :initform 0
    :initarg  :failed
    :documentation "The number of tests failed during test executions.")
   (passed
    :initform 0
    :initarg  :passed
    :documentation "The number of tests that passed during test executions.")
   (skipped
    :initform 0
    :initarg  :skipped
    :documentation "The number of tests that was skipped during test executions.")
   (test-reports
    :initform ()
    :reader   test-reports
    :documentation "A list of `cl-test-report' that contains all the test's reports"))
  (:documentation  "the clunit-report  instance is  used to  store the
 aggregated reports of all executed test cases (returned by `run-test'
 and `rerun-failed-tests'."))
