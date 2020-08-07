(in-package :clunit)

(defclass clunit-test-report ()
  ((test-name
    :initform nil
    :initarg :test-name
    :accessor   test-report-name
    :documentation    "the     reader    function    is     used    in
    test-case-execution-action (see utility functions)")
   (passed-p
    :initform t
    :initarg  :passed-p
    :reader test-report-passed-p
    :writer (setf test-report-passed))
   (skipped-p
    :initform nil
    :initarg  :skipped-p
    :reader skipped-p
    :writer (setf skipped-tests))
   (suite-list
    :initform ()
    :initarg  :suite-list
    :accessor suite-list)
   (assertion-conditions
    :initform ()
    :accessor assertion-conditions
    :documentation "the clunit-test-report instance  is used to store
     the report information for each executed test case.")))
