(in-package :clunit)

(defconstant +assertion-conditions-reserved-size+ 100)

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
    :writer (setf test-report-passed)
    :documentation "Non-nil if the report passed")
   (skipped-p
    :initform nil
    :initarg  :skipped-p
    :reader skipped-p
    :writer (setf skipped-tests)
    :documentation "Non-nil if the test was skipped")
   (suite-list
    :initform ()
    :initarg  :suite-list
    :accessor suite-list
    :documentation "A list of the suites this test belong.")
   (assertion-conditions
    :initform (make-array +assertion-conditions-reserved-size+ :adjustable t :fill-pointer 0)
    :accessor assertion-conditions
    :documentation "the clunit-test-report instance  is used to store
     the report information for each executed test case.")))
