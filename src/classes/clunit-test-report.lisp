(in-package :clunit)

(defclass clunit-test-report ()
  ((test-name
    :initform nil
    :initarg :test-name
    :reader   test-report-name
    :documentation    "the     reader    function    is     used    in
    test-case-execution-action (see utility functions)")
   (passed-p
    :initform t
    :initarg  :passed-p
    :reader test-report-passed-p)
   (skipped-p
    :initform nil
    :initarg  :skipped-p)
   (suite-list
    :initform (list)
    :initarg  :suite-list)
   (assertion-conditions
    :initform (list)
    :documentation "the clunit-test-report instance  is used to store
     the report information for each executed test case.")))
