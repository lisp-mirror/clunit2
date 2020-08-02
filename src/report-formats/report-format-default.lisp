(in-package :clunit)

;; README!!!!  The  pretty printing  in CLISP is  really broken  so we
;; just try has hard as we can to compensate.
(defmethod print-format ((clunit-report clunit-report) (format (eql :default)) stream)
  (pprint-logical-block (stream nil)
    (with-slots (passed failed errors skipped test-reports) clunit-report
      (when (plusp (+ failed errors))
        #-clisp (format stream "~:@_FAILURE DETAILS:~:@_~4:I================")
        #+clisp (format stream "~:@_FAILURE DETAILS:~:@_~8I================")
        (let (suite)
          (dolist (report (remove-if #'test-report-passed-p test-reports))
            (unless (equal suite (slot-value report 'suite-list))
              (setf suite (slot-value report 'suite-list))
              #-clisp (format stream "~4I~:@_~:@_~8I~A~{~^ -> ~A~}: (Test Suite)"
                              (first suite) (rest suite))
              #+clisp (format stream "~-4I~:@_~:@_~A~{~^ -> ~A~}: (Test Suite)~4I"
                              (first suite) (rest suite)))
            (format stream "~:W" report))))
      #-clisp (format stream  "~:@_~I~:@_SUMMARY:~:@_========")
      #+clisp (format stream  "~:@_~-8I~:@_SUMMARY:~:@_========")
      (let ((total (+ passed failed errors)))
        #-clisp (format stream
                        "~4I~:@_Test functions:~8I~:@_Executed: ~D~:@_Skipped:  ~D~4I~:@_~:@_Tested ~D assertion~:P.~8I"
                        (- (length test-reports) skipped) skipped total)
        #+clisp (format stream
                        "~4I~:@_Test functions:~8I~:@_Executed: ~D~:@_Skipped:  ~D~-8I~:@_~:@_Tested ~D assertion~:P.~8I"
                        (- (length test-reports) skipped) skipped total)
        (unless (zerop total)
          (let ((all-passed (and (zerop failed)
                                 (zerop errors)))
                (all-failed (and (zerop passed)
                                 (zerop errors)))
                (all-errors (and (zerop passed)
                                 (zerop failed))))
            (when (plusp passed)
              (format stream
                      "~:@_Passed: ~D/~D ~:[some tests not passed~;all tests passed~]"
                      passed total all-passed))
            (when (plusp failed)
              (format stream
                      "~:@_Failed: ~D/~D ~:[some tests failed~;all tests failed~]"
                      failed total all-failed))
            (when (plusp errors)
              (format stream "~:@_Errors: ~D/~D ~:[some tests had errors~;all tests had errors~]"
                      errors total all-errors))))))))

(defmethod print-format ((report clunit-test-report) (format (eql :default)) stream)
  (with-slots (test-name assertion-conditions) report
    (dolist (condition assertion-conditions)
      (unless (typep condition 'assertion-passed)
        (format stream "~:@_~A: ~<~:W~:>~:@_" test-name condition)))))

(defmethod print-format ((condition assertion-error) (format (eql :default)) stream)
  (pprint-logical-block (stream nil)
    (with-slots (message) condition
      (format stream "~A" message))))

(defmethod print-format ((condition assertion-failed) (format (eql :default)) stream)
  (pprint-logical-block (stream nil)
    (with-slots (expression expected returned forms) condition
      (format stream "Expression: ~S~:@_Expected: ~A~:@_Returned: ~A~{~^~:@_~:[~A~;~S => ~S~]~}"
              expression expected returned forms))))

(defmethod print-format ((condition assertion-fail-forced) (format (eql :default)) stream)
  (pprint-logical-block (stream nil)
    (with-slots (format-string args) condition
      (format stream "~?" format-string args))))
