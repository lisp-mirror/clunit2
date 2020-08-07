(in-package :clunit)

(defun rerun-failed-tests (&key
                             use-debugger
                             (report-progress t)
                             stop-on-fail
                             (last-report *last-clunit-report*)
                             (print-results-summary t))
  "Reruns all  failed tests recorded in  LAST-REPORT which defaults
to the  report of  the previous test  run. If  REPORT-PROGRESS is
non-NIL,  the  test progress  is  reported.   If USE-DEBUGGER  is
non-NIL, the debugger is invoked whenever an assertion fails.

If  STOP-ON-FAIL  is  non-NIL,  the  rest of  the  unit  test  is
cancelled when any assertion fails or an error occurs."
  (with-prepare-specials-for-testing (report-progress use-debugger stop-on-fail)
    (handler-bind ((error #'handle-error)
                   (warning #'muffle-warning)
                   (assertion-condition #'handle-assertion))
      (restart-case
          (progn
            (when *report-progress*
              (format *test-output-stream* "~%PROGRESS:~%========="))
            (setf *queued-test-reports* (list)
                  *last-clunit-report*  *clunit-report*)
            (flet ((process-test-report (test-report)
                     (with-slots (test-name suite-list passed-p) test-report
                       (unless passed-p
                         (let ((test-case    (get-test-case test-name))
                               (*suite-name* suite-list))
                           (when test-case
                             (when *report-progress*
                               (format *test-output-stream*
                                       "~%~VT~S~{~^ -> ~S~}: (Test Suite)"
                                       *tab-width*
                                       (first suite-list)
                                       (rest suite-list)))
                             (execute-test-case test-case)))))))
              (mapc #'process-test-report (test-reports last-report))))
        (cancel-unit-test ()
          :report (lambda (s) (format s "Cancel unit test execution."))
          nil)))
    (when print-results-summary
      (format *test-output-stream* "~%~a~%" *clunit-report*))
    *clunit-report*))
