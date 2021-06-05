(in-package :clunit)

(defun run-suite (suite &key use-debugger (report-progress t) stop-on-fail
                          signal-condition-on-fail
                          (print-results-summary t))
  "Executes a  test case called  SUITE. If REPORT-PROGRESS  is non-NIL,
the  test  progress  is  reported.

If  USE-DEBUGGER  is non-NIL,  the  debugger  is invoked  whenever  an
assertion fails.

If STOP-ON-FAIL  is non-NIL, the  rest of  the unit test  is cancelled
when any assertion fails or an error occurs.

if PRINT-RESULTS-SUMMARY  is non nil  the summary results of  tests is
printed on the standard output."
  (with-prepare-specials-for-testing (report-progress use-debugger stop-on-fail)
    (let ((test-suite (get-test-suite suite)))
      (unless test-suite
        (error "Test suite ~S is not defined." suite))
      (handler-bind ((error #'handle-error)
                     (warning #'muffle-warning)
                     (assertion-condition #'handle-assertion))
        (restart-case
            (progn
              (when *report-progress*
                (format *test-output-stream* "~%PROGRESS:~%========="))
              (setf *queued-test-reports* (list) *last-clunit-report* *clunit-report*)
              (execute-test-suite test-suite)
              (when *queued-test-reports*
                (when *report-progress*
                  (format *test-output-stream* "~%~%QUEUED TESTS:~%============="))
                (process-queued-tests)))
          (cancel-unit-test ()
            :report (lambda (s) (format s "Cancel unit test execution."))
            nil)))
      (setf *clunit-equality-test* #'equalp) ; Restore *CLUNIT-EQUALITY-TEST* to its default value
      (when print-results-summary
        (format *test-output-stream* "~%~a~%" *clunit-report*))
      (with-slots (passed failed errors)
          *clunit-report*
        (when (and signal-condition-on-fail
                   (or (plusp failed)
                       (plusp errors)))
          (error 'test-suite-failure
                 :test-errors errors
                 :test-fails failed
                 :total-tests (+ errors failed passed))))
      *clunit-report*)))

(defun execute-test-suite (test-suite)
  (with-slots (name test-cases child-suites) test-suite
    (report-suite-progress name *suite-name*)
    (restart-case
        ;; Extend the test suite call-chain list by appending the name
        ;; of the test suite being called.
        (let (obj (*suite-name* (append *suite-name* (list name))))
          ;; Execute test cases.
          (dolist (test test-cases)
            (setf obj (get-test-case test))
            ;; Check  if  reference is  now  stale,  if it  is  delete
            ;; it. References  become stale when  a test case  or test
            ;; suite was undef'ed.
            (if obj
                (execute-test-case obj)
                (setf test-cases (delete test test-cases))))
          ;; Execute child suites.
          (dolist (suite child-suites)
            (setf obj (get-test-suite suite))
            (if obj
                (execute-test-suite obj)
                (setf child-suites (delete suite child-suites)))))
      (skip-suite ()
        :report (lambda (s) (format s "Skip test suite ~S." name))
        nil))))

(defun get-defined-suites ()
  "Returns a list of all defined test suite names."
  (loop for key being the hash-key of *test-suite-hashtable* collect key))

(defun get-test-suite (name)
  "Retrieves the TEST-SUITE  instance associated with the  key NAME in
the hash table *test-suite-hashtable*"
  (gethash name *test-suite-hashtable*))

(defun (setf get-test-suite) (new-test-suite name)
  "Adds NEW-TEST-SUITE in  the hash table *test-suite-hashtable*  under the key
NAME."
  (setf (gethash name *test-suite-hashtable*) new-test-suite))

(defun defined-suite-p (suite-name)
  "Returns  non-nil if  a  test suite  called  SUITE-NAME is  defined,
otherwise returns NIL."
  (get-test-suite suite-name))

(defun delete-test-suite (name)
  "Deletes the TEST-SUITE instance associated with the key NAME in the
hash table *test-suite-hashtable*"
  (remhash name *test-suite-hashtable*))

(defun get-child-tests (suite-name)
  "Returns a  list of  all test  case names that  are children  of the
suite called SUITE-NAME."
  (let ((suite (get-test-suite suite-name)))
    (when suite
      (test-cases suite))))

(defun get-child-suites (suite-name)
  "Returns a  list of all  test suite names  that are children  of the
suite called SUITE-NAME."
  (let ((suite (get-test-suite suite-name)))
    (when suite
      (child-suites suite))))
