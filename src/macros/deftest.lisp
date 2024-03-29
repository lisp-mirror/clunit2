(in-package :clunit)

(defun collect-all-suite-dependencies (direct-dependency-list)
  (let ((results ()))
    (loop for suite in direct-dependency-list do
      (let ((dependencies (collect-suite-dependencies suite :symbol-suite-only t)))
        (loop for dependency in dependencies do
          (pushnew dependency results))))
    (append direct-dependency-list results)))

;; The DEFTEST macro has three possible forms:
;;
;;      1. Define a test case not associated with any test suite and with no dependencies.
;;              (deftest name () . body)
;;
;;      2. Define a test case which is associated with test suites: suite1 ... suiteN.
;;              (deftest name (suite1 suite2 ... suiteN) . body)
;;
;;      3.  Define a  test case  associated with  test suites:  suite1
;;      ... suiteN and depends on tests: test1 ... testN.
;;              (deftest name ((suite1 suite2 ... suiteN) (test1 test2 ... testN)) . body)
;;
;; DEFTEST Algorithm:
;; 1. Establish the DEFTEST form used.
;; 2. If test is associated with 1 or more test suites make sure all test suites are defined.
;; 3. If test depends on 1 or more test cases, emit a warning for any test case not defined.
;;    Remember if test does not exist at runtime the reference is removed. Whereas test suites
;;    have to be defined before we can refer to them.
;; 4. Add reference in each the test suite's TEST-CASES slot.
;; 5. Create a named lambda test function.
;; 6. Create new test suite instance and add it to lookup table.
;;

(defmacro deftest (name declarations &body body)
  "Defines a  test case called  NAME. DECLARATIONS declares  which test
suites this  test case is  associated with as  well as any  other test
cases that  it depends on.  The  test case body is  revaluated on each
run,  so any  redefinition  of  macros and  inline  functions will  be
automatically visible without having to redefine the test.

A test case will be queued until all tests cases it depends on have been run.
If all the test cases pass the queued test is executed otherwise its skipped.

The DEFTEST macro has three possible forms:

- Define a test case not associated with any test suite and with no dependencies.<br/>
   (deftest name () . body)

- Define a test case which is associated with test suites: suite1 ... suiteN.
   (deftest name (suite1 suite2 ... suiteN) . body)

- Define a test case associated with test suites: suite1 ... suiteN and
  depends on tests: test1 ... testN

   (deftest name ((suite1 suite2 ... suiteN) (test1 test2 ... testN)) . body)"
  (with-gensyms (parent-suites test-dependencies test-function)
    (multiple-value-bind (dependencies-suites dependencies-tests)
        (deftest-declaration-dependencies declarations)
      ;; Emit warnings for all dependencies on test cases that have not yet been defined.
      (ensure-dependency-exists name
                                dependencies-tests
                                #'get-test-case
                                "Defining test case ~S which has a dependency on undefined test case ~S.")
      (ensure-dependency-exists name
                                dependencies-suites
                                #'get-test-suite
                                "Trying to add test case ~S reference to test suite, but test suite ~S is not defined.")

      `(eval-when  (:execute :load-toplevel :compile-toplevel)
         (let ((,parent-suites     ',dependencies-suites)
               (,test-dependencies ',dependencies-tests)
               (,test-function     nil))
           ;; Add test case reference to each of its parent's TEST-CASES slot.
           (loop for parent in ,parent-suites do
             (pushnew ',name (test-cases (get-test-suite parent))))
           (setf ,test-function
                 (lambda ()
                   (block ,name
                     (with-test-restart
                         (let ((*test-name* ',name))
                           ;; If test  was not  called by any  test suite,
                           ;; then  do  not  attempt  to  expand  out  any
                           ;; fixtures.
                           ;; However, if the test  is being executed in
                           ;; a context with one or more test suites,
                           ;; expand out the fixtures in topological order
                           ,(let ((body-forms              `(progn ,@body))
                                  (all-dependencies-suites (collect-all-suite-dependencies dependencies-suites)))
                              (dolist (suite all-dependencies-suites)
                                (setf body-forms (expand-fixture suite body-forms)))
                              body-forms))))))
           ;; Create new test case instance and add it to lookup table.
           (add-test-case ',name
                          (make-instance 'clunit-test-case
                                         :name          ',name
                                         :dependencies  ,test-dependencies
                                         :test-function ,test-function)))))))

;; UNDEFTEST Algorithm:
;; 1. Check if test case is defined, if its not throw an error.
;; 2. Undefine test
;;
;; N.B. Remember  we do not bother  tracing the suites that  contain a
;; reference  to  this  test  case  because  they  will  update  their
;; references when they realise that it no longer exists.
;;
(defmacro undeftest (name)
  `(progn
     (unless (symbolp ',name)
       (error "In (undeftest name), NAME should be a symbol not ~S." ',name))
     (unless (get-test-case ',name)
       (error "In (undeftest name), test case ~S is not defined. " ',name))
     (delete-test-case ',name)))
