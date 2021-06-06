cage


Table of Contents
─────────────────

1. Important note: This branch is experimental
2. CLUnit
3. Overview
4. Example
5. Tests and assertions
6. BUGS
7. Notes


1 Important note: This branch is experimental
═════════════════════════════════════════════


2 CLUnit
════════


3 Overview
══════════

  CLUnit is a Common Lisp unit testing framework. It is designed to be
  easy to use so that you can quickly start testing.

  • Author: Tapiwa Gutu
  • Maintainer of this fork: cage

  CLUnit provides a rich set of features aimed at improving your unit
  testing experience:

  ⁃ Multiple inheritance for test suites allows you to group tests into
    hierarchies.
  ⁃ Composes the test results of each test run into a single report.
  ⁃ Allows redefinition of inline functions and macros without having to
    redefine your tests.
  ⁃ Supports composable test suite fixtures.
  ⁃ Allows for an interactive testing process which gives you access to
    the test environment.
  ⁃ Provides visual feedback of the unit test progress.
  ⁃ Extensible test reporting. Builtin support for default reporting and
    ["Test Anything Protocol" (TAP)] output.
  ⁃ Released under MIT license

  Check out the comprehensive [CLUnit Tutorial] [archived URL].


["Test Anything Protocol" (TAP)]
<http://en.wikipedia.org/wiki/Test_Anything_Protocol>

[CLUnit Tutorial] <http://tgutu.github.io/clunit>

[archived URL]
<https://web.archive.org/web/20200929000204/https://tgutu.github.io/clunit/>


4 Example
═════════

  ┌────
  │ (ql:quickload "clunit")
  │
  │ (use-package :clunit)
  │
  │ ;; Test suite for all number operation tests.
  │ (defsuite number-suite ())
  │
  │ ;; Test suite for floating point operations
  │ (defsuite float-suite (number-suite))
  │
  │ (defsuite integer-suite (number-suite))
  │
  │ ;; Define a test called TEST-INT1
  │ (deftest test-int-1 (integer-suite)
  │   (assert-true  (= 1 1))
  │   (assert-equalp 4 (+ 2 2)))
  │
  │ ;; Define a test called TEST-FLOAT1
  │ (deftest test-float-1 (float-suite)
  │   (assert-true (= 1.0 -1.0))
  │   (assert-equalp 4.0 (+ 2.0 2.0)))
  │
  │ ;; expressions returning multiple value van be tested too
  │ (defun approx-equal (a b) (< (- a 0.1) b (+ a 0.1)))
  │
  │ (deftest test-float-2 (float-suite)
  │   (assert-equality #'approx-equal
  │       (values 1 0.5)
  │       (floor 1.5)))
  │
  │ (run-suite 'number-suite)
  │
  └────

  which produces the output:

  ┌────
  │ PROGRESS:
  │ =========
  │
  │     NUMBER-SUITE: (Test Suite)
  │
  │         INTEGER-SUITE: (Test Suite)
  │             TEST-INT-1: ..
  │
  │         FLOAT-SUITE: (Test Suite)
  │             TEST-FLOAT-2: .
  │             TEST-FLOAT-1: F.
  │
  │ FAILURE DETAILS:
  │ ================
  │
  │     NUMBER-SUITE -> FLOAT-SUITE: (Test Suite)
  │         TEST-FLOAT-1: Expression: (= 1.0 -1.0)
  │                       Expected: T
  │                       Returned: NIL
  │
  │
  │ SUMMARY:
  │ ========
  │     Test functions:
  │         Executed: 3
  │         Skipped:  0
  │
  │     Tested 5 assertions.
  │         Passed: 4/5 some tests not passed
  │         Failed: 1/5 some tests failed
  └────


5 Tests and assertions
══════════════════════

  Each test, like `test-int1' in the above example, can contain a number
  of assertions, given in the table below:

  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Assertion                                Description
  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
   `assert-true EXPRESSION'                 Passes if the expression `EXPRESSION' is not `NIL'
   `assert-false EXPRESSION'                Passes if `EXPRESSION' is `NIL'
   `assert-eq VALUE EXPRESSION'             Passes if `(EQ VALUE EXPRESSION)' returns true
   `assert-eql VALUE EXPRESSION'            Passes if `(EQL VALUE EXPRESSION)' returns true
   `assert-equal VALUE EXPRESSION'          Passes if `(EQUAL VALUE EXPRESSION)' returns true
   `assert-equalp VALUE EXPRESSION'         Passes if `(EQUALP VALUE EXPRESSION)' returns true
   `assert-equality TEST VALUE EXPRESSION'  Passes if `(FUNCALL TEST VALUE EXPRESSION)' returns true
   `assert-equality* VALUE EXPRESSION'      Passes if  `(FUNCALL *clunit-equality-test* VALUE EXPRESSION)' returns true. By default *clunit-equality-test* is `EQUALP'
   `assert-expands EXPANSION EXPRESSION'    Tests macro expansion, passes if `(EQUALP EXPANSION (MACROEXPAND-1 EXPRESSION))' is true
   `assert-condition CONDITION EXPRESSION'  Passes if `EXPRESSION' signals `CONDITION'
   `assert-fails FORMAT-STRING'             Force test to fail, giving a format string for the message
  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  All of these tests take optional forms, which are evaluated and
  printed if the test fails.  These can be used to provide test
  diagnostics or documentation. For example

  ┌────
  │ (deftest test-suiteless ()
  │   (let ((a 1) (b 2) (c 3))
  │     (assert-true (= a b c) "This assertion is meant to fail." a b c )))
  │
  │ (run-test 'test-suiteless :report-progress nil)
  └────

  produces the output:

  ┌────
  │ FAILURE DETAILS:
  │ ================
  │     TEST-SUITELESS: Expression: (= A B C)
  │                     Expected: T
  │                     Returned: NIL
  │                     This assertion is meant to fail.
  │                     A => 1
  │                     B => 2
  │                     C => 3
  │
  │
  │ SUMMARY:
  │ ========
  │     Test functions:
  │         Executed: 1
  │         Skipped:  0
  │
  │     Tested 1 assertion.
  │         Failed: 1/1 all tests failed
  └────


6 BUGS
══════

  Please file bug report on the [issue tracker]


[issue tracker] <https://notabug.org/cage/clunit2/issues>


7 Notes
═══════

  This is a fork of <https://github.com/tgutu/clunit> .
