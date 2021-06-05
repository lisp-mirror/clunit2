(in-package :clunit)

(defun assertion-expander (&key result test result-expression report-expression expected forms)
  "Expands an assertion macro call."
  (let ((multiple-values-results-p (gensym)))
    `(with-assert-restart
       (let ((,result ,result-expression)
             (,multiple-values-results-p ,(and (listp expected)
                                              (eq (first expected)
                                                  'values))))
         (if ,test
             (signal-assertion :pass)
             (signal-assertion :fail
                               :returned   (cond
                                             (,multiple-values-results-p
                                              `(values ,@,result))
                                             ((listp ,result)
                                              (first ,result))
                                             (t
                                              ,result))
                               :expected   ',expected
                               :expression ',report-expression
                               :forms (list ,@(form-expander forms))))))))

(defun form-expander (forms)
"FORM-EXPANDER  manipulates  the  list  of forms  provided  to  an
 assertion  form, e.g.  (defmacro  assert-false (expression  &rest
 forms) . body)

 The members of  the forms list are printed out  when an assertion
 test fails.  The example  below, shows the  debug output  when an
 assertion form fails.

 (let ((x 1) (y 2) (z 3))
       ;;forms = '(x y \"Comment: This is meant to fail.\" z)
       (assert-true (= x y z) x y \"Comment: This is meant to fail.\" z))

      ======== Debug output ===========
      Expression: (= x y z)
      Expected: T
      Returned: NIL
      x => 1
  y => 2
  Comment: This is meant to fail.
      z => 3
      ==================================
      As  you   can  see,  the   reporting  is  somehow   able  to
      differentiate between  the symbols  x, y,  z and  the string
      comment.
      This is achieved by expanding '(x y \"Comment...\" z) => (T 'x
      x T 'y y NIL \"Comment...\" T 'z z)
      The T or NIL symbol  tells the reporting function whether to
      report the next two values as a pair or not.
      I went  at great  lengths to explain  this because  WHAT the
      function does is straight forward  from the code, but WHY it
      does it
      isn't too obvious unless someone tells you :o)"
  (loop for form  in forms
     if (typep form 'string)
     collect nil and collect form
     else
     collect t and collect `',form and collect form))

(defmacro assert-true (expression &body forms)
  "Evaluates  EXPRESSION as  an assertion,  an assertion  passes if  it
returns any non-NIL  value. FORMS and their values are  printed if the
test fails.  Remember in Common Lisp any non-NIL value is true, if you
want  a strict  binary  assertion test  use  (assert-eq t  expression)
instead.

Example:

(assert-true (= 1 1)) ; This assertion passes."
  (with-gensyms (result)
    (assertion-expander :result            result
                        :test              result
                        :result-expression expression
                        :report-expression expression
                        :expected          t
                        :forms             forms)))

(defmacro assert-false (expression &body forms)
  "Evaluates EXPRESSION  as an  assertion, an  assertion passes  if it
returns nil. FORMS and their values are printed if the test fails.

example:

(assert-false (= 1 2)) ; This assertion passes.

"
  (with-gensyms (result)
    (assertion-expander :result            result
                        :test              `(not ,result)
                        :result-expression expression
                        :report-expression expression
                        :expected          nil
                        :forms             forms)))

;; Equality assertion macros.

(defmacro gen-test-form (predicate values results)
  (let ((res (gensym)))
    `(progn
       (assert (listp ,values))
       (assert (listp ,results))
       (assert (functionp ,predicate))
       (and (= (length ,values)
               (length ,results))
            (let ((,res t))
              (loop named inner
                    for value  in ,values
                    for result in ,results do
                      (when (not (funcall ,predicate value result))
                        (setf ,res nil)
                        (return-from inner nil)))
              ,res)))))

(defmacro assert-eq (value expression &body forms)
  "Evaluates EXPRESSION  as an assertion,  an assertion passes  if (EQ
VALUE EXPRESSION) values non nil. FORMS  and their values are printed if
the test fails."
  (with-gensyms (result)
    (assertion-expander :result            result
                        :test              `(gen-test-form #'eq
                                                           (multiple-value-list ,value)
                                                           ,result)
                        :result-expression `(multiple-value-list ,expression)
                        :report-expression `(eq ,value ,expression)
                        :expected          value
                        :forms             forms)))

(defmacro assert-eql (value expression &body forms)
  "Evaluates EXPRESSION as  an assertion, an assertion  passes if (EQL
VALUE EXPRESSION) values non nil. FORMS  and their values are printed if
the test fails."
  (with-gensyms (result)
    (assertion-expander :result            result
                        :test              `(gen-test-form #'eql
                                                           (multiple-value-list ,value)
                                                           ,result)
                        :result-expression `(multiple-value-list ,expression)
                        :report-expression `(eql ,value ,expression)
                        :expected          value
                        :forms             forms)))

(defmacro assert-equal (value expression &body forms)
  "Evaluates EXPRESSION as an assertion, an assertion passes if (EQUAL
VALUE EXPRESSION) values non nil. FORMS  and their values are printed if
the test fails.

example
  (let ((q (+ 2 -2)))
       (assert-equal 4 q q))
  ;; This  assertion   fails  and   prints  the  message   below  with
  ;; *clunit-report-format* set to :DEFAULT.
  Expression: (EQUAL 4 Q)
  Expected: 4
  Returned: 0
  Q => 0
"
  (with-gensyms (result)
    (assertion-expander :result            result
                        :test              `(gen-test-form #'equal
                                                           (multiple-value-list ,value)
                                                           ,result)
                        :result-expression `(multiple-value-list ,expression)
                        :report-expression `(equal ,value ,expression)
                        :expected          value
                        :forms             forms)))

(defmacro assert-equalp (value expression &body forms)
  "Evaluates  EXPRESSION   as  an   assertion,  an   assertion  passes
if (EQUALP VALUE EXPRESSION) values non nil. FORMS and their values are
printed if the test fails."
  (with-gensyms (result)
    (assertion-expander :result            result
                        :test              `(gen-test-form #'equalp
                                                           (multiple-value-list ,value)
                                                           ,result)
                        :result-expression `(multiple-value-list ,expression)
                        :report-expression `(equalp ,value ,expression)
                        :expected          value
                        :forms             forms)))

(defmacro assert-equality (test value expression &body forms)
  "Evaluates  EXPRESSION   as  an   assertion,  an   assertion  passes
if  (FUNCALL TEST  VALUE  EXPRESSION) values non nil.  FORMS and  their
values are printed if the test fails.

Example:

(assert-equality #'string= \"some string\" \"another string\") ; This assertion fails.
"
  (with-gensyms (result)
    (assertion-expander :result            result
                        :test              `(gen-test-form ,test
                                                           (multiple-value-list ,value)
                                                           ,result)
                        :result-expression `(multiple-value-list ,expression)
                        :report-expression `(funcall ,test ,value ,expression)
                        :expected          value
                        :forms             forms)))

(defmacro assert-equality* (value expression &body forms)
  "Evaluates  EXPRESSION   as  an   assertion,  an   assertion  passes
if   (FUNCALL   *clunit-equality-test*   VALUE   EXPRESSION)   returns
true. FORMS and their values are printed if the test fails.

Example:

  (let ((*clunit-equality-test* #'string=))
       (assert-equality* \"some string\" \"another string\")) ; This assertion fails

"
  (with-gensyms (result)
    (assertion-expander :result            result
                        :test              `(gen-test-form ,*clunit-equality-test*
                                                           (multiple-value-list ,value)
                                                           ,result)
                        :result-expression `(multiple-value-list ,expression)
                        :report-expression `(funcall *clunit-equality-test* ,value ,expression)
                        :expected          value
                        :forms             forms)))

;; MACROEXPAND-1 assertion macro
(defmacro assert-expands (&environment env expansion expression &body forms)
  "Evaluates  EXPRESSION   as  an   assertion,  an   assertion  passes
if (EQUALP  EXPANSION (MACROEXPAND-1 EXPRESSION)) values non nil. FORMS
and their values are printed if the test fails."
  (with-gensyms (result)
    (assertion-expander :result            result
                        :test              `(equalp ,result ',expansion)
                        :result-expression `(macroexpand-1 ',expression ,env)
                        :report-expression `(macroexpand-1 ',expression)
                        :expected          expansion
                        :forms             forms)))

;; Condition assertion macro.
(defmacro assert-condition (condition expression &body forms)
  "Evaluates  EXPRESSION  as  an  assertion, an  assertion  passes  if
EXPRESSION signals  CONDITION. FORMS and  their values are  printed if
the test fails.

Example:

(assert-condition arithmetic-error (/ 1 0)) ; This assertion passes.
"
  `(with-assert-restart
     (handler-case
         (progn
           ;; We pass the expression  to EVAL because constant folding
           ;; in the SBCL compiler  removes intentional errors like (/
           ;; 1 0).
           ;; I don't completely understand  the reason for removing a
           ;; constant that signals an error from the compiled code.
           ,expression
           (signal-assertion :fail
                             :expression ',expression
                             :expected   ',condition
                             :forms      (list ,@(form-expander forms))))
       (,condition ()
         (signal-assertion :pass)))))

;; Force assertion failure.
(defun assert-fail (format-string &rest args)
  "Calling this function is equivalent to signalling a failed assertion.
The FORMAT-STRING  and ARGS are used  to print the failure  message as
follows:

  (format stream \"~?\" format-string args)

If  you want  to achieve  a nice  looking output  message, use  pretty
printing  directives in  the format  string e.g.  \"~:@_\" instead  of
\"%\"."
  (with-assert-restart
    (signal-assertion :fail-forced
                      :format-string format-string
                      :args          args)))
