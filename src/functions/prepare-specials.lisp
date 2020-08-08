(in-package :clunit)

(defmacro with-prepare-specials-for-testing ((report-progress
                                              use-debugger
                                              stop-on-fail)
                                             &body body)
  "Set some common  special variables to meaningful  values to prepare
for running tests or suite"
  `(let ((*clunit-report*   (make-instance 'clunit-report))
        (*report-progress* ,report-progress)
        (*use-debugger*    ,use-debugger)
        (*stop-on-fail*    ,stop-on-fail))
     ,@body))
