(in-package :clunit)

(defclass clunit-test-suite ()
  ((name
    :initarg :name
    :initform nil)
   (test-cases
    :initarg  :test-cases
    :initform (list))
   (child-suites
    :initarg  :child-suites
    :initform (list)))
  (:documentation   "The    slots   test-cases,    child-suites   and
 parent-suites hold  the symbol  names of test  cases and  test suites
 instead of the actual objects.  using an indirect reference like this
 allows us to undefine a test case or test suite.

 When we execute a test suite and  try to resolve the reference for an
 object.  if  the object is not  found, it means the  reference is now
 stale so the name of that test case or suite is removed."))
