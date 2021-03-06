(in-package :clunit)

(defclass named-class ()
  ((name
    :initarg  :name
    :initform nil
    :accessor name))
  (:documentation "A class for object with an identifier (the slot 'name')"))

(defclass clunit-test-case (named-class)
  ((dependencies
    :initarg :dependencies
    :initform ()
    :accessor dependencies
    :documentation "The DEPENDENCIES slot holds the names of the test cases that
 this test  case depends  on.  Using an  indirect reference  like this
 allows us to easily undefine a test case without much cleaning up.

 When we  execute a  test case  and we resolve  the references  of the
 tests  it depends  on.   If the  object  is not  found  it means  the
 reference is now stale, so we remove the name from the list.")
   (test-function
    :initarg  :test-function
    :initform nil
    :accessor test-function
    :documentation "The actual test, a function with no arguments that
    returns non nil if the test passed")))
