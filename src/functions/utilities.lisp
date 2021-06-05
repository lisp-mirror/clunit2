(in-package :clunit)

(defun all-symbols-p (list error-control-message)
  "Ensures `list' is a list of symbols.

In deftest  `declarations' can be  a list of suites  or a list  of two
elements, the first a  list of suites and the second  a list of tests,
each memebre of said list must be a symbol.
"
  (loop for i in list do
    (when (not (symbolp i))
      (error error-control-message list i))))

(defun deftest-declaration-dependencies (declarations)
  "When  `declarations' comes  from  the `deftest'  macro returns  the
dependencies (suites and optionally tests)"
  (unless (listp declarations)
    (error "In (deftest name declarations . body), DECLARATION should be a list not ~S."
           declarations))
  (let* ((maybe-list-suites       (first declarations))
         (maybe-list-tests        (second declarations))
         (separated-list-suites-p (and maybe-list-suites
                                       (listp maybe-list-suites)))
         (suites                  (if separated-list-suites-p
                                      maybe-list-suites
                                      declarations))
         (tests                   (if separated-list-suites-p
                                      maybe-list-tests
                                      nil)))
    (all-symbols-p suites
      "Every member of parent suites should be a symbol but ~s contains ~s which is not a symbol.")
    (all-symbols-p tests
      "Every member of test dependency should be a symbol but ~s contains ~s which is not a symbol.")
    (values suites tests)))

(defun ensure-dependency-exists (name list predicate control-message)
  (loop for i in list do
    (when (not (funcall predicate i))
      (error control-message name i))))
