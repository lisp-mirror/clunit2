(in-package :clunit)

(defun report-suite-progress (suite suite-list)
  (when *report-progress*
    (format *test-output-stream* "~%~%~VT~A: (Test Suite)"
            (* *tab-width* (1+ (length suite-list))) suite)))

(defun report-test-progress (test-name suite-list)
  (when *report-progress*
    (format *test-output-stream* "~%~VT~A: " (* *tab-width*
                                                (1+ (length suite-list)))
            test-name)))

(defun report-assertion-progress (type)
  (flet ((print-char (char)
           (format *test-output-stream* "~c" char)))
    (when *report-progress*
      (case type
        (:error (print-char #\E))
        (:fail  (print-char #\F))
        (:pass  (print-char #\.))))))
