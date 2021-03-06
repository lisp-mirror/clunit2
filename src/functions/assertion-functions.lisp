(in-package :clunit)

(defun signal-assertion (type &key expression expected returned forms message (format-string "") args)
  (case type
    (:pass
     (signal 'assertion-passed))
    (:error
     (error 'assertion-error :message message))
    (:fail-forced
     (error 'assertion-fail-forced :format-string format-string :args args))
    (:fail
     (error 'assertion-failed
            :expression expression
            :expected   expected
            :returned   returned
            :forms forms))))

(defun handle-error (error)
  ;; We record the error as an ASSERTION-ERROR condition.
  (handle-assertion (make-condition 'assertion-error :message (princ-to-string error))))

(defun handle-assertion (condition)
  "Records the result of assertion tests and records any errors that occur."
  (let ((restart (or (find-restart 'skip-assertion)
                     (find-restart 'skip-test))))
    (with-slots (passed failed errors) *clunit-report*
      (with-slots (passed-p assertion-conditions) *clunit-test-report*
        (vector-push-extend condition
                            assertion-conditions
                            +assertion-conditions-reserved-size+)
        (typecase condition
          (assertion-error
           (when passed-p
             (setf passed-p nil))
           (incf errors)
           (report-assertion-progress :error))
          (assertion-passed
           (incf passed)
           (report-assertion-progress :pass)
           (invoke-restart restart))  ; we do not  invoke the debugger
                                      ; for successful assertions.
          ((or assertion-failed assertion-fail-forced)
           (when passed-p
             (setf passed-p nil))
           (incf failed)
           (report-assertion-progress :fail)))))
    (when *stop-on-fail*
      (invoke-restart 'cancel-unit-test))
    (when (and restart
               (not *use-debugger*))
      (invoke-restart restart))))
