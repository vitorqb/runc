;;; runc-test.el --- Tests for runc

;; A mock for a runner
(defclass runc-test-runner (runc-i-runner) ((runnable)))
(cl-defmethod runc--runner-run ((runner runc-test-runner) runnable)
  (oset runner runnable runnable))

(ert-deftest runc--compile-command/simple-command ()
  (let* ((runnable (make-runc-runnable :program "whoami"))
         (result (runc--compile-command runnable)))
    (should (equal result "whoami"))))

(ert-deftest runc--compile-command/with-default-arguments ()
  (let* ((runnable (make-runc-runnable :program "echo" :default-args '("foo bar" "!@#")))
         (result (runc--compile-command runnable)))
    (should (equal result "echo foo\\ bar \\!\\@\\#"))))

(ert-deftest runc-run-test/calls-runner-with-runnable ()
  (let* ((runner (runc-test-runner))
         (runnable (make-runc-runnable)))
    (runc-run runnable runner)
    (should (equal runnable (oref runner runnable)))))

;;; runc-test.el ends here
