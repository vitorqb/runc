;;; runc-test.el --- Tests for runc

;; A mock for a runner
(defclass runc-test-runner (runc-i-runner) ((runnable) (args)))
(cl-defmethod runc--runner-run ((runner runc-test-runner) runnable args)
  (oset runner runnable runnable)
  (oset runner args args))

(ert-deftest tunc--initialize-process-buffer/writes-header ()
  (let ((runnable (make-runc-runnable :program "echo"
                                      :default-args '("foo" "bar")
                                      :directory "/foo/bar"))
        (datetime `(,24473 ,62901 ,144463 ,590000)))
    (with-temp-buffer
      (cl-letf (((symbol-function 'get-buffer-create) (lambda (_) (current-buffer)))
                ((symbol-function 'current-time)      (lambda () datetime)))
        (runc--initialize-process-buffer runnable)
        (should
         (equal
          (buffer-string)
          (s-join "\n"
           '("------------------------------------------------------------"
             "Start: Wed Oct 28 23:50:29 CET 2020"
             "Program: \"echo\""
             "Args: (\"foo\" \"bar\")"
             "Directory: \"/foo/bar\""
             "------------------------------------------------------------"
             ""))))))))

(ert-deftest runc--compile-command/simple-command ()
  (let* ((runnable (make-runc-runnable :program "whoami"))
         (result (runc--compile-command runnable)))
    (should (equal result "whoami"))))

(ert-deftest runc--compile-command/with-default-arguments ()
  (let* ((runnable (make-runc-runnable :program "echo" :default-args '("foo bar" "!@#")))
         (result (runc--compile-command runnable)))
    (should (equal result "echo foo\\ bar \\!\\@\\#"))))

(ert-deftest runc--compile-command/with-args ()
  (let* ((runnable (make-runc-runnable :program "echo" :default-args '("foo bar" "!@#")))
         (result (runc--compile-command runnable '("bab bobo" "bu!"))))
    (should (equal result "echo bab\\ bobo bu\\!"))))

(ert-deftest runc-run-test/calls-runner-with-runnable ()
  (let* ((runner (runc-test-runner))
         (runnable (make-runc-runnable))
         (args '("foo")))
    (runc-run runnable args runner)
    (should (equal runnable (oref runner runnable)))
    (should (equal args (oref runner args)))))

;;; runc-test.el ends here
