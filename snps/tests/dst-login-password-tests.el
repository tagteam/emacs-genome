;;; dst-login-password-tests.el --- DST credential tests -*- lexical-binding: t; -*-

;; Run with: emacs -Q --batch -l snps/tests/dst-login-password-tests.el \
;;             -f ert-run-tests-batch-and-exit

(require 'ert)
(load (expand-file-name "../dst-login-snps.el"
                        (file-name-directory (or load-file-name buffer-file-name))) nil t)

(defmacro dst-test-with-authinfo (contents &rest body)
  (declare (indent 1))
  `(let ((dst-login-authinfo-file (make-temp-file "dst-authinfo-test-"))
         (dst-ident "abc")
         (dst-firewall-code "test-pin")
         (dst-login-password-cache (make-hash-table :test #'equal))
         (dst-login--password-attempts (make-hash-table :test #'equal))
         (dst-login-project-status (make-hash-table :test #'equal))
         (dst-login-list '(("First" "123" "Windows") ("Second" "456" "srv")))
         (dst-login-save-changed-passwords t)
         (auth-source-netrc-cache nil))
     (unwind-protect
         (progn
           (with-temp-file dst-login-authinfo-file (insert ,contents))
           ,@body)
       (let ((buffer (get-file-buffer dst-login-authinfo-file)))
         (when buffer
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer)))
       (delete-file dst-login-authinfo-file))))

(ert-deftest dst-password-save-preserves-other-entries-and-comments ()
  (let ((other "machine mail.example login mail password keep-me\n")
        (comment "# explanatory comment\n"))
    (dst-test-with-authinfo
        (concat comment other
                "machine dstfse.local\n port 3389 login abc123@dstfse.local password \"old password\" # inline\n"
                "machine dstfse.local port 3389 login abc456@dstfse.local password second\n")
      (should (equal (dst-login--authinfo-password (car dst-login-list)) "old password"))
      (dst-login--save-project-password (car dst-login-list) "new with \\ slash")
      (should (equal (dst-login--authinfo-password (car dst-login-list)) "new with \\ slash"))
      (should (equal (dst-login--authinfo-password (cadr dst-login-list)) "second"))
      (with-temp-buffer
        (insert-file-contents dst-login-authinfo-file)
        (should (string-prefix-p (concat comment other) (buffer-string)))
        (should (string-match-p "# inline" (buffer-string))))
      (should (= (file-modes dst-login-authinfo-file) #o600)))))

(ert-deftest dst-password-save-adds-missing-and-updates-duplicates ()
  (dst-test-with-authinfo
      (concat "machine dstfse.local port 3389 login abc123@dstfse.local password old\n"
              "machine dstfse.local port 3389 login abc123@dstfse.local password older\n")
    (dst-login--save-project-password (car dst-login-list) "new")
    (dst-login--save-project-password (cadr dst-login-list) "second-new")
    (should (equal (dst-login--authinfo-password (cadr dst-login-list)) "second-new"))
    (with-temp-buffer
      (insert-file-contents dst-login-authinfo-file)
      (should-not (string-match-p "old" (buffer-string)))
      (should (= (how-many "password \"new\"" (point-min) (point-max)) 2)))))

(ert-deftest dst-password-authinfo-quote-roundtrip ()
  (dst-test-with-authinfo ""
    (dolist (password '("one\" two" "one' two" "foo\"'bar" "#hash" "back\\slash"))
      (dst-login--save-project-password (car dst-login-list) password)
      (should (equal (dst-login--authinfo-password (car dst-login-list)) password))))
  (should-error (dst-login--authinfo-token "both'\" quotes") :type 'user-error)
  (should-error (dst-login--authinfo-token "line\nbreak") :type 'user-error))

(ert-deftest dst-password-save-refuses-unsaved-authinfo ()
  (dst-test-with-authinfo "# existing\n"
    (with-current-buffer (find-file-noselect dst-login-authinfo-file)
      (goto-char (point-max))
      (insert "# unsaved\n"))
    (should-error (dst-login--save-project-password (car dst-login-list) "new") :type 'user-error)
    (with-temp-buffer
      (insert-file-contents dst-login-authinfo-file)
      (should (equal (buffer-string) "# existing\n")))))

(ert-deftest dst-password-reset-prompts-for-current-and-new ()
  (dst-test-with-authinfo ""
    (let (prompts submitted)
      (cl-letf (((symbol-function 'read-passwd)
                 (lambda (prompt &optional confirm &rest _)
                   (push (cons prompt confirm) prompts)
                   (if confirm "new" "temporary")))
                ((symbol-function 'dst-login--run-password-changes)
                 (lambda (changes new &rest _) (setq submitted (list changes new)))))
        (dst-change-password (car dst-login-list)))
      (should (= (length prompts) 2))
      (should (equal submitted (list (list (list (car dst-login-list) "temporary")) "new"))))))

(ert-deftest dst-password-all-reads-authinfo-not-cache-or-server ()
  (dst-test-with-authinfo
      (concat "machine dstfse.local port 3389 login abc123@dstfse.local password first\n"
              "machine dstfse.local port 3389 login abc456@dstfse.local password second\n")
    (puthash "abc123@dstfse.local" "stale" dst-login-password-cache)
    (setq dst-login-list (append dst-login-list (list (car dst-login-list))))
    (let (prompts submitted)
      (cl-letf (((symbol-function 'read-passwd)
                 (lambda (prompt &optional confirm &rest _)
                   (should confirm)
                   (push prompt prompts) "new"))
                ((symbol-function 'dst-login--run-password-changes)
                 (lambda (changes new &rest _) (setq submitted (list changes new)))))
        (dst-change-all-passwords))
      (should (= (length prompts) 1))
      (should (equal (mapcar #'cadr (car submitted)) '("first" "second")))
      (should (equal (cadr submitted) "new")))))

(ert-deftest dst-password-missing-entry-aborts-before-prompt-or-submission ()
  (dst-test-with-authinfo
      "machine dstfse.local port 3389 login abc123@dstfse.local password first\n"
    (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) (ert-fail "Unexpected prompt")))
              ((symbol-function 'dst-login--run-password-changes)
               (lambda (&rest _) (ert-fail "Unexpected submission"))))
      (should-error (dst-change-all-passwords) :type 'user-error))))

(ert-deftest dst-password-skips-already-current-password ()
  (dst-test-with-authinfo
      (concat "machine dstfse.local port 3389 login abc123@dstfse.local password new\n"
              "machine dstfse.local port 3389 login abc456@dstfse.local password old\n")
    (let (submitted)
      (cl-letf (((symbol-function 'dst-login--run-password-changes)
                 (lambda (changes &rest _) (setq submitted changes))))
        (dst-change-all-passwords "new"))
      (should (equal submitted (list (list (cadr dst-login-list) "old")))))))

(defmacro dst-test-with-form (&rest body)
  (declare (indent 0))
  `(dst-test-with-authinfo "machine dstfse.local port 3389 login abc123@dstfse.local password old\n"
     (let ((dst-login--webdriver-session "fake")
           (dst-login--authenticated t)
           (dst-login--download-timer nil)
           (dst-login-webdriver-timeout 0.01)
           (page-text "Your password has been successfully changed")
           filled requests)
       (cl-letf (((symbol-function 'dst-login--open-password-form) (lambda () '("main" "form" t)))
                 ((symbol-function 'dst-login--find-element) (lambda (_ id &rest _) id))
                 ((symbol-function 'dst-login--send-keys)
                  (lambda (id value) (push (cons id value) filled)))
                 ((symbol-function 'dst-login--webdriver-request)
                  (lambda (method endpoint &optional data) (push (list method endpoint data) requests)))
                 ((symbol-function 'dst-login--password-page-text) (lambda () page-text))
                 ((symbol-function 'display-buffer) #'ignore))
         ,@body))))

(ert-deftest dst-password-success-updates-cache-and-file ()
  (dst-test-with-form
    (puthash (dst-login--password-attempt-key (car dst-login-list) "new")
             '(2 . 0) dst-login--password-attempts)
    (dst-login--change-one-password (car dst-login-list) "old" "new")
    (should (dst-login--check-password-attempt (car dst-login-list) "new"))
    (should (equal (cdr (assoc "DomainUserName" filled)) "abc123@dstfse.local"))
    (should (equal (cdr (assoc "UserPass" filled)) "old"))
    (should (equal (cdr (assoc "NewUserPass" filled)) "new"))
    (should (equal (cdr (assoc "ConfirmNewUserPass" filled)) "new"))
    (should (equal (dst-login--authinfo-password (car dst-login-list)) "new"))
    (should (equal (gethash "abc123@dstfse.local" dst-login-password-cache) "new"))
    (should (equal (gethash "123" dst-login-project-status) "Password changed"))
    (should (member '("DELETE" "/session/fake/window" nil) requests))))

(ert-deftest dst-password-unconfirmed-keeps-file-and-stops-batch ()
  (dst-test-with-form
    (setq page-text "Rejected: old and new do not meet requirements")
    (puthash "abc123@dstfse.local" "old" dst-login-password-cache)
    (should-error
     (dst-login--run-password-changes
      (list (list (car dst-login-list) "old") (list (cadr dst-login-list) "old")) "new")
     :type 'user-error)
    (should (equal (dst-login--authinfo-password (car dst-login-list)) "old"))
    (should-not (gethash "abc123@dstfse.local" dst-login-password-cache))
    (should-not (gethash "456" dst-login-project-status))
    (should-not (member '("DELETE" "/session/fake/window" nil) requests))
    (with-current-buffer "*dst-password-changes*"
      (should (string-match-p "Rejected: \\[REDACTED\\] and \\[REDACTED\\]" (buffer-string))))))

(ert-deftest dst-password-save-failure-keeps-confirmed-new-cache ()
  (dst-test-with-form
    (cl-letf (((symbol-function 'dst-login--save-project-password)
               (lambda (&rest _) (error "Disk full"))))
      (should-error (dst-login--change-one-password (car dst-login-list) "old" "new")
                    :type 'user-error))
    (should (equal (gethash "abc123@dstfse.local" dst-login-password-cache) "new"))
    (should (equal (gethash "123" dst-login-project-status) "Changed; save failed"))))

(ert-deftest dst-password-form-new-same-or-reused-tab ()
  (dolist (mode '(new same reuse))
    (let ((dst-login--webdriver-session "fake")
          (dst-login-webdriver-timeout 0.01)
          (clicked nil)
          (window "main"))
      (cl-letf (((symbol-function 'dst-login--find-element) (lambda (&rest _) "button"))
                ((symbol-function 'dst-login--element-present-p)
                 (lambda (_ id)
                   (if (equal id "/Common/dst-FSE-password")
                       (and (equal window "main") (not clicked))
                     (and clicked (equal window (if (eq mode 'same) "main" "form"))))))
                ((symbol-function 'dst-login--webdriver-request)
                 (lambda (_ endpoint &optional data)
                   (cond
                    ((string-suffix-p "/window/handles" endpoint)
                     (if (or (eq mode 'reuse) (and clicked (eq mode 'new)))
                         '("main" "form") '("main")))
                    ((string-suffix-p "/window" endpoint) (setq window (alist-get 'handle data)))
                    ((string-suffix-p "/click" endpoint) (setq clicked t))))))
        (should (equal (dst-login--open-password-form)
                       (list "main" (if (eq mode 'same) "main" "form") (eq mode 'new))))))))

(defmacro dst-test-with-rdp-exit (code &rest body)
  "Run real child processes with fake RDP exit CODE and dummy credentials."
  (declare (indent 1))
  `(dst-test-with-authinfo
       "machine dstfse.local port 3389 login abc123@dstfse.local password old\n"
     (let ((original-make-process (symbol-function 'make-process))
           (launches 0))
       (unwind-protect
           (cl-letf (((symbol-function 'make-process)
                      (lambda (&rest args)
                        (cl-incf launches)
                        (apply original-make-process
                               (plist-put args :command
                                          (list "sh" "-c" (format "exit %d" ,code))))))
                     ((symbol-function 'display-buffer) #'ignore))
             ,@body)
         (when (get-buffer "*dst-rdp-123*")
           (kill-buffer "*dst-rdp-123*"))))))

(defun dst-test-run-rdp ()
  "Start a dummy connection and wait for its sentinel, bounded to 3 seconds."
  (dst-login--start-xfreerdp (car dst-login-list) "/tmp/dummy.rdp")
  (let ((deadline (+ (float-time) 3)))
    (while (and (equal (gethash "123" dst-login-project-status) "Running")
                (< (float-time) deadline))
      (accept-process-output nil 0.01)))
  (should-not (equal (gethash "123" dst-login-project-status) "Running")))

(ert-deftest dst-password-guard-blocks-third-rdp-attempt ()
  (dolist (code '(132 134 154))
    (dst-test-with-rdp-exit code
      (dst-test-run-rdp)
      (dst-test-run-rdp)
      (dst-login-forget-passwords)
      (should-error (dst-test-run-rdp) :type 'user-error)
      (should (= launches 2))
      (should (equal (gethash (dst-login--password-attempt-key (car dst-login-list) "old")
                             dst-login--password-attempts) '(2 . 0)))
      ;; Other projects and a corrected password remain usable.
      (should (dst-login--check-password-attempt (cadr dst-login-list) "old"))
      (dst-login--save-project-password (car dst-login-list) "corrected")
      (dst-login-forget-passwords)
      (dst-test-run-rdp)
      (should (= launches 3)))))

(ert-deftest dst-password-guard-does-not-count-network-failures ()
  (dst-test-with-rdp-exit 147
    (dotimes (_ 3) (dst-test-run-rdp))
    (should (= launches 3))
    (should (equal (gethash (dst-login--password-attempt-key (car dst-login-list) "old")
                           dst-login--password-attempts) '(0 . 0)))))

(ert-deftest dst-password-guard-blocks-lockout-immediately ()
  (dst-test-with-rdp-exit 135
    (dst-test-run-rdp)
    (should-error (dst-test-run-rdp) :type 'user-error)
    (should (= launches 1))))

(ert-deftest dst-password-guard-reserves-concurrent-attempts ()
  (dst-test-with-authinfo ""
    (let* ((project (car dst-login-list))
           (key (dst-login--begin-password-attempt project "old")))
      (dst-login--begin-password-attempt project "old")
      (should-error (dst-login--begin-password-attempt project "old") :type 'user-error)
      (dst-login--finish-password-attempt key 'rejected)
      (should-error (dst-login--begin-password-attempt project "old") :type 'user-error)
      (dst-login--finish-password-attempt key)
      (should (dst-login--begin-password-attempt project "old")))))

(ert-deftest dst-password-guard-releases-failed-process-start ()
  (dst-test-with-authinfo
      "machine dstfse.local port 3389 login abc123@dstfse.local password old\n"
    (cl-letf (((symbol-function 'make-process) (lambda (&rest _) (error "No executable")))
              ((symbol-function 'display-buffer) #'ignore))
      (dotimes (_ 3)
        (should-error (dst-login--start-xfreerdp (car dst-login-list) "/tmp/dummy.rdp"))))
    (should (equal (gethash (dst-login--password-attempt-key (car dst-login-list) "old")
                           dst-login--password-attempts) '(0 . 0)))))

(ert-deftest dst-password-guard-shared-by-form-and-rdp ()
  (dst-test-with-form
    (setq page-text "The current password is incorrect")
    (dotimes (_ 2)
      (should-error (dst-login--change-one-password (car dst-login-list) "old" "new")
                    :type 'user-error))
    (let ((request-count (length requests)))
      (should-error (dst-login--change-one-password (car dst-login-list) "old" "new")
                    :type 'user-error)
      (should (= request-count (length requests))))
    (should-error (dst-login--start-xfreerdp (car dst-login-list) "/tmp/dummy.rdp")
                  :type 'user-error)))

(ert-deftest dst-password-guard-checks-batch-before-any-submission ()
  (dst-test-with-form
    (let ((key (dst-login--begin-password-attempt (cadr dst-login-list) "bad")))
      (dst-login--finish-password-attempt key 'locked))
    (should-error
     (dst-login--run-password-changes
      (list (list (car dst-login-list) "old") (list (cadr dst-login-list) "bad")) "new")
     :type 'user-error)
    (should-not requests)))

(ert-deftest dst-password-guard-does-not-count-unsubmitted-form-failures ()
  (dst-test-with-form
    (cl-letf (((symbol-function 'dst-login--open-password-form)
               (lambda () (error "Browser unavailable"))))
      (dotimes (_ 3)
        (should-error (dst-login--change-one-password (car dst-login-list) "old" "new")
                      :type 'user-error)))
    (should (= (hash-table-count dst-login--password-attempts) 0))))

;;; dst-login-password-tests.el ends here
