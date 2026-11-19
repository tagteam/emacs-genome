;;; overleaf-snps-tests.el --- Local Overleaf regressions -*- lexical-binding: t; -*-

;; Run with the installed straight packages, without loading the user's init:
;; emacs -Q --batch -l snps/tests/overleaf-snps-tests.el -f ert-run-tests-batch-and-exit
(let* ((snps (expand-file-name ".." (file-name-directory load-file-name)))
       (build (expand-file-name "../straight/build" snps)))
  (add-to-list 'load-path snps)
  (dolist (dir (directory-files build t "^[^.]"))
    (when (file-directory-p dir) (add-to-list 'load-path dir))))
(require 'ert)
(require 'overleaf-snps)

(ert-deftest overleaf-snps-sso-url-boundaries ()
  (dolist (case '(("https://www.overleaf.com/login" . nil)
                  ("https://www.overleaf.com/sso" . nil)
                  ("https://idp.example.org/authenticate" . nil)
                  ("https://idp.example.org/project" . nil)
                  ("https://www.overleaf.com/project-fake" . nil)
                  ("http://www.overleaf.com/project" . nil)
                  ("https://www.overleaf.com:8443/project" . nil)
                  ("https://www.overleaf.com/project" . t)
                  ("https://www.overleaf.com/project/123" . t)
                  ("https://www.overleaf.com/project?sort=title" . t)))
    (cl-letf (((symbol-function 'webdriver-get-current-url)
               (lambda (_) (car case))))
      (should (eq (not (null (overleaf-snps--login-complete-p
                             nil "https://www.overleaf.com")))
                  (cdr case))))))

(ert-deftest overleaf-snps-sso-waits-before-saving-and-closing ()
  (with-temp-buffer
    (let* ((urls '("https://www.overleaf.com/login"
                   "https://www.overleaf.com/sso"
                   "https://idp.example.org/authenticate"
                   "https://www.overleaf.com/project"))
           (polls 0) saved stopped
           (overleaf-webdriver-browser 'chrome)
           (overleaf-cookies #'ignore)
           (overleaf--current-cookies nil)
           (overleaf-save-cookies (lambda (value) (setq saved (read value)))))
      (cl-letf (((symbol-function 'executable-find) (lambda (_) "/test/driver"))
                ((symbol-function 'overleaf--webdriver-make-service)
                 (lambda () (make-instance 'webdriver-service-chrome)))
                ((symbol-function 'overleaf--webdriver-make-capabilities) #'ignore)
                ((symbol-function 'webdriver-session-start) #'ignore)
                ((symbol-function 'webdriver-goto-url) #'ignore)
                ((symbol-function 'webdriver-get-current-url)
                 (lambda (_) (cl-incf polls) (pop urls)))
                ((symbol-function 'webdriver-get-all-cookies)
                 (lambda (_)
                   (should (= polls 4))
                   [((name . "overleaf_session2") (value . "TEST") (expiry . 9999999999))]))
                ((symbol-function 'webdriver-session-stop)
                 (lambda (_) (should saved) (setq stopped t)))
                ((symbol-function 'webdriver-service-stop) #'ignore)
                ((symbol-function 'sleep-for) #'ignore)
                ((symbol-function 'overleaf--message) #'ignore))
        (overleaf-login-via-browser "https://www.overleaf.com")
        (should (= polls 4))
        (should (equal (cadar saved) "overleaf_session2=TEST"))
        (should stopped)))))

(ert-deftest overleaf-snps-parse-projects-and-expired-login ()
  (should (equal (overleaf-snps--parse-projects
                  "<html><head><meta content='{&quot;projects&quot;:[{&quot;id&quot;:&quot;a&quot;,&quot;name&quot;:&quot;A &amp; B&quot;}]}' name='ol-prefetchedProjectsBlob'></head></html>")
                 '((:id "a" :name "A & B"))))
  (should (equal (overleaf-snps--parse-projects
                  (format "<meta name=\"ol-prefetchedProjectsBlob\" content=\"%s\">"
                          (url-hexify-string "{\"projects\":[{\"id\":\"b\",\"name\":\"æøå\"}]}")))
                 '((:id "b" :name "æøå"))))
  (should-not (overleaf-snps--parse-projects
               "<meta name='ol-prefetchedProjectsBlob' content='{&quot;projects&quot;:[]}'>"))
  (should-error (overleaf-snps--parse-projects "<html>Log in</html>") :type 'user-error)
  (should-error (overleaf-snps--parse-projects
                 "<meta name='ol-prefetchedProjectsBlob' content='{}'>") :type 'user-error))

(ert-deftest overleaf-snps-project-browser-selection-keeps-list ()
  (let ((browser (generate-new-buffer " *overleaf-test-projects*"))
        document selected)
    (unwind-protect
        (save-window-excursion
          (with-current-buffer browser
            (overleaf-snps-projects-mode)
            (setq-local overleaf-url "https://test.overleaf.invalid")
            (cl-letf (((symbol-function 'overleaf-snps--project-entries)
                       (lambda () '(("first" ["Same name" "owner-a"])
                                    ("second" ["Same name" "owner-b"]))))
                      ((symbol-function 'overleaf-connect)
                       (lambda ()
                         (setq document (current-buffer)
                               selected (list overleaf-url overleaf-project-id
                                              overleaf-document-id)))))
              (overleaf-snps-refresh)
              (should (tabulated-list-get-id))
              (goto-char (point-min))
              (while (not (equal (tabulated-list-get-id) "second"))
                (when (eobp) (ert-fail "Missing second project"))
                (forward-line 1))
              (should (eq (key-binding (kbd "RET")) #'overleaf-snps-select-project))
              (let ((before (buffer-string)))
                (call-interactively #'overleaf-snps-select-project)
                (should (equal selected '("https://test.overleaf.invalid" "second" nil)))
                (should (equal before (with-current-buffer browser (buffer-string))))
                (should-not (eq document browser))))))
      (when (buffer-live-p document) (kill-buffer document))
      (kill-buffer browser))))

(ert-deftest overleaf-snps-no-project-at-point ()
  (with-temp-buffer
    (overleaf-snps-projects-mode)
    (should-error (overleaf-snps-select-project) :type 'user-error)))

(ert-deftest overleaf-snps-header-toggles-are-buffer-local ()
  (with-temp-buffer
    (setq-local header-line-format "Previous header")
    (overleaf-mode 1)
    (should (equal header-line-format overleaf-snps--header-format))
    (should (string-match-p "Track changes: OFF | Auto-save: OFF"
                            (overleaf-snps--status-header)))
    (overleaf-toggle-track-changes)
    (overleaf-toggle-auto-save)
    (should (string-match-p "Track changes: ON | Auto-save: ON"
                            (overleaf-snps--status-header)))
    (with-temp-buffer
      (should (string-match-p "Track changes: OFF | Auto-save: OFF"
                              (overleaf-snps--status-header))))
    ;; Re-enabling must not overwrite the saved original header.
    (overleaf-mode 1)
    (overleaf-mode -1)
    (should (equal header-line-format "Previous header"))))

(ert-deftest overleaf-snps-header-restores-inherited-value ()
  (with-temp-buffer
    (should-not (local-variable-p 'header-line-format))
    (overleaf-mode 1)
    (overleaf-mode -1)
    (should-not (local-variable-p 'header-line-format))))

;;; overleaf-snps-tests.el ends here
