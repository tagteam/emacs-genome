;;; dst-login-password-browser-tests.el --- Local browser regression -*- lexical-binding: t; -*-

;; Requires ChromeDriver at `dst-login-driver-path'.  All pages and credentials
;; are local test fixtures; this test never connects to DST.
;; emacs -Q --batch -l snps/tests/dst-login-password-browser-tests.el \
;;   -f ert-run-tests-batch-and-exit

(require 'ert)
(load (expand-file-name "../dst-login-snps.el"
                        (file-name-directory (or load-file-name buffer-file-name))) nil t)

(ert-deftest dst-password-current-and-legacy-browser-forms ()
  (let* ((directory (make-temp-file "dst-form-test-" t))
         (dst-login-authinfo-file (expand-file-name "authinfo" directory))
         (dst-login-download-directory directory)
         (dst-ident "test")
         (project '("Test" "123" "Windows"))
         (dst-login-password-cache (make-hash-table :test #'equal))
         (dst-login--password-attempts (make-hash-table :test #'equal))
         (dst-login-project-status (make-hash-table :test #'equal))
         (dst-login-save-changed-passwords t)
         (dst-login--webdriver-session nil)
         (dst-login--webdriver-process nil)
         (dst-login--webdriver-profile nil)
         (dst-login--webdriver-port nil)
         (dst-login--authenticated nil)
         (dst-login-webdriver-timeout 5)
         (dst-login-url (concat "file://" directory "/index.html")))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer) #'ignore))
          (with-temp-file (expand-file-name "index.html" directory)
            (insert "<!doctype html><a id='/Common/dst-FSE-password' href='form.html'>Change Password</a>"))
          (dst-login--create-session)
          (dolist (button-id '("submitBtn" "btnSignIn"))
            (with-temp-file (expand-file-name "form.html" directory)
              (insert "<!doctype html><html><body><form onsubmit='return submitForm()'>"
                      "<input id='DomainUserName'><input type='password' id='UserPass'>"
                      "<input type='password' id='NewUserPass'><input type='password' id='ConfirmNewUserPass'>"
                      "<button type='submit' disabled id='" button-id "'>Change Password</button>"
                      "</form><div id='result'></div><script>"
                      "const field=id=>document.getElementById(id);"
                      "function valid(){const p=field('NewUserPass').value;"
                      "return field('DomainUserName').value.length>5 && field('UserPass').value.length>10"
                      " && p.length>=12 && /[A-Z]/.test(p) && /[a-z]/.test(p) && /[0-9]/.test(p)"
                      " && p===field('ConfirmNewUserPass').value;}"
                      "document.addEventListener('input',()=>{field('" button-id "').disabled=!valid();});"
                      "function submitForm(){field('result').textContent=valid()"
                      " && field('DomainUserName').value==='test123@dstfse.local'"
                      " && field('UserPass').value==='Temporary123!'"
                      " ? 'Your password has been successfully changed' : 'Rejected'; return false;}"
                      "</script></body></html>"))
            (with-temp-file dst-login-authinfo-file
              (insert "machine dstfse.local port 3389 login test123@dstfse.local password Temporary123!\n"))
            (dst-login--webdriver-request "POST" (dst-login--session-endpoint "/url")
                                          `((url . ,dst-login-url)))
            (let ((attempts (hash-table-count dst-login--password-attempts)))
              (should-error (dst-login--change-one-password project "Temporary123!" "short")
                            :type 'user-error)
              (should (= attempts (hash-table-count dst-login--password-attempts))))
            (dst-login--webdriver-request "POST" (dst-login--session-endpoint "/url")
                                          `((url . ,dst-login-url)))
            (dst-login--change-one-password project "Temporary123!" "New Dummy Password1!")
            (should (equal (dst-login--authinfo-password project) "New Dummy Password1!"))
            (should (dst-login--element-present-p "id" "/Common/dst-FSE-password"))))
      (dst-login--stop-webdriver)
      (delete-directory directory t))))

;;; dst-login-password-browser-tests.el ends here
