;;; overleaf-snps.el --- Overleaf browser, status, and SSO fix -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal additions to vale981/overleaf.el.  Keep the upstream checkout clean.
;; M-x overleaf lists projects; RET selects a project and prompts for its file.
;; Overleaf editing buffers show their buffer-local settings in the header line.
;; The browser-login override is based on upstream commit 51e061f; it waits for
;; the project page instead of treating an intermediate SSO redirect as success.

;;; Code:

(require 'overleaf)
(require 'cl-lib)
(require 'dom)
(require 'json)
(require 'tabulated-list)
(require 'url-parse)
(require 'url-util)

;;;; Browser login

(defun overleaf-snps--login-complete-p (session url)
  "Return non-nil when SESSION reaches the project page on Overleaf URL.
SSO redirects to other hosts or intermediate authentication pages must
not be mistaken for a completed login."
  (let ((current (url-generic-parse-url (webdriver-get-current-url session)))
        (target (url-generic-parse-url url)))
    (and (equal (url-type current) (url-type target))
         (equal (url-host current) (url-host target))
         (equal (url-port current) (url-port target))
         (string-match-p "\\`/project\\(?:/\\|[?#]\\|\\'\\)"
                         (url-filename current)))))

(defun overleaf-snps--login-via-browser (url)
  "Log into overleaf URL by driving a real browser via WebDriver.
This is `overleaf-login's fallback for sites that need something a
plain HTTP request can't do, such as solving a CAPTCHA, SSO, or
two-factor authentication: you complete the login yourself in the
browser window that pops up, and this obtains the resulting cookies.

After running this command, wait for the browser window to pop up and
for the login page to load.  Note that if the cookies are still valid,
the login page may not be shown and this command terminates without
user input.  Gives up (closing the browser) after 5 minutes if you
never navigate away from the login page, e.g. because you didn't
finish logging in.

Drives Firefox (via `geckodriver') or Chrome/Chromium (via
`chromedriver'), per `overleaf-webdriver-browser'; the corresponding
driver must be installed and on `exec-path'."
  (interactive
   (list
    (read-string "Overleaf URL: " (overleaf--url))))

  (overleaf--with-webdriver
   (unless (and (boundp 'overleaf-cookies)
                (boundp 'overleaf-save-cookies) overleaf-cookies overleaf-save-cookies)
     (user-error "Both overleaf-cookies and overleaf-save-cookies need to be set"))

   (setq-local overleaf-url url)
   (let ((session (make-instance 'webdriver-session
                                 :service (overleaf--webdriver-make-service)
                                 :requested-capabilities (overleaf--webdriver-make-capabilities))))
     (unwind-protect
         (let ((full-cookies (overleaf--get-full-cookies)))
           (webdriver-session-start session)
           (webdriver-goto-url session (concat (overleaf--url) "/login"))
           (overleaf--message "Log in to overleaf in the browser window that just opened...")

           ;; SSO leaves /login before authentication completes.  Wait
           ;; for the project page on the original Overleaf origin.
           (overleaf--webdriver-wait-until 300
             (overleaf-snps--login-complete-p session url))

           ;; The session cookie is set as soon as login completes --
           ;; no need to navigate into a project (which also assumed at
           ;; least one existed).
           (let ((cookies (webdriver-get-all-cookies session)))
             (setf (alist-get (car (overleaf--cookie-domains)) full-cookies nil nil #'string=)
                   (list
                    (substring (apply #'concat
                                      (mapcar (lambda (cookie)
                                                (format "%s=%s; " (alist-get 'name cookie) (alist-get 'value cookie)))
                                              cookies))
                               0 -2)
                    (alist-get 'expiry (aref cookies 0))))
             (funcall overleaf-save-cookies
                      (prin1-to-string full-cookies))
             (overleaf--message "Logged in, cookies saved.")))
       ;; `webdriver-session-stop' only force-kills the geckodriver
       ;; process (which takes the browser window down with it) *after*
       ;; its own graceful "end session" request succeeds; if that
       ;; request fails for any reason the browser is left orphaned
       ;; with nothing left to clean it up.  Killing the process
       ;; directly is idempotent, so it's safe to also do unconditionally
       ;; here regardless of how the graceful shutdown went.
       (ignore-errors (webdriver-session-stop session))
       (ignore-errors (webdriver-service-stop (oref session service)))))))


(advice-add 'overleaf-login-via-browser :override
            #'overleaf-snps--login-via-browser)

;;;; Project browser

(defun overleaf-snps--parse-projects (html)
  "Read the prefetched project list from Overleaf HTML.
Report an expired login or changed page instead of silently showing no projects."
  (unless (libxml-available-p)
    (user-error "This Emacs needs libxml support to read the project page"))
  (let* ((dom (with-temp-buffer
                (insert html)
                (libxml-parse-html-region (point-min) (point-max))))
         (meta (cl-find-if
                (lambda (node)
                  (equal (dom-attr node 'name) "ol-prefetchedProjectsBlob"))
                (dom-by-tag dom 'meta)))
         (blob (and meta (dom-attr meta 'content))))
    (unless blob
      (user-error "Cannot read projects; try M-x overleaf-login-via-browser"))
    (let ((data (json-parse-string (url-unhex-string blob)
                                 :object-type 'plist :array-type 'list)))
      (unless (plist-member data :projects)
        (user-error "Overleaf returned an unexpected project list"))
      (plist-get data :projects))))

(defun overleaf-snps--project-entries ()
  "Fetch project rows for the current buffer's Overleaf server."
  (let* ((overleaf--current-cookies nil)
         (page (plz 'get (concat (overleaf--url) "/project")
                 :headers `(("Cookie" . ,(overleaf--get-cookies))
                            ("Origin" . ,(overleaf--url))))))
    (mapcar (lambda (project)
              (list (plist-get project :id)
                    (vector (or (plist-get project :name) "(unnamed)")
                            (or (plist-get (plist-get project :owner) :email)
                                ""))))
            (overleaf-snps--parse-projects page))))

(defun overleaf-snps-refresh ()
  "Refresh the Overleaf project list, preserving the selected row."
  (interactive)
  (setq tabulated-list-entries (overleaf-snps--project-entries))
  (tabulated-list-print t)
  (unless (tabulated-list-get-id)
    (goto-char (point-min))
    (while (and (not (eobp)) (not (tabulated-list-get-id)))
      (forward-line 1)))
  (setq-local header-line-format
              (format " Overleaf | %d projects | RET: select | g: refresh | q: quit"
                      (length tabulated-list-entries))))

(defun overleaf-snps-select-project ()
  "Open the project at point in a separate buffer and choose a file."
  (interactive)
  (let ((id (tabulated-list-get-id))
        (entry (tabulated-list-get-entry))
        (url (overleaf--url)))
    (unless id
      (user-error "Move point to a project first"))
    (let ((buffer (generate-new-buffer (format "*overleaf: %s*" (aref entry 0)))))
      (pop-to-buffer buffer)
      (latex-mode)
      (setq-local overleaf-url url
                  overleaf-project-id id
                  overleaf-document-id nil)
      (overleaf-connect))))

(defvar overleaf-snps-projects-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'overleaf-snps-select-project)
    (define-key map (kbd "g") #'overleaf-snps-refresh)
    map)
  "Keymap for the Overleaf project browser.")

(define-derived-mode overleaf-snps-projects-mode tabulated-list-mode "Overleaf projects"
  "Browse Overleaf projects.  RET selects a project; g refreshes the list."
  (setq tabulated-list-format [("Project" 48 t) ("Owner" 35 t)]
        tabulated-list-padding 2
        tabulated-list-sort-key '("Project" . nil)
        tabulated-list-use-header-line nil)
  (setq-local revert-buffer-function
              (lambda (&rest _) (overleaf-snps-refresh)))
  (tabulated-list-init-header))

;;;###autoload
(defun overleaf (&optional url)
  "List all projects in *overleaf* for URL, defaulting to the current server.
With a prefix argument, prompt for the server.  RET selects a project and
opens a separate buffer where Overleaf prompts for a file."
  (interactive (list (if current-prefix-arg
                         (read-string "Overleaf URL: " (overleaf--url))
                       (overleaf--url))))
  (let ((url (or url (overleaf--url)))
        (buffer (get-buffer-create "*overleaf*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'overleaf-snps-projects-mode)
        (overleaf-snps-projects-mode))
      (setq-local overleaf-url url)
      (overleaf-snps-refresh))
    (pop-to-buffer buffer)))

;;;; Buffer-local status, using header-line-format as codex-ide does

(defface overleaf-snps-header-face
  '((t :inherit header-line :weight normal :height 1.0))
  "Face for the Overleaf status header."
  :group 'overleaf)

(defvar-local overleaf-snps--previous-header nil
  "Previous header state as (WAS-LOCAL . VALUE).")

(defconst overleaf-snps--header-format
  '((:eval (overleaf-snps--status-header)))
  "Dynamic header format for Overleaf editing buffers.")

(defun overleaf-snps--status-header ()
  "Show the current buffer's track-changes and auto-save settings."
  (propertize
   (format " Overleaf | Track changes: %s | Auto-save: %s"
           (if overleaf-track-changes "ON" "OFF")
           (if overleaf-auto-save "ON" "OFF"))
   'face 'overleaf-snps-header-face))

(defun overleaf-snps--update-header ()
  "Install the status header, or restore the old header when mode is disabled."
  (if overleaf-mode
      (progn
        (unless overleaf-snps--previous-header
          (setq overleaf-snps--previous-header
                (cons (local-variable-p 'header-line-format) header-line-format)))
        (setq-local header-line-format overleaf-snps--header-format))
    (when overleaf-snps--previous-header
      (when (equal header-line-format overleaf-snps--header-format)
        (if (car overleaf-snps--previous-header)
            (setq-local header-line-format (cdr overleaf-snps--previous-header))
          (kill-local-variable 'header-line-format)))
      (setq overleaf-snps--previous-header nil)))
  (force-mode-line-update))

(defun overleaf-snps--refresh-header (&rest _)
  "Redisplay the header after a setting is toggled."
  (force-mode-line-update))

(add-hook 'overleaf-mode-hook #'overleaf-snps--update-header)
(advice-add 'overleaf-toggle-track-changes :after #'overleaf-snps--refresh-header)
(advice-add 'overleaf-toggle-auto-save :after #'overleaf-snps--refresh-header)

;; Also update buffers that were connected before this file was loaded.
(dolist (buffer (buffer-list))
  (with-current-buffer buffer
    (when (bound-and-true-p overleaf-mode)
      (overleaf-snps--update-header))))

(provide 'overleaf-snps)
;;; overleaf-snps.el ends here
