;;; dst-login-snps.el --- Electric DST login dashboard -*- lexical-binding: t; -*-

;; Author: Thomas Alexander Gerds <tag@biostat.ku.dk>
;; Keywords: convenience

;;; Commentary:

;; This file provides an Emacs-owned DST workflow.  It speaks the W3C WebDriver
;; HTTP protocol directly to ChromeDriver; no Python helper is used.  Project
;; selection, password caching, browser control, download handling, and FreeRDP
;; launching are all implemented here.
;;
;; `dst-login-list' entries have exactly this shape:
;;
;;   ("Human-readable project description" "PROJECT-NUMBER" "SERVER")
;;
;; For example:
;;
;;   (setq dst-login-list
;;         '(("Cardiology collaborators" "709924" "Windows")
;;           ("Registry study" "9200" "srvfsegh4")))
;;
;; Project passwords are read from ~/.authinfo using `auth-source'.  For
;; example, if `dst-ident' is "abc", add one line per project:
;;
;;   machine dstfse.local port 3389 login abc709924@dstfse.local password "PROJECT-709924-PASSWORD"
;;   machine dstfse.local port 3389 login abc9200@dstfse.local password "PROJECT-9200-PASSWORD"
;;
;; Replace "abc" with your DST identifier and the passwords with your own.
;; Restrict access with chmod 600 ~/.authinfo.  Missing passwords are prompted
;; for, but are not written to the file.  Passwords are cached for this Emacs
;; session; press k in the dashboard after editing the file to reload them.
;; The firewall PIN is still configured separately in `dst-firewall-code'.
;;
;; In the dashboard, p changes/resets the project password (prompting for
;; current and new passwords), a changes all project passwords using authinfo,
;; and s changes a selected set.  The new password is confirmed twice.
;; Password changes use the existing firewall session, with 2FA if needed.
;; Confirmed changes update the session cache and matching authinfo entries.
;; Set `dst-login-save-changed-passwords' to nil to edit authinfo yourself.
;; Results and failures are recorded in *dst-password-changes*.
;; A password is blocked for its project after two rejected or unconfirmed
;; authentication attempts in this Emacs session.  Clearing the password
;; cache or reopening the dashboard does not reset this guard.  Correcting
;; the password allows a new attempt.  Network-only RDP failures do not count.
;; FreeRDP questions open an interactive *dst-rdp-PROJECT* buffer.  Review the
;; certificate details, type your answer (for example Y), and press RET.
;; The dashboard's l key also opens this buffer.  No answer is sent automatically.
;;
;; Load this file after dst-snps.el if both are used, so its `dst' and
;; `dst-change-password', `dst-change-password-loop', and
;; `dst-change-all-passwords' commands take precedence.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'comint)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-http)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defgroup dst-login nil
  "Electric dashboard for DST remote projects."
  :group 'convenience)

(defcustom dst-login-download-directory "~/tmp/"
  "Directory in which Chromium downloads DST launcher files."
  :type 'directory)

(defcustom dst-login-driver-path "~/bin/chromedriver"
  "Path to ChromeDriver."
  :type 'file)

(defcustom dst-login-webdriver-timeout 30
  "Seconds to wait for login-page elements controlled by ChromeDriver."
  :type 'integer)

(defcustom dst-login-webdriver-request-timeout 5
  "Seconds to wait for one ChromeDriver HTTP request."
  :type 'integer)

(defcustom dst-login-url "https://remote.dst.dk/"
  "DST remote login URL."
  :type 'string)

(defcustom dst-login-authinfo-file "~/.authinfo"
  "File containing DST project passwords in authinfo format.
Entries use machine dstfse.local, port 3389, and the full project login
as their login (for example, abc709924@dstfse.local).
An encrypted authinfo file such as ~/.authinfo.gpg can also be used."
  :type 'file)

(defcustom dst-login-save-changed-passwords t
  "Whether to update authinfo after DST confirms a password change."
  :type 'boolean)

(defcustom dst-login-download-timeout 60
  "Seconds to wait for a new RDP launcher after clicking a project."
  :type 'integer)

(defcustom dst-login-xfreerdp-program "xfreerdp"
  "FreeRDP executable."
  :type 'string)

(defcustom dst-login-xfreerdp-arguments '("/size:1900x1050" "/cert:tofu")
  "Arguments appended to each FreeRDP invocation."
  :type '(repeat string))

(defcustom dst-login-server-aliases '(("FSE Windows" . "Windows"))
  "Map human-readable server names to DST download-button suffixes."
  :type '(alist :key-type string :value-type string))

(defvar dst-ident nil
  "DST user identifier.")

(defvar dst-firewall-code nil
  "DST firewall PIN.")

(defvar dst-login-list nil
  "DST projects as (DESCRIPTION PROJECT-NUMBER SERVER) entries.")

(defvar dst-login-password-cache (make-hash-table :test #'equal)
  "Project passwords keyed by full login for this Emacs session.")

(defvar dst-login--password-attempts (make-hash-table :test #'equal)
  "Credential attempt counts for this Emacs session.
Keys are (FULL-LOGIN SHA256-OF-PASSWORD); values are (FAILURES . PENDING).
Pending attempts reserve slots so concurrent requests cannot bypass the limit.")

(defconst dst-login-buffer-name "*dst-login*")
(defconst dst-login--webdriver-buffer-name " *dst-login-chromedriver*")

(define-error 'dst-login-webdriver-error "ChromeDriver request failed")

(defvar dst-login--webdriver-process nil)
(defvar dst-login--webdriver-port nil)
(defvar dst-login--webdriver-session nil)
(defvar dst-login--webdriver-profile nil)
(defvar dst-login--webdriver-visible nil)
(defvar dst-login--authenticated nil)
(defvar dst-login--two-factor-pending nil)
(defvar dst-login--remote-status "Not connected")
(defvar dst-login-project-status (make-hash-table :test #'equal)
  "Current dashboard status for each DST project number.")
(defvar dst-login--pending-project nil)
(defvar dst-login--download-snapshot nil)
(defvar dst-login--download-deadline nil)
(defvar dst-login--download-timer nil)

(defvar-keymap dst-login-mode-map
  :parent special-mode-map
  "RET" #'dst-login-connect-at-point
  "0" #'dst-login-connect-number
  "1" #'dst-login-connect-number
  "2" #'dst-login-connect-number
  "3" #'dst-login-connect-number
  "4" #'dst-login-connect-number
  "5" #'dst-login-connect-number
  "6" #'dst-login-connect-number
  "7" #'dst-login-connect-number
  "8" #'dst-login-connect-number
  "9" #'dst-login-connect-number
  "v" #'dst-login-visible
  "c" #'dst-login-enter-two-factor
  "g" #'dst-login-refresh
  "l" #'dst-login-show-project-log
  "p" #'dst-change-password
  "a" #'dst-change-all-passwords
  "s" #'dst-change-password-loop
  "k" #'dst-login-forget-passwords
  "q" #'dst-login-quit)

(dotimes (offset 26)
  (define-key dst-login-mode-map
              (char-to-string (+ ?A offset))
              #'dst-login-connect-number))

(define-derived-mode dst-login-mode special-mode "DST-Login"
  "Major mode for the electric DST login dashboard."
  (setq-local truncate-lines t)
  (add-hook 'kill-buffer-hook #'dst-login--stop-webdriver nil t)
  (setq-local revert-buffer-function
              (lambda (&rest _ignore) (dst-login-refresh))))

(defun dst-login--validate-config ()
  (unless (and (stringp dst-ident) (not (string-empty-p dst-ident)))
    (user-error "Set `dst-ident' before running `dst'"))
  (unless (and (stringp dst-firewall-code)
               (not (string-empty-p dst-firewall-code)))
    (user-error "Set `dst-firewall-code' before running `dst'"))
  (unless (and (listp dst-login-list) dst-login-list)
    (user-error "Set `dst-login-list' before running `dst'"))
  (cl-loop for entry in dst-login-list
           for index from 1
           unless (and (= (length entry) 3)
                       (cl-every #'stringp entry))
           do (user-error
               "Entry %d in `dst-login-list' must be (DESCRIPTION PROJECT SERVER)"
               index)))

(defun dst-login--index-key (index)
  (cond
   ((< index 10) (char-to-string (+ ?0 index)))
   ((< index 36) (char-to-string (+ ?A (- index 10))))
   (t "")))

(defun dst-login--project-status (entry)
  (or (gethash (nth 1 entry) dst-login-project-status) "Idle"))

(defun dst-login--insert-project (entry index)
  (let* ((start (point))
         (key (dst-login--index-key index))
         (description
          (truncate-string-to-width
           (string-replace "\n" " " (nth 0 entry)) 25 nil nil "…"))
         (status
          (truncate-string-to-width
           (dst-login--project-status entry) 20 nil nil "…")))
    (insert (format " %-3s %-9s  %-12s  %-20s  %-25s\n"
                    key (nth 1 entry) (nth 2 entry) status description))
    (add-text-properties
     start (point)
     (list 'dst-login-project entry
           'dst-login-index index
           'mouse-face 'highlight
           'help-echo
           (if (string-empty-p key)
               "RET selects the project at point"
             (format "RET or %s selects this project" key))))))

(defun dst-login-refresh ()
  "Redraw the electric DST dashboard."
  (interactive)
  (let ((buffer (get-buffer-create dst-login-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'dst-login-mode)
        (dst-login-mode))
      (let ((inhibit-read-only t)
            (line (line-number-at-pos)))
        (erase-buffer)
        (insert (propertize "DST Login\n" 'face '(:height 1.4 :weight bold)))
        (insert (format "DST remote status: %s\n\n" dst-login--remote-status))
        (insert "RET selects the project at point; its displayed Key selects it directly\n")
        (insert "      v visible login, c login/enter 2FA, g refresh, l project log\n")
        (insert "      k forget passwords, q quit\n\n")
        (insert "      p change/reset password, a change all, s change selected\n\n")
        (insert (propertize " Key Project    Server        Status                  Description\n"
                            'face 'bold))
        (cl-loop for entry in dst-login-list
                 for index from 0
                 do (dst-login--insert-project entry index))
        (goto-char (point-min))
        (forward-line (max 0 (1- line)))))
    buffer))

(defun dst-login--set-remote-status (format-string &rest args)
  (setq dst-login--remote-status (apply #'format format-string args))
  (when (get-buffer dst-login-buffer-name)
    (dst-login-refresh)))

(defun dst-login--set-project-status (project format-string &rest args)
  (let ((number (if (listp project) (nth 1 project) project)))
    (puthash number (apply #'format format-string args)
             dst-login-project-status))
  (when (get-buffer dst-login-buffer-name)
    (dst-login-refresh)))

(defun dst-login--webdriver-live-p ()
  (and (processp dst-login--webdriver-process)
       (process-live-p dst-login--webdriver-process)))

(defun dst-login--allocate-port ()
  (let ((probe (make-network-process
                :name "dst-login-port-probe"
                :server t
                :host "127.0.0.1"
                :service t
                :noquery t)))
    (unwind-protect
        (process-contact probe :service)
      (delete-process probe))))

(defun dst-login--webdriver-url (endpoint)
  (format "http://127.0.0.1:%d%s" dst-login--webdriver-port endpoint))

(defun dst-login--webdriver-request (method endpoint &optional data)
  (let* ((url-request-method method)
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data
          (when data
            (encode-coding-string (json-encode data) 'utf-8)))
         (url-show-status nil)
         (url-proxy-services '(("no_proxy" . "127.0.0.1")))
         (buffer
          (url-retrieve-synchronously
           (dst-login--webdriver-url endpoint)
           t t dst-login-webdriver-request-timeout)))
    (unless buffer
      (signal 'dst-login-webdriver-error
              (list (format "No response to %s %s" method endpoint))))
    (unwind-protect
        (with-current-buffer buffer
          (let ((status url-http-response-status)
                response value webdriver-error)
            (goto-char url-http-end-of-headers)
            (unless (eobp)
              (let ((json-object-type 'alist)
                    (json-array-type 'list)
                    (json-key-type 'symbol)
                    (json-false :json-false)
                    (json-null :json-null))
                (setq response (json-read))))
            (setq value (alist-get 'value response)
                  webdriver-error (and (listp value)
                                       (alist-get 'error value)))
            (when (or (and status (>= status 400)) webdriver-error)
              (signal
               'dst-login-webdriver-error
               (list (or (and (listp value) (alist-get 'message value))
                         (format "HTTP %s from ChromeDriver" status))
                     value)))
            value))
      (kill-buffer buffer))))

(defun dst-login--wait-for-driver ()
  (let ((deadline (time-add (current-time) 10))
        ready)
    (while (and (not ready)
                (dst-login--webdriver-live-p)
                (time-less-p (current-time) deadline))
      (setq ready
            (condition-case nil
                (progn
                  (dst-login--webdriver-request "GET" "/status")
                  t)
              (error nil)))
      (unless ready
        (accept-process-output dst-login--webdriver-process 0.1)))
    (unless ready
      (error "ChromeDriver did not become ready; see %s"
             dst-login--webdriver-buffer-name))))

(defun dst-login--webdriver-sentinel (process event)
  (unless (process-live-p process)
    (setq dst-login--webdriver-session nil
          dst-login--authenticated nil)
    (when (get-buffer dst-login-buffer-name)
      (dst-login--set-remote-status
       "Disconnected — ChromeDriver %s" (string-trim event)))))

(defun dst-login--start-webdriver ()
  (unless (dst-login--webdriver-live-p)
    (unless (file-executable-p (expand-file-name dst-login-driver-path))
      (error "ChromeDriver is not executable: %s" dst-login-driver-path))
    (setq dst-login--webdriver-port (dst-login--allocate-port)
          dst-login--webdriver-profile
          (make-temp-file "dst-login-chrome-" t)
          dst-login--webdriver-session nil
          dst-login--webdriver-visible nil
          dst-login--authenticated nil)
    (let ((buffer (get-buffer-create dst-login--webdriver-buffer-name)))
      (with-current-buffer buffer
        (erase-buffer))
      (setq dst-login--webdriver-process
            (make-process
             :name "dst-login-chromedriver"
             :buffer buffer
             :command
             (list (expand-file-name dst-login-driver-path)
                   (format "--port=%d" dst-login--webdriver-port))
             :noquery t
             :sentinel #'dst-login--webdriver-sentinel)))
    (dst-login--wait-for-driver)))

(defun dst-login--delete-session ()
  (when dst-login--webdriver-session
    (ignore-errors
      (dst-login--webdriver-request
       "DELETE" (format "/session/%s" dst-login--webdriver-session)))
    (setq dst-login--webdriver-session nil)))

(defun dst-login--create-session (&optional visible)
  (dst-login--start-webdriver)
  (dst-login--delete-session)
  (let* ((download-directory
          (file-name-as-directory
           (expand-file-name dst-login-download-directory)))
         (arguments
          (append
           (unless visible '("--headless=new"))
           (list (concat "--user-data-dir=" dst-login--webdriver-profile)
                 "--no-first-run"
                 "--no-default-browser-check")))
         (preferences
          `(("download.default_directory" . ,download-directory)
            ("download.prompt_for_download" . :json-false)
            ("download.directory_upgrade" . t)
            ("safebrowsing.enabled" . t)))
         (chrome-options
          `(("args" . ,(vconcat arguments))
            ("prefs" . ,preferences)))
         (always-match
          `(("browserName" . "chrome")
            ("goog:chromeOptions" . ,chrome-options)))
         (payload
          `(("capabilities" . (("alwaysMatch" . ,always-match)))))
         (value (dst-login--webdriver-request "POST" "/session" payload)))
    (setq dst-login--webdriver-session
          (or (alist-get 'sessionId value)
              (alist-get 'sessionId (alist-get 'value value)))
          dst-login--webdriver-visible visible)
    (unless dst-login--webdriver-session
      (error "ChromeDriver created no session: %S" value))))

(defun dst-login--session-endpoint (suffix)
  (unless dst-login--webdriver-session
    (error "No active ChromeDriver session"))
  (format "/session/%s%s" dst-login--webdriver-session suffix))

(defun dst-login--css-string (value)
  (concat "\""
          (replace-regexp-in-string
           "\"" "\\\\\""
           (replace-regexp-in-string "\\\\" "\\\\\\\\" value t t)
           t t)
          "\""))

(defun dst-login--find-element-now (using value)
  ;; W3C WebDriver accepts CSS rather than the legacy "id"/"name"
  ;; strategies used by Selenium's convenience API.
  (pcase using
    ("id"
     (setq value (format "[id=%s]" (dst-login--css-string value))
           using "css selector"))
    ("name"
     (setq value (format "[name=%s]" (dst-login--css-string value))
           using "css selector")))
  (let ((element
         (dst-login--webdriver-request
          "POST" (dst-login--session-endpoint "/element")
          `((using . ,using) (value . ,value)))))
    (or (alist-get (intern "element-6066-11e4-a52e-4f735466cecf") element)
        (alist-get 'ELEMENT element)
        (error "ChromeDriver returned no element ID: %S" element))))

(defun dst-login--find-element (using value &optional timeout)
  (let ((deadline (time-add (current-time)
                            (or timeout dst-login-webdriver-timeout)))
        element last-error)
    (while (and (not element) (time-less-p (current-time) deadline))
      (condition-case error-data
          (setq element (dst-login--find-element-now using value))
        (dst-login-webdriver-error
         (setq last-error error-data)))
      (unless element
        (accept-process-output nil 0.2)))
    (or element
        (signal 'dst-login-webdriver-error
                (list (format "Timed out finding %s=%s" using value)
                      last-error)))))

(defun dst-login--element-present-p (using value)
  (condition-case nil
      (progn (dst-login--find-element-now using value) t)
    (dst-login-webdriver-error nil)))

(defun dst-login--login-page-p ()
  (or (dst-login--element-present-p "name" "username")
      (dst-login--element-present-p "name" "_F5_challenge")))

(defun dst-login--element-endpoint (element suffix)
  (dst-login--session-endpoint
   (format "/element/%s%s" (url-hexify-string element) suffix)))

(defun dst-login--send-keys (element text)
  (dst-login--webdriver-request
   "POST" (dst-login--element-endpoint element "/value")
   `((text . ,text)
     (value . ,(vconcat
                (mapcar #'char-to-string (string-to-list text)))))))

(defun dst-login--begin-login (&optional visible)
  (when (timerp dst-login--download-timer)
    (dst-login--cancel-download-timer)
    (when dst-login--pending-project
      (dst-login--set-project-status
       dst-login--pending-project "Paused for login")))
  (setq dst-login--authenticated nil
        dst-login--two-factor-pending nil)
  (dst-login--set-remote-status
   (if visible "Opening visible Chromium login…" "Opening the DST firewall…"))
  (redisplay)
  (unless (and dst-login--webdriver-session
               (eq (not (null visible))
                   (not (null dst-login--webdriver-visible))))
    (dst-login--create-session visible))
  (dst-login--webdriver-request
   "DELETE" (dst-login--session-endpoint "/cookie"))
  (dst-login--webdriver-request
   "POST" (dst-login--session-endpoint "/url")
   `((url . ,dst-login-url)))
  (let ((username (dst-login--find-element "name" "username")))
    (dst-login--send-keys
     username
     (concat dst-ident "\ue004" dst-firewall-code "\ue004\ue007")))
  (dst-login--find-element "name" "_F5_challenge")
  (setq dst-login--two-factor-pending t)
  (dst-login--set-remote-status "Waiting for the 2FA code")
  (call-interactively #'dst-login-enter-two-factor))

(defun dst-login-enter-two-factor ()
  "Submit 2FA, or start a fresh login when DST is not waiting for it."
  (interactive)
  (if (not dst-login--two-factor-pending)
      (dst-login--begin-login dst-login--webdriver-visible)
    (let ((code (string-trim (read-string "DST two-factor code: "))))
      (unless (string-empty-p code)
        (dst-login--set-remote-status "Submitting the 2FA code…")
        (let ((field (dst-login--find-element "name" "_F5_challenge")))
          (dst-login--webdriver-request
           "POST" (dst-login--element-endpoint field "/clear")
           (make-hash-table))
          (dst-login--send-keys field (concat code "\ue007")))
        (let ((deadline (time-add (current-time) dst-login-webdriver-timeout)))
          (while (and (dst-login--element-present-p "name" "_F5_challenge")
                      (time-less-p (current-time) deadline))
            (accept-process-output nil 0.2))
          (if (dst-login--element-present-p "name" "_F5_challenge")
              (progn
                (setq dst-login--two-factor-pending t)
                (dst-login--set-remote-status
                 "2FA was not accepted — press c to retry"))
            (setq dst-login--two-factor-pending nil
                  dst-login--authenticated t)
            (dst-login--set-remote-status "Connected")))))))

(defun dst-login-visible ()
  "Open a visible Chromium login and submit ident and firewall PIN."
  (interactive)
  (condition-case error-data
      (dst-login--begin-login t)
    (error
     (dst-login--set-remote-status "Visible login failed: %s"
                                   (error-message-string error-data))
     (signal (car error-data) (cdr error-data)))))

(defun dst-login--project-at-point ()
  (or (get-text-property (point) 'dst-login-project)
      (get-text-property (line-beginning-position) 'dst-login-project)))

(defun dst-login-connect-at-point ()
  "Download and open the project on the current line."
  (interactive)
  (let ((project (dst-login--project-at-point)))
    (unless project
      (user-error "Move point to a project line first"))
    (dst-login-connect project)))

(defun dst-login-connect-number ()
  "Download and open the project selected by a 0–9 or A–Z key."
  (interactive)
  (let* ((index
          (cond
           ((and (>= last-command-event ?0) (<= last-command-event ?9))
            (- last-command-event ?0))
           ((and (>= last-command-event ?A) (<= last-command-event ?Z))
            (+ 10 (- last-command-event ?A)))
           (t (user-error "That is not a DST project key"))))
         (project (nth index dst-login-list)))
    (unless project
      (user-error "There is no project for key %s"
                  (dst-login--index-key index)))
    (dst-login-connect project)))

(defun dst-login--project-user (project)
  "Return the full DST login for PROJECT."
  (concat dst-ident (nth 1 project) "@dstfse.local"))

(defun dst-login--password-attempt-key (project password)
  "Identify PROJECT's PASSWORD without storing another plaintext copy."
  (list (dst-login--project-user project) (secure-hash 'sha256 password)))

(defun dst-login--check-password-attempt (project password)
  "Reject a third use of PROJECT's failed PASSWORD, including pending attempts."
  (let* ((key (dst-login--password-attempt-key project password))
         (counts (gethash key dst-login--password-attempts '(0 . 0))))
    (when (>= (car counts) 2)
      (dst-login--set-project-status project "Password blocked")
      (user-error
       "Password blocked for project %s after two failed/unconfirmed attempts (or account lockout); correct authinfo and press k, or use p with a reset password"
       (nth 1 project)))
    (when (>= (+ (car counts) (cdr counts)) 2)
      (user-error "Wait for the pending password attempt(s) for project %s to finish"
                  (nth 1 project)))
    key))

(defun dst-login--begin-password-attempt (project password)
  "Reserve a password attempt for PROJECT and return its credential key."
  (let* ((key (dst-login--check-password-attempt project password))
         (counts (gethash key dst-login--password-attempts '(0 . 0))))
    (puthash key (cons (car counts) (1+ (cdr counts))) dst-login--password-attempts)
    key))

(defun dst-login--finish-password-attempt (key &optional outcome)
  "Release KEY's pending attempt and record OUTCOME.
OUTCOME is `rejected', `unconfirmed', `locked', or nil for other outcomes.
A lockout blocks further uses immediately.  Successful attempts do not erase
earlier failures for the same password."
  (when key
    (let* ((counts (gethash key dst-login--password-attempts '(0 . 0)))
           (failures (pcase outcome
                       ('locked 2)
                       ((or 'rejected 'unconfirmed) (min 2 (1+ (car counts))))
                       (_ (car counts)))))
      (puthash key (cons failures (max 0 (1- (cdr counts)))) dst-login--password-attempts))))

(defun dst-login--authinfo-password (project)
  "Read PROJECT's password from authinfo without prompting or session caching."
  (let* ((auth-sources (list dst-login-authinfo-file))
         (auth-source-do-cache nil)
         (entry (car (auth-source-search
                      :host "dstfse.local" :port "3389"
                      :user (dst-login--project-user project)
                      :require '(:secret) :max 1)))
         (secret (plist-get entry :secret))
         (password (if (functionp secret) (funcall secret) secret)))
    (and (stringp password) (not (string-empty-p password)) password)))

(defun dst-login--project-password (project)
  "Return PROJECT's cached password, authinfo secret, or prompted password."
  (let* ((number (nth 1 project))
         (user (dst-login--project-user project))
         (cached (gethash user dst-login-password-cache)))
    (or cached
        (let ((password (dst-login--authinfo-password project)))
          (unless (and (stringp password) (not (string-empty-p password)))
            (setq password
                  (read-passwd (format "DST password for %s (%s): "
                                       number (nth 0 project)))))
          (unless (and (stringp password) (not (string-empty-p password)))
            (user-error "No password entered"))
          (puthash user password dst-login-password-cache)
          password))))

(defun dst-login--authinfo-token (password)
  "Encode PASSWORD as a token understood by the authinfo parser."
  (when (or (string-empty-p password) (string-match-p "[\n\r]" password))
    (user-error "Passwords must be nonempty and contain no newlines"))
  (cond
   ((not (string-match-p "\"" password)) (concat "\"" password "\""))
   ((not (string-match-p "'" password)) (concat "'" password "'"))
   ((and (not (string-match-p "[ \t]" password))
         (not (memq (aref password 0) '(?\" ?\' ?#)))) password)
   (t (user-error "This password cannot be represented in authinfo; avoid mixing both quote types with whitespace"))))

(defun dst-login--check-authinfo-writable ()
  "Check that authinfo can be updated without discarding unsaved edits."
  (let* ((file (expand-file-name dst-login-authinfo-file))
         (buffer (get-file-buffer file)))
    (when (and buffer (buffer-modified-p buffer))
      (user-error "Save your edits to %s before changing passwords" file))
    (unless (file-writable-p file)
      (user-error "Cannot write %s" file))))

(defun dst-login--save-project-password (project password)
  "Update PROJECT's authinfo entry to PASSWORD after confirmed success.
Preserve other entries, formatting and comments.  Update all exact duplicate
entries; add an entry if none matches.  Never replace wildcard credentials."
  (dst-login--check-authinfo-writable)
  (let* ((file (expand-file-name dst-login-authinfo-file))
         (user (dst-login--project-user project))
         (token (dst-login--authinfo-token password))
         (visiting (get-file-buffer file))
         replacements fields password-start password-end entry-end matched)
    (with-temp-buffer
      (when (file-exists-p file) (insert-file-contents file))
      (cl-labels
          ((finish-entry
            ()
            (when (and (equal (or (cdr (assoc "machine" fields))
                                 (cdr (assoc "host" fields))) "dstfse.local")
                       (equal (or (cdr (assoc "login" fields))
                                  (cdr (assoc "user" fields))
                                  (cdr (assoc "account" fields))) user)
                       (equal (cdr (assoc "port" fields)) "3389"))
              (setq matched t)
              (push (if password-start
                        (list password-start password-end token)
                      (list entry-end entry-end (concat " password " token)))
                    replacements))))
        (goto-char (point-min))
        (while (progn (auth-source-netrc-parse-next-interesting) (not (eobp)))
          (let ((key (auth-source-netrc-parse-one)))
            (unless key (error "Cannot parse authinfo"))
            (when (member key '("machine" "default"))
              (finish-entry)
              (setq fields nil password-start nil password-end nil))
            (if (equal key "default")
                (push (cons key t) fields)
              (unless (auth-source-netrc-looking-at-token)
                (error "Missing value in authinfo"))
              (let ((start (match-beginning 0))
                    (end (match-end 0))
                    (value (auth-source-netrc-parse-one)))
                (setq entry-end end)
                (push (cons key value) fields)
                (when (equal key "password")
                  (setq password-start start password-end end))))))
        (finish-entry))
      ;; Apply edits from the end so positions remain valid.
      (dolist (replacement replacements)
        (goto-char (nth 0 replacement))
        (delete-region (nth 0 replacement) (nth 1 replacement))
        (insert (nth 2 replacement)))
      (unless matched
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert (format "machine dstfse.local port 3389 login %s password %s\n"
                        user token)))
      ;; Avoid backups of old credentials; file handlers support .authinfo.gpg.
      (let ((epa-file-encrypt-to (and visiting
                                      (buffer-local-value 'epa-file-encrypt-to visiting))))
        (write-region (point-min) (point-max) file nil 'silent))
      (set-file-modes file #o600))
    (setq auth-source-netrc-cache
          (assoc-delete-all dst-login-authinfo-file
                            (assoc-delete-all file auth-source-netrc-cache)))
    (when (buffer-live-p visiting)
      (with-current-buffer visiting (revert-buffer t t t)))))

(defun dst-login--switch-window (handle)
  "Select the ChromeDriver window HANDLE."
  (dst-login--webdriver-request
   "POST" (dst-login--session-endpoint "/window") `((handle . ,handle))))

(defun dst-login--password-webtop ()
  "Find and select the browser tab containing DST's password-change button."
  (or (cl-loop for handle in (dst-login--webdriver-request
                             "GET" (dst-login--session-endpoint "/window/handles"))
               thereis (progn
                         (dst-login--switch-window handle)
                         (and (dst-login--element-present-p
                               "id" "/Common/dst-FSE-password") handle)))
      (user-error "DST project page is unavailable; press c to log in again")))

(defun dst-login--open-password-form ()
  "Open the password form and return (WEBTOP-HANDLE FORM-HANDLE NEW-TAB-P)."
  (let* ((main (dst-login--password-webtop))
         (before (dst-login--webdriver-request
                  "GET" (dst-login--session-endpoint "/window/handles")))
         (button (dst-login--find-element "id" "/Common/dst-FSE-password"))
         (deadline (time-add (current-time) dst-login-webdriver-timeout))
         form)
    (dst-login--webdriver-request
     "POST" (dst-login--element-endpoint button "/click") (make-hash-table))
    (while (and (not form) (time-less-p (current-time) deadline))
      (let* ((handles (dst-login--webdriver-request
                       "GET" (dst-login--session-endpoint "/window/handles")))
             (new-handles (cl-set-difference handles before :test #'equal))
             (candidates (append new-handles
                                 (cl-remove-if (lambda (handle)
                                                 (or (equal handle main)
                                                     (member handle new-handles)))
                                               handles)
                                 (list main))))
        (dolist (handle candidates)
          (unless form
            (dst-login--switch-window handle)
            (when (dst-login--element-present-p "id" "DomainUserName")
              (setq form handle)))))
      (unless form (accept-process-output nil 0.2)))
    (unless form (error "Timed out opening the DST password form"))
    (list main form (not (member form before)))))

(defun dst-login--password-page-text ()
  "Return visible text from the current password form, excluding input values."
  (let ((body (dst-login--find-element "css selector" "body")))
    (dst-login--webdriver-request
     "GET" (dst-login--element-endpoint body "/text"))))

(defun dst-login--password-submit-button ()
  "Find the enabled submit button on the current or legacy DST password form.
DST now uses submitBtn; older forms used btnSignIn.  Disabled buttons mean
client-side validation failed, so do not reserve a password attempt yet."
  (let ((button (dst-login--find-element "css selector" "#submitBtn, #btnSignIn")))
    (unless (eq t (dst-login--webdriver-request
                   "GET" (dst-login--element-endpoint button "/enabled")))
      (user-error "DST has disabled Change Password; check the filled fields and password requirements. No change submitted. Form: %s"
                  (dst-login--password-page-text)))
    button))

(defun dst-login--password-result ()
  "Wait for explicit DST success; signal an error if success is unconfirmed."
  (let ((deadline (time-add (current-time) dst-login-webdriver-timeout))
        text success)
    (while (and (not success) (time-less-p (current-time) deadline))
      (setq text (dst-login--password-page-text)
            success (string-match-p
                     "your password has been successfully changed"
                     (downcase text)))
      (unless success (accept-process-output nil 0.2)))
    (unless success
      (error "DST did not confirm the password change. Check the result before retrying. Server page: %s"
             (or text "No response")))))

(defun dst-login--password-log (text)
  "Append non-secret TEXT to the password-change results buffer."
  (let ((buffer (get-buffer-create "*dst-password-changes*")))
    (with-current-buffer buffer
      (special-mode)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert text "\n")))
    buffer))

(defun dst-login--change-one-password (project current-password new-password)
  "Submit one password change and update credentials only on confirmed success."
  (let ((user (dst-login--project-user project))
        (number (nth 1 project))
        context submitted confirmed attempt)
    (dst-login--check-password-attempt project current-password)
    (unwind-protect
        (condition-case err
            (progn
              (dst-login--set-project-status project "Changing password")
              (setq context (dst-login--open-password-form))
              (dolist (field `(("DomainUserName" . ,user)
                               ("UserPass" . ,current-password)
                               ("NewUserPass" . ,new-password)
                               ("ConfirmNewUserPass" . ,new-password)))
                (let ((element (dst-login--find-element "id" (car field))))
                  (dst-login--webdriver-request
                   "POST" (dst-login--element-endpoint element "/clear") (make-hash-table))
                  (dst-login--send-keys element (cdr field))))
              (let ((button (dst-login--password-submit-button)))
                ;; A lost response can follow a successful submission: never retry.
                (setq attempt (dst-login--begin-password-attempt project current-password)
                      submitted t)
                (dst-login--webdriver-request
                 "POST" (dst-login--element-endpoint button "/click") (make-hash-table)))
              (dst-login--password-result)
              (setq confirmed t)
              ;; DST has just made this password valid, even if an earlier
              ;; attempt to use the same text was rejected before the reset.
              (let* ((key (dst-login--password-attempt-key project new-password))
                     (counts (gethash key dst-login--password-attempts '(0 . 0))))
                (puthash key (cons 0 (cdr counts)) dst-login--password-attempts))
              (puthash user new-password dst-login-password-cache)
              (when dst-login-save-changed-passwords
                (dst-login--save-project-password project new-password))
              (dst-login--set-project-status project "Password changed")
              (dst-login--password-log
               (format "%s: password changed; %s" number
                       (if dst-login-save-changed-passwords "authinfo updated"
                         "update authinfo manually"))))
          ((error quit)
           (when (and submitted (not confirmed))
             (remhash user dst-login-password-cache))
           (dst-login--set-project-status
            project (if confirmed "Changed; save failed" "Change unconfirmed"))
           (let ((detail (error-message-string err)))
             (dolist (secret (list current-password new-password))
               (setq detail (string-replace secret "[REDACTED]" detail)))
             (display-buffer
              (dst-login--password-log
               (format "%s: %s. %s\nBatch stopped; later projects were not changed."
                       number (if confirmed "Changed at DST; save authinfo manually"
                                "Password change NOT confirmed") detail))))
           (user-error "Password change stopped for %s; see *dst-password-changes*" number)))
      ;; Keep an unconfirmed result available for inspection.  On success,
      ;; close our new tab or navigate the original tab back to the webtop.
      ;; An unconfirmed submission may already have checked the password at
      ;; DST, so retain it against the limit even if the response was lost.
      (dst-login--finish-password-attempt
       attempt (and submitted (not confirmed) 'unconfirmed))
      (when context
        (ignore-errors
          (when confirmed
            (dst-login--switch-window (nth 1 context))
            (cond
             ((nth 2 context)
              (dst-login--webdriver-request
               "DELETE" (dst-login--session-endpoint "/window")))
             ((equal (car context) (nth 1 context))
              (dst-login--webdriver-request
               "POST" (dst-login--session-endpoint "/url") `((url . ,dst-login-url))))))
          (dst-login--switch-window (car context)))))))

(defun dst-login--password-project (&optional project)
  "Resolve PROJECT, the dashboard row, or a completion choice."
  (or (and (listp project) project)
      (and (stringp project)
           (or (assoc project dst-login-list)
               (cl-find project dst-login-list :key #'cadr :test #'equal)
               (user-error "Unknown DST project: %s" project)))
      (and (null project) (dst-login--project-at-point))
      (let* ((choices (mapcar (lambda (entry)
                               (cons (format "%s — %s" (nth 1 entry) (car entry)) entry))
                             dst-login-list))
             (choice (completing-read "DST project: " choices nil t)))
        (cdr (assoc choice choices)))))

(defun dst-login--run-password-changes (changes new-password &optional visual)
  "Apply CHANGES, a list of (PROJECT CURRENT-PASSWORD), in one DST session."
  (when (timerp dst-login--download-timer)
    (user-error "Wait for the launcher download before changing passwords"))
  (unless (and (stringp new-password) (not (string-empty-p new-password)))
    (user-error "No new password entered"))
  (when dst-login-save-changed-passwords
    (dst-login--authinfo-token new-password)
    (dst-login--check-authinfo-writable))
  (unless changes (user-error "No projects need a password change"))
  ;; Preflight every credential so a blocked later project cannot cause a
  ;; partially submitted batch.
  (dolist (change changes)
    (dst-login--check-password-attempt (car change) (cadr change)))
  (unless (and dst-login--authenticated
               (or (not visual) dst-login--webdriver-visible))
    (dst-login--begin-login visual))
  (unless dst-login--authenticated (user-error "Complete firewall authentication first"))
  (dst-login--password-log (format "\n%s — changing %d project password(s)"
                                   (format-time-string "%F %T") (length changes)))
  (dolist (change changes)
    (dst-login--change-one-password (car change) (cadr change) new-password))
  (message "Changed %d DST project password(s)%s" (length changes)
           (if dst-login-save-changed-passwords "; authinfo updated"
             "; remember to update authinfo")))

(defun dst-change-password (&optional project new-password _click-submit visual)
  "Change/reset PROJECT's password using the dashboard browser session.
Always prompt for the current password, including a temporary reset password.
Prompt for NEW-PASSWORD with confirmation unless supplied programmatically.
VISUAL (the interactive prefix argument) requests a visible browser.
The legacy CLICK-SUBMIT argument is retained for call compatibility; this
command submits the change and checks DST's response."
  (interactive (list nil nil nil current-prefix-arg))
  (dst-login--validate-config)
  (let* ((entry (dst-login--password-project project))
         (current (read-passwd (format "Current/reset password for %s: " (nth 1 entry))))
         (new (or new-password (read-passwd "New DST project password: " t))))
    (when (string-empty-p current) (user-error "No current password entered"))
    (when (equal current new) (user-error "New password is identical to the current password"))
    (dst-login--run-password-changes (list (list entry current)) new visual)))

(defun dst-login--change-project-passwords (projects new-password visual)
  "Read all current passwords for PROJECTS from authinfo and change them."
  (let (seen changes missing)
    (dolist (entry projects)
      (unless (member (nth 1 entry) seen)
        (push (nth 1 entry) seen)
        (let ((current (dst-login--authinfo-password entry)))
          (if current
              (push (list entry current) changes)
            (push (nth 1 entry) missing)))))
    (when missing
      (user-error "Missing authinfo password for project(s) %s; no changes submitted"
                  (string-join (nreverse missing) ", ")))
    (let ((new (or new-password (read-passwd "New password for DST projects: " t))))
      (setq changes (cl-remove-if (lambda (change) (equal (cadr change) new))
                                  (nreverse changes)))
      (dst-login--run-password-changes changes new visual))))

(defun dst-change-all-passwords (&optional new-password visual)
  "Change each unique project's password using current passwords from authinfo.
Prompt only for the new password (with confirmation), plus firewall 2FA if
needed.  Missing authinfo entries abort the entire batch before submission."
  (interactive (list nil current-prefix-arg))
  (dst-login--validate-config)
  (dst-login--change-project-passwords dst-login-list new-password visual))

(defun dst-change-password-loop (&optional new-password visual)
  "Change selected project passwords using their current authinfo passwords."
  (interactive (list nil current-prefix-arg))
  (dst-login--validate-config)
  (let* ((choices (mapcar (lambda (entry) (cons (nth 1 entry) entry)) dst-login-list))
         (selected (completing-read-multiple "DST project numbers (comma-separated): "
                                             choices nil t)))
    (unless selected (user-error "No projects selected"))
    (dst-login--change-project-passwords
     (mapcar (lambda (number) (cdr (assoc number choices))) selected) new-password visual)))

(defun dst-login--server-button-id (server)
  (if (string-prefix-p "/Common/" server)
      server
    (concat "/Common/dst-RDP_FSE_"
            (or (cdr (assoc-string server dst-login-server-aliases t))
                server))))

(defun dst-login--rdp-snapshot ()
  (let ((directory (file-name-as-directory
                    (expand-file-name dst-login-download-directory))))
    (when (file-directory-p directory)
      (mapcar
       (lambda (file)
         (let ((attributes (file-attributes file)))
           (list file
                 (file-attribute-modification-time attributes)
                 (file-attribute-size attributes))))
       (directory-files directory t "\\.rdp\\'" t)))))

(defun dst-login--new-rdp-file ()
  (let* ((directory (file-name-as-directory
                     (expand-file-name dst-login-download-directory)))
         (files (and (file-directory-p directory)
                     (directory-files directory t "\\.rdp\\'" t)))
         changed)
    (dolist (file files)
      (let* ((attributes (file-attributes file))
             (state (list file
                          (file-attribute-modification-time attributes)
                          (file-attribute-size attributes))))
        (unless (member state dst-login--download-snapshot)
          (push file changed))))
    (car (sort changed
               (lambda (left right)
                 (time-less-p (file-attribute-modification-time
                               (file-attributes right))
                              (file-attribute-modification-time
                               (file-attributes left))))))))

(defun dst-login--cancel-download-timer ()
  (when (timerp dst-login--download-timer)
    (cancel-timer dst-login--download-timer))
  (setq dst-login--download-timer nil))

(defun dst-login--poll-download ()
  (cond
   ((time-less-p dst-login--download-deadline (current-time))
    (dst-login--cancel-download-timer)
    (setq dst-login--authenticated nil)
    (dst-login--set-project-status
     dst-login--pending-project "Download timed out")
    (dst-login--set-remote-status
     "No launcher received — press c to refresh the login"))
   ((let ((file (dst-login--new-rdp-file)))
      (when file
        (dst-login--cancel-download-timer)
        (dst-login--finish-download file)
        t)))))

(defun dst-login--finish-download (downloaded-file)
  (let* ((project dst-login--pending-project)
         (number (nth 1 project))
         (target (expand-file-name
                  (format "dst-launcher-%s.rdp" number)
                  dst-login-download-directory)))
    (unless (equal (file-truename downloaded-file)
                   (and (file-exists-p target) (file-truename target)))
      (rename-file downloaded-file target t))
    (dst-login--set-project-status project "Starting FreeRDP")
    (dst-login--start-xfreerdp project target)))

(defun dst-login-show-project-log ()
  "Show FreeRDP output for the project at point, with input while it is running."
  (interactive)
  (let* ((project (or (dst-login--project-at-point)
                      (user-error "Move point to a project line first")))
         (buffer (get-buffer (format "*dst-rdp-%s*" (nth 1 project)))))
    (unless buffer
      (user-error "No connection log for project %s yet" (nth 1 project)))
    (pop-to-buffer buffer)
    (goto-char (point-max))))

(define-derived-mode dst-login-rdp-mode comint-mode "DST-RDP"
  "Interactive FreeRDP output.  Type a prompt's answer and press RET to send it."
  (setq-local buffer-read-only nil
              truncate-lines nil
              comint-ptyp t
              comint-process-echoes nil
              comint-prompt-read-only t
              comint-input-filter #'ignore
              comint-input-sender #'dst-login--rdp-send-input))

(defun dst-login--rdp-send-input (process input)
  "Send the user's INPUT to FreeRDP PROCESS without choosing an answer for them."
  (comint-simple-send process input)
  (when (process-get process 'dst-login-awaiting-input)
    (process-put process 'dst-login-awaiting-input nil)
    (process-put process 'dst-login-prompt-tail nil)
    (dst-login--set-project-status (process-get process 'dst-login-project) "Running")))

(defun dst-login--rdp-output-filter (process output)
  "Insert OUTPUT from PROCESS and expose interactive FreeRDP questions.
Keep a short tail to recognize prompts split across process-output chunks."
  (comint-output-filter process output)
  (when (buffer-live-p (process-buffer process))
    (let* ((text (concat (process-get process 'dst-login-prompt-tail) output))
           (case-fold-search t)
           (start 0)
           prompt-end)
      ;; FreeRDP uses (Y/T/N) for certificate trust and (Y/N) for consent.
      ;; Match the question, not merely a certificate warning in the log.
      (while (string-match "(y/\\(?:t/\\)?n)" text start)
        (setq prompt-end (match-end 0)
              start prompt-end))
      (process-put process 'dst-login-prompt-tail
                   (substring text (max (or prompt-end 0) (- (length text) 512))))
      (when prompt-end
        (process-put process 'dst-login-awaiting-input t)
        (dst-login--set-project-status (process-get process 'dst-login-project) "Awaiting reply")
        (pop-to-buffer (process-buffer process))
        (goto-char (point-max))
        (message "DST project %s needs input: review the prompt, type your answer, and press RET"
                 (process-get process 'dst-login-project))))))

(defun dst-login--rdp-exit-description (code)
  "Describe a FreeRDP exit CODE, retaining unknown codes verbatim."
  (or (cdr (assq code '((0 . "Closed")
                        (132 . "Authentication failed")
                        (134 . "Logon failed")
                        (135 . "Account locked out")
                        (148 . "Password expired")
                        (149 . "Password must change")
                        (151 . "Account disabled")
                        (154 . "Wrong password")
                        (155 . "Access denied"))))
      (format "Failed (exit %d)" code)))

(defun dst-login--rdp-append-log (buffer text)
  "Append TEXT to the connection log BUFFER, if it still exists."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert text)))))

(defun dst-login--rdp-sentinel (number proc event)
  "Report termination of project NUMBER's PROC and show failures in full.
EVENT is the process sentinel event.  FreeRDP is an external program;
its stdout and stderr contain its diagnostics, including any native
backtrace it emits, rather than an Emacs Lisp backtrace."
  (when (and (memq (process-status proc) '(exit signal failed))
             (not (process-get proc 'dst-login-result-handled)))
    (process-put proc 'dst-login-result-handled t)
    (let* ((code (process-exit-status proc))
           (normal-exit (eq (process-status proc) 'exit))
           (failed (not (and normal-exit (zerop code))))
           (description (if normal-exit
                            (dst-login--rdp-exit-description code)
                          (format "Stopped: %s" (string-trim event))))
           (buffer (process-buffer proc))
           (attempt (process-get proc 'dst-login-password-attempt))
           (outcome (and normal-exit
                         (cond ((= code 135) 'locked)
                               ((memq code '(132 134 154)) 'rejected)))))
      (dst-login--finish-password-attempt attempt outcome)
      (process-put proc 'dst-login-password-attempt nil)
      (dst-login--set-project-status number "%s" description)
      (dst-login--rdp-append-log
       buffer (format "\n[%s] %s (%s %d): %s\n"
                      (format-time-string "%F %T") description
                      (process-status proc) code (string-trim event)))
      (when (and attempt outcome)
        (let ((failures (car (gethash attempt dst-login--password-attempts))))
          (dst-login--rdp-append-log
           buffer (if (>= failures 2)
                      "This password is now blocked for this project. Correct authinfo and press k, or use p with a reset password.\n"
                    "Password rejected (1 of 2 attempts). Verify the password before retrying.\n"))))
      (when (and normal-exit (= code 135))
        (dst-login--rdp-append-log
         buffer "DST reports that this project account is locked.\nHave DST unlock it, or wait for DST's lockout period before retrying.\nCheck the project password in authinfo, then press k in the dashboard.\n"))
      (when failed
        (when (buffer-live-p buffer)
          (display-buffer buffer))
        (message "DST project %s: %s; see %s"
                 number description (if (buffer-live-p buffer)
                                        (buffer-name buffer)
                                      "connection log (buffer was killed)"))))))

(defun dst-login--start-xfreerdp (project launcher)
  (let* ((number (nth 1 project))
         (password (dst-login--project-password project))
         (user (dst-login--project-user project))
         (buffer (get-buffer-create (format "*dst-rdp-%s*" number)))
         (command (append (list dst-login-xfreerdp-program
                                launcher
                                (concat "/u:" user)
                                (concat "/p:" password))
                          dst-login-xfreerdp-arguments))
         attempt)
    (dst-login--check-password-attempt project password)
    ;; A Comint buffer must have one input target.  Never replace a running
    ;; connection or mix two certificate questions in the same project buffer.
    (when-let* ((existing (get-buffer-process buffer))
                (_ (process-live-p existing)))
      (pop-to-buffer buffer)
      (user-error "FreeRDP is already running for project %s; use its existing buffer" number))
    (with-current-buffer buffer
      (dst-login-rdp-mode))
    ;; Keep previous attempts, but never print the password-bearing command.
    (dst-login--rdp-append-log
     buffer (format "\n=== %s: project %s, server %s ===\nProgram: %s\nLauncher: %s\nAnswer FreeRDP questions here and press RET.\nFreeRDP stdout/stderr:\n"
                    (format-time-string "%F %T") number (nth 2 project)
                    dst-login-xfreerdp-program launcher))
    (dst-login--set-project-status project "Starting FreeRDP")
    (condition-case err
        (progn
          (setq attempt (dst-login--begin-password-attempt project password))
          (let ((proc (make-process
                       :name (format "dst-rdp-%s" number)
                       :buffer buffer
                       :command command
                       :connection-type 'pty
                       :filter #'dst-login--rdp-output-filter
                       :noquery t
                       :sentinel (apply-partially #'dst-login--rdp-sentinel number))))
            (process-put proc 'dst-login-password-attempt attempt)
            (process-put proc 'dst-login-project number)
            (set-marker (process-mark proc) (with-current-buffer buffer (point-max)) buffer)
            ;; The sentinel owns this reservation once the process exists.
            (setq attempt nil))
          (dst-login--set-project-status project "Running"))
      ((error quit)
       (dst-login--finish-password-attempt attempt)
       (dst-login--set-project-status project "Failed to start")
       (dst-login--rdp-append-log
        buffer (concat "\nCould not start FreeRDP: "
                       (string-replace password "[REDACTED]"
                                       (error-message-string err))
                       "\n"))
       (display-buffer buffer)
       (signal (car err) (cdr err))))))

(defun dst-login-connect (project)
  "Download PROJECT's launcher and open it with FreeRDP."
  (unless dst-login--authenticated
    (user-error "Open the firewall and enter the 2FA code first"))
  (when (timerp dst-login--download-timer)
    (user-error "Another DST launcher download is still in progress"))
  ;; Resolve the password before clicking so it cannot strand a download.
  (dst-login--check-password-attempt project (dst-login--project-password project))
  (make-directory (expand-file-name dst-login-download-directory) t)
  (setq dst-login--pending-project project
        dst-login--download-snapshot (dst-login--rdp-snapshot)
        dst-login--download-deadline
        (time-add (current-time) dst-login-download-timeout)
        dst-login--download-timer
        (run-at-time 0.2 0.2 #'dst-login--poll-download))
  (dst-login--set-project-status project "Requesting launcher")
  (condition-case error-data
      (let* ((button-id (dst-login--server-button-id (nth 2 project)))
             (button (dst-login--find-element
                      "id" button-id dst-login-download-timeout)))
        (dst-login--webdriver-request
         "POST" (dst-login--element-endpoint button "/click")
         (make-hash-table))
        (if (dst-login--login-page-p)
            (progn
              (dst-login--cancel-download-timer)
              (setq dst-login--authenticated nil)
              (dst-login--set-project-status project "Login required")
              (dst-login--set-remote-status
               "Login expired — press c to log in again"))
          (dst-login--set-project-status project "Downloading launcher")))
    (error
     (dst-login--cancel-download-timer)
     (setq dst-login--authenticated nil)
     (dst-login--set-project-status project "Launcher request failed")
     (dst-login--set-remote-status
      "Connection may have expired — press c to log in again")
     (signal (car error-data) (cdr error-data)))))

(defun dst-login-forget-passwords ()
  "Forget cached DST passwords so the next connection rereads authinfo.
This does not reset failed-password counts or unblock rejected passwords."
  (interactive)
  (clrhash dst-login-password-cache)
  (message "Forgot all cached DST project passwords"))

(defun dst-login--stop-webdriver ()
  (dst-login--cancel-download-timer)
  (dst-login--delete-session)
  (when (dst-login--webdriver-live-p)
    (set-process-sentinel dst-login--webdriver-process #'ignore)
    (delete-process dst-login--webdriver-process))
  (when (and dst-login--webdriver-profile
             (file-directory-p dst-login--webdriver-profile)
             (file-in-directory-p dst-login--webdriver-profile
                                  temporary-file-directory))
    (delete-directory dst-login--webdriver-profile t))
  (setq dst-login--webdriver-process nil
        dst-login--webdriver-port nil
        dst-login--webdriver-session nil
        dst-login--webdriver-profile nil
        dst-login--webdriver-visible nil
        dst-login--authenticated nil
        dst-login--two-factor-pending nil
        dst-login--remote-status "Not connected"))

(defun dst-login-quit ()
  "Close ChromeDriver, Chromium, and the DST dashboard."
  (interactive)
  (when (get-buffer dst-login-buffer-name)
    (kill-buffer dst-login-buffer-name)))

;;;###autoload
(defun dst ()
  "Open the electric DST dashboard and start firewall authentication."
  (interactive)
  (dst-login--validate-config)
  (pop-to-buffer (dst-login-refresh))
  (unless dst-login--authenticated
    (condition-case error-data
        (dst-login--begin-login)
      (error
       (dst-login--set-remote-status "Login failed: %s"
                                     (error-message-string error-data))
       (signal (car error-data) (cdr error-data))))))

(provide 'dst-login-snps)

;;; dst-login-snps.el ends here
