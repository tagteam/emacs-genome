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
;; Load this file after dst-snps.el if both are used, so this file's `dst'
;; command takes precedence.

;;; Code:

(require 'cl-lib)
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
  "Project passwords retained for the rest of this Emacs session.")

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
        (insert "      v visible login, c login/enter 2FA, g refresh, k forget passwords, q quit\n\n")
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

(defun dst-login--project-password (project)
  (let* ((number (nth 1 project))
         (cached (gethash number dst-login-password-cache)))
    (or cached
        (let ((password (read-passwd
                         (format "DST password for %s (%s): "
                                 number (nth 0 project)))))
          (when (string-empty-p password)
            (user-error "No password entered"))
          (puthash number password dst-login-password-cache)
          password))))

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

(defun dst-login--start-xfreerdp (project launcher)
  (let* ((number (nth 1 project))
         (password (dst-login--project-password project))
         (user (concat dst-ident number "@dstfse.local"))
         (buffer (get-buffer-create (format "*dst-rdp-%s*" number)))
         (command (append (list dst-login-xfreerdp-program
                                launcher
                                (concat "/u:" user)
                                (concat "/p:" password))
                          dst-login-xfreerdp-arguments)))
    (make-process
     :name (format "dst-rdp-%s" number)
     :buffer buffer
     :command command
     :noquery t
     :sentinel
     (lambda (proc event)
       (unless (process-live-p proc)
         (dst-login--set-project-status
          number
          (cond
           ((and (eq (process-status proc) 'exit)
                 (zerop (process-exit-status proc)))
            "Closed")
           ((eq (process-status proc) 'exit)
            (format "Failed (exit %d)" (process-exit-status proc)))
           (t (format "Stopped: %s" (string-trim event))))))))
    (dst-login--set-project-status project "Running")))

(defun dst-login-connect (project)
  "Download PROJECT's launcher and open it with FreeRDP."
  (unless dst-login--authenticated
    (user-error "Open the firewall and enter the 2FA code first"))
  (when (timerp dst-login--download-timer)
    (user-error "Another DST launcher download is still in progress"))
  ;; Prompt before clicking, so a missing password cannot strand a download.
  (dst-login--project-password project)
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
  "Forget all DST project passwords cached during this Emacs session."
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
