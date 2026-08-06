;;; absalon.el --- Org/Emacs client for KU Absalon (Canvas) -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: Generated for an Absalon teacher
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, hypermedia, org
;; URL: https://absalon.ku.dk

;;; Commentary:

;; A small, dependency-free client for the Canvas REST API used by the
;; University of Copenhagen's Absalon installation.  It supports:
;;
;; - encrypted token lookup through auth-source;
;; - paginated inspection of courses, modules, pages and files;
;; - an Org-file-as-source-of-truth workflow for Canvas pages;
;; - previewing a page change before pushing it;
;; - creating/updating pages, adding them to modules, and positioning items;
;; - Canvas's multi-step file upload protocol; and
;; - JSON course snapshots for inspection or version control.
;;
;; Writes are dry-run by default.  Set `absalon-write-enabled' non-nil, either
;; with Customize or temporarily with a let binding, before a mutating command
;; can contact Absalon.  Interactive write commands also ask for confirmation.
;;
;; Token setup (recommended ~/.authinfo.gpg entry):
;;
;;   machine absalon.ku.dk login api-token password YOUR_TOKEN
;;
;; Minimal setup:
;;
;;   (require 'absalon)
;;   (setq absalon-write-enabled t) ; only after testing read commands
;;
;; Useful read commands:
;;
;;   M-x absalon-show-profile
;;   M-x absalon-list-courses
;;   M-x absalon-list-modules
;;   M-x absalon-list-pages
;;   M-x absalon-list-files
;;
;; An Org page should contain metadata like this:
;;
;;   #+TITLE: Week 1: Introduction
;;   #+ABSALON_COURSE_ID: 12345
;;   #+ABSALON_PAGE_URL: week-1-introduction
;;   #+ABSALON_MODULE_ID: 67890
;;   #+ABSALON_POSITION: 1
;;   #+ABSALON_PUBLISHED: false
;;
;; `ABSALON_PAGE_URL' is optional for a new page.  After creation it is written
;; back into the Org file.  `ABSALON_MODULE_ID' is optional; if present, a newly
;; created page is also added to that module.  Position is one-based.
;;
;; From the Org buffer:
;;
;;   M-x absalon-org-preview-page
;;   M-x absalon-org-push-page
;;
;; Upload a local attachment with `absalon-upload-file'.  The returned Canvas
;; file can then be linked from a page.  Uploads require the external `curl'
;; executable only for the final multipart transfer; other operations use
;; Emacs's built-in URL library.
;;
;; Caveats: Kaltura and other LTI tools have separate APIs.  This package does
;; not manage quizzes, submissions, enrolments, or student data.  Test writes
;; in a sandbox/unpublished course first.

;;; Code:

(require 'auth-source)
(require 'json)
(require 'org)
(require 'ox-html)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-parse)
(require 'url-util)

(defgroup absalon nil
  "Interact with KU Absalon's Canvas API."
  :group 'applications
  :prefix "absalon-")

(defcustom absalon-base-url "https://absalon.ku.dk"
  "Absalon base URL, without a trailing slash."
  :type 'string)

(defcustom absalon-auth-host "absalon.ku.dk"
  "Host used to find the API token with `auth-source-search'."
  :type 'string)

(defcustom absalon-auth-user "api-token"
  "Login name used to find the API token with `auth-source-search'."
  :type 'string)

(defcustom absalon-per-page 100
  "Requested number of objects per Canvas API page."
  :type 'integer)

(defcustom absalon-write-enabled nil
  "When non-nil, permit mutating API requests.

Interactive mutating commands still request confirmation."
  :type 'boolean)

(defcustom absalon-confirm-writes t
  "When non-nil, interactive mutating commands ask for confirmation."
  :type 'boolean)

(defcustom absalon-org-html-preamble nil
  "HTML inserted before the body exported from an Org page."
  :type '(choice (const :tag "None" nil) string))

(defcustom absalon-org-html-postamble nil
  "HTML inserted after the body exported from an Org page."
  :type '(choice (const :tag "None" nil) string))

(define-error 'absalon-error "Absalon error")
(define-error 'absalon-http-error "Absalon HTTP error" 'absalon-error)

(defvar absalon--last-response-headers nil)
(defvar absalon--last-response-status nil)

(defun absalon--token ()
  "Return the API token from auth-source, or signal an error."
  (let* ((entry (car (auth-source-search
                      :host absalon-auth-host
                      :user absalon-auth-user
                      :require '(:secret)
                      :max 1)))
         (secret (plist-get entry :secret))
         (token (if (functionp secret) (funcall secret) secret)))
    (unless (and (stringp token) (not (string-empty-p token)))
      (signal 'absalon-error
              (list (format "No token for %s/%s in auth-source"
                            absalon-auth-host absalon-auth-user))))
    token))

(defun absalon--api-url (path &optional query)
  "Return API URL for PATH and optional QUERY alist."
  (let ((base (concat (string-remove-suffix "/" absalon-base-url)
                      "/api/v1/"
                      (string-remove-prefix "/" path))))
    (if query
        (concat base "?" (url-build-query-string query))
      base)))

(defun absalon--encode-form (params)
  "Encode PARAMS as application/x-www-form-urlencoded data."
  (mapconcat
   (lambda (pair)
     (concat (url-hexify-string (format "%s" (car pair)))
             "="
             (url-hexify-string (format "%s" (cdr pair)))))
   params "&"))

(defun absalon--parse-link-header (headers relation)
  "Find URL with RELATION in Canvas Link HEADERS."
  (when-let* ((line (seq-find
                     (lambda (x) (string-equal-ignore-case (car x) "link"))
                     headers)))
    (let ((parts (split-string (cdr line) "," t "[[:space:]]*"))
          found)
      (dolist (part parts found)
        (when (string-match
               (format "<\\([^>]+\\)>;[[:space:]]*rel=\\\"%s\\\""
                       (regexp-quote relation))
               part)
          (setq found (match-string 1 part)))))))

(defun absalon--response-headers ()
  "Parse HTTP headers in the current URL response buffer."
  (save-excursion
    (goto-char (point-min))
    (let (headers)
      (forward-line 1)
      (while (and (not (eobp)) (not (looking-at-p "\\r?$")))
        (when (looking-at "^\\([^:]+\\):[[:space:]]*\\(.*?\\)\\r?$")
          (push (cons (match-string 1) (match-string 2)) headers))
        (forward-line 1))
      (nreverse headers))))

(defun absalon--goto-response-body ()
  "Move point to the beginning of the current HTTP response body."
  (goto-char (point-min))
  (cond
   ;; `url-http-end-of-headers' is maintained by url-http and is more robust
   ;; than rediscovering the boundary with a regexp.
   ((and (boundp 'url-http-end-of-headers)
         url-http-end-of-headers
         (integer-or-marker-p url-http-end-of-headers))
    (goto-char url-http-end-of-headers)
    (skip-chars-forward "\r\n"))
   ;; Fallback for unusual URL handlers.  These are actual CR/LF characters;
   ;; double-escaped `\\r' and `\\n' would match letters in Emacs regexps.
   ((re-search-forward "\r?\n\r?\n" nil t))
   (t
    (signal 'absalon-http-error (list "Malformed HTTP response")))))

(defun absalon--request-url (url &optional method params)
  "Request URL with METHOD and form PARAMS; return decoded JSON.

The response status and headers are saved in `absalon--last-response-status'
and `absalon--last-response-headers'."
  (let* ((method (or method "GET"))
         (url-request-method method)
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " (absalon--token)))
            ("Accept" . "application/json")))
         (url-request-data (and params (absalon--encode-form params))))
    (when params
      (push '("Content-Type" . "application/x-www-form-urlencoded")
            url-request-extra-headers))
    (let ((buffer (url-retrieve-synchronously url t t 60)))
      (unless buffer
        (signal 'absalon-http-error (list "No HTTP response" url)))
      (unwind-protect
          (with-current-buffer buffer
            (setq absalon--last-response-status
                  (or (and (boundp 'url-http-response-status)
                           url-http-response-status)
                      0)
                  absalon--last-response-headers
                  (absalon--response-headers))
            (absalon--goto-response-body)
            (let ((body (buffer-substring-no-properties (point) (point-max))))
              (unless (<= 200 absalon--last-response-status 299)
                (signal 'absalon-http-error
                        (list absalon--last-response-status method url body)))
              (if (string-empty-p (string-trim body))
                  nil
                (condition-case err
                    (json-parse-string body
                                       :object-type 'alist
                                       :array-type 'list
                                       :null-object nil
                                       :false-object :false)
                  (json-parse-error
                   (signal 'absalon-http-error
                           (list "Invalid JSON response" url body err)))))))
        (kill-buffer buffer)))))

(defun absalon-request (path &optional method params)
  "Call Canvas API PATH using METHOD and form PARAMS."
  (absalon--request-url (absalon--api-url path) method params))

(defun absalon-get-all (path &optional query)
  "GET all paginated objects at API PATH with QUERY."
  (let* ((query (append query `((per_page ,absalon-per-page))))
         (url (absalon--api-url path query))
         all)
    (while url
      (let ((page (absalon--request-url url "GET")))
        (unless (listp page)
          (signal 'absalon-error (list "Expected an array" url page)))
        (setq all (nconc all page)
              url (absalon--parse-link-header
                   absalon--last-response-headers "next"))))
    all))

(defun absalon--ensure-write (&optional _description)
  "Signal an error unless mutating requests are enabled."
  (unless absalon-write-enabled
    (user-error
     "Dry-run mode: set `absalon-write-enabled' non-nil to permit writes")))

(defun absalon--confirm-write (description)
  "Require write mode and optionally confirm DESCRIPTION."
  (absalon--ensure-write)
  (when (and absalon-confirm-writes
             (not (yes-or-no-p (format "%s? " description))))
    (user-error "Cancelled")))

(defun absalon-profile ()
  "Return the current Absalon user profile."
  (absalon-request "users/self/profile"))

(defun absalon-courses (&optional include-completed)
  "Return current user's courses.
With INCLUDE-COMPLETED, do not restrict enrollment state to active."
  (absalon-get-all
   "courses"
   (unless include-completed '((enrollment_state "active")))))

(defun absalon-modules (course-id &optional include-items)
  "Return modules for COURSE-ID, optionally INCLUDE-ITEMS."
  (absalon-get-all
   (format "courses/%s/modules" course-id)
   (when include-items '(("include[]" "items")))))

(defun absalon-pages (course-id)
  "Return pages for COURSE-ID."
  (absalon-get-all (format "courses/%s/pages" course-id)))

(defun absalon-files (course-id)
  "Return files for COURSE-ID."
  (absalon-get-all (format "courses/%s/files" course-id)))

(defun absalon-page (course-id page-url)
  "Return PAGE-URL in COURSE-ID."
  (absalon-request
   (format "courses/%s/pages/%s"
           course-id (url-hexify-string page-url))))

(defun absalon--json-prepare (object)
  "Make parsed JSON OBJECT unambiguous to `json-serialize'.

Canvas objects are alists and Canvas arrays are lists.  Emacs cannot always
distinguish a list of alists from an alist, so convert array lists recursively
to vectors while retaining genuine alists as objects."
  (cond
   ((vectorp object)
    (apply #'vector (mapcar #'absalon--json-prepare object)))
   ((and (consp object)
         (seq-every-p
          (lambda (entry)
            (and (consp entry)
                 (or (symbolp (car entry)) (stringp (car entry)))))
          object))
    (mapcar (lambda (entry)
              (cons (car entry) (absalon--json-prepare (cdr entry))))
            object))
   ((consp object)
    (apply #'vector (mapcar #'absalon--json-prepare object)))
   (t object)))

(defun absalon--json-string (object)
  "Serialize parsed JSON OBJECT without list/alist ambiguity."
  (json-serialize (absalon--json-prepare object)
                  :null-object nil :false-object :false))

(defun absalon--display-json (object name)
  "Display OBJECT as pretty JSON in buffer NAME."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (absalon--json-string object))
        (json-pretty-print-buffer)
        (goto-char (point-min))
        (absalon--json-mode-if-needed)
        (view-mode 1)))
    (pop-to-buffer buffer)))

(defun absalon--json-mode-if-needed ()
  "Use `json-mode' if available, otherwise a suitable built-in mode."
  (cond ((fboundp 'json-mode) (json-mode))
        ((fboundp 'js-json-mode) (js-json-mode))
        (t (fundamental-mode))))

(defun absalon--read-course-id ()
  "Read a course ID, defaulting to Org metadata when available."
  (let ((default (and (derived-mode-p 'org-mode)
                      (absalon-org-keyword "ABSALON_COURSE_ID"))))
    (read-string (if default
                     (format "Course ID (default %s): " default)
                   "Course ID: ")
                 nil nil default)))

(defun absalon-show-profile ()
  "Show the current user's profile."
  (interactive)
  (absalon--display-json (absalon-profile) "*Absalon profile*"))

(defun absalon-list-courses (&optional include-completed)
  "Show courses; with prefix INCLUDE-COMPLETED include completed courses."
  (interactive "P")
  (absalon--display-json (absalon-courses include-completed)
                         "*Absalon courses*"))

(defun absalon-list-modules (course-id)
  "Prompt for COURSE-ID and show modules including items."
  (interactive (list (absalon--read-course-id)))
  (absalon--display-json (absalon-modules course-id t)
                         "*Absalon modules*"))

(defun absalon-list-pages (course-id)
  "Prompt for COURSE-ID and show pages."
  (interactive (list (absalon--read-course-id)))
  (absalon--display-json (absalon-pages course-id) "*Absalon pages*"))

(defun absalon-list-files (course-id)
  "Prompt for COURSE-ID and show files."
  (interactive (list (absalon--read-course-id)))
  (absalon--display-json (absalon-files course-id) "*Absalon files*"))

(defun absalon-org-keyword (key)
  "Return first value of Org file keyword KEY in current buffer."
  (let ((case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward
             (format "^[[:space:]]*#\\+%s:[[:space:]]*\\(.*?\\)[[:space:]]*$"
                     (regexp-quote key))
             nil t)
        (string-trim (match-string-no-properties 1))))))

(defun absalon-org-set-keyword (key value)
  "Set Org file keyword KEY to VALUE in current buffer."
  (let ((case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward
           (format "^[[:space:]]*#\\+%s:[[:space:]]*.*$" (regexp-quote key))
           nil t)
          (replace-match (format "#+%s: %s" key value) t t)
        (goto-char (point-min))
        (insert (format "#+%s: %s\n" key value))))))

(defun absalon--truthy-p (value)
  "Return non-nil when string VALUE denotes true."
  (and value (member (downcase value) '("t" "true" "yes" "1" "published"))))

(defun absalon-org-export-body ()
  "Export current Org buffer body to Canvas-friendly HTML."
  (unless (derived-mode-p 'org-mode)
    (user-error "Current buffer is not an Org buffer"))
  (let* ((org-export-with-title nil)
         (org-export-with-toc nil)
         (org-export-with-author nil)
         (org-export-with-date nil)
         (body (org-export-as 'html nil nil t
                              '(:with-title nil :with-toc nil
                                :with-author nil :with-date nil))))
    (concat (or absalon-org-html-preamble "")
            body
            (or absalon-org-html-postamble ""))))

(defun absalon-org-spec ()
  "Return an alist describing the current Org page."
  (unless (derived-mode-p 'org-mode)
    (user-error "Current buffer is not an Org buffer"))
  (let ((course-id (absalon-org-keyword "ABSALON_COURSE_ID"))
        (title (or (absalon-org-keyword "TITLE")
                   (and buffer-file-name (file-name-base buffer-file-name))))
        (page-url (absalon-org-keyword "ABSALON_PAGE_URL"))
        (module-id (absalon-org-keyword "ABSALON_MODULE_ID"))
        (position (absalon-org-keyword "ABSALON_POSITION"))
        (published (absalon-org-keyword "ABSALON_PUBLISHED")))
    (unless course-id (user-error "Missing #+ABSALON_COURSE_ID"))
    (unless title (user-error "Missing #+TITLE"))
    `((course-id . ,course-id)
      (title . ,title)
      (page-url . ,page-url)
      (module-id . ,module-id)
      (position . ,(and position (string-to-number position)))
      (published . ,(if (absalon--truthy-p published) t :false))
      (body . ,(absalon-org-export-body)))))

(defun absalon--html-normalize (html)
  "Normalize insignificant outer whitespace in HTML."
  (string-trim (or html "")))

(defun absalon-org-plan ()
  "Return a synchronization plan for the current Org page.

The plan has action `create', `update', or `unchanged'."
  (let* ((spec (absalon-org-spec))
         (course-id (alist-get 'course-id spec))
         (page-url (alist-get 'page-url spec))
         (remote (and page-url (absalon-page course-id page-url)))
         (changed
          (and remote
               (or (not (equal (alist-get 'title spec)
                               (alist-get 'title remote)))
                   (not (equal (absalon--html-normalize (alist-get 'body spec))
                               (absalon--html-normalize
                                (alist-get 'body remote))))
                   (not (eq (alist-get 'published spec)
                            (alist-get 'published remote)))))))
    `((action . ,(cond ((null remote) 'create)
                       (changed 'update)
                       (t 'unchanged)))
      (local . ,spec)
      (remote . ,remote))))

(defun absalon-org-preview-page ()
  "Preview local HTML and the proposed action without writing to Canvas."
  (interactive)
  (let* ((plan (absalon-org-plan))
         (local (alist-get 'local plan))
         (remote (alist-get 'remote plan))
         (buffer (get-buffer-create "*Absalon page preview*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Action: %s\nTitle: %s\nCourse: %s\nPage URL: %s\n\n"
                        (alist-get 'action plan)
                        (alist-get 'title local)
                        (alist-get 'course-id local)
                        (or (alist-get 'page-url local) "(new page)")))
        (insert "===== LOCAL HTML =====\n"
                (alist-get 'body local)
                "\n\n===== REMOTE HTML =====\n"
                (or (alist-get 'body remote) "(page does not exist)") "\n")
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buffer)))

(defun absalon-create-page (course-id title body &optional published)
  "Create a page in COURSE-ID with TITLE, BODY, and PUBLISHED state."
  (absalon--ensure-write (format "Create page %S" title))
  (absalon-request
   (format "courses/%s/pages" course-id) "POST"
   `(("wiki_page[title]" . ,title)
     ("wiki_page[body]" . ,body)
     ("wiki_page[published]" . ,(if published "true" "false")))))

(defun absalon-update-page (course-id page-url title body &optional published)
  "Update PAGE-URL in COURSE-ID with TITLE, BODY, and PUBLISHED state."
  (absalon--ensure-write (format "Update page %S" title))
  (absalon-request
   (format "courses/%s/pages/%s" course-id (url-hexify-string page-url))
   "PUT"
   `(("wiki_page[title]" . ,title)
     ("wiki_page[body]" . ,body)
     ("wiki_page[published]" . ,(if published "true" "false")))))

(defun absalon-module-items (course-id module-id)
  "Return all module items for MODULE-ID in COURSE-ID."
  (absalon-get-all
   (format "courses/%s/modules/%s/items" course-id module-id)))

(defun absalon-add-page-to-module (course-id module-id page-url
                                             &optional position)
  "Add PAGE-URL to MODULE-ID in COURSE-ID, optionally at POSITION."
  (absalon--ensure-write (format "Add page %s to module %s" page-url module-id))
  (absalon-request
   (format "courses/%s/modules/%s/items" course-id module-id) "POST"
   (append `(("module_item[type]" . "Page")
             ("module_item[page_url]" . ,page-url))
           (when position `(("module_item[position]" . ,position))))))

(defun absalon-position-module-item (course-id module-id item-id position)
  "Move ITEM-ID in MODULE-ID in COURSE-ID to one-based POSITION."
  (absalon--ensure-write
   (format "Move module item %s to position %s" item-id position))
  (absalon-request
   (format "courses/%s/modules/%s/items/%s" course-id module-id item-id)
   "PUT" `(("module_item[position]" . ,position))))

(defun absalon--module-page-item (course-id module-id page-url)
  "Find PAGE-URL item in MODULE-ID of COURSE-ID."
  (seq-find (lambda (item)
              (and (equal (alist-get 'type item) "Page")
                   (equal (alist-get 'page_url item) page-url)))
            (absalon-module-items course-id module-id)))

(defun absalon-org-push-page ()
  "Create or update the current Org page, then maintain module placement."
  (interactive)
  (let* ((plan (absalon-org-plan))
         (action (alist-get 'action plan))
         (spec (alist-get 'local plan))
         (course-id (alist-get 'course-id spec))
         (page-url (alist-get 'page-url spec))
         (title (alist-get 'title spec))
         (body (alist-get 'body spec))
         (published (eq (alist-get 'published spec) t))
         result)
    (unless (eq action 'unchanged)
      (absalon--confirm-write
       (format "%s Absalon page %S"
               (if (eq action 'create) "Create" "Update") title)))
    (when (and (eq action 'unchanged) (alist-get 'module-id spec))
      (absalon--confirm-write
       (format "Reconcile module placement for %S" title)))
    (pcase action
      ('unchanged (message "Absalon page is already up to date"))
      ('create
       (absalon--ensure-write (format "Create Absalon page %S" title))
       (setq result (absalon-create-page course-id title body published)
             page-url (alist-get 'url result))
       (unless page-url
         (signal 'absalon-error (list "Created page has no URL" result)))
       (absalon-org-set-keyword "ABSALON_PAGE_URL" page-url)
       (when buffer-file-name (save-buffer))
       (message "Created Absalon page %s" page-url))
      ('update
       (absalon--ensure-write (format "Update Absalon page %S" title))
       (setq result
             (absalon-update-page course-id page-url title body published))
       (message "Updated Absalon page %s" page-url)))
    (when-let* ((module-id (alist-get 'module-id spec)))
      (let* ((position (alist-get 'position spec))
             (item (absalon--module-page-item course-id module-id page-url)))
        (unless item
          (setq item (absalon-add-page-to-module
                      course-id module-id page-url position)))
        (when (and item position
                   (/= position (or (alist-get 'position item) -1)))
          (absalon-position-module-item
           course-id module-id (alist-get 'id item) position))))
    result))

(defun absalon--curl-multipart (upload-url upload-params file)
  "POST FILE and UPLOAD-PARAMS to Canvas UPLOAD-URL using curl.

Return decoded JSON.  The upload URL is supplied by Canvas and requires no API
token."
  (unless (executable-find "curl")
    (user-error "The curl executable is required for multipart uploads"))
  (let ((args (list "--silent" "--show-error" "--fail-with-body" "--location"
                    "--request" "POST"))
        (buffer (generate-new-buffer " *absalon-curl*")))
    (unwind-protect
        (progn
          (dolist (pair upload-params)
            (setq args
                  (append args
                          (list "--form-string"
                                (format "%s=%s" (car pair) (cdr pair))))))
          (setq args (append args
                             (list "--form" (concat "file=@" file)
                                   upload-url)))
          (let ((status (apply #'call-process "curl" nil buffer nil args)))
            (unless (zerop status)
              (signal 'absalon-http-error
                      (list "Multipart upload failed" status
                            (with-current-buffer buffer (buffer-string)))))
            (with-current-buffer buffer
              (json-parse-buffer :object-type 'alist :array-type 'list
                                 :null-object nil :false-object :false))))
      (kill-buffer buffer))))

(defun absalon-upload-file (course-id file &optional folder-id on-duplicate)
  "Upload FILE to COURSE-ID using Canvas's two-step protocol.

Optional FOLDER-ID selects a Canvas folder.  ON-DUPLICATE is normally
`overwrite', `rename', or nil (Canvas default).  Return the Canvas file object."
  (interactive
   (list (absalon--read-course-id)
         (read-file-name "File to upload: " nil nil t)))
  (setq file (expand-file-name file))
  (unless (file-regular-p file) (user-error "Not a regular file: %s" file))
  (when (called-interactively-p 'interactive)
    (absalon--confirm-write
     (format "Upload %s" (file-name-nondirectory file))))
  (absalon--ensure-write)
  (let* ((params `(("name" . ,(file-name-nondirectory file))
                   ("size" . ,(file-attribute-size (file-attributes file)))))
         (params (append params
                         (when folder-id `(("parent_folder_id" . ,folder-id)))
                         (when on-duplicate
                           `(("on_duplicate" . ,(symbol-name on-duplicate))))))
         (ticket (absalon-request
                  (format "courses/%s/files" course-id) "POST" params))
         (upload-url (alist-get 'upload_url ticket))
         (upload-params (alist-get 'upload_params ticket)))
    (unless (and upload-url upload-params)
      (signal 'absalon-error (list "Invalid upload ticket" ticket)))
    (absalon--curl-multipart upload-url upload-params file)))

(defun absalon-course-snapshot (course-id)
  "Return a read-only snapshot of teaching structure for COURSE-ID."
  `((generated-at . ,(format-time-string "%FT%T%z"))
    (course-id . ,course-id)
    (modules . ,(absalon-modules course-id t))
    (pages . ,(absalon-pages course-id))
    (files . ,(absalon-files course-id))))

(defun absalon-write-course-snapshot (course-id file)
  "Write a pretty JSON snapshot of COURSE-ID to FILE."
  (interactive
   (let ((id (absalon--read-course-id)))
     (list id (read-file-name "Write snapshot to: " nil
                              (format "absalon-course-%s.json" id)))))
  (let ((snapshot (absalon-course-snapshot course-id)))
    (with-temp-file file
      (insert (absalon--json-string snapshot))
      (json-pretty-print-buffer))
    (message "Wrote %s" (expand-file-name file))))

(provide 'absalon)

;;; absalon.el ends here
