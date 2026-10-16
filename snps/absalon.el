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
;;   M-x absalon-select-course
;;   M-x absalon-show-profile
;;   M-x absalon-list-courses
;;   M-x absalon-list-modules
;;   M-x absalon-list-pages
;;   M-x absalon-list-files
;;
;; Select a course, list its pages, then run `absalon-edit-page' on a row.
;; The Org editor pushes the page on save (C-x C-s), using the usual write
;; guard and confirmation settings.  `absalon-switch-course' changes course;
;; `absalon-refresh-course-list' refreshes the cached course choices.
;;
;; Create a module with `absalon-create-module'.  Start a page draft with
;; `absalon-org-new-page', then preview and push it from the Org buffer.
;; In list buffers, M creates a module and N starts a new Org page.
;; New pages are initially unpublished; set ABSALON_PUBLISHED to true to
;; publish when pushing.  A module and item position can be selected at creation.
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
;;   M-x absalon-org-pull-page
;;   M-x absalon-org-preview-page
;;   M-x absalon-org-push-page
;;
;; Upload a local attachment with `absalon-upload-file'.  The returned Canvas
;; file can then be linked from a page.  Uploads require the external `curl'
;; executable only for the final multipart transfer; other operations use
;; Emacs's built-in URL library.
;; The file view shows folders as a tree: RET/TAB toggles a folder, + creates
;; a folder, and u uploads to a folder selected by its full path.
;;
;; Caveats: Kaltura and other LTI tools have separate APIs.  This package does
;; not manage quizzes, submissions, enrolments, or student data.  Test writes
;; in a sandbox/unpublished course first.

;;; Code:

(require 'auth-source)
(require 'browse-url)
(require 'cl-lib)
(require 'dom)
(require 'json)
(require 'org)
(require 'ox-html)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
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

(defcustom absalon-write-enabled t
  "When non-nil, permit mutating API requests.

Interactive mutating commands still request confirmation."
  :type 'boolean)

(defcustom absalon-confirm-writes nil
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

(defvar absalon-courses nil
  "Cached course alists, populated by `absalon-refresh-course-list'.
The function named `absalon-courses' remains the uncached API reader.")

(defvar absalon-current-course nil
  "Selected course alist, or nil.  Set with `absalon-select-course'.")

(defvar-local absalon--page-identity nil
  "Cons of course ID and page URL identifying this page editor.")

(defvar-local absalon--pushing-page nil
  "Non-nil while pushing this buffer, to prevent recursive save pushes.")

(defvar-local absalon-view-kind nil
  "Kind of Absalon objects shown in the current list buffer.")
(defvar-local absalon-view-title nil
  "Human-readable title for the current Absalon view buffer.")
(defvar-local absalon-view-items nil
  "Absalon objects backing the current list buffer.")
(defvar-local absalon-view-context nil
  "Context alist for the current Absalon view buffer.")
(defvar-local absalon-view-object-table nil
  "Alist mapping tabulated-list row IDs to Absalon objects.")
(defvar-local absalon-view-refresh-function nil
  "Function used to refresh the current Absalon view buffer.")
(defvar-local absalon-view-refresh-args nil
  "Arguments passed to `absalon-view-refresh-function'.")

(defvar-local absalon--file-folders nil
  "Folder objects backing the current file tree.")
(defvar-local absalon--file-objects nil
  "File objects backing the current file tree.")
(defvar-local absalon--collapsed-folders nil
  "Folder ID strings collapsed in the current file tree.")

(defvar-local absalon-detail-object nil
  "Absalon object backing the current detail buffer.")
(defvar-local absalon-detail-context nil
  "Context alist for the current detail buffer.")
(defvar-local absalon-detail-refresh-function nil
  "Function used to refresh the current detail buffer.")
(defvar-local absalon-detail-refresh-args nil
  "Arguments passed to `absalon-detail-refresh-function'.")

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

(defun absalon-refresh-course-list (&optional include-completed)
  "Refresh and return the cached `absalon-courses'.
With prefix INCLUDE-COMPLETED, also include completed courses.
Update the selected course record, or clear it if it is no longer listed."
  (interactive "P")
  (let ((courses (absalon-courses include-completed))
        (selected-id (absalon--object-id-string absalon-current-course 'id)))
    (setq absalon-courses courses
          absalon-current-course
          (seq-find (lambda (course)
                      (equal selected-id (absalon--object-id-string course 'id)))
                    courses)))
  (when (called-interactively-p 'interactive)
    (message "Refreshed %d Absalon courses" (length absalon-courses)))
  absalon-courses)

(defun absalon-select-course (&optional course-id)
  "Select COURSE-ID from the course cache and return its course alist.
Interactively, or without COURSE-ID, offer course-name completion.
Fetch courses automatically when the cache is empty."
  (interactive)
  (let* ((id (if course-id
                (progn
                  (unless absalon-courses (absalon-refresh-course-list))
                  (format "%s" course-id))
               (absalon--select-course-id)))
         (course (seq-find
                  (lambda (course)
                    (equal id (absalon--object-id-string course 'id)))
                  absalon-courses)))
    (unless course (user-error "No Absalon course with ID %s" id))
    (setq absalon-current-course course)
    (message "Selected Absalon course: %s [%s]"
             (or (absalon--get 'name course) (absalon--get 'course_code course)) id)
    course))

(defun absalon-switch-course (&optional course-id)
  "Select another COURSE-ID and reopen the current course list view.
Interactively, offer course-name completion.  Existing Org editors remain
associated with their own course, even after switching."
  (interactive)
  (let* ((kind absalon-view-kind)
         (course (absalon-select-course course-id))
         (id (absalon--object-id-string course 'id)))
    (pcase kind
      ((or 'modules 'module-items) (absalon--show-modules id))
      ('pages (absalon--show-pages id))
      ('files (absalon--show-files id)))
    course))

(defun absalon--require-course-id (&optional course-id)
  "Return COURSE-ID, the selected course ID, or prompt for a course."
  (or course-id
      (absalon--string-or-nil (absalon--get 'id absalon-current-course))
      (absalon--object-id-string (absalon-select-course) 'id)))

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

(defun absalon-folders (course-id)
  "Return all folders in COURSE-ID, including empty folders."
  (absalon-get-all (format "courses/%s/folders" course-id)))

(defun absalon--folder-path (folder)
  "Return a display path for FOLDER."
  (absalon--object-id-string folder 'full_name 'name 'id))

(defun absalon--folder-at-point ()
  "Return the folder ID for the current tree row, if any."
  (when (and (derived-mode-p 'absalon-files-mode) (tabulated-list-get-id))
    (let ((object (absalon-view-current-object)))
      (absalon--object-id-string
       object (if (string-prefix-p "folder:" (tabulated-list-get-id))
                  'id 'folder_id)))))

(defun absalon--read-folder-id (course-id &optional prompt)
  "Select a folder path in COURSE-ID using PROMPT, and return its ID.
Default to the folder at point when browsing the same course."
  (let* ((folders (absalon-folders course-id))
         (choices (mapcar (lambda (folder)
                            (cons (format "%s/ [%s]" (absalon--folder-path folder)
                                          (absalon--get 'id folder))
                                  (absalon--object-id-string folder 'id)))
                          folders))
         (current (and (equal (format "%s" course-id) (absalon--view-course-id))
                       (absalon--folder-at-point)))
         (root (seq-find (lambda (folder)
                           (null (absalon--get 'parent_folder_id folder)))
                         folders))
         (default (car (rassoc (or current
                                  (and root (absalon--object-id-string root 'id)))
                              choices))))
    (unless choices (user-error "No folders available in course %s" course-id))
    (or (cdr (assoc (completing-read (or prompt "Folder: ") choices nil t
                                    nil nil default)
                    choices))
        (user-error "No folder selected"))))

(defun absalon--file-command-course-id ()
  "Return the current view or Org page's course ID, or the selected course."
  (absalon--require-course-id
   (or (and (derived-mode-p 'absalon-list-mode)
            (absalon--view-course-id
             (and (tabulated-list-get-id) (absalon-view-current-object))))
       (and (derived-mode-p 'org-mode)
            (absalon-org-keyword "ABSALON_COURSE_ID"))
       (absalon--get 'course-id absalon-detail-context))))

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

(defconst absalon--courses-format
  [("ID" 10 t)
   ("Name" 36 t)
   ("Code" 18 t)
   ("State" 12 t)
   ("Start" 16 t)
   ("End" 16 t)])

(defconst absalon--modules-format
  [("Pos" 5 t)
   ("ID" 10 t)
   ("Name" 40 t)
   ("Published" 10 t)
   ("Items" 7 t)
   ("State" 12 t)
   ("Unlock" 16 t)])

(defconst absalon--module-items-format
  [("Pos" 5 t)
   ("Type" 14 t)
   ("Title" 42 t)
   ("Published" 10 t)
   ("Indent" 6 t)
   ("Target" 32 t)])

(defconst absalon--pages-format
  [("URL" 28 t)
   ("Title" 42 t)
   ("Published" 10 t)
   ("Front" 7 t)
   ("Updated" 16 t)
   ("Editors" 16 t)])

(defconst absalon--files-format
  [("Name" 54 nil)
   ("Kind" 8 nil)
   ("Size" 10 nil)
   ("Updated" 16 nil)
   ("ID" 10 nil)
   ("Type" 24 nil)])

(defvar absalon-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'absalon-view-open)
    (define-key map (kbd "b") #'absalon-view-browse)
    (define-key map (kbd "e") #'absalon-edit-page)
    (define-key map (kbd "f") #'absalon-view-list-files)
    (define-key map (kbd "g") #'absalon-view-refresh)
    (define-key map (kbd "i") #'absalon-view-list-module-items)
    (define-key map (kbd "j") #'absalon-view-json-at-point)
    (define-key map (kbd "J") #'absalon-view-json-all)
    (define-key map (kbd "m") #'absalon-view-list-modules)
    (define-key map (kbd "M") #'absalon-create-module)
    (define-key map (kbd "N") #'absalon-org-new-page)
    (define-key map (kbd "p") #'absalon-view-list-pages)
    (define-key map (kbd "s") #'absalon-view-write-snapshot)
    (define-key map (kbd "v") #'absalon-view-show-page)
    map)
  "Keymap for `absalon-list-mode'.")

(define-derived-mode absalon-list-mode tabulated-list-mode "Absalon"
  "Browse Absalon objects returned by Canvas list endpoints."
  (setq truncate-lines t
        tabulated-list-padding 2))

(defvar absalon-files-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'absalon-files-open)
    (define-key map (kbd "TAB") #'absalon-files-toggle-folder)
    (define-key map (kbd "+") #'absalon-create-folder)
    (define-key map (kbd "u") #'absalon-upload-file)
    map)
  "Keymap for `absalon-files-mode'.")

(define-derived-mode absalon-files-mode absalon-list-mode "Absalon-Files"
  "Browse course folders and files as an expandable tree.
Use RET or TAB to toggle a folder, + to create a folder, and u to upload.")

(defvar absalon-detail-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "b") #'absalon-detail-browse)
    (define-key map (kbd "e") #'absalon-edit-page)
    (define-key map (kbd "g") #'absalon-detail-refresh)
    (define-key map (kbd "j") #'absalon-detail-json)
    map)
  "Keymap for `absalon-detail-mode'.")

(define-derived-mode absalon-detail-mode special-mode "Absalon-Detail"
  "Show a human-readable Absalon object detail buffer.")

(defun absalon--get (key object &optional default)
  "Return KEY from parsed JSON OBJECT alist, or DEFAULT."
  (if-let* ((entry (and (listp object)
                        (or (assoc key object)
                            (and (symbolp key)
                                 (assoc (symbol-name key) object))
                            (and (stringp key)
                                 (let ((symbol (intern-soft key)))
                                   (and symbol (assoc symbol object))))))))
      (cdr entry)
    default))

(defun absalon--format-value (value &optional empty)
  "Return a compact display string for VALUE, using EMPTY for nil."
  (cond
   ((null value) (or empty ""))
   ((eq value :false) "false")
   ((eq value t) "true")
   ((stringp value) value)
   ((numberp value) (number-to-string value))
   ((symbolp value) (symbol-name value))
   ((and (listp value)
         (seq-every-p (lambda (item) (not (consp item))) value))
    (mapconcat (lambda (item) (absalon--format-value item)) value ", "))
   (t (format "%S" value))))

(defun absalon--string-or-nil (value)
  "Return VALUE formatted as a non-empty string, or nil."
  (when value
    (let ((text (absalon--format-value value)))
      (unless (string-empty-p text) text))))

(defun absalon--format-bool (value)
  "Return VALUE as a human-readable boolean string."
  (cond
   ((eq value t) "yes")
   ((eq value :false) "no")
   ((null value) "")
   ((stringp value)
    (let ((downcased (downcase value)))
      (cond ((member downcased '("true" "yes" "1" "published")) "yes")
            ((member downcased '("false" "no" "0" "unpublished")) "no")
            (t value))))
   (t (absalon--format-value value))))

(defun absalon--format-time (value)
  "Return ISO timestamp VALUE as a compact local time string."
  (if (and (stringp value) (not (string-empty-p value)))
      (condition-case nil
          (format-time-string "%Y-%m-%d %H:%M" (date-to-time value))
        (error value))
    ""))

(defun absalon--format-bytes (value)
  "Return byte count VALUE as a compact human-readable size."
  (let ((bytes (cond ((numberp value) value)
                     ((and (stringp value) (not (string-empty-p value)))
                      (string-to-number value)))))
    (cond
     ((null bytes) "")
     ((< bytes 1024) (format "%d B" bytes))
     ((< bytes 1048576) (format "%.1f KB" (/ bytes 1024.0)))
     ((< bytes 1073741824) (format "%.1f MB" (/ bytes 1048576.0)))
     (t (format "%.1f GB" (/ bytes 1073741824.0))))))

(defun absalon--object-id-string (object &rest keys)
  "Return first non-empty value for KEYS in OBJECT as a string."
  (or (seq-some
       (lambda (key) (absalon--string-or-nil (absalon--get key object)))
       keys)
      ""))

(defun absalon--list-row-id (kind object index &rest keys)
  "Return a stable row ID for KIND, OBJECT, and fallback INDEX."
  (let ((id (apply #'absalon--object-id-string object keys)))
    (format "%s:%s" kind (if (string-empty-p id) index id))))

(defun absalon--view-kind-name (kind)
  "Return a human-readable name for view KIND."
  (pcase kind
    ('courses "course")
    ('modules "module")
    ('module-items "module item")
    ('pages "page")
    ('files "file")
    (_ "item")))

(defun absalon--base-browser-url ()
  "Return `absalon-base-url' without a trailing slash."
  (string-remove-suffix "/" absalon-base-url))

(defun absalon--absolute-url (url)
  "Return URL made absolute against `absalon-base-url' when needed."
  (when-let* ((url (absalon--string-or-nil url)))
    (cond ((string-match-p "\\`https?://" url) url)
          ((string-prefix-p "/" url)
           (concat (absalon--base-browser-url) url))
          (t url))))

(defun absalon--course-browser-url (course-id)
  "Return browser URL for COURSE-ID."
  (when-let* ((course-id (absalon--string-or-nil course-id)))
    (concat (absalon--base-browser-url)
            "/courses/" (url-hexify-string course-id))))

(defun absalon--page-browser-url (course-id page-url)
  "Return browser URL for PAGE-URL in COURSE-ID."
  (when-let* ((course-url (absalon--course-browser-url course-id))
              (page-url (absalon--string-or-nil page-url)))
    (concat course-url "/pages/" (url-hexify-string page-url))))

(defun absalon--module-browser-url (course-id module-id)
  "Return browser URL for MODULE-ID in COURSE-ID."
  (when-let* ((course-url (absalon--course-browser-url course-id))
              (module-id (absalon--string-or-nil module-id)))
    (concat course-url "/modules#module_" (url-hexify-string module-id))))

(defun absalon--view-context-value (key)
  "Return KEY from `absalon-view-context'."
  (absalon--get key absalon-view-context))

(defun absalon--view-course-id (&optional object)
  "Return the course ID implied by OBJECT or the current view context."
  (or (absalon--string-or-nil (absalon--view-context-value 'course-id))
      (absalon--string-or-nil (absalon--get 'course_id object))
      (and (eq absalon-view-kind 'courses)
           (absalon--string-or-nil (absalon--get 'id object)))))

(defun absalon--view-module-id (&optional object)
  "Return the module ID implied by OBJECT or the current view context."
  (or (absalon--string-or-nil (absalon--view-context-value 'module-id))
      (absalon--string-or-nil (absalon--get 'module_id object))
      (and (eq absalon-view-kind 'modules)
           (absalon--string-or-nil (absalon--get 'id object)))))

(defun absalon--object-browser-url (kind object context)
  "Return a Canvas browser URL for KIND OBJECT using CONTEXT."
  (let* ((course-id (or (absalon--string-or-nil (absalon--get 'course-id context))
                        (absalon--string-or-nil (absalon--get 'course_id object))
                        (and (eq kind 'courses)
                             (absalon--string-or-nil (absalon--get 'id object)))))
         (module-id (or (absalon--string-or-nil (absalon--get 'module-id context))
                        (absalon--string-or-nil (absalon--get 'module_id object))
                        (and (eq kind 'modules)
                             (absalon--string-or-nil (absalon--get 'id object)))))
         (page-url (or (absalon--string-or-nil (absalon--get 'page_url object))
                       (and (eq kind 'pages)
                            (absalon--string-or-nil (absalon--get 'url object))))))
    (or (absalon--absolute-url (absalon--get 'html_url object))
        (absalon--absolute-url (absalon--get 'external_url object))
        (pcase kind
          ('courses (absalon--course-browser-url course-id))
          ('modules (absalon--module-browser-url course-id module-id))
          ('module-items
           (cond ((and page-url course-id)
                  (absalon--page-browser-url course-id page-url))
                 (t nil)))
          ('pages (absalon--page-browser-url course-id page-url))
          ('files (absalon--absolute-url (absalon--get 'url object)))
          (_ nil)))))

(defun absalon--course-row (course index)
  "Return tabulated-list row for COURSE at INDEX."
  (list (absalon--list-row-id 'course course index 'id)
        (vector (absalon--object-id-string course 'id)
                (absalon--object-id-string course 'name)
                (absalon--object-id-string course 'course_code)
                (absalon--object-id-string course 'workflow_state)
                (absalon--format-time (absalon--get 'start_at course))
                (absalon--format-time (absalon--get 'end_at course)))))

(defun absalon--module-row (module index)
  "Return tabulated-list row for MODULE at INDEX."
  (let* ((items (absalon--get 'items module))
         (items-count (if items
                          (number-to-string (length items))
                        (absalon--format-value
                         (absalon--get 'items_count module)))))
    (list (absalon--list-row-id 'module module index 'id)
          (vector (absalon--object-id-string module 'position)
                  (absalon--object-id-string module 'id)
                  (absalon--object-id-string module 'name)
                  (absalon--format-bool (absalon--get 'published module))
                  items-count
                  (absalon--object-id-string module 'workflow_state)
                  (absalon--format-time (absalon--get 'unlock_at module))))))

(defun absalon--module-item-row (item index)
  "Return tabulated-list row for module ITEM at INDEX."
  (list (absalon--list-row-id 'module-item item index 'id 'page_url 'content_id)
        (vector (absalon--object-id-string item 'position)
                (absalon--object-id-string item 'type)
                (absalon--object-id-string item 'title)
                (absalon--format-bool (absalon--get 'published item))
                (absalon--object-id-string item 'indent)
                (or (absalon--string-or-nil (absalon--get 'page_url item))
                    (absalon--string-or-nil (absalon--get 'external_url item))
                    (absalon--object-id-string item 'content_id)))))

(defun absalon--page-row (page index)
  "Return tabulated-list row for PAGE at INDEX."
  (list (absalon--list-row-id 'page page index 'url 'page_id 'id)
        (vector (absalon--object-id-string page 'url 'page_id 'id)
                (absalon--object-id-string page 'title)
                (absalon--format-bool (absalon--get 'published page))
                (absalon--format-bool (absalon--get 'front_page page))
                (absalon--format-time (absalon--get 'updated_at page))
                (absalon--format-value (absalon--get 'editing_roles page)))))

(defun absalon--file-row (file index &optional depth)
  "Return tabulated-list row for FILE at INDEX, indented to DEPTH."
  (list (absalon--list-row-id 'file file index 'id 'display_name 'filename)
        (vector (concat (make-string (* 2 (or depth 0)) ?\s) "    "
                        (absalon--object-id-string file 'display_name 'filename))
                "File"
                (absalon--format-bytes (absalon--get 'size file))
                (absalon--format-time (or (absalon--get 'updated_at file)
                                          (absalon--get 'modified_at file)))
                (absalon--object-id-string file 'id)
                (absalon--object-id-string file 'content-type 'content_type))))

(defun absalon--file-tree-data (folders files collapsed)
  "Return (ENTRIES . OBJECT-TABLE) for FOLDERS, FILES and COLLAPSED IDs.
Keep original API objects in the object table for row actions and JSON."
  (let ((by-id (make-hash-table :test #'equal))
        (children (make-hash-table :test #'equal))
        (contents (make-hash-table :test #'equal))
        (visited (make-hash-table :test #'equal))
        (folders (sort (copy-sequence folders)
                       (lambda (a b) (string-lessp (downcase (absalon--folder-path a))
                                                  (downcase (absalon--folder-path b))))))
        (files (sort (copy-sequence files)
                     (lambda (a b)
                       (string-lessp (downcase (absalon--object-id-string a 'display_name 'filename))
                                     (downcase (absalon--object-id-string b 'display_name 'filename))))))
        entries table)
    (dolist (folder folders)
      (puthash (absalon--object-id-string folder 'id) folder by-id)
      (push folder (gethash (absalon--object-id-string folder 'parent_folder_id)
                            children)))
    (dolist (file files)
      (push file (gethash (absalon--object-id-string file 'folder_id) contents)))
    (cl-labels
        ((emit-file (file depth)
           (let ((row (absalon--file-row file 0 depth)))
             (push row entries)
             (push (cons (car row) file) table)))
         (walk (folder depth visible)
           (let ((id (absalon--object-id-string folder 'id)))
             (unless (gethash id visited)
               (puthash id t visited)
               (when visible
                 (let ((row-id (concat "folder:" id)))
                   (push (list row-id
                               (vector (concat (make-string (* 2 depth) ?\s)
                                               (if (member id collapsed) "[+] " "[-] ")
                                               (absalon--object-id-string folder 'name) "/")
                                       "Folder" ""
                                       (absalon--format-time (absalon--get 'updated_at folder))
                                       id "")) entries)
                   (push (cons row-id folder) table)))
               (let ((show (and visible (not (member id collapsed)))))
                 (dolist (child (reverse (gethash id children)))
                   (walk child (1+ depth) show))
                 (when show
                   (dolist (file (reverse (gethash id contents)))
                     (emit-file file (1+ depth)))))))))
      (dolist (folder folders)
        (unless (gethash (absalon--object-id-string folder 'parent_folder_id) by-id)
          (walk folder 0 t)))
      ;; Missing parents and cyclic API data must not hide accessible objects.
      (dolist (folder folders)
        (unless (gethash (absalon--object-id-string folder 'id) visited)
          (walk folder 0 t)))
      (dolist (file files)
        (unless (gethash (absalon--object-id-string file 'folder_id) by-id)
          (emit-file file 0))))
    (cons (nreverse entries) (nreverse table))))

(defun absalon--render-file-tree ()
  "Render the cached file tree, preserving the selected row when possible."
  (let ((data (absalon--file-tree-data absalon--file-folders absalon--file-objects
                                      absalon--collapsed-folders)))
    (setq tabulated-list-entries (car data)
          absalon-view-object-table (cdr data))
    (tabulated-list-print t)))

(defun absalon-files-toggle-folder ()
  "Expand or collapse the folder at point without another API request."
  (interactive)
  (let ((row-id (tabulated-list-get-id)))
    (unless (and row-id (string-prefix-p "folder:" row-id))
      (user-error "No folder on this line"))
    (let ((id (absalon--object-id-string (absalon-view-current-object) 'id)))
      (setq absalon--collapsed-folders
            (if (member id absalon--collapsed-folders)
                (remove id absalon--collapsed-folders)
              (cons id absalon--collapsed-folders)))
      (absalon--render-file-tree))))

(defun absalon-files-open ()
  "Toggle the folder at point, or open the selected file in a browser."
  (interactive)
  (if (string-prefix-p "folder:" (or (tabulated-list-get-id) ""))
      (absalon-files-toggle-folder)
    (absalon-view-browse)))

(defun absalon--display-list (kind title items format row-function
                                   refresh-function refresh-args
                                   &optional context sort-key)
  "Display ITEMS in a human-readable Absalon list buffer."
  (let ((buffer (get-buffer-create (format "*Absalon %s*" title)))
        entries table
        (index 0))
    (dolist (object items)
      (let* ((row (funcall row-function object index))
             (id (nth 0 row))
             (columns (nth 1 row)))
        (push (list id columns) entries)
        (push (cons id object) table)
        (setq index (1+ index))))
    (with-current-buffer buffer
      (absalon-list-mode)
      (setq absalon-view-kind kind
            absalon-view-title title
            absalon-view-items items
            absalon-view-context context
            absalon-view-object-table (nreverse table)
            absalon-view-refresh-function refresh-function
            absalon-view-refresh-args refresh-args
            tabulated-list-format format
            tabulated-list-entries (nreverse entries)
            tabulated-list-sort-key sort-key
            tabulated-list-padding 2
            truncate-lines t)
      (tabulated-list-init-header)
      (tabulated-list-print t)
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

(defun absalon-view-current-object ()
  "Return the Absalon object represented by the row at point."
  (let ((id (tabulated-list-get-id)))
    (unless id
      (user-error "No Absalon item on this line"))
    (or (cdr (assoc id absalon-view-object-table))
        (user-error "No Absalon item on this line"))))

(defun absalon-view-json-at-point ()
  "Show raw JSON for the Absalon object at point."
  (interactive)
  (absalon--display-json
   (absalon-view-current-object)
   (format "*Absalon %s JSON*" (absalon--view-kind-name absalon-view-kind))))

(defun absalon-view-json-all ()
  "Show raw JSON for all objects in the current Absalon list buffer."
  (interactive)
  (unless (derived-mode-p 'absalon-list-mode)
    (user-error "This is not an Absalon list buffer"))
  (absalon--display-json
   absalon-view-items
   (format "*Absalon %s JSON*" (or absalon-view-title "response"))))

(defun absalon-view-refresh ()
  "Refresh the current Absalon list buffer."
  (interactive)
  (unless absalon-view-refresh-function
    (user-error "This Absalon buffer cannot be refreshed"))
  (apply absalon-view-refresh-function absalon-view-refresh-args))

(defun absalon-view-browse ()
  "Open the Canvas URL for the Absalon object at point."
  (interactive)
  (when (and (derived-mode-p 'absalon-files-mode)
             (string-prefix-p "folder:" (or (tabulated-list-get-id) "")))
    (user-error "Use RET to expand or collapse this folder"))
  (let* ((object (absalon-view-current-object))
         (url (absalon--object-browser-url
               absalon-view-kind object absalon-view-context)))
    (unless url
      (user-error "No Canvas browser URL for this item"))
    (browse-url url)))

(defun absalon-view-list-modules ()
  "List modules for the course represented by this buffer or row."
  (interactive)
  (let* ((object (ignore-errors (absalon-view-current-object)))
         (course-id (absalon--view-course-id object)))
    (unless course-id
      (user-error "No course ID for this item"))
    (absalon--show-modules course-id)))

(defun absalon-view-list-pages ()
  "List pages for the course represented by this buffer or row."
  (interactive)
  (let* ((object (ignore-errors (absalon-view-current-object)))
         (course-id (absalon--view-course-id object)))
    (unless course-id
      (user-error "No course ID for this item"))
    (absalon--show-pages course-id)))

(defun absalon-view-list-files ()
  "List files for the course represented by this buffer or row."
  (interactive)
  (let* ((object (ignore-errors (absalon-view-current-object)))
         (course-id (absalon--view-course-id object)))
    (unless course-id
      (user-error "No course ID for this item"))
    (absalon--show-files course-id)))

(defun absalon-view-list-module-items ()
  "List items for the module represented by this buffer or row."
  (interactive)
  (let* ((object (absalon-view-current-object))
         (course-id (absalon--view-course-id object))
         (module-id (absalon--view-module-id object))
         (items (absalon--get 'items object))
         (module-name (absalon--get 'name object)))
    (unless course-id
      (user-error "No course ID for this item"))
    (unless module-id
      (user-error "No module ID for this item"))
    (absalon--show-module-items course-id module-id items module-name)))

(defun absalon-view-show-page ()
  "Show a readable detail buffer for the page represented by the row at point."
  (interactive)
  (let* ((object (absalon-view-current-object))
         (course-id (absalon--view-course-id object))
         (page-url (or (absalon--string-or-nil (absalon--get 'page_url object))
                       (and (eq absalon-view-kind 'pages)
                            (absalon--string-or-nil
                             (absalon--get 'url object))))))
    (unless course-id
      (user-error "No course ID for this item"))
    (unless page-url
      (user-error "This item is not an Absalon page"))
    (absalon--show-page-detail course-id page-url)))

(defun absalon-view-edit-page-as-org ()
  "Pull the page represented by the row at point into an Org buffer."
  (interactive)
  (let* ((object (absalon-view-current-object))
         (course-id (absalon--view-course-id object))
         (page-url (or (absalon--string-or-nil (absalon--get 'page_url object))
                       (and (eq absalon-view-kind 'pages)
                            (absalon--string-or-nil
                             (absalon--get 'url object))))))
    (unless course-id
      (user-error "No course ID for this item"))
    (unless page-url
      (user-error "This item is not an Absalon page"))
    (absalon--open-page-org-buffer course-id page-url)))

(defun absalon-view-write-snapshot (file)
  "Write a JSON course snapshot for the course represented by this row."
  (interactive
   (let* ((object (ignore-errors (absalon-view-current-object)))
          (course-id (absalon--view-course-id object)))
     (unless course-id
       (user-error "No course ID for this item"))
     (list (read-file-name "Write snapshot to: " nil
                           (format "absalon-course-%s.json" course-id)))))
  (let* ((object (ignore-errors (absalon-view-current-object)))
         (course-id (absalon--view-course-id object)))
    (unless course-id
      (user-error "No course ID for this item"))
    (absalon-write-course-snapshot course-id file)))

(defun absalon-view-open ()
  "Run the default action for the Absalon row at point.
Pages open in the Org editor; use `absalon-view-show-page' for HTML details."
  (interactive)
  (pcase absalon-view-kind
    ('courses (absalon-view-list-modules))
    ('modules (absalon-view-list-module-items))
    ('pages (absalon-edit-page))
    ('module-items
     (let* ((object (absalon-view-current-object))
            (type (absalon--get 'type object)))
       (cond ((and (equal type "Page")
                   (absalon--string-or-nil (absalon--get 'page_url object)))
              (absalon-edit-page))
             ((absalon--object-browser-url
               absalon-view-kind object absalon-view-context)
              (absalon-view-browse))
             (t (absalon-view-json-at-point)))))
    ('files (absalon-files-open))
    (_ (absalon-view-json-at-point))))

(defun absalon--detail-browser-url ()
  "Return browser URL for the current detail buffer."
  (absalon--object-browser-url
   (absalon--get 'kind absalon-detail-context)
   absalon-detail-object absalon-detail-context))

(defun absalon-detail-browse ()
  "Open the Canvas URL for the current detail buffer."
  (interactive)
  (let ((url (absalon--detail-browser-url)))
    (unless url
      (user-error "No Canvas browser URL for this item"))
    (browse-url url)))

(defun absalon-detail-json ()
  "Show raw JSON for the current detail buffer."
  (interactive)
  (unless absalon-detail-object
    (user-error "This buffer has no Absalon object"))
  (absalon--display-json absalon-detail-object "*Absalon detail JSON*"))

(defun absalon-detail-edit-page-as-org ()
  "Pull the current page detail into an Org buffer."
  (interactive)
  (let ((course-id (absalon--string-or-nil
                    (absalon--get 'course-id absalon-detail-context)))
        (page-url (absalon--string-or-nil
                   (absalon--get 'page-url absalon-detail-context))))
    (unless (and course-id page-url)
      (user-error "This detail buffer is not an Absalon page"))
    (absalon--open-page-org-buffer
     course-id page-url absalon-detail-object)))

(defun absalon-detail-refresh ()
  "Refresh the current detail buffer."
  (interactive)
  (unless absalon-detail-refresh-function
    (user-error "This Absalon detail buffer cannot be refreshed"))
  (apply absalon-detail-refresh-function absalon-detail-refresh-args))

(defun absalon--show-courses (include-completed)
  "Show courses in a human-readable list buffer."
  (absalon--display-list
   'courses
   (if include-completed "courses (all)" "courses")
   (absalon-refresh-course-list include-completed)
   absalon--courses-format
   #'absalon--course-row
   #'absalon--show-courses
   (list include-completed)
   nil
   '("Name" . nil)))

(defun absalon--show-modules (course-id)
  "Show modules for COURSE-ID in a human-readable list buffer."
  (absalon--display-list
   'modules
   (format "course %s modules" course-id)
   (absalon-modules course-id t)
   absalon--modules-format
   #'absalon--module-row
   #'absalon--show-modules
   (list course-id)
   `((course-id . ,course-id))))

(defun absalon--show-module-items (course-id module-id &optional items module-name)
  "Show ITEMS for MODULE-ID in COURSE-ID in a human-readable list buffer."
  (absalon--display-list
   'module-items
   (format "course %s module %s items" course-id module-id)
   (or items (absalon-module-items course-id module-id))
   absalon--module-items-format
   #'absalon--module-item-row
   #'absalon--show-module-items
   (list course-id module-id nil module-name)
   `((course-id . ,course-id)
     (module-id . ,module-id)
     (module-name . ,module-name))))

(defun absalon--show-pages (course-id)
  "Show pages for COURSE-ID in a human-readable list buffer."
  (absalon--display-list
   'pages
   (format "course %s pages" course-id)
   (absalon-pages course-id)
   absalon--pages-format
   #'absalon--page-row
   #'absalon--show-pages
   (list course-id)
   `((course-id . ,course-id))
   '("Title" . nil)))

(defun absalon--show-files (course-id)
  "Show folders and files for COURSE-ID as an expandable tree."
  (let* ((folders (absalon-folders course-id))
         (files (absalon-files course-id))
         (title (format "course %s files" course-id))
         (buffer (get-buffer-create (format "*Absalon %s*" title))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'absalon-files-mode)
        (absalon-files-mode))
      (setq absalon-view-kind 'files
            absalon-view-title title
            absalon-view-context `((course-id . ,course-id))
            absalon-view-items `((folders . ,(vconcat folders))
                                 (files . ,(vconcat files)))
            absalon-view-refresh-function #'absalon--show-files
            absalon-view-refresh-args (list course-id)
            absalon--file-folders folders
            absalon--file-objects files
            tabulated-list-format absalon--files-format
            tabulated-list-sort-key nil)
      (tabulated-list-init-header)
      (absalon--render-file-tree))
    (pop-to-buffer buffer)))

(defun absalon--refresh-files-view (course-id)
  "Refresh a file tree for COURSE-ID when it is the current buffer."
  (when (and (derived-mode-p 'absalon-files-mode)
             (equal (format "%s" course-id) (absalon--view-course-id)))
    (absalon-view-refresh)))

(defun absalon--show-page-detail (course-id page-url)
  "Show PAGE-URL in COURSE-ID as a readable detail buffer."
  (let* ((page (absalon-page course-id page-url))
         (title (or (absalon--string-or-nil (absalon--get 'title page))
                    page-url))
         (buffer (get-buffer-create (format "*Absalon page: %s*" title)))
         (browser-url (absalon--page-browser-url course-id page-url))
         (body (or (absalon--get 'body page) "")))
    (with-current-buffer buffer
      (absalon-detail-mode)
      (setq absalon-detail-object page
            absalon-detail-context `((kind . pages)
                                     (course-id . ,course-id)
                                     (page-url . ,page-url))
            absalon-detail-refresh-function #'absalon--show-page-detail
            absalon-detail-refresh-args (list course-id page-url))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert title "\n" (make-string (length title) ?=) "\n\n")
        (dolist (line `(("Course" . ,course-id)
                        ("Page URL" . ,page-url)
                        ("Published" . ,(absalon--format-bool
                                          (absalon--get 'published page)))
                        ("Front page" . ,(absalon--format-bool
                                           (absalon--get 'front_page page)))
                        ("Updated" . ,(absalon--format-time
                                        (absalon--get 'updated_at page)))
                        ("Canvas" . ,(or browser-url ""))))
          (insert (car line) ": " (cdr line) "\n"))
        (insert "\nHTML\n\n" body)
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

(defun absalon--read-course-id ()
  "Read a course ID, defaulting to Org metadata or the current Absalon view."
  (let ((default
         (or (and (derived-mode-p 'org-mode)
                  (absalon-org-keyword "ABSALON_COURSE_ID"))
             (and (derived-mode-p 'absalon-list-mode)
                  (absalon--view-course-id
                   (and (tabulated-list-get-id)
                        (absalon-view-current-object))))
             (absalon--get 'course-id absalon-detail-context)
             (absalon--string-or-nil
              (absalon--get 'id absalon-current-course)))))
    (read-string (if default
                     (format "Course ID (default %s): " default)
                   "Course ID: ")
                 nil nil default)))

(defun absalon--select-course-id ()
  "Select a cached course by name and return its ID as a string."
  (unless absalon-courses (absalon-refresh-course-list))
  (let* ((choices
         (mapcar (lambda (course)
                   (let ((id (absalon--object-id-string course 'id)))
                     (cons (format "%s [%s]"
                                   (or (absalon--get 'name course)
                                       (absalon--get 'course_code course)
                                       "Course")
                                   id)
                           id)))
                 absalon-courses))
         (default (car (rassoc (absalon--string-or-nil
                               (absalon--get 'id absalon-current-course))
                              choices))))
    (unless choices
      (user-error "No Absalon courses available"))
    (or (cdr (assoc (completing-read "Course: " choices nil t nil nil default)
                    choices))
        (user-error "No course selected"))))

(defun absalon--creation-text (value label)
  "Return trimmed VALUE for LABEL, rejecting empty or multiline input."
  (let ((text (string-trim (format "%s" (or value "")))))
    (when (or (string-empty-p text) (string-match-p "[\n\r]" text))
      (user-error "%s must be non-empty and on one line" label))
    text))

(defun absalon--check-position (position)
  "Require optional POSITION to be a positive, one-based integer."
  (when (and position (not (and (integerp position) (> position 0))))
    (user-error "Position must be a positive integer"))
  position)

(defun absalon--read-position ()
  "Read an optional one-based position; empty input means append."
  (let ((input (string-trim (read-string "Position (empty to append): "))))
    (unless (string-empty-p input)
      (unless (string-match-p "\\`[0-9]+\\'" input)
        (user-error "Position must be a positive integer"))
      (absalon--check-position (string-to-number input)))))

(defun absalon--read-new-page-module (course-id)
  "Select an optional module in COURSE-ID, defaulting to the current module."
  (let* ((object (and (derived-mode-p 'absalon-list-mode)
                      (tabulated-list-get-id)
                      (absalon-view-current-object)))
         (default-id
          (or (and (derived-mode-p 'org-mode)
                   (absalon-org-keyword "ABSALON_MODULE_ID"))
              (absalon--view-module-id object)))
         (choices
          (mapcar (lambda (module)
                    (let ((id (absalon--object-id-string module 'id)))
                      (cons (format "%s [%s]"
                                    (absalon--get 'name module) id)
                            id)))
                  (absalon-modules course-id)))
         (default (car (rassoc (absalon--string-or-nil default-id) choices)))
         (choice (completing-read "Module (empty for none): " choices
                                  nil t default)))
    (cdr (assoc choice choices))))

(defun absalon-org-new-page (course-id title &optional module-id position)
  "Start a new Org page draft with TITLE in COURSE-ID.
Optional MODULE-ID places the page in that module on push, at POSITION
when specified (one-based).  The draft is unpublished and has no remote
page URL until `absalon-org-push-page' creates it.  Return the Org buffer.
This command only prepares a local draft and does not require write mode."
  (interactive
   (let* ((course-id (absalon--read-course-id))
          (title (read-string "New page title: "))
          (module-id (absalon--read-new-page-module course-id)))
     (list course-id title module-id
           (and module-id (absalon--read-position)))))
  (setq course-id (absalon--creation-text course-id "Course ID")
        title (absalon--creation-text title "Page title"))
  (when module-id
    (setq module-id (absalon--creation-text module-id "Module ID")))
  (absalon--check-position position)
  (when (and position (not module-id))
    (user-error "A module is required to set a page position"))
  (let ((buffer (generate-new-buffer (format "*Absalon new page: %s*" title))))
    (with-current-buffer buffer
      (org-mode)
      (insert (format "#+TITLE: %s\n#+ABSALON_COURSE_ID: %s\n"
                      title course-id))
      (when module-id
        (insert (format "#+ABSALON_MODULE_ID: %s\n" module-id)))
      (when position
        (insert (format "#+ABSALON_POSITION: %d\n" position)))
      (insert "#+ABSALON_PUBLISHED: false\n\n"))
    (with-current-buffer buffer
      (absalon--setup-page-editor))
    (pop-to-buffer buffer)
    buffer))

(defun absalon-show-profile ()
  "Show the current user's profile."
  (interactive)
  (absalon--display-json (absalon-profile) "*Absalon profile*"))

(defun absalon-list-courses (&optional include-completed)
  "Show courses; with prefix INCLUDE-COMPLETED include completed courses."
  (interactive "P")
  (absalon--show-courses include-completed))

(defun absalon-list-modules (&optional course-id)
  "Show modules in COURSE-ID, defaulting to the selected course."
  (interactive)
  (absalon--show-modules (absalon--require-course-id course-id)))

(defun absalon-list-pages (&optional course-id)
  "Show pages in COURSE-ID, defaulting to the selected course."
  (interactive)
  (absalon--show-pages (absalon--require-course-id course-id)))

(defun absalon-list-files (&optional course-id)
  "Show the folder/file tree in COURSE-ID, defaulting to the selected course.
If no course is selected, offer course-name completion."
  (interactive)
  (absalon--show-files (absalon--require-course-id course-id)))

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
        (goto-char (absalon--org-metadata-end))
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

(defun absalon--html-collapse-space (text)
  "Collapse HTML whitespace in TEXT for inline Org output."
  (replace-regexp-in-string "[ \t\n\r]+" " " (or text "")))

(defun absalon--html-tag (node)
  "Return NODE's DOM tag, or nil."
  (and (consp node) (dom-tag node)))

(defun absalon--html-block-tag-p (tag)
  "Return non-nil when TAG should be rendered as an Org block."
  (memq tag '(address article aside blockquote body dd div dl dt fieldset
              figcaption figure footer form h1 h2 h3 h4 h5 h6 header hr html
              li main nav ol p pre section table tbody td tfoot th thead tr ul)))

(defun absalon--html-has-block-child-p (node)
  "Return non-nil when NODE has a block-level child element."
  (seq-some (lambda (child)
              (and (consp child)
                   (absalon--html-block-tag-p (absalon--html-tag child))))
            (dom-children node)))

(defun absalon--html-wrap-org (marker text)
  "Wrap TEXT in Org MARKER when TEXT is non-empty."
  (setq text (string-trim text))
  (if (string-empty-p text)
      ""
    (concat marker text marker)))

(defun absalon--html-link-org (href label)
  "Return an Org link for HREF and LABEL."
  (setq href (string-trim (or href ""))
        label (string-trim (or label "")))
  (cond
   ((string-empty-p href) label)
   ((or (string-empty-p label) (string= href label))
    (format "[[%s]]" href))
   (t (format "[[%s][%s]]" href label))))

(defun absalon--html-text-content (node)
  "Return plain text content below NODE."
  (cond
   ((stringp node) node)
   ((consp node)
    (mapconcat #'absalon--html-text-content (dom-children node) ""))
   (t "")))

(defun absalon--html-inline (node)
  "Convert inline HTML NODE to Org markup."
  (cond
   ((stringp node) (absalon--html-collapse-space node))
   ((not (consp node)) "")
   (t
    (let* ((tag (absalon--html-tag node))
           (children (dom-children node))
           (content (lambda ()
                      (mapconcat #'absalon--html-inline children ""))))
      (pcase tag
        ((or 'b 'strong) (absalon--html-wrap-org "*" (funcall content)))
        ((or 'em 'i) (absalon--html-wrap-org "/" (funcall content)))
        ('code (absalon--html-wrap-org "~" (funcall content)))
        ((or 's 'strike 'del) (absalon--html-wrap-org "+" (funcall content)))
        ('br "\n")
        ('a (absalon--html-link-org (dom-attr node 'href) (funcall content)))
        ('img (absalon--html-link-org (dom-attr node 'src)
                                      (or (dom-attr node 'alt) "image")))
        ((or 'iframe 'video 'audio 'source)
         (absalon--html-link-org (or (dom-attr node 'src)
                                     (dom-attr node 'href))
                                 (symbol-name tag)))
        (_ (funcall content)))))))

(defun absalon--html-inline-without-lists (node)
  "Return inline Org text for NODE, excluding nested lists."
  (mapconcat (lambda (child)
               (if (and (consp child)
                        (memq (absalon--html-tag child) '(ul ol)))
                   ""
                 (absalon--html-inline child)))
             (dom-children node) ""))

(defun absalon--html-insert-list (node ordered indent)
  "Insert NODE as an Org list.  ORDERED non-nil means numbered list.
INDENT is the number of leading spaces before each bullet."
  (let ((index 1))
    (dolist (child (dom-children node))
      (when (and (consp child) (eq (absalon--html-tag child) 'li))
        (let* ((bullet (if ordered (format "%d." index) "-"))
               (text (string-trim
                      (absalon--html-inline-without-lists child))))
          (insert (make-string indent ?\s) bullet " ")
          (insert (if (string-empty-p text) " " text) "\n")
          (dolist (grandchild (dom-children child))
            (when (and (consp grandchild)
                       (memq (absalon--html-tag grandchild) '(ul ol)))
              (absalon--html-insert-list
               grandchild (eq (absalon--html-tag grandchild) 'ol)
               (+ indent 2))))
          (setq index (1+ index))))))
  (when (zerop indent) (insert "\n")))

(defun absalon--html-table-rows (node)
  "Return table row nodes below NODE."
  (let (rows)
    (dolist (child (dom-children node))
      (when (consp child)
        (if (eq (absalon--html-tag child) 'tr)
            (push child rows)
          (setq rows (nconc (nreverse (absalon--html-table-rows child))
                            rows)))))
    (nreverse rows)))

(defun absalon--html-table-cells (row)
  "Return table cell nodes in ROW."
  (seq-filter (lambda (child)
                (and (consp child)
                     (memq (absalon--html-tag child) '(td th))))
              (dom-children row)))

(defun absalon--html-table-cell-org (cell)
  "Return Org table text for CELL."
  (replace-regexp-in-string
   (regexp-quote "|") "\\vert{}"
   (string-trim (absalon--html-inline cell)) t t))

(defun absalon--html-insert-table (node)
  "Insert HTML table NODE as an Org table."
  (dolist (row (absalon--html-table-rows node))
    (let ((cells (mapcar #'absalon--html-table-cell-org
                         (absalon--html-table-cells row))))
      (when cells
        (insert "| " (mapconcat #'identity cells " | ") " |\n"))))
  (insert "\n"))

(defun absalon--html-heading-level (tag)
  "Return Org heading level for HTML heading TAG."
  (pcase tag
    ('h1 1) ('h2 2) ('h3 3) ('h4 4) ('h5 5) ('h6 6)))

(defun absalon--html-insert-container (node)
  "Insert NODE's children as Org blocks or a paragraph."
  (if (absalon--html-has-block-child-p node)
      (dolist (child (dom-children node))
        (absalon--html-insert-block child))
    (let ((text (string-trim (absalon--html-inline node))))
      (unless (string-empty-p text)
        (insert text "\n\n")))))

(defun absalon--html-insert-block (node)
  "Insert HTML NODE as Org markup into the current buffer."
  (cond
   ((stringp node)
    (let ((text (string-trim (absalon--html-collapse-space node))))
      (unless (string-empty-p text)
        (insert text "\n\n"))))
   ((not (consp node)) nil)
   (t
    (let ((tag (absalon--html-tag node)))
      (pcase tag
        ((or 'h1 'h2 'h3 'h4 'h5 'h6)
         (let ((text (string-trim (absalon--html-inline node))))
           (unless (string-empty-p text)
             (insert (make-string (absalon--html-heading-level tag) ?*)
                     " " text "\n\n"))))
        ('p
         (let ((text (string-trim (absalon--html-inline node))))
           (unless (string-empty-p text)
             (insert text "\n\n"))))
        ('br (insert "\n"))
        ('hr (insert "-----\n\n"))
        ('pre
         (let ((text (string-trim-right (absalon--html-text-content node))))
           (unless (string-empty-p text)
             (insert "#+begin_example\n" text "\n#+end_example\n\n"))))
        ('blockquote
         (insert "#+begin_quote\n")
         (dolist (child (dom-children node))
           (absalon--html-insert-block child))
         (insert "#+end_quote\n\n"))
        ('ul (absalon--html-insert-list node nil 0))
        ('ol (absalon--html-insert-list node t 0))
        ('table (absalon--html-insert-table node))
        ((or 'script 'style) nil)
        (_ (absalon--html-insert-container node)))))))

(defun absalon--dom-by-id (node id)
  "Return first DOM node below NODE whose id attribute is ID."
  (cond
   ((not (consp node)) nil)
   ((equal (dom-attr node 'id) id) node)
   (t (seq-some (lambda (child) (absalon--dom-by-id child id))
                (dom-children node)))))

(defun absalon--html-to-org (html)
  "Convert Canvas HTML fragment HTML to best-effort Org markup."
  (unless (fboundp 'libxml-parse-html-region)
    (user-error "This Emacs was built without libxml HTML parsing"))
  (let* ((dom (with-temp-buffer
                (insert "<!doctype html><html><body><div id=\"absalon-root\">")
                (insert (or html ""))
                (insert "</div></body></html>")
                (libxml-parse-html-region (point-min) (point-max))))
         (root (or (absalon--dom-by-id dom "absalon-root") dom)))
    (with-temp-buffer
      (dolist (child (dom-children root))
        (absalon--html-insert-block child))
      (goto-char (point-min))
      (while (re-search-forward "[ 	]+$" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "\n\{3,\}" nil t)
        (replace-match "\n\n"))
      (let ((org (string-trim (buffer-string))))
        (if (string-empty-p org) "" (concat org "\n"))))))

(defun absalon--org-metadata-end ()
  "Return point after the initial Org keyword block."
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp))
                (looking-at-p
                 "\\(?:[[:space:]]*$\\|[[:space:]]*#\\+[[:alnum:]_-]+:.*$\\)"))
      (forward-line 1))
    (point)))

(defun absalon--org-body-nonempty-p ()
  "Return non-nil when the current Org buffer has non-keyword body text."
  (not (string-empty-p
        (string-trim
         (buffer-substring-no-properties
          (absalon--org-metadata-end) (point-max))))))

(defun absalon--org-replace-body (body)
  "Replace current Org buffer body with BODY, preserving initial keywords."
  (let ((body (string-trim body))
        (start (absalon--org-metadata-end)))
    (delete-region start (point-max))
    (goto-char start)
    (unless (or (bobp)
                (save-excursion
                  (forward-line -1)
                  (looking-at-p "[[:space:]]*$")))
      (insert "\n"))
    (unless (string-empty-p body)
      (insert body)
      (unless (bolp) (insert "\n")))))

(defun absalon--page-published-keyword (page)
  "Return PAGE published state as an Org keyword value."
  (if (eq (absalon--get 'published page) t) "true" "false"))

(defun absalon--insert-page-as-org (course-id page-url page)
  "Insert PAGE from COURSE-ID and PAGE-URL into the current Org buffer."
  (let ((title (or (absalon--string-or-nil (absalon--get 'title page))
                   (absalon--string-or-nil page-url)
                   "Absalon page"))
        (remote-url (or (absalon--string-or-nil (absalon--get 'url page))
                        page-url))
        (body (absalon--html-to-org (absalon--get 'body page))))
    (absalon-org-set-keyword "TITLE" title)
    (absalon-org-set-keyword "ABSALON_COURSE_ID" course-id)
    (absalon-org-set-keyword "ABSALON_PAGE_URL" remote-url)
    (absalon-org-set-keyword "ABSALON_PUBLISHED"
                             (absalon--page-published-keyword page))
    (absalon--org-replace-body body)))

(defun absalon--open-page-org-buffer (course-id page-url &optional page)
  "Open PAGE-URL from COURSE-ID as an Org editor that pushes on save.
Reuse an existing editor for this page without overwriting local edits."
  (setq course-id (format "%s" course-id))
  (let* ((identity (cons course-id page-url))
         (buffer (seq-find
                  (lambda (buffer)
                    (equal identity
                           (buffer-local-value 'absalon--page-identity buffer)))
                  (buffer-list))))
    (unless buffer
      (let* ((page (or page (absalon-page course-id page-url)))
             (title (or (absalon--string-or-nil (absalon--get 'title page))
                        page-url)))
        (setq buffer (generate-new-buffer (format "*Absalon org: %s*" title)))
        (with-current-buffer buffer
          (org-mode)
          (absalon--insert-page-as-org course-id page-url page)
          (absalon--setup-page-editor)
          (set-buffer-modified-p nil))))
    (pop-to-buffer buffer)
    buffer))

(defun absalon-edit-page (&optional course-id page-url)
  "Edit PAGE-URL in COURSE-ID as Org, pushing changes on save.
Use the page at point in a page/module-item list or page detail buffer.
Else offer page-name completion in the selected course.  Explicit
COURSE-ID and PAGE-URL take precedence over buffer context."
  (interactive)
  (let* ((object (and (memq absalon-view-kind '(pages module-items))
                      (tabulated-list-get-id)
                      (absalon-view-current-object)))
         (context-course (or (and object (absalon--view-course-id object))
                             (absalon--get 'course-id absalon-detail-context)))
         (context-page
          (or (and object
                   (absalon--get (if (eq absalon-view-kind 'pages)
                                     'url 'page_url)
                                 object))
              (absalon--get 'page-url absalon-detail-context))))
    (setq course-id (absalon--require-course-id (or course-id context-course)))
    (setq page-url
          (or page-url
              (and context-course
                   (equal (format "%s" course-id) (format "%s" context-course))
                   context-page)
              (let* ((choices
                      (mapcar (lambda (page)
                                (cons (format "%s [%s]"
                                              (absalon--get 'title page)
                                              (absalon--get 'url page))
                                      (absalon--get 'url page)))
                              (absalon-pages course-id)))
                     (choice (progn
                               (unless choices
                                 (user-error "No pages in course %s" course-id))
                               (completing-read "Page: " choices nil t))))
                (or (cdr (assoc choice choices))
                    (user-error "No page selected")))))
    (absalon--open-page-org-buffer course-id page-url)))

(defun absalon--save-page-without-file ()
  "Handle saving a fileless page editor before its `after-save-hook' runs.
The actual push is performed by `absalon-push-page'.  File-visiting buffers
return nil here so Emacs also saves the local Org file normally."
  (unless buffer-file-name
    (set-buffer-modified-p nil)
    t))

(defun absalon--setup-page-editor ()
  "Install buffer-local save support for the current Org page."
  (setq-local buffer-offer-save t)
  (setq absalon--page-identity
        (when-let* ((page-url (absalon-org-keyword "ABSALON_PAGE_URL")))
          (cons (absalon-org-keyword "ABSALON_COURSE_ID") page-url)))
  (add-hook 'write-contents-functions #'absalon--save-page-without-file nil t)
  (add-hook 'after-save-hook #'absalon-push-page nil t))

(defun absalon-push-page ()
  "Push the current Org page using the existing write and confirmation settings.
This command also serves as a buffer-local `after-save-hook'.  Failed or
cancelled pushes leave the buffer modified, so saving can be retried."
  (interactive)
  (unless absalon--pushing-page
    (let ((absalon--pushing-page t))
      (condition-case err
          (let ((result (absalon-org-push-page)))
            (setq absalon--page-identity
                  (when-let* ((page-url (absalon-org-keyword "ABSALON_PAGE_URL")))
                    (cons (absalon-org-keyword "ABSALON_COURSE_ID") page-url)))
            (unless buffer-file-name (set-buffer-modified-p nil))
            result)
        ((error quit)
         (set-buffer-modified-p t)
         (signal (car err) (cdr err)))))))

(defun absalon-org-pull-page (course-id page-url)
  "Pull PAGE-URL in COURSE-ID into the current Org buffer.

The command replaces the local Org body with a best-effort conversion of the
Canvas HTML body, while preserving the initial Org keyword block and updating
TITLE, ABSALON_COURSE_ID, ABSALON_PAGE_URL, and ABSALON_PUBLISHED."
  (interactive
   (let* ((course-id (absalon--read-course-id))
          (page-url (read-string
                     (if-let* ((default (absalon-org-keyword
                                         "ABSALON_PAGE_URL")))
                         (format "Page URL (default %s): " default)
                       "Page URL: ")
                     nil nil (absalon-org-keyword "ABSALON_PAGE_URL"))))
     (list course-id page-url)))
  (unless (derived-mode-p 'org-mode)
    (user-error "Current buffer is not an Org buffer"))
  (when (and (called-interactively-p 'interactive)
             (absalon--org-body-nonempty-p)
             (not (yes-or-no-p
                   "Replace current Org body with the Absalon page body? ")))
    (user-error "Cancelled"))
  (let ((page (absalon-page course-id page-url)))
    (absalon--insert-page-as-org course-id page-url page)
    (message "Pulled Absalon page %s" page-url)
    page))

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

(defun absalon-create-module (course-id name &optional position)
  "Create and return an unpublished module named NAME in COURSE-ID.
Optional POSITION is one-based; nil appends the module to the course.
Interactively, prompt for these values and confirm the write."
  (interactive
   (list (absalon--read-course-id)
         (read-string "New module name: ")
         (absalon--read-position)))
  (setq course-id (absalon--creation-text course-id "Course ID")
        name (absalon--creation-text name "Module name"))
  (absalon--check-position position)
  (when (called-interactively-p 'interactive)
    (absalon--confirm-write
     (format "Create module %S in course %s" name course-id)))
  (absalon--ensure-write)
  (let ((module
         (absalon-request
          (format "courses/%s/modules" course-id) "POST"
          (append `(("module[name]" . ,name))
                  (when position `(("module[position]" . ,position)))))))
    (when (called-interactively-p 'interactive)
      (message "Created Absalon module %s: %s"
               (absalon--get 'id module) name)
      (when (and (eq absalon-view-kind 'modules)
                 (equal course-id (absalon--view-course-id)))
        (absalon-view-refresh)))
    module))

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
  (let* ((absalon--pushing-page t)
         (plan (absalon-org-plan))
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

(defun absalon-create-folder (course-id name &optional parent-folder-id)
  "Create and return a folder named NAME in COURSE-ID.
Optional PARENT-FOLDER-ID specifies the parent; nil uses the course root.
Interactively, select the parent by path and confirm the write."
  (interactive
   (let* ((course-id (absalon--file-command-course-id))
          (name (read-string "New folder name: ")))
     (list course-id name (absalon--read-folder-id course-id "Parent folder: "))))
  (setq course-id (absalon--creation-text course-id "Course ID")
        name (absalon--creation-text name "Folder name"))
  (when parent-folder-id
    (setq parent-folder-id (absalon--creation-text parent-folder-id "Parent folder ID")))
  (when (called-interactively-p 'interactive)
    (absalon--confirm-write
     (format "Create folder %S in course %s" name course-id)))
  (absalon--ensure-write)
  (let ((folder (absalon-request
                 (format "courses/%s/folders" course-id) "POST"
                 (append `(("name" . ,name))
                         (when parent-folder-id
                           `(("parent_folder_id" . ,parent-folder-id)))))))
    (when (called-interactively-p 'interactive)
      (absalon--refresh-files-view course-id)
      (message "Created Absalon folder %s: %s" (absalon--get 'id folder) name))
    folder))

(defun absalon-upload-file (course-id file &optional folder-id on-duplicate)
  "Upload FILE to COURSE-ID using Canvas's two-step protocol.

Optional FOLDER-ID selects a Canvas folder.  ON-DUPLICATE is normally
`overwrite', `rename', or nil (Canvas default).  Return the Canvas file object.
Interactively, select a destination folder by its full path."
  (interactive
   (let* ((course-id (absalon--file-command-course-id))
          (file (read-file-name "File to upload: " nil nil t)))
     (list course-id file (absalon--read-folder-id course-id "Upload to folder: "))))
  (setq file (expand-file-name file))
  (unless (file-regular-p file) (user-error "Not a regular file: %s" file))
  (when (called-interactively-p 'interactive)
    (absalon--confirm-write
     (format "Upload %s to folder %s in course %s"
             (file-name-nondirectory file) (or folder-id "(root)") course-id)))
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
    (let ((result (absalon--curl-multipart upload-url upload-params file)))
      (when (called-interactively-p 'interactive)
        (absalon--refresh-files-view course-id)
        (message "Uploaded %s" (file-name-nondirectory file)))
      result)))

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
