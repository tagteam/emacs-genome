;;; dst-type.el --- Type text into a FreeRDP editor -*- lexical-binding: t; -*-

;;; Commentary:
;; Load this file, then use M-x dst-type-file,
;; M-x dst-type-buffer or M-x dst-type-region.  First place the remote
;; editor's caret at the destination.  Select US layout in Windows and
;; disable the remote editor's auto-indent, auto-pairing and completion.
;; Requires xdotool and X11.  All sequencing and delays run in Emacs.
;; Do not type or switch windows during transmission.  Focus is checked
;; between lines, but a focus change during a line can misdirect keystrokes.
;; Receipt cannot be verified; inspect the remote text after transmission.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup dst-type nil
  "Type text into a remote desktop."
  :group 'external)

(defcustom dst-type-character-delay 50
  "Delay between keystrokes, in milliseconds."
  :type 'natnum)

(defcustom dst-type-line-delay 0.3
  "Pause after each line, in seconds."
  :type 'number)

(defcustom dst-type-start-delay 2
  "Pause after activating the remote window, in seconds."
  :type 'number)

(defcustom dst-type-window-name "FreeRDP"
  "Window title regular expression used by xdotool."
  :type 'string)

(defcustom dst-type-window-id nil
  "Explicit decimal X11 window ID, or nil to search by title.
When nil, prompt with completion if several matching windows are visible."
  :type '(choice (const nil) string))

(defcustom dst-type-keymap
  (let (keys)
    ;; Standard X11 evdev keycodes.  Encode the receiving Windows layout,
    ;; not X keysyms: xdotool's keysym lookup can pick another XKB group or
    ;; a synthetic key that FreeRDP cannot translate to a scan code.
    (dolist (row '((10 "1234567890-=" "!@#$%^&*()_+")
                   (24 "qwertyuiop[]" "QWERTYUIOP{}")
                   (38 "asdfghjkl;'" "ASDFGHJKL:\"")
                   (52 "zxcvbnm,./" "ZXCVBNM<>?")
                   (49 "`" "~") (51 "\\" "|") (65 " " nil)
                   (23 "\t" nil)))
      (cl-loop for char across (nth 1 row) for code from (car row)
               do (push (cons char (number-to-string code)) keys))
      (when (nth 2 row)
        (cl-loop for char across (nth 2 row) for code from (car row)
                 do (push (cons char (format "50+%d" code)) keys))))
    keys)
  "Characters mapped to physical xdotool key sequences.
The default is a US Windows keyboard on standard X11 evdev keycodes.
The remote Windows layout must match this map; the local layout need not.
Numeric keycodes bypass xdotool's ambiguous keysym/group lookup.  For
example, US `(' is Shift (50) plus the 9 key (18), hence \"50+18\".
Unsupported characters are rejected before any text is sent.  Customize
this map for other receiving layouts or additional characters."
  :type '(alist :key-type character :value-type string))

(cl-defstruct (dst-type--job (:constructor dst-type--make-job))
  text (position 0) window executable character-delay line-delay start-delay
  keymap process timer)

(defvar dst-type--job nil
  "Current typing job, including pauses between subprocesses.")

(defun dst-type--finish (job message-text)
  "Stop JOB and report MESSAGE-TEXT."
  (when (eq job dst-type--job)
    (setq dst-type--job nil)
    (when (timerp (dst-type--job-timer job))
      (cancel-timer (dst-type--job-timer job)))
    (when (process-live-p (dst-type--job-process job))
      (delete-process (dst-type--job-process job)))
    (with-current-buffer (get-buffer-create "*dst-type*")
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert message-text "\n")))
    (message "%s" message-text)))

(defun dst-type--call (job args callback)
  "Run xdotool with ARGS for JOB, then pass stdout to CALLBACK.
No shell is involved.  Errors stop the job and appear in *dst-type*."
  (when (eq job dst-type--job)
    (condition-case err
        (let ((output "")
              (default-directory temporary-file-directory))
          (setf (dst-type--job-process job)
                (make-process
                 :name "dst-type" :buffer nil :stderr (get-buffer-create "*dst-type*")
                 :command (cons (dst-type--job-executable job) args)
                 :coding 'utf-8-unix :connection-type 'pipe :noquery nil
                 :filter (lambda (_process chunk) (setq output (concat output chunk)))
                 :sentinel
                 (lambda (process _event)
                   (when (and (eq job dst-type--job)
                              (memq (process-status process) '(exit signal)))
                     (setf (dst-type--job-process job) nil)
                     (if (and (eq (process-status process) 'exit)
                              (= (process-exit-status process) 0))
                         (condition-case callback-error
                             (funcall callback output)
                           (error (dst-type--finish
                                   job (error-message-string callback-error))))
                       (dst-type--finish
                        job "DST typing failed; see *dst-type* for xdotool errors")))))))
      (error (dst-type--finish job (error-message-string err))))))

(defun dst-type--pause (job seconds callback)
  "Wait SECONDS without blocking Emacs, then invoke CALLBACK for JOB."
  (setf (dst-type--job-timer job)
        (run-at-time seconds nil
                     (lambda ()
                       (when (eq job dst-type--job)
                         (setf (dst-type--job-timer job) nil)
                         (condition-case err
                             (funcall callback)
                           (error (dst-type--finish job (error-message-string err)))))))))

(defun dst-type--check-focus (job callback)
  "Invoke CALLBACK only if JOB's window is still active."
  (dst-type--call
   job '("getactivewindow")
   (lambda (output)
     (if (equal (string-trim output) (dst-type--job-window job))
         (funcall callback)
       (dst-type--finish job "DST focus changed; typing stopped")))))

(defun dst-type--next-line (job)
  "Type the next line of JOB, preserving whether it ends in a newline."
  (let* ((text (dst-type--job-text job))
         (position (dst-type--job-position job))
         (end (string-match "\n" text position)))
    (if (= position (length text))
        (dst-type--finish job "DST typing finished; check the remote text")
      (let* ((line (substring text position (or end (length text))))
             (delay (number-to-string (dst-type--job-character-delay job)))
             (advance (lambda (_output)
                        (setf (dst-type--job-position job)
                              (if end (1+ end) (length text)))
                        (dst-type--pause job (dst-type--job-line-delay job)
                                         (lambda () (dst-type--next-line job)))))
             (return (lambda (_output)
                       (if end
                           (dst-type--check-focus
                            job (lambda ()
                                  (dst-type--call
                                   job (list "key" "--clearmodifiers" "--delay"
                                             delay "36") advance)))
                         (funcall advance nil)))))
        ;; Normalize CRLF without removing other whitespace.
        (when (and end (string-suffix-p "\r" line))
          (setq line (substring line 0 -1)))
        (dst-type--check-focus
         job (lambda ()
               (if (string-empty-p line)
                   (funcall return nil)
                 (dst-type--call
                  job (append (list "key" "--clearmodifiers" "--delay" delay)
                              (mapcar (lambda (char)
                                        (cdr (assq char (dst-type--job-keymap job))))
                                      (string-to-list line)))
                  return))))))))

(defun dst-type--activate (job window)
  "Activate WINDOW and begin typing JOB after the startup pause."
  (unless (string-match-p "\\`[0-9]+\\'" window)
    (error "Window ID must be a decimal integer"))
  (setf (dst-type--job-window job) window)
  (dst-type--call
   job (list "windowactivate" "--sync" window)
   (lambda (_output)
     (dst-type--pause job (dst-type--job-start-delay job)
                      (lambda () (dst-type--next-line job))))))

(defun dst-type--choose-window (job windows &optional choices)
  "Collect titles for WINDOWS, then prompt for JOB's destination.
CHOICES holds title/ID labels paired with their window IDs."
  (when (eq job dst-type--job)
    (if windows
        (let ((id (car windows)))
          (dst-type--call
           job (list "getwindowname" id)
           (lambda (title)
             (dst-type--choose-window
              job (cdr windows)
              (cons (cons (format "%s [ID %s]"
                                  (replace-regexp-in-string
                                   "[\r\n]+" " " (string-trim title)) id)
                          id)
                    choices)))))
      (condition-case nil
          (let* ((choices (nreverse choices))
                 (choice (completing-read "Remote window: " choices nil t)))
            (when (eq job dst-type--job)
              (dst-type--activate job (cdr (assoc choice choices)))))
        (quit (dst-type--finish job "DST typing canceled"))))))

(defun dst-type--send (text)
  "Send a snapshot of TEXT asynchronously using xdotool directly."
  (when dst-type--job
    (user-error "A DST typing job is already running"))
  (when (string-match-p "\0" text)
    (user-error "Cannot type text containing NUL characters"))
  ;; Validate the entire snapshot before activating the remote window, so
  ;; an unsupported character cannot leave a partially typed program.
  (setq text (replace-regexp-in-string "\r\n" "\n" text t t))
  (cl-loop for char across text
           unless (or (= char ?\n) (assq char dst-type-keymap))
           do (user-error "Character %c (U+%04X) is not in dst-type-keymap"
                          char char))
  (unless (and (integerp dst-type-character-delay)
               (>= dst-type-character-delay 0)
               (numberp dst-type-line-delay) (>= dst-type-line-delay 0)
               (numberp dst-type-start-delay) (>= dst-type-start-delay 0))
    (user-error "Typing delays must be nonnegative; character delay must be an integer"))
  (let ((executable (executable-find "xdotool")))
    (unless executable (user-error "Cannot find xdotool"))
    (when (and dst-type-window-id
               (not (and (stringp dst-type-window-id)
                         (string-match-p "\\`[0-9]+\\'" dst-type-window-id))))
      (user-error "Window ID must be a decimal integer string"))
    (let ((job (dst-type--make-job
                :text (substring-no-properties text) :executable executable
                :keymap (copy-tree dst-type-keymap)
                :character-delay dst-type-character-delay
                :line-delay dst-type-line-delay :start-delay dst-type-start-delay)))
      (with-current-buffer (get-buffer-create "*dst-type*")
        (let ((inhibit-read-only t)) (erase-buffer)))
      (setq dst-type--job job)
      (message "DST typing started; keep the remote editor focused")
      (if dst-type-window-id
          (dst-type--activate job dst-type-window-id)
        (dst-type--call
         job (list "search" "--onlyvisible" "--name" dst-type-window-name)
         (lambda (output)
           (let ((windows (delete-dups (split-string output "[\r\n]+" t))))
             (cond
              ((null windows) (error "No matching remote window"))
              ((null (cdr windows)) (dst-type--activate job (car windows)))
              (t (dst-type--choose-window job windows))))))))))

;;;###autoload
(defun dst-type-stop ()
  "Cancel the current typing job and pending delays.
Keystrokes already sent to the remote desktop cannot be recalled."
  (interactive)
  (if dst-type--job
      (dst-type--finish dst-type--job "DST typing canceled")
    (message "No DST typing job is running")))

;;;###autoload
(defun dst-type-file (file)
  "Type the contents of FILE from disk into the remote editor.
Emacs decodes the file before sending it as UTF-8."
  (interactive "fFile to type: ")
  (dst-type--send
   (with-temp-buffer
     (insert-file-contents file)
     (buffer-string))))

;;;###autoload
(defun dst-type-buffer ()
  "Type the entire current buffer, including unsaved changes.
Ignore narrowing; use `dst-type-region' to send only part of a buffer."
  (interactive)
  (dst-type--send
   (save-restriction
     (widen)
     (buffer-substring-no-properties (point-min) (point-max)))))

;;;###autoload
(defun dst-type-region (begin end)
  "Type the region between BEGIN and END into the remote editor."
  (interactive "r")
  (dst-type--send (buffer-substring-no-properties begin end)))

(provide 'dst-type)
;;; dst-type.el ends here
