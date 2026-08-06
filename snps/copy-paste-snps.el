;;; copy-paste-snps.el --- Clipboard integration with cliphist -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'seq)
(require 'consult)

(defcustom copy-paste-cliphist-command
  (or (executable-find "cliphist")
      (expand-file-name "~/go/bin/cliphist"))
  "Path to cliphist executable."
  :type 'string)

(defcustom copy-paste-cliphist-limit 5
  "Number of cliphist items to fetch."
  :type 'integer)

(defvar copy-paste-cliphist-cache nil)
(defvar copy-paste-cliphist-cache-time 0)

(defcustom copy-paste-cliphist-cache-ttl 2
  "Seconds before refreshing cliphist cache."
  :type 'integer)

(defun copy-paste-cliphist-fetch ()
  "Fetch cliphist entries (multi-line aware)."
  (let* ((cmd (format "%s list" copy-paste-cliphist-command))
         (raw (shell-command-to-string cmd)))
    ;; cliphist separates entries by newline but multiline entries are indented
    (let ((lines (split-string raw "\n"))
          (items '())
          (current ""))
      (dolist (line lines)
        (if (string-prefix-p " " line)
            ;; continuation of previous item
            (setq current (concat current "\n" (string-trim-left line)))
          ;; new item
          (when (not (string-empty-p current))
            (push current items))
          (setq current line)))
      (when (not (string-empty-p current))
        (push current items))
      (seq-take (nreverse items) copy-paste-cliphist-limit))))

(defun copy-paste-cliphist-items ()
  "Return cached cliphist items."
  (if (or (null copy-paste-cliphist-cache)
          (> (- (float-time) copy-paste-cliphist-cache-time)
             copy-paste-cliphist-cache-ttl))
      (progn
        (setq copy-paste-cliphist-cache (copy-paste-cliphist-fetch))
        (setq copy-paste-cliphist-cache-time (float-time))
        copy-paste-cliphist-cache)
    copy-paste-cliphist-cache))

(defun copy-paste-format-candidate (cand)
  "Format candidate for display (single-line preview)."
  (let* ((preview (replace-regexp-in-string "\n" " ↩ " cand))
         (short (truncate-string-to-width preview 80 nil nil "…")))
    short))

(defun copy-paste-consult-clipboard ()
  "Select from kill-ring and cliphist with preview."
  (interactive)
  (let* ((kill-items (delete-dups (seq-filter #'stringp kill-ring)))
         (cliphist-items (copy-paste-cliphist-items))
         (cliphist-items (seq-remove (lambda (item)
                                       (member item kill-items))
                                     cliphist-items))
         (candidates
          (append
           (mapcar (lambda (s) (propertize s 'source 'kill)) kill-items)
           (mapcar (lambda (s) (propertize s 'source 'cliphist)) cliphist-items)))
         (annotated
          (mapcar (lambda (s)
                    (cons (copy-paste-format-candidate s) s))
                  candidates))
         (choice
          (consult--read annotated
                         :prompt "Clipboard: "
                         :require-match t
                         :category 'clipboard
                         :sort nil)))
    (when choice
      (kill-new choice)
      (insert choice))))

(provide 'copy-paste-snps)
