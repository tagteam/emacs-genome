;;  displaying lists and completion 
;;  ------------------------------------------------------------------

(defvar file-list-display-help-string
  (concat ""
	  ;; (substitute-command-keys "list of matching files. here are some commands and keys. M-x `describe-bindings' shows all key-bindings\n\n`\\<file-list-mode-map>\\[file-list-choose-file]' find file `\\<file-list-mode-map>\\[file-list-choose-file-other-window]' in other window;`\\<file-list-mode-map>\\[file-list-choose-magic]' open, i.e. find with external program\n`S f', `S p', `S t', `S s' sorting and `/f' `/p' `/s' `/t' sub-selection by name, path, size, time\n `g' grep; `x' shell command; `d' dired; `t' toggle display\n\n")))
	  (substitute-command-keys "finding files: RET, SPACE, M-RET; sorting list: S f, S p, S t, S s; selecting from list: /f /p /s /t (by name, path, size, time)\nother commands: g grep; x shell command; d dired; t toggle display ...\n\n")))


(defun file-list-current-display-buffer ()
  (if (eq major-mode 'file-list-completion-mode)
  (buffer-name (current-buffer))
  nil))

(defun file-list-display-match-list (&optional file-list match-info display-buffer)
  "This function shows file-list in display-buffer.
Sets the value of file-list-current-file-list in display-buffer."
  (let ((file-list (or file-list file-list-current-file-list))
	(display-buffer (or display-buffer
			    (file-list-current-display-buffer)
			    file-list-display-buffer))
	(active (file-list-current-display-buffer))
	match-info-string
	display-list)
    (switch-to-buffer display-buffer)
    ;; sort list
    (setq file-list
	  (sort file-list
		(lambda (e f)
		  (> (length (caddr e)) (length (caddr f))))))
    (setq display-list
	  (cond ((= file-list-display-level 2)
		 (mapcar
		  (lambda (entry)
		    (let ((file-name
			   (if (featurep 'xemacs)
			       (file-list-replace-in-string
				(file-list-make-file-name entry) " " "\\\ " 'literal)
			     (file-list-replace-in-string
			      (file-list-make-file-name entry) " " "\\ " 'literal)))
			  (rest (caddr entry))
			  rest-string)
		      (while rest
			(setq rest-string
			      (concat rest-string "\n" (format "%13s" (caar rest))
				      " : " (cdar rest)))
			(setq rest (cdr rest)))
		      (concat (when rest-string "\n") file-name rest-string)))
		  file-list))
		((= file-list-display-level 1)
		 (mapcar
		  (lambda (entry)
		    (if (featurep 'xemacs)
			(file-list-replace-in-string
			 (file-list-make-file-name entry) " " "\\\ " 'literal)
		      (file-list-replace-in-string
		       (file-list-make-file-name entry) " " "\\ " 'literal)))
		  file-list))
		(t (mapcar
		    (lambda (entry)
		      (file-list-replace-in-string
		       (car entry) " " "\\\ " 'literal)) file-list))))
    ;; match history
    (if match-info
	(if active
	    (setq file-list-match-history
		  (concat file-list-match-history match-info))
	  (setq file-list-match-history match-info))
      (setq file-list-match-history nil))
    ;; (message "bla:" file-list-match-history)
    (setq match-info-string
	  (format "Match-info: %i files %s\n\ncurrent file-list:\n"
		  (length display-list)
		  (or file-list-match-history "match")))
    (if (featurep 'xemacs)
	;; xemacs
	(with-output-to-temp-buffer
	    display-buffer 
	  (display-completion-list
	   display-list
	   ;; :activate-callback 'file-list-choose-file
	   :help-string file-list-display-help-string
	   :completion-string
	   match-info-string))
      ;; emacs
      (with-output-to-temp-buffer
	  display-buffer 
	(display-completion-list display-list))
      (let ((buffer-read-only nil))
	(when (re-search-forward "\\(Possible completions are:\\)" nil t)
	  (delete-region (point-min) (point)))
	(insert match-info-string)))
    ;; (replace-match match-info-string nil t))))
    (switch-to-buffer display-buffer)
    (file-list-completion-mode)
    (setq file-list-current-file-list file-list)
    (file-list-beginning-of-file-list)))

;; (file-list-switch-to-file-list)))))


(defun file-list-quit (&optional force)
  "Close file-list window and switch to another buffer."
  (interactive)
  (when (eq major-mode 'file-list-completion-mode)
    (when (or force (not (one-window-p)))
      (delete-window))
    (if (featurep 'xemacs)
	(switch-to-other-buffer 0)
      (switch-to-buffer (other-buffer)))))


; completition
; --------------------------------------------------------------------

(defun file-list-completion-mode ()
  (interactive)
  ;; (when (string= file-list-display-buffer (buffer-name))
  ;; (when (string-match "file-list" (buffer-name))
  (kill-all-local-variables)
  (use-local-map file-list-mode-map)
  (setq major-mode 'file-list-completion-mode)
  (setq mode-name "file-list-display")
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'file-list-current-file-list)
  (make-local-variable 'file-list-match-history)
  (setq font-lock-defaults
	'(file-list-font-lock-keywords t nil ((?' . "."))))
  (turn-on-font-lock)
  (run-hooks 'file-list-completion-mode-hook))

;; this was not a good idea:
;; (add-hook 'completion-setup-hook 'file-list-completion-mode)

(defun file-list-switch-to-file-list ()
  (interactive)
  (let (w)
    (if (setq w (get-buffer-window file-list-display-buffer t))
	(select-window w))
    ;;(display-buffer file-list-display-buffer nil)
    (file-list-beginning-of-file-list)
    (file-list-completion-mode)))
	
(defun file-list-toggle-display-mode ()
  "Toggles display between the three stages 'filename',
'absolute filename' and 'absolute file-name with attributes and font-lock'."
  (interactive)
    (setq file-list-display-level
	  (cond ((= file-list-display-level 2) 1)
		((= file-list-display-level 1) 0)
		((= file-list-display-level 0) 2)))
    (file-list-display-match-list file-list-current-file-list))


(defun file-list-clear-display ()
  (interactive)
  (when (file-list-current-display-buffer)
    (dolist (entry file-list-current-file-list)
      (when (> (length (cdr entry)) 1)
	(setcdr entry (list (car (cdr entry))))))
    (file-list-display-match-list file-list-current-file-list)))
    

(defun file-list-beginning-of-file-name ()
  "Find the beginning of the file-name at point"
  (unless (and (looking-at "[^ \t\n]")
	       (save-excursion
		 (and
		  (progn (backward-char 1) (looking-at "[ \t\n]"))
		  (progn (backward-char 1) (not (looking-at "\\\\"))))))
    (let ((found nil)
	  (pmin (- (save-excursion
		     (file-list-beginning-of-file-list)) 1)))
      (if (and (= file-list-display-level 2)
	       (save-excursion (beginning-of-line) (looking-at "[ \t\n]+")))
	  (re-search-backward "^[^ \t\n]" pmin t)
	(skip-chars-backward " \t\n")
	(re-search-backward "[ \t\n]" pmin t)
	(while (and (not found) (not (bobp)))
	  (backward-char 1)
	  (if (looking-at "\\\\")
	      (re-search-backward "[ \t\n]" pmin t)
	    (forward-char 1)
	    (setq found 'yes)))
	(skip-chars-forward "\t\n ")))))

(defun file-list-find-end-of-file-name ()
  "Find the end of file-name.
Works only if the point is at the beginning of a file-name.
Returns the point at the end of the file-name."
  (let (found)
    (skip-chars-forward "^ \t\n")
    (while (and (not found) (not (eobp)))
      (if (save-excursion
	    (backward-char 1)
	    (looking-at "\\\\"))
	  (progn
	    (skip-chars-forward " \t\n")
	    (skip-chars-forward "^ \t\n"))
	(setq found 'yes)))
    (point)))

(defun file-list-end-of-file-name ()
  "Goto end of file-name. Works only if the point is at the beginning of a file-name."
  (file-list-beginning-of-file-name)
  (goto-char (file-list-find-end-of-file-name)))


(defun file-list-file-at-point (&optional exists-p)
  (unless (eq major-mode 'file-list-completion-mode)
    ;; (string= (buffer-name (current-buffer)) file-list-display-buffer)
    (error (format "Works only in buffer %s." file-list-display-buffer)))
  (if (= file-list-display-level 0)
      (error "No absolute filename at point! Command needs absolute file names (toggle is on `t' or M-x file-list-toggle-display-mode)"))
  (save-excursion
    (let (fname)
      (file-list-beginning-of-file-name)
      (let ((fbeg (point))
	    (fend (file-list-find-end-of-file-name)))
	(if (featurep 'xemacs)
	    (setq fname (buffer-substring fbeg fend))
	  (setq fname (buffer-substring-no-properties fbeg fend)))
	(setq fname (file-list-replace-in-string fname "\\\\" "")))
      (if (not fname) (error "No absolute filename at point!")
	(if (or (not exists-p)
		(file-exists-p fname))
	    fname
	  (error "file %s does not exist on disk" fname))))))

(defun file-list-end-of-file-list ()
  "Return the point of the end of displayed file-list."
  (when (eq major-mode 'file-list-completion-mode)
    ;; (set-buffer file-list-display-buffer)
    (goto-char (point-max))
    (re-search-backward "^[^ \t\n]" nil t)
    (end-of-line)
    (point)))

(defun file-list-beginning-of-file-list ()
  "Return the point of the beginning of displayed file-list."
  (interactive)
  (when (eq major-mode 'file-list-completion-mode)
    ;; (set-buffer file-list-display-buffer)
    (goto-char (point-min))
    (re-search-forward "^current file-list:[\n]+" nil t)
    (re-search-forward "^[/a-zA-Z]+" nil t)
    (beginning-of-line)
    (point)))

(defun file-list-next-file (arg)
  "Move to the file-name of the ARGth entry below point."
  (interactive "p")
  (if (featurep 'xemacs) 
      (let ((beg (save-excursion (file-list-beginning-of-file-list))))
	(when (< (point) beg) (goto-char beg))
	(let ((tmp (+ arg 1)))
	  (if (> arg 1) 
	      (while (> tmp 1)
		(file-list-next-file 1)
		(setq tmp (- tmp 1)))
	    ;;    (next-list-mode-item arg)
	    (file-list-beginning-of-file-name)
	    (file-list-end-of-file-name)
	    (unless (>= (point)
			(file-list-end-of-file-list))
	      (skip-chars-forward " \t\n"))
	    (if (and (= file-list-display-level 2)
		     (save-excursion (beginning-of-line) (looking-at "[ \t\n]+")))
		(progn (re-search-forward "^[^ \t\n]" (file-list-end-of-file-list) t)
		       (unless (= (point) (file-list-end-of-file-list)) (backward-char 1))
		       (skip-chars-forward " \t\n"))))))
    (next-completion arg)))



(defun file-list-previous-file (arg)
  "Move to the file-name of the ARGth entry above point."
  (interactive "p")
  (if (featurep 'xemacs)
      (let ((tmp (+ arg 1)))
    (if (> arg 1) 
	(while (> tmp 1)
	  (file-list-previous-file 1)
	  (setq tmp (- tmp 1)))
      	(file-list-beginning-of-file-name)
	(if (eq (point) (save-excursion (file-list-beginning-of-file-list)))
	    (message "Beginning of file-list")
	(skip-chars-backward " \n\t")
	(file-list-beginning-of-file-name)
	(when (= file-list-display-level 2)
	  (skip-chars-forward "\t\n")))))
    (previous-completion arg)))


(defun file-list-nth-in-list (entry file-list)
  "Return the position of entry in file-list."
  (let ((ename (if (stringp entry) entry (file-list-make-file-name entry))))
    (if (string= ename (file-list-make-file-name (car file-list)))
	0
      (let ((rest (cdr file-list))
	    (pos 1))
	(while 
	    (not
	     (cond ((string= (file-list-make-file-name (car rest)) ename)
		    pos)
		   (rest (setq rest (cdr rest)
			       pos (+ pos 1))
			 nil)
		   (t (setq pos nil)
		      t))))
	pos))))


; high-lighting 
; --------------------------------------------------------------------
  

; (make-face 'file-list-directory-name-face)
; (set-face-foreground 'file-list-directory-name-face  "#0000ff") ;; bright blue

; (make-face 'file-list-file-name-face)
; (set-face-foreground 'file-list-file-name-face  "#9932CC")    ;; DarkOrchid

; (make-face 'file-list-grep-lineno-face)
; (set-face-foreground 'file-list-grep-lineno-face  "#ff0000")    ;; pure red

; (make-face 'file-list-grep-face)
; (set-face-foreground 'file-list-grep-face  "#00b000")    ;; green


; (defvar file-list-font-lock-keywords
;   '(("\\(/[^\t\n ]+/\\)\\([^/\t\n]+\\)\t"
; ;    ("\\(/[^\t\n ]+/\\)\\([^/\t\n]+\\)\\([\n\t ]+/\\|$\\)"
;      (1 file-list-directory-name-face)
;      (2 file-list-file-name-face))
;     ("\\(.*\\) : \\(.*\\)$"
;      (1 file-list-grep-lineno-face t)
;      (2 file-list-grep-face t)))
;   "Font Lock regexp for file-list-completion-list.")

(defvar file-list-font-lock-keywords
  '(("\\(/[^\t\n ]+/\\)\\([^/\t\n]+\\)" ;; file-list-file-name-regexp
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face))
    ("^ +\\(.*\\) : \\(.*\\)$";;file-list-info-regexp
     (1 font-lock-reference-face t)
     (2 font-lock-string-face t)))
  "Font Lock regexp for file-list-completion-list.")


(provide 'file-list-display)