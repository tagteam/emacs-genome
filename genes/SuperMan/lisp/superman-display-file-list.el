;; superman-display-file-list.el --- List files in projects

;; Copyright (C) 2014-2016  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <tag@biostat.ku.dk>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defun superman-view-file-list ()
  (interactive)
  (when superman-view-mode
    (superman-file-list (superman-view-current-project))))

(defun superman-file-list (project &optional ext)
  "List files in project's location that match extension EXT"
  (if (featurep 'superman-file-list)
      (let ((dir (superman-project-home project)))
	(cond ((file-list-select nil (or ext ".")
				 nil nil dir (concat "*FileList[" (car project) "]*")))
	      (t
	       (switch-to-buffer (concat "*FileList[" (car project) "]*"))
	       (toggle-read-only -1)
	       (erase-buffer)
	       (insert "FILE-LIST: No files in project"))))
    (error "superman-file-list.el not loaded")))

(defun superman-file-capture-button (c &rest args)
  (superman-make-button "track"
			'(:fun superman-capture-file-at-point
			       :face superman-next-project-button-face
			       :help "Capture file for project control")))

(defvar superman-file-list-balls
  '(("FileName" ("fun" superman-trim-string) ("width" 88) ("face" superman-file-name-face))
    ("Path" ("fun" superman-dont-trim) ("face" superman-directory-name-face)))
  ;; ("Capture" ("fun" superman-file-capture-button) ("width" 7) ("face" "no-face") ("face" superman-next-project-button-face))
  "balls used to display file-lists.")

  
(defun superman-display-file-list (dir &optional list filter sort balls view-buffer refresh no-project)
  "Format file-list displays."
  (let* ((project (if no-project nil
		    (ignore-errors (superman-view-current-project))))
	 (nick (if project (car project)
		 (or (get-text-property (point-min) 'nickname)
		     file-list-display-buffer)))
	 (pbuf (if file-list-mode
		   (get-text-property (point-min) 'project-buffer)
		 (buffer-name)))
	 (dir (or dir (get-text-property (point-min) 'dir)
		  (when project
		    (superman-project-home project))
		  (error "Missing location")))
	 (view-buf (or view-buffer (get-buffer-create (concat "*FileList[ " nick "]*"))))
	 (list (or list file-list-current-file-list))
	 ;; when balls are specified force level 0
	 ;; and provide file-name and directory name
	 (level (if balls 0 file-list-display-level))
	 (balls (if (= level 0)
		    '(("FileName" ("fun" superman-trim-string) ("width" 88) ("face" superman-file-name-face))
		      ("Path" ("fun" superman-dont-trim) ("face" superman-directory-name-face)))
		  '(("FileName" ("fun" superman-dont-trim)))))
	 (count 0)
	 (new-buffer (not file-list-mode))
	 cycle)
    (switch-to-buffer view-buf)
    (setq buffer-read-only t)
    (let ((buffer-read-only nil))
      (setq file-list-current-file-list list)
      (run-hooks 'superman-file-list-pre-display-hook)
      (if new-buffer
	  (progn
	    (org-mode)
	    (font-lock-mode -1)
	    (file-list-mode)
	    (goto-char (point-min))
	    (erase-buffer)
	    ;; create header (but only in new buffers)
	    ;; prepare buffer if necessary
	    (goto-char (point-min))
	    (insert (superman-make-button
		     (concat "FileList"
			     (concat ": " dir))
		     '(:fun file-list-reload
			    :face superman-project-button-face
			    :help "Refresh file-list")))
	    (unless no-project
	      (put-text-property (point-at-bol) (point-at-eol) 'nickname nick)
	      (put-text-property (point-min) (1+ (point-min)) 'project-buffer pbuf)
	      (put-text-property (point-at-bol) (point-at-eol) 'git-dir (superman-git-toplevel dir)))
	    (put-text-property (point-at-bol) (point-at-eol) 'dir dir)
	    (put-text-property (point-at-bol) (point-at-eol) 'filter filter)
	    (put-text-property (point-at-bol) (point-at-eol) 'sort sort)
	    (unless no-project
	      (put-text-property (point-at-bol) (point-at-eol) 'index (superman-get-index project))
	      (insert "  " (superman-make-button "Project view" '(:fun superman-view-back :face superman-next-project-button-face :help "Back to :width view."))
		      "  " (superman-make-button "Git" '(:fun superman-git-display :face superman-next-project-button-face :help "Control project's :width repository."))
		      "  " (superman-make-button "Todo" '(:fun superman-project-todo :face superman-next-project-button-face :help "View project's :width list."))
		      "  " (superman-make-button "Time-line" '(:fun superman-project-timeline :face superman-next-project-button-face :help "View project':width timeline."))))
	    (insert "\n")
	    (superman-view-insert-action-buttons
	     '((" attributes   " (:fun file-list-ls :face file-list-info-button-face :help "Show file attributes"))
	       ("  ls -la      " (:fun file-list-ls :face file-list-info-button-face :help "Show output of ls -la for all files"))
	       ("  grep        " (:fun file-list-grep :face file-list-info-button-face :help "Run grep on all files"))
	       ("  search      " (:fun file-list-grep :face file-list-info-button-face :help "Search through all files")))
	     nil
	     "Info     :")
	    (superman-view-insert-action-buttons
	     '((" rename files " (:fun file-list-rename :face file-list-action-button-face :help "Rename all files"))
	       (" copy files   " (:fun file-list-copy :face file-list-action-button-face :help "Copy all files to a new directory"))
	       (" delete files " (:fun file-list-remove :face file-list-action-button-face :help "Delete all files"))
	       ("find & replace" (:fun file-list-query-replace :face file-list-action-button-face :help "Run interactive query replace through all files"))
	       ("shell-command " (:fun file-list-shell-command :face file-list-action-button-face :help "Run the same shell command on all files"))
	       ;; (" update list  " file-list-reload nil "Update file-list")
	       )
	     nil
	     "Action   :")
	    (superman-view-insert-action-buttons
	     `(("  file-name   " (:fun file-list-by-name :face file-list-filter-button-face :help "Filter files with matching name"))
	       ("      .ext    " (:fun file-list-by-ext :face file-list-filter-button-face :help "Filter files with matching extension"))
	       ("     /path    " (:fun file-list-by-path :face file-list-filter-button-face :help "Filter files whose path do match"))
	       ("      time    " (:fun file-list-by-time :face file-list-filter-button-face :help "Filter files younger than"))
	       ("      size    " (:fun file-list-by-size :face file-list-filter-button-face :help "Filter files bigger than")))
	     nil
	     "FilterIn :")
	    (superman-view-insert-action-buttons
	     `(("  file-name   " (:fun (lambda () (interactive) (file-list-by-name 1)) :face file-list-inverse-filter-button-face :help "Filter files whose name doesn't match"))
	       ("      .ext    " (:fun (lambda () (interactive) (file-list-by-ext 1)) :face file-list-inverse-filter-button-face :help "Filter files whose extension doesn't match"))
	       ("     /path    " (:fun (lambda () (interactive) (file-list-by-path 1)) :face file-list-inverse-filter-button-face :help "Filter files whose path doesn't match"))
	       ("      time    " (:fun (lambda () (interactive) (file-list-by-time 1)) :face file-list-inverse-filter-button-face :help "Filter files older than"))
	       ("      size    " (:fun (lambda () (interactive) (file-list-by-size 1)) :face file-list-inverse-filter-button-face :help "Filter files smaller than")))
	     nil
	     "FilterOut:")
	    ;; ("clear display" file-list-clear-display)) 
	    (insert "\n* ")
	    (put-text-property (point-at-bol) (point-at-eol) 'file-list-section-start t)
	    (insert "\n"))
	;; end of header for new buffers
	;; 
	;; update existing buffer
	(delete-region 
	 (previous-single-property-change (point-max) 'column-names)
	 (point-max))
	(goto-char (previous-single-property-change (point-max) 'column-names))
	(end-of-line)
	(insert "\n"))
      ;; find maximal length of file-name
      (when (= level 0)
	(let ((maxfile (apply 'max (mapcar #'(lambda (x) (length (car x))) list))))
	  (setcdr (assoc "width" (assoc "FileName" balls)) `(,maxfile))))
      ;; insert the file-list 
      (while list
	(let* ((el (car list))
	       (file (car el))
	       (path (cadr el))
	       (rest (caddr el))
	       file-path
	       line
	       appendix)
	  (put-text-property 0 (length file) 'face 'superman-file-name-face file)
	  (put-text-property 0 (length path) 'face 'superman-directory-name-face path)
	  (setq file-path (cond ((= level 0)
				 `(list (("FileName" . ,file)
					 ("Path" . ,path))))
				((= level 1)
				 `(list (("FileName" . ,(concat path file)))))
				((= level 2)
				 `(list (("FileName" . ,(file-list-make-file-name~ el)))))
				((= level 3)
				 `(list (("FileName" . ,(concat "[[" path file "]]")))))))
	  (setq line (superman-format-thing file-path balls 'no-face))
	  (insert line)
	  (setq count (1+ count))
	  ;; results of grep and ls 
	  (while rest
	    (let ((key (caar rest))
		  (val (cdar rest)))
	      (put-text-property 0 (length key) 'face 'font-lock-warning-face key)
	      (setq appendix
		    (concat appendix
			    "\n"
			    (format "%13s" (caar rest))
			    " : " (cdar rest)))
	      (setq rest (cdr rest))))
	  ;; each beginning line has the filename saved as text-property
	  (put-text-property (point-at-bol) (1+ (point-at-bol)) 'filename (file-list-make-file-name el))
	  (put-text-property (point-at-bol) (1+ (point-at-bol)) 'superman-item-marker t)
	  (when appendix (insert appendix))
	  (insert "\n")
	  (setq list (cdr list))))
      (goto-char (next-single-property-change (point-min) 'file-list-section-start))
      (goto-char (point-at-bol))
      (kill-line)
      (when new-buffer
	(insert "\n\n"))
      ;; insert the section name		
      (superman-view-insert-section-name
       ;; dir
       "Files:"
       count
       balls
       nil
       'file-list-reload
       "Refresh view")
      (put-text-property (point-at-bol) (point-at-eol) 'file-list-section-start t)
      (insert " ")
      (insert (superman-make-button "Update list"
				    '(:fun file-list-reload
					   :face superman-next-project-button-face
					   :help "Update :width list"))
	      " "
	      (superman-make-button "Clear display"
				    '(:fun file-list-clear-display
					   :face superman-next-project-button-face
					   :help "Return to clean :width list"))
	      " "
	      (superman-make-button (concat "Mode: " (cond ((= level 0) "file | path")
							   ((= level 1) "/path/file")
							   ((= level 2) "~/path/file")
							   ((= level 3) "[[org-link]]")))
				    '(:fun file-list-toggle-display-mode
					   :face superman-next-project-button-face
					   :help "Change display of file-name and path-name")))
      ;; insert updated filters		
      (if (not file-list-filter)
	  (when (and (not new-buffer)
		     (next-single-property-change (point) 'active-filters))
	    (goto-char (next-single-property-change (point) 'active-filters))
	    (beginning-of-line)
	    (kill-line))
	;; (delete-region (point-at-bol) (point-at-eol)))
	;; insert (updated) filters
	(if new-buffer
	    (insert "\n")
	  (forward-line 1)
	  (delete-region (point-at-bol) (point-at-eol)))
	(insert 
	 (superman-make-button
	  "* Delete filters:" 
	  '(:fun file-list-remove-all-filters
		 :face superman-capture-button-face
		 :help "Press button to remove all filters"))
	 (concat " [" (int-to-string (length file-list-filter)) "]"))
	(put-text-property (1- (point)) (point) 'active-filters t)
	(save-excursion
	  (forward-line 1)
	  (unless (looking-at "^[ \t]*$")
	    (insert "\n"))))
      (when file-list-filter
	(dolist (x file-list-filter nil)
	  (insert " " (superman-make-button
		       (car x)
		       `(:fun (lambda () (interactive)
				(file-list-remove-filter ,(car x)))
			      :face file-list-active-filter-button-face
			      :help "Press button to remove this filter")))))
      ;; insert the column names
      (when new-buffer
	(insert "\n")
	(when superman-empty-line-after-cat (insert "\n"))
	;; (insert (superman-column-names balls))
	(insert "Sort:\t"
		(superman-make-button "file-name" '(:fun file-list-button-sort-by-name
							 :face superman-column-name-face
							 :help "Sort by :width name"))
		" "
		(superman-make-button "path" '(:fun file-list-button-sort-by-path
						    :face superman-column-name-face
						    :help "Sort by :width name"))
		" "
		(superman-make-button "time" '(:fun file-list-button-sort-by-time
						    :face superman-column-name-face
						    :help "Sort by :width modified"))
		" "
		(superman-make-button "size" '(:fun file-list-button-sort-by-time
						    :face superman-column-name-face
						    :help "Sort :width size")))
	(put-text-property (point-at-bol) (point-at-eol) 'column-names t)
	(beginning-of-line))
      (run-hooks 'superman-file-list-pre-display-hook))))

(defun file-list-button-sort-by-name ()
  (interactive)
  (let ((buffer-read-only nil)
	(reverse (get-text-property (previous-single-property-change (point) 'button) 'reverse))
	(here (point)))
    (when (get-text-property (point-at-bol) 'column-names)
      (put-text-property
       (previous-single-property-change (point) 'button)
       (next-single-property-change (point) 'button)
       'reverse (not reverse)))
    (file-list-sort-by-name reverse)
    (goto-char here)))

(defun file-list-button-sort-by-ext ()
  (interactive)
  (let ((buffer-read-only nil)
	(reverse (get-text-property (previous-single-property-change (point) 'button) 'reverse))
	(here (point)))
    (when (get-text-property (point-at-bol) 'column-names)
      (put-text-property
       (previous-single-property-change (point) 'button)
       (next-single-property-change (point) 'button)
       'reverse (not reverse)))
    (file-list-sort-by-ext reverse)
    (goto-char here)))

(defun file-list-button-sort-by-path ()
  (interactive)
  (let ((buffer-read-only nil)
	(reverse (get-text-property (previous-single-property-change (point) 'button) 'reverse))
	(here (point)))
    (when (get-text-property (point-at-bol) 'column-names)
      (put-text-property
       (previous-single-property-change (point) 'button)
       (next-single-property-change (point) 'button)
       'reverse (not reverse)))
    (file-list-sort-by-path reverse)
        (goto-char here)))

(defun file-list-button-sort-by-time ()
  (interactive)
  (let ((buffer-read-only nil)
	(reverse (get-text-property (previous-single-property-change (point) 'button) 'reverse))
	(here (point)))
	(when (get-text-property (point-at-bol) 'column-names)
	  (put-text-property
	   (previous-single-property-change (point) 'button)
	   (next-single-property-change (point) 'button)
	   'reverse (not reverse)))
    (file-list-sort-by-time reverse)
    (goto-char here)))

(defun file-list-button-sort-by-size ()
  (interactive)
  (let ((buffer-read-only nil)
	(reverse (get-text-property (previous-single-property-change (point) 'button) 'reverse))
	(here (point)))
	(when (get-text-property (point-at-bol) 'column-names)
	  (put-text-property
	   (previous-single-property-change (point) 'button)
	   (next-single-property-change (point) 'button)
	   'reverse (not reverse)))
    (file-list-sort-by-size reverse)
    (goto-char here)))

(defun superman-file-list-refresh-display (&optional file-list)
  (interactive)
  (if (not file-list-mode)
      (error "Works only in buffers with file-list-mode")
    (superman-display-file-list
     (get-text-property (point-min) 'dir)
     (or file-list file-list-current-file-list)
     (get-text-property (point-min) 'filter)
     (get-text-property (point-min) 'sort)
     (get-text-property (point-min) 'balls)
     (current-buffer)
     'refresh
     (not (get-text-property (point-min) 'project-buffer)))))

(defun file-list-reload ()
  "Update below the directory which is stored at point-min.
 Then remove non-existing files and redisplay file-list.
Finally apply filter stored at point-min and all elements of
 `file-list-filter'."
  (interactive)
  (let* ((dir (get-text-property (point-min) 'dir))
	 (main-filter (get-text-property (point-min) 'filter))
	 (active-filter-list
	  (mapcar 'car file-list-filter))
	 (flist
	  (progn
	    (setq file-list-filter nil)
	    (file-list-update-below-dir dir)
	    (file-list-select
	     nil
	     (nth 0 main-filter)
	     (nth 1 main-filter)
	     (nth 2 main-filter)
	     dir
	     nil t))))
    (while active-filter-list
      (let* ((filter (car active-filter-list))
	     (regexp (get-text-property 0 'regexp filter))
	     (by (get-text-property 0 'by filter))
	     (inverse (get-text-property 0 'inverse filter)))
	(setq flist
	      (file-list-select flist regexp by inverse dir nil t)))
      (setq active-filter-list (cdr active-filter-list)))
    ;; (file-list-select-existing-files)
    (setq file-list-current-file-list flist)
    (superman-file-list-refresh-display)))

(defun file-list-clear-display ()
  (interactive)
  (when file-list-mode
    (dolist (entry file-list-current-file-list)
      (when (> (length (cdr entry)) 1)
	(setcdr entry (list (car (cdr entry))))))
    (superman-file-list-refresh-display)))


(defun file-list-remove-all-filters ()
  (interactive)
  (setq file-list-filter nil)
  (file-list-reload))
  
(defun file-list-remove-filter (filter)
  (let ((ff (cdr (assoc filter file-list-filter))))
    (setq file-list-current-file-list
	  (append file-list-current-file-list (car ff)))
    (setq file-list-filter
	  (delete-if '(lambda (x) (string= (car x) filter)) file-list-filter))
    (superman-display-file-list
     (get-text-property (point-min) 'dir)
     file-list-current-file-list
     (get-text-property (point-min) 'filter)
     (get-text-property (point-min) 'sort)
     (get-text-property (point-min) 'balls)
     (current-buffer)
     'refresh
     (not (get-text-property (point-min) 'project-buffer)))))

  
(defun file-list-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map file-list-mode-map)
  (setq major-mode 'file-list-mode)
  (setq mode-name "file-list")
  (make-local-variable 'file-list-current-file-list)
  (make-local-variable 'file-list-filter)
  (make-local-variable 'file-list-display-level)
  (make-local-variable 'file-list-mode)
  (make-local-variable 'file-list-match-history)
  (setq file-list-mode t)
  (run-hooks 'file-list-display-hook))


(defvar file-list-display-help-string
  (concat ""
	  ;; (substitute-command-keys "list of matching files. here are some commands and keys. M-x `describe-bindings' shows all key-bindings\n\n`\\<file-list-mode-map>\\[file-list-choose-file]' find file `\\<file-list-mode-map>\\[file-list-choose-file-other-window]' in other window;`\\<file-list-mode-map>\\[file-list-choose-magic]' open, i.e. find with external program\n`S f', `S p', `S t', `S s' sorting and `/f' `/p' `/s' `/t' sub-selection by name, path, size, time\n `g' grep; `x' shell command; `d' dired; `t' toggle display\n\n")))
	  (substitute-command-keys "finding files: RET, SPACE, M-RET; sorting list: S f, S p, S t, S s; selecting from list: /f /p /s /t (by name, path, size, time)\nother commands: g grep; x shell command; d dired; t toggle display ...\n\n")))


(defun file-list-current-display-buffer ()
  (if file-list-mode
      (buffer-name (current-buffer))
    nil))


(defun file-list-quit (&optional force)
  "Close file-list window and switch to another buffer."
  (interactive)
  (let ((pbuf (get-text-property (point-min) 'project-buffer)))
    (if pbuf
	(progn
	  (kill-buffer (current-buffer))
	  (switch-to-buffer pbuf)
	  (superman-redo))
      (if (or force (not (one-window-p)))
	  (delete-window))
      (switch-to-buffer (other-buffer)))))
;; (file-list-mode
;; (superman-view-back))
;; (t (error "Not in file-list"))))

(defvar file-list-mode nil "Variable to indicate file-list-mode")


(defun file-list-switch-to-file-list ()
  (interactive)
  (let (w)
    (when (setq w (get-buffer-window file-list-display-buffer t))
      (select-window w)
      (file-list-beginning-of-file-list))))
	
(defun file-list-toggle-display-mode ()
  "Toggles display between the 4 stages
 (0) file | path
 (1) /path/file fullname
 (2) ~/path/to/file home replaced by ~
 (3) [[/path/file]] orgmode link
"
  (interactive)
  (setq file-list-display-level
	(cond ((= file-list-display-level 3) 2)
	      ((= file-list-display-level 2) 1)
	      ((= file-list-display-level 1) 0)
	      ((= file-list-display-level 0) 3)))
  (superman-file-list-refresh-display))


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
  "Return the absolute file-name at point and error if there is none."
  (let (fname)
    (cond (file-list-mode
	   (or (setq fname (get-text-property (point-at-bol) 'filename))
	       (let ((pos (previous-single-property-change (point)  'filename)))
		 (when pos
		   (setq fname (get-text-property (previous-single-property-change pos 'filename) 'filename))))))
	  (t (error (format "Works only in %s buffers." file-list-display-buffer))))
    (if (not fname) (error "No absolute filename at point!")
      (set-text-properties 0 (length fname) nil fname)
      (if (or (not exists-p)
	      (file-exists-p fname))
	  fname
	(error "file %s does not exist on disk" fname)))))
	

(defun file-list-end-of-file-list ()
  "Return the point of the end of displayed file-list."
  (when file-list-mode
    ;; (set-buffer file-list-display-buffer)
    (goto-char (point-max))
    (re-search-backward "^[^ \t\n]" nil t)
    (end-of-line)
    (point)))

(defun file-list-beginning-of-file-list ()
  "Return the point of the beginning of displayed file-list."
  (interactive)
  (when file-list-mode
    (goto-char (point-min))
    (re-search-forward "^current file-list:[\n]+" nil t)
    (re-search-forward "^[/a-zA-Z]+" nil t)
    (beginning-of-line)
    (point)))

(defun file-list-next-file (arg)
  "Move to the next item in the file list.
With prefix argument arg, move arg items (negative arg means move backward)."
  (interactive "p")
  (let ((beg (point-min)) (end (point-max)))
    (while (and (> arg 0) (not (eobp)))
      ;; If in a completion, move to the end of it.
      (when (get-text-property (point) 'filename)
	(goto-char (next-single-property-change (point) 'filename nil end)))
      ;; Move to start of next one.
      (unless (get-text-property (point) 'filename)
	(goto-char (next-single-property-change (point) 'filename nil end)))
      (setq arg (1- arg)))
    (while (and (< arg 0) (not (bobp)))
      (let ((prop (get-text-property (1- (point)) 'filename)))
	;; If in a completion, move to the start of it.
	(when (and prop (eq prop (get-text-property (point) 'filename)))
	  (goto-char (previous-single-property-change
		      (point) 'filename nil beg)))
	;; Move to end of the previous completion.
	(unless (or (bobp) (get-text-property (1- (point)) 'filename))
	  (goto-char (previous-single-property-change
		      (point) 'filename nil beg)))
	;; Move to the start of that one.
	(goto-char (previous-single-property-change
		    (point) 'filename nil beg))
	(setq arg (1+ arg))))))


(defun file-list-previous-file (arg)
  "Move to the previous item in file-list.
With prefix argument arg, move arg items backward."
  (interactive "p")
  (file-list-next-file (- arg)))
  
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


(provide 'superman-display-file-list)
;;; superman-display-file-list.el ends here

