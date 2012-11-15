;;; file-list-action.el --- file-list actions

;; Copyright (C) 2012  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <tag@biostat.ku.dk>
;; Keywords: files, convenience

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




;;; commands on all files in file-list or just on the file-at-point
;;; ----------------------------------------------------------------
;;

(defun file-list-quote-filename (name)
  ;; for shell commands
  (concat "\"" name "\""))


(defun file-list-choose-file (arg &optional event extent buffer magic)
  (interactive "P")
  (let ((grep-line
	 (save-excursion
	   (beginning-of-line)
	   (if (looking-at "\\(^[ \t]+\\)\\([0-9]+\\)\\( : \\)")
	       (match-string 2) nil)))
	(file-name (file-list-file-at-point))
	fun)
    (cond ((and magic
		(if arg
		    (setq fun (read-shell-command (format "icommand on %s " file-name)
						  nil nil nil))
		  (setq fun (cdr (assoc-ignore-case
				  (concat "\." (file-name-extension
						file-name))
				  file-list-magic-alist)))))
	   (async-shell-command
	    (concat "PATH=~/bin:\"${PATH}\";" fun " "
		    (file-list-quote-filename file-name))))
    ; (start-process-shell-command
	   ;; "file-list-find-magic"
	   ;; nil
	   ;; (concat fun " "
	   ;; (file-list-quote-filename file-name))))
	   (t
	    (cond ((not arg) nil)
		  ((= arg 4) (switch-to-buffer-other-window (current-buffer)))
		  ((= arg 5) (switch-to-buffer-other-frame (current-buffer)))
		  (t nil))
	    (find-file file-name)
	    (when grep-line
	      (goto-line (string-to-int grep-line)))))))

(defun file-list-choose-file-other-window (&optional event extent buffer magic)
  (interactive)
  (file-list-choose-file 4 event extent buffer magic))
				   
(defun file-list-choose-magic (arg &optional event extent buffer)
  (interactive "P")
  (file-list-choose-file arg event extent buffer 'magic))


(defun file-list-omit-file-at-point (&optional arg)
  "Omit entry of filename at point from current file list.
If ARG keep only filename at point."
  (interactive "P")
  (let* ((c-list (progn (set-buffer (file-list-current-display-buffer)) file-list-current-file-list))
	 ;; (gc-cons-threshold file-list-gc-cons-threshold)
	 (filename (file-list-file-at-point))
	 (nth-in-list (file-list-nth-in-list filename file-list-current-file-list))
	 (steps (if (> nth-in-list 0) (- nth-in-list 1) 0))
					; 	 (dlev file-list-display-level)
	 currline destline)
					;     (if (= dlev 2) (setq file-list-display-level 1))
    (if arg (progn
	      (setq file-list-current-file-list
		    (list (file-list-make-entry filename)))
	      (file-list-display-match-list))
      (setq file-list-current-file-list
	    (delete (nth nth-in-list file-list-current-file-list)
		    file-list-current-file-list))
      (file-list-display-match-list)
      (file-list-next-file 1)
      (file-list-previous-file 1)
      )))
;; try to find the old position of point ... does not work for display level 2
;; (when (> steps 0)
;; (setq currline (line-number (point)))
;; (save-excursion
;; (setq destline
;; (line-number (progn (file-list-next-file steps) (point)))))
;; (progn (scroll-up-in-place (- destline currline))
;; (center-to-window-line))))))


;; commands
;; --------------------------------------------------------------------



(defun file-list-find (arg &optional file-list)
  (interactive "p")
  (cond ((not arg) nil)
	((= arg 4) (switch-to-buffer-other-window (current-buffer)))
	((= arg 5) (switch-to-buffer-other-frame (current-buffer)))
	(t nil))
  (let ((file-list (or file-list file-list-current-file-list)))
    (mapcar (lambda (entry)
	      (find-file (file-list-make-file-name entry)))
	    file-list)))


(defun file-list-dired (&optional file-list dir)
  (interactive)
  (let* ((file-list (or file-list file-list-current-file-list)))
    (dired (cons file-list-home-directory
		 (mapcar 'file-list-make-file-name file-list)))))


(defun file-list-shell-command-at-point (&optional file-list)
  (interactive)
  (let* ((fatp (file-list-file-at-point))
	 (command (read-shell-command "Shell-command "
				      nil
				      nil
				      shell-command-history)))
    (shell-command (concat "cd "
			   (file-name-directory fatp) ";"
			   command " " 	      (file-list-quote-filename fatp)))))
    
	

(defun file-list-shell-command (&optional file-list)
  "Shell command on all files in file-list-current-file-list.
Switches to the corresponding directory of each file."
  (interactive)
  (let ((file-list (or file-list file-list-current-file-list))
	(command (read-shell-command "Shell-command "
				     nil
				     nil
				     shell-command-history)))
    (dolist (f file-list)
      (let* ((fname (file-list-make-file-name f))
	     (fcommand (concat command " " (file-list-quote-filename fname))))
	(message fcommand)
	(shell-command
	 (concat "cd " (file-name-directory fname) ";" fcommand))))))


(defun file-list-rename-file-at-point ()
  (interactive)
  (let* ((oldname (file-list-file-at-point))
	 (newname (expand-file-name (read-file-name (format "Enter new name for %s " oldname)))))
    (rename-file oldname newname 'ok)
    (file-list-update-file-alist oldname 'delete)
    (file-list-update-file-alist newname)
    (file-list-update-current-file-list oldname newname)
    (file-list-display-match-list)
    (re-search-forward newname nil t)))



(defun file-list-rename (&optional file-list one-by-one)
  (interactive)
  (if one-by-one
      (let ((file-list (or file-list file-list-current-file-list)))
	(dolist (entry file-list)
	  (let* ((oldname (file-list-make-file-name entry))
		 (newname
		  (read-file-name (format "Enter new name for %s " (car entry))
				  (cdr entry)))
		 (newdir (if (not (file-name-absolute-p newname))
			     (file-name-directory oldname)
			   (file-name-directory (expand-file-name newname))))
		 (filename (file-name-nondirectory newname)))
	    (unless (and newdir (file-directory-p newdir))
	      (if (yes-or-no-p
		   (format "Directory %s does not exist create it? "
			   newdir))
		  (make-directory newdir)))
	    (setq filename
		  (file-list-make-file-name
		   (cons filename newdir)))
;	  (shell-command (concat "mv " oldname " " filename))
	    (rename-file oldname filename)
	    ;; update current file list
	    (dolist (entry file-list-current-file-list)
	      (setcar entry filename) (setcdr entry newdir)))))
    (let* ((file-list (or file-list file-list-current-file-list))
	   (old-regexp (read-string 
			"Regexp matching all filenames "))
	   (replacement (read-string
			 (format "Replace match ('%s') with " old-regexp))))
      (dolist (entry file-list)
	(let* ((old-name (car entry))
	       (new-name (file-list-replace-in-string old-name old-regexp replacement nil)))
	  (setcar entry new-name)
	  (rename-file (concat (cadr entry) old-name)
		       (concat (cadr entry) new-name) t)))))
  (file-list-display-match-list file-list-current-file-list))


(defun file-list-move (&optional ask file-list target copy)
  (interactive "P")
  (let* ((file-list-intern (or file-list file-list-current-file-list))
	 (target (or target
		     (expand-file-name
		      (file-name-as-directory
		       (read-directory-name
			"Target directory "
			nil nil nil)))))
	 (use-path (if ask (yes-or-no-p "Use path as part of file-name? "))))
    (cond ((file-directory-p target) nil)
	  ((yes-or-no-p (format "Directory %s does not exist create it? "
				target))
	   (make-directory target))
	  ((yes-or-no-p "Abort moving files ") (file-list-quit))
	  (t (file-list-move nil file-list-intern nil)))
    (while file-list-intern
      (let* ((entry (car file-list-intern))
	     (oldname (file-list-make-file-name entry))
	     (newcar (if use-path
			 (concat (replace-in-string (replace-in-string (car (cdr entry)) "^/" "") "/" "_") (car entry))
		       (car entry)))
	     (newname (file-list-make-file-name (cons newcar (list target))))
	     (ilist (mapcar 'car (setq file-list-intern (cdr file-list-intern)))))
	;; stop if there are duplicate (non-absolute) file-names in current file-list
	(if (member newcar ilist)
	    (error "Duplicate file-name '%s' in current file-list!" (car entry))
	  (if copy (progn
		     (condition-case nil
			 (copy-file oldname newname nil)
		       (error
			(progn
			  (message (concat "Could not move" oldname " to " newname ". File exists"))
			  nil))))
	    (if (condition-case nil
		    (rename-file oldname newname nil)
		  (error
		   (progn
		     (message (concat "Could not move" oldname " to " newname))
		     nil)))
		(message (concat "Successfully moved " oldname " to " newname))))
	  (if copy
	      (setq file-list-current-file-list
		    (append file-list-current-file-list (list (file-list-make-entry newname))))
	    (if file-list
		(setcar (cdr (assoc (car entry) file-list-current-file-list)) target)
	      (setcar (cdr entry) target))))))
    (file-list-display-match-list file-list-current-file-list)))


(defun file-list-move-file-at-point (&optional ask)
  (interactive "P")
  (let ((file (file-list-file-at-point)))
    (file-list-move ask (list (file-list-make-entry file)) nil)))


(defun file-list-copy (&optional ask file-list target path)
  (interactive "P")
  (file-list-move ask file-list target 'copy))

(defun file-list-copy-file-at-point (&optional ask)
  (interactive "P")
  (let ((file (file-list-file-at-point)))
    (file-list-copy ask (list (file-list-make-entry file)) nil)))


(defun file-list-ls (&optional file-list nodisplay)
  (interactive)
  (let* ((gc-cons-threshold file-list-gc-cons-threshold)
	 (file-list-display-level 2)
	 (file-list (or file-list file-list-current-file-list))
	 (attr-list
	  (mapcar
	   (lambda (attr)
	     (let* ((rest (split-string
			   (file-list-replace-in-string attr "[ \t]+" " ") " ")))
	       (cons (car (reverse rest))
		     (list (cons "mod" (car rest))
			   (cons "owner" (caddr rest))
			   (cons "group" (nth 3 rest))
			   (cons "size" (nth 4 rest))
			   (cons "created"
				 (concat (nth 5 rest)
					 " "
					 (nth 6 rest)
					 " "
					 (nth 7 rest)))))))
	   (split-string (shell-command-to-string
			  (concat "ls -l "
				  (file-list-concat-file-names file-list)))
			 "\n"))))
    (setq file-list-current-file-list
	  (mapcar
	   (lambda (entry)
	     (let ((add (cdr (assoc
			      (file-list-make-file-name entry)
			      attr-list))))
	       (if add (list (car entry) (cadr entry) add)
		 entry)))
	   file-list))
    (unless nodisplay
      (file-list-display-match-list file-list-current-file-list))))


(defun file-list-attributes (&optional file-list nodisplay)
  (interactive)
  (let ((gc-cons-threshold file-list-gc-cons-threshold)
;	(file-list-display-level 2)
	(file-list (file-list-select-existing-files
		    (or file-list file-list-current-file-list))))
    (setq file-list-current-file-list
	  (mapcar
	   (lambda (entry)
	     (let ((add (file-attributes (file-list-make-file-name entry))))
	       (list (car entry) (cadr entry)
		     (list (cons "mod" (nth 8 add))
			   (cons "time"
				 (format-time-string file-list-format-time-string
						     (nth 5 add)))
;			   (cons "status-time"
;				 (format-time-string file-list-format-time-string
;						     (nth 6 add)))
			   
;			   (cons "access-time"
;				 (format-time-string file-list-format-time-string
;						     (nth 4 add)))
			   
;     "Human-readable" output.  Use unit suffixes: Byte, Kilobyte,
;     Megabyte, Gigabyte, Terabyte and Petabyte in order to reduce the
;     number of digits to four or fewer using base 2 for sizes.
;     (FreeBSD man page of C<df>: http://www.freebsd.org/cgi/man.cgi?query=df)

;   byte      B
;   kilobyte  K = 2**10 B = 1024 B
;   megabyte  M = 2**20 B = 1024 * 1024 B
;   gigabyte  G = 2**30 B = 1024 * 1024 * 1024 B
;   terabyte  T = 2**40 B = 1024 * 1024 * 1024 * 1024 B

;   petabyte  P = 2**50 B = 1024 * 1024 * 1024 * 1024 * 1024 B
;   exabyte   E = 2**60 B = 1024 * 1024 * 1024 * 1024 * 1024 * 1024 B
;   zettabyte Z = 2**70 B = 1024 * 1024 * 1024 * 1024 * 1024 * 1024 * 1024 B
;   yottabyte Y = 2**80 B = 1024 * 1024 * 1024 * 1024 * 1024 * 1024 * 1024 * 1024 B
			   (cons "size"
; 				 (int-to-string (nth 7 add))
				 (file-list-convert-bytes (nth 7 add)))))))
	   file-list))
    (unless nodisplay
      (setq file-list-display-level 2)
      (file-list-display-match-list file-list-current-file-list)))
  file-list-current-file-list)

(defun file-list-convert-bytes (int)
  (let* ((string (int-to-string int))
	 (len (length string))
 	 mega kilo byte)
    (if (> len 10)
	"huge (> 1G)"
      (setq mega (/ int 1048576)
	    kilo (/ (mod int 1048576) 1024)
	    byte (if (> kilo 0) (mod int kilo) int))
      (concat (when (> mega 0) (concat (int-to-string mega) "M "))
	      (when (or (> mega 0) (> kilo 0)) (concat (int-to-string kilo) "K "))
	      (int-to-string byte) "B "))))
      
(defun file-list-remove-file-at-point ()
  (interactive)
  (let* ((fname (file-list-file-at-point))
	 (do-it (yes-or-no-p (format "Delete %s ? " fname))))
    (when do-it (delete-file fname))
    (file-list-update-file-alist fname 'delete)
    (setq file-list-current-file-list
	  (delete nil (mapcar (lambda (entry)
				(unless (string= (file-list-make-file-name entry) fname)
				  entry)) file-list-current-file-list)))
    (file-list-display-match-list)
    (file-list-beginning-of-file-list)))
    ;; (file-list-switch-to-file-list)))


(defun file-list-remove (&optional file-list)
  (interactive)
  (let ((gc-cons-threshold file-list-gc-cons-threshold)
	;; (fdbuf (get-buffer file-list-display-buffer))
	(file-list (or file-list file-list-current-file-list)))
    (file-list-display-match-list file-list)
    (when;;  (and
	   ;; (equal (current-buffer) fdbuf)
	   (yes-or-no-p
	    "Really really move all these files to /dev/null in the sky? ")
      ;;)
      (file-list-select-existing-files file-list)
      (dolist (fn file-list)
	(delete-file (file-list-make-file-name fn)))
      ;; update file-list-current-file-list and file-list-alist
      (setq file-list-current-file-list nil)
      (file-list-display-match-list)
;      (file-list-update file-list-home-directory)
;      (file-list-quit)
      )))
      


(defun file-list-grep (&optional file-list)
  (interactive)
  (when (eq major-mode 'file-list-completion-mode)
    ;; (file-list-switch-to-file-list)
    (let* ((file-list (or file-list file-list-current-file-list))
	   (grep-regexp (read-string
			 "Regexp for grep on files in current file-list: "
			 nil file-list-grep-history))
	   (files (file-list-concat-file-names file-list))
	   (file-list-buffer (current-buffer))
	   (hits-buf (get-buffer-create "*file-list-grep-hits*")))
      (message "Waiting for grep ...")
      (shell-command
       (concat "grep -n " grep-regexp " "  files " " "/dev/null")
       hits-buf)
      (if (not (buffer-live-p hits-buf))
	  (error "No grep hits for '%s'." grep-regexp)
	(setq file-list-display-level 2))
      (save-excursion 
	(set-buffer hits-buf)
	(goto-char (point-min))
	(let (grep-hits grep-hit-list file-name)
	  (while (re-search-forward "\\(^/.*\\)\\(:[0-9]+:\\)\\(.*$\\)" nil t)
	    (setq file-name (match-string 1))
	    (setq grep-hits (list (cons (substring
					 (match-string 2) 1 -1)
					(match-string 3))))
	    (while (re-search-forward (concat "\\(" file-name "\\)\\(:[0-9]+:\\)\\(.*$\\)") nil t)
	      (setq grep-hits
		    (append grep-hits
			    (list (cons (substring (match-string 2) 1 -1)
					(match-string 3)))))
	      (forward-line 1))
	    (setq grep-hit-list (append (list (cons file-name grep-hits))
					grep-hit-list)))
	  (kill-buffer hits-buf)
	  (switch-to-buffer file-list-buffer)
	  (file-list-beginning-of-file-list)
	  ;; (file-list-switch-to-file-list)
	  (if (not grep-hit-list)
	      (message "No grep hits for '%s'." grep-regexp)
	    ;;replace current entry with new entry
	    (setq file-list-current-file-list
		  (mapcar
		   (lambda (entry)
		     (let ((add (cdr (assoc
				      (file-list-make-file-name entry)
				      grep-hit-list))))
		       (if add
			   (list (car entry) (cadr entry)
				 (if (> (length entry) 2)
				     (append (caddr entry) add)
				   add))
			 entry)))
		   file-list)))
	  (file-list-display-match-list file-list-current-file-list))))))

; (defun file-list-restrict-to-matching-files (&optional file-list)
;   (interactive)
;   (let ((file-list (or file-list file-list-current-file-list))
; 	file)
;     (while pfile-list
;       (setq
;       (if (and (file-list-
;     (setq file-list-current-file-list
; 	  (delete (nth nth-in-list file-list-current-file-list)
; 		  file-list-current-file-list))
;     (file-list-display-match-list file-list)

(provide 'file-list-action)
;;; file-list-action.el ends here
