;; -*- lexical-binding: t; -*-
(require 'tramp)

(defun tramp-to-biostatlibrary ()
  (interactive)
  (find-file
   "/ssh:grb615@10.128.7.62:/srv/www/htdocs/bib/"))

;; (defun tramp ()
  ;; (interactive)
  ;; (find-file
   ;; "/ssh:grb615@rao:/maps/projects/biostat01/people/grb615/"))

(defun cox ()
  (interactive)
  (let ((PW (save-excursion
	      (find-file "~/.authinfo")
	      (goto-char (point-min))
	      (re-search-forward "exchange.ku.dk login grb615 port 587 password ")
	      (replace-regexp-in-string "[ \t\n]+" ""
					(buffer-substring-no-properties (point) (point-at-eol)))))
	(cox-buffer (get-buffer-create "*ess-cox*")))
    (shell cox-buffer)
    (goto-char (point-max))
    (insert "ssh grb615@cox")
    (comint-send-input)
    (sit-for 1)
    (insert PW)
    (comint-send-input)))

(defun sync-with-work (&optional dir)
  (interactive)
  (save-some-buffers)
  (let* ((dirname (or dir (directory-file-name (expand-file-name (read-directory-name "Directory to synchronize with grb615@rao: ")))))
	 (mother (file-name-directory  dirname))
	 (remote-mother (replace-regexp-in-string
			 (expand-file-name "~")
			 "grb615@rao//maps/projects/biostat01/people/grb615/"
			 mother)))
    (find-file "~/.unison/sync-with-work.prf")
    (erase-buffer)
    (insert "root = " mother
	    "\n" "root = ssh://" remote-mother
	    "\npath=" (file-name-nondirectory dirname) "\nignore = Name .*\nignore=Name *.o\nignore=Name *.so")
    (save-buffer)
    ;; (async-shell-command "unison-gtk2 sync-with-work.prf")
    ;; (switch-to-buffer (get-buffer-create "*sync-with-work*"))
    (superman-goto-shell)
    (goto-char (point-max))
    (comint-send-input)
    (insert "~/bin/sync-with-work")
    (comint-send-input)))
;; (insert (concat
;; "rsync -e ssh -avzAHX --delete-after " dir-name " " remote-dir-name))))

(defun copy-from-work (&optional arg)
  (interactive "p")
  (let ((remote (read-file-name "Copy remote file: " "/ssh:grb615@rao:/maps/projects/biostat01/people/grb615/"))
	(local  (read-file-name "Destination:")))
    (copy-file remote local arg)))

(defun copy-to-work ()
  (interactive)
  (let* ((local  (read-file-name "Copy file:"))
	 (remote (read-directory-name "To remote directory: " "/ssh:grb615@rao:/maps/projects/biostat01/people/grb615/")))
    (copy-file local remote)))

(defun save-at-work ()
  (interactive)
  (let* ((file-name (buffer-file-name))
	 (remote-file-name  (replace-regexp-in-string
			     (expand-file-name "~")
			     "/ssh:grb615@rao:/maps/projects/biostat01/people/grb615/"
			     file-name)))
    ;; (if (y-or-n-p (concat "save file as " remote-file-name "? "))
    (write-file (replace-regexp-in-string
		 (expand-file-name "~")
		 "/ssh:grb615@rao:/maps/projects/biostat01/people/grb615/"
		 file-name))))
(defun get-from-work ()
  (interactive)
  (let* ((file-name (read-file-name "Get file: "
				    "/ssh:grb615@rao:/maps/projects/biostat01/people/grb615/" nil t))
	 (target (read-file-name (concat "Copy " file-name " to: ")
				 (replace-regexp-in-string
				  "/ssh:grb615@rao:/maps/projects/biostat01/people/grb615/"
				  "~/"
				  file-name))))
    ;; (if (y-or-n-p (concat "save file as " remote-file-name "? "))
    (copy-file file-name target)))

(defun scp (&optional prompt)
  (interactive "P")
  (let* ((file-name (if prompt (read-file-name "Secure copy-file: ")
		      (buffer-file-name)))
	 (remote-file-name (replace-regexp-in-string
			    (expand-file-name "~")
			    "/ssh:grb615@rao:/maps/projects/biostat01/people/grb615/"
			    (expand-file-name file-name))))
    (if (y-or-n-p (concat "save file as " remote-file-name "? "))
	(write-file remote-file-name))))

(defun open-at-work ()
  (interactive)
  (let* ((file-name (buffer-file-name))
	 (remote-file-name
	  (if file-name
	      (replace-regexp-in-string
	       (expand-file-name "~")
	       "/ssh:grb615@rao:/maps/projects/biostat01/people/grb615/"
	       file-name)
	    "/ssh:grb615@rao:/maps/projects/biostat01/people/grb615/")))
    (find-file remote-file-name)))



(provide 'tramp-tags)
