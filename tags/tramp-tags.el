(require 'tramp)

(defun tramp-to-biostatlibrary ()
  (interactive)
  (find-file
   "/ssh:grb615@10.128.7.62:/srv/www/htdocs/bib/"))

;; (defun tramp ()
  ;; (interactive)
  ;; (find-file
   ;; "/ssh:grb615@doob:/home/ifsv/grb615/"))

(defun sync-with-work ()
  (interactive)
  (save-some-buffers)
  (let* ((dirname (directory-file-name (expand-file-name (read-directory-name "Directory to synchronize with grb615@doob: "))))
	 (mother (file-name-directory  dirname))
	 (remote-mother (replace-regexp-in-string
			   (expand-file-name "~")
			   "grb615@doob//home/ifsv/grb615"
			   mother)))
    (find-file "~/.unison/sync-with-work.prf")
    (erase-buffer)
    (insert "root = " mother
	    "\n" "root = ssh://" remote-mother
	    "\npath=" (file-name-nondirectory dirname) "\nignore = Name .*\nignore=Name *.o\nignore=Name *.so")
    (save-buffer)
    (async-shell-command "unison-gtk2 sync-with-work.prf")))
    ;; (insert (concat
	     ;; "rsync -e ssh -avzAHX --delete-after " dir-name " " remote-dir-name))))

(defun copy-from-work (&optional arg)
  (interactive "p")
  (let ((remote (read-file-name "Copy remote file: " "/grb615@doob:/home/ifsv/grb615/"))
	(local  (read-file-name "Destination:")))
    (copy-file remote local arg)))

(defun copy-to-work ()
  (interactive)
  (let* ((local  (read-file-name "Copy file:"))
	 (remote (read-directory-name "To remote directory: " "/grb615@doob:/home/ifsv/grb615/")))
    (copy-file local remote)))

(defun save-at-work ()
  (interactive)
  (let* ((file-name (buffer-file-name))
	 (remote-file-name  (replace-regexp-in-string
			     (expand-file-name "~")
			     "/grb615@doob:/home/ifsv/grb615/"
			     file-name)))
    ;; (if (y-or-n-p (concat "save file as " remote-file-name "? "))
    (write-file (replace-regexp-in-string
		 (expand-file-name "~")
		 "/grb615@doob:/home/ifsv/grb615/"
		 file-name))))
(defun get-from-work ()
  (interactive)
  (let* ((file-name (read-file-name "Get file: "
				    "/grb615@doob:/home/ifsv/grb615/" nil t))
	 (target (read-file-name (concat "Copy " file-name " to: ")
				 (replace-regexp-in-string
				  "/grb615@doob:/home/ifsv/grb615/"
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
			    "/grb615@doob:/home/ifsv/grb615/"
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
	       "/ssh:grb615@doob:/home/ifsv/grb615/"
	       file-name)
	    "/ssh:grb615@doob:/home/ifsv/grb615/")))
    (find-file remote-file-name)))



(provide 'tramp-tags)
