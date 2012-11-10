;; utility functions 
;; --------------------------------------------------------------------


(defun file-list-clear-home ()
  "Delete all files whose names are matched by file-list-clear-home-regexp."
  (interactive)
  (setq file-list-current-file-list nil)
  (save-window-excursion
    (let ((death-row (file-list-select-existing-files
		      (file-list-extract-sublist
		       (file-list-list
			(getenv "HOME"))
		       file-list-clear-home-regexp))))
      (if (null death-row)
	  (message (format "Your $HOME is clean: there are no files matching %s"
			   file-list-clear-home-regexp))
	(file-list-display-match-list death-row)
	(file-list-remove death-row)
	(message nil)))))


(defun file-list-tar-current-file-list () ;(file-list &optional untar)
  (interactive)
  (let ((file-list file-list-current-file-list)
	(tarfile
; 	 (unless untar
	 (expand-file-name
	  (read-file-name
	   "Name for new tar file': "
	   nil nil nil))))
;     )(if untar (while file-list
; 		(let ((filename (file-list-make-file-name
; 				 (car file-list))))
; 		  (when (string= (file-name-extension filename)
; 				 "tar")
; 		    (shell-command (concat
; 				    "tar xvf "
; 				    filename)))
; 		  (setq file-list (cdr file-list))))
    (shell-command (concat  "tar cvf " tarfile " "
			    (file-list-concat-file-names file-list)))
    (setq file-list-current-file-list
	  (add-to-list 'file-list-current-file-list
		       (file-list-make-entry tarfile) 'append))
    (file-list-display-match-list file-list-current-file-list)
    (file-list-update-file-alist tarfile)
    ))


(defun file-list-mml-attach-file-at-point ()
  "Attach file at point to the outgoing MIME message."
  (interactive)
  (let* ((fatpoint (file-list-file-at-point))
	 (type (mm-default-file-encoding fatpoint))
	 (description nil)
	 (buf (if (save-excursion (set-buffer file-list-reference-buffer)
				  (string-match "message" (symbol-name
							   major-mode)))
		  file-list-reference-buffer
		(read-buffer "attach to buffer "
			     (find-if (lambda (b)
					(save-excursion (set-buffer b)
							(string= "message-mode" (buffer-name b))))
				      (buffer-list))
			     t))))
    (switch-to-buffer buf)
    (goto-char (point-max))
    (mml-insert-empty-tag 'part
			  'type type
			  'filename fatpoint
			  'disposition "attachment"
			  'description description))
  (file-list-switch-to-file-list))


(defun file-list-mml-attach (&optional file-list)
  "Attach files in file-list to the outgoing MIME message."
  (interactive)
;  (save-window-excursion
    (let* ((file-list (or file-list file-list-current-file-list))
	   (flist (mapcar 'file-list-make-file-name file-list))
	   (active (string= (buffer-name (current-buffer))
			    file-list-display-buffer))
	   (buf (cond ((not active) (current-buffer))
		      ((string-match "mail" (buffer-name file-list-reference-buffer))
		       file-list-reference-buffer)
		      (t (read-buffer "Attach all these files to buffer " 
				      (find-if (lambda (b)
					(save-excursion (set-buffer b)
							(string= "message-mode" (buffer-name b))))
				      (buffer-list)) t))))
	 file type description)
      (switch-to-buffer buf)
      (while flist
	(setq file (car flist)
	      type (mm-default-file-encoding file)
	    description nil)
	(mml-insert-empty-tag 'part
			      'type type
			      'filename file
			      'disposition "attachment"
			    'description description)
	(setq flist (cdr flist)))
      (file-list-quit t)))


		     
;(defun file-list-dummy (&optional file-list)
;  (interactive)
;  (let* ((file-list (or file-list file-list-current-file-list)))
;    (dolist (file file-list)
;      (save-window-excursion
;	(find-file (file-list-make-file-name file))
;	(save-restriction
;	  (widen)
;	  (goto-char (point-min))
;	  (re-search-forward "RQ91" nil t)
;	  (insert "\n")
;;	  (insert "RQ70 RQ71 RQ72 RQ73 RQ74 RQ75 RQ76 RQ77 RQ78 RQ79 RQ80 RQ81 RQ82 RQ83 RQ84 RQ85 RQ86 RQ87 RQ88 RQ89 RQ90 RQ91")
;	  (save-buffer))))))


		     
(defun file-list-query-replace (&optional file-list)
  (interactive)
  (let* ((buffer-read-only nil)
	 (file-list (or file-list file-list-current-file-list))
	 (args (query-replace-read-args "Query-replace" nil)))
    (dolist (file file-list)
      (save-window-excursion
	(find-file (file-list-make-file-name file))
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (query-replace (car args) (cadr args))
	  (save-buffer))))))


(defun file-list-replace (&optional file-list)
  (interactive)
  (let* ((file-list (or file-list file-list-current-file-list))
	 (string (read-string "String to replace: "))
	 (replacement (read-string "Replacement string: ")))
    (dolist (file file-list)
      (save-window-excursion
	(find-file (file-list-make-file-name file))
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (while (re-search-forward string nil t)
	    (replace-match replacement))
	  (save-buffer)
	  (kill-this-buffer))))))
  

(defun file-list-downcase-filenames (&optional file-list)
  (interactive)
  (let ((file-list (or file-list file-list-current-file-list)))
    (dolist (entry file-list)
      (let* ((oldname (file-list-make-file-name entry))
	     (newname (file-list-make-file-name (cons (downcase (car entry)) (cdr entry)))))
	(rename-file oldname newname)))
    ;; update current file list
    (dolist (entry file-list-current-file-list)
      (setcar entry (downcase (car entry)))))
  (file-list-display-match-list file-list-current-file-list))


;(defun file-list-compress (&optional file-list method)
;  (interactive)
;  (let* ((file-list (or file-list file-list-current-file-list))
;	 (method (or method "gzip")))
;    (file-list-select-existing-files file-list)
;    (dolist (fcons file-list)
;      (let* ((fname (file-list-make-file-name fcons))
;	     (success (shell-command-to-string (concat method " " fname))))
;	(unless (string= success "")
;	  (file-list-display-match-list)
;	  (error success))
;	(setcar fcons (concat (car fcons) ".gz"))))
;    (file-list-display-match-list)))


(defun file-list-choose-dir (arg &optional event extent buffer)
  (interactive "P")
  (let ((dirname
	 (progn (set-list-mode-extent)
		(extent-string (extent-at (point))))))
    (file-list-select-internal nil nil nil nil dirname)))



(defun file-list-diagnosis (&optional dir)
  (interactive)
  (get-buffer-create "*file-list-diagnosis*")
  (switch-to-buffer "*file-list-diagnosis*")
  (erase-buffer)
  (let* ((gc-cons-threshold file-list-gc-cons-threshold)
	 (dir (or dir file-list-home-directory))
	 (dirlist (file-list-list dir t t t))
	 (overall 0) 
	 (diaglist (sort (let ((tail dirlist) dlist entry size)
			   (while tail 
			     (if (setq entry (assoc (cadr (car tail)) dlist))
				 (progn (setq size (nth 7 (file-attributes (file-list-make-file-name (car tail)))))
					;; size can be out of range ...
					(if (= size -1) nil
					  (setcdr entry (+ size (cdr entry)))
					  (setq overall (+ size overall))))
			       (setq dlist (cons (cons (cadar tail) 0) dlist)))
			     (setq tail (cdr tail)))
			   dlist)
			 (lambda (a b) (> (cdr a) (cdr b)))))
	 ;;1 KByte=2^10 Bytes
	 ;;1 Mbyte=2^20 Bytes
	 ;;1 GByte=2^30 Bytes
	 ;;1 Tbyte=2^40 Bytes
	 (breaks `(,(cons 1048576 "1 Gigabyte")
		   ,(cons 512000 "500 Megabyte")
		   ,(cons 204800 "200 Megabyte")
		   ,(cons 102400 "100 Megabyte")
		   ,(cons 1024 "1 Megabyte")
		   ,(cons 0 "0 Byte")
		   ,(cons -1 nil)))
	 (break (car breaks))
	 (first t)
	 lastbreak)
    (insert (make-string (window-width (selected-window)) (string-to-char "#")) 
	    "\n\nSize of all assessable files below " dir "\n"
	    "Gigabyte: " (int-to-string (/ overall (* 1024 1024 1024))) "\n"
	    "Megabyte: " (int-to-string (/ overall (* 1024 1024))) "\n"
	    "Kilobyte: " (int-to-string (/ overall 1024)) "\n\n"
	    (make-string (window-width (selected-window)) (string-to-char "#")) "\n")
    (while diaglist
      (let* ((y (car diaglist))
	     (name (car y))
	     (size-in-kbytes (/ (cdr y) 1024)))
	(cond ((and first (>= size-in-kbytes (car break)))
	       (insert "\n"
		       (make-string (window-width (selected-window)) (string-to-char "~"))
		       "\n"
		       (cond ((= (car lastbreak) 0)
			      "0 Bytes")
			     (lastbreak
			      (concat "Cumulative size (Kilobytes) between " (cdr break) " and " (cdr lastbreak)))
			     (t (concat "Cumulative size (Kilobytes) >= " (cdr break))))
		       "\n"
		       (make-string (window-width (selected-window)) (string-to-char "~"))
		       "\n\n")
	       (insert name " : " (int-to-string size-in-kbytes) "\n")
	       (setq first nil))
	      ((>= size-in-kbytes (car break)) (insert name " : " (int-to-string size-in-kbytes) "\n"))
	      (t (setq lastbreak break)
		 (setq breaks (cdr breaks))
		 (setq break (car breaks))
		 (setq first t)))
	(if (not first) (setq diaglist (cdr diaglist))))))
  (goto-char (point-min)))



(provide 'file-list-util)