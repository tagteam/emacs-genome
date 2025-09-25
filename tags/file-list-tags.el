;;; file-list  -*- lexical-binding: t; -*- 
;;; ------------------------------------------------------------

;; (add-to-list 'load-path
;; (expand-file-name 
;; (concat (getenv "HOME") "/emacs-genome/genes/file-list/")))

;; (require 'file-list)
;; (require 'file-list-vars)
;; (require 'file-list)

;; (setq file-list-verbose nil
;; file-list-exclude-dirs nil)
;; (list
;; (cons file-list-home-directory 
;; "\\(\\\.[a-w]\\|#\\|auto/\\|local/\\|source\\|autosave\\|News\\|etc\\)")))

; copy to mailcap-mime-data?
(setq file-list-magic-alist
      '((".dvi"      . "xdvi")
	(".eps"      . "gv")
	(".prn"      . "gv")
	(".doc"      . "oowriter")
	(".xls"      . "oocalc")
	(".gif"      . "gqview")
	(".icon"     . "gqview")
	(".ief"      . "gqview")
	(".jpe"      . "gqview")
	(".pdf"      . "acroread")
	(".ps"       . "gv")
	(".eps"      . "gv")
	(".psF"      . "gv")
	(".tif"      . "gqview")
	(".tiff"     . "gqview")
	(".xbm"      . "gqview")
	(".xpm"      . "gqview")
	(".jpg"      . "gqview")
	(".jpeg"     . "gqview")))



; (defun file-list-tramp (arg &optional file-list)
;   (interactive "P")
;   (let* ((user (read-string "User name on foreign server (default gerds): " nil nil "gerds"))
; 	 (method (completing-read "Method for tramp (default ssh): " tramp-methods nil t nil nil "ssh"))
; 	 (host (read-string "Address of host (e.g. sun6.imbi.uni-freiburg.de or www): "))
; 	 (dir (read-string (format "List files in dir (e.g.%s or /var/www/htdocs/people) below %s: " (concat "~" user) (concat "/[" method "/" user "@" host "]/"))))
; 	 (tramp-dir (concat "/[" method "/" user "@" host "]" dir)))
;     (file-list-select file-list nil nil arg tramp-dir)))


(defun p-hely-mail ()
  "Attach file visited by current buffer to a new mail" 
  (interactive)
  (let* ((thefile (buffer-file-name))
	 (type (mm-default-file-encoding thefile))
	 (description nil))
    (gnus-group-mail)
    (goto-char (point-max))
    (insert "\n")
    (mml-insert-empty-tag 'part
			  'type type
			  'filename thefile
			  'disposition "attachment"
			  'description description)))

(defun file-list-mml-attach (&optional file-list)
  "Attach files in file-list to the outgoing MIME message."
  (interactive)
  ;; (save-window-excursion
  (let* ((file-list (or file-list file-list-current-file-list))
	 (flist
	  (progn
	    (mapcar
	     #'(lambda (x)(set-text-properties 0 (length (car x)) nil (car x))(set-text-properties 0 (length (cadr x)) nil (cadr x)) x)
	     file-list)
	    (mapcar 'file-list-make-file-name file-list)))
	 (active file-list-mode)
	 (buf (cond ((not active) (current-buffer))
		    ((file-list-message-buffer-p file-list-reference-buffer))
		    ((save-excursion (file-list-message-buffer-p (progn (other-window 1) (window-buffer)))))
		    (t (read-buffer "Attach all these files to buffer " 
				    (cl-find-if (lambda (b)
					       (save-excursion (set-buffer b)
							       (string= "message-mode" (buffer-name b))))
					     (buffer-list)) t))))
	 file type description)
    (switch-to-buffer-other-window buf)
    (while flist
      (setq file (car flist)
	    type (mm-default-file-encoding file)
	    description nil)
      (save-excursion
	(goto-char (point-max))
	(insert "\n")
	(mml-insert-empty-tag 'part
			      'type type
			      'filename file
			      'disposition "attachment"
			      'description description))
      (setq flist (cdr flist)))))
;; )
;; (file-list-quit t)))


(defun file-list-mml-attach-file-at-point ()
  "Attach file at point to the outgoing MIME message."
  (interactive)
  ;; (save-window-excursion
  (let* ((fatpoint (file-list-file-at-point))
	 (type (mm-default-file-encoding fatpoint))
	 (description nil)
	 ;; find the message buffer
	 (buf (cond ((file-list-message-buffer-p file-list-reference-buffer))
		    ((save-excursion (file-list-message-buffer-p (progn (other-window 1) (window-buffer)))))
		    (t (read-buffer "attach to buffer "
				    (cl-find-if (lambda (b)
					       (save-excursion (set-buffer b)
							       (string= "message-mode" (buffer-name b))))
					     (buffer-list))
				    t)))))
    (switch-to-buffer-other-window buf)
    (save-excursion
      (goto-char (point-max))
      (insert "\n")
      (mml-insert-empty-tag 'part
			    'type type
			    'filename fatpoint
			    'disposition "attachment"
			    'description description))))
;; )



(defun file-list-buffer-mode (buffer)
  "Returns the major mode of buffer."
  (with-current-buffer buffer major-mode))

(defun file-list-message-buffer-p (buffer)
  "Returns buffer if the major mode of buffer is message-mode."
  (if (eq (with-current-buffer buffer major-mode) 'message-mode)
      buffer))

;; (file-list-switch-to-file-list))

; (defun mml-attach-files ()
;   "Create list of files and pass it to mml-attach-file-list function"
;   (interactive)
;   (let* ((cur-buf (current-buffer))
; 	 (dir (read-directory-name "Directory: "
; 				   nil (getenv "HOME") t))
; 	 (match (read-from-minibuffer
; 		 (format "Regexp matching file-names in directory %s: " 
; 			 dir)))
; 	 (files (directory-files dir 'full match nil t))
; 	 (flist files)
; 	 (buf (get-buffer-create " *mm-attach-file-list*"))
; 	 (doit (save-excursion
; 		 (pop-to-buffer buf t)
; 		 (erase-buffer buf)
; 		 (while flist (insert (car flist) "\n")
; 			(setq flist (cdr flist)))
; 		 (yes-or-no-p "Really attach all these files?"))))
;     (switch-to-buffer buf)
;     (kill-buffer-and-window)
;     (switch-to-buffer cur-buf)
;     (if doit (mml-attach-file-list files))))


(defun file-list-insert-files (&optional file-list)
  "Insert all the files in the current file-list into a buffer."
  (interactive)
  (let* ((file-list (or file-list file-list-current-file-list))
	 (flist (mapcar 'file-list-make-file-name file-list))
	 (active (string= (buffer-name (current-buffer))
			  file-list-display-buffer))
	 (buf (current-buffer))
	 file type description)
    (switch-to-buffer buf)
    (while flist
      (setq file (car flist))
	(goto-char (point-max))
	(insert-file file)
	(setq flist (cdr flist)))))


(defun file-list-ogg-to-mp3 (&optional file-list)
  (interactive)
  "Using lame, convert all the files in the current file-list from ogg to mp3."
  (let* ((file-list (or file-list file-list-current-file-list)))
    (while file-list
      (shell-command (concat "lame -V0 "
			     "\""
			     (file-list-make-file-name (car file-list))
			     "\" \""
			     (file-name-sans-extension
			      (file-list-make-file-name (car file-list)))
			     ".mp3"
			     "\""))
      (setq file-list (cdr file-list)))))
				  
(provide 'file-list-tags)

