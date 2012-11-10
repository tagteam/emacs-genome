;; file-list-iswitchf
;;
;; switch to a file without knowing and typing the path
 
(defun file-list-iswitchf-internal (&optional dir file-list fun prompt)
  (let* ((gc-cons-threshold file-list-gc-cons-threshold)
	 (prompt (or prompt file-list-iswitchf-prompt))
	 (minibuffer-completion-table file-list)
	 (fun (or fun 'find-file))
	 (dir (cond ((and dir
			  (file-directory-p dir)
			  (file-name-as-directory
			   (expand-file-name dir))))
	      (t file-list-home-directory)))
	 (file-list
	  (or file-list
	      (progn (if file-list-update (file-list-update dir nil))
		     (file-list-list dir nil nil 'recursive nil))))
	 (file (completing-read
		prompt
		file-list
		nil
		t
		nil
		'file-list-iswitchf-history))
	 (duplicates (file-list-extract-sublist 
		      file-list
		      (lambda (entry)
			(when (string= file (car entry))
			  entry)))))
    (if (= (length duplicates) 1)
	(funcall fun (file-list-make-file-name (car duplicates)))
      (funcall fun 
	       (completing-read
		"Duplicate names. Select one: "
		(mapcar (lambda (entry) (cons
					 (file-list-make-file-name entry)
					 ""))
			duplicates)
		nil 
		t
		dir)))))


(defun file-list-iswitchf-file ()
  "Switch to a file in the file-list of file-list-home-directory."
  (interactive)
  (unless file-list-alist
    (file-list-initialize))
  (file-list-iswitchf-internal))
;   (file-list-iswitchf-internal file-list-home-directory))

(defun file-list-find-magic (file-name &optional ask-for-prog)
  "Open a file with the application found in file-list-magic-alist."
  (let ((prog (if ask-for-prog
		  (read-shell-command (format "icommand on %s " file-name)
				      nil nil nil)
		(cdr (assoc-ignore-case
		      (concat "\."
			      (file-name-extension file-name))
		      file-list-magic-alist)))))
    (if prog (start-process-shell-command
	      "file-list-find-magic"
	      nil
	      prog
	      (file-list-quote-filename file-name))
      (find-file file-name))))

(defun file-list-iswitchf-magic (arg)
  "Switch to file in file-list of file-list-home-directory."
  (interactive "P")
  (file-list-iswitchf-internal
   file-list-home-directory
   nil
   '(lambda (file)
      (file-list-find-magic
       file
       arg))
       "Find file magic "))

(defun file-list-iswitchf-file-other-window ()
  "See file-list-iswitchf-file."
  (interactive)
  (file-list-iswitchf-internal file-list-home-directory
		      nil
		      'find-file-other-window
		      (concat file-list-iswitchf-prompt "(other window) ")))

(defun file-list-iswitchf-file-other-frame ()
  "See file-list-iswitchf-file."
  (interactive)
  (file-list-iswitchf-internal
   file-list-home-directory
   nil
   'find-file-other-frame
   (concat file-list-iswitchf-prompt "(other frame) ")))


(defun file-list-iswitchf-below-directory (dir)
  "Like file-list-iswitchf-file but prompts for directory."
  (interactive "D iswitchf below directory ")
  (file-list-iswitchf-internal (file-name-as-directory
		       (expand-file-name dir))))

(defun file-list-iswitchf-below-directory-other-window (dir)
  "See file-list-iswitchf-below-directory."
  (interactive "D iswitchf below directory ")
  (file-list-iswitchf-internal (file-name-as-directory
		       (expand-file-name dir)
		       nil
		      'find-file-other-window
		      (concat file-list-iswitchf-prompt "(other window) "))))

(defun file-list-iswitchf-below-directory-other-frame (dir)
  "See file-list-iswitchf-below-directory."
  (interactive "D iswitchf below directory ")
  (file-list-iswitchf-internal (file-name-as-directory
		       (expand-file-name dir)
		       nil
		      'find-file-other-frame
		      (concat file-list-iswitchf-prompt "(other frame) "))))

(provide 'file-list-iswitchf)