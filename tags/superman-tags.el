
(defun superman-sync-project-with-work ()
  (interactive)
  (let ((pdir (expand-file-name (get-text-property (point-min) 'dir))))
    (sync-with-work pdir)))

(add-hook 'superman-view-mode-hook
	  '((define-key superman-view-mode-map
			"W" 'superman-sync-project-with-work)))

(defun superman-ess-set-project-dir ()
  (interactive)
  (when superman-current-project
    (let* ((dir (cdaadr superman-current-project)))
      (ess-eval-linewise 
       (concat "setwd('" dir "')")))))
