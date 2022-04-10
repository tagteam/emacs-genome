(defun superman-ess-set-project-dir ()
  (interactive)
  (when superman-current-project
    (let* ((dir (cdaadr superman-current-project)))
      (insert (concat "setwd('" dir "')")))))
