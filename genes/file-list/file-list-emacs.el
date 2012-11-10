;; submenu, add-button
;; directory-files one argument less
;; replace-in-string is called replace-regexp-in-string
(defun file-list-replace-in-string (str regexp newtext &optional literal)
  (if (featurep 'xemacs)
      (replace-in-string str regexp newtext)
    (replace-regexp-in-string regexp newtext str nil literal)))

;; display-completion-list 
; (display-completion-list completions &optional common-substring)

(provide 'file-list-emacs)