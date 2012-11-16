(require 'hippie-exp)

(defun eg-hippie-expand (arg)
  (interactive "P")
  ;; hippie-expand does not have a customization-feature (like
  ;; dabbrev-expand) to search case-sensitive for completions.
  ;; So we must set 'case-fold-search' temp. to nil!
  (let ((old-case-fold-search case-fold-search))
    (setq case-fold-search nil)
    (hippie-expand arg)
    (setq case-fold-search old-case-fold-search)))

(mapcar
 (function
  (lambda (x)
    (define-key x (kbd "ESC p") 'previous-complete-history-element)
    (define-key x (kbd "ESC n") 'next-complete-history-element)))
 (list minibuffer-local-completion-map minibuffer-local-isearch-map
       minibuffer-local-map minibuffer-local-must-match-map))


;;shows possible completions in minibuff, might be handy for some purposes

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-expand-all-abbrevs
	try-complete-file-name
	try-complete-file-name-partially
	try-expand-list
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol
	try-expand-whole-kill))

(provide 'hippie-snps)