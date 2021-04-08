; -------------------------global keybindings-------------------------
;; {{{ howto
(global-set-key "\C-hh" 'howto)
;; }}}
(when (featurep 'helm)
  (global-set-key "\C-x\C-b" 'helm-buffers-list))
;;{{{windows
(global-set-key [f4] 'delete-other-windows)
(global-set-key [(meta f4)] 'google-search-prompt)
;; (global-set-key [(control v)] 'anything-recoll-search)
(global-set-key [(control v)] 'helm-recoll-search)
(global-set-key [f5] 'eg/kill-this-buffer)
(global-set-key [f10] 'undo)
(global-set-key "\M-o" 'other-window)
;;(global-set-key [(f6)] 'shell-window)
;; (global-set-key [(f6)] 'shell-toggle-cd)
(global-set-key [(f6)] 'superman-goto-shell)
(global-set-key [(control f6)] 'eshell-window)
(global-set-key [(f7)] 'ielm-window);; (global-set-key [(f2)] 'select-project)
;;}}}

;;{{{ moving around 
(setq home-end-enable t)
(global-set-key [(meta down)] 'forward-paragraph)
(global-set-key [(meta up)] 'backward-paragraph)
;;}}}
;;{{{ buffers
;(global-set-key '(meta n) 'switch-to-next-buffer-in-group)
;(global-set-key '(meta p) 'switch-to-previous-buffer-in-group)
;(global-set-key '(meta N) 'switch-to-next-buffer)
;(global-set-key '(meta P) 'switch-to-previous-buffer)
;(global-set-key (kbd "\M-p") 'next-mode-buffer-backward)
;(global-set-key (kbd "\M-n") 'next-mode-buffer)
(global-set-key "\M-p" 'next-mode-buffer-backward)
(global-set-key "\M-n" 'next-mode-buffer)
;;gnus
(global-set-key [(f1)] 'find-frame-start-gnus) 
(global-set-key [(control f1)] 'gnus-other-frame)
;; (global-set-key [(f3)] 'fit-frame)
(global-set-key "\C-xm" 'eg/gnus-group-mail)
(global-set-key [(meta insert)] 'planner-diary-add-entry)
;;utility
(global-set-key [(control f12)] 'visible-mode)
(global-set-key [(f12)] 'folding-mode)
;; (global-set-key [(f9)] 'toggle-truncate-lines)
(global-set-key [f9] 'org)
(global-set-key [(meta f9)] 'org-capture)
;; (global-set-key [(f11)] 'query-replace)
(global-set-key [(f11)] 'superman-capture-item) 
(global-set-key "\M-;" 'comment-or-uncomment-line-or-region)
(global-set-key [(meta æ)] 'comment-or-uncomment-line-or-region)

;(global-set-key [return] 'newline)
(global-set-key "\C-g" 'keyboard-escape-quit)
;(global-set-key "\M-q" 'fill-paragraph-or-region)
(global-set-key "\M-q" 'eg/indent-paragraph)
(global-set-key "\M-Q" '(lambda () (interactive) (mark-paragraph) (fill-region-as-paragraph (region-beginning) (region-end))))
(global-set-key "\M-l" 'mark-line)
(global-set-key "\M-\C-l" 'mark-end-of-line)
(global-set-key "\C-z" 'fold-dwim-toggle)
;(define-key global-window-system-map "\C-z" 'fold-dwim-toggle)
;;}}}
;;{{{ edit
(global-set-key "\M-y" 'yank-or-pop)
(global-set-key "\M-r" 'copy-region-as-kill)
(global-set-key "\M-e" 'hippie-expand)
(global-set-key "\M-i" 'dabbrev-expand)
(global-set-key "\M-v" 'view-file)
(global-set-key "\M-0" 'klammer-island)
(global-set-key "\M-9" 'geschweifte-klammer-island)
(global-set-key "\M-'" 'string-island)
(global-set-key "\M-4" 'dollar-island)
(global-set-key "\C-cr" 'rebox-comment-or-region)
;; (global-set-key "\C-xp" 'point-to-register)
;; (global-set-key "\C-xy" 'insert-register)
(global-set-key "\C-c\C-f" 'font-lock-fontify-buffer)
;; (global-set-key "\C-ct" 'xdict-query)
;; (global-set-key "\C-ct" 'helm-dictionary)
;; (global-set-key "\C-u\C-ct" 'google-translate-query-translate-reverse)

; (global-set-key "\C-cT" 'translate)
; (global-set-key "\C-ct" 'translate-at-point)
; (global-set-key "\C-cT" 'translate)
;; (global-set-key "\C-cu" 'german-mode)
;(global-set-key [(meta N)] 'switch-to-next-buffer)
;(global-set-key [(meta P)] 'switch-to-previous-buffer)
;(global-set-key [(meta left)] '(lambda () (interactive) (other-window 1)))
;(global-set-key [(meta right)] '(lambda () (interactive) (other-window 1)))

;;}}}
(provide 'global-key-tags)
;;; global-key-tags.el ends here
