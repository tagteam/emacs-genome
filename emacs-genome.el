(add-to-list 'load-path (concat (getenv "HOME") "/emacs-genome/snps/"))
;; general purpose utility functions
(require 'eg-utility-snps nil t)
;; look, feel and behaviour
(require 'appearance-snps nil t)
;; anything
(require 'recentf nil t)
;; (require 'helm nil t)
;; (require 'helm-config nil t)
;; (require 'helm-recoll-snps nil t)
;; shell within emacs
(require 'shell-snps nil t)
;; completion
(require 'hippie-exp nil t)
;; buffer cycling
(require 'iswitchb nil t)
(iswitchb-mode t)
(setq iswitchb-default-method 'samewindow)
(setq iswitchb-case t)
(require 'cycle-buffer-snps nil t)
;; window cycling
(require 'winner nil t)
(winner-mode)
;; Emacs speaks statistics
; (setq ess-etc-directory-list nil)
(add-to-list 'load-path (concat (getenv "HOME") "/emacs-genome/genes/ess/lisp"))
(require 'ess-site nil t)
(require 'ess-R-snps nil t)
(require 'tag-R-keys nil t)
(require 'ess-edit nil t)
;; LaTeX
(require 'latex-snps nil t)
;; keybindings
(require 'key-snps nil t)
;; folding
(when (and (require 'folding nil t) (require 'fold-dwim nil t))
    (require 'folding-snps nil t))
;; orgmode
(require 'org-snps nil t)
(require 'org-structure-snps nil t)
;; superman
(add-to-list 'load-path (concat (getenv "HOME") "/emacs-genome/genes/SuperMan/lisp"))
(require 'superman-manager nil t)
;; start-up behaviour
(add-hook 'after-init-hook '(lambda ()
			      (recentf-mode)
			      (recentf-open-files)))
			      ;; (split-window-vertically)
			      ;; (totd)));; tip of the day
