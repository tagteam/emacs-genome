(setq emacs-novice t)
(add-to-list 'load-path (concat (getenv "HOME") "/emacs-genome/snps/"))
(add-to-list 'load-path (concat (getenv "HOME") "/emacs-genome/genes/"))
;; general purpose utility functions
(require 'eg-utility-snps nil t)
;; look and feel
(require 'appearance-snps nil t)
;; anything
(require 'recentf nil t)
(require 'anything nil t)
(require 'anything-recoll-snps nil t)
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
(setq ess-etc-directory-list nil)
(require 'ess-site nil t)
(require 'ess-R-snps nil t)
(require 'ess-edit nil t)
;; LaTeX
(require 'auctex-snps nil t)
;; keybindings
(require 'key-snps nil t)
;; folding
(require 'folding-snps nil t)
;; start-up behaviour
(add-hook 'after-init-hook '(lambda ()
			      (recentf-mode)
			      (recentf-open-files)))
			      ;; (split-window-vertically)
			      ;; (totd)));; tip of the day
