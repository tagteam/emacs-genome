(setq emacs-novicet)
(add-to-list 'load-path (concat (getenv "HOME") "/emacs-genome/snps/"))
(add-to-list 'load-path (concat (getenv "HOME") "/emacs-genome/genes/"))
;; general purpose utility functions
(require 'eg-utility-snps nil t)
;; completion
(require 'hippie-exp nil t)
;; buffer cycling
(require 'iswitchb nil t)
(iswitchb-mode nil t)
(setq iswitchb-default-method 'samewindow)
(setq iswitchb-case nil t)
(require 'cycle-buffer-snps nil t)
;; Emacs speaks statistics
(setq ess-etc-directory-list nil)
(require 'ess-site nil t)
(require 'ess-R-snps nil t)
(require 'ess-edit nil t)
;; LaTeX
(require 'latex-snps nil t)
;; keybindings
(require 'key-snps nil t)
;; anything-recoll
(require 'anything nil t)
(require 'anything-recoll-snps nil t)
;; folding
(require 'folding-snps nil t)

