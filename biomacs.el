(setq emacs-novice t)
(add-to-list 'load-path (concat (getenv "HOME") "/emacs-genome/snps/"))
(add-to-list 'load-path (concat (getenv "HOME") "/emacs-genome/genes/"))
;; general purpose utility functions
(require 'eg-utility-snps t)
;; completion
(require 'hippie-exp t)
;; buffer cycling
(require 'iswitchb t)
(iswitchb-mode t)
(setq iswitchb-default-method 'samewindow)
(setq iswitchb-case t)
(require 'cycle-buffer-snps t)
;; Emacs speaks statistics
(setq ess-etc-directory-list nil)
(require 'ess-site t)
(require 'ess-R-snps t)
(require 'ess-edit t)
;; LaTeX
(require 'latex-snps t)
;; keybindings
(require 'key-snps t)
;; anything-recoll
(require 'anything t)
(require 'anything-recoll-snps t)
;; folding
(require 'folding-snps t)

