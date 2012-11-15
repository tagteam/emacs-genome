(setq emacs-novice t)
(add-to-list 'load-path (concat (getenv "HOME") "/emacs-genome/snps/"))
(add-to-list 'load-path (concat (getenv "HOME") "/emacs-genome/genes/"))
;; general purpose utility functions
(require 'eg-utility-snps)
;; completion
(require 'hippie-exp)
;; buffer cycling
(require 'iswitchb)
(iswitchb-mode t)
(setq iswitchb-default-method 'samewindow)
(setq iswitchb-case t)
(require 'cycle-buffer-snps)
;; Emacs speaks statistics
(setq ess-etc-directory-list nil)
(require 'ess-site)
;;(load (concat (getenv "HOME") "/emacs-genome/genes/ESS/lisp/ess-site"))
(require 'ess-R-snps)
(require 'ess-edit)
;; LaTeX
(require 'latex-snps)
;; keybindings
(require 'key-snps)
;; anything-recoll
(require 'anything)
(require 'anything-recoll-snps)


