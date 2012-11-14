(add-to-list 'load-path (concat (getenv "HOME") "/emacs-genome/snps/"))
(add-to-list 'load-path (concat (getenv "HOME") "/emacs-genome/genes/"))
(load (concat (getenv "HOME") "/emacs-genome/genes/ess/lisp/ess-site"))
;; utility functions
(require 'eg-utility-snps)
;; buffer cycling
(require 'cycle-buffer-snps)
;; Emacs speaks statistics
(setq ess-etc-directory-list nil)
(load (concat (getenv "HOME") "/emacs-genome/genes/ess/lisp/ess-site"))
(require 'ess-R-snps)
(require 'ess-edit)
;; LaTeX
(require 'latex-snps)
;; keybindings
(require 'key-snps)


