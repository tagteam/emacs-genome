(defun try-require (lib)
  (if (ignore-errors (require lib))
      (message "library %s loaded" (symbol-name lib))
    (message "Error loading %s" lib)))

;; (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 120)
(add-to-list 'load-path (concat (getenv "HOME") "/emacs-genome/snps/"))
;; general purpose utility functions
(try-require 'eg-utility-snps)
;; look, feel and behaviour
(try-require 'appearance-snps)
;; anything
(try-require 'recentf)
(try-require 'helm)
(try-require 'helm-config)
(try-require 'helm-recoll-snps)
;; shell within emacs
(try-require 'shell-snps)
;; completion
(try-require 'hippie-exp)
;; buffer cycling
(try-require 'iswitchb)
(iswitchb-mode t)
(setq iswitchb-default-method 'samewindow)
(setq iswitchb-case t)
(try-require 'cycle-buffer-snps)
;; window cycling
(if (try-require 'winner)
(winner-mode))
;; Emacs speaks statistics
; (setq ess-etc-directory-list nil)
(add-to-list 'load-path (concat (getenv "HOME") "/emacs-genome/genes/ess/lisp"))
(try-require 'ess-site)
(try-require 'ess-R-snps)
(try-require 'ess-edit)
;; ssh
(try-require 'ssh)
;; LaTeX
(try-require 'latex-snps)
;; keybindings
(try-require 'key-snps)
;; folding
(when (and (try-require 'folding) (try-require 'fold-dwim))
    (try-require 'folding-snps))
;; orgmode
(try-require 'org-snps)
(try-require 'org-structure-snps)
;; superman
(add-to-list 'load-path (concat (getenv "HOME") "/emacs-genome/genes/SuperMan/lisp"))
(try-require 'superman-manager)
(superman-parse-projects)
;; file-list 
(add-to-list 'load-path (concat (getenv "HOME") "/emacs-genome/genes/file-list/"))
(try-require 'file-list)
;; start-up behaviour
(setq inhibit-startup-screen 'yes-please)
(add-hook 'after-init-hook '(lambda ()
			      (recentf-mode)
			      (recentf-open-files)))
			      ;; (split-window-vertically)
			      ;; (totd)));; tip of the day
