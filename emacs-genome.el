;;; emacs-genome.el --- loading snps and genes from the emacs-genome

;; Copyright (C) 2014  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <tag@biostat.ku.dk>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file loads the available snps and genes from the emacs-genome.
;; In order to use this put the following lines into your .emacs:
;;  
;; (setq emacs-genome "/path/to/emacs-genome")
;;
;; E.g., under linux, when you have downloaded
;; the emacs-genome in your HOME directory you can say:
;;
;; (setq emacs-genome (concat (getenv "HOME") "/emacs-genome/"))
;; 

;;; Code:


(defun try-require (lib)
  (if (ignore-errors (require lib))
      (message "library %s loaded" (symbol-name lib))
    (message "Error loading %s" lib)))
(unless (file-exists-p emacs-genome)
  (error (concat  "File: " emacs-genome " does not exist")))

(add-to-list 'load-path (concat emacs-genome "/snps/"))
;; general purpose utility functions
(try-require 'eg-utility-snps)
;; look, feel and behaviour
(try-require 'appearance-snps)
;; anything
(try-require 'recentf)
(when (file-exists-p (concat emacs-genome "/genes/helm/"))
  (add-to-list 'load-path (concat emacs-genome "/genes/helm/"))
  (try-require 'helm)
  (try-require 'helm-config)
  (try-require 'helm-recoll-snps))
;; iedit
(when (file-exists-p (concat emacs-genome "/genes/emacs-iedit/"))
  (add-to-list 'load-path (concat emacs-genome "/genes/emacs-iedit/"))
  (try-require 'iedit))
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
;; pandoc: converting code and documents
(when (file-exists-p (concat emacs-genome "/genes/pandoc-mode/"))
  (add-to-list 'load-path (concat emacs-genome "/genes/pandoc-mode/"))
  (try-require 'pandoc-mode))
;; pandoc: converting code and documents
(when (file-exists-p (concat emacs-genome "/genes/google-translate/"))
  (add-to-list 'load-path (concat emacs-genome "/genes/google-translate/"))
  (try-require 'google-translate))
;; 
(try-require 'browse-url-snps)
;; Emacs speaks statistics
;; (setq ess-etc-directory-list nil)
(add-to-list 'load-path (concat emacs-genome "/genes/ess/lisp"))
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
(when (file-exists-p (concat emacs-genome "/genes/org-mode/lisp/"))
  (add-to-list 'load-path (concat emacs-genome "/org-mode/lisp/"))
  (try-require 'org-snps)
  (try-require 'org-structure-snps)
  ;; superman
  (add-to-list 'load-path (concat emacs-genome "/genes/SuperMan/lisp"))
  (setq superman (try-require 'superman-manager))
  (superman-parse-projects))
;; start-up behaviour
(setq inhibit-startup-screen 'yes-please)
(if (and superman (file-exists-p superman-profile))
    (add-hook 'after-init-hook '(lambda ()
				  ;;(recentf-mode)
				  ;;(recentf-open-files)
				   (S)
				  ;; (S-todo)
				  ;;(superman-calendar)
				  ;; (recentf-open-files)
				  ))
  ;; (superman-set-config "*SuperMan-Calendar* / *S-TODO* | *S* / *Open Recent*")))
  (add-hook 'after-init-hook '(lambda ()
				(recentf-mode)
				(recentf-open-files))))
;; (split-window-vertically)
;; (totd)));; tip of the day


(provide 'emacs-genome)
;;; emacs-genome.el ends here
