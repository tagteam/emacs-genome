;;; emacs-genome.el --- loading snps and genes from the emacs-genome

;; Copyright (C) 2014 -- 2015  Thomas Alexander Gerds

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
(if (not (and (boundp 'emacs-genome) 
	      (file-directory-p emacs-genome)))
    (error "Cannot load emacs-genome: Variable emacs-genome does not locate a directory.")
  (message (concat "Reading genes and snps from: " emacs-genome))
  (if (string-match "\\(.*/emacs-genome/\\).*" emacs-genome)
      (setq emacs-genome (match-string 1 emacs-genome))
    (error "Variable emacs-genome is not a valid directory")))
(let ((eg-load-paths 
       (list "snps/"
	     "genes/emacs-iedit/"
	     "genes/mark-down/"
	     "genes/emacs-sos/" 
	     "genes/deft/"
	     "genes/mic-paren/"
	     "genes/helm/"
	     "genes/ssh-el/"
	     "genes/pandoc-mode/"
	     "genes/ess/lisp/"
	     "genes/use-package/"
	     "genes/auctex/" 
	     "genes/org-mode/lisp/"
	     "genes/org-mode/contrib/lisp/"
	     "genes/SuperMan/lisp/"
	     "genes/ido-ubiquitous/"
	     "genes/emacs-epackage--lib-header-button/")))
  (while eg-load-paths 
    (add-to-list 'load-path
		 (expand-file-name (car eg-load-paths) emacs-genome))
    (setq eg-load-paths (cdr eg-load-paths))))


(require 'use-package)
;; general purpose look feel behaviour snps
(use-package eg-utility-snps)
;; look, feel and behaviour
(use-package appearance-snps)
;; keybindings
(use-package key-snps)
;; folding
(use-package folding
  :if (require 'fold-dwim)
  :config 
  (use-package folding-snps)
  :commands folding-mode)
;; completion/expansion
(use-package hippie-exp
  :commands hippie-expand)
;; iedit
(use-package iedit
  :commands iedit-mode)
;; goto-last-change
(use-package goto-chg 
  :commands goto-chg)
;; buffer switching
(use-package ido
  :config
  (ido-mode t)
  ;;flexibly match names via fuzzy matching
  (setq ido-enable-flex-matching t)
  ;; use ido-mode everywhere, in buffers and for finding files
  (setq ido-everywhere t)
  ;;(setq ido-use-filename-at-point 'guess); for find-file-at-point
  ;;(setq ido-use-url-at-point t); look for URLs at point
  ;; sort-order, gives preferences to org 
  (setq ido-file-extensions-order '("org" "R" "pdf" "tex" "el"))
  (setq ido-default-buffer-method 'selected-window)
  ;; Last visited files appear in ido-switch-buffer C-x b
  (setq ido-use-virtual-buffers t))
(use-package ido-ubiquitous)
;; buffer cycling
(use-package cycle-buffer-snps
  :init
  ;; (icomplete-mode t)
  ;; (iswitchb-mode t)
  ;; (setq iswitchb-default-method 'samewindow)
  ;; (setq iswitchb-case t)
  )
;; window cycling
(use-package winner
  :config
  (winner-mode))
;; browse url
(use-package browse-url-snps
  :commands (browse-url google-search-prompt))
;; recent files
(recentf-mode 1)
(use-package recentf
  :commands recentf-open-files)
;; sos
(use-package sos
  :commands sos)
;; deft
(use-package deft)
;; anything/helm
(use-package helm
  :config
  (use-package helm-config)
  ;; (use-package helm-recoll-snps)
  )
;; shell and ssh within emacs
(use-package shell-snps)
(use-package ssh)
;; pandoc: converting code and documents
(use-package pandoc-mode)
(use-package markdown-mode)
;; Emacs speaks statistics
;; (setq ess-etc-directory-list nil)
(use-package ess-site
  :config
  (use-package ess-R-snps)
  (use-package ess-edit))
;; google translate
;; (when (file-exists-p (expand-file-name "genes/auto-dictionary-mode/" emacs-genome))
;; (add-to-list 'load-path (expand-file-name "genes/auto-dictionary-mode/" emacs-genome)))
;; (setq trans-command (expand-file-name "snps/trans" emacs-genome))
;; LaTeX
(use-package latex-snps)
;; orgmode
(use-package org
  :config
  (setq org-odt-data-dir (expand-file-name "genes/org-mode/etc/" emacs-genome))
  (use-package org-snps)
  (use-package org-structure-snps))
;; superman
(use-package superman-manager
  :config
  ;; project profile
  (unless (file-exists-p superman-profile)
    (setq superman-profile "~/.SuperMan.org"))
  (superman-parse-projects) 
  ;; header buttons
  (use-package header-button)
  (add-hook 'org-mode-hook #'(lambda ()
			       (when (buffer-file-name)
				 (superman-org-headline-mode)))))
;; start-up behaviour
(setq inhibit-startup-screen 'yes-please)

(use-package emacs-genes)
;; (add-hook 'after-init-hook 'eg 'append)
(eg)
;; (setq initial-scratch-message (superman-make-button "bla"))
;; (setq initial-buffer-choice (expand-file-name "EmacsGenome.org" emacs-genome))
(provide 'emacs-genome)
;;; emacs-genome.el ends here
