;;; emacs-genome.el --- loading snps and genes from the emacs-genome

;; Copyright (C) 2014 -- 2022  Thomas Alexander Gerds

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

;; check if emacs-genome is bound 
(if (not (and (boundp 'emacs-genome) (file-directory-p emacs-genome)))
    (error "The variable `emacs-genome' is either not set or its value is not a directory.")
  (message (concat "Reading genes and snps from: " emacs-genome)))

;; snps are small modifications of the emacs genome which
;; intend to have nice effects on the emacs phenotype
(add-to-list 'load-path (expand-file-name "snps/" emacs-genome))
;; tags are personal user specific modifications
;; put your personal emacs configurations in this folder and
;; call from .emacs
(add-to-list 'load-path (expand-file-name "tags/" emacs-genome))

;; package manager
;; straight is were the genes are
(add-to-list 'load-path (expand-file-name "straight/" emacs-genome))
(require 'straight-snps)

;; look, feel and behaviour
(require 'appearance-snps)
;; keybindings
(require 'global-key-snps)
;; org mode
(use-package org)
(require 'org-snps)

;;  :straight org-plus-contrib 

;; folding
(require 'folding-snps)

;; completion/expansion
(use-package company)

;; (use-package ac-R)

(use-package hippie-exp
  :commands hippie-expand)

(use-package auto-complete)

(use-package popup-complete)

(use-package yasnippet)
(setq yas-snippet-dirs `(,(concat emacs-genome "/snps/yasnippets")))
(yas-global-mode 1)
(use-package auto-yasnippet)

;; auto header for R-files
(use-package header2 
  :config
  (require 'header2-snps))

;; header buttons
;; (use-package header-button)

;; iedit mode 
(use-package iedit
  :commands iedit-mode)

;; buffer and window cycling
(use-package ido
  :config
  (ido-mode 'buffers)
  ;;flexibly match names via fuzzy matching
  (setq ido-enable-flex-matching t)
  ;; use ido-mode everywhere, in buffers and for finding files
  (setq ido-everywhere nil)
  ;; sort-order, gives preferences to org 
  (setq ido-file-extensions-order '("org" "R" "pdf" "tex" "el"))
  (setq ido-default-buffer-method 'selected-window)
  ;; Last visited files appear in ido-switch-buffer C-x b
  (setq ido-use-virtual-buffers t))
(use-package ido-completing-read+)
;; buffer cycling
(require 'cycle-buffer-snps)
;; window cycling
(use-package winner
  :config
  (winner-mode))

;; browse url
(require 'browse-url-snps)
;;  :commands (browse-url google-search-prompt))

;; deft
(use-package deft)

;; anything/helm
(use-package helm)
;;  :config
;;  (use-package helm-config))

;; git
(use-package magit)

;; shell and ssh within emacs
(require 'shell-snps)
(use-package ssh)

;; pandoc: converting code and documents
(use-package pandoc-mode)

;; markdown
(use-package  markdown-mode)

;; Emacs speaks statistics: mostly R
(use-package ess-site                   ; ESS - Emacs Speaks Statistics
  :straight ess
  :commands R
  :hook (ess-mode . subword-mode))

;; smart equal assign
(straight-use-package
 '(ess-smart-equal :type git :host github :repo "genovese/ess-smart-equals"))
(ess-smart-equals-activate)

(require 'ess-edit)
(require 'ess-R-snps)
;; (setq ess-use-auto-complete 'script-only)

;; LaTeX
(use-package tex-site
  :straight auctex)
(require 'latex-snps)

;; superman
(add-to-list 'load-path (expand-file-name "genes/SuperMan/lisp" emacs-genome))
(require 'superman-manager)

  ;; project profile
  (unless (file-exists-p superman-profile)
    (setq superman-profile "~/.SuperMan.org"))
  (superman-parse-projects)
  ;; header buttons
;;  (use-package header-button)
;;  (add-hook 'org-mode-hook #'(lambda ()
;;			       (when (buffer-file-name)
;;				 (superman-org-headline-mode)))))

(global-set-key [(f2)] 'superman-switch-to-project)
(add-to-list 'file-list-directory-filter "^\\\.[a-zA-Z]+/")
(add-to-list 'file-list-file-name-filter "~$")

;; start-up behaviour
(setq inhibit-startup-screen 'yes-please)

(require 'emacs-genes)
(eg)

;; backtransform to original package location
;(setq package-user-dir orig-package-user-dir)

(provide 'emacs-genome)
;;; emacs-genome.el ends here
