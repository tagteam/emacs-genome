;;; emacs-genome.el --- loading snps and genes from the emacs-genome

;; Copyright (C) 2014 -- 2020  Thomas Alexander Gerds

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
(if (not (and (boundp 'emacs-genome) 
	      (file-directory-p emacs-genome)))
    (let ((mess (concat "Cannot load emacs-genome: Variable emacs-genome does not specify a directory file."
			"\nTo investigate the problem you could start an interactive lisp session via M-x ielm RET,"
			"\nand then evaluate the variable\n\nELISP> emacs-genome\n\n and the test\n\nELISP> (file-directory-p emacs-genome)\n\nat the prompt."
			"\n\nIf you have downloaded the emacs-genome in the folder\n\n" 
			(expand-file-name "~" nil)
			"\n\nThen, the value of the variable emacs-genome should be\n\n" (expand-file-name "~/emacs-genome/" nil)
			"\n\nThat is, you should have a line\n\n(setq emacs-genome '~/emacs-genome/')\n\n in your init file (e.g., ~/.emacs or ~/.emacs.d/init.el).")))
      (pop-to-buffer "*EG load error*")
      (erase-buffer)
      (insert mess) 
      (error mess))
  (message (concat "Reading genes and snps from: " emacs-genome)))

;; locate emacs packages to emacs-genome
(require 'package)
(setq eg-elpa-sources '(("elpa" . "http://tromey.com/elpa/")
			("gnu" . "http://elpa.gnu.org/packages/")
			("org" . "http://orgmode.org/elpa/")
			("melpa" . "http://melpa.org/packages/")
			("melpa-stable" . "http://stable.melpa.org/packages/")
			("marmalade" . "http://marmalade-repo.org/packages/")))
(dolist (source eg-elpa-sources) (add-to-list 'package-archives source t))
(add-to-list 'package-directory-list (expand-file-name "genes/" emacs-genome))
(add-to-list 'package-directory-list package-user-dir)
(setq orig-package-user-dir package-user-dir)
(setq package-user-dir (expand-file-name "genes/" emacs-genome))
(setq package-enable-at-startup nil)   ; To prevent initialising twice
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)

(use-package bind-key  :ensure t)


(add-to-list 'load-path (expand-file-name "snps/" emacs-genome))
(add-to-list 'load-path (expand-file-name "genes/SuperMan/lisp" emacs-genome))
;; (add-to-list 'load-path (expand-file-name "genes/org-mode/lisp/" emacs-genome))


;; general purpose look feel behaviour snps
(use-package eg-utility-snps)
;; look, feel and behaviour
(use-package appearance-snps)
;; keybindings
(use-package key-snps)

(use-package htmlize :ensure t)

(use-package org
  :ensure org-plus-contrib ;; ensure org's devel
  :pin org
  :config
  ;; (setq org-odt-data-dir (expand-file-name "genes/org-mode/etc/" emacs-genome))
  (use-package org-eldoc)
  (use-package org-snps)
  (use-package org-structure-snps))
;; org-ref
(use-package org-ref :ensure t)

(use-package org-ref-snps)

(use-package company
  :ensure t :config)

;; (use-package doremi)
;; (use-package rainbow-mode
  ;; :ensure t)

;; mic-paren
;; (use-package mic-paren)
  ;; :pin gnu
  ;; :ensure t)
;; folding
(use-package folding)
(use-package fold-dwim)
(use-package folding-snps)
;; (use-package folding
;; :if (require 'fold-dwim nil t)
;; :init (require 'folding-snps nil t)
;; :commands folding-mode)

;; indentation and fill
(use-package aggressive-fill-paragraph
  :ensure t)

;; completion/expansion
(use-package hippie-exp
  :commands hippie-expand)

(use-package auto-complete
  :ensure t)

(use-package popup-complete
  :ensure t)

(use-package yasnippet
  :ensure t)
;; :config
(setq yas-snippet-dirs `(,(concat emacs-genome "/snps/yasnippets")))
(yas-global-mode 1)

(use-package auto-yasnippet
  :ensure t)

;; header buttons
(use-package header-button)
;; emacs-epackage--lib-header-button
;; iedit
(use-package iedit
  :ensure t
  :commands iedit-mode)
;; goto-last-change
(use-package goto-chg 
  :commands goto-chg)
;; buffer switching
(use-package ido
  :config
  (ido-mode 'buffers)
  ;;flexibly match names via fuzzy matching
  (setq ido-enable-flex-matching t)
  ;; use ido-mode everywhere, in buffers and for finding files
  (setq ido-everywhere nil)
  ;;(setq ido-use-filename-at-point 'guess); for find-file-at-point
  ;;(setq ido-use-url-at-point t); look for URLs at point
  ;; sort-order, gives preferences to org 
  (setq ido-file-extensions-order '("org" "R" "pdf" "tex" "el"))
  (setq ido-default-buffer-method 'selected-window)
  ;; Last visited files appear in ido-switch-buffer C-x b
  (setq ido-use-virtual-buffers t))
(use-package ido-completing-read+ :ensure t)
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
;; (use-package recentf
  ;; :commands recentf-open-files)
;; (recentf-mode 1)
;; sos
;; (use-package sos  :commands sos)
;; deft
(use-package deft
  :ensure t)

;; anything/helm
(use-package helm
  :ensure t
  :config
  (use-package helm-config)
  ;; (use-package helm-recoll-snps)
  )
(use-package helm-dictionary
  :ensure t)

;; shell and ssh within emacs
(use-package shell-snps)
(use-package ssh
  :ensure t)
;; pandoc: converting code and documents
(use-package pandoc-mode
  :ensure t)

(use-package  markdown-mode :ensure t)
;; Emacs speaks statistics
;; (setq ess-etc-directory-list nil)
(use-package ess-site
 :ensure ess)
(use-package ess-R-snps)
(use-package ess-edit)
;; (setq ess-use-auto-complete 'script-only)

;; completion in R 
;; (use-package ac-R :ensure t)

(use-package google-translate
  :ensure t)
;; (when (file-exists-p (expand-file-name "genes/auto-dictionary-mode/" emacs-genome))
;; (add-to-list 'load-path (expand-file-name "genes/auto-dictionary-mode/" emacs-genome)))
;; (setq trans-command (expand-file-name "snps/trans" emacs-genome))
;; LaTeX
(use-package tex-site
  :ensure auctex)
(use-package latex-snps)

;;(use-package orgmode-accessories 
;;  :ensure t)
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


(global-set-key [(f2)] 'superman-switch-to-project)
;; (global-set-key [(meta f2)] 'superman-switch-config)
(add-to-list 'file-list-directory-filter "^\\\.[a-zA-Z]+/")
(add-to-list 'file-list-file-name-filter "~$")


;; start-up behaviour
(setq inhibit-startup-screen 'yes-please)

(use-package emacs-genes)
;; (add-hook 'after-init-hook 'eg 'append)
(eg)
;; (setq initial-scratch-message (superman-make-button "bla"))
;; (setq initial-buffer-choice (expand-file-name "EmacsGenome.org" emacs-genome))

;; backtransform to original package location
(setq package-user-dir orig-package-user-dir)

(provide 'emacs-genome)
;;; emacs-genome.el ends here
