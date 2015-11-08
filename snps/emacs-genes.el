;;; emacs-genes.el

;; Copyright (C) 2015  Thomas Alexander Gerds

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
;;----------------------------------------------------------------------
;; created: Apr 12 2015 (09:51) 
;; Version: 
;; last-updated: Nov  5 2015 (06:32) 
;;           By: Thomas Alexander Gerds
;;     Update #: 33
;;----------------------------------------------------------------------
;; 
;;; Commentary: Show emacs genes and test functionality. Then 
;;              display either featured functions (buttons) and/or help on
;;              how to achieve functionality, e.g., install missing
;;              software.
;;; Change Log:
;;----------------------------------------------------------------------
;; 
;;; Code:
;;; Commentary:
;; 
;;; Code:

(defvar eg-alist '(
		   ;; ("emacs-genome"
		   ;; :test (:var emacs-genome)
		   ;; :fun (lambda (&optional arg) (interactive) (superman-view-directory emacs-genome)))
		   ("Projects" :fun S)
		   ("Calendar" :fun superman-calendar)
		   ("Todo-list" :fun superman-todo)
		   ;; ("org-mode" :test org)
		   ;; ("superman"
		   ;; :test (:genes superman-manager)
		   ;; :fun superman)
		   ("R" :test ess
		    :fun eg/start-R
		    #'(lambda ()
			(interactive)
			(and (featurep 'ess)
			     inferior-R-program-name
			     (ignore-errors
			       (R)
			       (eq major-mode 'inferior-ess-mode)))))
		   ;; ("LaTeX" :test tex-site)
		   ;; ("helm" :test helm
		   ;; :emacs-genes eg-helm-emacs-genes)
		   ;; ("recentf" :test recentf)
		   ;;("hippie" :test hippie-exp)
		   )
  "List of emacs-genome emacs-genes")

(defun eg/start-R ()
  (interactive)
  (r 1))

(defvar eg-button-faces
  '(("superman" superman-project-button-face)))

(defface eg-default-genes-button-face
  '((t (:height 3.0
		:foreground "gray22"
		:background "gray90"
		:box (:line-width 1 :color "gray88" :style released-button))))
  "Default face used for emacs genome genes buttons."
  :group 'emacs-genome)

(defun eg (&optional test force)
  "Show and test emacs-genome functionality.
Illustrate functional emacs-genome emacs-genes and provide hints on how to install
yet non-functional emacs-genes."
  (interactive)
  (let* ((eg-buf-name "*Emacs Genome*")
	 (eg-buf (get-buffer eg-buf-name)))
    (unless (and (not force) eg-buf)
      (set-buffer
       (setq eg-buf (get-buffer-create eg-buf-name)))
      ;; (org-mode)
      (erase-buffer)
      (font-lock-mode -1)
      (emacs-genome-mode)
      (let* ((emacs-genes eg-alist)
	     (button-width 13))
	(insert "\nYour emacs has been genetically modified.\n\n")
	;; (insert
	;; (superman-make-button
	;; "EmacsGenome"
	;; '(lambda () (interactive) (superman-switch-to-project "emacs-genome"))
	;; 'eg-default-genes-button-face nil nil button-width)
	;; "\n\n")
	;; (put-text-property (point-min) (1+ (point-min)) 'redo-cmd '(eg t t))
	(while emacs-genes
	  (let* ((genes (car emacs-genes))
		 (this-gene (car genes))
		 (genes-attr (cdr genes))
		 (is-genes
		  (or (and (plist-get genes-attr :ftest)
			   (featurep (plist-get genes-attr :ftest)))
		      (and (plist-get genes :vtest)
			   (boundp (plist-get genes-attr :vtest)))
		      (and (plist-get genes :test)
			   (functionp (plist-get genes-attr :test))
			   (funcall (plist-get genes-attr :test)))
		      (featurep (intern this-gene))))
		 (genes-fun (or (plist-get genes-attr :fun)
				(intern (concat "eg-" this-gene "-fun"))))
		 (genes-nofun (or (plist-get genes-attr :nofun)
				  (intern (concat "eg-" this-gene "-nofun"))))
		 (genes-emacs-genes (plist-get genes-attr :emacs-genes))
		 (fun (cond
		       ((functionp genes-fun) genes-fun)
		       ((functionp genes-nofun) genes-nofun)
		       (t 'eg-missing-genes))))
	    (insert (superman-make-button
		     this-gene
		     ;; `(lambda () (interactive)
		     ;; (eg-show-gene
		     ;; ,(concat emacs-genome "genes/" this-gene "-gene.org")))
		     fun
		     'eg-default-genes-button-face nil nil button-width) "\n\n")
	    (setq emacs-genes (cdr emacs-genes))))
	(setq buffer-read-only t)))
    (superman-set-config eg-buf-name)))

(defun eg-show-gene (file)
  (interactive)
  (get-buffer-create (concat "*" file "*"))
  (set-buffer (concat "*" file "*"))
  (erase-buffer)
  (insert-file file)
  (goto-char (point-min))
  (while (re-search-forward "^BUTTON[ \t]+" nil t)
    (replace-match "")
    (let* ((sexp-start (point))
	   (sexp-end (progn (forward-sexp) (point)))
	   (code  (buffer-substring sexp-start sexp-end t)
	   (result (eval-region sexp-start sexp-end t)))
      (kill-region sexp-start sexp-end)
      (insert result)))))
	  


(defvar emacs-genome-mode-map (make-sparse-keymap)
  "Keymap used for `emacs-genome-mode' commands.")
   
(define-minor-mode emacs-genome-mode 
  "Toggle org projectmanager document emacs-genome mode.
With argument ARG turn emacs-genome-mode on if ARG is positive, otherwise
turn it off.

Enabling emacs-genome mode electrifies the emacs-genome buffer for project management."
     :lighter " *EG*"
     :group 'org
     :keymap 'emacs-genome-mode-map)

(defun emacs-genome-on ()
  (interactive)
  (when emacs-genome-hl-line (hl-line-mode 1))
  (emacs-genome-mode t))

(defun eg-next ()
  (interactive)
  (goto-char (or (next-single-property-change (point-at-eol) 'button)
		 (next-single-property-change (point-min) 'button)))
  (beginning-of-line))

(defun eg-previous ()
  (interactive)
  (goto-char (or (previous-single-property-change (point-at-bol) 'button)
		 (previous-single-property-change (point-max) 'button)))
  (beginning-of-line))

(define-key emacs-genome-mode-map "R" 'eg-redo)
(define-key emacs-genome-mode-map [(tab)] 'eg-next)
(define-key emacs-genome-mode-map [(backtab)] 'eg-previous)

(defun eg-redo ()
  "Refresh emacs genome buffer."
  (interactive)
  (eval (get-text-property (point-min) 'redo-cmd)))

(provide 'emacs-genes)
;;; emacs-genes.el ends here

