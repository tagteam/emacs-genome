;;; tag-R-keys.el --- ess setup file for statistical software R

;; Copyright (C) 2013  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <tag@biostat.ku.dk>
;; Keywords: convenience, tools

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

;; 

;;; Code:
;;{{{ R mode
(add-hook 'ess-mode-hook 'tag-R-keys)
(defun tag-R-keys ()
  (interactive)
  (define-key ess-mode-map "\M-F" 'ess-eval-function-and-go)
  (define-key ess-mode-map "\M-j" 'ess-eval-region-and-go)
  (define-key ess-mode-map "\M-r" 'copy-region-as-kill)
  (define-key ess-mode-map "\M-k" 'R-inferior-clear)
  (define-key ess-mode-map "\M-q" 'emacs-genome-indent-paragraph)
;  (define-key ess-mode-map "\M-e" 'ess-complete-object-name)
  (define-key ess-mode-map "\M-m" 'ess-edit-motif)
  (define-key ess-mode-map "\M-u" 'ess-edit-dev-off)
  (define-key ess-mode-map "\C-z" 'fold-dwim-toggle)
  (define-key ess-mode-map "\C-cf" 'ess-edit-insert-call)
  (define-key ess-mode-map "\C-cv" 'ess-edit-insert-vector)
  (define-key ess-mode-map "\C-cp" 'ess-edit-insert-path)
  (define-key ess-mode-map "\C-ch" 'ess-edit-mark-call)
  (define-key ess-mode-map "\C-cF" 'ess-edit-insert-file-name)
  (define-key ess-mode-map "\M-\t" 'ess-edit-indent-call-sophisticatedly)
  (define-key ess-mode-map [(meta return)] '(lambda () (interactive) (ess-edit-next-arg nil)))
  (define-key ess-mode-map "\M-A" '(lambda () (interactive) (s-goto-next-arg t)))
  (define-key ess-mode-map "\M-\C-c" 's-config)
  (define-key ess-mode-map "\M-l" 'mark-line)
  (if  (featurep 'xemacs) 
      (define-key ess-mode-map [(delete)] 'backward-or-forward-delete-char))
  (define-key ess-mode-map [(backspace)] 'delete-backward-char)
  (define-key ess-mode-map [(meta backspace)] 'backward-kill-word)
  (setq ess-fancy-comments nil))
;;}}}
;;{{{ inferior mode
(add-hook 'inferior-ess-mode-hook
	  (lambda ()
	    (local-set-key "\M-\t" 'ess-edit-indent-call-sophisticatedly)
	    (local-set-key "\M-F" 'end-of-buffer)
	    (local-set-key "\C-cF" 'ess-edit-insert-file-name)
	    (local-set-key "\C-cf" 'ess-edit-insert-call)
	    (local-set-key "\C-cv" 'ess-edit-insert-vector)
	    (local-set-key "\C-cp" 'ess-edit-insert-path)
	    (local-set-key "\M-r" 'copy-region-as-kill)
	    (local-set-key "\M-p" 'comint-previous-matching-input-from-input)
	    (local-set-key "\M-n" 'comint-next-matching-input-from-input)
	    (setq comint-scroll-to-bottom-on-input 'all)
	    (setq comint-input-ring-size 5000)))
;;}}}
;;{{{ Rd mode
(add-hook 'Rd-mode-hook
	  '(lambda ()
	     (define-key Rd-mode-map "_" 'eg-ess-smart-underscore)
	     (define-key Rd-mode-map "\M-\t" 'ess-edit-indent-call-sophisticatedly)
	     (define-key Rd-mode-map "\C-cF" 'ess-edit-insert-file-name)
	     (define-key Rd-mode-map "\C-cf" 'ess-edit-insert-call)
	     (define-key Rd-mode-map "\C-cv" 'ess-edit-insert-vector)
	     (define-key Rd-mode-map "\C-cp" 'ess-edit-insert-path)
	     (define-key Rd-mode-map "\M-k" 'R-inferior-clear)
	     (define-key Rd-mode-map "\M-j" 'eg-ess-eval-and-go)))

;;}}}
(provide 'tag-R-keys)
;;; tag-R-keys.el ends here
