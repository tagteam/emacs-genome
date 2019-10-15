;;; ess-R-minor-mode.el --- R-minor mode for evaluation from other major modes

;; Copyright (C) 2013  Thomas Alexander Gerds

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

;; 

;;; Code:

;;{{{ R minor mode
;; Look for an Emacs Lisp library that supports "multiple
;; major modes" like mumamo, mmm-mode or multi-mode.
(defvar R-minor-mode nil)
(make-variable-buffer-local 'R-minor-mode)

(defvar R-minor-mode-map (make-sparse-keymap)
  "Keymap used for `R-minor-mode' commands.")

(or (assq 'R-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (append minor-mode-map-alist
		  (list (cons 'R-minor-mode R-minor-mode-map)))))

(or (assq 'R-minor-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(R-minor-mode " R") minor-mode-alist)))

(defun R-minor-mode (&optional arg)
  "A minor mode for using ess commands."
  (interactive "P")
  ;; (make-variable-buffer-local 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list (lambda (old) (ess-complete-object-name)))
  (setq ess-indent-with-fancy-comments nil)
  (setq R-minor-mode
	(not (or (and (null arg) R-minor-mode)
		 (<= (prefix-numeric-value arg) 0)))))

(define-key R-minor-mode-map "_" 'eg/ess-smart-underscore)
(define-key R-minor-mode-map "\M-F" 'ess-eval-function-and-go)
(define-key R-minor-mode-map "\M-j" 'eg/ess-eval-and-go)
(define-key R-minor-mode-map "\M-r" 'copy-region-as-kill)
(define-key R-minor-mode-map "\M-k" 'eg-switch-to-R)
(define-key R-minor-mode-map "\M-q" 'eg/indent-paragraph)
(define-key R-minor-mode-map "\M-m" 'ess-edit-motif)
(define-key R-minor-mode-map "\M-u" 'ess-edit-dev-off)
(define-key R-minor-mode-map "\C-z" 'fold-dwim-toggle)
(define-key R-minor-mode-map "\C-cf" 'ess-edit-insert-call)
(define-key R-minor-mode-map "\C-cv" 'ess-edit-insert-vector)
(define-key R-minor-mode-map "\C-cp" 'ess-edit-insert-path)
(define-key R-minor-mode-map "\C-ch" 'ess-edit-mark-call)
(define-key R-minor-mode-map "\C-cF" 'ess-edit-insert-file-name)
(define-key R-minor-mode-map "\M-\t" 'ess-edit-indent-call-sophisticatedly)
(define-key R-minor-mode-map [(meta return)] '(lambda () (interactive) (ess-edit-next-arg nil)))
(define-key R-minor-mode-map "\M-A" '(lambda () (interactive) (s-goto-next-arg t)))
(define-key R-minor-mode-map "\M-\C-c" 's-config)
(define-key R-minor-mode-map "\M-l" 'mark-line)
(if  (featurep 'xemacs) 
    (define-key R-minor-mode-map [(delete)] 'backward-or-forward-delete-char))
(define-key R-minor-mode-map [(backspace)] 'delete-backward-char)
(define-key R-minor-mode-map [(meta backspace)] 'backward-kill-word)

;;}}}


(provide 'ess-R-minor-mode)
;;; ess-R-minor-mode.el ends here
