;;; c-tags.el --- c and c++ stuff                    -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <grb615@ku.dk>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:


(add-to-list 'auto-mode-alist '("\\.cpp" . c++-mode))
(add-hook 'c++-mode-hook 'c++-keys)
(add-hook 'c-mode-hook 'c-keys)

(defun c-eval-buffer (arg)
  (interactive "P")
  (let ((bufn (expand-file-name (buffer-file-name (current-buffer))))
	(comm (if arg
		  (read-shell-command "Command for evaluation: " "R CMD SHLIB ")
		"R CMD SHLIB ")))
    (save-buffer)
    (shell-command (concat comm bufn))))

(defun c-keys ()
  (interactive)
  (define-key c-mode-map "\C-c\C-c" 'c-eval-buffer)
  (define-key c-mode-map "\M-\C-i" 'ess-edit-indent-call-sophisticatedly)
  (define-key c-mode-map "\M-k" 'c-eval-buffer)
  (define-key c-mode-map "\M-k" #'(lambda nil  (interactive) (ess-switch-to-ESS 't))))

(defun c++-keys ()
  (interactive)
  (define-key c++-mode-map "\C-c\C-c" 'c-eval-buffer)
  (define-key c++-mode-map "\M-\C-i" 'ess-edit-indent-call-sophisticatedly)
  (define-key c++-mode-map "\M-k" 'c-eval-buffer)
  (define-key c++-mode-map "\M-k" #'(lambda nil (interactive) (ess-switch-to-ESS 't))))

(fset 'insert-Rprintf 'c-insert-Rprintf)
(fset 'Rprintf 'c-insert-Rprintf)
(defun c-insert-Rprintf ()
  (interactive)
  (let* (list obj type)
    (while (not (string= "" (setq obj (read-string "Object: "))))
      (setq type (if (y-or-n-p "integer? ") "%d" "%1.2f"))
      (setq list (append list `((,obj ,type)))))
    (insert "Rprintf(")
    (insert "\"")
    (mapcar #'(lambda (x) (insert (car x) "=" (cadr x) "\\t")) list)
    (insert "\\n\"")
    (mapcar #'(lambda (x) (insert "," (car x))) list)
    (insert ");")))


(provide 'c-tags)
;;; c-tags.el ends here
