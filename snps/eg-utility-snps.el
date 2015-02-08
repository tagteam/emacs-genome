;;; eg-utility-snps.el --- emacs genome utility functions

;; Copyright (C) 2012  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <tag@linuxifsv007>
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


(defun yank-or-pop (arg)
  "Combine `yank' with `yank-pop'."
  (interactive "*p") 
  (if (eq last-command 'yank)
      (yank-pop arg)
    (yank arg))
  nil)

(defun count-words (start end)
  "Print number of words in the region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (count-matches "\\sw+"))))

(defun looking-at-backward (regexp)
  (let* ((begin (point))
	 (found (re-search-backward regexp nil t)))
    (goto-char begin)
    (and found (= begin (match-end 0)))))


(defun fit-frame ()
  "Fit the emacs frame to the current display"
  (interactive)
  (if (featurep 'xemacs)
      (progn
	(set-frame-position (buffer-dedicated-frame) 40 30)
	(set-frame-pixel-height (buffer-dedicated-frame) (/ (* (device-pixel-height) 8) 9))
	(set-frame-pixel-width (buffer-dedicated-frame) (/ (* (device-pixel-width) 23) 24)))
    (when (window-system)
      (set-frame-position (selected-frame) 40 30)
      (set-frame-height (selected-frame)
			(/ (* (/ (x-display-pixel-height) (frame-char-height)) 8) 9))
      (set-frame-width (selected-frame)
		       (/ (* (/ (x-display-pixel-width)
				;; (if (> (length (terminal-list)) 1)
				;; (* (frame-char-width) (length (terminal-list)))
				(if  (string-match "27909\\|29377" (system-name))
				    (* (frame-char-width) 2)
				  (frame-char-width)))
			     12) 13)))))


(defun mark-line (arg)
  "Push the mark in line.
The place mark goes is the same place \\[forward-line] would
move to with the same argument."
  (interactive "p")
  (beginning-of-line)  
  (push-mark
   (save-excursion
     (end-of-line arg)
     (point))
   nil t))

(defun winner-cycle (&optional backward)
  (interactive)
  (let ((n (ring-length winner-pending-undo-ring)))
   (if backward 
       (setq winner-counter (max 1 (1- winner-counter)))
     (setq winner-counter (min n (1+ winner-counter))))
  (winner-set (ring-ref winner-pending-undo-ring winner-counter))
  (message "Winner undo ring (%d / %d)"
	   winner-counter
	   (1- (ring-length winner-pending-undo-ring)))))

(defun winner-cycle-backwards ()
  (interactive)
  (winner-cycle t))

(defun comment-or-uncomment-line ()
  (interactive)
  (let ((beg (point (beginning-of-line)))
	(end (point (end-of-line))))
    (comment-or-uncomment-region beg end)))


(defun comment-line ()
  (interactive)
  (let ((beg (progn (beginning-of-line) (point)))
	(end (progn (end-of-line) (point))))
  (comment-region beg end))
  (forward-line))


(defun eg/sort-region (&optional separator)
  (interactive)
  (let ((separator (or separator ","))
	(sort-fold-case nil))
    (narrow-to-region (region-beginning) (region-end))
    (goto-char (point-min))
    (while (re-search-forward (concat "[ \t\n]*" separator "[ \t\n]*") nil t)
      (replace-match "\n"))
    (sort-fields 1 (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "\n" nil t)
      (replace-match ", "))
    (widen)))


(unless (featurep 'xemacs)
  (defun kill-entire-line ()
    (interactive)
    (progn
      (beginning-of-line)
      (kill-line))))

(defun eg/mark-paragraph (&optional arg)
  (interactive "p")
  (cond 
   ((eq major-mode 'org-mode)
    (if (org-babel-where-is-src-block-head)
	(if (= arg 4)
	    (org-babel-mark-block)
	  (mark-paragraph))
      (org-mark-subtree)))
   (t (mark-paragraph))))

(defun eg/indent-paragraph ()
  (interactive)
  (cond 
   ((eq major-mode 'org-mode)
    (cond ((string= (car (org-babel-get-src-block-info)) "emacs-lisp")
	   (let* ((info (org-edit-src-find-region-and-lang))
		  (beg  (nth 0 info))
		  (end (nth 1 info)))
	     (narrow-to-region beg end)
	     (emacs-lisp-mode)
	     (indent-region beg end)
	     (org-mode)
	     (widen)))
	  ((string= (car (org-babel-get-src-block-info)) "R")
	   (let ((org-src-preserve-indentation t))
	     (org-edit-special)
	     (indent-region (point-min) (point-max))
	     (org-edit-src-exit)))
	  ;; (let* ((info (org-edit-src-find-region-and-lang))
	  ;; (beg  (nth 0 info))
	  ;; (end (nth 1 info)))
	  ;; (narrow-to-region beg end)
	  ;; (R-mode)
	  ;; (indent-region beg end)
	  ;; (org-mode)
	  ;; (widen)))
	  (t (fill-paragraph))))
   ((eq major-mode 'Rd-mode) nil)
   ((eq major-mode 'bibtex-mode)
    (save-excursion
      (let ((beg (progn (backward-paragraph 1) (point)))
	    (end (progn (forward-paragraph 1)
			(point))))
	(narrow-to-region beg end)
	(bibtex-reformat)
	(widen))))
   ((eq major-mode 'latex-mode)
    (unless (save-excursion
	      (re-search-forward "begin{document}" nil t))
      (LaTeX-fill-paragraph))) ;; in the preamble do nothing
   (t (save-excursion
	(let ((beg (progn (backward-paragraph 1) (point)))
	      (end (progn (forward-paragraph 1)
			  (point))))
	  (unless (string-match "ess-\\|c-\\|emacs" (symbol-name major-mode))
	    (fill-region beg end nil nil nil))
	  (when (string-match "ess-\\|c-\\|emacs" (symbol-name major-mode))
	    (indent-region beg end nil)))))))
 
(provide 'eg-utility-snps)
;;; eg-utility-snps.el ends here
