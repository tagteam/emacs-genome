;;; utility-tags.el --- utility functions for tagteam

;; Copyright (C) 2012-2021  Thomas Alexander Gerds

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

(defun list-to-alist (list)
  (mapcar* 'cons list (make-list (length list) `())))

(defun mapcar* (f &rest args)
  (if (not (memq 'nil args))
      (cons (apply f (mapcar 'car args))
	    (apply 'mapcar* f
		   (mapcar 'cdr args)))))
(defun flatten (list)
  (let (flist)
    (dolist (l list flist) 
      (setq flist (append flist l)))))



(defun replace-in-string (string regexp newtext)
  "In STRING, replace all matches for REGEXP with NEWTEXT.
Hack to get a common function for all Emacsen.  Note that Oort Gnus
has
`gnus-replace-in-string', but we don't want to load Gnus."
  (cond
   ;; Emacs 21 and later
   ((fboundp 'replace-regexp-in-string)
    (replace-regexp-in-string regexp newtext string))
   ;; Emacs < 21; XEmacs
   (t
    ;; Code duplicated from `subr.el' revision 1.423 of Emacs. Neither
    ;; `replace-in-string' from XEmacs 21.4.15 nor the Gnus replacement works
    ;; correctly when an empty string is matched.
    (let ((rep newtext)
	  (l (length string))
	  (start 0) ;; (or start 0) in `subr.el'
	  fixedcase literal subexp
	  matches str mb me)
      (save-match-data
	(while (and (< start l) (string-match regexp string start))
	  (setq mb (match-beginning 0)
		me (match-end 0))
	  ;; If we matched the empty string, make sure we advance by
	  one char
	  (when (= me mb) (setq me (min l (1+ mb))))
	  ;; Generate a replacement for the matched substring.
	  ;; Operate only on the substring to minimize string consing.
	  ;; Set up match data for the substring for replacement;
	  ;; presumably this is likely to be faster than munging the
	  ;; match data directly in Lisp.
	  (string-match regexp (setq str (substring string mb me)))
	  (setq matches
		(cons (replace-match (if (stringp rep)
					 rep
				       (funcall rep (match-string 0
								  str)))
				     fixedcase literal str subexp)
		      (cons (substring string start mb) ; unmatched
			    prefix
			    matches)))
	  (setq start me))
	;; Reconstruct a string from the pieces.
	(setq matches (cons (substring string start l) matches)) ;
	leftover
	(apply #'concat (nreverse matches)))))))

(defun trim-string (string)
  (if string 
      (replace-in-string string "^[ \t\n]+\\|[ \t\n]+$" "")))

(defun middle-of-line ()
  (interactive)
  (let ((maxcol (save-excursion
		  (end-of-line 1)
		  (current-column))))
    (goto-column (/ maxcol 2))))


(defun space-to-onespace ()
  (interactive)
  (if (region-active-p)
      (canonically-space-region (region-beginning) (region-end))
    (canonically-space-region
     (point-min) (point-max))))

(defun space-to-onetab ()
  (interactive)
  (save-excursion
    (goto-char 1)
    (while (re-search-forward "[\t ]+" nil t)
      (replace-match "\t"))))

(defun tab-to-onetab ()
  (interactive)
  (save-excursion
    (goto-char 1)
    (while (re-search-forward "[\t]+" nil t)
      (replace-match "\t"))))

(defun copy-increase-replace (&optional reg maxcount)
  (interactive)
  (let ((reg (or reg "[0-9]+"))
	(maxcount (or maxcount 10))
	(text (buffer-substring (region-beginning) (region-end)))
	(counter 0))
    (goto-char (region-end))
    (insert "\n")
    (while (< counter maxcount)
      (setq counter (+ counter 1))
      (insert (replace-in-string text reg (number-to-string counter)))
      )))


(defun klammer-island ()
  (interactive)
  (insert "(")
  (re-search-forward "[^ }\t\n]*" nil t)
  (insert ")")
  (skip-syntax-backward ")"))

(defun geschweifte-klammer-island ()
  (interactive)
  (insert "{")
  (re-search-forward "[^ ,\.}\t\n]*" nil t)
  (insert "}")
  (backward-char 1))

(defun geschweifte-klammer-island1 ()
  (interactive)
  (insert "\\{")
  (re-search-forward "[^ ,\.$)}\t\n]*" nil t)
  (insert "\\}")
  (backward-char 2))

(defun string-island ()
  (interactive)
  (insert "\"")
  (re-search-forward "[^ ,)\t\n]*" nil t)
  (insert "\"")
  (backward-char 2)
  (if (looking-at "]") (progn (forward-char 1)
			      (transpose-chars 1)
			      (backward-char 3)))
  (forward-char 1))

(defun dollar-island ()
  (interactive)
  (insert "\\(")
  (re-search-forward "[^ ,\.\t\n]*" nil t)
  (insert "\\)")
  (backward-char 2))

(defun insert-date (&optional format-string)
  "Inserts the current date at the point."
  (interactive)
  (let ((format-string (or format-string "%d %b %Y (%R)")))
  (insert (format-time-string format-string))))

(defun comment-or-uncomment-line ()
  (interactive)
  (let ((beg (point-at-bol))
	(end (point-at-eol)))
    (comment-or-uncomment-region beg end)))

(defun comment-line ()
  (interactive)
  (let ((beg (progn (beginning-of-line) (point)))
	(end (progn (end-of-line) (point))))
  (comment-region beg end))
  (forward-line))

(defun toggle-truncate-lines (&optional arg) 
  (interactive)
  (if (= (frame-width (selected-frame))
	 (window-width (selected-window)))
      (setq truncate-lines (not truncate-lines))
    (if (or truncate-partial-width-windows truncate-lines)
	(setq truncate-lines nil
	   truncate-partial-width-windows nil)
      (setq truncate-partial-width-windows t))))


(provide 'utility-tags)
;;; tag-utility-tags.el ends here
