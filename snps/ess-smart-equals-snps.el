;;; ess-smart-equals-snps.el --- snps for ess-smart-args  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Thomas Alexander Gerds

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



(provide 'ess-smart-equals-snps)

(setq ess-smart-equals-context-function 'essmeq--context-data-table-appropriate)

(defun essmeq-inside-brackets-p (&optional pos)
  "Return t if position POS is inside brackets.
POS defaults to point if no value is given."
  (save-excursion
    (let ((ppss (syntax-ppss pos))
          (r nil))
      (while (and (> (nth 0 ppss) 0)
                  (not r))
        (goto-char (nth 1 ppss))
        (cond
	 ((char-equal ?\[ (char-after)) (setq r 'conditional))
	 ((char-equal ?\( (char-after)) (setq r 'arglist))
	 ((char-equal ?\, (char-after)) (setq r 'arglist)))
        (setq ppss (syntax-ppss)))
      r)))


(defun essmeq--context-data-table-appropriate (&optional pos)
  "Compute context at position POS. Returns a context symbol or t.
If `ess-smart-equals-context-function' is non-nil, that function
is called and a non-nil return value is used as the context; a
nil value falls back on the ordinary computation.

There are two known issues here. First, `ess-inside-call-p' does
not detect a function call if the end parens are not closed. This
is mostly fixed by using `essmeq--inside-call-p' instead. Second,
because the R modes characterize % as a string character, a
single % (e.g., an incomplete operator) will cause checks for
function calls or brackets to fail. This can be fixed with a
temporary % insertion, but at the moment, the added complexity
does not seem worthwhile. Note similarly that when
`ess-inside-string-p' returns a ?%, we could use the % context to
limit to matches to the %-operators."
  (save-excursion
    (let (bracket-pos)
    (when pos (goto-char pos))
    (cond
     (ess-smart-equals-overriding-context)
     ((ess-inside-comment-p)  'comment)
     ((let ((closing-char (ess-inside-string-p)))
        (and closing-char (/= closing-char ?%)))
      ;; R syntax table makes % a string character, which we ignore
      'string)
     ((essmeq-inside-brackets-p))
     ((essmeq--inside-call-p)
      (if (save-excursion
            (goto-char (ess-containing-sexp-position))
            (or (ess-climb-call-name "if")
                (ess-climb-call-name "while")))
          'conditional
        'arglist))
     (t)))))
;;; ess-smart-equals-snps.el ends here
