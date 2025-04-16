;;; umlaute.el --- fix danish and german umlaute     -*- lexical-binding: t; -*-

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

(defun american ()
  (interactive)
  (ispell-change-dictionary "american")
  (flyspell-buffer)
  (flyspell-mode))




;;{{{ danish
(defun danish ()
  (interactive)
  (danish-mode 'on)
  (ispell-change-dictionary "danish")
  (flyspell-buffer)
  (flyspell-mode))
(defvar danish-mode nil)
(make-variable-buffer-local 'danish-mode)

(defvar danish-map (make-sparse-keymap)
  "Keymap used for `danish-mode' commands.")

(defun danish-ae () (interactive) (insert (char-to-string 230)))
(defun danish-Ae () (interactive) (insert (char-to-string 198)))
(defun danish-ue () (interactive) (insert (char-to-string 229)))
(defun danish-Ue () (interactive) (insert (char-to-string 197)))
(defun danish-Oe () (interactive) (insert (char-to-string 216)))
(defun danish-oe-1 () (interactive)
       ;; (if (eq last-command 'other-window)
	   ;; (other-window 1)
	 ;; (if (not (eq last-command
		      ;; 'danish-oe-1))
	     (insert (char-to-string 248)))
	   ;; (undo)
	   ;; (other-window 1)
	   ;; )))

(define-key danish-map "\C-o" 'other-window)
(define-key danish-map "\M-a" 'danish-ae)
(define-key danish-map "\M-A" 'danish-Ae)
(define-key danish-map "\M-u" 'danish-ue)
(define-key danish-map "\M-U" 'danish-Ue)
;(define-key danish-map "\M-o" 'danish-oe)
(define-key danish-map "\M-'" 'danish-oe-1)
(define-key danish-map "\M-\"" 'danish-Oe)

(or (assq 'danish-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (append minor-mode-map-alist
		  (list (cons 'danish-mode danish-map)))))

(defun danish-mode (&optional arg)
  "A minor mode for easy access to Danish characters."
  (interactive "P")
  (setq ispell-local-dictionary "da")
  ;; (flyspell-mode)
  ;; (flyspell-buffer)
  (setq danish-mode
	(not (or (and (null arg) danish-mode)
		 (<= (prefix-numeric-value arg) 0)))))

(or (assq 'danish-mode minor-mode-alist)
              (setq minor-mode-alist
                    (cons '(danish-mode (char-to-string 248)) minor-mode-alist)))

(or (assq 'danish-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (append minor-mode-map-alist
		  (list (cons 'danish-mode danish-map)))))

(defun danish-umlaute  ()
  (interactive)
  (if (region-active-p)
      (narrow-to-region (region-beginning) (region-end)))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "oe" nil t)
      (replace-match "ø"))
    (goto-char (point-min))
    (while (re-search-forward "Oe" nil t)
      (replace-match "Ø"))
    (while (re-search-forward "aa" nil t)
      (replace-match "å"))
    (goto-char (point-min))
    (while (re-search-forward "Aa" nil t)
      (replace-match "Å"))
    (while (re-search-forward "ae" nil t)
      (replace-match "æ"))
    (goto-char (point-min))
    (while (re-search-forward "Ae" nil t)
      (goto-char (point-min))
      (replace-match "Æ")))
  (if (region-active-p) (widen)))

(defun danish-umlaute-to-text  (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (narrow-to-region (region-beginning) (region-end)))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "ø" nil t)
      (replace-match "oe"))
    (goto-char (point-min))
    (while (re-search-forward "Ø" nil t)
      (replace-match "Oe"))
    (while (re-search-forward "å" nil t)
      (replace-match "aa"))
    (goto-char (point-min))
    (while (re-search-forward "Å" nil t)
      (replace-match "Aa"))
    (while (re-search-forward "æ" nil t)
      (replace-match "ae"))
    (goto-char (point-min))
    (while (re-search-forward "Æ" nil t)
      (goto-char (point-min))
      (replace-match "Ae")))
  (if (region-active-p) (widen)))

(defun repair-danish  (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (narrow-to-region (region-beginning) (region-end)))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "Ã¸" nil t)
      (replace-match "ø" t))
    (goto-char (point-min))
    (goto-char (point-min))
    (while (re-search-forward "Ã¥" nil t)
      (replace-match "å" t))
    (goto-char (point-min))
    (while (re-search-forward "Ã¦" nil t)
      (replace-match "æ" t))
    (goto-char (point-min))
  (if (region-active-p) (widen))))

(defun fix-latex-umlaute ()
  (interactive)
  (save-excursion
					;  accent
    (goto-char (point-min))
    (while (re-search-forward "á" nil t)
      (replace-match "\\\\'{a}"))
    (goto-char (point-min))
    (while (re-search-forward "ó" nil t)
      (replace-match "\\\\'{o}"))
    (goto-char (point-min))
    (while (re-search-forward "é" nil t)
      (replace-match "\\\\'{e}"))
    (goto-char (point-min))
    (while (re-search-forward "ô" nil t)
      (replace-match "\\\\^{o}"))
					; spanish
    (while (re-search-forward "ñ" nil t)
      (replace-match "\\\\~n"))    
    
					;   german
    (goto-char (point-min))
    (while (re-search-forward "ë" nil t)
      (replace-match "\\\\\\\"e"))
    (goto-char (point-min))
    (while (re-search-forward "ä" nil t)
      (replace-match "\\\\\"a"))
    (goto-char (point-min))
    (while (re-search-forward "ö" nil t)
      (replace-match "\\\\\"o"))
    (goto-char (point-min))
    (while (re-search-forward "ü" nil t)
      (replace-match "\\\\\"u"))
					;  danish
    (goto-char (point-min))
    (while (re-search-forward "å" nil t)
      (replace-match "\\\\aa "))
    (goto-char (point-min))
    (while (re-search-forward "æ" nil t)
      (replace-match "\\\\ae "))
    (goto-char (point-min))
    (while (re-search-forward "ø" nil t)
      (replace-match "\\\\o "))))
;;}}}

;;{{{ german

(defvar german-mode nil)
(make-variable-buffer-local 'german-mode)

(defvar german-map (make-sparse-keymap)
  "Keymap used for `german-mode' commands.")
(defun german-ae () (interactive) (insert (char-to-string 228)))
(defun german-Ae () (interactive) (insert (char-to-string 196)))
(defun german-ue () (interactive) (insert (char-to-string 252)))
(defun german-Ue () (interactive) (insert (char-to-string 220)))
(defun german-oe () (interactive) (insert (char-to-string 246)))
(defun german-oe-1 () (interactive)
  (if (not (eq last-command
	       'german-oe-1))
      (insert (char-to-string 246))
    (undo)
    (other-window 1)
    ))
(defun german-Oe () (interactive) (insert (char-to-string 214)))
(defun german-sz () (interactive) (insert (char-to-string 223)))

(define-key german-map "\C-o" 'other-window)
(define-key german-map "\M-s" 'german-sz)
(define-key german-map "\M-a" 'german-ae)
(define-key german-map "\M-A" 'german-Ae)
(define-key german-map "\M-u" 'german-ue)
(define-key german-map "\M-U" 'german-Ue)
;(define-key german-map "\M-o" 'german-oe)
(define-key german-map "\M-o" 'german-oe-1)
(define-key german-map "\M-O" 'german-Oe)

(defun german-mode (&optional arg)
  "A minor mode for easy access german umlaute"
  (interactive "P")
  (setq ispell-local-dictionary "de")
  (setq german-mode
	(not (or (and (null arg) german-mode)
		 (<= (prefix-numeric-value arg) 0)))))


(or (assq 'german-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (append minor-mode-map-alist
		  (list (cons 'german-mode german-map)))))


(or (assq 'german-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(german-mode (concat " " (char-to-string 196)))
			      minor-mode-alist)))

;;}}}

;;{{{ fix umlaute
(defun Fix-danish-characters ()
  (interactive)
  (let ((clist
	 '(("æ" "ae")
	   ("Æ" "Ae")
	   ("ø" "oe")
	   ("Ø" "Oe")
	   ("å" "aa")
	   ("Å" "Aa"))))
    (while clist
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward (caar clist) nil t)
	  (replace-match  (cadr (car clist))))
	)
      (setq clist (cdr clist)))))

(defun fix-danish-characters ()
  (interactive)
  (let ((clist
	 '(("æ" "æ")
	   ("Æ" "Æ")
	   ("ø" "ø")
	   ("Ø" "Ø")
	   ("å" "å")
	   ("Å" "Å"))))
    (while clist
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward (caar clist) nil t)
	  (replace-match  (cadr (car clist))))
	)
      (setq clist (cdr clist)))))
(defun fix-danish-characters ()
  (interactive)
  (let ((clist
	 '(("æ" "æ")
	   ("Æ" "Æ")
	   ("ø" "ø")
	   ("Ø" "Ø")
	   ("å" "å")
	   ("Å" "Å"))))
    (while clist
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward (caar clist) nil t)
	  (replace-match  (cadr (car clist))))
	)
      (setq clist (cdr clist)))))

(defun org-fix-danish-characters-html (output backend info)
  (interactive)
  (let ((clist
	 '(("Æ" "&AElig;")
	   ("Ø" "&Oslash;")
	   ("Å" "&Aring;")
	   ("æ" "&aelig;")
	   ("ø" "&oslash;")
	   ("å" "&aring;"))))
    (while clist
      (setq output 
	    (replace-regexp-in-string (caar clist) (cadr (car clist)) output))
      (setq clist (cdr clist)))
    output))

(defun fix-umlaute  (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (narrow-to-region (region-beginning) (region-end)))
  (save-excursion
    (goto-char (point-min))
    ;; oe
    (goto-char (point-min))
    (while (re-search-forward "ø\\|��\\|\370\\|\366" nil t)
      (replace-match "oe" t))
    ;; "
    (goto-char (point-min))
    (while (re-search-forward "\240\\|\267\\|\247" nil t)
      (replace-match "" t))
    ;; Oe
    (goto-char (point-min))
    (while (re-search-forward "�\\|��\\|\330" nil t)
      (replace-match "Oe" t))
    ;; ae
    (goto-char (point-min))
    (while (re-search-forward "\344\\|\201\346\\|\346\\|\303\246" nil t)
      (replace-match "ae" t))
    ;; Ae
    (goto-char (point-min))
    (while (re-search-forward "�\\|\306" nil t)
      (replace-match "AE" t))
    ;; aa
    (goto-char (point-min))
    (while (re-search-forward "\201\345\\|\345\\|\305" nil t)
      (replace-match "aa" t))
    ;; e'
    (goto-char (point-min))
    (while (re-search-forward "��\\|\351" nil t)
      (replace-match "e" t))
    ;; E'
    (goto-char (point-min))
    (while (re-search-forward "\311" nil t)
      (replace-match "E" t))
    ;; ue
    (goto-char (point-min))
    (while (re-search-forward "�" nil t)
      (replace-match "ue" t))
    ;; Ue
    (goto-char (point-min))
    (while (re-search-forward "�" nil t)
      (replace-match "Ue" t))
    ;; sz
    (goto-char (point-min))
    (while (re-search-forward "�" nil t)
      (replace-match "ss" t)))
  (if (region-active-p) (widen)))
;;}}}

(provide 'umlaute)
;;; umlaute.el ends here
