;;; gnus-notmuch-snps.el --- Searching for mails

;; Copyright (C) 2013-2017  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <tag@biostat.ku.dk>
;; Keywords: abbrev

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

;; If you read your mail with gnus and use the nnml backend then you can
;; search the mail archive using notmuch. The code below invokes nnir to
;; display the search results in an ephemeral group. 
;;
;; In addition to the code below, I have the following in my .gnus.el:
;; 
;; (setq nnir-search-engine 'notmuch)
;; (setq nnir-notmuch-remove-prefix (expand-file-name nnml-directory))
;; (setq nnir-notmuch-additional-switches nil)
;; (add-to-list 'nnir-method-default-engines '(nnml . notmuch))
;; (add-hook 'gnus-summary-prepared-hook 'gnus-notmuch-show-search-form)
;;
;; and I use "s" to invoke the search from the gnus group buffer: 
;;
;; (define-key gnus-group-mode-map "s" 'gnus-notmuch-search)
;;
;; 
;;; Code:

(require 'nnir)

(setq nnir-search-engine 'notmuch)
(setq nnir-notmuch-remove-prefix nnml-directory)
(setq nnir-notmuch-additional-switches nil)

(define-key gnus-group-mode-map "s" 'gnus-notmuch-search)

(defun gnus-notmuch-search ()
  (interactive)
  (save-window-excursion (async-shell-command "notmuch new" "*notmuch-update*"))
  (switch-to-buffer "*gnus-notmuch-search*")
  (delete-other-windows)
  (gnus-notmuch-insert-form))
  

(defun gnus-notmuch-insert-form ()
  (erase-buffer)
  ;; (let ((from "from:")
  ;; (to "to:")
  ;; (period "date:")
  ;; (search "expression:"))
  (font-lock-mode -1)
  (font-lock-default-function nil)
  ;; (add-text-properties 0 (length from) '(face superman-capture-button-face) from)
  ;; (add-text-properties 0 (length to) '(face superman-capture-button-face) to)
  ;; (add-text-properties 0 (length period) '(face superman-capture-button-face) period)
  ;; (add-text-properties 0 (length search) '(face superman-capture-button-face) search)
  ;; (insert "# Use tab on from: and to: fields to complete from bbdb\n"
  ;; from
  ;; (insert " ")
  ;; (add-text-properties (- (point) 1) (point) '(beg-from t))
  ;; (backward-char 1)
  ;; (insert " " from)
  ;; (add-text-properties (- (point) 0) (- (point) (length from)) '(read-only "Not on field"))
  ;; ;; to
  ;; (forward-char 1)
  ;; (insert " ")
  ;; (add-text-properties (- (point) 1) (point) '(beg-to t))
  ;; (backward-char 1)
  ;; (insert "  " to)
  ;; (add-text-properties (- (point) 0) (- (point) (length to)) '(read-only "Not on field"))
  ;; (add-text-properties (- (point) (length to) 1) (- (point) (length to)) '(end-from t))
  ;; ;; period
  ;; (forward-char 1)
  ;; (insert " ")
  ;; (add-text-properties (- (point) 1) (point) '(beg-period t))
  ;; (backward-char 1)
  ;; ;; search
  (insert
   (superman-make-button "Search:" `(:fun gnus-notmuch-run-search 
					  :width 8
					  :face font-lock-warning-face
					  :help "* search syntax

 +term1 -term2 NEAR/6 term3 ADJ/17 term4 
 
 wildcards: wildc* finds wildcar and wildcards etc

  from:term (match from: headers)
  to:term (match To: or Cc: headers)
  attachment:word match messages with word in an attachment filename.
  subject:word    match messages with word in the subject field.")))
  (add-text-properties (- (point) 1) (point) '(beg-search t))
  (insert "   ")
  ;; (put-text-property (- (point) 3) (point) 'face 'notmuch-date-face)
  (insert "\ndate:3weeks..now")
  (insert "\nfrom:   ")  
  (insert "\nto:   ")
  (insert "\n///")
  (put-text-property (point-at-bol) (1+ (point-at-bol)) 'end-search t)
  (put-text-property (point-at-bol) (point-at-eol) 'face 'font-lock-warning-face)
  (insert "\n\n")
  (insert "\n"
	  (superman-make-button
	   "Evaluate search (J)" `(:fun gnus-notmuch-run-search
					    :width 23 
					    :face superman-capture-button-face
					    :help "Hints: 
				  https://notmuchmail.org/searching/
				  https://notmuchmail.org/manpages/notmuch-search-terms-7/

To do move/tick/delete message in search results and press 
'A W' : gnus-warp-to-article 
 or 
'A T' : gnus-summary-refer-thread.\n")))
  (insert " "
	  (superman-make-button
	   "Reset/update (R)" `(:fun gnus-notmuch-search
				     :width 21 
				     :face superman-capture-button-face
				     :help "Hints: 
To do move/tick/delete message in search results and press 
'A W' : gnus-warp-to-article 
 or 
'A T' : gnus-summary-refer-thread.\n")))
  (insert "\n"
	  (superman-make-button "Change period (P)"
				`(:fun gnus-notmuch-change-period
				       :width 23
				       :face superman-capture-button-face
				       :help "Change time limits of search"))
	  " "
	  (superman-make-button "Toggle period (T)"
				`(:fun 'gnus-notmuch-change-period-1
				       :width 21
				       :face superman-capture-button-face
				       :help "Change time limits of search")))
  (insert "\n\n\nReferences:
 https://notmuchmail.org/searching/
 https://notmuchmail.org/manpages/notmuch-search-terms-7/
  ")
  (gnus-notmuch-search-mode)
  (goto-char (point-min))
  (forward-char 8))

(defun gnus-notmuch-change-period-1 ()
  (interactive)
  (save-excursion
    (let ((current
	   (progn
	     (goto-char (point-min))
	     (re-search-forward "^date:" nil t)
	     (buffer-substring-no-properties (point) (- (re-search-forward "\\.\\." nil t) 2)))))
      (cond ((string-match "3weeks" current)
	     (gnus-notmuch-change-period "1day..now"))
	    ((string-match "1day" current)
	     (gnus-notmuch-change-period "1week..now"))
	    ((string-match "1week" current)
	     (gnus-notmuch-change-period "1month..now"))
	    ((string-match "1month" current)
	     (gnus-notmuch-change-period "1year..now"))
	    (t (gnus-notmuch-change-period "3weeks..now"))))))

(defun gnus-notmuch-change-period (&optional period)
  (interactive)
  (let ((period (or period (read-string "date: "))))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^date:" nil t)
      (kill-line)
      (insert period))))


(defface notmuch-search-face
  '((t (:height 1.0
		:foreground "black"
		:background "aquamarine"
		:box (:line-width 1 :color "gray88" :style released-button))))
  "Default face used to mark the search field."
  :group 'superman)

(defface notmuch-date-face
  '((t (:height 1.0
		:foreground "black"
		:background "LightYellow1"
		:box (:line-width 1 :color "gray88" :style released-button))))
  "Default face used to mark free date fields."
  :group 'superman)


(defun gnus-notmuch-search-mode ()
  (interactive)
  (kill-all-local-variables)
  (font-lock-mode -1)
  (font-lock-default-function nil)
  (use-local-map gnus-notmuch-search-mode-map)
  (setq major-mode 'gnus-notmuch-search-mode)
  (setq mode-name "gnus-notmuch-search")
  (run-hooks 'gnus-notmuch-search-mode-hook))

(defvar gnus-notmuch-search-mode-map
  (let ((map (make-sparse-keymap 'gnus-notmuch-search-mode-map)))
    map))

(define-key gnus-notmuch-search-mode-map [(return)] 'gnus-notmuch-run-search)
(define-key gnus-notmuch-search-mode-map "J" 'gnus-notmuch-run-search)
(define-key gnus-notmuch-search-mode-map "\t" 'gnus-notmuch-complete-from-bbdb)
(define-key gnus-notmuch-search-mode-map "U" #'(lambda () (interactive) (async-shell-command "notmuch new")))
(define-key gnus-notmuch-search-mode-map "R" 'gnus-notmuch-search)
(define-key gnus-notmuch-search-mode-map "P" 'gnus-notmuch-change-period)
(define-key gnus-notmuch-search-mode-map "T" 'gnus-notmuch-change-period-1)

(defun gnus-notmuch-run-search ()
  (interactive)
  (goto-char (point-min))
  (re-search-forward "^from:" nil t)
  (if (looking-at "[ \t]$")
      (delete-region (point-at-bol) (point-at-eol))
    (when (looking-at "[ \t]+") (replace-match "")))
  (re-search-forward "^to:" nil t)
  (if (looking-at "[ \t]$")
      (delete-region (point-at-bol) (point-at-eol))
    (when (looking-at "[ \t]+") (replace-match "")))
  (goto-char (point-min))
  (let* ((search
	  (replace-regexp-in-string
	   "\n" " "	  
	   (replace-regexp-in-string
	    "^[\t ]*" ""
	    (buffer-substring-no-properties
	     (next-single-property-change (point-min) 'beg-search)
	     (next-single-property-change (point-min) 'end-search)))))
	 (Q (replace-regexp-in-string "\\(from\\|to\\):[ \t\n]+" "" search)))
    (set-text-properties 0 (length Q) nil Q)
    (gnus-group-make-nnir-group nil
				`((nnir-query-spec (query . ,Q))
				  (nnir-group-spec ("nnml:" ("nnml:*")))))))

(defun gnus-notmuch-quick-search ()
  (interactive)
  (let ((Q (read-string "Query: " "date:3month..now ")))
    (gnus-group-make-nnir-group
     nil
     `((nnir-query-spec (query . ,Q)) (nnir-group-spec ("nnml:" ("nnml:*")))))))

(defun gnus-notmuch-show-search-form (&optional size)
  (interactive)
  (when (string-match "^nnir:" gnus-newsgroup-name)
    (let ((bw (get-buffer-window "*gnus-notmuch-search*")))
      (if bw (select-window bw)
	(split-window-below (or size 3))
	(switch-to-buffer "*gnus-notmuch-search*")))))

(defun gnus-notmuch-complete-from-bbdb ()
  (interactive)
  (when (save-excursion (beginning-of-line)
			(or (looking-at "from: ") (looking-at "to: ")))
    (bbdb-complete-name)))


(provide 'gnus-notmuch-snps)
;;; gnus-notmuch-snps.el ends here
