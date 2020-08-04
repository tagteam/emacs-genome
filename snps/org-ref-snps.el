;;; org-ref-snps.el --- snps for org-ref             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Thomas Alexander Gerds

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

;(setq org-ref-bibliography-notes "~/epapers/org-ref-notes.org")
(setq doi-utils-download-pdf nil)
(defun bibtex-add-entry (&optional query)
  (interactive)
  (if (not (eq major-mode 'bibtex-mode))
      (message "works only in bibtex mode")
    (let ((query
	   (read-string
	    "Query: "
	    ;; now set initial input
	    (cond
	     ;; If region is active assume we want it
	     ((region-active-p)
	      (replace-regexp-in-string
	       "\n" " "
	       (buffer-substring (region-beginning) (region-end))))
	     ;; type or paste it in
	     (t
	      nil))))
	  (bibfile (buffer-file-name (current-buffer))))
      (doi-utils-add-entry-from-crossref-query
       query bibfile))))
    

(provide 'org-ref-snps)
;;; org-ref-snps.el ends here
