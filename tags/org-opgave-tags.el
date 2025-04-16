;;; org-opgave-tags.el --- export org to Rmd for teaching exercises  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Thomas Alexander Gerds

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(provide 'org-opgave-tags)
(require 'ox-ravel)

;; (add-to-list 'load-path (expand-file-name "genes/orgmode-accessories/" emacs-genome))
;; https://github.com/Fuco1/org-mode/blob/master/contrib/lisp/ox-extra.el
(require 'ox-extra)
(require 'ox-md)
(ox-extras-activate '(ignore-headlines))

(defun never-plain-export ()
  (if (memq org-export-current-backend '(html latex docx))
      "no" "yes"))
(add-to-list 'superman-org-export-target-list "opgave")
(defvar exercise-with-code nil)
(defvar exercise-with-solutions nil)

(defun exercise-with-code ()
  (buffer-name)
  (if exercise-with-code "both" "none"))

(defun exercise-with-solutions ()
  (if exercise-with-solutions "both" "none"))

(defun superman-Rmd-export (exercise-with-code exercise-with-solutions)
  "Export to Rmd."
  (interactive "p")
  ;; (org-ravel-export-to-file 'ravel-markdown nil a s v b nil nil nil "md"))
  (org-ravel-export-to-file 'ravel-markdown nil nil nil nil nil nil nil nil "md"))
       ;; (kmacro-exec-ring-item (("rm" 0 "%d")) 1))

;; since :with-auto-yaml-header does not work we overwrite this
(defun org-ravel--yaml-header (info)
  (let ((rmd_yaml (plist-get info :with-auto-yaml-header)))
    (cons rmd_yaml "")))

(defun superman-export-as-opgave (&optional arg)
  "Export orgmode R-exercise to 4 different targets:
   Rmd without code
   Rmd with code
   pdf with opgave text 
   pdf with solutions

here is how to mark the solution sections

*** Solution :solution:ignore:

and similarly the rdm only sections

*** rmd section :rmd:ignore:

there are two blocks:

#+BEGIN_SRC R  :results output :exports (exercise-with-code) :cache yes :eval (never-plain-export)
  # put your R-code here 
#+END_SRC

#+BEGIN_SRC R  :results output :exports (exercise-with-solutions)  :session *R* :cache yes  :eval (never-plain-export)
library(data.table)
space <- fread('data/space.csv')
space
#+END_SRC
"
  (interactive)
  (let* ((org-buf (current-buffer))
	 (fname (file-name-sans-extension (file-name-nondirectory (buffer-file-name org-buf))))
	 (dir  "~/metropolis/Teaching/demogRafi/exercises/")
	 ;; (rmd-file (org-export-output-file-name ".Rmd"))
	 (rmd-file (concat dir fname ".Rmd"))
	 (rmd-opgave-file (concat dir fname "-opgave.Rmd"))
	 (rmd-solution-file (concat dir fname "-with-solution.Rmd"))
	 (latex-junk (mapcar #'(lambda(x) (concat dir fname x))
			     '(".tex" ".toc" ".synctex.gz" ".fdb_latexmk" ".aux" ".out" ".log")))
	 (org-export-exclude-tags (list "noexport"))
	 (org-export-with-toc t)
	 (exercise-with-code t)
	 (exercise-with-solutions t))
    ;; 
    ;; export to pdf without code without solutions without rmd sections with toc
    ;;
    (save-buffer)
    (setq exercise-with-code nil  ;; evaluate blocks/chunks when :exports (exercise-with-code)
	  org-export-exclude-tags (list "noexport" "rmd" "RMD" "Rmd" "solution" "solutions") ;; ignore these sections
	  org-export-with-toc t)
    ;; (superman-export-as-latex)
    ;; (org-latex-export-to-pdf)
    (superman-org-export-as "pdf")
    ;; 
    ;; export to Rmd with code without toc (first without solutions)
    ;;
    (switch-to-buffer org-buf)
    (setq exercise-with-code t  ;; evaluate blocks/chunks when :exports (exercise-with-code)
	  org-export-exclude-tags (list "opgave" "noexport" "solution" "solutions") ;; ignore these sections
	  org-export-with-toc nil)
    (superman-Rmd-export t nil)
    (copy-file rmd-file rmd-opgave-file 'ok)
    (setq exercise-with-code nil)
    ;; 
    ;; export to Rmd with code without toc (now with solutions)
    ;;
    (setq exercise-with-solutions t ;; evaluate 
	  org-export-exclude-tags (list "opgave" "noexport" "instruks");; now include solution sections
	  org-export-with-toc nil)
    (superman-Rmd-export t t)
    (copy-file rmd-file rmd-solution-file 'ok)
    (setq exercise-with-solutions nil)
    (save-buffer)
    ;; delete the now obsolete rmd file
    ;; (delete-file rmd-file nil)
    ;; revert rmd buffer
    (find-file rmd-solution-file)
    (revert-buffer t t t)
    (save-excursion (goto-char (point-min))
		    (while 
			(re-search-forward ":[a-zA-Z]+:ignore:" nil t)
		      (replace-match "")))
    (save-buffer)
    (find-file rmd-opgave-file)
    (save-excursion (goto-char (point-min))
		    (while 
			(re-search-forward ":[a-zA-Z]+:ignore:" nil t)
		      (replace-match "")))
    (save-buffer)
    (copy-file rmd-opgave-file rmd-file 'ok)
    (find-file rmd-file)
    (revert-buffer t t t)
    (mapcar 'delete-file latex-junk)
    (delete-file rmd-opgave-file)
    (superman-set-config (concat (buffer-name org-buf) " | " rmd-file " / " rmd-solution-file))))

;;; org-opgave-tags.el ends here
