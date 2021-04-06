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

(add-to-list 'load-path (expand-file-name "genes/orgmode-accessories/" emacs-genome))
(require 'ox-extra)
(require 'ox-md)
(require 'ox-ravel)
(require 'superman)

(defun never-plain-export ()
  (if (memq org-export-current-backend '(html latex docx))
      "no" "yes"))
(add-to-list 'superman-org-export-target-list "opgave")

(defun exercise-with-code ()
  (if exercise-with-code "both" "none"))

(defun exercise-with-solutions ()
  (if exercise-with-solutions "both" "none"))

(defun superman-Rmd-export (exercise-with-code exercise-with-solutions) "Keyboard macro."
       (interactive "p")
       (kmacro-exec-ring-item (quote ("rm" 0 "%d")) 1))

(defun superman-export-as-opgave (&optional arg)
  "Export orgmode R-exercise to 3 different targets:
   Rmd without code
   Rmd with code
   html with solutions

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
	 (rmd-file (org-export-output-file-name ".Rmd"))
	 (rmd-code-file (concat (file-name-sans-extension rmd-file) "-with-code.Rmd"))
	 (rmd-solution-file (concat (file-name-sans-extension rmd-file) "-with-solution.Rmd"))
	 (org-export-exclude-tags (list "noexport"))
	 (org-export-with-toc t)
	 (exercise-with-code t)
	 (exercise-with-solutions t))
    ;; 
    ;; export to Rmd with code without toc (first without solutions)
    ;;
    (save-buffer)
    ;; to avoid pop-up 
    ;; (save-window-excursion
    ;; (find-file rmd-file)
    ;; (kill-buffer))
    (setq exercise-with-code t  ;; evaluate blocks/chunks when :exports (exercise-with-code)
	  org-export-exclude-tags (list "noexport" "solution" "solutions") ;; ignore these sections
	  org-export-with-toc nil)
    (Rmd-export)
    (copy-file rmd-file (concat (file-name-sans-extension rmd-file) "-with-code.Rmd") 'ok)
    (setq exercise-with-code nil)
    ;; 
    ;; export to Rmd with code without toc (now with solutions)
    ;;
    (setq exercise-with-solutions t ;; evaluate 
	  org-export-exclude-tags (list "noexport" "instruks");; now include solution sections
	  org-export-with-toc nil)
    (Rmd-export)
    (copy-file rmd-file (concat (file-name-sans-extension rmd-file) "-with-solution.Rmd") 'ok)
    (setq exercise-with-solutions nil)
    (save-buffer)
    ;; revert rmd buffer
    (find-file (concat (file-name-sans-extension rmd-file) "-with-solution.Rmd"))
    (revert-buffer t t t)
    (find-file (concat (file-name-sans-extension rmd-file) "-with-code.Rmd"))
    (revert-buffer t t t)
    (copy-file rmd-code-file "~/Desktop/Demografi/" 'yes 'yes)
    (copy-file rmd-solution-file "~/Desktop/Demografi/" 'yes 'yes)
    (superman-set-config (concat (buffer-name org-buf) " | " rmd-code-file " / " rmd-solution-file))))

;;; org-opgave-tags.el ends here
