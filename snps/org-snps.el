;;; org-snps.el --- 

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

;;; Code:

;;{{{ loading packages

(add-to-list 'load-path (concat (getenv "HOME") "/emacs-genome/genes/org-mode/lisp"))
(add-to-list 'load-path (concat (getenv "HOME") "/emacs-genome/genes/org-mode/contrib/lisp/"))

(try-require 'org-capture)
(try-require 'org-exp-bibtex nil t)
(try-require 'org-clock)
(try-require 'ob-R)
(try-require 'ox-latex)
(try-require 'ox-beamer)
(condition-case nil 
    (try-require 'ox-odt)
  (error nil))
;;}}}

;;{{{ global keys
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
;;}}}
;;{{{ babel emacs-lisp

(defun eg/indent-elisp-org-buffer ()
  (interactive)
  (emacs-lisp-mode)
  (save-excursion (goto-char (point-min))
		  (while (re-search-forward "\\(^[ \t]+\\)\\((defun\\|(defvar\\)" nil t)
		    (replace-match (match-string-no-properties 2))))
  (mark-whole-buffer)
  (indent-region)
  (org-mode))

;;}}}
;;{{{adding special project agenda view

;; (setq org-time-stamp-custom-formats '("<%m/%d/%y %a>" . "<%m/%d/%y %a %H:%M>")
(setq org-time-stamp-custom-formats '("<%d/%b/%Y %a>" . "<%m/%d/%y %a %H:%M>"))

;;}}}
;;{{{ org mode

(defun org-mode-p () (eq major-mode 'org-mode))
(setq org-return-follows-link t)
;; (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)
(add-hook 'org-mode-hook
	  #'(lambda nil
	     (define-key org-mode-map (kbd "C-x c") 'superman-view-mode)
	     (define-key org-mode-map [(control tab)] 'hide-subtree)
	     (define-key org-mode-map [(meta e)] 'hippie-expand)
	     (define-key org-mode-map [(control e)] 'end-of-line)
	     (define-key org-mode-map [(control z)] 'org-shifttab)
	     (define-key org-mode-map "\C-xpd" 'superman-view-documents)
	     (define-key org-mode-map "\C-c\C-v" 'superman-browse-this-file)
	     (define-key org-mode-map [(meta up)] 'backward-paragraph)
	     (define-key org-mode-map [(meta down)] 'forward-paragraph)))

;;}}}
;;{{{ export
;; see http://orgmode.org/manual/Export-options.html
;; C-c C-e t     (org-insert-export-options-template)


(defun eg/org-save-and-run (&optional arg)
  (interactive)
  (save-buffer)
  (let ((cmd (completing-read "Command (default make-pdf): " 
			      '("make-pdf" "make-html")
			      nil 'must-match nil nil "make-pdf" nil))
	(dir (when arg (read-file-name "Publishing directory: " default-directory))))
    (cond ((string= cmd "make-pdf")
	   ;;	   (call-interactively 'org-export-as-latex)
	   (if org-beamer-mode
	       (org-beamer-export-as-latex 3 nil nil nil nil nil)
	     (org-latex-export-as-latex 3 nil nil nil nil nil)))
	  ((string= cmd "make-html")
	   (org-html-export-as-html-and-open 3)))))

;;}}}
;;{{{ latex + latexmk

;; (delete-if (lambda (x) (and (listp x) (string= (nth 1 x) "amssymb"))) org-export-latex-default-packages-alist)
;; (setq org-entities-user nil)
;; '(("space" "\\ " nil " " " " " " " "))
;; (setq org-export-latex-hyperref-format "\\ref{%s}")

(setq org-latex-default-packages-alist
      '(("AUTO" "inputenc" t)
	("" "graphicx" t)
	("" "longtable" nil)
	("" "float" nil)
	("" "wrapfig" nil)
	("" "amsmath" t)
	("" "hyperref" nil)))

 ;; [NO-DEFAULT-PACKAGES]

(add-to-list 'org-latex-classes
      '("org-article"
         "\\documentclass{article}
         [PACKAGES]
         [EXTRA]"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
      '("beamer"
         "\\documentclass{beamer}
         [NO-DEFAULT-PACKAGES]
         [PACKAGES]
         [EXTRA]"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(add-to-list 'org-latex-classes
             '("jss"
               "\\documentclass[useAMS,article,shortnames]{jss}
	       [NO-DEFAULT-PACKAGES]
               [PACKAGES]
               [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             `("book"
               "\\documentclass{book}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))


(add-to-list 'org-latex-classes
	     '("biom"
               "\\documentclass[useAMS,usenatbib]{biom}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
	     '("biomref"
               "\\documentclass[useAMS,usenatbib,referee]{biom}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
	     '("simauth"
               "\\documentclass{simauth}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
	     '("amsart"
               "\\documentclass{amsart}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(add-to-list 'org-latex-classes
	     '("scrartcl"
               "\\documentclass{scrartcl}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(setq org-export-allow-BIND t)
;; (setq org-latex-listings t)
(setq org-latex-listings t)
(setq org-latex-listings-options
    '(("basicstyle" "\\small")
      ("numbers" "left")))
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))
;; (add-to-list 'org-latex-to-pdf-process '("latexmk -f -pdf %s"))
;; (setq org-latex-to-pdf-process '("latexmk -f -pdf %s"))
(setf org-babel-default-inline-header-args
      '((:session . "none")
	(:results . "silent")
	(:exports . "results")))

(defun org-babel-clear-all-results ()
  "clear all results from babel-org-mode"
  (interactive)
  (org-babel-map-src-blocks nil (org-babel-remove-result)))

;; (add-hook 'org-latex-after-save-hook 'latex-save-and-run)
;; (remove-hook 'org-latex-after-save-hook 'latex-save-and-run)

(defun eg/org-indent ()
  "visit tex-buf or R-buf"
  (interactive)
  (if (string= (car (org-babel-get-src-block-info)) "R")
      (ess-edit-indent-call-sophisticatedly)
    (pcomplete)))

(defun eg/org-mark-block-or-element ()
  "Mark block or element."
  (interactive)
  (if (not (org-babel-get-src-block-info))
    (org-mark-element)
	(goto-char (org-babel-where-is-src-block-head))
	(looking-at org-babel-src-block-regexp)
	(push-mark (match-end 5) nil t)
	(goto-char (match-beginning 5))))
    

(defun eg/org-switch-to-assoc-buffer ()
  "visit tex-buf or R-buf"
  (interactive)
  (if (string= (car (org-babel-get-src-block-info)) "R")
      (ess-switch-to-end-of-ESS)
    (pop-to-buffer (org-file-name nil nil nil ".tex"))))


(defun eg/org-ess-eval-function ()
  "If in R_SRC block, evaluate function at point."
  (interactive)
  (if (string= (car (org-babel-get-src-block-info)) "R")
        (ess-eval-function-and-go)))

(defun eg/org-smart-underscore ()
    "Smart \"_\" key: insert <- if in SRC R block unless in string/comment."
    (interactive)
    (if (and
	 (string= (car (org-babel-get-src-block-info)) "R")
	 (not (ess-inside-string-or-comment-p (point))))
      (ess-insert-S-assign)
      (insert "_")))

(defun eg/org-export-as-latex (&optional start-new-process)
  "See library tex-buf for help on TeX-process"
  (interactive "P")
  (if (string= (car (org-babel-get-src-block-info)) "R")
      (eg/ess-eval-and-go)
    (save-buffer)
    (let* ((obuf (current-buffer))
	   ;; (org-export-show-temporary-export-buffer nil)
	   (texbuf (org-file-name nil nil nil ".tex"))
	   (name (org-file-name nil nil nil ""))
	   (curdir (file-name-directory (org-file-name nil nil t "")))
	   (texfile (org-file-name nil nil t ".tex"))
	   (procbuf (TeX-process-buffer-name name))
	   (process (TeX-process name))
	   (delete start-new-process)
	   (start start-new-process))
      (save-window-excursion
	(when (get-file-buffer texbuf)
	  (kill-buffer (get-file-buffer texbuf))
	  (find-file texfile)))
      ;; (org-latex-export-as-latex)
      (if org-beamer-mode
	  (org-beamer-export-to-latex)
	(org-latex-export-to-latex))
      (save-excursion
	(when (and delete process) (delete-process process))
	(when (or start (not (and process (eq (process-status process) 'run))))
	  (TeX-run-command "make-pdf" (concat "latexmk -pvc -pdf -f " name) name)))
      (when start-new-process
	(save-excursion
	  (delete-other-windows)
	  (split-window-horizontally)
	  (other-window 1)
	  ;; (switch-to-buffer texbuf)
	  (if (get-file-buffer texbuf)
	      (progn
	      (switch-to-buffer (get-file-buffer texbuf))
	      (revert-buffer t t t))
	  (find-file texfile))
	  (split-window-vertically)
	  (other-window 1)
	  (switch-to-buffer "*R*")
	  (split-window-vertically)
	  (other-window 1)
	  (switch-to-buffer procbuf))))))


	  ;; (switch-to-buffer texbuf)
	  ;; (TeX-next-error))))))

(defun eg/org-next-latex-error (&optional run)
    "Show next latex error.

If a prefix argument RUN is given run latex first.
depend on it being positive instead of the entry in `TeX-command-list'."
    (interactive "P")
    (let* ((obuf (current-buffer))
	   (texbuf (org-file-name nil nil nil ".tex")))
      (save-window-excursion
	(switch-to-buffer texbuf)
	(if run
	    (TeX-command "LaTeX" 'TeX-master-file nil))
	(TeX-next-error run))))

(defun org-turn-on-auto-export ()
  (interactive)
  (add-hook 'after-save-hook 'eg/org-export-as-latex))

(defun org-turn-off-auto-export ()
  (interactive)
  (remove-hook 'after-save-hook 'eg/org-export-as-latex))

;; (setq org-export-html-after-blockquotes-hook nil)
;; (add-hook 'org-export-html-after-blockquotes-hook '(lambda ()
						     ;; (save-excursion
						       ;; (re-search-backward "html" nil t)
						       ;; (beginning-of-line)
						       ;; (insert "nonesense")
						     ;; (message (buffer-file-name)))))
(defun org-file-name (&optional file buf full ext)
  "Function to turn name.xxx into name.org. When FILE is given
it must be regular file-name, it may be the absolute filename including
the directory. If FILE is nil and BUF is given, then use the filename of
the file visited by buffer BUF. If BUF is also nil then use 
'current-buffer'. If FULL is non-nil return the absolute filename.
If EXT is given then turn name.xxx into name.ext. EXT must be a string like '.tex'" 
  (let ((name (or file (buffer-file-name (or buf (current-buffer))))))
    (concat (file-name-sans-extension
	     (if full
		 name
	       (file-name-nondirectory name)))
	    (or ext ".org"))))

;;}}}
;;{{{ bibtex

;; I configured reftex to handle my biblio. It inserts a citation in this way
;; [[bib:myKey]]
;; where
;; #+LINK: bib file:~/mydocs/mybib.bib::%s

;; To customize this citation I have defined in my .emacs
(defun eg/org-mode-setup ()
  (interactive)
  (load-library "reftex")
  (reftex-parse-all) ; to make reftex aware of the biblio
                     ; # \bibliography{biblio}
   (reftex-set-cite-format
     '((?b . "[[bib::%l]]")
       (?n . "[[note::%l]]")
       (?c . "\\cite{%l}")))
    (define-key org-mode-map "\C-c\C-g" 'reftex-citation)
)

;;}}}
;;{{{ babel R
;; for block options see: org-babel-common-header-args-w-values
;; org-babel-R-evaluate-session
;; org-babel-R-write-object-command
;; org-babel-comint-eval-invisibly-and-wait-for-file
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (perl . t)
   (latex . t)
   (ditaa . t)
   (dot . t)
   (sh . t)
   (R . t)))

;; Do not confirm source block evaluation
(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(add-to-list 'org-src-lang-modes '("emacs-lisp" . emacs-lisp))

;;}}}
;;{{{ babel emacs-lisp

(defun eg/org-lazy-load (&optional only-this-block)
  "Tangle, load and show emacs-lisp code. "
  (interactive "P")
  (when (string= (car (org-babel-get-src-block-info)) "emacs-lisp")
  (save-buffer)
  (let ((tfile (car (org-babel-tangle only-this-block nil 'emacs-lisp))))
    (switch-to-buffer-other-window (find-file-noselect tfile t))
    (revert-buffer t t)
    ;; (find-file-other-window tfile)
  (eval-buffer))))

;;}}}
;;{{{ org mode hook
(add-hook 'org-mode-hook
	  #'(lambda nil
	     (define-key org-mode-map [(meta k)] 'eg/org-switch-to-assoc-buffer)
	     (define-key org-mode-map [(meta j)] 'eg/org-export-as-latex)
	     (define-key org-mode-map [(control shift e)] 'eg/org-lazy-load)
	     (define-key org-mode-map [(control xx)] 'eg/org-lazy-load)
	     (define-key org-mode-map "\M-F" 'ess-eval-function-and-go)
	     (define-key org-mode-map [(meta control i)] 'eg/org-indent)
	     (define-key org-mode-map "_" 'eg/org-smart-underscore)))

;;}}}
;;{{{ list documents

 ;; Example:
 ;; #+BEGIN_SRC emacs-lisp :results list
;; (org-directory-files '.../tex/' '\.tex$')
;; #+END_SRC"

(defun org-directory-files (dir &optional file-regexp path-regexp path-inverse-match sort-by reverse)
  "List files as org-links that match FILE-REGEXP
 in a directory which is related to the directory DIR. "
  (interactive)
  (let* ((file-regexp (or file-regexp "."))
	 (files (file-list-select-internal nil file-regexp nil nil dir nil t)))
    (setq files (file-list-sort-internal files (or sort-by "time") reverse 'dont))
    (if path-regexp
	(setq files (file-list-select-internal files path-regexp "path" path-inverse-match)))
    (when files 
      (mapcar #'(lambda (x)
		 (concat "[["(replace-in-string (file-list-make-file-name x) (getenv "HOME") "~") "]["
			 (car x) "]]")) files))))



(defun org-heading-directory-files-maybe (&optional limit)
  (interactive)
  (let ((dir (org-entry-get nil "DIR" nil))
	(beg (save-excursion (org-back-to-heading t) (point)))
	(end (save-excursion (outline-next-heading) (point)))
	(limit (or limit 10))
	(count 0) 
	flist)
    (when dir
      (save-window-excursion
      (setq flist (org-directory-files "\." dir nil  dir "time" nil)))
      (org-back-to-heading)
      (when flist
	(unless (re-search-forward org-property-end-re end t)
	  (end-of-line))
	(if (re-search-forward "BEGIN_SRC FILE-LIST" end t)
	    (progn 
	      (org-babel-mark-block)
	      (delete-region (region-beginning) (region-end)))
	  (insert "\n\n#+BEGIN_SRC FILE-LIST\n#+END_SRC\n")
	  (previous-line 1))
      (while (and flist (< count limit))
	(insert (car flist) "\n")
	(setq flist (cdr flist)
	      count (+ count 1)))))))



;;}}}
;;{{{ org-run-script
(defun eg/org-run-script (&optional server R)
  (interactive)
  (let* ((buf (buffer-file-name (current-buffer)))
	 (code (concat (file-name-sans-extension buf) ".R"))
	 (log (concat code "out"))
	 (R  (or R "/usr/local/bin/R"))
	 (cmd (concat "ssh " server " 'nohup nice -19 " R " --no-restore CMD BATCH " code " " log "'")))
    (save-buffer)
    (org-babel-tangle nil code "R")
    (save-excursion
      (when (get-buffer "*Async Shell Command*")
	(with-current-buffer "*Async Shell Command*"
	  (rename-uniquely)))
      (async-shell-command cmd))
    (concat "[[" log "]]")))



(defun org-project-files (file-regexp &optional path-regexp path-inverse-match subdir sort-by reverse)
  "List files as org-links that match FILE-REGEXP
 in a directory which is related to the directory DIR. "
  (interactive)
  (let* ((dir (concat (file-name-directory
		       (buffer-file-name (current-buffer)))
		      (if subdir (concat "../" subdir) "..")))
	 (files (file-list-select-internal nil file-regexp nil nil dir nil t)))
    (setq files (file-list-sort-internal files (or sort-by "time") reverse 'dont))
    (if path-regexp
    (setq files (file-list-select-internal files path-regexp "path" path-inverse-match)))
    (when files 
      (mapcar #'(lambda (x)
		 (concat "[["(replace-in-string (file-list-make-file-name x) (getenv "HOME") "~") "]["
			 (car x) "]]")) files))))
;;}}}

(provide 'org-snps)
;;; org-snps.el ends here
