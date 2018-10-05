;;{{{ allow bind
;; (setq org-export-allow-BIND t)
(setq org-export-allow-bind-keywords t)
;;}}}

(defun fix-orgmode-latex-blocks ()
  (interactive)
  (save-excursion
    (let ((case-fold-search t))
      (goto-char (point-min))
      (while (re-search-forward "BEGIN_LaTeX" nil t)
	(replace-match "BEGIN_EXPORT latex"))
      (goto-char (point-min))
      (while (re-search-forward "end_LaTeX" nil t)
	(replace-match "END_EXPORT")))))
(fset 'beamer-fix-latex-blocks 'fix-orgmode-latex-blocks)


(defun fix-orgmode-rmd-exercise ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward "BEGIN_SRC R" nil t)
	(let ((end (point-at-eol)))
	  (if (re-search-forward ":eval" end t)
	      ;; (error "dont know how to treat this block")
	      nil
	    (goto-char end)
	    (insert " :eval (never-plain-export)")))))))
	

(defun fix-orgmode-footnotes ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\[\\([0-9]+\\)\\]" nil t)
      (let* ((fn (match-string-no-properties 1))
	     (new-fn (concat "[fn:" fn "]")))
	(when (y-or-n-p (concat "Replace " fn " with " new-fn "? "))
	  (replace-match new-fn))))))
(fset 'beamer-fix-footnotes 'fix-orgmode-footnotes)

;; (setq org-odt-data-dir "~/emacs-genome/genes/org-mode/etc/")

;;{{{ applications for org-open-things


(eval-after-load "org"
  '(progn
     ;; Change .pdf association directly within the alist
     (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")))
;; NOTE: when the exporter is opening the exported file,
;;       the following maybe overwritten by a mailcap entry
(add-to-list 'org-file-apps '("\\.xls[x]?" . "soffice %s"))
(add-to-list 'org-file-apps '("\\.csv[2]?" . (lambda (file link) (find-file-other-window link))))
(add-to-list 'org-file-apps '("\\.doc[x]?" . "soffice %s"))
(add-to-list 'org-file-apps '("\\.odt" . "soffice %s"))
(add-to-list 'org-file-apps '("\\.ppt[x]?" . "soffice %s"))
(add-to-list 'org-file-apps '("\\.png" . "geeqie %s"))
(add-to-list 'org-file-apps '("\\.ps" . "evince %s"))
(add-to-list 'org-file-apps '("\\.eps" . "gv %s"))

(setq org-odt-preferred-output-format "docx")
;;}}}
;;{{{ do NOT let TAB bring up special R-edit
(remove-hook 'org-tab-first-hook 'org-src-native-tab-command-maybe)
;;}}}
 
;;{{{ load path and org-directory
(setq org-directory "~/metropolis")
;;}}}

(add-hook 'org-mode-hook
	  '(lambda nil
	     (setq comment-region-function 'comment-region-default)
	     (define-key org-mode-map "\C-cf" 'ess-edit-insert-call)
	     (define-key org-mode-map "\C-cv" 'ess-edit-insert-vector)
	     (define-key org-mode-map "\C-cp" 'ess-edit-insert-path)
	     (define-key org-mode-map "\C-ch" 'ess-edit-mark-call)
	     (define-key org-mode-map "\C-cF" 'ess-edit-insert-file-name)
	     (define-key org-mode-map "\M-\t" 'ess-edit-indent-call-sophisticatedly)
	     (define-key org-mode-map [(meta h)] 'eg/mark-paragraph)))

(global-set-key [f9] 'org)

;;{{{ html export
(defun my-org-html-postamble (plist)
  (format
    (format "Last update: %s by %s." (format-time-string "%d %b %Y") user-full-name)))
  ;; "<div id=\"bottomBox\">Last update: %s by Thomas Alexander Gerds: (format-time-string "%d %b %Y") using %c</div>"))
  ;; (format "Last update : %s" (format-time-string "%d %b %Y")))
(setq org-html-postamble 'my-org-html-postamble)

;; (setq org-html-postamble t
      ;; org-html-style-include-default nil
      ;; org-html-style-include-scripts nil
      ;; org-html-metadata-timestamp-format "%Y-%m-%d"
      ;; org-html-postamble-format
      ;; '(("en" "<div id=\"bottomBox\">last updated by %a: %d using %c</div>")))
;; org-export-date-timestamp-format

(setq org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://publicifsv.sund.ku.dk/~tag/styles/practicals.css\" />")

;;}}}

;;{{{ Rmd-tutorials and exercises

;; (defun superman-get-org-headline-string-element  (headline backend info)
  ;; "Return the org element representation of an element."
  ;; (let ((prop-point (next-property-change 0 headline)))
    ;; (if prop-point (plist-get (text-properties-at prop-point headline) :parent))))

;; (defun superman-org-exclude-section (headline backend info)
  ;; "Insert a clearpage at end of heading if property clearpage is non-nil."
  ;; (when (org-export-derived-backend-p backend 'html)
    ;; (let ((elm (superman-get-org-headline-string-element headline backend info)))
      ;; (when (and elm (org-element-property :CLEARPAGE elm))
        ;; (concat headline "\\clearpage\\n")))))
;; (add-to-list 'org-export-filter-headline-functions 'superman-org-exclude-section)

;; export to Rmd
(add-to-list 'load-path (expand-file-name "genes/orgmode-accessories/" emacs-genome))
(require 'ox-extra)
(require 'ox-md)
(require 'ox-ravel)
(ox-extras-activate '(ignore-headlines))

(defun never-plain-export ()
  (if (memq org-export-current-backend '(html latex docx))
      "no" "yes"))

(defun rmd-only ()
  (if (memq org-export-current-backend '(ravel-markdown))
      "results" "results"))

(defun not-rmd ()
  (if (memq org-export-current-backend '(ravel-markdown))
      "yes" "no"))

(add-to-list 'superman-org-export-target-list "rmd/html")
(add-to-list 'superman-org-export-target-list "exercise")

(defun superman-ravel-export-to-Rmd (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let* ((extension ".Rmd")
	 (file (org-export-output-file-name extension subtreep))
					;(org-export-coding-system org-html-coding-system)
	 )
    (org-export-to-file 'ravel-markdown file
      async subtreep visible-only body-only ext-plist)))

(fset 'Rmd-export
      (lambda (&optional arg) "Keyboard macro."
	(interactive "p") 
	(kmacro-exec-ring-item (quote ("rm" 0 "%d")) arg)))

(setq exercise-without-code nil)
(setq exercise-with-code nil)
(setq exercise-with-solutions nil)
(setq rmd-only nil)
(setq html-only nil)

(defun exercise-with-code ()
  (if exercise-with-code "both" "none"))
(defun exercise-without-code ()
  (if exercise-without-code "both" "none"))
(defun exercise-with-solutions ()
  (if exercise-with-solutions "both" "none"))
(defun graphics-for-solutions ()
  (if exercise-with-solutions "graphics" "none"))
(defun exercise-without-solutions ()
  (if (not exercise-with-solutions) "both" "none"))
(defun eval-graphics-with-solutions ()
  (if exercise-with-solutions "both" "none"))


;; Usage

;; #+BEGIN_SRC R  :results output :exports (exercise-with-code)  :session *R* :cache yes :eval (never-plain-export)
;; #+BEGIN_SRC R  :results output :exports (exercise-without-code)  :session *R* :cache yes :eval (never-plain-export) 
;; #+BEGIN_SRC R :exports (exercise-with-solutions) :results output  :session *R* :cache yes 


(defun superman-export-as-vignette ()
  "Export orgmode to Rmd target:
"
  (interactive)
  (let ((rmd-file (org-export-output-file-name ".Rmd"))
	(org-export-exclude-tags (list "noexport"))
	(org-export-with-toc t))
    ;; to the rmd versions
    (save-buffer)
    ;; third, export to Rmd with code without toc
    ;;
    (save-window-excursion
      (find-file rmd-file)
      ;; to avoid pop-up 
      (kill-buffer))
    (Rmd-export)
    ;; revert rmd buffer
    (save-window-excursion
      (find-file (org-export-output-file-name ".Rmd"))
      (revert-buffer t t t))
    (superman-set-config (concat (buffer-name) " | " rmd-file))))

(defun superman-export-as-exercise ()
  "Export orgmode R-exercise to 4 different targets:
   html without code
   Rmd without code
   Rmd with code
   html with solutions

here is how to mark the solution sections

*** Solution :solution:ignore:

and similarly the rdm only sections

*** rmd section :rmd:ignore:

there are two blocks:

#+BEGIN_SRC R  :results output :exports (exercise-without-code) :cache yes :eval (never-plain-export)
  # put your R-code here 
#+END_SRC

#+BEGIN_SRC R  :results output :exports (exercise-with-code)  :session *R* :cache yes  :eval (never-plain-export)
library(data.table)
space <- fread('data/space.csv')
space
#+END_SRC

"
  (interactive)
  (let ((rmd-file (org-export-output-file-name ".Rmd"))
	(org-export-exclude-tags (list "noexport"))
	(org-export-with-toc t)
	(exercise-with-solutions nil)
	(exercise-without-solutions nil)
	(exercise-with-code nil)
	(exercise-without-code nil)
	(html-file (org-export-output-file-name ".html")))
    ;;
    ;; first, the solutions as html
    ;;
    (save-buffer)
    (setq exercise-with-solutions t)
    (setq org-export-exclude-tags (list "noexport" "rmd" "exercises"))
    (org-html-export-to-html)
    (rename-file html-file (concat (file-name-sans-extension html-file) "-with-solutions.html") 'ok)
    (setq exercise-with-solutions nil)
    ;;
    ;; second, the html version of the exercises with links
    ;; to the rmd versions
    ;; 
    (save-excursion
      (goto-char (point-min))
      ;; when link does not exist org breaks the flow
      (ignore-errors (insert "- [[./" rmd-file "][Open R-studio Rmarkdown version without R-code]]\n"))
      (ignore-errors (insert "- [[./" (concat (file-name-sans-extension rmd-file) "-with-code.Rmd") "][Open R-studio Rmarkdown version with R-code]]\n")))
    (save-buffer)
    (setq exercise-with-code t)
    (setq org-export-exclude-tags (list "noexport" "rmd" "solution"))
    (org-html-export-to-html)
    ;; get rid of the links to the rmd files
    (save-excursion
      (goto-char (point-min))
      (while (looking-at "- \\[\\[.*Rmd\\]\\[.*\\]\n")
	(kill-region (point-min) (1+ (point-at-eol)))))
    (setq exercise-with-code nil)
    ;; 
    ;; third, export to Rmd with code without toc
    ;;
    (save-window-excursion
      (find-file rmd-file)
      ;; to avoid pop-up 
      (kill-buffer))
    (setq exercise-with-code t
	  org-export-exclude-tags (list "noexport" "html" "solution" "exercises")
	  org-export-with-toc nil)
    (Rmd-export)
    (copy-file rmd-file (concat (file-name-sans-extension rmd-file) "-with-code.Rmd") 'ok)
    (setq exercise-with-code nil)
    ;; 
    ;; fourth, export to Rmd without code without toc
    ;;
    (setq exercise-without-code t)
    (Rmd-export)
    (setq exercise-without-code nil)
    ;; revert rmd buffer
    (save-window-excursion
      (find-file (org-export-output-file-name ".Rmd"))
      (revert-buffer t t t))))

;;;###autoload
(defun org-rmd-export-to-rmd
  (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let ((outfile (org-export-output-file-name ".Rmd" subtreep)))
    (org-export-to-file 'ravel-markdown outfile
      async subtreep visible-only body-only ext-plist)))


(defun superman-export-as-rmd/html ()
  "Save current buffer, then export to docx via soffice."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; when link does not exist org breaks the flow
    (ignore-errors (insert "[[./" (org-export-output-file-name ".Rmd") "][Open R-studio Rmarkdown version of this file]]\n"))
    )
  (save-buffer)
  (org-html-export-to-html)
  (save-excursion
    (goto-char (point-min))
    (kill-region (point-min) (1+ (point-at-eol))))
  ;; remove toc
  (save-excursion
    (goto-char (point-min))
    (when (and (re-search-forward "\\#\\+OPTIONS:" nil t)
	       (re-search-forward "toc:t" nil t))
      (kill-region (point) (1- (point)))
      (insert "nil")
      (org-set-regexps-and-options)
      ;;(org-ctrl-c-ctrl-c)
      ))
  ;; (superman-ravel-export-to-Rmd)
  (save-window-excursion
    (find-file (org-export-output-file-name ".Rmd"))
    ;; to avoid pop-up 
    (kill-buffer))
  (Rmd-export)
  ;; add toc
  (save-excursion
    (goto-char (point-min))
    (when (and (re-search-forward "\\#\\+OPTIONS:" nil t)
	       (re-search-forward "toc:nil" nil t))
      (kill-region (point) (- (point) 3))
      (insert "t")
      (org-set-regexps-and-options)
      ;;(org-ctrl-c-ctrl-c)
      ))
  (save-window-excursion
    (find-file (org-export-output-file-name ".Rmd"))
    (revert-buffer t t t))
  ;; (setq superman-org-export-target "rmd/html")
  ;; (superman-org-headline-mode))
  )



(defun superman-browse-org-export-target-rmd/html ()
  (interactive)
  (find-file-other-window (org-export-output-file-name ".Rmd"))
  (browse-url (org-export-output-file-name ".html")))

(defun superman-browse-org-export-target-exercise ()
  (interactive)
  (browse-url (org-export-output-file-name ".html"))
  (find-file-other-window (org-export-output-file-name "-with-code.Rmd")))

;;}}}

;;{{{ deft hack
(require 'deft)
(setq deft-extension "org"
      deft-directory  "~/Howto/"
      deft-text-mode 'org-mode)
;; (global-set-key (kbd "<f9>") 'deft)

;; (defun deft-find-all-files ()
(setq deft-use-filename-as-title t)
(deft-find-all-files)

(defun howto ()
  (interactive)
  (deft))
;; (switch-to-buffer "*deft-howto*")
;; (unless (eq major-mode 'deft-mode)
;; (deft-local-setup)
;; (setq deft-directory "~/Howto/")
;; (setq deft-find-all-files-function 'deft-find-all-files-local)
;; (setq deft-buffer (current-buffer))
;; (deft-local-mode))

(defun browse-org ()
  (interactive)
  (switch-to-buffer "*deft-browse-org*")
  (deft-local-setup)
  (setq deft-directory "~/research/OkDoc/")
  (setq deft-find-all-files-function 'deft-find-all-files-local)
  (setq deft-buffer (current-buffer))
  (deft-local-mode))

;;}}}
;;{{{ structure template
;; emacs-lisp 
(add-to-list
 'org-structure-template-alist
 '("el" "#+BEGIN_SRC  emacs-lisp :export code
\n#+END_SRC"))
;; html
(add-to-list
 'org-structure-template-alist
 '("Ha" "#+Title:\n#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"http://192.38.117.59/~tag/styles/BiomacsStyle.css\" />\n# #+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"~/public_html/styles/BiomacsStyle.css\" />\n#+OPTIONS: H:3 num:t toc:t :nil @:t ::t |:t ^:t -:t f:t *:t <:t\n#+PROPERTY: cache yes\n#+PROPERTY: session *R*\n"))
;;}}}


(defun org-clean-results-markup-garbage ()
  (interactive)
  (visible-mode 1)
  (goto-char (point-min))
  (while (re-search-forward "\\#\\+RESULTS\\|:RESULTS:\\|:END:" nil t)
    (beginning-of-line)
    (kill-line)))




(provide 'org-tag-snps)
