;;; latex-tags.el --- custom latex for emacs-genome 

;; Copyright (C) 2012  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <tag@biostat.ku.dk>
;; Keywords: tex, convenience

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

;;;   LaTeX, bibtex, reftex, bibcite, latexmk
(require 'tex-site)
(require 'tex-buf)
(setq TeX-parse-self t) 
(setq TeX-auto-save t) 
(add-to-list 'auto-mode-alist (cons "\\.tex\\'" 'latex-mode))

(setq LaTeX-using-Biber nil)
;;{{{ count words

;; sudo cp texcount.pl /usr/local/bin/texcount.pl
;; http://app.uio.no/ifi/texcount/download.html
(defun latex-word-count ()
  (interactive)
  (shell-command (concat "/usr/local/bin/texcount.pl "
					; "uncomment then options go here "
                         (buffer-file-name))))

;;}}}
;;{{{ bibtex
(setq bibtex-autokey-prefix-string ""
      bibtex-autokey-names 3
      bibtex-autokey-names-stretch 3
      bibtex-autokey-name-length 'infty
      bibtex-autokey-name-separator ""
      bibtex-autokey-name-case-convert 'capitalize
      bibtex-autokey-name-year-separator "_"
      bibtex-autokey-year-title-separator "_"
      bibtex-autokey-year-length 4 
      bibtex-autokey-titleword-case-convert 'capitalize
      bibtex-autokey-titlewords 3
      bibtex-autokey-titlewords-stretch 3
      bibtex-autokey-titleword-length 10
      bibtex-maintain-sorted-entries t)
(add-hook
 'bibtex-mode-hook
 (lambda nil
   (define-key bibtex-mode-map
     [(meta backspace)] 'backward-kill-word)))

;;}}}
;;{{{ reftex
(autoload 'reftex-mode    "reftex" "RefTeX Minor Mode" t) 
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil) 
(setq reftex-toc-follow-mode t)
(setq reftex-enable-partial-scans t) 
(setq reftex-save-parse-info t) 
(setq reftex-use-multiple-selection-buffers t) 
(setq reftex-plug-into-AUCTeX t)
(setq reftex-file-extensions      
      '(("Snw" "Rnw" "nw" "tex" ".tex" ".ltx") ("bib" ".bib")))
;; (setq bib-cite-use-reftex-view-crossref t) 
;; (autoload 'turn-on-bib-cite "bib-cite") 
(setq font-lock-maximum-decoration '((latex-mode . t) (t . t)))
;;}}}
;;{{{ LaTeX-mode-hook

;; (setq TeX-file-extensions      
      ;; '("Snw" "Rnw" "nw" "tex" "sty" "cls" "ltx" "texi" "texinfo"))
;; (add-hook 'LaTeX-mode-hook 'turn-on-bib-cite) 
(add-hook 'LaTeX-mode-hook 'tex-pdf-mode)   ; with AUCTeX LaTeX mode 
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode

;; (setq TeX-view-program-list '(("Okular" "okular --unique %u")))
;; (add-hook 'LaTeX-mode-hook '(lambda ()
                  ;; (add-to-list 'TeX-expand-list
                       ;; '("%u" Okular-make-url))))
;; (defun Okular-make-url () (concat
               ;; "file://"
               ;; (expand-file-name (funcall file (TeX-output-extension) t)
                         ;; (file-name-directory (TeX-master-file)))
               ;; "#src:"
               ;; (TeX-current-line)
               ;; (expand-file-name (TeX-master-directory))
               ;; "./"
               ;; (TeX-current-file-name-master-relative)))
;; (setq TeX-view-program-selection '((output-pdf "Okular")))


(add-hook 'LaTeX-mode-hook (lambda ()
  (push 
    '("Latexmk" "latexmk %s" TeX-run-TeX nil t
      :help "Run Latexmk on file")
    TeX-command-list)))

(add-hook 'LaTeX-mode-hook
	  #'(lambda ()
	     (add-to-list 'TeX-command-list '("make" "latexmk -f %t" TeX-run-command nil "nil") t)
	     (add-to-list 'TeX-command-list '("make-dvi" "latexmk -pvc -dvi -f %t" TeX-run-command nil "nil") t)
	     (add-to-list 'TeX-command-list '("make-ps"  "latexmk -pvc -ps -f %t" TeX-run-command nil "nil") t)
	     (add-to-list 'TeX-command-list '("make-pdf" "latexmk -pvc -pdf -f %t" TeX-run-command nil "nil") t)
	     (add-to-list 'TeX-command-list '("make-ps2pdf" "latexmk -pvc -pdfps -f %t" TeX-run-command nil "nil") t)
	     (add-to-list 'TeX-command-list '("make-dvi-landscape" "latexmk -pvc -l -dvi -f %t" TeX-run-command nil "nil") t)
	     (add-to-list 'TeX-command-list '("make-ps-landscape" "latexmk  -pvc -l -ps -f %t" TeX-run-command nil "nil") t)
	     (add-to-list 'TeX-command-list '("make-pdf-landscape" "latexmk -pvc -l -pdf -f %t" TeX-run-command nil "nil") t)
	     (add-to-list 'TeX-command-list '("make-ps2pdf-landscape" "latexmk -pvc -l -pdfps -f %t" TeX-run-command nil "nil") t)))

(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook #'(lambda ()
					;math-mode
			      (LaTeX-math-mode)
			      (TeX-source-specials-mode)
			      (setq TeX-master t)
			      (setq LaTeX-enable-toolbar nil)
			      ;; (define-key LaTeX-mode-map [(meta j)] 'latex-save-and-run) 
			      (define-key LaTeX-mode-map "\M-4" 'dollar-island)
			      (define-key LaTeX-mode-map "\M-q" 'eg/indent-paragraph)
			      ;; (define-key LaTeX-mode-map "\M-q" 'LaTeX-fill-paragraph)
			      (define-key LaTeX-mode-map "\M-9" 'geschweifte-klammer-island1)		
			      (define-key LaTeX-mode-map "\C-o" 'other-window)
			      (define-key LaTeX-mode-map "\M-k" 'delete-other-windows)	
			      ;; (define-key LaTeX-mode-map "\M-\C-j" 'kdvi-jump-to-line)
			      ))

;;}}}

;;{{{ custom functionality

(defun latex-save-and-run ()
  (interactive)
  (save-buffer)
  (TeX-command-master))


(defun latex-boldify-region (&optional start stop)
  (interactive "r")
  (save-excursion
    (let ((start  (if start start
		        (re-search-backward "[ ,(\t\n]" nil t)
			    (forward-char 1)
			        (point)))
	    (stop   (if stop stop
		          (re-search-forward "[^ ,)\t\n]*" nil t)
			      (forward-char 1)
			          (point))))
      ;; first insert at `stop' because
      ;; inserting at `start' changes the
      ;; position of `stop'
      (goto-char stop)
      (insert "}")
      (goto-char start)
      (insert "{\\bf "))))


(defun latex-refresh-equation-labeling (&optional arg)
  (interactive "sKey for float or equation labels (default: `eq'): ")
  (save-excursion
    (goto-char 1)
    (let ((counter "1")
	  (key (if (string= arg "") "eq" arg)))
      (while (re-search-forward (concat "label{" key ":\\(.+\\)}") nil t)
	(let ((oldlabel (match-string 1))
	      (newnum counter))
	  (unless (string= oldlabel newnum)
	    (replace-match (concat "label{" key ":" newnum "}"))
	    (save-excursion
	      (goto-char 1)
	      (while (re-search-forward (concat "ref{" key ":" oldlabel "}") nil t)
		(replace-match (concat "ref{" key ":" newnum "TMP}")))))
	  (setq counter (int-to-string (+ 1 (string-to-int counter))))))
      (goto-char 1)
      (while (re-search-forward (concat "\\(ref{" key ":\\)\\([0-9]+\\)\\(TMP}\\)") nil t)
	(replace-match (concat (match-string 1) (match-string 2) "}"))))))

;; (defun clean-bib ()
;; (interactive)
;; (while (re-search-forward "\\[\\([0-9]+\\)\\]" nil t)
;; (replace-match (concat "\\\\cite{bib"  (match-string 1) "}"))))

(defvar latex-insert-graphics-counter nil)
(setq latex-insert-graphics-counter 0)
(defvar latex-insert-graphics-file nil)
(defun latex-insert-graphics (&optional file)
  (interactive)
  (if (eq last-command 'latex-insert-graphics)
      (progn (cond ((eq latex-insert-graphics-counter 0)
		    (undo)
		    (insert (concat "\\includegraphics[width=0.9\\textwidth,angle=-90]{"
				    latex-insert-graphics-file
				    "}\n")))
		   ((eq latex-insert-graphics-counter 1)
		    (insert (concat "\\includegraphics[width=0.9\\textwidth]{"
				    latex-insert-graphics-file
				    "}\n")))
		   ((eq latex-insert-graphics-counter 2)
		    (insert (concat "\\includegraphics[width=0.9\\textwidth]{"
				    (expand-file-name latex-insert-graphics-file)
				    "}\n")))
		   ((eq latex-insert-graphics-counter 3)
		    (insert (concat "\\includegraphics[width=0.9\\textwidth, angle=-90]{"
				    (expand-file-name latex-insert-graphics-file)
				    "}\n"))))
	     (setq latex-insert-graphics-counter (+ 1 latex-insert-graphics-counter))
	     (if (> latex-insert-graphics-counter 3) setq latex-insert-graphics-counter 0))
    (setq latex-insert-graphics-file (read-file-name "Graphics file: "))
    (insert (concat "\\includegraphics[width=0.9\\textwidth]{"
		    (file-name-nondirectory latex-insert-graphics-file)
	    "}\n"))
    (message "Call same command again to toggle file expansion and/or angle")))

(defun latex-insert-multicol ()
  (interactive)
  (insert (concat "\\multicolumn{Anzahl}{Format}{Text}")))

(defun latex-insert-column ()
  (interactive)
  (insert (concat "\\column{.5\\textwidth}")))

(defun latex-insert-multirow ()
  (interactive)
  (insert (concat "\\multirow{no.rows}{width}{text}"))
  (message "load-package multirow"))

(defun latex-insert-framebox ()
  (interactive)
  (insert (concat "\\framebox[width][pos]{text}}")))

(defun latex-make-2x2-handouts ()
  (interactive)
  (let ((fname (file-name-sans-extension (buffer-file-name))))
    (async-shell-command "pdfnup --nup 2x2 --suffix '2x2' --batch " (concat fname ".pdf"))))


(defun bib2txt ()
  (interactive)
  (when (region-active-p)
    (narrow-to-region (region-beginning) (region-end)))
  ;; (narrow-to-region beg end))
  (let ((bib (buffer-substring (point-min) (point-max)))
	(conf (current-window-configuration))
	tmp-buf)
    (widen)
    (condition-case nil
	(delete-file "~/tmp/tmp.html")
      (delete-file "~/tmp/tmp.txt")
      (delete-file "~/tmp/tmp.bib")
      (error nil))
    (when (setq tmp-buf
		(get-file-buffer "~/tmp/tmp.bib"))
      (save-excursion
	(set-buffer tmp-buf)
	(revert-buffer t t)))
    (find-file "~/tmp/tmp.bib")
    (erase-buffer)
    (insert bib)
    (save-buffer)
    (shell-command  "bibtex2html -nobibsource -o ~/tmp/tmp ~/tmp/tmp.bib")
    ;; (shell-command "vilistextum ~/tmp/tmp.html ~/tmp/tmp.txt")
    (when (setq tmp-buf
		(get-file-buffer "~/tmp/tmp.txt"))
      (save-excursion
	(set-buffer tmp-buf)
	(revert-buffer t t)))
    (find-file "~/tmp/tmp.txt")
    (erase-buffer)
    (shell-command "html2text -ascii ~/tmp/tmp.html > ~/tmp/tmp.txt")
    ;; (when (setq tmp-buf (get-file-buffer "~/tmp/tmp.txt"))
    (when (setq tmp-buf
		(get-file-buffer "~/tmp/tmp.txt"))
      (save-excursion
	(set-buffer tmp-buf)
	(revert-buffer t t)))
    (find-file "~/tmp/tmp.txt")
    (revert-buffer t t)
    (set-window-configuration conf)
    (pop-to-buffer tmp-buf)))


(defun umlaut-repair-string  (string)
  (setq string (replace-in-string string "ó" "o"))
  (setq string (replace-in-string string "á" "a"))
  (setq string (replace-in-string string "ñ" "n"))
  (setq string (replace-in-string string "ë" "ee"))
  (setq string (replace-in-string string "é" "ee"))
  (setq string (replace-in-string string "á" "a"))
  (setq string (replace-in-string string "ä" "ae"))
  (setq string (replace-in-string string "ö" "oe"))
  (setq string (replace-in-string string "ü" "ue"))
  (setq string (replace-in-string string "ø" "oe"))
  (setq string (replace-in-string string "Ø" "Oe"))
  (setq string (replace-in-string string  "å" "aa"))
  (setq string (replace-in-string string "Å" "Aa"))
  (setq string (replace-in-string string "æ" "ae"))
  (setq string (replace-in-string string "Æ" "Ae")))



(defun latex-fix-umlauts ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "Position{Research Assistant}" nil t)
    (beginning-of-line)
;     (insert "\n\\newpage\n")
    (goto-char (point-min))
    (while (re-search-forward "[^\\\\]&" nil t)
      (replace-match " \\\\&"))
    (goto-char (point-min))
    (while (re-search-forward "~" nil t)
      (replace-match "\\'"))
    (goto-char (point-min))
    (while (re-search-forward "~" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "~" nil t)
      (replace-match ""))
					;  accent
    (goto-char (point-min))
    (while (re-search-forward "á" nil t)
      (replace-match "\\\\acute{a}"))
    (goto-char (point-min))
    (while (re-search-forward "ó" nil t)
      (replace-match "\\\\'{o}"))
    (goto-char (point-min))
    (while (re-search-forward "é" nil t)
      (replace-match "\\\\'{e}"))
    (goto-char (point-min))
    (while (re-search-forward "ô" nil t)
      (replace-match "\\\\^{o}"))
    
					;   german
    (goto-char (point-min))
    (while (re-search-forward "ë" nil t)
      (replace-match "\\\\\\\"e"))
    (goto-char (point-min))
    (while (re-search-forward "ä" nil t)
      (replace-match "\\\\\\\"a"))
    (goto-char (point-min))
    (while (re-search-forward "ö" nil t)
      (replace-match "\\\\\\\"o"))
    (goto-char (point-min))
    (while (re-search-forward "ü" nil t)
      (replace-match "\\\\\\\"u"))
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



(defun eg/bibtex-generate-autokey ()
  (let* ((pnt (point))
         (min (bibtex-beginning-of-entry))
         (max (bibtex-end-of-entry))
         (namefield (bibtex-autokey-get-namefield min max))
         (name-etal "")
         (namelist
          (let ((nl (bibtex-autokey-get-namelist namefield)))
            (if (or (not (numberp bibtex-autokey-names))
                    (<= (length nl)
                        (+ bibtex-autokey-names
                           bibtex-autokey-names-stretch)))
                nl
              (setq name-etal bibtex-autokey-additional-names)
              (let (nnl)
                (while (< (length nnl) bibtex-autokey-names)
                  (setq nnl (append nnl (list (car nl)))
                        nl (cdr nl)))
                nnl))))
         (namepart
          (concat
           (mapconcat (lambda (name) name)
                      namelist
                      bibtex-autokey-name-separator)
           name-etal))
         (yearfield (bibtex-autokey-get-yearfield min max))
         (yearpart
          (if (equal yearfield "")
              ""
            (substring
             yearfield
             (- (length yearfield) bibtex-autokey-year-length))))
	 
         (titlestring (bibtex-autokey-get-titlestring min max))
         (titlelist (bibtex-autokey-get-titlelist titlestring))
         (titlepart
          (mapconcat
           (lambda (name) name)
           titlelist
           bibtex-autokey-titleword-separator))
         (autokey
          (concat
           bibtex-autokey-prefix-string
           yearpart
           (if (not
                (or
                 (equal namepart "")
                 (equal yearpart "")))
               bibtex-autokey-name-year-separator)
	   namepart
           (if (not
                (or
                 (and
                  (equal namepart "")
                  (equal yearpart ""))
                 (equal titlepart "")))
               bibtex-autokey-year-title-separator)
           titlepart)))
    (if bibtex-autokey-before-presentation-function
        (setq
         autokey
         (funcall bibtex-autokey-before-presentation-function autokey)))
    (goto-char pnt)
    autokey))

;;}}}
;;{{{ toolbar
(setq LaTeX-enable-toolbar nil)
;;}}}

(defun dollar2equation ()
  (interactive)
  (while (re-search-forward "\\$\\$" nil t)
    (replace-match "#+BEGIN_LaTeX\n\\\\begin{equation*}\n")
    (re-search-forward "\\$\\$" nil t)
    (replace-match "\n\\\\end{equation*}\n#+END_LaTeX")))


;;{{{ clean directory
(defun latex-cleanup (&optional dir)
  (interactive)
  (let ((dir (or dir (file-name-directory (buffer-file-name (current-buffer))))))
    (shell-command (concat "cd " dir ";rm *.aux *.log *.snm *.fdb* *.vrb *.nav *.toc *.bbl *.out *.blg *.run.xml *blx.bib"))))
    ;;}}}
;;{{{ Collecting all graphics files in a new sub-directory 
(defun latex-collect-graphics ()
  "Successively find includegraphics statements and copy corresponding
graphics to a new user-defined directory. As a side effect the
includegraphics statements are changed"
  (interactive)
  (let* ((curdir (file-name-directory (expand-file-name (buffer-file-name (current-buffer)))))
	 (dir (expand-file-name (read-directory-name "Directory for collecting graphics: " curdir curdir nil)))
	 (mess1 "Messages from latex graphics collection process:\n\nFiles successfully copied:\n----------------------------\n\n")
	 (mess2 "\nFiles not found (missing extension in latex document?)\n--------------------------------------------------------\n\n")
	 (mess3 "\nFiles that existed in target dir (these are not overwritten!)\n-----------------------------------------------------------------\n\n")
	 (mess4 "\nFiles that are commented out\n------------------------------------------------\n\n"))
    (unless (file-exists-p dir)
      (if (y-or-n-p (concat "Directory " dir " does not exist. Create? "))
	  (make-directory dir)
	(error nil)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\\includegraphics.*{\\(.*\\)[}]+" nil t)
	(let* ((match (match-string-no-properties 1))
	       (source (expand-file-name match))
	       (target (expand-file-name (concat (file-name-as-directory dir) (file-name-nondirectory source))))
	       (smart-target (if (string-match (getenv "HOME") target)
				 (replace-in-string target (getenv "HOME") "~")))
	       (very-smart-target (if (string-match curdir target)
				      (replace-in-string target curdir "./") nil)))
	  (if (save-excursion
		(let ((end (point)))
		  (beginning-of-line)
		  (re-search-forward "%" end t)))
	      (setq mess4 (concat mess4 source "\n"))
	    (if (not (file-exists-p source))
		(setq mess2 (concat mess2 source "\n"))
	      (if (file-exists-p target)
		  (setq mess3 (concat mess3 target "\n"))
		(copy-file source target)
		(setq mess1 (concat mess1 (concat source " copied to " target ".\n"))))
	      (re-search-backward "{" nil t)
	      (forward-char 1)
	      (looking-at match)
	      (replace-match (or very-smart-target smart-target target)))))))
    (pop-to-buffer "*Latex graphics collection messages*")
    (erase-buffer)
    (insert mess1 mess2 mess3)
    (goto-char (point-min))))

(defun latex-collect ()
  (interactive)
  (let* ((tex-buf  (current-buffer))
	 (tex-file (buffer-file-name tex-buf))
	 (dd (file-name-directory tex-file))
	 (target (file-name-as-directory (concat (file-name-sans-extension tex-file) "-local")))
	 (collect-buf (get-buffer-create "*latex-collect*")))
    (unless (file-exists-p target)
      (make-directory target))
    (copy-file
     tex-file
     (concat target (file-name-nondirectory tex-file)) 'ok)
    (set-buffer collect-buf)
    (erase-buffer)
    (insert
     "Copied: "
     (concat target (file-name-nondirectory tex-file)) "\n")
    (set-buffer tex-buf)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "includegraphics.*{\\(.*\\)}" nil t)
	(let ((gfile (expand-file-name (match-string-no-properties 1) dd)))
	  (if (file-exists-p gfile)
	      (progn
		(copy-file gfile (concat target (file-name-nondirectory gfile)) 'ok)
		(set-buffer collect-buf)
		(insert "Copied: " (concat target (file-name-nondirectory gfile)) "\n"))
	    (set-buffer collect-buf)
	    (insert (concat "File does not exist: " gfile) "\n"))
	  (set-buffer tex-buf))))
    (dired target)
    (pop-to-buffer collect-buf)))
;;}}}
  
(provide 'latex-tags)
;;; latex-tags.el ends here
