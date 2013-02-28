;;; latex-snps.el --- custom latex for emacs-genome 

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

;; LaTeX and reftex settings
;; Use M-j to latex or latexmk a document

;;; Code:

;;{{{ load auctex
(add-to-list  'load-path (concat (getenv "HOME") "/emacs-genome/genes/auctex/"))
(add-to-list  'load-path (concat (getenv "HOME") "/emacs-genome/genes/auctex/style"))
(add-to-list  'load-path (concat (getenv "HOME") "/emacs-genome/genes/auctex/preview"))

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(require 'tex-buf)
;;}}}
;;{{{ LaTeX mode hook
(add-hook 'LaTeX-mode-hook
	  '(lambda ()
	     (LaTeX-math-mode)
	     (TeX-source-correlate-mode)
	     (setq TeX-master t)
	     (TeX-PDF-mode t)
	     (define-key LaTeX-mode-map "\C-ce" 'TeX-next-error)
	     (define-key LaTeX-mode-map "\M-q" 'eg/indent-paragraph)
	     (define-key LaTeX-mode-map "\M-j" 'eg/latex-save-and-run)))
;;}}}
;;{{{ TeX shell and master and custom 
(setq TeX-shell "/bin/bash")
(setq-default TeX-master nil) ; Query for master file.
(setq TeX-parse-self t) 
(setq TeX-auto-save t)
(setq TeX-file-extensions
      '("Snw" "Rnw" "nw" "tex" "sty" "cls" "ltx" "texi" "texinfo"))
(setq reftex-try-all-extensions t)
;;}}}
;;{{{ reftex
(autoload 'reftex-mode    "reftex" "RefTeX Minor Mode" t) 
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil) 
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-file-extensions '(("Snw" "Rnw" "nw" "tex" ".tex" ".ltx") ("bib" ".bib")))
;;}}}
;;{{{ latex run command
(defun eg/latex-save-and-run ()
  "Efficiently combine two actions:

 (1) save the current buffer
 (2) run the TeX-command-master.

"
  (interactive)
  (save-buffer)
  (TeX-command-master))
;;}}}
;;{{{ latexmk
(add-hook 'LaTeX-mode-hook
	  '(lambda ()
	     (add-to-list 'TeX-command-list '("make" "latexmk -f %t" TeX-run-command nil "nil") t)
	     (add-to-list 'TeX-command-list '("make-dvi" "latexmk -pvc -dvi -f %t" TeX-run-command nil "nil") t)
	     (add-to-list 'TeX-command-list '("make-ps"  "latexmk -pvc -ps -f %t" TeX-run-command nil "nil") t)
	     (add-to-list 'TeX-command-list '("make-pdf" "latexmk -pvc -pdf -f %t" TeX-run-command nil "nil") t)
	     (add-to-list 'TeX-command-list '("make-ps2pdf" "latexmk -pvc -pdfps -f %t" TeX-run-command nil "nil") t)
	     (add-to-list 'TeX-command-list '("make-dvi-landscape" "latexmk -pvc -l -dvi -f %t" TeX-run-command nil "nil") t)
	     (add-to-list 'TeX-command-list '("make-ps-landscape" "latexmk  -pvc -l -ps -f %t" TeX-run-command nil "nil") t)
	     (add-to-list 'TeX-command-list '("make-pdf-landscape" "latexmk -pvc -l -pdf -f %t" TeX-run-command nil "nil") t)
	     (add-to-list 'TeX-command-list '("make-ps2pdf-landscape" "latexmk -pvc -l -pdfps -f %t" TeX-run-command nil "nil") t)))
;;}}}


(provide 'latex-snps)
;;; latex-snps.el ends here
