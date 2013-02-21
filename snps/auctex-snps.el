;;; auctex-snps.el --- custom latex for emacs-genome 

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

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
;; (require 'tex-site)
;; (require 'tex-buf)
(setq TeX-parse-self t) 
(setq TeX-auto-save t)
(add-to-list 'auto-mode-alist (cons "\\.tex\\'" 'latex-mode))
(add-hook 'LaTeX-mode-hook 'tex-pdf-mode)   ; with AUCTeX LaTeX mode 
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(autoload 'reftex-mode    "reftex" "RefTeX Minor Mode" t) 
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil) 
(setq reftex-toc-follow-mode t)
(setq reftex-enable-partial-scans t) 
(setq reftex-save-parse-info t) 
(setq reftex-use-multiple-selection-buffers t) 
(setq reftex-plug-into-AUCTeX t)
(setq reftex-file-extensions      
      '(("Snw" "Rnw" "nw" "tex" ".tex" ".ltx") ("bib" ".bib")))
(setq bib-cite-use-reftex-view-crossref t) 
;; (autoload 'turn-on-bib-cite "bib-cite")
;; (add-hook 'LaTeX-mode-hook 'turn-on-bib-cite) 
(setq font-lock-maximum-decoration '((latex-mode . t) (t . t)))
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook '(lambda ()
			      (LaTeX-math-mode)
			      (TeX-source-specials-mode)
			      (setq TeX-master t)
			      (setq LaTeX-enable-toolbar nil)
			      (define-key LaTeX-mode-map "\M-q" 'emacs-genome-indent-paragraph)
			      (define-key LaTeX-mode-map "\M-j" 'latex-save-and-run)))


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

(defun latex-save-and-run ()
  (interactive)
  (save-buffer)
  (TeX-command-master))

(provide 'auctex-snps)
;;; auctex-snps.el ends here
