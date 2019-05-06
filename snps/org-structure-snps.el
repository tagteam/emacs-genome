;;; org-structure-snps.el --- org structure templates

;; Copyright (C) 2013  Thomas Alexander Gerds

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

;; Defines structure template for LaTeX
;; <La
;; for Beamer
;; <Lb
;; and several for R
;; <Rs silent block (no export of results) 
;; <Rc code only
;; <Rr results only
;; <Rb both results and code
;; <Re like Rb but output format example instead of raw org
;; <Rg graphics, prompts for file name
;; <Rt inline text: paste R results into the middle of a sentence 

;; 
;;; Code:
;; (setq org-structure-template-alist nil)

;;{{{ Latex & Beamer
(add-to-list
 'org-structure-template-alist
 '("Lf" "#+LaTeX: \\blfootnote{}"))
(add-to-list
 'org-structure-template-alist
 `("La" ,(concat
	  "* HEADER :noexport:\n"
	  "\n#+TITLE: "
	  "\n#+Author: " user-full-name "\n"
"#+LANGUAGE:  en
#+OPTIONS:   H:3 num:t toc:nil \\n:nil @:t ::t |:t ^:t -:t f:t *:t <:t
#+OPTIONS:   TeX:t LaTeX:t skip:nil d:t todo:t pri:nil tags:not-in-toc author:t
#+LaTeX_CLASS: org-article
#+LaTeX_HEADER:\\usepackage{authblk}
#+LaTeX_HEADER:\\author{"
user-full-name
"}
# #+LaTeX_HEADER:\\affil{Department of Biostatistics, University of Copenhagen, Copenhagen, Denmark}
#+PROPERTY: header-args :session *R*
#+PROPERTY: header-args :tangle yes
#+PROPERTY: header-args :cache yes")))

(add-to-list
 'org-structure-template-alist
 `("Lb" ,(concat
	  "* Introduction\n"
	  "** Part I\n"
	  "*** First slide\n"
	  "* HEADER :noexport:\n"
	  "\n#+TITLE:"
	  "\n#+Author: " user-full-name 
	  "\n#+Latex_header:\\institute{}"
	  "\n#+DATE: 
#+EMAIL:" user-mail-address
"\n#+OPTIONS: H:3 num:t toc:nil \\n:nil @:t ::t |:t ^:t -:t f:t *:t <:t
#+OPTIONS: TeX:t LaTeX:t skip:nil d:t todo:t pri:nil tags:not-in-toc
#+INFOJS_OPT: view:nil toc:nil ltoc:t mouse:underline buttons:0 path:http://orgmode.org/org-info.js
#+startup: beamer
#+LaTeX_CLASS: beamer
#  #+LaTeX_HEADER: \\titlegraphic{\\includegraphics[width=3cm]{xx.jpeg}}
#  #+ LaTeX_class_options: [handout]
#+LaTeX_class_options: [table] 
#+LaTeX_HEADER: \\subtitle{}
#+LaTeX_HEADER: \\setbeamertemplate{footline}[frame number]
#+LaTeX_HEADER: \\setbeamertemplate{navigation symbols}{}
#+LATEX_HEADER: \\RequirePackage{fancyvrb}
#+LATEX_HEADER: \\RequirePackage{array}
#+LATEX_HEADER: \\RequirePackage{multirow}
#+LATEX_HEADER: \\DefineVerbatimEnvironment{verbatim}{Verbatim}{fontsize=\\small,formatcom = {\\color[rgb]{0.5,0,0}}}
#+PROPERTY: header-args :session *R*
#+PROPERTY: header-args :tangle yes
#+PROPERTY: header-args :cache yes")))

;; table attributes
(add-to-list
 'org-structure-template-alist '("Bt" "
#+ATTR_LATEX: :environment longtable :align l|lp{3cm}r|l :mode math :environment bmatrix :math-suffix \times :caption \bicaption{HeadingA}{HeadingB}
"))

;; slide with different background colour
(add-to-list
 'org-structure-template-alist '("Bb" "
 *** bla
:PROPERTIES:
    :BEAMER_env: ignoreheading
    :END:
\\setbeamercolor{background canvas}{bg=black}

*** 
\\color{white}

\huge replace me

*** bla
:PROPERTIES:
    :BEAMER_env: ignoreheading
    :END:
\\setbeamercolor{background canvas}{bg=white}

"))
 
;; Shrinking a slide
(add-to-list
 'org-structure-template-alist
 '("Lt" "#+ATTR_LaTeX: :align lp{8cm}"))

;; Shrinking a slide
(add-to-list
 'org-structure-template-alist
 '("Bs" " :PROPERTIES:
 :BEAMER_opt: shrink=25
 :END:"))
;;; Two column slides
(add-to-list
 'org-structure-template-alist
 '("Bc" "
**** Untitled column
    :PROPERTIES:
    :BEAMER_col: 0.5
    :END:

**** Titled column 
    :PROPERTIES:
    :BEAMER_col: 0.5
    :BEAMER_env: block
    :END:

**** Back to no columns 
    :PROPERTIES:
    :BEAMER_env: ignoreheading
    :END:
    "))

;;}}}
;;{{{ R code objects
(add-to-list
 'org-structure-template-alist
 '("Rs" "#+BEGIN_SRC R :results output raw drawer  :exports none :session *R* :cache yes \n?\n#+END_SRC"))
(add-to-list
 'org-structure-template-alist
 '("Rb" "#+ATTR_LATEX: :options otherkeywords={}, deletekeywords={}\n#+BEGIN_SRC R :exports both :results output raw drawer  :session *R* :cache yes \n?\n#+END_SRC"))

(add-to-list
 'org-structure-template-alist
 '("Re" "#+ATTR_LATEX: :options otherkeywords={}, deletekeywords={}\n#+BEGIN_SRC R :exports both :results output :session *R* :cache yes \n?\n#+END_SRC"))

(add-to-list
 'org-structure-template-alist
 '("Rc" "#+ATTR_LATEX: :options otherkeywords={}, deletekeywords={}\n#+BEGIN_SRC R :exports code :results output raw  :session *R* :cache yes \n?\n#+END_SRC"))
(add-to-list
 'org-structure-template-alist
 '("Rl" "#+BEGIN_SRC R  :results output latex   :exports results  :session *R*\n?\n#+END_SRC"))
(add-to-list
 'org-structure-template-alist
 '("Ro" "#+BEGIN_SRC R  :results output raw drawer  :exports results  :session *R* :cache yes \n?\n#+END_SRC"))
(add-to-list
 'org-structure-template-alist
 '("Rv" "#+BEGIN_SRC R  :results value  :exports results  :session *R* :cache yes \n?\n#+END_SRC"))


(add-to-list
 'org-structure-template-alist
 '("Bo" "Hi Brice"))

(add-to-list
 'org-structure-template-alist
 '("Rr" "#+BEGIN_SRC R  :results output raw drawer  :exports results  :session *R* :cache yes \n?\n#+END_SRC"))
(add-to-list
 'org-structure-template-alist
 '("Rg"  "#+BEGIN_SRC R :results graphics :file %file :exports none :session *R* :cache yes \n?\n#+END_SRC\n\n#+name: fig:1\n#+ATTR_LATEX: :width 0.7\\textwidth\n#+CAPTION:\n"))

(add-to-list
 'org-structure-template-alist
 '("g" "\n\n#+name: fig:1\n#+ATTR_LATEX: :width 0.7\\textwidth\n#+CAPTION:\n"))

(add-to-list
 'org-structure-template-alist
 '("Lg" "\n\n#+name: fig:1\n#+ATTR_LATEX: :width 0.7\\textwidth\n#+CAPTION:\n"))

(add-to-list
 'org-structure-template-alist
 '("Bg" "\n\n#+name: fig:1\n#+ATTR_LATEX: :width 0.7\\textwidth\n#+CAPTION:\n"))

(add-to-list
 'org-structure-template-alist
 '("RG" "#+BEGIN_SRC R :results graphics  :file filename :exports results :session *R* :cache yes \n?\n#+END_SRC"))

(add-to-list
 'org-structure-template-alist
 '("Rt" "SRC_R{}"))

;;}}}


;;{{{ graphics
(add-to-list
 'org-structure-template-alist
 '("Lw" "#+name: fig:1\n#+ATTR_LATEX: :width 0.7\\textwidth\n#+CAPTION:\n#+results: fig1"))

(add-to-list
 'org-structure-template-alist
 '("d" "#+ATTR_LATEX: :width 0.5\\textwidth\n\n#+BEGIN_SRC dot :file figure1.png :cmdline -Kdot -Tpng \n digraph overview{\"A\" -> {\"b\",\"c\"};}?\n#+END_SRC"))
;;}}}

(provide 'org-structure-snps)
;;; org-structure-snps.el ends here
