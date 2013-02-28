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
;; <Rs silent block
;; <Rc code only
;; <Rr results only
;; <Rb both results and code
;; <Rg graphics, prompts for file name
;; <Rt inline text: paste R results into the middle of a sentence 

;; 
;;; Code:

;;{{{ Latex & Beamer
(add-to-list
 'org-structure-template-alist
 '("Lf" "#+LaTeX: \\blfootnote{}"))
(add-to-list
 'org-structure-template-alist
 `("La" ,(concat "#+TITLE: 
#+LANGUAGE:  en
#+OPTIONS:   H:3 num:t toc:nil \\n:nil @:t ::t |:t ^:t -:t f:t *:t <:t
#+OPTIONS:   TeX:t LaTeX:t skip:nil d:nil todo:t pri:nil tags:not-in-toc author:nil
#+LaTeX_HEADER:\\usepackage{authblk}
#+LaTeX_HEADER:\\usepackage{natbib}
#+LaTeX_HEADER:\\usepackage[table,usenames,dvipsnames]{xcolor}
#+LaTeX_HEADER:\\definecolor{lightGray}{gray}{0.98}
#+LaTeX_HEADER:\\definecolor{medioGray}{gray}{0.83}
#+LaTeX_HEADER:\\rowcolors{1}{medioGray}{lightGray}
#+LaTeX_HEADER:\\usepackage{attachfile}
#+LaTeX_HEADER:\\usepackage{array}
#+LaTeX_HEADER:\\usepackage[T1]{fontenc}
#+LaTeX_HEADER:\\renewcommand*\\familydefault{\\sfdefault}
#+LaTeX_HEADER:\\author{"
user-full-name
"}
#+LaTeX_HEADER:\\affil{Department of Biostatistics, University of Copenhagen}
#+LaTeX_HEADER:\\newcommand{\\sfootnote}[1]{\\renewcommand{\\thefootnote}{\\fnsymbol{footnote}}\\footnote{#1}\\setcounter{footnote}{0}\\renewcommand{\\thefootnote}{\\arabic{foot note}}}
#+LaTeX_HEADER:\\makeatletter\\def\\blfootnote{\\xdef\\@thefnmark{}\\@footnotetext}\\makeatother
#+EXPORT_SELECT_TAGS: export
#+EXPORT_EXCLUDE_TAGS: noexport
#+LaTeX_HEADER: \\itemsep2pt
#+COLUMNS: %40ITEM %10BEAMER_env(Env) %9BEAMER_envargs(Env Args) %4BEAMER_col(Col) %10BEAMER_extra(Extra)
#+LaTeX_HEADER: \\usepackage{color}
#+LATEX_HEADER: \\lstset{
#+LATEX_HEADER: keywordstyle=\\color{blue},
#+LATEX_HEADER: commentstyle=\\color{red},
#+LATEX_HEADER: stringstyle=\\color[rgb]{0,.5,0},
#+LATEX_HEADER: basicstyle=\\ttfamily\\small,
#+LATEX_HEADER: columns=fullflexible,
#+LATEX_HEADER: breaklines=true,        % sets automatic line breaking
#+LATEX_HEADER: breakatwhitespace=false,    % sets if automatic breaks should only happen at whitespace
#+LATEX_HEADER: numbers=left,
#+LATEX_HEADER: numberstyle=\\ttfamily\\tiny\\color{gray},
#+LATEX_HEADER: stepnumber=1,
#+LATEX_HEADER: numbersep=10pt,
#+LATEX_HEADER: backgroundcolor=\\color{white},
#+LATEX_HEADER: tabsize=4,
#+LATEX_HEADER: showspaces=false,
#+LATEX_HEADER: showstringspaces=false,
#+LATEX_HEADER: xleftmargin=.23in,
#+LATEX_HEADER: frame=single,
#+LATEX_HEADER: basewidth={0.5em,0.4em}
#+LATEX_HEADER: }
#+PROPERTY: session *R* 
#+PROPERTY: cache yes")))

(add-to-list
 'org-structure-template-alist
 `("Lb" ,(concat "#+TITLE: 
#+Author: " user-full-name 
"\n#+DATE: 
#+EMAIL:" user-mail-address
"\n#+LANGUAGE:  en
#+OPTIONS:   H:3 num:t toc:nil \\n:nil @:t ::t |:t ^:t -:t f:t *:t <:t
#+OPTIONS:   TeX:t LaTeX:t skip:nil d:nil todo:t pri:nil tags:not-in-toc
#+INFOJS_OPT: view:nil toc:nil ltoc:t mouse:underline buttons:0 path:http://orgmode.org/org-info.js
#+EXPORT_SELECT_TAGS: export
#+EXPORT_EXCLUDE_TAGS: noexport
#+LINK_UP:
#+LINK_HOME: 
#+startup: beamer
#+LaTeX_CLASS: beamer
#  #+ LaTeX_class_options: [handout]
#+LaTeX_HEADER:\\usepackage{natbib}
#+LaTeX_HEADER: \\usepackage{attachfile}
#+LaTeX_HEADER: \\usepackage{array}
#+LATEX_CMD: pdflatex
#+BEAMER_FRAME_LEVEL: 2
#+LaTeX_HEADER: \\usetheme[numbers]{Dresden}
#+LaTeX_HEADER: \\setbeamercolor{structure}{fg=white}
#+LaTeX_HEADER: \\setbeamercolor*{palette primary}{fg=black,bg=white}
#+LaTeX_HEADER: \\setbeamercolor*{palette secondary}{use=structure,fg=white,bg=white}
#+LaTeX_HEADER: \\setbeamercolor*{palette tertiary}{use=structure,fg=white,bg=structure.fg!50!black}
#+LaTeX_HEADER: \\setbeamercolor*{palette quaternary}{fg=white,bg=black}
#+LaTeX_HEADER: \\setbeamercolor{item}{fg=red}
#+LaTeX_HEADER: \\setbeamercolor{subitem}{fg=orange}
#+LaTeX_HEADER: \\setbeamercolor*{sidebar}{use=structure,bg=structure.fg}
#+LaTeX_HEADER: \\setbeamercolor*{palette sidebar primary}{use=structure,fg=structure.fg!10}
#+LaTeX_HEADER: \\setbeamercolor*{palette sidebar secondary}{fg=white}
#+LaTeX_HEADER: \\setbeamercolor*{palette sidebar tertiary}{use=structure,fg=structure.fg!50}
#+LaTeX_HEADER: \\setbeamercolor*{palette sidebar quaternary}{fg=white}
#+LaTeX_HEADER: \\setbeamercolor*{titlelike}{parent=palette primary}
#+LaTeX_HEADER: \\setbeamercolor*{separation line}{}
#+LaTeX_HEADER: \\setbeamercolor*{fine separation line}{}
#+LaTeX_HEADER: \\setbeamertemplate{footline}[frame number]
#+LaTeX_HEADER: \\setbeamertemplate{navigation symbols}{}
#+LaTeX_HEADER: \\setbeamertemplate{subitem}[circle]
#+LaTeX_HEADER: \\newcommand{\\sfootnote}[1]{\\renewcommand{\\thefootnote}{\\fnsymbol{footnote}}\\footnote{#1}\\setcounter{footnote}{0}\\renewcommand{\\thefootnote}{\\arabic{foot note}}}
#+LaTeX_HEADER:\\makeatletter\\def\\blfootnote{\\xdef\\@thefnmark{}\\@footnotetext}\\makeatother
#+LATEX_HEADER: \\lstset{
#+LATEX_HEADER:         keywordstyle=\\color{blue},
#+LATEX_HEADER:         commentstyle=\\color{red},
#+LATEX_HEADER:         stringstyle=\\color[rgb]{0,.5,0},
#+LATEX_HEADER:         basicstyle=\\ttfamily\\small,
#+LATEX_HEADER:         columns=fullflexible,
#+LATEX_HEADER: breaklines=true,        % sets automatic line breaking
#+LATEX_HEADER: breakatwhitespace=false,    % sets if automatic breaks should only happen at whitespace
#+LATEX_HEADER:         numbers=left,
#+LATEX_HEADER:             numberstyle=\\ttfamily\\tiny\\color{gray},
#+LATEX_HEADER:    stepnumber=1,
#+LATEX_HEADER:    numbersep=10pt,
#+LATEX_HEADER:    backgroundcolor=\\color{white},
#+LATEX_HEADER:    tabsize=4,
#+LATEX_HEADER:    showspaces=false,
#+LATEX_HEADER:    showstringspaces=false,
#+LATEX_HEADER:    xleftmargin=.23in,
#+LATEX_HEADER:         frame=single,
#+LATEX_HEADER:         basewidth={0.5em,0.4em}
#+LATEX_HEADER:         }
#+LATEX_HEADER: \\RequirePackage{fancyvrb}
#+LATEX_HEADER: \\DefineVerbatimEnvironment{verbatim}{Verbatim}{fontsize=\\small,formatcom = {\\color[rgb]{0.5,0,0}}}
#+EXPORT_SELECT_TAGS: export
#+EXPORT_EXCLUDE_TAGS: noexport
#+LaTeX_HEADER: \\itemsep2pt
#+COLUMNS: %40ITEM %10BEAMER_env(Env) %9BEAMER_envargs(Env Args) %4BEAMER_col(Col) %10BEAMER_extra(Extra)
#+PROPERTY: session *R* 
#+PROPERTY: cache yes")))

;; Shrinking a slide
(add-to-list
 'org-structure-template-alist
 '("Bs" " :PROPERTIES:
 :BEAMER_envargs: [shrink=5]
 :END:"))
;;; Two column slides
(add-to-list
 'org-structure-template-alist
 '("Bt" "
*** Column 1                                          :B_ignoreheading:
    :PROPERTIES:
    :BEAMER_env: ignoreheading
    :BEAMER_col: 0.5
    :END:

*** Column 2                                            :B_ignoreheading:
    :PROPERTIES:
    :BEAMER_col: 0.5
    :BEAMER_env: ignoreheading
    :END:
    "))

;;}}}
;;{{{ R code objects
(add-to-list
 'org-structure-template-alist
 '("Rs" "#+BEGIN_SRC R :results silent  :exports none :session *R* \n?\n#+END_SRC"))
(add-to-list
 'org-structure-template-alist
 '("Rb" "#+BEGIN_SRC R :exports both :results output raw  :session *R* \n?\n#+END_SRC"))
(add-to-list
 'org-structure-template-alist
 '("Rc" "#+BEGIN_SRC R :exports code :results silent  :session *R* \n?\n#+END_SRC"))
(add-to-list
 'org-structure-template-alist
 '("Rl" "#+BEGIN_SRC R  :results output latex   :exports results  :session *R*\n?\n#+END_SRC"))
(add-to-list
 'org-structure-template-alist
 '("Ro" "#+BEGIN_SRC R  :results output raw  :exports results  :session *R* \n?\n#+END_SRC"))
(add-to-list
 'org-structure-template-alist
 '("Rv" "#+BEGIN_SRC R  :results value  :exports results  :session *R* \n?\n#+END_SRC"))
(add-to-list
 'org-structure-template-alist
 '("Rr" "#+BEGIN_SRC R  :results output raw  :exports results  :session *R* \n?\n#+END_SRC"))
(add-to-list
 'org-structure-template-alist
 '("Rg" "#+BEGIN_SRC R :results graphics  :file %file :exports results :session *R* \n?\n#+END_SRC"))
(add-to-list
 'org-structure-template-alist
 '("Rt" "SRC_R{}"))

;;}}}

;;{{{ graphics
(add-to-list
 'org-structure-template-alist
 '("d" "#+ATTR_LATEX: width=0.5\\textwidth\n\n#+BEGIN_SRC dot :file figure1.png :cmdline -Kdot -Tpng \n digraph overview{\"A\" -> {\"b\",\"c\"};}?\n#+END_SRC"))
;;}}}

(provide 'org-structure-snps)
;;; org-structure-snps.el ends here
