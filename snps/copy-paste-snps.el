;;; copy-paste-snps.el --- emacs version of copy and paste

;; Copyright (C) 2011  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <tagteam@sund.ku.dk>
;; Keywords: local, convenience

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

;; ,----
;; | 
;; | the emacs default is C-y for yank and M-y for yank-pop
;; | the following code combines yank and yank-pop
;; | such that the same key (for me: "M-y") is used for
;; | both commands ...
;; `----

;; ,----
;; | my fingers have difficulties to reach "M-w", so I bind copy to "M-r"  
;; `----

;;; Code:

(defun yank-or-pop (arg)
  "Combine `yank' with `yank-pop'."
  (interactive "*p") 
  (if (eq last-command 'yank)
      (yank-pop arg)
    (yank arg))
  nil)
(global-set-key "\M-y" 'yank-or-pop)
(global-set-key "\M-r" 'copy-region-as-kill)
;; similar to kill-ring-save)

(provide 'copy-paste-snps)
;;; copy-paste-snp.el ends here
