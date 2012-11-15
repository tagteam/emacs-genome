;;; file-list-keymap.el --- file-list keybindings

;; Copyright (C) 2012  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <tag@biostat.ku.dk>
;; Keywords: files, convenience

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





;; keybindings for the file-list-display-buffer

(define-key file-list-mode-map [(return)] 'file-list-choose-file)
(define-key file-list-mode-map [(meta return)] 'file-list-choose-magic)
(if (featurep 'xemacs)
    (define-key file-list-mode-map [(space)] 'file-list-choose-file-other-window)
  (define-key file-list-mode-map (kbd "SPC") 'file-list-choose-file-other-window))
(define-key file-list-mode-map "a" 'file-list-mml-attach-file-at-point)
(define-key file-list-mode-map "A" 'file-list-mml-attach)
(define-key file-list-mode-map "b" 'file-list-previous-file)
(define-key file-list-mode-map "c" 'file-list-copy-file-at-point)
(define-key file-list-mode-map "C" 'file-list-copy)
(define-key file-list-mode-map "d" 'file-list-dired)
(define-key file-list-mode-map "e" 'file-list-end-of-file-list)
(define-key file-list-mode-map "f" 'file-list-choose-file)
(define-key file-list-mode-map "F" 'file-list-find)
(define-key file-list-mode-map "g" 'file-list-grep)
(define-key file-list-mode-map "k" 'file-list-remove-file-at-point)
(define-key file-list-mode-map "K" 'file-list-remove)
(define-key file-list-mode-map "l" 'file-list-attributes)
(define-key file-list-mode-map "L" 'file-list-ls)
(define-key file-list-mode-map "m" 'file-list-move-file-at-point)
(define-key file-list-mode-map "M" 'file-list-move)
(define-key file-list-mode-map "\t" 'file-list-next-file)
(define-key file-list-mode-map "n" 'file-list-next-file)
(define-key file-list-mode-map "o" 'file-list-omit-file-at-point)
(define-key file-list-mode-map "p" 'file-list-previous-file)
(define-key file-list-mode-map "q" 'file-list-quit)
(define-key file-list-mode-map "r" 'file-list-rename-file-at-point)
(define-key file-list-mode-map "R" 'file-list-rename)
(define-key file-list-mode-map "t" 'file-list-toggle-display-mode)
(define-key file-list-mode-map "u" 'file-list-redisplay)
(define-key file-list-mode-map "x" 'file-list-shell-command-at-point)
(define-key file-list-mode-map "X" 'file-list-shell-command)
(define-key file-list-mode-map "y" 'file-list-add)
(define-key file-list-mode-map "\C-c" 'file-list-clear-display)
(define-key file-list-mode-map "\C-t" 'file-list-toggle-display-mode)
(define-key file-list-mode-map "Ss" 'file-list-sort-by-size)
(define-key file-list-mode-map "St" 'file-list-sort-by-time)
(define-key file-list-mode-map "Sf" 'file-list-sort-by-name)
(define-key file-list-mode-map "Sp" 'file-list-sort-by-path)
(define-key file-list-mode-map "Uu" 'file-list-update)
(define-key file-list-mode-map "Ud" 'file-list-update-below-dir)
(define-key file-list-mode-map "/s" 'file-list-by-size)
(define-key file-list-mode-map "/t" 'file-list-by-time)
(define-key file-list-mode-map "/f" 'file-list-by-name)
(define-key file-list-mode-map "/p" 'file-list-by-path)
(define-key file-list-mode-map "/a" 'file-list-add)


(defun file-list-default-keybindings ()
  "Set up default keybindings'."
  (interactive)
  (global-unset-key "\C-xf")
  (global-set-key (read-kbd-macro "C-x f f")  'file-list-iswitchf-file)
  (global-set-key (read-kbd-macro "C-x f m") 'file-list-iswitchf-magic)
  
  (global-set-key (read-kbd-macro "C-x f 4 f") 'file-list-iswitchf-file-other-window)
  (global-set-key (read-kbd-macro "C-x f 5 f") 'file-list-iswitchf-file-other-frame)
  (global-set-key (read-kbd-macro "C-x f 4 d") 'file-list-iswitchf-below-directory-other-window)
  (global-set-key (read-kbd-macro "C-x f 5 d") 'file-list-iswitchf-below-directory-other-frame)

  (global-set-key (read-kbd-macro "C-x f d") 'file-list-iswitchf-below-directory)

  (global-set-key (read-kbd-macro "C-x f u") 'file-list-update)
  (global-set-key (read-kbd-macro "C-x f U") 'file-list-update-below-dir)

  (global-set-key (read-kbd-macro "C-x f s") 'file-list-by-size)
  (global-set-key (read-kbd-macro "C-x f t") 'file-list-by-time)
  (global-set-key (read-kbd-macro "C-x f p") 'file-list-by-path)
  (global-set-key (read-kbd-macro "C-x f P") 'file-list-by-path-below-directory)
  (global-set-key (read-kbd-macro "C-x f l") 'file-list-by-name)
  (global-set-key (read-kbd-macro "C-x f L") 'file-list-by-name-below-directory))


;(find-menu-item current-menubar '("File" "List by file-size"))
; (defun file-list-add-submenu (&optional delete)
;   (interactive "P")
;   (if delete
;       (mapcar '(lambda (string)
; 		 (delete-menu-item `("File" ,(concat "List by " string))))
; 	      (list "file-size" "file-age" "file-name" "path-name"))
;     (add-menu-button '("File") ["%_List by file-name" file-list-by-name t] "Save" nil)
;     (add-menu-button '("File") ["List by path-name" file-list-by-path t] "Save" nil)
;     (add-menu-button '("File") ["List by file-age" file-list-by-time t] "Save" nil)
;     (add-menu-button '("File") ["List by file-size" file-list-by-size t] "Save" nil)
;     (add-menu-button '("File") "-----" "Save" nil)))

(provide 'file-list-keymap)
;;; file-list-keymap.el ends here