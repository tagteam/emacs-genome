;;; file-list-vars.el --- file-list custom

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






;; custom 
;; ----------------------------------------------------------------

(defgroup file-list nil
  "Listing of filenames below directories."
  :group 'extensions
  :link '(emacs-commentary-link :tag "Commentary" "file-list.el")
  :link '(emacs-library-link :tag "Lisp file" "file-list.el"))


(defcustom file-list-max-length 200000
  "Maximal number of file-names that can be stored in a single entry of file-list-alist."
  :type 'integer
  :group 'file-list)

(defcustom file-list-home-directory
  (file-name-as-directory (getenv "HOME"))
  "Default directory for all file-list-iswitchf and file-list-list commands."
  :type '(repeat regexp)
  :group 'file-list)

(defcustom file-list-default-directories
  (list file-list-home-directory)
  "List of directories that are initialized by the command file-list-initialize."
  :type '(repeat regexp)
  :group 'file-list)

(defcustom file-list-exclude-dirs  nil
  "String matching directories that should be excluded below file-list-home-directory"
 :type 'alist
 :group 'file-list)

(defcustom file-list-exclude-dir-regexp nil
  "Alist where each car is a regexp to be matched against directory-names
 and the cdr a regexp that is matched against the names of all subdirectories
 which should be omitted when listing files. For example
 (setq file-list-exclude-dir-regexp
      (list
       (cons file-list-home-directory
	     \"\\(^\\\.[a-w]\\|^auto$\\)\")))
 excludes all directories below file-list-home-directory that start with `.' and
 also those that are called `auto'. 
"
 :type 'alist
 :group 'file-list)

(defcustom file-list-iswitchf-prompt "iswitchf "
  "Prompt-string for file-list-iswitchf."
  :type 'string
  :group 'file-list)

(defcustom file-list-format-time-string "%a %d %b %Y, %T %Z"
  "Format of time string. See format-time-string for details."
  :type 'string
  :group 'file-list)



(defcustom file-list-clear-home-regexp
  "\\(\\\.~[0-9]?[0-9]?~$\\|\\\.aux\\)"
  "Regexp that matches files that should be deleted (frequently)."
  :type '(repeat regexp)
  :group 'file-list)

(defcustom file-list-follow-links nil
  "If non-nil follow linked directories when building or updating file-lists."
  :type 'boolean
  :group 'file-list)

(defcustom file-list-verbose nil
  "If non-nil echo names of expanded directories when building/updating of file-lists."
  :type 'boolean
  :group 'file-list)


(defcustom file-list-update t
  "If non-nil update file-list-alist just before file-list-iswitchf or file-list-list commands."
  :type 'boolean
  :group 'file-list)

;;  variables 
;;  ------------------------------------------------------------------

(defvar file-list-alist nil
  "Alist where the car of each entry is a directory name and the
cdr is the corresponding file-(a)list for that directory.")

(defvar file-list-current-file-list nil
  "List of currently selected file-names.")

(defvar file-list-excluded-dir-list nil
  "List of directories for which files are excluded.")

(defvar file-list-dir-list nil
  "Alist of directories for which files are listed.
The cdr of each entry is the modification time.")

(defvar file-list-gc-cons-threshold 50000)

(defvar file-list-display-buffer "*File-list*"
  "Buffer for displaying matching files.")

(defvar file-list-dirlist-buffer "*Dir-list*")

(defvar file-list-reference-buffer nil)

(defvar file-list-file-name-regexp
  "\\([\n\t ]/[^\t\n ]+/\\)\\([^/\t\n ]+\\)")

(defvar file-list-file-info-regexp
  "^ +\\(.*\\) : \\(.*\\)$")

(defvar file-list-iswitchf-history nil
  "History for file-list-iswitchf commands.")

(defvar file-list-regexp-history nil
  "History for regexp used by file-list commands.")

(defvar file-list-grep-history nil
  "History for regexp used by file-list command grep.")

(defvar file-list-match-history nil
  "History for matching file-list.")

(defvar file-list-display-level 1
  "In file-list-display-buffer:\n\n0 - display non-absolute file names (i.e. without path)\n
1 - display absolute file names\n
2 - display absolute file names and extra information for file (ie the result file-list-grep or file-list-attributes)")

(defvar file-list-completion-mode-hook nil
  "Normal hook run at the end kof setting up text in file-list-display-buffer.
Good for binding keys.")

(defvar file-list-magic-alist
  '((".dvi"      . "xdvi")
    (".eps"      . "evince")
    (".prn"      . "evince")
    (".doc"      . "oowriter")
    (".xls"      . "oocalc")
    (".gif"      . "geeqie")
    (".icon"     . "geeqie")
    (".ief"      . "geeqie")
    (".jpe"      . "geeqie")
    (".jpg"      . "geeqie")
    (".pdf"      . "evince")
    (".ps"       . "evince")
    (".eps"       . "evince")
    (".psF"      . "evince")
    (".tif"      . "geeqie")
    (".tiff"     . "geeqie")
    (".xbm"      . "geeqie")
    (".xpm"      . "geeqie")
    (".jpg"      . "geeqie")
    (".jpeg"     . "geeqie"))
  "*An assoc list of file extensions and the program called by the choose magic function.")

(defvar file-list-mode-map
  (let ((map (make-sparse-keymap 'file-list-mode-map)))
;    (set-keymap-parents map (list completion-list-mode-map))
    map)
  "Default keymap to use when choosing action for file
list in completion buffer.")

(provide 'file-list-vars)
;;; file-list-vars.el ends here