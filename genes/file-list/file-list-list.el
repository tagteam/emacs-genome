;;; file-list-list.el --- file-list listings

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






;; pre selection and sorting of files for display in
;; the file-list-display-buffer

;;  selection
;;  ------------------------------------------------------------------
  
(defun file-list-select-existing-files (&optional file-list)
  "Check existence (on disk) of all files in file-list-current-file-list.
Return  the sublist of the existing files. Does not re-display selected list."
  (interactive)
  (let ((gc-cons-threshold file-list-gc-cons-threshold)
	(file-list (or file-list file-list-current-file-list)))
    (setq file-list-current-file-list
	  (delete nil (mapcar (lambda (entry)
				(when (file-exists-p
				       (file-list-make-file-name entry))
				  entry))
			      file-list))))
  file-list-current-file-list)

(defun file-list-redisplay ()
  (interactive)
  (file-list-select-existing-files)
  (file-list-display-match-list))

(defun file-list-select-internal (&optional file-list regexp by inverse dir display-buffer dont-display)
  "Returns sublist of filenames in file-list matched by regexp.
Changes the file-list-current-file-list. See also file-list-add."
  (setq file-list-reference-buffer (current-buffer))
  (let* (
	 ;; (gc-cons-threshold file-list-gc-cons-threshold)
	 (display-buffer (or display-buffer
			     (file-list-current-display-buffer)
			     file-list-display-buffer))
	 (file-list (cond (file-list)
			  (dir (when file-list-update
				 (file-list-update dir nil))
			       (file-list-list dir nil nil 'recursive nil))
			  ((or (eq major-mode 'file-list-completion-mode)
			       (eq major-mode 'superman-file-list-mode))
			   file-list-current-file-list)
			  (t (if file-list-update
				 (file-list-update dir nil))
			     ;;FIXME: write function file-list-update-file-list-alist
			     (file-list-list
			      file-list-home-directory nil nil 'recursive nil))))
	 (prompt-string (format "Select files whose %s %s "
				(cond ((not by) "filename")
				      ((string= by "path") "pathname")
				      ((string= by "time") "age in days")
				      ((string= by "size") "size in bytes")
				      (t "filename"))
				(cond ((not by) (format "is%s matched by regexp " (if inverse " not" "")))
				      ((string= by "time") (format "exceeds%s " (if (not inverse) " not" "")))
				      ((string= by "size") (format "exceeds%s " (if inverse " not" "")))
				      (t (format "is%s matched by regexp " (if inverse " not" ""))))))
	 (regexp (or regexp (read-string
			     prompt-string
			     nil
			     'file-list-regexp-history)))
	 (match-info (cond ((string= by "time")
			    (format "\nage"  "%s '%s' day%s"
				    (if inverse " no-exceed:" "exceed:") regexp (if (string= regexp "1") "" "s")))
			   ((string= by "size")
			    (format "\nsize %s '%s'" (if inverse " no-exceed:" "exceed:") regexp))
			   ((string= by "path")
			    (format "\ndirectory-name %s '%s'" (if inverse " no-match:" "match:") regexp))
			   (t (format "\nfile-name %s '%s'" (if inverse " no-match:" "match:") regexp))))
	 ;; (match-info (cond ((string= by "time")
	 ;; (format "whose age exceeds%s '%s' day%s" (if inverse "" " not")
	 ;; regexp (if (string= regexp "1") "" "s")))
	 ;; ((string= by "size")
	 ;; (format "whose size exceeds%s '%s' bytes" (if inverse " not" "") regexp))
	 ;; ((string= by "path")
	 ;; (format "whose path match%s '%s'" (if inverse " not" "") regexp))
	 ;; (t (format "whose filename match%s '%s'" (if inverse " not" "") regexp))))
	 (test (cond 
		((not by) nil)
		((string= by "path")
		 '(lambda (entry)
		    (if (not (file-exists-p
			      (file-list-make-file-name entry)))
			nil
		      (let ((keep (string-match regexp (cadr entry))))
			(cond ((and (not inverse) (not keep)) nil)
			      ((and inverse keep) nil)
			      (t entry))))))
		((string= by "size")
		 '(lambda (entry)
		    (if (not (file-exists-p
			      (file-list-make-file-name entry)))
			nil
		      (let ((keep 
			     (< (string-to-int regexp)
				(nth 7  (file-attributes
					 (file-list-make-file-name entry))))))
			(cond ((and (not inverse) (not keep)) nil)
			      ((and inverse keep) nil)
			      (t entry))))))
		((string= by "time")
		 '(lambda (entry)
		    (if (not (file-exists-p
			      (file-list-make-file-name entry)))
			nil
		      (let* ((entry-time (nth 5 (file-attributes (file-list-make-file-name entry))))
			     (keep (or (not entry-time)
				       (file-list-time-less-p
					(subtract-time (current-time) (days-to-time (string-to-int regexp)))
					entry-time))))
			(cond ((and (not inverse) (not keep)) nil)
			      ((and inverse keep) nil)
			      (t entry))))))
		(t nil)))
	 (sub-file-list (file-list-extract-sublist
			 file-list
			 (or test regexp)
			 inverse)))
    (if (null sub-file-list)
	(progn
	  (message "No files are matched by this criteria." regexp)
	  nil)
      (unless dont-display
	(if (eq major-mode 'superman-file-list-mode)
	    (superman-display-file-list nil sub-file-list match-info (current-buffer))
	  (file-list-display-match-list sub-file-list match-info display-buffer))
	(setq file-list-current-file-list sub-file-list))
      sub-file-list)))


(defun file-list-by-name (&optional arg file-list dir)
  "Returns sublist of filenames (in file-list) whose path is matched by regexp.
This changes the value of file-list-current-file-list."
  (interactive "P")
  (file-list-select-internal file-list nil nil arg dir))


(defun file-list-by-name-below-directory (&optional arg file-list)
  "Reads directory name and returns sublist of filenames below directory whose
raw file-name match a regexp.
This changes the value of file-list-current-file-list."
  (interactive "P")
  (let ((dir (read-directory-name (format
				   "%s of files by name below directory: "
				   (if arg "Inverse selection" "Selection")))))
  (file-list-select-internal file-list nil nil arg dir)))


(defun file-list-by-path (&optional arg file-list)
 "Returns sublist of filenames (in file-list) whose path-name is matched by regexp.
This changes the value of file-list-current-file-list."
  (interactive "P")
    (file-list-select-internal file-list nil "path" arg))

(defun file-list-by-path-below-directory (&optional arg file-list)
 "Reads directory name and returns sublist of filenames (in file-list) whose path
is matched by a regexp. This changes the value of file-list-current-file-list."
  (interactive "P")
  (let ((dir (read-directory-name (format
				   "%s of files by pathname below directory: "
				   (if arg "Inverse selection" "Selection")))))
    (file-list-select-internal file-list nil "path" arg dir)))


(defun file-list-by-time (&optional arg file-list)
  "Returns sublist of filenames (in file-list) whose size is lower than a given value.
This changes the value of file-list-current-file-list."
  (interactive "P")
  (file-list-select-internal file-list nil "time" arg))


(defun file-list-by-time-below-directory (&optional arg file-list)
  "Reads directory name and returns sublist of filenames (in file-list) whose age
is less or greater than a number of days which also has to be specified.
This changes the value of file-list-current-file-list."
  (interactive "P")
  (let ((dir (read-directory-name (format
				   "%s of files by age below directory: "
				   (if arg "Inverse selection" "Selection")))))
  (file-list-select-internal file-list nil "time" arg dir)))


(defun file-list-by-size (&optional arg file-list)
 "Returns sublist of filenames (in file-list) whose size is lower than a given value.
This changes the value of file-list-current-file-list."
  (interactive "P")
  (file-list-select-internal file-list nil "size" arg))


(defun file-list-by-size-below-directory (&optional arg file-list)
  "Reads directory name and returns sublist of filenames (in file-list) whose size
is less or greater than a number in bytes which also has to be specified.
This changes the value of file-list-current-file-list."
  (interactive "P")
  (let ((dir (read-directory-name (format
				   "%s of files by size below directory: "
				   (if arg "Inverse selection" "Selection")))))
    (file-list-select-internal file-list nil "size" arg dir)))


(defun file-list-add (&optional file-list dir regexp)
  "Finds sublist of filenames below dir matched by regexp.
The result is appended to the value file-list-current-file-list.
See also file-list-select."
  (interactive)
  (let* ((file-list (or file-list
			file-list-current-file-list))
	 (dir (or dir
		  (read-directory-name
		   (format "Directory for files to add (default: %s) "
			   file-list-home-directory)
		   file-list-home-directory
		   file-list-home-directory t)))
	 (regexp (or regexp
		     (read-string "Regexp to match filenames "
				  nil
				  'file-list-regexp-history)))
	 (add-file-list
	  (file-list-extract-sublist
	   (progn (when file-list-update (file-list-update dir nil))
		  (file-list-list dir nil nil 'recursive nil))
	   regexp))
	 new-list)
    (if (= (length add-file-list) 0)
	(message (format "No files added." dir regexp))
      ;; update current file-list
      (let ((name-list (mapcar 'file-list-make-file-name file-list)))
	(dolist (entry add-file-list)
	  (unless (member (file-list-make-file-name entry) name-list)
	    (setq new-list (append new-list (list entry))))))
      (setq file-list-current-file-list (append file-list new-list))
      (file-list-display-match-list file-list-current-file-list))))

;;  sorting file-lists 
;;  ------------------------------------------------------------------

;; next functions stolen from ognus' time-date.el
(defun file-list-time-less-p (t1 t2)
  "Say whether time value T1 is less than time value T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (< (nth 1 t1) (nth 1 t2)))))

(defun days-to-time (days)
  "Convert DAYS into a time value."
  (let* ((seconds (* 1.0 days 60 60 24))
	 (rest (expt 2 16))
	 (ms (condition-case nil (floor (/ seconds rest))
	       (range-error (expt 2 16)))))
    (list ms (condition-case nil (round (- seconds (* ms rest)))
	       (range-error (expt 2 16))))))

(defalias 'subtract-time 'time-subtract)

(defun time-subtract (t1 t2)
  "Subtract two time values.
Return the difference in the format of a time value."
  (let ((borrow (< (cadr t1) (cadr t2))))
    (list (- (car t1) (car t2) (if borrow 1 0))
	  (- (+ (if borrow 65536 0) (cadr t1)) (cadr t2)))))

(defun file-list-sort-by-size (&optional reverse file-list)
  (interactive "P")
  (let ((file-list (or file-list file-list-current-file-list)))
    (file-list-sort-internal file-list "size" reverse)))

(defun file-list-sort-by-time (&optional reverse file-list)
  (interactive "P")
  (let ((file-list (or file-list file-list-current-file-list)))
     (file-list-sort-internal file-list "time" reverse)))

(defun file-list-sort-by-name (&optional reverse file-list)
  (interactive "P")
  (let ((file-list (or file-list file-list-current-file-list)))
     (file-list-sort-internal file-list "name" reverse)))

(defun file-list-sort-by-path (&optional reverse file-list)
  (interactive "P")
  (let ((file-list (or file-list file-list-current-file-list)))
     (file-list-sort-internal file-list "path" reverse)))

(defun file-list-parse-size (string)
  (let ((lst (mapcar 'string-to-int (delq "" (split-string string "G[ ]*\\|M[ ]*\\|K[ ]*\\|B[ ]*"))))
	terra giga mega kilo byte)
    ;; terra
    (if (nth 4 lst)
	lst
      ;; giga
      (if (nth 3 lst) (append (list 0) lst)
	;; mega
	(if (nth 2 lst) (append (list 0 0) lst)
	  ;; mega
	  (if (nth 1 lst) (append (list 0 0 0) lst)
	    ;; byte
	    (if (nth 0 lst) (append (list 0 0 0 0) lst)
	      (list 0 0 0 0 0))))))))


(defun file-list-sort-internal (&optional file-list by reverse dont-display)
  (let ((gc-cons-threshold file-list-gc-cons-threshold)
	(file-list (or file-list file-list-current-file-list))
	(sortfun (cond
		  ((string= by "name")
		   (lambda (a b)
		     (string-lessp (car a) (car b))))
		  ((string= by "path")
		   (lambda (a b)
		     (string-lessp (cadr a) (cadr b))))
		  ((string= by "time")
		   (lambda (a b)
		     (let ((attr-a (file-attributes (file-list-make-file-name a)))
			   (attr-b (file-attributes (file-list-make-file-name b))))
		       (if (or (not attr-a) (not attr-b))
			   t
			 (not (file-list-time-less-p (nth 5 attr-a) (nth 5 attr-b)))))))
		  ((string= by "size")
		   (lambda (a b)
		     (let ((size-a (file-list-parse-size (cdr (assoc "size" (caddr a)))))
			   (size-b (file-list-parse-size (cdr (assoc "size" (caddr b)))))
			   stop
			   res)
		       (while (and (not stop) size-a)
			 (if (= (nth 0 size-a) (nth 0 size-b))
			     (setq size-a (cdr size-a) size-b (cdr size-b))
			   (setq stop t)
			   (setq res (> (nth 0 size-a) (nth 0 size-b)))))
		       res)))
		  ;; (> (string-to-int (cdr (assoc "size" (caddr a))))
		  ;; (string-to-int (cdr (assoc "size" (caddr b)))))))
		  (t nil)))
	sorted-list)
    (message "Sorting file list by %s" by)
    (when (string= by "size")
      (setq file-list (file-list-attributes file-list t)))
    (when sortfun
      ;; (setq file-list-current-file-list
      (setq sorted-list
	    (if reverse
		(reverse (sort file-list sortfun))
	      (sort file-list sortfun))))
    (if dont-display
	sorted-list
      (setq file-list-current-file-list sorted-list)
      (message "File list sorted by %s%s" by (if reverse " in reverse order" ""))
      (if (eq major-mode 'superman-file-list-mode)
	  (superman-display-file-list nil file-list-current-file-list)
      (file-list-display-match-list file-list-current-file-list)))))



(provide 'file-list-list)
;;; file-list-list.el ends here
