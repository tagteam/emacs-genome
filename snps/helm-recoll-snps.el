;;; helm-recoll-snps.el --- combine helm with recoll

;; Copyright (C) 2012  Thomas Alexander Gerds

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


;;; Code:

;;; Recoll (xapian) text indexing engine plugin, derived from helm-c-source-locate

;; (defvar helm-c-recoll-options
  ;; '("recoll" "-t" "-b")
  ;; "A list where the `car' is the name of the recoll program followed by options.")

(defun get-header (file hdr len)
  (let* ((str (replace-regexp-in-string "\n" "" (shell-command-to-string (concat "grep ^" hdr ": " file))))
	 (start (+ (length hdr) 2))
         (end (min (+ len start) (length str))))
         (if (< start end)
	     (substring-no-properties str start end)
	   str)))

(setq helm-c-source-recoll-mail
      '((name . "Recoll")
	(candidates-process . (lambda ()
				(apply 'start-process "recoll-process" nil
				       (append helm-c-recoll-options
					       (list "type:message")
					       (list helm-pattern)))))
	(candidate-transformer
	 . (lambda (cs)
	     (mapcar (function (lambda (c)
				 (setq c (replace-regexp-in-string "file://" "" c))
				 (if (string-match ".*/[0-9]+$" c)
				     (let ((from (get-header c "From" 20))
					   (subject (get-header c "Subject" 30))
					   (date (get-header c "Date" 16))
					   (id  (get-header c "Message-ID" 100)))
				       (format "%s\t%s\t%s :ID: %s" subject from date id))
				   c)))
		     cs)))
	(type . file)
        (action . gnus-goto-article-from-helm-pattern)
	(requires-pattern . 3)
	(delayed)))

(defun gnus-goto-article-from-helm-pattern (pattern)
  "Open a summary buffer containing the current article."
  (interactive)
  (unless (gnus-alive-p) (with-temp-buffer (gnus)))
  (let* ((message-id (cadr (split-string pattern " :ID: ")))
	 (info (nnml-find-group-number message-id  "nnml:"))
	 (group (concat "nnml:" (car info)))
	 (id (cdr info)))
    (gnus-summary-read-group group 1)
    (gnus-summary-refer-article message-id)))

(defun search-mail ()
  (interactive)
  (helm 'helm-c-source-recoll-mail))

(defun helm-mail ()
  (interactive)
  (switch-to-buffer "*Helm-mail*")
  (insert "Search: ")
  ;; (local-set-key "RET")
  ;; (helm-mail-mode)
  (helm 'helm-c-source-recoll-mail ""))

;; (defun helm-mail-mode ()
;; )

(global-set-key "\C-x\C-m" 'search-mail)

(setq helm-c-source-recoll
      '((name . "Recoll")
	(candidates-process . (lambda ()
				(apply 'start-process "recoll-process" nil
				       (append helm-c-recoll-options
					       (list helm-pattern)))))
	(candidate-transformer
	 . (lambda (cs)
	     (mapcar (function (lambda (c)
				 (setq c (replace-regexp-in-string "file://" "" c))
				 (if (string-match ".*/[0-9]+$" c)
				     (concat "email" c)
				   c)))
		     cs)))
	(type . file)
	(action . find-file)
	(requires-pattern . 3)
	(delayed)))
;; "Source for retrieving files matching the current input pattern with recoll.")
;; (helm 'helm-c-source-recoll)


(defvar helm-c-recoll-options
  '("recoll" "-t" "-b") 
  "A list where the `car' is the name of the recoll program followed by options.
You do not need to include the -c option since this is already included, and the config directory
can be passed as a argument to `helm-c-source-recoll'")

(defun helm-recoll-search ()
  (interactive)
  (helm 'helm-c-source-recoll))

(defun helm-c-source-recoll (name confdir)
  "Function to create helm source for recoll search results.
The source variable will be named `helm-c-source-recoll-NAME' where NAME is the first arg to the function
 (and should be a valid symbol name - i.e. no spaces).
The CONFDIR arg should be a string indicating the path to the config directory which recoll should use."
  (eval
   `(defvar ,(intern (concat "helm-c-source-recoll-" name))
      '((name . ,(concat "Recoll " name))
        (candidates-process . (lambda ()
			       (apply 'start-process "recoll-process" nil 
				      (append helm-c-recoll-options
					      '("-c" ,confdir)
					      (list helm-pattern)))))
        (candidate-transformer
         . (lambda (cs)
             (mapcar (function (lambda (c)
                                 ;; (replace-regexp-in-string "file://" "" c)))
				 (replace-regexp-in-string "a" "" c)))
                     cs)))
        (type . file)
	(action . find-file)
        (requires-pattern . 3)
        (delayed))
      ,(concat "Source for retrieving files matching the current input pattern, using recoll with the configuration in "
               confdir))))

 
;(helm-c-source-recoll "reports" "~/.recoll_reports")
(provide 'helm-recoll-snps)
;;; helm-recoll-snps.el ends here
