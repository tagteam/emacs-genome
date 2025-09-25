;; -*- lexical-binding: t; -*-
;; first time usage needs: notmuch setup
(setq notmuch-show-elide-non-matching-messages t)
(setq notmuch-show-only-matching-messages t)

;;{{{ search-help
(require 'notmuch)
(require 'notmuch-lib)
(setq notmuch-hello-sections
      (list #'notmuch-hello-insert-help
	    #'notmuch-hello-insert-saved-searches
	    #'notmuch-hello-insert-search
	    #'notmuch-hello-insert-recent-searches
	    #'notmuch-hello-insert-alltags
	    #'notmuch-hello-insert-footer))

(defun notmuch-hello-insert-help ()
  (widget-insert "from:word \t match messages with word in the from address field.\n")
  (widget-insert "to:word match \t messages with word in either the To: or Cc: headers.\n")
  (widget-insert "attachment:word\t match messages with word in an attachment filename.\n")
  (widget-insert "subject:word\t match messages with word in the subject field.\n"))

;;}}}
;;{{{ Gnus

;; adapted from http://www.emacswiki.org/emacs/NotMuch

(setq notmuch-fcc-dirs nil)
(defun notmuch-file-to-group (file)
  "Calculate the Gnus group name from the given file name."
  (let ((group (directory-file-name (file-name-directory file))))
    (concat "nnml:"
	    (replace-regexp-in-string "/" "."
				      (replace-regexp-in-string "^/" "" (replace-regexp-in-string (expand-file-name "~/mail") "" group))))))


(defun notmuch-goto-message-in-gnus ()
  "Open a summary buffer containing the current notmuch
     article."
  (interactive)
  (unless (gnus-alive-p) (with-temp-buffer (gnus)))
  (let ((group (notmuch-file-to-group (notmuch-show-get-filename)))
     	(message-id
     	 (replace-regexp-in-string "\"" ""
				   (replace-regexp-in-string "^id:" ""
							     (notmuch-show-get-message-id)))))
    (if (and group message-id)
	(org-gnus-follow-link group message-id)
      (message "Couldn't get relevant infos for switching to Gnus."))))
;; (progn
;; (gnus-summary-read-group group 1) ; have to show at least one old message
;; (gnus-summary-refer-article message-id)) ; simpler than org-gnus method?
;; (message "Couldn't get relevant infos for switching to Gnus."))))

(define-key notmuch-show-mode-map (kbd "C-c C-c") 'notmuch-goto-message-in-gnus)
(define-key notmuch-search-mode-map " " 'notmuch-goto-message-in-gnus)
;;}}}


(provide 'notmuch-tags)

