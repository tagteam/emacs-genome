;;{{{load bbdb and define bbdb-field
(require 'bbdb)
(require 'bbdb-com)
;; (defun bbdb-field (field string elidep)
  ;; "Display all entries in the BBDB matching the regexp STRING in the field field."
  ;; (interactive "xField where to search : \nsRegular Expression for Search: \nP")
  ;; (let* ((target (cons field string))
	 ;; (bbdb-elided-display (bbdb-grovel-elide-arg elidep))
	 ;; (recs (bbdb-search (bbdb-records) nil nil nil target)))
    ;; (bbdb-display-records recs)
    ;; recs))
;;}}}
;;{{{ gnus/message settings

(require 'bbdb-mua)
(require 'bbdb-message)
(bbdb-initialize 'gnus 'message)
(bbdb-mua-auto-update-init 'message) ;; use 'gnus for incoming messages too
(setq bbdb-mua-auto-update-p 'query) ;; or 'create to create without asking

(setq bbdb-ignore-some-messages-alist '(("Newsgroups" . ".")))
(add-hook 'bbdb-mode-hook
	  (lambda ()
	    (define-key bbdb-mode-map "M" 'eg/gnus-summary-limit-to-author)))
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(add-hook 'message-setup-hook 'bbdb-mail-aliases)
(add-hook 'mail-setup-hook 'bbdb-mail-aliases)
(add-hook 'bbdb-create-hook 'bbdb/gnus-add-gnus-private)




(defun bbdb/gnus-add-gnus-private (record)
  "For use as a `bbdb-create-hook'; prompts for a gnus-group"
  (bbdb-record-set-field record 'gnus-private
		       (read-string "Insert value for gnus-private field: ")))

;; set Gcc to gnus-privat when composing mail from bbdb
(setq bbdb-send-mail 'gnus)
;; (defadvice bbdb-send-mail (before dmg-change-gcc activate)
 ;; (if (string-equal (buffer-name) "*BBDB*")
     ;; (let ((gcc (save-excursion
		  ;; (set-buffer bbdb-buffer-name)
		  ;; (bbdb-record-getprop (bbdb-current-record) 'gnus-private))))
       ;; (print gcc)
       ;; (if gcc
	   ;; ;; (progn
	   ;; ;; (setq hold gnus-message-archive-group)
	   ;; (setq gnus-message-archive-group (concat "nnml:" gcc))))))


;;}}}
;;{{{ pop-up bbdb buffer in gnus 
(setq bbdb-use-pop-up 'vertical)

;;}}}
;;{{{other settings
(setq bbdb-no-duplicates-p nil
      bbdb-complete-name-allow-cycling t
      bbdb-electric-p nil
      bbdb-case-fold-search t
      bbdb-default-country ""
      bbdb-north-american-phone-numbers-p nil
      bbdb-silent-running t
      bbdb-dwim-net-address-allow-redundancy 'netonly
      bbdb-use-alternate-names t)


;; keine US-Schemata für Adressen
(setq bbdb-address-editing-function 'bbdb-address-edit-continental)
(setq bbdb-continental-zip-regexp
      "^\\s *\\([A-Z][A-Z]?\\s *-\\s *\\)?[0-9][0-9][0-9]")
;;}}}
;;{{{old stuff not loaded
;; (defun bbdb-show-all-gnus-private-fields ()
  ;; (interactive)
  ;; (let (g-p-list
	;; (g-p-buf (get-buffer-create "*bbdb-gnus-private-fields*")))
    ;; (find-file bbdb-file)
    ;; (goto-char (point-min))
    ;; (while (re-search-forward "gnus-private \\. \\\"\\([^\\\)]*\\)\\\"" nil t)
      ;; (setq g-p-list (add-to-list 'g-p-list (match-string 1))))
    ;; (pop-to-buffer g-p-buf)
    ;; (erase-buffer)
    ;; (goto-char (point-min)) 
    ;; (while g-p-list
      ;; (insert (car g-p-list) "\n")
      ;; (setq g-p-list (cdr g-p-list)))))


;; (defun eg/bbdb-create-bbdb-accounts ()
  ;; (interactive)
  ;; (set-buffer  (get-buffer "Students.txt"))
  ;; (let* ((mails (buffer-substring (point-min) (point-max) (get-buffer "Students.txt")))
	 ;; (mail-list (split-string mails "[ \n\t]*,[ \n\t]*"))
	 ;; (i 0)
	 ;; mail)
    ;; (while mail-list
      ;; (setq i (+ 1 i))
      ;; (setq mail (car mail-list))
      ;; (bbdb-create-internal (concat "Student " (int-to-string i))
			    ;; nil
			    ;; mail
			    ;; nil
			    ;; nil
			    ;; "Course.2008-3-12")
      ;; (setq mail-list (cdr mail-list)))))


;(defun bbdb-record-sortkey-mine (record)
;  (or (bbdb-cache-sortkey (bbdb-record-cache record))
;      (bbdb-cache-set-sortkey (bbdb-record-cache record)
;			      (downcase
;			       (concat 
;				(bbdb-record-getprop record 'sortkey)
;;				(bbdb-record-getprop record 'titel)
;				(bbdb-record-lastname record)
;				(bbdb-record-firstname record)
;				(bbdb-record-company record)
;				)))))

;(defalias 'bbdb-record-sortkey 'bbdb-record-sortkey-mine)
;(bbdb-resort-database)


;; (defun bbdb-switch-to-other-bbdb-file (&optional db dont-ask)
;; (interactive)
;; (let ((use-db db))
;; (unless use-db
;; (setq use-db (if dont-ask (expand-file-name "~/.bbdb")
;; (read-file-name "Use bbdb database "))))
;; (setq bbdb-file use-db
;; bbdb-buffer (get-file-buffer use-db))))
;; (setq bbdb-print-alist '((omit-area-code . "^(000) ")
			 ;; (phone-on-first-line . "^[ 	]*$")
			 ;; (ps-fonts)
			 ;; (font-size . 10)
			 ;; (separator . 0)))
;; (setq bbdb-print-brief-alist
      ;; '((columns . 1)
	;; (separator . 1)
	;; (n-phones . 2)
	;; (n-addresses . 1)
	;; (include-files "~/tex/bbdb-print-brief.sty" "~/tex/bbdb-cols.sty")))

;; (setq bbdb-print-elide '(tex-name aka mail-alias creation-date timestamp net))
;; (setq bbdb-print-full-alist '((columns . 3)
			      ;; (separator . 2)
			      ;; (include-files "~/tex/bbdb-print.sty" "~/tex/bbdb-cols.sty")))
;; (setq bbdb-print-require 'name)
;;}}}
(provide 'bbdb-tags)
