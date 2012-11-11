;;{{{ global custom
(setq-default ess-language "R")
(setq inferior-ess-font-lock-keywords nil)
(unless (featurep 'xemacs)
  (setq ess-eval-deactivate-mark t))
(setq ess-eval-visibly-p t)

;;}}}
;;{{{ inferior mode
(defun ess-show-buffer (buf &optional visit)
  (pop-to-buffer buf t (selected-frame)))
(setq-default ess-ask-for-ess-directory nil)
(setq-default ess-directory (concat (getenv "HOME") "/R/"))
(setq-default ess-history-directory (concat (getenv "HOME") "/R/"))
(setq ess-display-buffer-reuse-frames nil)
(add-hook 'inferior-ess-mode-hook
	  (lambda ()
	    (local-set-key "\M-\t" 'ess-edit-indent-call-sophisticatedly)
	    (local-set-key "\M-F" 'end-of-buffer)
	    (local-set-key "\C-cF" 'ess-edit-insert-file-name)
	    (local-set-key "\C-cf" 'ess-edit-insert-call)
	    (local-set-key "\C-cv" 'ess-edit-insert-vector)
	    (local-set-key "\C-cp" 'ess-edit-insert-path)
	    (local-set-key "\M-r" 'copy-region-as-kill)
	    (local-set-key "\M-p" 'comint-previous-matching-input-from-input)
	    (local-set-key "\M-n" 'comint-next-matching-input-from-input)
	    (setq comint-scroll-to-bottom-on-input 'all)
	    (setq comint-input-ring-size 5000)))
;;}}}
;;{{{ R mode
(add-hook 'ess-mode-hook 'my-R-mode)
(defun tag-ess-eval-and-go ()
  (interactive)
  (if (region-active-p)
      (let* ((start (region-beginning))
	     (end (region-end))
	     (visibly (< (length (buffer-substring-no-properties start end)) 300)))
	(ess-eval-region-and-go start end visibly))
    (ess-eval-line-and-step)))
(defun r (arg)
  (interactive "P")
  (cond ((not (one-window-p)) nil)
	(arg (split-window-horizontally))
	(t (split-window-vertically)))
  (other-window 1)
  (R))

(defun my-R-mode ()
  (interactive)
  (setq split-width-threshold nil)
  (define-key ess-mode-map "\M-F" 'ess-eval-function-and-go)
  (define-key ess-mode-map "\M-j" 'ess-eval-region-and-go)
  (define-key ess-mode-map "\M-r" 'copy-region-as-kill)
  (define-key ess-mode-map "\M-k" 'R-inferior-clear)
  (define-key ess-mode-map "\M-q" 'my-indent-paragraph)
  (make-variable-buffer-local 'hippie-expand-try-functions-list)
  (setq hippie-expand-try-functions-list
	(append (list 'ess-complete-object-name)
		hippie-expand-try-functions-list))
;  (define-key ess-mode-map "\M-e" 'ess-complete-object-name)
  (define-key ess-mode-map "\M-m" 'ess-edit-motif)
  (define-key ess-mode-map "\M-u" 'ess-edit-dev-off)
  (define-key ess-mode-map "\C-z" 'fold-dwim-toggle)
  (define-key ess-mode-map "\C-cf" 'ess-edit-insert-call)
  (define-key ess-mode-map "\C-cv" 'ess-edit-insert-vector)
  (define-key ess-mode-map "\C-cp" 'ess-edit-insert-path)
  (define-key ess-mode-map "\C-ch" 'ess-edit-mark-call)
  (define-key ess-mode-map "\C-cF" 'ess-edit-insert-file-name)
  (define-key ess-mode-map "\M-\t" 'ess-edit-indent-call-sophisticatedly)
  (define-key ess-mode-map [(meta return)] '(lambda () (interactive) (ess-edit-next-arg nil)))
  (define-key ess-mode-map "\M-A" '(lambda () (interactive) (s-goto-next-arg t)))
  (define-key ess-mode-map "\M-\C-c" 's-config)
  (define-key ess-mode-map "\M-l" 'mark-line)
  (if  (featurep 'xemacs) 
      (define-key ess-mode-map [(delete)] 'backward-or-forward-delete-char))
  (define-key ess-mode-map [(backspace)] 'delete-backward-char)
  (define-key ess-mode-map [(meta backspace)] 'backward-kill-word)
  (setq ess-fancy-comments nil))
;;}}}
;;{{{ Rd mode
(add-hook 'Rd-mode-hook
	  '(lambda ()
	     (define-key Rd-mode-map "_" 'my-ess-smart-underscore)
	     (define-key Rd-mode-map "\M-\t" 'ess-edit-indent-call-sophisticatedly)
	     (define-key Rd-mode-map "\C-cF" 'ess-edit-insert-file-name)
	     (define-key Rd-mode-map "\C-cf" 'ess-edit-insert-call)
	     (define-key Rd-mode-map "\C-cv" 'ess-edit-insert-vector)
	     (define-key Rd-mode-map "\C-cp" 'ess-edit-insert-path)
	     (define-key Rd-mode-map "\M-k" 'R-inferior-clear)
	     (define-key Rd-mode-map "\M-j" 'tag-ess-eval-and-go)))

;;}}}
;;{{{ command history
(setq comint-input-ring-size 5000)
;;(add-hook 'ess-send-input-hook
;;	  '(lambda nil
;;	     (if (>= (point) (marker-position
;;			      (process-mark
;;			       (get-buffer-process (current-buffer)))))
;;		 (comint-send-input)
;;	       (comint-copy-old-input))
;;	     (setq ess-object-list nil)
;;	     (let ((debug-on-error nil))
;;	       (error ""))))

;;The function ess-eval-line-and-step (^c^n) calls
;;ess-eval-linewise with the line to evaluate.

;You can add a defadvice for ess-eval-linewise that explicitly calls
;comint-add-to-input-history with the command.

;Something like (untested) should do what you want.


;; (defadvice ess-eval-linewise (before smart-toggle-visibly first activate)
  ;; (and (not (eq major-mode 'inferior-ess-mode)) (< (length text-withtabs) 300))
      ;; (setq invisibly nil)
 ;; (setq invisibly t))

;; (defadvice ess-eval-region (before smart-toggle-visibly first activate)
  ;; (if (and (not (eq major-mode 'inferior-ess-mode)) (< (length (buffer-substring-no-properties start end)) 300))
      ;; (setq toggle t)
    ;; (setq toggle nil)))


(defadvice ess-eval-linewise (after add-history first activate)
  (if (and (not (eq major-mode 'inferior-ess-mode)) (< (length text-withtabs) 300))
      (save-excursion
	(set-buffer (process-buffer (get-ess-process ess-current-process-name)))
	(comint-add-to-input-history
	 (let* ((str text-withtabs)
		(pos (string-match "\n" str)))
	   (while pos
	     (setq str (concat (substring str 0 pos)
			       (substring str (+ 1 pos))))
	     (setq pos (string-match "\n" str)))
	   str)))))

;;Your other idea of not adding long commands to the history can be
;;handled with comint-input-filter.  For example, the default filter
;;rejects short commands.
(setq comint-input-filter
      #'(lambda (str)
	  (and (not (string-match "\\`\\s *\\'" str))
	       ;; Ignore '!!' and kin
	       (> (length str) 2)
	       (< (length str) 300))))

(defun comint-add-to-input-history (cmd)
  "Maybe add CMD to the input history.  
CMD is only added to the input history if `comint-input-filter'
returns non-nil when called on CMD. If `comint-input-ignoredups' is
non-nil then duplicates are ignored."
  (if (and (funcall comint-input-filter cmd)
	   (or (null comint-input-ignoredups)
	       (not (ring-p comint-input-ring))
	       (ring-empty-p comint-input-ring)
	       (not (string-equal (ring-ref comint-input-ring 0)
				  cmd))))
      (ring-insert comint-input-ring cmd)))

;;}}}
;;{{{ run script on gauss or borel
(defadvice shell-command (after shell-in-new-buffer (command &optional output-buffer error-buffer))
    (when (get-buffer "*Async Shell Command*")
      (with-current-buffer "*Async Shell Command*"
         (rename-uniquely))))
 (ad-activate 'shell-command)
(try-require 'autorevert)

(defun ess-run-script-elsewhere (&optional ask)
  (interactive "P")
  (let* ((code (buffer-file-name (current-buffer)))
	 (log (concat code "out"))
	 (server (if ask (read-string "Name of the server (defaults to gauss): " nil nil "gauss") "gauss"))
	 (R  (if ask (read-string "Name of R (defaults to /usr/local/bin/R): " nil nil "/usr/local/bin/R") "/usr/local/bin/R"))
	 ;; (car (split-string (shell-command-to-string  (concat "ssh " server " 'which R'")) "\n")))
	 (cmd (concat "ssh -X " server " 'nohup nice -19 " R " --no-restore --no-save CMD BATCH " code " " log "'")))
    ;; (when (yes-or-no-p (concat "Run this command?: " cmd))
    (save-buffer)
    (save-excursion
      (when (get-buffer "*Async Shell Command*")
	(with-current-buffer "*Async Shell Command*"
	  (rename-uniquely)))
      (async-shell-command cmd))
    (find-file-other-window log)
    (unless (auto-revert-active-p)
      (auto-revert-mode))))

(defun tag-org-run-script (&optional server R)
  (interactive)
  (let* ((buf (buffer-file-name (current-buffer)))
	 (code (concat (file-name-sans-extension buf) ".R"))
	 (log (concat code "out"))
	 (R  (or R "/usr/local/bin/R"))
	 (cmd (concat "ssh " server " 'nohup nice -19 " R " --no-restore CMD BATCH " code " " log "'")))
    (save-buffer)
    (org-babel-tangle nil code "R")
    (save-excursion
      (when (get-buffer "*Async Shell Command*")
	(with-current-buffer "*Async Shell Command*"
	  (rename-uniquely)))
      (async-shell-command cmd))
    (concat "[[" log "]]")))

;;}}}
;;{{{ R minor mode
;; Look for an Emacs Lisp library that supports "multiple
;; major modes" like mumamo, mmm-mode or multi-mode.
(defvar R-minor-mode nil)
(make-variable-buffer-local 'R-minor-mode)

(defvar R-minor-mode-map (make-sparse-keymap)
  "Keymap used for `R-minor-mode' commands.")

(or (assq 'R-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (append minor-mode-map-alist
		  (list (cons 'R-minor-mode R-minor-mode-map)))))

(or (assq 'R-minor-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(R-minor-mode " R") minor-mode-alist)))

(defun R-minor-mode (&optional arg)
  "A minor mode for using ess commands."
  (interactive "P")
  (make-variable-buffer-local 'hippie-expand-try-functions-list)
  (setq hippie-expand-try-functions-list
	(append (list 'ess-complete-object-name)
		hippie-expand-try-functions-list))
  (setq ess-fancy-comments nil)
  (setq R-minor-mode
	(not (or (and (null arg) R-minor-mode)
		 (<= (prefix-numeric-value arg) 0)))))

(define-key R-minor-mode-map "_" 'my-ess-smart-underscore)
(define-key R-minor-mode-map "\M-F" 'ess-eval-function-and-go)
(define-key R-minor-mode-map "\M-j" 'tag-ess-eval-and-go)
(define-key R-minor-mode-map "\M-r" 'copy-region-as-kill)
(define-key R-minor-mode-map "\M-k" 'R-inferior-clear)
(define-key R-minor-mode-map "\M-q" 'my-indent-paragraph)
(define-key R-minor-mode-map "\M-m" 'ess-edit-motif)
(define-key R-minor-mode-map "\M-u" 'ess-edit-dev-off)
(define-key R-minor-mode-map "\C-z" 'fold-dwim-toggle)
(define-key R-minor-mode-map "\C-cf" 'ess-edit-insert-call)
(define-key R-minor-mode-map "\C-cv" 'ess-edit-insert-vector)
(define-key R-minor-mode-map "\C-cp" 'ess-edit-insert-path)
(define-key R-minor-mode-map "\C-ch" 'ess-edit-mark-call)
(define-key R-minor-mode-map "\C-cF" 'ess-edit-insert-file-name)
(define-key R-minor-mode-map "\M-\t" 'ess-edit-indent-call-sophisticatedly)
(define-key R-minor-mode-map [(meta return)] '(lambda () (interactive) (ess-edit-next-arg nil)))
(define-key R-minor-mode-map "\M-A" '(lambda () (interactive) (s-goto-next-arg t)))
(define-key R-minor-mode-map "\M-\C-c" 's-config)
(define-key R-minor-mode-map "\M-l" 'mark-line)
(if  (featurep 'xemacs) 
    (define-key R-minor-mode-map [(delete)] 'backward-or-forward-delete-char))
(define-key R-minor-mode-map [(backspace)] 'delete-backward-char)
(define-key R-minor-mode-map [(meta backspace)] 'backward-kill-word)

;;}}}
;;{{{ smart underscore
(defun my-ess-smart-underscore ()
  (interactive)
  (if (not (eq last-command 'my-ess-smart-underscore))
      (insert " <- ")
    (undo)
    (insert "_")))
;;}}}
;;{{{ clearing the inferior window
(defun R-inferior-clear()
  (interactive)
  (save-excursion
    (let ((pbuf (or
		 (condition-case nil
		     (process-buffer (get-ess-process ess-local-process-name))
		   (error nil))
		 (concat "*" ess-current-process-name "*"))))
      ;; (pop-to-buffer pbuf)
      (set-buffer pbuf)
      (erase-buffer)
      (comint-send-input)  
      (ess-switch-to-end-of-ESS))))
;;}}}
;;{{{ highlighted sweave
(add-to-list 'auto-mode-alist '("\\.Rnw\\'" . Rnw-mode))
(add-to-list 'auto-mode-alist '("\\.Snw\\'" . Rnw-mode))
;;}}}
;;{{{ tracebug not loaded!
;; (when (and (not emacs-novice) (try-require 'ess-tracebug))
    ;; (add-hook 'ess-post-run-hook 'ess-tracebug  t))
;;}}}
(provide 'ess-R-snps)