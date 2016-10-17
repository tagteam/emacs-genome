;; ess-sas-snps.el
;;
;; Copyright (C) 2002-2014, Thomas A. Gerds <tag@biostat.ku.dk>
;; Version: 1.1.1 (21 April 2011)
;; Version: 1.1.2 (09 July 2014)
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;;
;;; Description
;;;
;;; Customized start-up behaviour for emacs interactive sas (ess)
;;; featuring SAS-mode-hooks and font-lock highlighting of the log-buffer
;;; put this file in your emacs load-path and load it from .emacs (init.el)
;;; 
;;;  (require 'ess-sas-snps)
;;;
;;;  Then, open a file with some sas code and start an interactive sas session using
;;;
;;;  M-x sas RET
;;; 
;;; Code
;;; --------------------------------------------------------------------

(require 'superman-manager)

(defvar sas-mode-map nil		
  "Keymap for SAS-mode")
  
(defvar SAS-mode-hook nil "Hook  for customizing SAS mode.
  Called after `SAS-mode' is entered and variables have been initialised.")

(defvar ess-sas-startup-config "multi-windows" "Controls the configuration
of the inferior SAS-buffers after calling M-x SAS.
If multi-frames put *SAS.log* and *SAS.lst* in seperate frames.")

(defvar SAS-log-mode-hook nil "Hook for customizing SAS log mode.
  Called after `SAS-log-mode' is entered and variables have been initialised.")

(defvar SAS-lst-mode-hook nil "Hook  for customizing SAS lst mode.
  Called after `SAS-lst-mode' is entered and variables have been initialised.")

(defvar font-sas-log-warning-face 'font-sas-log-warning-face
  "Face to use for WARNINGS in SAS log files.
This variable should not be set.
The corresponding face should be set using `customize-faces'.")

(defvar font-sas-log-error-face 'font-sas-log-error-face
  "Face to use for ERRORS in SAS log files.
This variable should not be set.
The corresponding face should be set using `customize-faces'.")

(defvar font-sas-log-note-face 'font-sas-log-note-face
  "Face to use for NOTES in SAS log files.
This variable should not be set.
The corresponding face should be set using `customize-faces'.")


(make-face 'font-sas-log-warning-face)
(set-face-foreground 'font-sas-log-warning-face  "#00b000")    ;; green

(make-face 'font-sas-log-error-face)
(set-face-foreground 'font-sas-log-error-face  "#ff0000")    ;; pure red

(make-face 'font-sas-log-note-face)
(set-face-foreground 'font-sas-log-note-face  "#0000ff") ;; bright blue

(defvar SAS-log-font-lock-keywords
  '(("^ERROR.*\\(\n[ \t]+\\w.*\\)*" . font-sas-log-error-face)
    ("[ \t]+_+[ \t]*\n[ \t]+[0-9]+" . font-sas-log-error-face) ;; the error location
    ("^WARNING.*\\(\n[ \t]+\\w.*\\)*" . font-sas-log-warning-face)
    ("^NOTE.*\\(\n[ \t]+\\w.*\\)*" . font-sas-log-note-face))
  "Font Lock regexs for SAS-log.")


; (setq auto-mode-alist
; 	(append
; 	 '(("\\.lst\\'" . eg/SAS-lst-mode);sasl
; 	   ;; Too many *.log files, not only SAS :
; 	   ("\\.log\\'" . eg/SAS-log-mode));sasl
; 	 auto-mode-alist))


(defun SAS-mode (&optional proc-name)
  "Major mode for editing SAS source.  See ess-mode for more help."
  (interactive)
  (setq ess-customize-alist SAS-customize-alist)
  (ess-mode SAS-customize-alist proc-name)
  (if sas-mode-local-map nil
    (setq sas-mode-local-map (copy-keymap (current-local-map)))
    (ess-sas-edit-keys-toggle ess-sas-edit-keys-toggle)
    (if ess-sas-local-unix-keys (ess-sas-local-unix-keys))
    (if ess-sas-local-pc-keys (ess-sas-local-pc-keys))
    (if ess-sas-global-unix-keys (ess-sas-global-unix-keys))
    (if ess-sas-global-pc-keys (ess-sas-global-pc-keys)))
  (use-local-map sas-mode-local-map)
  (run-hooks 'SAS-mode-hook))

(defun eg/SAS-lst-mode ()
  "ess-lst-mode for SAS."
  (interactive)
;  (use-local-map sas-log-lst-mode-map)
  (run-hooks 'SAS-lst-mode-hook))
;  (if sas-toolbar
;      (sas-toolbar-install-toolbar)))

(defun eg/SAS-log-mode ()
  "ess-log-mode for SAS."
  (interactive)
;  (use-local-map sas-log-lst-mode-map)
  (run-hooks 'SAS-log-mode-hook))
;  (if sas-toolbar
;      (sas-toolbar-install-toolbar)))



(defun sas ()
  "Call 'SAS', from SAS Institute."
  (interactive)
  (run-hooks 'ess-SAS-pre-run-hook)
  (setq ess-sas-start-buffer (current-buffer))
  (defalias 'ess-switch-to-ESS (symbol-function 'ess-sas-switch-to-ESS))
  (setq-default ess-customize-alist SAS-customize-alist)
  (let* ((temp-dialect "SAS")) ;(cdr (rassoc ess-dialect SAS-customize-alist))))
    (ess-write-to-dribble-buffer
     (format "(SAS): ess-dial=%s, temp-dial=%s\n"
	     ess-dialect
	     temp-dialect))
    (ess-SAS-pre-run-hook temp-dialect)
    (inferior-ess)
    (let* ((sas-proc ess-current-process-name)
	   (ess-sas-log-buffer
	    (get-buffer (concat "*" sas-proc ".log*")))
	   (ess-sas-lst-buffer
	    (get-buffer (concat "*" sas-proc ".lst*"))))
      ;; (switch-to-buffer ess-sas-start-buffer)
      (set-buffer ess-sas-lst-buffer)
      (eg/SAS-lst-mode)
      (set-buffer ess-sas-log-buffer)
      (eg/SAS-log-mode)
      (cond ((string= ess-sas-startup-config "multi-frame")
	     (delete-other-windows)
	     (set-buffer ess-sas-lst-buffer)
	     (make-frame)
	     (set-buffer ess-sas-log-buffer)
	     (make-frame))
	    ((string= ess-sas-startup-config "multi-windows")
	     (delete-other-windows)
	     (split-window-horizontally)
	     (switch-to-buffer-other-window ess-sas-log-buffer) 
	     (split-window-vertically)
	     (switch-to-buffer-other-window ess-sas-lst-buffer))
	    (t (delete-other-windows))))))

(defun ess-sas-switch-to-ESS (eob-p)
  "Switch to the current ESS[SAS] log buffer.
With (prefix) EOB-P non-nil, positions cursor at end of buffer."
  (interactive "P")
  (ess-make-buffer-current)
  (if (and ess-current-process-name
	   (get-process ess-current-process-name))
      (let ((bufname
	     (if (string-match "SAS" ess-current-process-name)
		 (concat "*" ess-current-process-name ".log*")
	       (process-buffer
		(get-process ess-current-process-name)))))
	(pop-to-buffer (get-buffer bufname))
	(raise-frame (window-frame
		      (get-buffer-window bufname t)))
	(if eob-p (goto-char (point-max))))
    (ding)))

(defun ess-sas-eval-region-and-go (start end vis)
  "Send the current region to SAS and switch to the log buffer.
Arg has same meaning as for `ess-eval-region'."
  (interactive "r\nP")
  (ess-eval-region start end vis)
  (ess-sas-switch-to-ESS t))

(defun erase-sas-log-and-lst-buffer ()
  "Erase SAS-output buffers"
  (interactive)
  (save-excursion
    (let ((procname (cond (ess-local-process-name)
			  (ess-current-process-name)
			  (t nil))))
      (if procname (progn
		     (set-buffer (get-buffer (concat "*" procname  ".log*")))
		     (erase-buffer)
		     (set-buffer (get-buffer (concat "*" procname ".lst*")))
		     (erase-buffer))
	(message "Current ess-process is not SAS")))))

(defun ess-sas-windows ()
  (interactive)
  (save-excursion
    (let ((ess-sas-log-buffer
	   (get-buffer (concat "*" ess-current-process-name ".log*")))
	  (ess-sas-lst-buffer
	 (get-buffer (concat "*" ess-current-process-name ".lst*"))))
      (delete-other-windows)
      (split-window-vertically)
      (split-window-horizontally)    
    (switch-to-buffer-other-window ess-sas-lst-buffer) 
    (switch-to-buffer-other-window ess-sas-log-buffer))
    ))


(defun ess-sas-get-log ()
  (interactive)				
    (let ((procname (cond (ess-local-process-name)
			  (ess-current-process-name)
			  (t nil))))
      (if (and procname (string-match "SAS" procname))
    (switch-to-buffer (concat "*" ess-current-process-name ".log*"))
  (ess-sas-goto-log))))

(defun ess-sas-get-lst ()
  (interactive)				
    (let ((procname (cond (ess-local-process-name)
			  (ess-current-process-name)
			  (t nil))))
      (if (and procname (string-match "SAS" procname))
    (switch-to-buffer (concat "*" ess-current-process-name ".lst*"))
  (ess-sas-goto-lst))))

(defun sas-show-errors ()
  (interactive)
(let ((sas-error "\\(^ERROR\\|^WARNING\\)"))
  (if (re-search-forward sas-error nil t)
      nil
      (progn (goto-char 1)	     
	     (cond ((re-search-forward sas-error nil t))
		   (t (message "Keine Fehler-Meldungen")))))))


(defun eg/ess-sas-submit ()
  "Save the .sas file and submit to shell using a function that
depends on the value of  `ess-sas-submit-method'"
  (interactive)
  (ess-sas-goto-sas)
  (save-buffer)
  (ess-sas-file-path)
;  (message "running SAS process (batch-input) -- please wait")
  (eg/ess-sas-submit-sh)
  (ess-sas-goto-sas))

(defun eg/ess-sas-submit-sh ()
  "Unix or bash in the *shell* buffer.
Multiple processing is supported on this platform.
SAS may not be found in your PATH.  You can alter your PATH to include
SAS or you can specify the PATHNAME (PATHNAME can NOT contain spaces),
i.e. let `ess-sas-submit-command' be your local equivalent of
\"/usr/local/sas612/sas\"."
  (let ((command (concat ess-sas-submit-command " "
			 (expand-file-name (buffer-name (current-buffer))))))
    (shell-command command)
    (message (concat "running  " command 
		     " under " (getenv "SHELL") "-- please wait"))
    ))


;; hooks
;; --------------------------------------------------------------------

(add-hook 'ess-SAS-pre-run-hook
	  '(lambda ()
	     (setq SAS-directory default-directory)
	     (setq ess-ask-for-ess-directory nil)
	     (setq ess-ask-about-transfile nil)
	     (setq ess-source-directory (file-name-as-directory
					 (concat (getenv "HOME") "/sas/")))
	     (setq ess-directory (file-name-as-directory
				  (concat (getenv "HOME"))))))

;; (setq SAS-mode-hook nil)
(add-hook 'SAS-mode-hook
	  '(lambda ()
	     (setq comment-start "\/\*") 
	       (setq comment-end "\*\/")
	       ;; Local map settings, AFTER initialization.
	       ;; (use-local-map sas-mode-map)
	       (local-set-key "\t" 'sas-indent-line)
	       (local-set-key "\M-k" 'erase-sas-log-and-lst-buffer)
	       (local-set-key "\M-K" 'ess-sas-windows)
	       (local-set-key "\M-j" 'ess-eval-region-and-go)
	       (local-set-key "\M-l" 'mark-line)
	       (local-set-key "\M-i" 'dabbrev-expand)
	       (local-set-key "\M-q" 'indent-region)
	       (local-set-key "\M-\C-p" 'run-proc-print)
	       (local-set-key "\M-P" 'run-proc-print1)
	       (local-unset-key [(meta backspace)])))
;	       (if sas-toolbar
;		   (sas-toolbar-install-toolbar))
;	       (eg/menu)))
    
(add-hook 'SAS-log-mode-hook
	  '(lambda ()
	     (setq buffer-read-only nil)
	     (local-set-key "\M-e" 'sas-show-errors)
	     (local-set-key "\M-r" 'ess-revert-wisely)
	     (make-local-variable 'font-lock-defaults)
	     (setq font-lock-defaults 
		   '(SAS-log-font-lock-keywords t nil ((?' . "."))))
	     (turn-on-font-lock)))


(add-hook 'SAS-lst-mode-hook
	  '(lambda ()
	     (local-set-key "\M-r" 'ess-revert-wisely)
	     (setq buffer-read-only nil)))


(provide 'ess-sas-snps)
