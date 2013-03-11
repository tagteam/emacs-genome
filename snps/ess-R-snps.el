;;; ess-R-snps.el --- ess setup file for statistical software R

;; Copyright (C) 2012  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <tag@biostat.ku.dk>
;; Keywords: convenience, tools

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

;;{{{ global custom
(setq ess-use-tracebug nil)
(setq ess-display-buffer-reuse-frames nil)
(setq-default ess-ask-for-ess-directory nil)
(setq-default ess-directory (concat (getenv "HOME") "/R/"))
(setq-default ess-history-directory (concat (getenv "HOME") "/R/"))
(setq-default ess-language "R")
(setq-default ess-dialect "R")
(setq inferior-ess-font-lock-keywords nil)
(unless (featurep 'xemacs)
  (setq ess-eval-deactivate-mark t))
(setq ess-eval-visibly-p t)
;;}}}
;;{{{ key bindings

(add-hook 'ess-mode-hook 'eg/R-keybindings)
(defun eg/R-keybindings ()
  (interactive)
  (define-key ess-mode-map "\M-j" 'ess-eval-region-and-go)
  (define-key ess-mode-map "\M-r" 'copy-region-as-kill)
  (define-key ess-mode-map "\M-q" 'eg/indent-paragraph)
  (define-key ess-mode-map "\M-l" 'mark-line)
  (define-key ess-mode-map [(backspace)] 'delete-backward-char)
  (define-key ess-mode-map [(meta backspace)] 'backward-kill-word))
;;}}}
;;{{{ expanding objects

(setq ess-tab-complete-in-script t)
(setq ess-first-tab-never-complete nil)
(add-hook 'ess-mode-hook
	  '(lambda ()
	     (make-variable-buffer-local 'hippie-expand-try-functions-list)
	     (setq hippie-expand-try-functions-list
		   (append (list 'ess-complete-object-name)
			   hippie-expand-try-functions-list))))
;;}}}
;;{{{ R mode
(defun eg/ess-eval-and-go ()
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

(add-hook 'ess-mode-hook 'eg/R-mode)
(defun eg/R-mode ()
  (interactive)
  (setq split-width-threshold nil)
  (setq dabbrev-abbrev-skip-leading-regexp "\\$") 
  (setq ess-fancy-comments nil))
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

;; (defun ess-show-buffer (buf &optional visit)
  ;; (pop-to-buffer buf t (selected-frame)))

;; (defadvice ess-eval-linewise (after add-history first activate)
  ;; (if (and (not (eq major-mode 'inferior-ess-mode)) (< (length text-withtabs) 300))
      ;; (save-excursion
	;; (set-buffer (process-buffer
		     ;; (get-ess-process
		      ;; ess-current-process-name)))
	;; (comint-add-to-input-history
	 ;; (let* ((str text-withtabs)
		;; (pos (string-match "\n" str)))
	   ;; (while pos
	     ;; (setq str (concat (substring str 0 pos)
			       ;; (substring str (+ 1 pos))))
	     ;; (setq pos (string-match "\n" str)))
	   ;; str)))))

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
;;{{{ smart underscore
(defun eg/ess-smart-underscore ()
  (interactive)
  (if (not (eq last-command 'eg/ess-smart-underscore))
      (insert " <- ")
    (undo)
    (insert "_")))
;;}}}
;;{{{ clearing the inferior window
(defun R-inferior-clear(&optional arg)
  (interactive "P")
  (save-excursion
    (let ((pbuf (or
		 (condition-case nil
		     (process-buffer (get-ess-process ess-local-process-name))
		   (error nil))
		 (concat "*" ess-current-process-name "*"))))
      ;; (pop-to-buffer pbuf)
      (set-buffer pbuf)
      (when arg (erase-buffer)
	    (comint-send-input))
      (ess-switch-to-end-of-ESS))))
;;}}}
(provide 'ess-R-snps)
;;; ess-R-snps.el ends here
