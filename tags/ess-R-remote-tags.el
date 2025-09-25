;;; ess-R-remote-tags.el --- R on ssh servers  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2021  Thomas Alexander Gerds

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
;;
;; Conveniently running R on linux server for people at biostat.ku.dk
;; 1. interactive session
;;    M-x ess-biostat-server
;; 2. non-interactive in background (e.g., for simulations)
;;    M-x ess-run-script-biostat-server
;; 3. customize: in the following change to your KU-user name 
;;    and put modified lines in .emacs
;;
;; (setq ess-remote-servers
;;       '(("gauss" "abc123" "/home/ifsv/abc123/" nil)
;;         ("borel" "abc123" "/home/ifsv/abc123/" nil)
;;         ("rasch" "abc123" "/home/ifsv/abc123/" nil)))
;; 
;;; Code:

(defvar ess-remote-servers nil "Association list consisting of elements of 
 the form (\"server user home-directory R\"). E.g., Put the 
 following lisp code into your .emacs:
 (setq ess-remote-servers '((\"gauss\" \"abc123\" \"/home/ifsv/abc123/\" nil)
			   (\"borel\" \"abc123\" \"/home/ifsv/abc123/\" \"/usr/bin/R\")))
 to connect to server gauss with user name abc123 (abc123@gauss) and to server
 borel with user name abc123. On server gauss R is assumed to live in /usr/local/bin/R,
 for server bore R should be living in /usr/bin/R")

(defadvice shell-command (after shell-in-new-buffer (command &optional output-buffer error-buffer))
  (when (get-buffer "*Async Shell Command*")
    (with-current-buffer "*Async Shell Command*"
      (rename-uniquely))))
(ad-activate 'shell-command)

(defun ess-biostat-server (&optional server user home R)
  (interactive)
  (let* ((server (or server (assoc (completing-read "Start interactive R session on server: " ess-remote-servers) ess-remote-servers)))
	 (server-name (nth 0 server))
	 (user (or user (nth 1 server)))
	 (remote-home (or home (nth 2 server)))
	 (R (or R  (nth 3 server) "/usr/local/bin/R")))
    (if (require 'ssh)
	(ssh (concat user "@" server-name " -X " R " --no-save")))
    (message "When you see the prompt run M-x ess-remote RET and in your R-script M-x ess-switch-process RET")))

(defun ess-run-script-biostat-server (&optional ask)
  "Connect to one of the servers in list `ess-remote-servers'."
  (interactive "P")
  (let* ((server (assoc (completing-read "Name of the server: " ess-remote-servers) ess-remote-servers))
	 (server-name (nth 0 server))
	 (user (nth 1 server))
	 (remote-home (nth 2 server))
	 (local-Rfile (expand-file-name (buffer-file-name (current-buffer))))
	 (shared-home (string-match remote-home local-Rfile))
	 (remote-Rfile (if shared-home local-Rfile
			 (replace-regexp-in-string
			  (expand-file-name "~")
			  remote-home
			  local-Rfile)))
	 (log (concat (file-name-sans-extension remote-Rfile) "-" (format-time-string "%Y-%m-%d-%s") ".Rout"))
	 (copy-cmd (unless shared-home (concat "scp " local-Rfile " " user "@" server-name ":" remote-Rfile)))
	 (R  (or (nth 3 server) "/usr/local/bin/R"))
	 (cmd
	  (concat "ssh -X " user "@" server-name
		  " 'nohup nice -9 "
		  R
		  " --no-save --no-restore CMD BATCH "
		  remote-Rfile " " log "'")))
    (save-buffer)
    (when (or (not ask) (yes-or-no-p (concat "Run this command?: " cmd)))
      (save-buffer)
      (when copy-cmd
	(message copy-cmd)
	(shell-command copy-cmd))
      (message cmd)
      (save-excursion
	(when (get-buffer "*Async Shell Command*")
	  (with-current-buffer "*Async Shell Command*"
	    (rename-uniquely)))
	(async-shell-command cmd))
      (message cmd)
      (if shared-home
	  (find-file-other-window log)
	(find-file-other-window (concat "/scp:" user "@" server-name ":" log)))
      (require 'autorevert nil t)
      (unless (auto-revert-active-p)
	(auto-revert-mode)))))


(defun cox ()
  (interactive)
  (shell "*ess-cox*")
  (insert "cox.unicph.domain")
  ;; -X R --no-save")
  (comint-send-input)
  (sit-for 5)
  (insert "R")
  (comint-send-input)
  (sit-for 5)
  (set-buffer-process-coding-system 'utf-8 'utf-8)
  (ess-remote "*ess-cox*" "R"))

(defun rao ()
  (interactive)
  (shell "*ess-rao*")
  (insert "rao.unicph.domain")
  ;; -X R --no-save")
  (comint-send-input)
  (sit-for 5)
  (insert "R")
  (comint-send-input)
  (sit-for 5)
  (company-mode nil)
  (set-buffer-process-coding-system 'utf-8 'utf-8)
  (ess-remote "*ess-rao*" "R"))


(defun gauss ()
  (interactive)
  (shell "*ess-gauss*")
  (insert "gauss.unicph.domain")
  ;; -X R --no-save")
  (comint-send-input)
  (sit-for 5)
  (insert "R")
  (comint-send-input)
  (sit-for 5)
  (set-buffer-process-coding-system 'utf-8 'utf-8)
  (ess-remote "*ess-gauss*" "R"))

(defun doob ()
  (interactive)
  (shell "*ess-doob*")
  (insert "doob.unicph.domain")
  ;; -X R --no-save")
  (comint-send-input)
  (sit-for 2)
  (insert "R")
  (comint-send-input)
  (sit-for 2)
  (set-buffer-process-coding-system 'utf-8 'utf-8)
  (ess-remote "*ess-doob*" "R"))

(provide 'ess-R-remote-tags)
;;; ess-R-remote-tags.el ends here
