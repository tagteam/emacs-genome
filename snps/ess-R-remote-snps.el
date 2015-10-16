;;; ess-R-remote-snps.el --- R on ssh servers

;; Copyright (C) 2013  Thomas Alexander Gerds

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

;;; Code:

;;{{{ run script elsewhere, e.g on a ssh server called gauss
(defadvice shell-command (after shell-in-new-buffer (command &optional output-buffer error-buffer))
 (when (get-buffer "*Async Shell Command*")
 (with-current-buffer "*Async Shell Command*"
 (rename-uniquely))))
 (ad-activate 'shell-command)

(defvar ess-remote-servers nil "Association list consisting of elements of 
 the form (\"server user home-directory R\"). E.g., Put the 
 following lisp code into your .emacs:
 (setq ess-remote-servers '((\"gauss johnd\" \"/home/johnd/\")
			   (\"fisher doej\" \"/home/doej/\" \"/usr/bin/R\")))
 to connect to server gauss with user name johnd (johnd@gauss) and to server
 fisher with user name doej. On server gauss R is assumed to live in /usr/local/bin/R,
 for server fisher R should be living in /usr/bin/R")

(defun ess-wake-server (&optional server user home R)
  (interactive)
  (let* ((server (or server (assoc (completing-read "Start interactive R session on server: " ess-remote-servers) ess-remote-servers)))
	 (server-name (nth 0 server))
	 (user (or user (nth 1 server)))
	 (remote-home (or home (nth 2 server)))
	 (R (or R  (nth 3 server) "/usr/local/bin/R")))
    (if (require 'ssh)
	(ssh (concat user "@" server-name " -X " R " --no-save")))
    (message "When you see the prompt run M-x ess-remote RET and in your R-script M-x ess-switch-process RET")))

(defun ess-run-script-elsewhere (&optional ask)
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
;;}}}


(provide 'ess-R-remote-snps)
;;; ess-R-remote-snps.el ends here
