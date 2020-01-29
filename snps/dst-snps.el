;;; dst-snps.el --- Interactively start xfreerdp connection to Danmark statistics -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019  Thomas Alexander Gerds

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Prequisites
;;
;; a. You have a linux computer (may work for mac as well but not tested)
;; b. Your linux computer has the following programs installed
;;
;;     (i) xfreerdp version 2.0.0-dev3 or higher
;;  see 
;;      http://publicifsv.sund.ku.dk/~tag/HomePage/biobuntu/dst.html
;;
;;    (ii) wmctrl
;;   (iii) xdotool
;;    (iv) chromium-browser (or another java-script enabled browser which is *not* your standard browser)
;; c. You have an otherwise unused directory for keeping the launcher files (dst-directory)
;; 
;;; Setup:
;;
;; 1. Load this file from your init (.emacs) via one of the
;;    following lines. The first two require that you save the
;;    file in your load-path or modify your load-path to contain the
;;    place where the file dst-snps.el lives:
;;
;;    (require 'dst-snps)
;;    (use-package dst-snps)
;;    (load-file "/path/to/dst-snps.el")
;;
;; 2. In your init file (.emacs) adapt the following variables
;;    according to your own data
;;    dst-directory   : the directory where you keep the launcherxyZ123.rdp files
;;    dst-window-size : a list of two strings 
;;    dst-ident : you three or four letter name
;;    dst-servers: a list of servers and click positions for the correponding button
;;    dst-login-list: a list whose elements are lists with three elements,
;;    ("human-readable project identification" "project-user-name" "password")
;;
;;    you can have elements like this
;;
;;    ("human-readable project identification" "project-user-name" nil)
;; 
;;    to get prompted for the password
;;
;;    and you can have (setq dst-login-list nil) to get prompted for both
;;    user name and password.
;;
;;    Here is my setup:
;;
;;    (setq dst-directory "~/dst")
;;    (setq dst-click-position "45 141")
;;    (setq dst-window-size '("1900" "1050"))
;;    (setq dst-servers '(("FSE Windows" . "999 276")
;;   		    ("srvfsegh4" .  "435 325")
;; 		    ("srvfsegh5" .  "740 325")))
;;
;;    (setq dst-login-list
;;          ;; ("some-text-to-identify-project" "project-number" "password")
;;          '(("Regitze" "3775"  "xxxxxxxxx")
;;            ("Anne Lyng, Mette Friberg Hitz, Jacob Holm" "4432" "xxxxxxxxx")
;;            ("Brice, Peter, Anders Nissen, CJ Lee, Anne, MortenLamberts" "4791" "xxxxxxxxx")
;;            ("Carolina, Steen, Kristian, Shahzleen, Kathrine Bach, Carlo" "3661" "xxxxxxxxx")
;;            ("Peter Enemark Aalborg" "6076" "xxxxxxxxx")
;;            ("Henrik, Laura, Morten Malmborg" "3740" "xxxxxxxxx")
;;            ("Jonas Bille Nielsen, Morten Wagner" "3607" "xxxxxxxxx")
;;            ("RWAS" "6220" "xxxxxxxxx")))
;;
;;; Usage:
;;     Step 1: https://remote.dst.dk/
;;
;;     M-x dst-open-firewall
;;
;;     Step 2: download launcher and connect
;; 
;;     M-x dst RET
;;
;;     if this fails you may have to download a new launcher ...
;;  
;;; Code:
(require 'superman-file-list)
(defvar dst-directory "~/dst")
(defvar dst-login-list nil)
(defvar dst-emacs-window-identifier "")
(defvar dst-login-history nil)
(defvar dst-last-project nil)
(defvar dst-auto-click 'chromium)
(defvar dst-window-size nil)
(defvar dst-screen-setting 'laptop)

(defun dst-current-window ()
  "Return currently active window as xdotool number"
  (replace-regexp-in-string
   "\n" ""  
   (shell-command-to-string "xdotool getactivewindow")))

(defun dst-current-window-name ()
  "Return title of currently active window"
  (replace-regexp-in-string "\n" ""
   (shell-command-to-string
    (concat "xdotool getwindowname "
	    (dst-current-window)))))

(defun dst-chromium-window ()
  (let ((cwin (shell-command-to-string
	       "xdotool search --onlyvisible --class 'chromium'")))
    (if (string= cwin "") nil
      (replace-regexp-in-string "\n" "" cwin))))

(defun dst-chromium-status ()
  (let ((chromium-window (dst-chromium-window)))
    (when chromium-window
      (replace-regexp-in-string "\n" "" 
      (shell-command-to-string
       (concat
	"xdotool getwindowname " chromium-window))))))

(defun dst-launch-browser ()
  "Start chromium browser unless already running."
  (interactive)
  (let* ((chromium-window (dst-chromium-window)))
    ;; start chromium when necessary
    (unless chromium-window
      (async-shell-command
       (concat "chromium-browser --disable-infobars -new-instance -new-window http://remote.dst.dk/vdesk/hangup.php3;")))))
    
(defun dst-browser ()
  (interactive)
  "Check if chromium is running and whether it is showing dst remote"
  (let* ((emacs-window (dst-current-window))
	 (async-shell-command-buffer 'new-buffer)
	 (chromium-window (dst-chromium-window))
	 (chromium-status (dst-chromium-status))
	 (response
	  ;; three different states: 0 = logged in, 1 = waiting for ident + password, 2 = logged out
	  (cond ((not chromium-status)
		 (error  "Cannot see a chromium window via dst-chromium-status."))
		((string-match "logout page - Chromium" chromium-status) 
		 (message "DST browser: logout")
		 "logout")
		((string-match "remote.dst.dk - Chromium" chromium-status)
		 (message "DST browser: waiting for login")
		 "waiting")
		((string-match "F5 Dynamic Webtop - Chromium" chromium-status)
		 (message "DST browser: waiting")
		 "running")
		(t (dst-launch-browser)))))
    `(:status ,response :window ,chromium-window)))
  
(defun dst-goto-click ()
  (interactive)
  (let* ((serv (if (= (length dst-servers) 1) (caar dst-servers)
		 (completing-read "Server: " dst-servers)))
	 (pos (cdr (assoc serv dst-servers))))
    (dst-focus-chromium)
    (shell-command (concat "xdotool mousemove " pos ";"))))

(defun dst-current-mouse-position ()
  (interactive)
  (message (shell-command-to-string
	    "xdotool getmouselocation")))

(defun dst-open-firewall ()
  "Start alternative browser and open firewall at remote.dst.dk."
  (interactive)
  (unless dst-firewall-code 
    (error "You need to set the variable `dst-firewall-code'"))
  (unless dst-ident 
    (error "You need to set the variable `dst-ident'"))
  (let* ((async-shell-command-buffer 'new-buffer)
	 (emacs-window  (dst-active-window-name ""))
	 (cw (dst-browser))
	 (chromium-status (plist-get cw :status))
	 (chromium-window (plist-get cw :window))
	 (cmd-1
	  (unless (string= chromium-status "running")
	    (concat
	     "wmctrl -a " chromium-window ";"
	     "xdotool mousemove " dst-click-position " click 1;"
	     "sleep 0.1;"
	     "xdotool mousemove " dst-click-position " click 1;"
	     "sleep 0.1;"
	     "xdotool key Tab;"
	     "sleep 0.1;"
	     (unless (string= chromium-status "waiting") 
	       "xdotool key Return;")
	     "wmctrl -a " emacs-window ";")))
	 cmd-2)
    ;; xdotool getmouselocation
    (if (not cmd-1) chromium-window
      (message cmd-1)
      (shell-command-to-string cmd-1)
      (sleep-for 1)
      (setq cw (dst-browser)
            chromium-status (plist-get cw :status)
            chromium-window (plist-get cw :window))
      (message chromium-status)
      (sleep-for 1)
      (if (string= chromium-status "waiting")
	  (progn
	    (setq cmd-2
	     (concat
	      "wmctrl -a " chromium-window ";"
	      "xdotool mousemove " dst-click-position " click 1;"
	      "sleep 0.1;"
	      "xdotool key Tab;"
	      "sleep 1.1;"
	      "xdotool type '" dst-ident "';"
	      "sleep 0.1;"
	      "xdotool key Tab;"
	      "sleep 1.1;"
	      "xdotool type '" dst-firewall-code "';"
	      "sleep 0.1;"
	      "xdotool key Return;"))
	    (message cmd-2)
	    (shell-command-to-string cmd-2))
	(message chromium-status)
	;;(message "Cannot start typing ...")
	))))
  
(defun dst-select (&optional project)
  (interactive)
  (let* ((last-launcher (car (directory-files dst-directory t "launch" t)))
	 (project (or project (if dst-login-list
				  (let* ((p (ido-completing-read
					     "Choose DST-project: "
					     dst-login-list
					     nil nil nil dst-login-history dst-last-project)))
				    (setq dst-last-project p)
				    (assoc p dst-login-list))
				(let* ((u (read-string "DST project number (only last 4 digits, e.g., 3274): "))
				       (p (read-passwd (concat "password for " u ": "))))
				  `("none" ,u ,p)))))
	 (user (concat dst-ident (nth 1 project) "@dstfse.local"))
	 (cmd  (concat "xfreerdp " last-launcher " /size:"
		       (nth 0 dst-window-size)
		       "x" (nth 1 dst-window-size) 
		       " /u:" user " /p:"
		       (or (nth 2 project)
			   (read-passwd (concat "Password for " user ": "))))))
    `(:project ,project :cmd ,cmd)))

(defun dst-download-launcher (server pos)
  ;; remove old launchers
  (shell-command (concat "rm " dst-directory "/launch*.rdp"))
  ;; download new launcher
  (shell-command (concat
		  "wmctrl -a F5 Dynamic Webtop;"
		  "xdotool mousemove " pos " click 1;"
		  "sleep 2;"))
  (shell-command-to-string (concat "mv ~/Downloads/launch*.rdp " dst-directory)))

(defun dst-connect (&optional server)
  (interactive)
  (let ((emacs-window  (dst-current-window)))
    ;; test firewall
    ;(dst-open-firewall);; starts browser if necessary, moves browser in position
    (let* ((server (or server (completing-read "Server: " dst-servers)))
	   (pos (cdr (assoc server dst-servers)))
	   pro-cmd cmd project rdp-buffer  
	   (async-shell-command-buffer 'new-buffer))
      (dst-download-launcher server pos)
      (shell-command (concat "wmctrl -a " emacs-window))
      (setq pro-cmd (dst-select)
	    cmd (plist-get pro-cmd :cmd)
	    project (plist-get pro-cmd :project))
      (setq rdp-buffer (generate-new-buffer "*rdp-response*"))
      (message cmd)
      (async-shell-command cmd rdp-buffer)
      (sleep-for 1)
      (if (process-live-p (get-buffer-process rdp-buffer))
	  (message "First attempt succeeded")
	(message "Second attempt  ...")
	(dst-download-launcher server pos)
	(shell-command (concat "wmctrl -a " emacs-window))
	(setq rdp-buffer (generate-new-buffer "*rdp-response*"))
	(setq proc-cmd  (dst-select project)
	      cmd (plist-get pro-cmd :cmd))
	(message cmd)
	(async-shell-command cmd rdp-buffer)))))

(defun dst1 () (interactive) (dst-connect "FSE Windows"))
(defun dst4 () (interactive) (dst-connect "srvfsegh4"))
(defun dst5 () (interactive) (dst-connect "srvfsegh5"))


(defun dst-change-all-passwords ()
  (interactive)
  (let ((ddlist dst-login-list)
	(new (read-string (concat "New password: "))))
    (while ddlist
      (setq this-project (car ddlist))
      (dst-change-password this-project new 'submit)
    (setq ddlist (cdr ddlist)))))

(defun dst-change-password (&optional project new-password click-submit)
  "Go through the list of projects and change the passwords."
  (interactive)
  (let* ((log-buf (get-buffer-create "*dst-change-password-log*"))
	 (wlist (shell-command-to-string "wmctrl -l"))
	 (emacs-window  (dst-active-window-name ""))
	 (this-project (or project
			   (let ((p (ido-completing-read
				     "Choose DST-project: "
				     dst-login-list
				     nil nil nil dst-login-history dst-last-project)))
			     (assoc p dst-login-list))))
	 (new (or new-password (read-string (concat "New password for " (nth 1 this-project) " (old is " (nth 2 this-project) "): "))))
	 (cw (dst-browser))
	 (chromium-status (plist-get cw :status))
	 (chromium-window (plist-get cw :window))
	 cmd)
    (unless (string= chromium-status "running")
      (error "Chromium not logged into DST"))
    (if (string-match new (nth 2 this-project))
	(message (concat "Already new password: "(nth 0 this-project)))
      (setq cmd (concat
		 "wmctrl -a " chromium-window ";"
		 "xdotool mousemove " dst-change-password-pos " click 1;" ;; change-password-button
		 "sleep 4;"
		 "xdotool mousemove " dst-domain-user-pos " click 1;"  ;; domain user field
		 "sleep 1;"
		 "xdotool type " dst-ident (nth 1 this-project) "@dstfse.local;"
		 "sleep 1;"
		 "xdotool mousemove " dst-current-password-pos " click 1;"  ;; current password
		 "sleep 1;"
		 "xdotool type " (nth 2 this-project) ";"
		 "sleep 1;"
		 "xdotool mousemove " dst-new-password-pos" click 1;" ;; new password
		 "sleep 1;"
		 "xdotool type " new ";"
		 "sleep 1;"
		 "xdotool mousemove " dst-confirm-password-pos " click 1;" ;; confirm password
		 "xdotool type " new ";"
		 "sleep 1;"
		 (when click-submit
		   (concat "xdotool mousemove " dst-submit-button-pos " click 1;"
			   "sleep 1;"
			   "xdotool key ctrl+w;"
			   "sleep 1;"
			   "wmctrl -a " emacs-window))))
      (when (or click-submit
		(y-or-n-p (concat "Run this: " cmd)))
	(message cmd)
	(shell-command cmd)
	(save-excursion (set-buffer log-buf) 
			(insert "Project: " (nth 1 this-project) "\told: " (nth 2 this-project) "\tnew: " new)
			)))))


(provide 'dst-snps)
;;; dst-snps.el ends here
