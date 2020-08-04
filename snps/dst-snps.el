;;; dst-snps.el --- Interactively start xfreerdp connection to Danmark statistics -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020  Thomas Alexander Gerds

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
;; a. You have ubuntu (may work on other linux and mac as well but not tested)
;; b. You have the following programs installed
;;
;;     (i) xfreerdp version 2.0.0-dev3 or higher
;;  see
;;      https://github.com/FreeRDP/Remmina/wiki/Compile-on-Ubuntu-18.04
;;      http://publicifsv.sund.ku.dk/~tag/HomePage/biobuntu/dst.html
;;
;;    (ii) xdotool
;;    (iv) chromium-browser
;;         (may work with other java-script enabled browser which
;;          is *not* your standard browser but would require a lot
;;          of changes to the code below)
;;
;; c. You have a specific directory on your computer for keeping the
;;    launcher files (dst-directory).
;; 
;;; Setup:
;;
;; 1. Load the file you are reading once via M-x eval-buffer RET and
;;    always by chaning your init (.emacs) file using one of the
;;    following 3 methods. The first 2 methods:
;;
;;    (require 'dst-snps) 
;;    (use-package dst-snps)
;;
;;    both require that you save the file dst-snps.el somewhere in your emacs
;;    load-path variable. If you don't know how to do this use the third method:
;;
;;    (load-file "/path/to/dst-snps.el")
;;
;; 2. In your init file (.emacs) adapt values of the following variables
;;    according to your own setting (see my setting below)
;;    dst-directory: the directory where you keep the launcherxyZ123.rdp files
;;    dst-click-position: to find this position:
;;                        I) start chromium-browser (M-x dst-start-browser)
;;                       II) place mouse cursor on the red f5 symbol
;;                      III) switch to a terminal back with the keyboard,
;;                           i.e., without changing the mouse position. e.g., Alt-TAB
;;                       IV) evaluate the command: xdotool getmouselocation
;;                        V) save the x and the y position
;;    dst-servers: a list of servers and click positions for the correponding buttons.
;;                 the click positions for the servers are obtained with the same procedure
;;                 as for dst-click-position described above
;;    dst-window-size : the width and height of the remote desktop
;;    dst-ident : your three or four letter name in the realm of DST 
;;    dst-login-list: a list whose elements are lists with three elements,
;;
;;    ("human-readable project identification" "project-number" "password")
;;
;;    you can have elements like this
;;
;;    ("human-readable project identification" "project-number" nil)
;; 
;;    to get prompted for the password
;;
;;    Here is my setup:
;;
;;    (setq dst-directory "~/dst")
;;    (setq dst-indent "WJA") ;; most people have a 4 letter name. I have only three letters.
;;    (setq dst-click-position "45 141") ;; mouse position of the red f5 symbol in chromium-browser 
;;    (setq dst-window-size '("1900" "1050")) ;; size of the remote desktop window
;;    (setq dst-servers '(("FSE Windows" . "999 276") ;; mouse position of the standard server
;;   		    ("srvfsegh4" .  "435 325") ;; mouse position of Gentofte server 4
;; 		    ("srvfsegh5" .  "740 325")))  ;; mouse position of Gentofte server 5
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
;;     Step 1: M-x dst-start-browser
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
(defvar dst-use-wmctrl t)

(defun dst-current-window ()
  "Return currently active window as xdotool number"
  (replace-regexp-in-string
   "\n" ""  
   (shell-command-to-string "xdotool getactivewindow")))

(defvar dst-click-delay "1"
  "number of seconds between clicks and keystrokes")


;; xprop -id $(xprop -root _NET_ACTIVE_WINDOW | cut -d ' ' -f 5) WM_NAME | awk -F '"' '{print $2}'
;; xprop -id $(xprop -root _NET_ACTIVE_WINDOW | cut -d ' ' -f 5) WM_NAME | sed -e 's/.*"\(.*\)".*/\1/'
(defun dst-current-window-name ()
  "Return title of currently active window"
  (replace-regexp-in-string "\n" ""
   (shell-command-to-string
    (concat "xdotool getwindowname "
	    ;; "xdotool getwindowfocus getwindowname"
	    (dst-current-window)))))

(defun dst-chromium-window ()
  (let ((cwin (shell-command-to-string
	       "xdotool search --onlyvisible --class 'chromium'")))
    (if (string= cwin "") nil
      (replace-regexp-in-string "\n" "" cwin))))

(defun dst-chromium-status ()
  (let* ((chromium-window (dst-chromium-window))
	 (chromium-status
	  (when chromium-window
	    (replace-regexp-in-string
	     "\n" "" 
	     (shell-command-to-string
	      (concat
	       "xdotool getwindowname " chromium-window))))))
    ;; three different states: 0 = logged in, 1 = waiting for ident + password, 2 = logged out
    (cond ((not chromium-status)
	   (message  "Cannot see  Chromium. Start it via dst-start-browser."))
	  ((string-match "logout page - Chromium" chromium-status) 
	   (message "DST browser: logout")
	   "logout")
	  ((string-match "remote.dst.dk - Chromium" chromium-status)
	   (message "DST browser: waiting for login")
	   "waiting")
	  ((string-match "F5 Dynamic Webtop - Chromium" chromium-status)
	   (message "DST browser: running")
	   "running")
	  (t
	   (error  "Cannot see Chromium. Start it via dst-start-browser."))
	  )))

(defun dst ()
  "3-in-1 function. 
   Step 1: open DST in Chromium.
   Step 2: login-dst
   Step 3: choose project, download launcher and start session."
  (interactive)
  (if (string= "running" (dst-start-browser))
      (if (dst-open-firewall)
	  (let* ((project (let* ((p (ido-completing-read
				     "Choose DST-project: "
				     dst-login-list
				     nil nil nil dst-login-history dst-last-project)))
			    (setq dst-last-project p)
			    (assoc p dst-login-list)))
		 (server (or (nth 3 project)
			     (completing-read "Server: " dst-servers)))
		 (user (concat dst-ident (nth 1 project) "@dstfse.local"))
		 (pss (nth 2 project))
		 (pos (cdr (assoc server dst-servers)))
		 pro-cmd cmd project rdp-buffer
		 (launcher (dst-download-launcher pos))
		 (cmd  (concat "xfreerdp " launcher " /size:"
			       (nth 0 dst-window-size)
			       "x" (nth 1 dst-window-size) 
			       " /u:" user " /p:" pss))
		 (async-shell-command-buffer 'new-buffer))
	    (setq rdp-buffer (generate-new-buffer "*rdp-response*"))
	    (message cmd)
	    (async-shell-command cmd rdp-buffer)
	    (sleep-for 1)
	    (if (process-live-p (get-buffer-process rdp-buffer))
		(message "First attempt succeeded")
	      (message "Second attempt  ...")
	      (setq launcher (dst-download-launcher pos))
	      (setq rdp-buffer (generate-new-buffer "*rdp-response*"))
	      (setq proc-cmd  (dst-select launcher project)
		    cmd (plist-get pro-cmd :cmd))
	      (message cmd)
	      (async-shell-command cmd rdp-buffer))))))
  
  

(defun dst-start-browser ()
  "Start chromium browser unless already running."
  (interactive)
  (let* ((cwin (dst-chromium-window))
	 ;; (ewin (dst-current-window))
	 ;; (emacs-window-name (dst-active-window-name))
	 (async-shell-command-buffer 'new-buffer)
	 (obuf (get-buffer-create "*chromium-output-buffer*"))
	 (ebuf (get-buffer-create "*chromium-error-buffer*")))
    ;; start chromium when necessary
    (if cwin
	"running" ;;(message "Chromium already running")
      (message "Launching chromium-browser. You have to manually go back to emacs to open the firewall via M-x dst-open-firewall.")
      (sit-for 1)
      (async-shell-command
        "chromium-browser --disable-infobars -new-instance -new-window http://remote.dst.dk/vdesk/hangup.php3"
	       ;; (if dst-use-wmctrl
	 ;; "wmctrl -a " emacs-window-name)
		   ;; "xdotool windowraise " ewin))
       obuf ebuf))))

(defun dst-show-domain-user-pos ()
  (interactive)
  (let ((cwin (dst-chromium-window)))
    (if (not cwin)
	(message "Cannot see chromium. Use M-x dst-start-browser RET to start it.")
      (shell-command (concat "xdotool windowraise " cwin
			     " mousemove --window " (dst-chromium-window) " " dst-domain-user-pos)))))
(defun dst-show-current-password-pos ()
  (interactive)
  (let ((cwin (dst-chromium-window)))
    (if (not cwin)
	(message "Cannot see chromium. Use M-x dst-start-browser RET to start it.")
      (shell-command (concat "xdotool windowraise " cwin
			     " mousemove --window " (dst-chromium-window) " " dst-current-password-pos)))))
(defun dst-show-new-password-pos ()
  (interactive)
  (let ((cwin (dst-chromium-window)))
    (if (not cwin)
	(message "Cannot see chromium. Use M-x dst-start-browser RET to start it.")
      (shell-command (concat "xdotool windowraise " cwin
			     " mousemove --window " (dst-chromium-window) " " dst-new-password-pos)))))
(defun dst-show-confirm-password-pos ()
  (interactive)
  (let ((cwin (dst-chromium-window)))
    (if (not cwin)
	(message "Cannot see chromium. Use M-x dst-start-browser RET to start it.")
      (shell-command (concat "xdotool windowraise " cwin
			     " mousemove --window " (dst-chromium-window) " " dst-confirm-password-pos)))))

(defun dst-show-submit-button-pos ()
  (interactive)
  (let ((cwin (dst-chromium-window)))
    (if (not cwin)
	(message "Cannot see chromium. Use M-x dst-start-browser RET to start it.")
      (shell-command (concat "xdotool windowraise " cwin
			     " mousemove --window " (dst-chromium-window) " " dst-submit-button-pos)))))

(defun dst-show-click-position ()
  (interactive)
  (let ((cwin (dst-chromium-window)))
    (if (not cwin)
	(message "Cannot see chromium. Use M-x dst-start-browser RET to start it.")
      (shell-command (concat "xdotool windowraise " cwin
			     " mousemove --window " (dst-chromium-window) " " dst-click-position)))))

(defun dst-show-change-password-position ()
  (interactive)
  (let ((cwin (dst-chromium-window))
	(ewin (dst-current-window)))
    (if (not cwin)
	(message "Cannot see chromium. Use M-x dst-start-browser RET to start it.")
      (shell-command (concat "xdotool windowraise " cwin
			     " mousemove --window "
			     (dst-chromium-window) " "
			     dst-change-password-pos)))))


(defun dst-goto-click ()
  (interactive)
  (let* ((serv (if (= (length dst-servers) 1) (caar dst-servers)
		 (completing-read "Server: " dst-servers)))
	 (pos (cdr (assoc serv dst-servers))))
    ;; (dst-focus-chromium)
    (shell-command (concat "xdotool mousemove --window " (dst-chromium-window) " " pos ";"))))

(defun dst-current-mouse-position ()
  (interactive)
  (message (shell-command-to-string
	    "xdotool getmouselocation")))

(defun dst-open-firewall ()
  "Start alternative browser and open firewall at remote.dst.dk."
  (interactive)
  (let ((buffer-read-only t))
    (unless dst-firewall-code
      (error "You need to set the variable `dst-firewall-code', i.e., when your code is 1234 add a line (setq dst-firewall-code \"1234\") to your .emacs file"))
    (unless dst-ident 
      (error "You need to set the variable `dst-ident', i.e., when your indent is ABCD add a line (setq dst-indent \"ABCD\") to your .emacs file"))
    (let* ((cwin (dst-chromium-window))
	   (status (dst-chromium-status))
	   (buffer-read-only t)
	   cmd)
      (unless cwin
	(error "Cannot see chromium. Use M-x dst-start-browser RET to start it."))
      (if	(string= status "running")
	  'running;; (message "Nothing to do. Firewall is already open.")
	(if (string= status "waiting")
	    (setq cmd (concat "xdotool windowraise " cwin " mousemove --sync --window " (dst-chromium-window) " " dst-click-position
			      ";xdotool click 1;"
			      "sleep " dst-click-delay ";"
			      "xdotool key Tab;"
			      "sleep " dst-click-delay ";"
			      "xdotool type '" dst-ident "';"
			      "sleep " dst-click-delay ";"
			      "xdotool key Tab;"
			      "sleep " dst-click-delay ";"
			      "xdotool type '" dst-firewall-code "';"
			      "sleep " dst-click-delay ";"
			      "xdotool key Return;"))
	  ;; logout
	  (setq cmd (concat "xdotool windowraise " cwin " mousemove --sync --window " (dst-chromium-window) " " dst-click-position
			    " xdotool click 1;"
			    "sleep " dst-click-delay ";"
			    "xdotool key Tab;"
			    "sleep " dst-click-delay ";"
			    "xdotool key Return;"
			    "sleep " dst-click-delay ";"
			    "xdotool type '" dst-ident "';"
			    "sleep " dst-click-delay ";"
			    "xdotool key Tab;"
			    "sleep " dst-click-delay ";"
			    "xdotool type '" dst-firewall-code "';"
			    "sleep " dst-click-delay ";"
			    "xdotool key Return;")))
	(message cmd)
	(shell-command cmd)
	nil))))
     
  
(defun dst-select (launcher &optional project)
  (interactive)
  (let* ((launcher (or launcher (car (directory-files dst-directory t "launch" t))))
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
	 (cmd  (concat "xfreerdp " launcher " /size:"
		       (nth 0 dst-window-size)
		       "x" (nth 1 dst-window-size) 
		       " /u:" user " /p:"
		       (or (nth 2 project)
			   (read-passwd (concat "Password for " user ": "))))))
    `(:project ,project :cmd ,cmd)))

(defun dst-download-launcher (pos)
  ;; remove old launchers
  (shell-command-to-string (concat "rm " dst-directory "/launch*.rdp"))
  ;; download new launcher
  (let ((ewin (dst-current-window))
	(cwin (dst-chromium-window))
	cmd result)
    (unless cwin
      (error "Cannot see chromium. Use M-x dst-start-browser RET to start it."))
    (unless (string= "running" (dst-chromium-status))
      (message "Need to open firewall via M-x dst-open-firewall"))
    (setq cmd
     (concat
      "xdotool windowraise " cwin "; xdotool mousemove --sync --window " (dst-chromium-window) " " pos ";"
      "xdotool click 1; sleep " dst-click-delay ";"
      "xdotool windowraise " ewin ";"))
    (message cmd)
    (message "HA:")
    (message (shell-command-to-string cmd))
    (setq result (shell-command-to-string (concat "mv ~/Downloads/launch*.rdp " dst-directory)))
    (if (string-match "cannot" result)
	nil
       (car (directory-files dst-directory t "launch" t)))))

(defun dst-connect (&optional server)
  (interactive)
  (let* ((server (or server (completing-read "Server: " dst-servers)))
	 (pos (cdr (assoc server dst-servers)))
	 pro-cmd cmd project rdp-buffer
	 (launcher (dst-download-launcher pos))
	 (async-shell-command-buffer 'new-buffer))
    (setq pro-cmd (dst-select launcher)
	  cmd (plist-get pro-cmd :cmd)
	  project (plist-get pro-cmd :project))
    (setq rdp-buffer (generate-new-buffer "*rdp-response*"))
    (message cmd)
    (async-shell-command cmd rdp-buffer)
    (sleep-for 1)
    (if (process-live-p (get-buffer-process rdp-buffer))
	(message "First attempt succeeded")
      (message "Second attempt  ...")
      (setq launcher (dst-download-launcher pos))
      (setq rdp-buffer (generate-new-buffer "*rdp-response*"))
      (setq proc-cmd  (dst-select launcher project)
	    cmd (plist-get pro-cmd :cmd))
      (message cmd)
      (async-shell-command cmd rdp-buffer))))


(defun dst1 () (interactive) (dst-connect "FSE Windows"))
(defun dst4 () (interactive) (dst-connect "srvfsegh4"))
(defun dst5 () (interactive) (dst-connect "srvfsegh5"))


(defun dst-change-all-passwords ()
  "Go through the list of projects and change the passwords."
  (interactive)
  (let ((ddlist dst-login-list)
	(new (read-string (concat "New password: "))))
    (while ddlist
      (setq this-project (car ddlist))
      (dst-change-password this-project new 'submit)
      (setq ddlist (cdr ddlist)))))

;;(dst-change-password (car dst-login-list) "bla" nil)

(defun dst-change-password (&optional project new-password click-submit)
  "Change passwords."
  (let* ((emacs-window  (dst-current-window))
	 (log-buf (get-buffer-create "*dst-change-password-log*"))
	 (this-project (or project
			   (let ((p (ido-completing-read
				     "Choose DST-project: "
				     dst-login-list
				     nil nil nil dst-login-history dst-last-project)))
			     (assoc p dst-login-list))))
	 (new (or new-password (read-string (concat "New password for " (nth 1 this-project) " (old is " (nth 2 this-project) "): "))))
	 (buffer-read-only t)
	 (chromium-window (dst-chromium-window))
	 (chromium-status (dst-chromium-status))
	 cmd)
    (unless (string= chromium-status "running")
      (error "Chromium not logged into DST"))
    (if (string-match new (nth 2 this-project))
	(message (concat "Already new password: "(nth 0 this-project)))
      (setq cmd (concat
		 "xdotool windowraise " chromium-window ";"
		 "xdotool mousemove --sync --window " (dst-chromium-window) " " dst-change-password-pos " click 1;" ;; change-password-button
		 "sleep 1;"
		 "xdotool mousemove --window " (dst-chromium-window) " " dst-domain-user-pos " click 1;"  ;; domain user field
		 "sleep 1;"
		 "xdotool type " dst-ident (nth 1 this-project) "@dstfse.local;"
		 "sleep 1;"
		 "xdotool mousemove --window " (dst-chromium-window) " " dst-current-password-pos " click 1;"  ;; current password
		 "sleep 1;"
		 "xdotool type " (nth 2 this-project) ";"
		 "sleep 1;"
		 "xdotool mousemove --window " (dst-chromium-window) " " dst-new-password-pos" click 1;" ;; new password
		 "sleep 1;"
		 "xdotool type " new ";"
		 "sleep 1;"
		 "xdotool mousemove --window " (dst-chromium-window) " " dst-confirm-password-pos " click 1;" ;; confirm password
		 "xdotool type " new ";"
		 "sleep 3;"
		 (when click-submit
		   (concat "xdotool mousemove --window " (dst-chromium-window) " " dst-submit-button-pos " click 1;"
			   "sleep 5;"
			   "xdotool key ctrl+w;"
			   "sleep 1;"
			   "xdotool windowraise " emacs-window))))
      (when (or click-submit
		(y-or-n-p (concat "Run this: " cmd)))
	(message cmd)
	(shell-command cmd)
	(save-excursion (set-buffer log-buf)
			(goto-char (point-max))
			(insert "Project: " (nth 1 this-project) "\told: " (nth 2 this-project) "\tnew: " new "\n")
			)))))


(provide 'dst-snps)
;;; dst-snps.el ends here
