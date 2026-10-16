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
(require 'cl-lib)
(require 'comint)

(defvar dst-change-password-python-script "~/bin/dst-change-password.py"
  "Python script used by `dst-change-password'.")
(defvar dst-ident nil
  "DST ident from dst-snps.el.")
(defvar dst-firewall-code nil
  "DST PIN/firewall code from dst-snps.el.")
(defvar dst-login-list nil
  "DST project list from dst-snps.el.")
(defvar dst-login-history nil
  "Completion history from dst-snps.el.")
(defvar dst-last-project nil
  "Last selected DST project from dst-snps.el.")

(defvar dst-login-list nil)
(defvar dst-emacs-window-identifier "")
(defvar dst-login-history nil)
(defvar dst-last-project nil)
(defvar dst-auto-click 'chromium)
(defvar dst-window-size nil)
(defvar dst-screen-setting 'laptop)
(defvar dst-use-wmctrl t)


;; need these xdotool wrappers to type passwords 
(defun dst-xfree-window ()
  (let ((cwin (shell-command-to-string "xdotool search --class 'xfreerdp'")))
    (if (string= cwin "") nil
      (replace-regexp-in-string "\n" "" cwin))))
(defun dst-raise-dst ()
  (interactive)
  (shell-command-to-string (concat "xdotool windowraise " (dst-xfree-window) " mousemove --sync --window " (dst-xfree-window) " " dst-click-position
				     ";xdotool click 1;")))

(defun dst (&optional arg)
  "3-in-1 function. 
   Step 1: open DST in Chromium.
   Step 2: login-dst
   Step 3: choose project, download launcher and start xfreerdp session."
  (interactive "P")
    (let* ((p (ido-completing-read
				   "Choose DST-project: "
				   dst-login-list
				   nil nil nil dst-login-history dst-last-project))
	   (project (let* ()
		      (setq dst-last-project p)
		      (assoc p dst-login-list)))
	   	       (server (or (nth 3 project)
			   (completing-read "Server: " dst-servers)))
	   (python-command
	    (concat 
             "python3 ~/bin/dst-login.py "
	     ;; "dst-login "
	     dst-ident " "
	     dst-firewall-code " "
	     (nth 1 project) " "
	     "--password '" (nth 2 project) "' "
	     (if arg "--visual " "")
	     "--download-button-id /Common/dst-RDP_FSE_" (nth 3 project))))
      ;; (message python-command)
      (shell "*dst-remote*")
      (switch-to-buffer "*dst-remote*")
      (insert python-command)
      (comint-send-input)))

(defun dst-open-firewall ()
  "Start chromium browser and open firewall at remote.dst.dk."
  (interactive)
  (unless dst-firewall-code
    (error "You need to set the variable `dst-firewall-code', i.e., when your code is 1234 add a line (setq dst-firewall-code \"1234\") to your .emacs file"))
  (unless dst-ident 
    (error "You need to set the variable `dst-ident', i.e., when your indent is ABCD add a line (setq dst-indent \"ABCD\") to your .emacs file"))
  (let ((buffer-read-only t)
	(python-command
	 (concat 
          "python3 ~/bin/dst-open-firewall.py "
	  dst-ident " "
	  dst-firewall-code " "
	  "--visual ")))
    (message python-command)
    ;; (shell-command python-command)))
    (shell "*dst-remote*")
    (switch-to-buffer "*dst-remote*")
    (insert python-command)
    (comint-send-input)))
(defun dst--change-password-check-config ()
  (unless (bound-and-true-p dst-ident)
    (error "You need to set `dst-ident'"))
  (unless (bound-and-true-p dst-firewall-code)
    (error "You need to set `dst-firewall-code'")))

(defun dst--change-password-resolve-project (project)
  (cond
   ((and (listp project) (nth 1 project)) project)
   ((and (stringp project) dst-login-list)
    (or (assoc project dst-login-list)
        (cl-find-if (lambda (entry) (string= project (nth 1 entry)))
                    dst-login-list)
        (list project project nil)))
   ((stringp project)
    (list project project nil))
   (dst-login-list
    (let* ((choice (ido-completing-read
                    "Choose DST-project: "
                    dst-login-list nil nil nil dst-login-history dst-last-project))
           (entry (assoc choice dst-login-list)))
      (setq dst-last-project choice)
      entry))
   (t
    (let ((number (read-string "DST project number: ")))
      (list number number nil)))))

(defun dst--change-password-start-process (buffer-name args secret-lines)
  (let* ((buffer (get-buffer-create buffer-name))
         (process-connection-type nil))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (apply #'make-comint-in-buffer
           "dst-change-password" buffer "python3" nil args)
    (let ((proc (get-buffer-process buffer)))
      (unless proc
        (error "Could not start dst-change-password process"))
      (dolist (line secret-lines)
        (process-send-string proc (concat line "\n")))
      (pop-to-buffer buffer)
      (message "Enter the SMS code in %s when prompted" buffer-name)
      proc)))

(defun dst--change-password-python-base-args ()
  (list "-u"
        (expand-file-name dst-change-password-python-script)
        dst-ident
        dst-firewall-code))

(defun dst--change-password-start-batch (buffer-name projects new-password &optional visual)
  (dst--change-password-start-process
   buffer-name
   (append (dst--change-password-python-base-args)
           (list "--batch-stdin"
                 (number-to-string (length projects))
                 "--new-password-stdin"
                 "--submit")
           (when visual
             (list "--visual")))
   (cons new-password
         (mapcar (lambda (project)
                   (concat (nth 0 project) "\t" (nth 1 project)))
                 projects))))

(defun dst-change-password (&optional project new-password click-submit visual)
  "Change one DST project password via `dst-change-password.py'.
With VISUAL non-nil, show the Selenium browser.  Programmatic
CLICK-SUBMIT keeps compatibility with the old xdotool function."
  (interactive (list nil nil nil current-prefix-arg))
  (dst--change-password-check-config)
  (let* ((entry (dst--change-password-resolve-project project))
         (project-number (nth 1 entry))
         (current-password (or (nth 2 entry)
                               (read-passwd
                                (format "Current password for %s: " project-number))))
         (new-password (or new-password
                           (read-passwd
                            (format "New password for %s: " project-number) t)))
         (submit (or click-submit
                     (y-or-n-p
                      (format "Submit password change for %s now? " project-number))))
         (args (append (dst--change-password-python-base-args)
                       (list project-number
                             "--password-stdin"
                             "--new-password-stdin"
                             (if submit "--submit" "--no-submit"))
                       (when (or visual (not submit))
                         (list "--visual")))))
    (when (string= current-password new-password)
      (user-error "New password is identical to the current password"))
    (dst--change-password-start-process
     (format "*dst-change-password-%s*" project-number)
     args
     (list current-password new-password))))

(defun dst-change-password-loop (&optional new-password visual)
  "Select several DST projects and change them in one Python session."
  (interactive (list nil current-prefix-arg))
  (dst--change-password-check-config)
  (let ((new-password (or new-password
                          (read-passwd "New password for selected DST projects: " t)))
        seen
        projects
        done)
    (while (not done)
      (let* ((entry
              (if dst-login-list
                  (let* ((choices (cons (list "DONE" nil nil) dst-login-list))
                         (choice (ido-completing-read
                                  "Choose DST-project, or DONE: "
                                  choices nil nil nil dst-login-history dst-last-project)))
                    (if (string= choice "DONE")
                        nil
                      (setq dst-last-project choice)
                      (assoc choice dst-login-list)))
                (let ((number (read-string "DST project number, empty to start: ")))
                  (unless (string-empty-p number)
                    (list number number nil))))))
        (if (not entry)
            (setq done t)
          (let* ((number (nth 1 entry))
                 (current-password
                  (or (nth 2 entry)
                      (read-passwd (format "Current password for %s: " number)))))
            (cond
             ((member number seen)
              (message "Already selected %s" number))
             ((string= current-password new-password)
              (message "Skipping %s: current password already matches new password" number))
             (t
              (push number seen)
              (push (list number current-password) projects)))))))
    (setq projects (nreverse projects))
    (unless projects
      (user-error "No projects selected"))
    (unless (y-or-n-p (format "Submit password change for %d projects in one DST session? "
                              (length projects)))
      (user-error "Canceled"))
    (dst--change-password-start-batch
     "*dst-change-password-loop*" projects new-password visual)))

(defun dst-change-all-passwords (&optional new-password visual)
  "Change passwords for each unique project in `dst-login-list' in one session."
  (interactive (list nil current-prefix-arg))
  (dst--change-password-check-config)
  (unless dst-login-list
    (user-error "`dst-login-list' is empty"))
  (let ((new-password (or new-password
                          (read-passwd "New password for all DST projects: " t)))
        seen
        projects)
    (dolist (entry dst-login-list)
      (let ((number (nth 1 entry)))
        (unless (member number seen)
          (push number seen)
          (let ((current-password
                 (or (nth 2 entry)
                     (read-passwd (format "Current password for %s: " number)))))
            (if (string= current-password new-password)
                (message "Skipping %s: current password already matches new password" number)
              (push (list number current-password) projects))))))
    (setq projects (nreverse projects))
    (unless projects
      (user-error "No projects need a password change"))
    (unless (y-or-n-p (format "Submit password change for %d projects in one DST session? "
                              (length projects)))
      (user-error "Canceled"))
    (dst--change-password-start-batch
     "*dst-change-all-passwords*" projects new-password visual)))
  

(provide 'dst-snps)

;;; dst-snps.el ends here
