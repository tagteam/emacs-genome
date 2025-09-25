;; -*- lexical-binding: t; -*-
;; dst user x56 password uuu888UUU (rrr888RRR) (vCH9byRp)
;; fjernskrivebord 7389, computer:DS7217 

(setq dst-click-delay "0.5")
(setq dst-directory "~/dst")
;; (setq dst-window-size "/size:1600x1000")
(setq dst-window-size '("1900" "1050"))
(setq dst-ident "WJA")
;; (setq dst-ident "FBVQ")
(setq dst-firewall-code "6632")
;; sasmita

;;xdotool getmouselocation
(setq dst-click-position "45 141")
;;(setq dst-click-position "960 624")

;;(shell-command "xdotool mousemove 727 276")
(setq dst-domain-user-pos "975 350"
      dst-current-password-pos "975 377"
      dst-new-password-pos "975 408"
      dst-confirm-password-pos "975 433"
      dst-submit-button-pos "1061 477")

;; (setq dst-domain-user-pos "1021 318"
      ;; dst-current-password-pos "1021 344"
      ;; dst-new-password-pos "1021 375"
      ;; dst-confirm-password-pos "1021 402"
      ;; dst-submit-button-pos "1063 444")

;; (shell-command (concat "xdotool mousemove " dst-change-password-pos))
(setq dst-change-password-pos "943 346")
;;(setq dst-change-password-pos "750 240")
(setq dst-servers '(("Windows" . "134 497")
		    ("srvfsecancer4" .  "685 354")
		    ("srvfsecancer3" .  "413 354")
		    ("srvfsegh4" .  "1217 466")
		    ("srvfsegh5" .  "1496 466")
		    ("srvfsegh6" .  "137 640")
		    ("srvfseatru1" . "406 501")))

;; for RWAS project where screen is double locked
(defun dst-type-password-twice ()
  (interactive)
  (shell-command-to-string
   (concat "xdotool windowraise " (dst-xfree-window) " mousemove --sync --window " (dst-xfree-window) " 941 449" 
	   ";xdotool click 1;sleep 2;"
	   "xdotool type " dst-pw ";xdotool key Return;"
	   "sleep 1;"
	   "xdotool type " dst-pw ";xdotool key Return;")))

;; to unlock the dst windows server
(defun dst-type-password ()
  (interactive)
  (shell-command-to-string
   (concat "xdotool windowraise " (dst-xfree-window) " mousemove --sync --window " (dst-xfree-window) " 941 449" 
	   ";xdotool click 1;sleep 2;xdotool type " dst-pw ";xdotool key Return;")))

;; WJA: 6632
;; WJAxxxx@dstfse.local
;; Kohl17MAISE!
(setq dst-pw "4b578MiC#17!MacS")
;; ("3573 Overall meta project" "3573"  ,dst-pw "srvfsegh4")
(setq dst-login-list
      `(
	("707655 Steno Main" "707655" ,dst-pw "Windows")
	("709873 DMreg-projekt" "709873" ,dst-pw "Windows")
	("3740 Diabetes" "3740" ,dst-pw "srvfsegh5")
	("3740 Diabetes server 4" "3740" ,dst-pw "srvfsegh4")
	("6582 Corona, Covid, Mavish, Grimur" "706582" ,dst-pw "srvfsegh5")
	("6220 RWAS" "6220" ,dst-pw "Windows")
	("3775 Hannah, Marcella, Regitze" "3775"  ,dst-pw "srvfsegh4")
	("706818 Alexander Falkentoft, deepthi, novo, Jarl, Jannik, Caroline, Christina Lee" "706818" ,dst-pw "srvfsegh4")
	;; ("3826 Liv" "3826"  ,dst-pw "srvfsegh4")
	;; ("3661 Sidsel, Daniel, Carolina, Steen, Kristian, Shahzleen, Carlo" "3661" ,dst-pw "srvfsegh4")
	;; ("3662 Mitroflow" "3662" ,dst-pw "srvfsegh4")
	;; ("3657 Louise" "3657" ,dst-pw "srvfsegh4")
	;; ("6130 Maria D'Souza" "6130" ,dst-pw "srvfsegh4")
	("6322 Marianna Hjertetal" "6322" ,dst-pw "srvfsegh4")
	;; ("6734 Genetics Maria" "6734" ,dst-pw "srvfsegh4")
	("9200 Statins" "9200" ,dst-pw "srvfsegh4")
 	;; ("3607 Jonas L. Isaksen, Jonas Bille Nielsen, Morten Wagner" "3607" ,dst-pw "Windows")
	("4265 Kessing" "4265" ,dst-pw "Windows")
	("7535 Mie Balling" "707535" ,dst-pw "Windows")
	("706338 Mathias" "706338" ,dst-pw "srvfsegh4")
	("9200 Daniel, Alex, statins, forkalkning" "709200" ,dst-pw "Windows")
	("7352 Simon-Kessing" "707352" ,dst-pw "Windows")
	("9281 Dea Rasmus-Steno CKD" "709281" ,dst-pw  "srvfsegh4")
	("707089 Deepthi" "707089" ,dst-pw "srvfsegh4")
	("709924 Steno NF report" "709924" ,dst-pw "Windows")
	))


(defun dst-open-x56 ()
  (interactive)
  ;; (setq dst-ident "x51") 
  ;; (setq dst-firewall-code "9726")
  (let ((dst-ident "x56")
	(dst-firewall-code "7389"))
    (dst-open-firewall)))

(defun dst56 ()
  (interactive)
  (let ((emacs-window  (dst-getwindow-name "")) last-launcher)
    (shell-command
     (concat
      "wmctrl -a F5 Dynamic Webtop;"
      "xdotool mousemove 1082 272 click 1;"
      "sleep 1;"
;      "xdotool type DS6466;"
      "xdotool type DS7217;"
      "xdotool key Return;"
      "sleep 1;"
      "rm ~/dst/launch*;mv ~/Downloads/launch*.rdp ~/dst;"
      "wmctrl -a " emacs-window ";"))
  (setq last-launcher (car (directory-files dst-directory t "launch" t)))
  (find-file last-launcher)
  (re-search-forward "span monitors:i:1" nil t)
  (delete-char -1) (insert "0")
  (re-search-forward "use multimon:i:1" nil t)
  (delete-char -1) (insert "0")
  (re-search-forward "x56" nil t)
  (delete-char -1) (insert "1")
  (save-buffer)
  (sleep-for 1)
    (async-shell-command (concat "xfreerdp " last-launcher " /p:dstFall2019!") "*x56*")))
