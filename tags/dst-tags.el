;; -*- lexical-binding: t; -*-
;; codex resume 01a056a4-39ac-7090-8c80-039d4dc88f67
(setq dst-directory "~/dst")
(setq dst-window-size '("1900" "1050"))
(setq dst-ident "WJA")
(setq dst-firewall-code "759927")


(defun dst-xfree-window ()
  (let ((cwin (shell-command-to-string "xdotool search --class 'xfreerdp'")))
    (if (string= cwin "") nil
      (replace-regexp-in-string "\n" "" cwin))))
(defun dst-raise-dst ()
  (interactive)
  (shell-command-to-string (concat "xdotool windowraise " (dst-xfree-window) " mousemove --sync --window " (dst-xfree-window) " " dst-click-position
				     ";xdotool click 1;")))

;; for windows server where screen is double locked
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



;; WJAxxxx@dstfse.local
(setq dst-pw "mAcs4bb77miC99!")

(setq dst-login-list
      `(
	("707655 Steno Main" "707655" ,dst-pw "Windows")
	("710746 Holbaek Fie" "710746" "88mAcs4!bb78miC" "Windows")
	("709873 DMreg-projekt" "709873" ,dst-pw "Windows")
	("3740 Diabetes server 4" "3740" ,dst-pw "srvfsegh5")
	("6582 Corona, Covid, Mavish, Grimur" "706582" ,dst-pw "srvfsegh5")
	("6220 RWAS" "6220" ,dst-pw "Windows")
	("3775 Hannah, Marcella, Regitze" "3775"  ,dst-pw "srvfsegh4")
	("707376 Filip" "707376"  "779BLUBbla!!" "srvfsegh4")
	("706818 Alexander Falkentoft, deepthi, novo, Jarl, Jannik, Caroline, Christina Lee" "706818" ,dst-pw "srvfsegh4")
	("6322 Marianna Hjertetal" "6322" ,dst-pw "srvfsegh4")
	("9200 Statins Caecilie Denholt Stilling Michaela/Helene" "709200" ,dst-pw "srvfsegh4")
	("4265 Kessing" "4265" ,dst-pw "Windows")
	("7535 Mie Balling" "707535" ,dst-pw "Windows")
	("706338 Mathias" "706338" ,dst-pw "srvfsegh4")
	("7352 Simon-Kessing" "707352" ,dst-pw "Windows")
	("9281 Dea Rasmus-Steno CKD" "709281" ,dst-pw  "srvfsegh4")
	("707089 Deepthi" "707089" ,dst-pw "srvfsegh4")
	("709924 Steno NF report" "709924" ,dst-pw "Windows")
	("708406 Oebus" "708406" ,dst-pw "Windows")
	))

;; ("3573 Overall meta project" "3573"  ,dst-pw "srvfsegh4")
(setq dst-login-list
      `(
	("707655 Steno Main" "707655" "Windows")
	("710746 Holbaek Fie" "710746"  "Windows")
	("709873 DMreg-projekt" "709873" "Windows")
	("3740 Diabetes server 4" "3740" "srvfsegh5")
	("6582 Corona, Covid, Mavish, Grimur" "706582" "srvfsegh5")
	("6220 RWAS" "6220" "Windows")
	("3775 Hannah, Marcella, Regitze" "3775"  "srvfsegh4")
	("707376 Filip" "707376"  "srvfsegh4")
	("706818 Alexander Falkentoft, deepthi, novo, Jarl, Jannik, Caroline, Christina Lee" "706818" "srvfsegh4")
	("6322 Marianna Hjertetal" "6322" "srvfsegh4")
	("9200 Statins Caecilie Denholt Stilling Michaela/Helene" "709200" "srvfsegh4")
	("4265 Kessing" "4265" "Windows")
	("7535 Mie Balling" "707535" "Windows")
	("706338 Mathias" "706338" "srvfsegh4")
	("7352 Simon-Kessing" "707352" "Windows")
	("9281 Dea Rasmus-Steno CKD" "709281"  "srvfsegh4")
	("707089 Deepthi" "707089" "srvfsegh4")
	("709924 Steno NF report" "709924" "Windows")
	("708406 Oebus" "708406" "Windows")
	))


