;; dst user x56 password uuu888UUU (rrr888RRR) (vCH9byRp)
;; fjernskrivebord 7389, computer:DS7217 

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
(setq dst-domain-user-pos "1021 318"
      dst-current-password-pos "1021 344"
      dst-new-password-pos "1021 375"
      dst-confirm-password-pos "1021 402"
      dst-submit-button-pos "1063 444")

(setq dst-change-password-pos "1242 332")
;;(setq dst-change-password-pos "750 240")
(setq dst-servers '(("FSE Windows" . "391 319")
		    ("srvfseatru1" .  "134 300")
		    ("srvfsecancer1" .  "424 478")
		    ("srvfsecancer2" .  "685 478")
		    ("srvfsegh4" .  "1226 478")
		    ("srvfsegh5" .  "1515 478")
		    ("srvfsegh6" .  "158 599")))

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
  ;; (shell-command (concat "xfreerdp " last-launcher " /p:uuu777UUU"))))
;; " /p:uuu777UUU"))))
;; uuu777UUU
;; dstFall2019!
;; rrr888RRR!!!

(defun dst-type-password ()
  (interactive)
  (let ((xfree-win (shell-command-to-string "xdotool search --onlyvisible --name 'xfree'")))
  (shell-command
   (concat
   "xdotool windowraise " xfree-win " mousemove --sync --window " xfree-win " " 
   "xdotool type u7_b9_!!_pP;"
   "xdotool key Return;"
))))


;; WJA: 6632
;; WJAxxxx@dstfse.local
;; (mapcar 'cadr dst-login-list)
;; paste0("70",c("3678" "3775" "3573" "3826" "4791" "3661" "3662" "3657" "6130" "6322" "3716" "6818" "6582" "6076" "6734" "3740" "3607" "7239" "4432" "4265" "6220"))
;; sampa921G!
;; Gsampa7713G!
;; Ba!styrko20
;; nuEr!dEnD3r
;; m!au537PIF
;; u7_b9_!!_pP
;; Yui89!#!iuY
;; bla12BLUB!!
;; BLUB87BluB!!
(setq dst-login-list
      '(("3678 Thomas Sehested" "3678"  "bla999BLA!!!" "srvfsegh4")
	("3775 Marcella, Regitze" "3775"  "bla999BLA!!!" "srvfsegh4")
	("3573 Overall meta project" "3573"  "bla999BLA!!!" "srvfsegh4")
	("3826 Liv" "3826"  "bla999BLA!!!" "srvfsegh4")
	("4791 Brice, Peter, Anders Nissen, CJ Lee, Anne, MortenLamberts" "4791" "bla999BLA!!!" "srvfsegh4")
	;; ("4778 Gro Askgaard Alcohol" "4778" "NA" "srvfsegh1")
	("3661 Sidsel, Daniel, Carolina, Steen, Kristian, Shahzleen, Carlo" "3661" "bla999BLA!!!" "srvfsegh4")
	("7958 Nicklas Vinter aalborg" "7958" "Gsampa7713G!" "srvfseatru1")
	("3662 Mitroflow" "3662" "bla999BLA!!!" "srvfsegh4")
	("3657 Louise" "3657" "bla999BLA!!!" "srvfsegh4")
	("6130 Maria D'Souza" "6130" "bla999BLA!!!" "srvfsegh4")
	("6322 Julie Andersen" "6322" "bla999BLA!!!" "srvfsegh4")
	("3716 Julie Andersen" "3716" "bla999BLA!!!" "srvfsegh4")
	("6818 Alexander Falkentoft, deepthi, novo, Jarl, Jannik, Caroline, Christina Lee" "6818" "bla999BLA!!!" "srvfsegh4")
	("6582 Corona, Covid, Mavish, Grimur" "6582" "bla999BLA!!!" "srvfsegh4")
	("6359 Eva Havers-Borgersen" "6359" "bla999BLA!!!" "srvfsegh4") 
	("6076 Peter Enemark Aalborg" "6076" "bla999BLA!!!" "srvfsegh4")
	("6734 Genetics Maria" "6734" "bla999BLA!!!" "srvfsegh4")
	("6502 Ano HF" "6502" "bla999BLA!!!" "srvfsegh4")
	("3740 Mariam, Henrik, Laura, Morten Malmborg" "3740" "bla999BLA!!!" "srvfsegh4")
 	("3607 Jonas L. Isaksen, Jonas Bille Nielsen, Morten Wagner" "3607" "bla999BLA!!!" "FSE Windows")
	("4265 Kessing" "4265" "bla999BLA!!!" "FSE Windows")
	("6220 RWAS" "6220" "alb777ALB!!!" "FSE Windows")))
;;	("7239 Cancer Ole Esben Marie Measurement error" "7239" "bla999BLA!!!" "srvfsecancer2")
;;	("4432 Anne Lyngholm, Mette Friberg Hitz, Jacob Holm" "4432" "bla999BLA!!!" "FSE Windows")
;;	7239
;;zAq!12345

