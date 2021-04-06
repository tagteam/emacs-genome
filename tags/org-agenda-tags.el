;;; org-agenda-tags.el --- org custom commands used by tag

;; Copyright (C) 2013  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <tag@linuxifsv007>
;; Keywords: tools

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

;;{{{ agenda files
(setq org-agenda-files '("~/Memory/Calendar/org/Calendar.org"))
(setq org-agenda-show-all-dates t)
;; (setq org-agenda-files (list "~/research/OkDoc/" "~/epapers/Readme.org"))
;; (defun org-read-agenda-files ()
  ;; (interactive)
  ;; (setq org-agenda-files (directory-files "~/research/OkDoc" 'full org-agenda-file-regexp))
  ;; (setq org-agenda-files (directory-files "~/research/OkDoc" 'full org-agenda-file-regexp))
  ;; (add-to-list 'org-agenda-files "~/epapers/Readme.org"))
;; (org-read-agenda-files)

;;}}}
;;{{{ holidays
;; see variable calendar-holidays
(setq calendar-week-start-day 1)
(setq european-calendar-style 'european)
(setq calendar-mark-holidays-flag t)
(setq calendar-date-display-form
      '((if dayname
            (concat dayname ", "))
        day " " monthname " " year))
(setq calendar-time-display-form
      '(24-hours ":" minutes))


(setq holiday-general-holidays nil
      holiday-local-holidays nil
      holiday-christian-holidays nil
      holiday-hebrew-holidays nil
      holiday-islamic-holidays nil
      holiday-bahai-holidays nil
      holiday-oriental-holidays nil
      holiday-solar-holidays nil )

;; (setq calendar-day-name-array ["søndag" "mandag" "tirsdag" "onsdag" "torsdag" "fredag" "lørdag"])
;; (setq calendar-month-name-array ["januar" "februar" "marts" "april" "maj" "juni" "juli" "august" "september" "oktober" "november" "december"])
;; Calculation of easter, the fix point for many holidays (taken from
;; sv-kalender.el, originally from holiday-easter-etc)
(defun da-easter (year)
  "Calculate the date for Easter in YEAR."
  (let* ((century (1+ (/ year 100)))
         (shifted-epact (% (+ 14 (* 11 (% year 19))
                              (- (/ (* 3 century) 4))
                              (/ (+ 5 (* 8 century)) 25)
                              (* 30 century))
                           30))
         (adjusted-epact (if (or (= shifted-epact 0)
                                 (and (= shifted-epact 1)
                                      (< 10 (% year 19))))
                             (1+ shifted-epact)
                           shifted-epact))
         (paschal-moon (- (calendar-absolute-from-gregorian
                           (list 4 19 year))
                          adjusted-epact)))
    (calendar-dayname-on-or-before 0 (+ paschal-moon 7))))

(setq holiday-general-holidays
      (append holiday-general-holidays
	      '((holiday-fixed 1 1 "Nytårsdag")
		(holiday-fixed 1 6 "Hellige 3 konger")
		;; Easter and Pentecost
		(holiday-filter-visible-calendar
		 (mapcar
		  (lambda (dag)
		    (list (calendar-gregorian-from-absolute
			   (+ (da-easter displayed-year) (car dag)))
			  (cadr dag)))
		  '(( -49 "Fastelavn")
		    ( -7 "Palmesøndag")
		    ( -3 "Skærtorsdag")
		    ( -2 "Langfredag")
		    ( 0 "Påskedag")
		    ( +1 "Anden påskedag")
		    ( +26 "Store bededag")
		    ( +39 "Kristi himmelfartsdag")
		    ( +49 "Pinsedag")
		    ( +50 "Anden pinsedag"))))
		(holiday-fixed 12 24 "Juleaften")
		(holiday-fixed 12 25 "Juledag")
		(holiday-fixed 12 26 "Anden juledag")
		(holiday-fixed 12 31 "Nytårsaften"))))

(setq holiday-other-holidays
      (append holiday-other-holidays
	      '((holiday-fixed 3 8 "Kvindernes internationale kampdag")
		(holiday-fixed 5 1 "Arbejdernes internationale kampdag")
		(holiday-fixed 5 4 "Danmarks befrielse")
		(holiday-float 5 0 2 "Mors dag")
		(holiday-fixed 6 5 "Grundlovsdag")
		(holiday-fixed 6 5 "Fars dag")
		(holiday-fixed 6 15 "Valdemarsdag (Dannebrog)")
		(holiday-fixed 6 24 "Skt. Hans dag"))))

;; (setq holiday-other-holidays
      ;; (append holiday-other-holidays



;;}}}
;;{{{ Calendar
(defun Calendar ()
  (interactive)
  (let ((org-agenda-span 28)
	(org-agenda-sticky nil)
	(org-agenda-custom-commands 
	 '(("C" "Calendar"
	    ((agenda ""
		     ((org-agenda-files
		       '("~/Memory/Calendar/org/Calendar.org")))))
	    ((org-agenda-compact-blocks nil)
	     (org-agenda-show-all-dates nil)
	     (org-agenda-buffer-name (concat "*Calendar*"))
	     (org-agenda-window-setup 'current-window)
	     (org-agenda-overriding-header "Calendar"))))))
    (push ?C unread-command-events)
    (call-interactively 'org-agenda)))

;;}}}
;;{{{ cycle personal agendas
(setq org-agenda-sticky t)
(setq org-priority-faces '((?A . (:foreground "red" :weight bold))
			   (?B . (:foreground "orange" :weight bold ))
                           (?C . (:foreground "darkgreen"))))
(defun org ()
  (interactive)
  (superman-calendar)
  (forward-line 2))
  
(defun org-cycle-list ()
  (interactive)
  (let ((cur (car org-action-list))
	(rest (cdr org-action-list)))
    (setq org-action-list rest)
    (add-to-list 'org-action-list cur 'append)))
;;}}}
(provide 'org-agenda-tags)
;;; org-agenda-tags.el ends here


