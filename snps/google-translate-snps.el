;;; google-translate-snps.el --- short cuts to google translate

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
(require 'thingatpt)
(require 'auto-dictionary)
(require 'google-translate-mode)

(defvar google-translate-languages '("en" "fr" "es" "de")
  "List of translation languages. See `google-translate-language-alist'.")

;; see https://sites.google.com/site/tomihasa/google-language-codes
(defvar google-translate-language-alist '(("af" . "Afrikaans")
					  ("ak" . "Akan")
					  ("sq" . "Albanian")
					  ("am" . "Amharic")
					  ("ar" . "Arabic")
					  ("hy" . "Armenian")
					  ("az" . "Azerbaijani")
					  ("eu" . "Basque")
					  ("be" . "Belarusian")
					  ("bem" . "Bemba")
					  ("bn" . "Bengali")
					  ("bh" . "Bihari")
					  ("bs" . "Bosnian")
					  ("br" . "Breton")
					  ("bg" . "Bulgarian")
					  ("km" . "Cambodian")
					  ("ca" . "Catalan")
					  ("chr" . "Cherokee")
					  ("ny" . "Chichewa")
					  ("zh" . "Chinese")
					  ("co" . "Corsican")
					  ("hr" . "Croatian")
					  ("cs" . "Czech")
					  ("da" . "Danish")
					  ("nl" . "Dutch")
					  ("en" . "English")
					  ("eo" . "Esperanto")
					  ("et" . "Estonian")
					  ("ee" . "Ewe")
					  ("fo" . "Faroese")
					  ("tl" . "Filipino")
					  ("fi" . "Finnish")
					  ("fr" . "French")
					  ("fy" . "Frisian")
					  ("gaa" . "Ga")
					  ("gl" . "Galician")
					  ("ka" . "Georgian")
					  ("de" . "German")
					  ("el" . "Greek")
					  ("gn" . "Guarani")
					  ("gu" . "Gujarati")
					  ("ht" . "Haitian")
					  ("ha" . "Hausa")
					  ("haw" . "Hawaiian")
					  ("iw" . "Hebrew")
					  ("hi" . "Hindi")
					  ("hu" . "Hungarian")
					  ("is" . "Icelandic")
					  ("ig" . "Igbo")
					  ("id" . "Indonesian")
					  ("ia" . "Interlingua")
					  ("ga" . "Irish")
					  ("it" . "Italian")
					  ("ja" . "Japanese")
					  ("jw" . "Javanese")
					  ("kn" . "Kannada")
					  ("kk" . "Kazakh")
					  ("rw" . "Kinyarwanda")
					  ("rn" . "Kirundi")
					  ("xx" . "-klingon")
					  ("kg" . "Kongo")
					  ("ko" . "Korean")
					  ("kri" . "Krio")
					  ("ku" . "Kurdish")
					  ("ckb" . "Kurdish")
					  ("ky" . "Kyrgyz")
					  ("lo" . "Laothian")
					  ("la" . "Latin")
					  ("lv" . "Latvian")
					  ("ln" . "Lingala")
					  ("lt" . "Lithuanian")
					  ("loz" . "Lozi")
					  ("lg" . "Luganda")
					  ("ach" . "Luo")
					  ("mk" . "Macedonian")
					  ("mg" . "Malagasy")
					  ("ms" . "Malay")
					  ("ml" . "Malayalam")
					  ("mt" . "Maltese")
					  ("mi" . "Maori")
					  ("mr" . "Marathi")
					  ("mfe" . "Mauritian")
					  ("mo" . "Moldavian")
					  ("mn" . "Mongolian")
					  ("sr" . "Montenegrin")
					  ("ne" . "Nepali")
					  ("pcm" . "Nigerian")
					  ("nso" . "Northern")
					  ("no" . "Norwegian")
					  ("nn" . "Norwegian")
					  ("oc" . "Occitan")
					  ("or" . "Oriya")
					  ("om" . "Oromo")
					  ("ps" . "Pashto")
					  ("fa" . "Persian")
					  ("pl" . "Polish")
					  ("pt" . "Portuguese")
					  ("pa" . "Punjabi")
					  ("qu" . "Quechua")
					  ("ro" . "Romanian")
					  ("rm" . "Romansh")
					  ("nyn" . "Runyakitara")
					  ("ru" . "Russian")
					  ("gd" . "Scots")
					  ("sr" . "Serbian")
					  ("sh" . "Serbo-Croatian")
					  ("st" . "Sesotho")
					  ("tn" . "Setswana")
					  ("crs" . "Seychellois")
					  ("sn" . "Shona")
					  ("sd" . "Sindhi")
					  ("si" . "Sinhalese")
					  ("sk" . "Slovak")
					  ("sl" . "Slovenian")
					  ("so" . "Somali")
					  ("es" . "Spanish")
					  ("su" . "Sundanese")
					  ("sw" . "Swahili")
					  ("sv" . "Swedish")
					  ("tg" . "Tajik")
					  ("ta" . "Tamil")
					  ("tt" . "Tatar")
					  ("te" . "Telugu")
					  ("th" . "Thai")
					  ("ti" . "Tigrinya")
					  ("to" . "Tonga")
					  ("lua" . "Tshiluba")
					  ("tum" . "Tumbuka")
					  ("tr" . "Turkish")
					  ("tk" . "Turkmen")
					  ("tw" . "Twi")
					  ("ug" . "Uighur")
					  ("uk" . "Ukrainian")
					  ("ur" . "Urdu")
					  ("uz" . "Uzbek")
					  ("vi" . "Vietnamese")
					  ("cy" . "Welsh")
					  ("wo" . "Wolof")
					  ("xh" . "Xhosa")
					  ("yi" . "Yiddish")
					  ("yo" . "Yoruba")
					  ("zu" . "Zulu")))


(defun google-translate (&optional source target text)
  (interactive)
  (let* ((beg (when (region-active-p) (region-beginning)))
	 (default (if beg (buffer-substring-no-properties beg  (region-end))))
	 (langs google-translate-languages)
	 (source (or source (adict-guess-dictionary)))
	 (target (or target (cadr (member source langs)) (car langs)))
	 (text (or text (read-string "google translate: "
				     (or default (thing-at-point 'word))))))
    (pop-to-buffer "*Google Translate Results*")
    (setq buffer-read-only t)
    (let ((buffer-read-only nil)
	  (header-line ""))
      (erase-buffer)
      (insert (trans t source target text))
      (recode-region  (point-min)  (point-max) 'utf-8 'utf-8)
      (goto-char (point-min))
      (let ((abbr-text (replace-regexp-in-string "\n" " " text)))
	(insert "Google translate query: " (substring abbr-text 0 (min 80 (length abbr-text))) " (press n for new query)\n\n"))
      (setq header-line
	    (concat header-line " "
		    (header-button-format
		     (concat "Source: " (cdr (assoc source google-translate-language-alist))" (press s to change)")
		     :action
		     `(lambda (&optional arg) (interactive) (google-translate-toggle-source-language)))
		    "\t"
		    (header-button-format
		     (concat "Target: " (cdr (assoc target google-translate-language-alist))" (press t to change)")
		     :action
		     `(lambda (&optional arg) (interactive) (google-translate-toggle-target-language)))))
      (google-translate-mode-on)
      (put-text-property (point-min) (1+ (point-min)) 'query text)
      (put-text-property (point-min) (1+ (point-min)) 'source source)
      (put-text-property (point-min) (1+ (point-min)) 'target target)
      (end-of-line)
      (org-set-local 'header-line-format header-line))))


(defun google-translate-toggle-source-language ()
  (interactive)
  (let* ((source (get-text-property (point-min) 'source))
	 (target (get-text-property (point-min) 'target))
	 (query (get-text-property (point-min) 'query))
	 (langs (copy-seq google-translate-languages))
	 (sources (delete target langs))
	 (rest (member source sources))
	 (next (if (> (length rest) 1) (cadr rest) (car sources))))
    (google-translate next target query)))

(defun google-translate-toggle-target-language ()
  (interactive)
  (let* ((source (get-text-property (point-min) 'source))
	 (target (get-text-property (point-min) 'target))
	 (query (get-text-property (point-min) 'query))
	 (langs (copy-seq google-translate-languages))
	 (targets (delete source langs))
	 (rest (member target targets))
	 (next (if (> (length rest) 1) (cadr rest) (car targets))))
    (google-translate source next query)))

(defun google-translate-new-query ()
  (interactive)
  (let* ((source (get-text-property (point-min) 'source))
	 (target (get-text-property (point-min) 'target)))
    (google-translate source target nil)))

;;{{{ Minor mode

(defvar google-translate-mode-map (make-sparse-keymap)
  "Keymap used for `google-translate-mode' commands.")
   
(define-minor-mode google-translate-mode
     "Toggle superman project view mode.
With argument ARG turn google-translate-mode on if ARG is positive, otherwise
turn it off.
                   
Enabling google-translate mode electrifies the column view for documents
for git and other actions like commit, history search and pretty log-view."
     :lighter " *google*"
     :group 'org
     :keymap 'google-translate-mode-map)

(defun google-translate-mode-on ()
  (interactive)
  (google-translate-mode t))

;; (define-key google-translate-mode-map "t" 'google-translate-toggle-language)
(define-key google-translate-mode-map "n" 'google-translate-new-query)
(define-key google-translate-mode-map "t" 'google-translate-toggle-target-language)
(define-key google-translate-mode-map "s" 'google-translate-toggle-source-language)

;;}}}


(provide 'google-translate-snps)
;;; google-translate-snps.el ends here
