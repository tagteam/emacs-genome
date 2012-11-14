;;; anything-recoll-snps.el --- combine anything with recoll

;; Copyright (C) 2012  Thomas Alexander Gerds

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


;;; Code:






(add-to-list 'load-path (concat (getenv "HOME") "/emacs-genome/genes/anything-config"))
(try-require 'anything)
(try-require 'anything-config)

;;; Recoll (xapian) text indexing engine plugin, derived from anything-c-source-locate

;; (defvar anything-c-recoll-options
  ;; '("recoll" "-t" "-b")
  ;; "A list where the `car' is the name of the recoll program followed by options.")

(defvar anything-c-source-recoll
  '((name . "Recoll")
    (candidates . (lambda ()
                    (apply 'start-process "recoll-process" nil
                           (append anything-c-recoll-options
                                   (list anything-pattern)))))
    (candidate-transformer
     . (lambda (cs)
	 (mapcar (function (lambda (c)
			     (replace-regexp-in-string "file://" "" c)))
		 cs)))

    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files matching the current input pattern with recoll.")
;; (anything 'anything-c-source-recoll)


(defvar anything-c-recoll-options
  '("recoll" "-t" "-b") 
  "A list where the `car' is the name of the recoll program followed by options.
You do not need to include the -c option since this is already included, and the config directory
can be passed as a argument to `anything-c-source-recoll'")

(defun anything-recoll-search ()
  (interactive)
 (anything 'anything-c-source-recoll))

(defun anything-c-source-recoll (name confdir)
  "Function to create anything source for recoll search results.
The source variable will be named `anything-c-source-recoll-NAME' where NAME is the first arg to the function
 (and should be a valid symbol name - i.e. no spaces).
The CONFDIR arg should be a string indicating the path to the config directory which recoll should use."
  (eval
   `(defvar ,(intern (concat "anything-c-source-recoll-" name))
      '((name . ,(concat "Recoll " name))
        (candidates . (lambda ()
                        (apply 'start-process "recoll-process" nil 
                               (append anything-c-recoll-options
                                       '("-c" ,confdir)
                                       (list anything-pattern)))))
        (candidate-transformer
         . (lambda (cs)
             (mapcar (function (lambda (c)
                                 (replace-regexp-in-string "file://" "" c)))
                     cs)))
        (type . file)
        (requires-pattern . 3)
        (delayed))
      ,(concat "Source for retrieving files matching the current input pattern, using recoll with the configuration in "
               confdir))))

 
;(anything-c-source-recoll "reports" "~/.recoll_reports")
(provide 'anything-recoll-snps)
;;; anything-recoll-snps.el ends here