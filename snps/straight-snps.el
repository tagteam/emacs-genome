;;; straight-snps.el --- loading the straight package manager into the emacs-genome  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <tag@dub>
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

;;; Commentary:

;; since 2022 we are using the git based straight-package-manager
;; instead of emacs' build-in package-manager
;; https://jeffkreeftmeijer.com/emacs-straight-use-package/
;;
;; straight can be combined with use-package:
;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
;;
;; here we achieve that straight uses the emacs-genome
;; instead of the default .emacs.d

;;; Code:

(setq user-emacs-directory emacs-genome
      straight-base-dir emacs-genome)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil
      straight-use-package-by-default t
      straight-vc-git-default-protocol 'ssh)
(straight-use-package 'use-package)
(require 'use-package)
(setq use-package-enable-imenu-support t)
(setq use-package-ensure-function 'straight-use-package-ensure-function)

;; locate emacs packages to emacs-genome
(setq straight-base-dir emacs-genome)




(provide 'straight-snps)
;;; straight-snps.el ends here
