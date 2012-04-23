;;; prelude-packages.el --- Emacs Prelude: default package selection.
;;
;; Copyright (c) 2011-2012 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://batsov.com/emacs-prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Takes care of the automatic installation of all the packages required by
;; Emacs Prelude.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq url-http-attempt-keepalives nil)

(defvar prelude-packages
  '(ack-and-a-half expand-region gist helm helm-projectile magit magithub melpa
                   rainbow-mode volatile-highlights yasnippet zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun prelude-install-packages ()
  (unless (prelude-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs Prelude is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (dolist (p prelude-packages)
      (unless (package-installed-p p)
        (package-install p)))))

(prelude-install-packages)

(defmacro prelude-auto-install (ext mode)
  `(add-to-list 'auto-mode-alist
                `(,ext . (lambda ()
                           (package-install ',mode)
                           (,mode)))))

(defvar prelude-auto-install-alist
  '(("\\.markdown\\'" . markdown-mode)
    ("\\.md\\'" . markdown-mode)
    ("\\.haml\\'" . haml-mode)
    ("\\.scss\\'" . scss-mode)
    ("\\.sass\\'" . sass-mode)
    ("\\.groovy\\'" . groovy-mode)
    ("\\.yml\\'" . yaml-mode)
    ("\\.php\\'" . php-mode)
    ("\\.hs\\'" . haskell-mode)
    ("\\.less\\'" . less-css-mode)
    ("\\.lua\\'" . lua-mode)
    ("\\.coffee\\'" . coffee-mode)
    ("\\.erl\\'" . erlang-mode)
    ("\\.feature\\'" . feature-mode)))

;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually if it's already installed
(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(dolist (entry prelude-auto-install-alist)
  (let ((ext (car entry))
        (mode (cdr entry)))
    (unless (package-installed-p mode)
      (prelude-auto-install ext mode))))

(provide 'prelude-packages)
;;; prelude-packages.el ends here
