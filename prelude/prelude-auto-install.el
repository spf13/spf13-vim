;;; prelude-auto-install.el --- Emacs Prelude: auto-install required packages
;;
;; Copyright (c) 2011-2012 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://batsov.com/emacs-prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A simple mechanism to automatically install missing packages.

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


(defmacro prelude-auto-install (ext mode)
  `(unless (fboundp ',mode)
     (add-to-list 'auto-mode-alist
                  `(,ext . (lambda ()
                            (package-install ',mode)
                            (,mode))))))

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
    ("\\.coffee\\'" . coffee-mode)
    ("\\.lua\\'" . lua-mode)))

;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually if it's already installed
(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(dolist (entry prelude-auto-install-alist)
  (let ((ext (car entry))
        (mode (cdr entry)))
   (prelude-auto-install ext mode)))

(provide 'prelude-auto-install)
;;; prelude-auto-install.el ends here
