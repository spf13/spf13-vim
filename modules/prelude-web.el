;;; prelude-web.el --- Emacs Prelude: web template support
;;
;; Copyright © 2011-2013 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://www.batsov.com/emacs-prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for web-mode.

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

(prelude-ensure-module-deps '(web-mode))

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist
'("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))



(eval-after-load 'web-mode
  '(progn
     (defun prelude-web-mode-defaults ()
       ;; Disable whitespace-mode when using web-mode
       (whitespace-mode -1)
       ;; Customizations
       (setq web-mode-markup-indent-offset 4)
       (setq web-mode-css-indent-offset 2)
       (setq web-mode-code-indent-offset 4)
       (setq web-mode-disable-autocompletion t)
       (local-set-key (kbd "RET") 'newline-and-indent))
     (setq prelude-web-mode-hook 'prelude-web-mode-defaults)

     (add-hook 'web-mode-hook (lambda ()
                                 (run-hooks 'prelude-web-mode-hook)))))

(provide 'prelude-web)
;;; prelude-web.el ends here
