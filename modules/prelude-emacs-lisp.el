;;; prelude-emacs-lisp.el --- Emacs Prelude: Nice config for Elisp programming.
;;
;; Copyright (c) 2011 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar.batsov@gmail.com>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/Prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Nice config for Elisp Programming.

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

(require 'prelude-lisp)

(defun prelude-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(defun prelude-emacs-lisp-mode-defaults ()
  (run-hooks 'prelude-lisp-coding-hook)
  (turn-on-eldoc-mode)
  (prelude-remove-elc-on-save)
  (rainbow-mode +1))

(setq prelude-emacs-lisp-mode-hook 'prelude-emacs-lisp-mode-defaults)

(add-hook 'emacs-lisp-mode-hook (lambda () (run-hooks 'prelude-emacs-lisp-mode-hook)))

;; ielm is an interactive Emacs Lisp shell
(defun prelude-ielm-mode-defaults ()
  (run-hooks 'prelude-interactive-lisp-coding-hook)
  (turn-on-eldoc-mode))

(setq prelude-ielm-mode-hook 'prelude-ielm-mode-defaults)

(add-hook 'ielm-mode-hook (lambda () (run-hooks 'prelude-ielm-mode-hook)))

(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

(provide 'prelude-emacs-lisp)

;;; prelude-emacs-lisp.el ends here
