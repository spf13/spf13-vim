;;; prelude-emacs-lisp.el --- Emacs Prelude: Nice config for Elisp programming.
;;
;; Copyright © 2011-2013 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience
;; Package-Requires: ((prelude-lisp "1.0.0"))

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
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))
            nil
            t))

(defun prelude-visit-ielm ()
  "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
  (interactive)
  (prelude-start-or-switch-to 'ielm "*ielm*"))

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'prelude-visit-ielm)

(defun prelude-conditional-emacs-lisp-checker ()
  "Don't check doc style in Emacs Lisp test files."
  (let ((file-name (buffer-file-name)))
    (when (and file-name (string-match-p ".*-tests?\\.el\\'" file-name))
      (setq-local flycheck-checkers '(emacs-lisp)))))

(defun prelude-emacs-lisp-mode-defaults ()
  "Sensible defaults for `emacs-lisp-mode'."
  (run-hooks 'prelude-lisp-coding-hook)
  (turn-on-eldoc-mode)
  (prelude-remove-elc-on-save)
  (rainbow-mode +1)
  (setq mode-name "EL")
  (prelude-conditional-emacs-lisp-checker))

(setq prelude-emacs-lisp-mode-hook 'prelude-emacs-lisp-mode-defaults)

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (run-hooks 'prelude-emacs-lisp-mode-hook)))

;; ielm is an interactive Emacs Lisp shell
(defun prelude-ielm-mode-defaults ()
  "Sensible defaults for `ielm'."
  (run-hooks 'prelude-interactive-lisp-coding-hook)
  (turn-on-eldoc-mode))

(setq prelude-ielm-mode-hook 'prelude-ielm-mode-defaults)

(add-hook 'ielm-mode-hook (lambda ()
                            (run-hooks 'prelude-ielm-mode-hook)))

(eval-after-load "elisp-slime-nav"
  '(diminish 'elisp-slime-nav-mode))
(eval-after-load "rainbow-mode"
  '(diminish 'rainbow-mode))
(eval-after-load "eldoc"
  '(diminish 'eldoc-mode))

;; enable elisp-slime-nav-mode
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

(provide 'prelude-emacs-lisp)

;;; prelude-emacs-lisp.el ends here
