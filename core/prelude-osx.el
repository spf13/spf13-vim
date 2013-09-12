;;; prelude-osx.el --- Emacs Prelude: OSX specific settings.
;;
;; Copyright © 2011-2013 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some OSX specific stuff.

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

;; On OS X Emacs doesn't use the shell PATH if it's not started from
;; the shell. Let's fix that:
(prelude-ensure-module-deps '(exec-path-from-shell))
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; It's all in the Meta
(setq ns-function-modifier 'hyper)

(defun prelude-swap-meta-and-super ()
  "Swap the mapping of Meta and Super.
Very useful for people using their Mac with a
Windows external keyboard from time to time."
  (interactive)
  (if (eq mac-command-modifier 'super)
      (progn
        (setq mac-command-modifier 'meta)
        (setq mac-option-modifier 'super)
        (message "Command is now bound to META and Option is bound to SUPER."))
    (progn
      (setq mac-command-modifier 'super)
      (setq mac-option-modifier 'meta)
      (message "Command is now bound to SUPER and Option is bound to META."))))

(define-key prelude-mode-map (kbd "C-c w") 'prelude-swap-meta-and-super)
(define-key prelude-mode-map (kbd "s-/") 'hippie-expand)

(menu-bar-mode +1)

(provide 'prelude-osx)
;;; prelude-osx.el ends here
