;;; prelude-mode.el --- Emacs Prelude: minor mode
;;
;; Copyright (c) 2011 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar.batsov@gmail.com>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/Prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A minor mode defining a local keymap, plus a menu.

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

(defvar prelude-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-p o") 'prelude-open-with)
    (define-key map (kbd "C-c C-p g") 'prelude-google)
    ;; mimic popular IDEs binding, note that it doesn't work in a terminal session
    (define-key map [(shift return)] 'prelude-insert-empty-line)
    (define-key map [(control shift up)]  'prelude-move-line-up)
    (define-key map [(control shift down)]  'prelude-move-line-down)
    (define-key map (kbd "C-c C-p n") 'prelude-cleanup-buffer)
    (define-key map (kbd "C-c C-p f")  'prelude-recentf-ido-find-file)
    (define-key map (kbd "C-c C-p i") 'prelude-ido-goto-symbol)
    (define-key map (kbd "C-M-\\") 'prelude-indent-region-or-buffer)
    (define-key map (kbd "C-c C-p h") 'prelude-view-url)
    (define-key map (kbd "C-c C-p e") 'prelude-eval-and-replace)
    (define-key map (kbd "C-c C-p s") 'prelude-swap-windows)
    (define-key map (kbd "C-c C-p d") 'prelude-duplicate-current-line-or-region)
    (define-key map (kbd "C-c C-p r") 'prelude-rename-file-and-buffer)
    (define-key map (kbd "C-c C-p t") 'prelude-visit-term-buffer)
    (define-key map (kbd "C-c C-p k") 'prelude-kill-other-buffers)
    map)
  "Keymap for Prelude mode.")

(easy-menu-define prelude-mode-menu prelude-mode-map
  "Menu for Prelude mode"
  '("Prelude"
    ("Files"
     ["Open with..." prelude-open-with]
     ["Delete file and buffer" prelude-delete-file-and-buffer]
     ["Rename file and buffer" prelude-rename-file-and-buffer]
     ["Copy file name to clipboard" prelude-copy-file-name-to-clipboard])

    ("Buffers"
     ["Clean up buffer" prelude-cleanup-buffer]
     ["Kill other buffers" prelude-kill-other-buffers])

    ("Editing"
     ["Insert empty line" prelude-insert-empty-line]
     ["Move line up" prelude-move-line-up]
     ["Move line down" prelude-move-line-down]
     ["Indent buffer" prelude-indent-buffer]
     ["Indent buffer or region" prelude-indent-buffer-or-region]
     ["Duplicate line or region" prelude-duplicate-current-line-or-region]
     ["Copy to clipboard as blockquote" prelude-indent-blockquote-and-copy-to-clipboard]
     ["Copy to clipboard as nested blockqoute" prelude-indent-nested-blockquote-and-copy-to-clipboard]
     ["Insert date" prelude-insert-date]
     ["Eval and replace" prelude-eval-and-replace])

    ("Navigation"
     ["Enable arrow keys" prelude-restore-arrow-keys]
     ["Go to symbol" prelude-ido-goto-symbol])

    ("Windows"
     ["Swap windows" prelude-swap-windows])

    ("General"
     ["Visit term buffer" prelude-visit-term-buffer]
     ["Search in Google" prelude-google]
     ["View URL" prelude-view-url])))

;; define minor mode
(define-globalized-minor-mode prelude-global-mode prelude-mode prelude-on)

(defun prelude-on ()
  (prelude-mode t))

(defun prelude-off ()
  (easy-menu-remove))

(define-minor-mode prelude-mode
  "Minor mode to consolidate Emacs Prelude extensions."
  :lighter "Prelude"
  :keymap prelude-mode-map
  (if prelude-mode
      ;; on start
      (easy-menu-add prelude-mode-menu prelude-mode-map)
    ;; on stop
    (prelude-off)))

(provide 'prelude-mode)
;;; prelude-mode.el ends here
