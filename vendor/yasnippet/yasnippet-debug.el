;;; yasnippet-debug.el --- debug functions for yasnippet

;; Copyright (C) 2010  João Távora

;; Author: João Távora(defun yas/debug-snippet-vars () <joaotavora@gmail.com>
;; Keywords: emulations, convenience

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

;; Just some debug functions

;;; Code:

(require 'yasnippet)

(defun yas/debug-snippet-vars ()
  "Debug snippets, fields, mirrors and the `buffer-undo-list'."
  (interactive)
  (with-output-to-temp-buffer "*YASnippet trace*"
    (princ "Interesting YASnippet vars: \n\n")

    (princ (format "\nPost command hook: %s\n" post-command-hook))
    (princ (format "\nPre  command hook: %s\n" pre-command-hook))

    (princ (format "%s live snippets in total\n" (length (yas/snippets-at-point (quote all-snippets)))))
    (princ (format "%s overlays in buffer:\n\n" (length (overlays-in (point-min) (point-max)))))
    (princ (format "%s live snippets at point:\n\n" (length (yas/snippets-at-point))))


    (dolist (snippet (yas/snippets-at-point))
      (princ (format "\tsid: %d control overlay from %d to %d\n"
                     (yas/snippet-id snippet)
                     (overlay-start (yas/snippet-control-overlay snippet))
                     (overlay-end (yas/snippet-control-overlay snippet))))
      (princ (format "\tactive field: %d from %s to %s covering \"%s\"\n"
                     (yas/field-number (yas/snippet-active-field snippet))
                     (marker-position (yas/field-start (yas/snippet-active-field snippet)))
                     (marker-position (yas/field-end (yas/snippet-active-field snippet)))
                     (buffer-substring-no-properties (yas/field-start (yas/snippet-active-field snippet)) (yas/field-end (yas/snippet-active-field snippet)))))
      (when (yas/snippet-exit snippet)
        (princ (format "\tsnippet-exit: at %s next: %s\n"
                       (yas/exit-marker (yas/snippet-exit snippet))
                       (yas/exit-next (yas/snippet-exit snippet)))))
      (dolist (field (yas/snippet-fields snippet))
        (princ (format "\tfield: %d from %s to %s covering \"%s\" next: %s%s\n"
                       (yas/field-number field)
                       (marker-position (yas/field-start field))
                       (marker-position (yas/field-end field))
                       (buffer-substring-no-properties (yas/field-start field) (yas/field-end field))
                       (yas/debug-format-fom-concise (yas/field-next field))
                       (if (yas/field-parent-field field) "(has a parent)" "")))
        (dolist (mirror (yas/field-mirrors field))
          (princ (format "\t\tmirror: from %s to %s covering \"%s\" next: %s\n"
                         (marker-position (yas/mirror-start mirror))
                         (marker-position (yas/mirror-end mirror))
                         (buffer-substring-no-properties (yas/mirror-start mirror) (yas/mirror-end mirror))
                         (yas/debug-format-fom-concise (yas/mirror-next mirror)))))))

    (princ (format "\nUndo is %s and point-max is %s.\n"
                   (if (eq buffer-undo-list t)
                       "DISABLED"
                     "ENABLED")
                   (point-max)))
    (unless (eq buffer-undo-list t)
      (princ (format "Undpolist has %s elements. First 10 elements follow:\n" (length buffer-undo-list)))
      (let ((first-ten (subseq buffer-undo-list 0 19)))
        (dolist (undo-elem first-ten)
          (princ (format "%2s:  %s\n" (position undo-elem first-ten) (truncate-string-to-width (format "%s" undo-elem) 70))))))))

(defun yas/debug-format-fom-concise (fom)
  (when fom
    (cond ((yas/field-p fom)
           (format "field %d from %d to %d"
                   (yas/field-number fom)
                   (marker-position (yas/field-start fom))
                   (marker-position (yas/field-end fom))))
          ((yas/mirror-p fom)
           (format "mirror from %d to %d"
                   (marker-position (yas/mirror-start fom))
                   (marker-position (yas/mirror-end fom))))
          (t
           (format "snippet exit at %d"
                   (marker-position (yas/fom-start fom)))))))


(defun yas/exterminate-package ()
  (interactive)
  (yas/global-mode -1)
  (yas/minor-mode -1)
  (mapatoms #'(lambda (atom)
                (when (string-match "yas/" (symbol-name atom))
                  (unintern atom)))))

(defun yas/debug-test (&optional quiet)
  (interactive "P")
  (yas/load-directory (or (and (listp yas/snippet-dirs)
                               (first yas/snippet-dirs))
                          yas/snippet-dirs
                          "~/Source/yasnippet/snippets/"))
  (set-buffer (switch-to-buffer "*YAS TEST*"))
  (mapc #'yas/commit-snippet (yas/snippets-at-point 'all-snippets))
  (erase-buffer)
  (setq buffer-undo-list nil)
  (setq undo-in-progress nil)
  (snippet-mode)
  (yas/minor-mode 1)
  (let ((abbrev))
    (setq abbrev "$f")
    (insert abbrev))
  (unless quiet
    (add-hook 'post-command-hook 'yas/debug-snippet-vars 't 'local)))

(provide 'yasnippet-debug)
;;; yasnippet-debug.el ends here

