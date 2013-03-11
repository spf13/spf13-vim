;;; prelude-core.el --- Emacs Prelude: Core Prelude functions.
;;
;; Copyright © 2011-2013 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Here are the definitions of most of the functions added by Prelude.

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

(require 'thingatpt)

(defun prelude-open-with ()
  "Open visited file in external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      (read-shell-command "Open current file with: "))
                    " "
                    buffer-file-name))))

(defun prelude-buffer-mode (buffer-or-name)
  "Retrieve the `major-mode' of BUFFER-OR-NAME."
  (with-current-buffer buffer-or-name major-mode))

(defun prelude-visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))

(defun prelude-google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
       (read-string "Google: "))))))

(defun prelude-indent-rigidly-and-copy-to-clipboard (begin end arg)
  "Indent region between BEGIN and END by ARG columns and copy to clipboard."
  (interactive "r\nP")
  (let ((arg (or arg 4))
        (buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring-no-properties buffer begin end)
      (indent-rigidly (point-min) (point-max) arg)
      (clipboard-kill-ring-save (point-min) (point-max)))))

(defun prelude-insert-empty-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (open-line 1)
  (forward-line 1)
  (indent-according-to-mode))

(defun prelude-move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun prelude-move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun prelude-indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun prelude-indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (prelude-indent-buffer)
        (message "Indented buffer.")))))

(defun prelude-annotate-todo ()
  "Put fringe marker on TODO: lines in the curent buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "TODO:" nil t)
      (let ((overlay (make-overlay (- (point) 5) (point))))
        (overlay-put overlay
                     'before-string
                     (propertize (format "A")
                                 'display '(left-fringe right-triangle)))))))

(defun prelude-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun prelude-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (-dotimes arg
                (lambda (n)
                  (goto-char end)
                  (newline)
                  (insert region)
                  (setq end (point))))
      (goto-char (+ origin (* (length region) arg) arg)))))

;; TODO doesn't work with uniquify
(defun prelude-rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (message "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file name new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)))))))

(defun prelude-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (delete-file filename)
      (message "Deleted file %s" filename)))
  (kill-buffer))

(defun prelude-view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    ;; TODO: switch to nxml/nxhtml mode
    (cond ((search-forward "<?xml" nil t) (xml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

(defun prelude-untabify-buffer ()
  "Remove all tabs from the current buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun prelude-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (prelude-indent-buffer)
  (prelude-untabify-buffer)
  (whitespace-cleanup))

(defun prelude-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun prelude-recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory prelude-dir 0))

(defun prelude-sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun prelude-switch-or-start (function buffer)
  "If BUFFER is current, bury it, otherwise invoke FUNCTION."
  (if (equal (buffer-name (current-buffer)) buffer)
      (bury-buffer)
    (if (get-buffer buffer)
        (switch-to-buffer buffer)
      (funcall function))))

(defun prelude-insert-date ()
  "Insert a timestamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun prelude-conditionally-enable-paredit-mode ()
  "Enable `paredit-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))

(add-hook 'minibuffer-setup-hook 'prelude-conditionally-enable-paredit-mode)

(defun prelude-recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun prelude-swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (if (/= (count-windows) 2)
      (message "You need exactly 2 windows to do this.")
    (let* ((w1 (car (window-list)))
           (w2 (cadr (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))
  (other-window 1))

(defun prelude-kill-other-buffers ()
  "Kill all buffers but the current one.
Doesn't mess with special buffers."
  (interactive)
  (-each
   (->> (buffer-list)
     (-filter #'buffer-file-name)
     (--remove (eql (current-buffer) it)))
   #'kill-buffer))

(defun prelude-create-scratch-buffer ()
  "Create a new scratch buffer."
  (interactive)
  (progn
    (switch-to-buffer
     (get-buffer-create (generate-new-buffer-name "*scratch*")))
    (emacs-lisp-mode)))

(defvar prelude-tips
  '("Press <C-c o> to open a file with external program."
    "Press <C-c p f> to navigate a project's files with ido."
    "Press <C-c h> to navigate a project in Helm."
    "Press <C-c g> to search in Google."
    "Press <C-c r> to rename the current buffer and file it's visiting."
    "Press <C-c t> to open a terminal in Emacs."
    "Explore the Prelude menu to find out about some of Prelude extensions to Emacs."
    "Access the official Emacs manual by pressing <C-h r>."
    "Visit WikEmacs at http://wikemacs.org to find out even more about Emacs."))

(defun prelude-tip-of-the-day ()
  "Display a random entry from `prelude-tips'."
  (interactive)
  ;; pick a new random seed
  (random t)
  (message
   (concat "Prelude tip: " (nth (random (length prelude-tips)) prelude-tips))))

(defun prelude-eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.

    If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

(defun prelude-exchange-point-and-mark ()
  "Identical to `exchange-point-and-mark' but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(defun prelude-update ()
  "Update Prelude to its latest version."
  (interactive)
  (when (y-or-n-p "Do you want to update Prelude?")
    (message "Updating Prelude...")
    (cd prelude-dir)
    (shell-command "git pull")
    (message "Update finished. Restart Emacs to complete the process.")))

(provide 'prelude-core)
;;; prelude-core.el ends here
