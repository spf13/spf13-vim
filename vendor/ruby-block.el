;;; ruby-block.el --- highlight matching block

;; Copyright (C) 2007-2009  khiker

;; Author: khiker <khiker.mail+elisp@gmail.com>
;; Keywords: languages, faces, ruby

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Usage:

;; Add this line to your .emacs
;;
;; (require 'ruby-block)
;; (ruby-block-mode t)
;;
;; In addition, you can also add this line too.
;;
;; ;; do overlay
;; (setq ruby-block-highlight-toggle 'overlay)
;; ;; display to minibuffer
;; (setq ruby-block-highlight-toggle 'minibuffer)
;; ;; display to minibuffer and do overlay
;; (setq ruby-block-highlight-toggle t)
;;
;; Default is minibuffer.
;;
;; Tested on Emacs 22.3.2 and Emacs 23.0.95.2.

;;; Note:

;; A ruby-mode.el is necessary to use this package.

;;; Code:

(require 'ruby-mode)

;; Variables:

(defconst ruby-block-version "0.0.8"
  "Ruby block package version.")

(defconst ruby-block-keyword-list
  (list "end" "for" "while" "until" "if" "class" "module"
        "case" "unless" "def" "begin" "do")
  "Keyword for highlighting.")

(defconst ruby-block-keyword-regex
  "\\(end\\|for\\|while\\|until\\|if\\|class\\|module\\|case\\|unless\\|def\\|begin\\|do\\)"
  "Rregular expression to look for correspondence.")

(defgroup ruby-block nil
  "Ruby block"
  :tag "Ruby block"
  :group 'ruby-block)

(defcustom ruby-block-delay 0.50
  "*Time in seconds to delay before showing a matching paren."
  :type  'number
  :group 'ruby-block)

(defcustom ruby-block-highlight-face 'highlight
  "*Face for block highlighting."
  :type  'face
  :group 'ruby-block)

(defcustom ruby-block-highlight-toggle 'minibuffer
  "*How do you display corresponding line.
Default is minibuffer. display to minibuffer.

The possible choice is as follows.

nil        => nothing
minibuffer => minibuffer
overlay    => overlay
t          => minibuffer and overlay"
  :type  '(choice (const :tag "nothing" nil)
                  (const :tag "minibuffer" minibuffer)
                  (const :tag "overlay" overlay)
                  (const :tag "minibuffer and overlay" t))
  :group 'ruby-block)

(defvar ruby-block-timer nil)

(defvar ruby-block-highlight-overlay nil)


;; Functions:

(define-minor-mode ruby-block-mode
  "In ruby-mode, Displays the line where there is keyword corresponding
to END keyword. this is Minor mode for ruby-mode only."
  :init-value t
  :global nil
  :keymap nil
  :lighter " RBlock"
  (if ruby-block-mode
      (ruby-block-start-timer)
    (ruby-block-stop-timer)))

(defun ruby-block-start-timer ()
  "start timer."
  (when ruby-block-timer
    (cancel-timer ruby-block-timer))
  (setq ruby-block-timer
        (run-with-idle-timer ruby-block-delay t 'ruby-block-hook)))

(defun ruby-block-stop-timer ()
  "stop timer."
  (when ruby-block-timer
    (cancel-timer ruby-block-timer)
    (setq ruby-block-timer nil)))

(defun ruby-block-hook ()
  "When Major-mode is ruby-mode, this package is running."
  (if (eq major-mode 'ruby-mode)
      (condition-case err
          (ruby-block-function)
        (error
         (setq ruby-block-mode nil)
         (message "Error: %S; ruby-block-mode now disabled." err)))
    (setq ruby-block-mode nil)))

(defun ruby-block-get-line-start-pos ()
  (save-excursion
    (let ((xor '(lambda (a b) (and (or a b) (not (and a b)))))
          (point (point))
          (count 0))
      (while (and (not (funcall xor (bobp) (eolp)))
                  (> point (point-min)))
        (setq point (1- point))
        (goto-char (1- (point))))
      ;; delete linefeed of start point.
      (when (and (eolp) (>= (point-max) (1+ point)))
        (setq point (1+ point)))
      point)))

(defun ruby-block-get-line-end-pos ()
  (save-excursion
    (let ((xor '(lambda (a b) (and (or a b) (not (and a b)))))
          (point (point)))
      (while (and (not (funcall xor (eobp) (eolp)))
                  (>= (point-max) point))
        (setq point (1+ point))
        (goto-char (1+ (point))))
      point)))

(defun ruby-block-function ()
  "Point position's word decides behavior."
  (let ((current (current-word)))
    (setq current (car (member current ruby-block-keyword-list)))
    (cond
     ;; not keyword
     ((null current)
      nil)
     ;; keyword "end"
     ((and (string= "end" current)
           (eq 'font-lock-keyword-face (get-text-property (point) 'face)))
      (let ((point (ruby-block-get-corresponding-point))
            (slinep 0)(elinep 0))
        ;; get whole line(exists point). and, display minibuffer.
        (when (> point 0)
          (save-excursion
            (goto-char point)
            (setq slinep (ruby-block-get-line-start-pos)
                  elinep (ruby-block-get-line-end-pos)))
          ;; display line contents to minibuffer
          (when (or (eq ruby-block-highlight-toggle t)
                    (eq ruby-block-highlight-toggle 'minibuffer))
            (message "%d: %s" (1+ (count-lines (point-min) slinep))
                     (buffer-substring slinep elinep)))
          ;; do overlay.
          (when (or (eq ruby-block-highlight-toggle t)
                    (eq ruby-block-highlight-toggle 'overlay))
            (ruby-block-do-highlight slinep elinep)))))
     ;; keyword except "end"
     (t
      nil))))

(defun ruby-block-get-corresponding-point ()
  "Get point of corresponding line."
  (let ((orig-col (- (point) (ruby-block-get-line-start-pos)))
        (recent-col (- (point) (ruby-block-get-line-start-pos)))
        (count 1)(check t)(point 0)(face "")(string ""))
    (save-excursion
      (while check
        (if (re-search-backward ruby-block-keyword-regex (point-min) t 1)
            (setq point  (match-beginning 1)
                  face   (get-text-property point 'face)
                  string (current-word))
          (setq point -1 face "" string "" check nil))
        (when (and (eq face 'font-lock-keyword-face)
                   (not (string= string "elsif"))
                   (member string ruby-block-keyword-list)
                   ;; case: STMT if(or unless, while, untill) EXPR
                   (if (member string '("if" "unless" "while" "until"))
                       (let ((col (- point (ruby-block-get-line-start-pos))))
                         (if (or (> (+ orig-col 3) col)
                                 (> (+ recent-col 3) col))
                             t nil))
                     t))
          (if (and (string= string "end") check)
              (setq count (1+ count)
                    recent-col (- (point) (ruby-block-get-line-start-pos)))
            (setq count (1- count))))
        (when (= count 0)
          (setq check nil)))
      point)))

(defun ruby-block-do-highlight (beg end)
  "Do overlay corresponding line."
  (if ruby-block-highlight-overlay
      (move-overlay  ruby-block-highlight-overlay beg end)
    (setq ruby-block-highlight-overlay (make-overlay beg end)))
  (overlay-put ruby-block-highlight-overlay
               'face ruby-block-highlight-face)
  (add-hook 'pre-command-hook 'ruby-block-highlight-done))

(defun ruby-block-highlight-done ()
  "After do overlay, restore the line to original color."
  (remove-hook 'pre-command-hook 'ruby-block-highlight-done)
  (if ruby-block-highlight-overlay
      (delete-overlay ruby-block-highlight-overlay)))

(defun ruby-block-highlight-toggle ()
  "Switch on/off for ruby-block-mode."
  (interactive)
  (if ruby-block-highlight-toggle
      (setq ruby-block-highlight-toggle nil)
    (setq ruby-block-highlight-toggle t)))

(provide 'ruby-block)

;; Local Variables:
;; Coding: iso-2022-7bit
;; End:

;;; ruby-block.el ends here
