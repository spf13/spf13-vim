;;; prelude-erlang.el --- Emacs Prelude: Erlang programming support.
;;
;; Copyright (c) 2011 Gleb Peregud
;;
;; Author: Gleb Peregud <gleber.p@gmail.com>
;; Version: 1.0.0
;; Keywords: convenience erlang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Erlang is a concurrent functional language.

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

(defcustom wrangler-path nil
  "*The location of wrangler elisp directory"
  :group 'prelude-erlang
  :type 'string
  :safe 'stringp)

(when (require 'erlang-start nil t)

  (eval-after-load 'erlang-mode
    '(progn
       (flymake-mode)))

  (when (not (null wrangler-path))
    (add-to-list 'load-path wrangler-path)
    (require 'wrangler)))

(defun erlang-rebar-compile ()
  (interactive)
  (let* ((dir (or (projectile-get-project-root)
                  (file-name-directory (buffer-file-name))))
         (pref (concat "cd " dir " && "))
         (cmd (cond ((file-exists-p (expand-file-name "rebar" dir))    "./rebar compile")
                    ((executable-find "rebar")                         "rebar compile")
                    ((file-exists-p (expand-file-name "Makefile" dir)) "Makefile")
                    (t nil))))
    (if cmd
        (compilation-start (concat pref cmd))
      (call-interactively 'inferior-erlang-compile))
    ))

(add-hook 'erlang-mode-hook (lambda ()
                              (make-variable-buffer-local 'projectile-project-root-files)
                              (setq projectile-project-root-files '("rebar.config" ".git" ".hg" ".bzr" ".projectile"))
                              (setq erlang-compile-function 'erlang-rebar-compile)))

(provide 'prelude-erlang)

;;; prelude-erlang.el ends here
