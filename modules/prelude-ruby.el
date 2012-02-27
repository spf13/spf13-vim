;;; prelude-ruby.el --- Emacs Prelude: A nice setup for Ruby (and Rails) devs.
;;
;; Copyright (c) 2011 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar.batsov@gmail.com>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/Prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for Ruby and Rails development.

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

;; Rake files are ruby, too, as are gemspecs, rackup files, and gemfiles.
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))

;; We never want to edit Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")

;; yari provides a nice Emacs interface to ri
(require 'yari)

;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

; TODO fix ruby-end and package ruby-block for marmalade
(require 'ruby-block)
(require 'ruby-end)

(defun prelude-ruby-mode-defaults ()
  (inf-ruby-keys)
  ;; turn off the annoying input echo in irb
  (setq comint-process-echoes t)
  (ruby-block-mode t)
  (local-set-key (kbd "C-h r") 'yari))

(setq prelude-ruby-mode-hook 'prelude-ruby-mode-defaults)

(add-hook 'ruby-mode-hook (lambda () (run-hooks 'prelude-ruby-mode-hook)))

(require 'haml-mode)
(require 'scss-mode)

(defun prelude-css-mode-defaults ()
  (setq css-indent-offset 2)
  (rainbow-mode +1))

(setq prelude-css-mode-hook 'prelude-css-mode-defaults)

(add-hook 'css-mode-hook (lambda () (run-hooks 'prelude-css-mode-hook)))

(defun prelude-scss-mode-defaults ()
  (prelude-css-mode-hook)
  ;; turn off annoying auto-compile on save
  (setq scss-compile-at-save nil))

(setq prelude-scss-mode-hook 'prelude-scss-mode-defaults)

(add-hook 'scss-mode-hook (lambda () (run-hooks 'prelude-scss-mode-hook)))

;; cucumber support
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

(provide 'prelude-ruby)

;;; prelude-ruby.el ends here
