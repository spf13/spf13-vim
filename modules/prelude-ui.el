;;; prelude-ui.el --- Emacs Prelude: UI optimizations and tweaks.
;;
;; Copyright (c) 2011 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar.batsov@gmail.com>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/Prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; We dispense with most of the point and click UI, reduce the startup noise,
;; configure smooth scolling and a nice theme that's easy on the eyes (zenburn).

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

;; customization
(defgroup ui nil
  "Emacs Prelude UI"
  :group 'prelude)

(defcustom prelude-use-minimalistic-ui t
  "If set to true Prelude will dispense of most the UI that's mouse related - 
menu bar, tool bar, etc"
  :type 'boolean
  :group 'ui)

(defcustom prelude-use-smooth-scrolling t
  "Overrides the default scrolling behavior with a much more common one."
  :type 'boolean
  :group 'ui)

(defcustom prelude-use-default-prelude-theme t
  "If set to true Prelude will load up its default theme (Zenburn),
instead of Emacs's default theme."
  :type 'boolean
  :group 'ui)

(defcustom prelude-enhance-modeline t
  "If set to true Prelude will augment the default modeline settings."
  :type 'boolean
  :group 'ui)


(when prelude-use-minimalistic-ui
 ;; the toolbar is just a waste of valuable screen estate
 (tool-bar-mode -1)
 ;; the menu bar is mostly useless as well
 ;; but removing it under OS X doesn't make much sense
 (unless (eq system-type 'darwin)
   (menu-bar-mode -1))
 ;; the blinking cursor is nothing, but an annoyance
 (blink-cursor-mode -1)

 ;; disable startup screen
 (setq inhibit-startup-screen t))


(when prelude-use-smooth-scrolling
 ;; nice scrolling
 (setq scroll-margin 0
       scroll-conservatively 100000
       scroll-preserve-screen-position 1))

(when prelude-enhance-modeline
 ;; mode line settings
 (line-number-mode t)
 (column-number-mode t)
 (size-indication-mode t))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; custom Emacs 24 color themes support
(add-to-list 'custom-theme-load-path (concat prelude-dir "themes/"))

(when prelude-use-default-prelude-theme
  (load-theme 'zenburn t))

(provide 'prelude-ui)
;;; prelude-ui.el ends here
