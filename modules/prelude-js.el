;;; prelude-js.el --- Emacs Prelude: js-mode configuration.
;;
;; Copyright (c) 2011 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar.batsov@gmail.com>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/Prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for js-mode.

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

;; customize
(defgroup javascript nil
  "Emacs Prelude JavaScript programming support"
  :group 'prelude)

(defcustom prelude-enable-js-hook t
  "Enable Prelude's default JavaScript hook."
  :type 'boolean
  :group 'javascript)

(defun prelude-js-coding-hook ()
  (prelude-coding-hook)
  ;; electric-layout-mode doesn't play nice with js-mode
  (electric-layout-mode -1))

(when prelude-enable-js-hook
  (add-hook 'js-mode-hook 'prelude-js-coding-hook))

(provide 'prelude-js)

;;; prelude-js.el ends here
