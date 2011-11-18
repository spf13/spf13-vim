;;; prelude-python.el --- Emacs Prelude: python.el configuration.
;;
;; Copyright (c) 2011 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar.batsov@gmail.com>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/Prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for python.el (the latest and greatest
;; Python mode Emacs has to offer).

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
(defgroup python nil
  "Emacs Prelude C programming support"
  :group 'prelude)

(defcustom prelude-enable-python-hook t
  "Enable Prelude's default Python hook."
  :type 'boolean
  :group 'python)

(require 'python)

;; (defun prelude-python-coding-hook ()
;; )

;; (when prelude-enable-python-hook
;;   (add-hook 'python-mode-hook 'prelude-python-coding-hook))

(provide 'prelude-python)

;;; prelude-python.el ends here
