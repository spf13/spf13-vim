;;; init.el --- Emacs Prelude: configuration entry point.
;;
;; Copyright (c) 2011 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar.batsov@gmail.com>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/Prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file simply sets up the default load path and requires
;; the various modules defined within Emacs Prelude.

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

;; on OS X Emacs doesn't use the shell PATH
;; if you're using homebrew modifying the PATH is essential
(if (string= system-type "darwin")
    (push "/usr/local/bin" exec-path))

(defvar prelude-dir "~/.emacs.d/")

(defvar vendor-dir (concat prelude-dir "vendor/"))

(add-to-list 'load-path prelude-dir)
(add-to-list 'load-path vendor-dir)

(require 'prelude-ui)
(require 'prelude-packages)
(require 'prelude-core)
(require 'prelude-editor)
(require 'prelude-global-keybindings)

;; programming & markup languages support
(require 'prelude-c)
(require 'prelude-clojure)
(require 'prelude-common-lisp)
(require 'prelude-emacs-lisp)
(require 'prelude-haskell)
(require 'prelude-markdown)
(require 'prelude-perl)
(require 'prelude-ruby)

;;; init.el ends here
