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

(defgroup prelude nil
  "Emacs Prelude"
  :group 'convenience)

;; On OS X Emacs doesn't use the shell PATH if it's not started from
;; the shell. If you're using homebrew modifying the PATH is essential.
(if (eq system-type 'darwin)
    (push "/usr/local/bin" exec-path))

(defvar prelude-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Prelude distribution.")
(defvar prelude-modules-dir (concat prelude-dir "modules/")
  "This directory houses all of the built-in Prelude module. You should
avoid modifying the configuration there.")
(defvar prelude-vendor-dir (concat prelude-dir "vendor/")
  "This directory house Emacs Lisp packages that are not yet available in
ELPA (or Marmalade).")
(defvar prelude-personal-dir (concat prelude-dir "personal/")
  "Users of Emacs Prelude are encouraged to keep their personal configuration
changes in this directory. All Emacs Lisp files there are loaded automatically
by Prelude.")

(add-to-list 'load-path prelude-modules-dir)
(add-to-list 'load-path prelude-vendor-dir)
(add-to-list 'load-path prelude-personal-dir)

;; config changes made through the customize UI will be store here
(setq custom-file (concat prelude-personal-dir "custom.el"))

;; the core stuff
(require 'prelude-ui)
(require 'prelude-packages)
(require 'prelude-core)
(require 'prelude-editor)
(require 'prelude-global-keybindings)

(defcustom prelude-c-module t
  "Enable Prelude's C module."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-clojure-module t
  "Enable Prelude's Clojure module."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-coffee-script-module t
  "Enable Prelude's CoffeeScript module."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-common-lisp-module t
  "Enable Prelude's Common Lisp module."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-emacs-lisp-module t
  "Enable Prelude's Emacs Lisp module."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-erc-module t
  "Enable Prelude's ERC module."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-haskell-module t
  "Enable Prelude's Haskell module."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-js-module t
  "Enable Prelude's JavaScript module."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-latex-module t
  "Enable Prelude's LaTeX module."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-markdown-module t
  "Enable Prelude's Markdown module."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-org-module t
  "Enable Prelude's org-mode module."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-perl-module t
  "Enable Prelude's Perl module."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-python-module t
  "Enable Prelude's Python module."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-ruby-module t
  "Enable Prelude's Ruby module."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-scheme-module t
  "Enable Prelude's Scheme module."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-xml-module t
  "Enable Prelude's XML module."
  :type 'boolean
  :group 'prelude)


;; programming & markup languages support
(when prelude-c-module
  (require 'prelude-c))
(when prelude-clojure-module
  (require 'prelude-clojure))
(when prelude-coffee-script-module
  (require 'prelude-coffee))
(when prelude-common-lisp-module
  (require 'prelude-common-lisp))
(when prelude-emacs-lisp-module
  (require 'prelude-emacs-lisp))
(when prelude-erc-module
  (require 'prelude-erc))
(when prelude-haskell-module
  (require 'prelude-haskell))
(when prelude-js-module
  (require 'prelude-js))
(when prelude-latex-module
  (require 'prelude-latex))
(when prelude-markdown-module
  (require 'prelude-markdown))
(when prelude-org-module
  (require 'prelude-org))
(when prelude-perl-module
  (require 'prelude-perl))
(when prelude-perl-module
  (require 'prelude-python))
(when prelude-ruby-module
  (require 'prelude-ruby))
(when prelude-scheme-module
  (require 'prelude-scheme))
(when prelude-xml-module
  (require 'prelude-xml))

;; load the personal settings (this includes `custom-file')
(when (file-exists-p prelude-personal-dir)
  (mapc 'load (directory-files prelude-personal-dir nil "^[^#].*el$")))

;;; init.el ends here
