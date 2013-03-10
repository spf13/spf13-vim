;;; prelude-erlang.el --- Emacs Prelude: Erlang programming support.
;;
;; Copyright © 2011-2013 Gleb Peregud
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

(require 'prelude-programming)
(prelude-ensure-module-deps '(erlang))

(defcustom wrangler-path nil
  "The location of wrangler elisp directory."
  :group 'prelude-erlang
  :type 'string
  :safe 'stringp)

(require 'projectile)

(when (require 'erlang-start nil t)

  (eval-after-load 'erlang-mode
    '(progn
       (flymake-mode)))

  (when (not (null wrangler-path))
    (add-to-list 'load-path wrangler-path)
    (require 'wrangler)))

(add-hook 'erlang-mode-hook (lambda ()
                              (setq erlang-compile-function 'projectile-compile-project)))

(provide 'prelude-erlang)

;;; prelude-erlang.el ends here
