;; erlang-start.el --- Load this file to initialize the Erlang package.

;; Copyright (C) 1998  Ericsson Telecom AB

;; Author:   Anders Lindgren
;; Version:  2.3
;; Keywords: erlang, languages, processes
;; Created:  1996-09-18
;; Date:     1998-03-16

;;; Commentary:

;; Introduction:
;; ------------
;;
;; This package provides support for the programming language Erlang.
;; The package provides an editing mode with lots of bells and
;; whistles, compilation support, and it makes it possible for the
;; user to start Erlang shells that run inside Emacs.
;;
;; See the Erlang distribution for full documentation of this package.

;; Installation:
;; ------------
;;
;; Place this file in Emacs load path, byte-compile it, and add the
;; following line to the appropriate init file:
;;
;;    (require 'erlang-start)
;;
;; The full documentation contains much more extensive description of
;; the installation procedure.

;; Reporting Bugs:
;; --------------
;;
;; Please send bug reports to the following email address:
;;     support@erlang.ericsson.se
;;
;; Please state as exactly as possible:
;;    - Version number of Erlang Mode (see the menu), Emacs, Erlang,
;;	and of any other relevant software.
;;    - What the expected result was.
;;    - What you did, preferably in a repeatable step-by-step form.
;;    - A description of the unexpected result.
;;    - Relevant pieces of Erlang code causing the problem.
;;    - Personal Emacs customisations, if any.
;;
;; Should the Emacs generate an error, please set the emacs variable
;; `debug-on-error' to `t'.  Repeat the error and enclose the debug
;; information in your bug-report.
;;
;; To set the variable you can use the following command:
;;     M-x set-variable RET debug-on-error RET t RET

;;; Code:

;;
;; Declare functions in "erlang.el".
;;

(autoload 'erlang-mode "erlang" "Major mode for editing Erlang code." t)
(autoload 'erlang-version "erlang" 
  "Return the current version of Erlang mode." t)
(autoload 'erlang-shell "erlang" "Start a new Erlang shell." t)
(autoload 'run-erlang "erlang" "Start a new Erlang shell." t)

(autoload 'erlang-compile "erlang"
  "Compile Erlang module in current buffer." t)

(autoload 'erlang-man-module "erlang" 
  "Find manual page for MODULE." t)
(autoload 'erlang-man-function "erlang"
  "Find manual page for NAME, where NAME is module:function." t)

(autoload 'erlang-find-tag "erlang"
  "Like `find-tag'.  Capable of retreiving Erlang modules.")
(autoload 'erlang-find-tag-other-window "erlang"
  "Like `find-tag-other-window'.  Capable of retreiving Erlang modules.")


;;
;; Associate files extensions ".erl" and ".hrl" with Erlang mode.
;;

(let ((a '("\\.erl\\'" . erlang-mode))
      (b '("\\.hrl\\'" . erlang-mode)))
  (or (assoc (car a) auto-mode-alist)
      (setq auto-mode-alist (cons a auto-mode-alist)))
  (or (assoc (car b) auto-mode-alist)
      (setq auto-mode-alist (cons b auto-mode-alist))))

;;
;; Associate files using interpreter "escript" with Erlang mode.
;; 

(add-to-list 'interpreter-mode-alist (cons "escript" 'erlang-mode))

;;
;; Ignore files ending in ".jam", ".vee", and ".beam" when performing
;; file completion.
;;

(let ((erl-ext '(".jam" ".vee" ".beam")))
  (while erl-ext
    (let ((cie completion-ignored-extensions))
      (while (and cie (not (string-equal (car cie) (car erl-ext))))
	(setq cie (cdr cie)))
      (if (null cie)
	  (setq completion-ignored-extensions
		(cons (car erl-ext) completion-ignored-extensions))))
    (setq erl-ext (cdr erl-ext))))


;;
;; The end.
;;

(provide 'erlang-start)

;; erlang-start.el ends here.
