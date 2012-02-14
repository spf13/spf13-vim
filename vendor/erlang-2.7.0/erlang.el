;;; erlang.el --- Major modes for editing and running Erlang

;; Copyright (C) 2004  Free Software Foundation, Inc.
;; Author:   Anders Lindgren
;; Keywords: erlang, languages, processes
;; Date:     2011-12-11

;; %CopyrightBegin%
;;
;; Copyright Ericsson AB 1996-2011. All Rights Reserved.
;;
;; The contents of this file are subject to the Erlang Public License,
;; Version 1.1, (the "License"); you may not use this file except in
;; compliance with the License. You should have received a copy of the
;; Erlang Public License along with this software. If not, it can be
;; retrieved online at http://www.erlang.org/.
;;
;; Software distributed under the License is distributed on an "AS IS"
;; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
;; the License for the specific language governing rights and limitations
;; under the License.
;;
;; %CopyrightEnd%
;;

;; Lars Thorsén's modifications of 2000-06-07 included.
;; The original version of this package was written by Robert Virding.
;;
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
;;      erlang-bugs@erlang.org
;; or if you have a patch suggestion to:
;;      erlang-patches@erlang.org
;; Please state as exactly as possible:
;;    - Version number of Erlang Mode (see the menu), Emacs, Erlang,
;;	and of any other relevant software.
;;    - What the expected result was.
;;    - What you did, preferably in a repeatable step-by-step form.
;;    - A description of the unexpected result.
;;    - Relevant pieces of Erlang code causing the problem.
;;    - Personal Emacs customisations, if any.
;;
;; Should the Emacs generate an error, please set the Emacs variable
;; `debug-on-error' to `t'.  Repeat the error and enclose the debug
;; information in your bug-report.
;;
;; To set the variable you can use the following command:
;;     M-x set-variable RET debug-on-error RET t RET
;;; Code:

;; Variables:

(defconst erlang-version "2.7"
  "The version number of Erlang mode.")

(defvar erlang-root-dir nil
  "The directory where the Erlang system is installed.
The name should not contain the trailing slash.

Should this variable be nil, no manual pages will show up in the
Erlang mode menu.")

(eval-and-compile
  (defconst erlang-emacs-major-version
    (if (boundp 'emacs-major-version)
	emacs-major-version
      (string-match "\\([0-9]+\\)\\.\\([0-9]+\\)" emacs-version)
      (erlang-string-to-int (substring emacs-version
				       (match-beginning 1) (match-end 1))))
    "Major version number of Emacs."))

(eval-and-compile
  (defconst erlang-emacs-minor-version
  (if (boundp 'emacs-minor-version)
      emacs-minor-version
    (string-match "\\([0-9]+\\)\\.\\([0-9]+\\)" emacs-version)
    (erlang-string-to-int (substring emacs-version
			      (match-beginning 2) (match-end 2))))
  "Minor version number of Emacs."))

(defconst erlang-xemacs-p (string-match "Lucid\\|XEmacs" emacs-version)
  "Non-nil when running under XEmacs or Lucid Emacs.")

(defvar erlang-xemacs-popup-menu '("Erlang Mode Commands" . nil)
  "Common popup menu for all buffers in Erlang mode.

This variable is destructively modified every time the Erlang menu
is modified.  The effect is that all changes take effect in all
buffers in Erlang mode, just like under GNU Emacs.

Never EVER set this variable!")

(defvar erlang-menu-items '(erlang-menu-base-items
			    erlang-menu-skel-items
			    erlang-menu-shell-items
			    erlang-menu-compile-items
			    erlang-menu-man-items
			    erlang-menu-personal-items
			    erlang-menu-version-items)
  "*List of menu item list to combine to create Erlang mode menu.

External programs which temporarily add menu items to the Erlang mode
menu may use this variable.  Please use the function `add-hook' to add
items.

Please call the function `erlang-menu-init' after every change to this
variable.")

(defvar erlang-menu-base-items
  '(("Indent"
     (("Indent Line" erlang-indent-command)
      ("Indent Region " erlang-indent-region
       (if erlang-xemacs-p (mark) mark-active))
      ("Indent Clause" erlang-indent-clause)
      ("Indent Function" erlang-indent-function)
      ("Indent Buffer" erlang-indent-current-buffer)))
    ("Edit"
     (("Fill Comment" erlang-fill-paragraph)
      ("Comment Region" comment-region
       (if erlang-xemacs-p (mark) mark-active))
      ("Uncomment Region" erlang-uncomment-region
       (if erlang-xemacs-p (mark) mark-active))
      nil
      ("Beginning of Function" erlang-beginning-of-function)
      ("End of Function" erlang-end-of-function)
      ("Mark Function" erlang-mark-function)
      nil
      ("Beginning of Clause" erlang-beginning-of-clause)
      ("End of Clause" erlang-end-of-clause)
      ("Mark Clause" erlang-mark-clause)
      nil
      ("New Clause" erlang-generate-new-clause)
      ("Clone Arguments" erlang-clone-arguments)
      nil
      ("Align Arrows" erlang-align-arrows)))
    ("Syntax Highlighting"
     (("Level 4" erlang-font-lock-level-4)
      ("Level 3" erlang-font-lock-level-3)
      ("Level 2" erlang-font-lock-level-2)
      ("Level 1" erlang-font-lock-level-1)
      ("Off" erlang-font-lock-level-0)))
    ("TAGS"
     (("Find Tag" find-tag)
      ("Find Next Tag" erlang-find-next-tag)
					;("Find Regexp" find-tag-regexp)
      ("Complete Word" erlang-complete-tag)
      ("Tags Apropos" tags-apropos)
      ("Search Files" tags-search))))
  "Description of menu used in Erlang mode.

This variable must be a list.  The elements are either nil representing
a horizontal line or a list with two or three elements.  The first is
the name of the menu item, the second is the function to call, or a
submenu, on the same same form as ITEMS.  The third optional argument
is an expression which is evaluated every time the menu is displayed.
Should the expression evaluate to nil the menu item is ghosted.

Example:
    '((\"Func1\" function-one)
      (\"SubItem\"
       ((\"Yellow\" function-yellow)
        (\"Blue\" function-blue)))
      nil
      (\"Region Function\" spook-function midnight-variable))

Call the function `erlang-menu-init' after modifying this variable.")

(defvar erlang-menu-shell-items
  '(nil
    ("Shell"
     (("Start New Shell" erlang-shell)
      ("Display Shell"   erlang-shell-display))))
  "Description of the Shell menu used by Erlang mode.

Please see the documentation of `erlang-menu-base-items'.")

(defvar erlang-menu-compile-items
  '(("Compile"
     (("Compile Buffer" erlang-compile)
      ("Display Result" erlang-compile-display)
      ("Next Error"     erlang-next-error))))
  "Description of the Compile menu used by Erlang mode.

Please see the documentation of `erlang-menu-base-items'.")

(defvar erlang-menu-version-items
  '(nil
    ("Version" erlang-version))
  "Description of the version menu used in Erlang mode.")

(defvar erlang-menu-personal-items nil
  "Description of personal menu items used in Erlang mode.

Please see the variable `erlang-menu-base-items' for a description
of the format.")

(defvar erlang-menu-man-items nil
  "The menu containing man pages.

The format of the menu should be compatible with `erlang-menu-base-items'.
This variable is added to the list of Erlang menus stored in
`erlang-menu-items'.")

(defvar erlang-menu-skel-items '()
  "Description of the menu containing the skeleton entries.
The menu is in the form described by the variable `erlang-menu-base-items'.")

(defvar erlang-mode-hook nil
  "*Functions to run when Erlang mode is activated.

This hook is used to change the behaviour of Erlang mode.  It is
normally used by the user to personalise the programming environment.
When used in a site init file, it could be used to customise Erlang
mode for all users on the system.

The functions added to this hook are run every time Erlang mode is
started.  See also `erlang-load-hook', a hook which is run once,
when Erlang mode is loaded into Emacs, and `erlang-shell-mode-hook'
which is run every time a new inferior Erlang shell is started.

To use a hook, create an Emacs lisp function to perform your actions
and add the function to the hook by calling `add-hook'.

The following example binds the key sequence C-c C-c to the command
`erlang-compile' (normally bound to C-c C-k).  The example also
activates Font Lock mode to fontify the buffer and adds a menu
containing all functions defined in the current buffer.

To use the example, copy the following lines to your `~/.emacs' file:

    (add-hook 'erlang-mode-hook 'my-erlang-mode-hook)

    (defun my-erlang-mode-hook ()
      (local-set-key \"\\C-c\\C-c\" 'erlang-compile)
      (if window-system
          (progn
            (setq font-lock-maximum-decoration t)
            (font-lock-mode 1)))
      (if (and window-system (fboundp 'imenu-add-to-menubar))
          (imenu-add-to-menubar \"Imenu\")))")

(defvar erlang-load-hook nil
  "*Functions to run when Erlang mode is loaded.

This hook is used to change the behaviour of Erlang mode.  It is
normally used by the user to personalise the programming environment.
When used in a site init file, it could be used to customize Erlang
mode for all users on the system.

The difference between this hook and `erlang-mode-hook' and
`erlang-shell-mode-hook' is that the functions in this hook
is only called once, when the Erlang mode is loaded into Emacs
the first time.

Natural actions for the functions added to this hook are actions which
only should be performed once, and actions which should be performed
before starting Erlang mode.  For example, a number of variables are
used by Erlang mode before `erlang-mode-hook' is run.

The following example sets the variable `erlang-root-dir' so that the
manual pages can be retrieved (note that you must set the value of
`erlang-root-dir' to match the location of Erlang on your system):

    (add-hook 'erlang-load-hook 'my-erlang-load-hook)

    (defun my-erlang-load-hook ()
       (setq erlang-root-dir \"/usr/local/erlang\"))")

(defvar erlang-new-file-hook nil
  "Functions to run when a new Erlang source file is being edited.

A useful function is `tempo-template-erlang-normal-header'.
\(This function only exists when the `tempo' package is available.)")

(defvar erlang-check-module-name 'ask
  "*Non-nil means check that module name and file name agrees when saving.

If the value of this variable is the atom `ask', the user is
prompted.  If the value is t the source is silently changed.")

(defvar erlang-electric-commands
  '(erlang-electric-comma
    erlang-electric-semicolon
    erlang-electric-gt)
  "*List of activated electric commands.

The list should contain the electric commands which should be active.
Currently, the available electric commands are:
    `erlang-electric-comma'
    `erlang-electric-semicolon'
    `erlang-electric-gt'
    `erlang-electric-newline'

Should the variable be bound to t, all electric commands
are activated.

To deactivate all electric commands, set this variable to nil.")

(defvar erlang-electric-newline-inhibit t
  "*Set to non-nil to inhibit newline after electric command.

This is useful since a lot of people press return after executing an
electric command.

In order to work, the command must also be in the
list `erlang-electric-newline-inhibit-list'.

Note that commands in this list are required to set the variable
`erlang-electric-newline-inhibit' to nil when the newline shouldn't be
inhibited.")

(defvar erlang-electric-newline-inhibit-list
  '(erlang-electric-semicolon
    erlang-electric-comma
    erlang-electric-gt)
  "*Commands which can inhibit the next newline.")

(defvar erlang-electric-semicolon-insert-blank-lines nil
  "*Number of blank lines inserted before header, or nil.

This variable controls the behaviour of `erlang-electric-semicolon'
when a new function header is generated.  When nil, no blank line is
inserted between the current line and the new header.  When bound to a
number it represents the number of blank lines which should be
inserted.")

(defvar erlang-electric-semicolon-criteria
  '(erlang-next-lines-empty-p
    erlang-at-keyword-end-p
    erlang-at-end-of-function-p)
  "*List of functions controlling `erlang-electric-semicolon'.
The functions in this list are called, in order, whenever a semicolon
is typed.  Each function in the list is called with no arguments,
and should return one of the following values:

  nil             -- no determination made, continue checking
  'stop           -- do not create prototype for next line
  (anything else) -- insert prototype, and stop checking

If every function in the list is called with no determination made,
then no prototype is inserted.

The test is performed by the function `erlang-test-criteria-list'.")

(defvar erlang-electric-comma-criteria
  '(erlang-stop-when-inside-argument-list
    erlang-stop-when-at-guard
    erlang-next-lines-empty-p
    erlang-at-keyword-end-p
    erlang-at-end-of-clause-p
    erlang-at-end-of-function-p)
  "*List of functions controlling `erlang-electric-comma'.
The functions in this list are called, in order, whenever a comma
is typed.  Each function in the list is called with no arguments,
and should return one of the following values:

  nil             -- no determination made, continue checking
  'stop           -- do not create prototype for next line
  (anything else) -- insert prototype, and stop checking

If every function in the list is called with no determination made,
then no prototype is inserted.

The test is performed by the function `erlang-test-criteria-list'.")

(defvar erlang-electric-arrow-criteria
  '(erlang-stop-when-in-type-spec
    erlang-next-lines-empty-p
    erlang-at-end-of-function-p)
  "*List of functions controlling the arrow aspect of `erlang-electric-gt'.
The functions in this list are called, in order, whenever a `>'
is typed.  Each function in the list is called with no arguments,
and should return one of the following values:

  nil             -- no determination made, continue checking
  'stop           -- do not create prototype for next line
  (anything else) -- insert prototype, and stop checking

If every function in the list is called with no determination made,
then no prototype is inserted.

The test is performed by the function `erlang-test-criteria-list'.")

(defvar erlang-electric-newline-criteria
  '(t)
  "*List of functions controlling `erlang-electric-newline'.

The electric newline commands indents the next line.  Should the
current line begin with a comment the comment start is copied to
the newly created line.

The functions in this list are called, in order, whenever a comma
is typed.  Each function in the list is called with no arguments,
and should return one of the following values:

  nil             -- no determination made, continue checking
  'stop           -- do not create prototype for next line
  (anything else) -- trigger the electric command.

If every function in the list is called with no determination made,
then no prototype is inserted.  Should the atom t be a member of the
list, it is treated as a function triggering the electric command.

The test is performed by the function `erlang-test-criteria-list'.")

(defvar erlang-next-lines-empty-threshold 2
  "*Number of blank lines required to activate an electric command.

Actually, this value controls the behaviour of the function
`erlang-next-lines-empty-p' which normally is a member of the
criteria lists controlling the electric commands.  (Please see
the variables `erlang-electric-semicolon-criteria' and
`erlang-electric-comma-criteria'.)

The variable is bound to a threshold value, a number, representing the
number of lines which must be empty.

Setting this variable to zero, electric commands will always be
triggered by `erlang-next-lines-empty-p', unless inhibited by other
rules.

Should this variable be nil, `erlang-next-lines-empty-p' will never
trigger an electric command.  The same effect would be reached if the
function `erlang-next-lines-empty-p' would be removed from the criteria
lists.

Note that even if `erlang-next-lines-empty-p' should not trigger an
electric command, other functions in the criteria list could.")

(defvar erlang-new-clause-with-arguments nil
  "*Non-nil means that the arguments are cloned when a clause is generated.

A new function header can be generated by calls to the function
`erlang-generate-new-clause' and by use of the electric semicolon.")

(defvar erlang-compile-use-outdir t
  "*When nil, go to the directory containing source file when compiling.

This is a workaround for a bug in the `outdir' option of compile.  If the
outdir is not in the current load path, Erlang doesn't load the object
module after it has been compiled.

To activate the workaround, place the following in your `~/.emacs' file:
    (setq erlang-compile-use-outdir nil)")

(defvar erlang-indent-level 4
  "*Indentation of Erlang calls/clauses within blocks.")
(put 'erlang-indent-level 'safe-local-variable 'integerp)

(defvar erlang-indent-guard 2
  "*Indentation of Erlang guards.")
(put 'erlang-indent-guard 'safe-local-variable 'integerp)

(defvar erlang-argument-indent 2
  "*Indentation of the first argument in a function call.
When nil, indent to the column after the `(' of the
function.")
(put 'erlang-argument-indent 'safe-local-variable '(lambda (val) (or (null val) (integerp val))))

(defvar erlang-tab-always-indent t
  "*Non-nil means TAB in Erlang mode should always re-indent the current line,
regardless of where in the line point is when the TAB command is used.")

(defvar erlang-error-regexp-alist
  '(("^\\([^:( \t\n]+\\)[:(][ \t]*\\([0-9]+\\)[:) \t]" . (1 2)))
  "*Patterns for matching Erlang errors.")

(defvar erlang-man-inhibit (eq system-type 'windows-nt)
  "Inhibit the creation of the Erlang Manual Pages menu.

The Windows distribution of Erlang does not include man pages, hence
there is no attempt to create the menu.")

(defvar erlang-man-dirs
  '(("Man - Commands" "/man/man1" t)
    ("Man - Modules" "/man/man3" t)
    ("Man - Files" "/man/man4" t)
    ("Man - Applications" "/man/man6" t))
  "*The man directories displayed in the Erlang menu.

Each item in the list should be a list with three elements, the first
the name of the menu, the second the directory, and the last a flag.
Should the flag the nil, the directory is absolute, should it be non-nil
the directory is relative to the variable `erlang-root-dir'.")

(defvar erlang-man-max-menu-size 35
  "*The maximum number of menu items in one menu allowed.")

(defvar erlang-man-display-function 'erlang-man-display
  "*Function used to display man page.

The function is called with one argument, the name of the file
containing the man page.  Use this variable when the default
function, `erlang-man-display', does not work on your system.")

(defvar erlang-compile-extra-opts '()
  "*Additional options to the compilation command.
This is an elisp list of options. Each option can be either:
- an atom
- a dotted pair
- a string
Example: '(bin_opt_info (i . \"/path1/include\") (i . \"/path2/include\"))")

(defvar erlang-compile-command-function-alist
  '((".erl\\'" . inferior-erlang-compute-erl-compile-command)
    (".xrl\\'" . inferior-erlang-compute-leex-compile-command)
    (".yrl\\'" . inferior-erlang-compute-yecc-compile-command)
    ("." . inferior-erlang-compute-erl-compile-command))
  "*Alist of filename patterns vs corresponding compilation functions.
Each element looks like (REGEXP . FUNCTION). Compiling a file whose name
matches REGEXP specifies FUNCTION to use to compute the compilation
command. The FUNCTION will be called with two arguments: module name and
default compilation options, like output directory. The FUNCTION
is expected to return a string.")

(defvar erlang-leex-compile-opts '()
  "*Options to pass to leex when compiling xrl files.
This is an elisp list of options. Each option can be either:
- an atom
- a dotted pair
- a string")

(defvar erlang-yecc-compile-opts '()
  "*Options to pass to yecc when compiling yrl files.
This is an elisp list of options. Each option can be either:
- an atom
- a dotted pair
- a string")

(eval-and-compile
  (defvar erlang-regexp-modern-p
    (if (> erlang-emacs-major-version 21) t nil)
    "Non-nil when this version of Emacs uses a modern version of regexp.
Supporting \_< and \_> This is determined by checking the version of Emacs used."))

(eval-and-compile
  (defconst erlang-atom-quoted-regexp 
    "'\\(?:[^\\']\\|\\(?:\\\\.\\)\\)*'"
    "Regexp describing a single-quoted atom"))

(eval-and-compile
  (defconst erlang-atom-regular-regexp
    (if erlang-regexp-modern-p
	"\\_<[[:lower:]]\\(?:\\sw\\|\\s_\\)*\\_>"
      "\\<[[:lower:]]\\(?:\\sw\\|\\s_\\)*\\>")
    "Regexp describing a regular (non-quoted) atom"))

(eval-and-compile
  (defconst erlang-atom-regexp 
    (concat "\\(" erlang-atom-quoted-regexp "\\|" 
	    erlang-atom-regular-regexp "\\)")
    "Regexp describing an Erlang atom."))

(eval-and-compile
  (defconst erlang-atom-regexp-matches 1
    "Number of regexp parenthesis pairs in `erlang-atom-regexp'.
  
This is used to determine parenthesis matches in complex regexps which
contains `erlang-atom-regexp'."))


(eval-and-compile
  (defconst erlang-variable-regexp 
    (if erlang-regexp-modern-p 
	"\\_<\\([[:upper:]_]\\(?:\\sw\\|\\s_\\)*\\)\\_>"
      "\\<\\([[:upper:]_]\\(?:\\sw\\|\\s_\\)*\\)\\>")
    "Regexp which should match an Erlang variable.

The regexp must be surrounded with a pair of regexp parentheses."))

(eval-and-compile
  (defconst erlang-variable-regexp-matches 1
    "Number of regexp parenthesis pairs in `erlang-variable-regexp'.

This is used to determine matches in complex regexps which contains
`erlang-variable-regexp'."))


(eval-and-compile
  (defun erlang-regexp-opt (strings &optional paren)
    "Like `regexp-opt', except if PAREN is `symbols', then the
resulting regexp is surrounded by \\_< and \\_>."
    (if (eq paren 'symbols)
	(if erlang-regexp-modern-p
	    (concat "\\_<" (regexp-opt strings t) "\\_>")
	  (concat "\\<" (regexp-opt strings t) "\\>"))
      (regexp-opt strings paren))))


(eval-and-compile 
  (defvar erlang-keywords
    '("after"
      "begin"
      "catch"
      "case"
      "cond"
      "end"
      "fun"
      "if"
      "let"
      "of"
      "query"
      "receive"
      "try"
      "when")
    "Erlang reserved keywords"))

(eval-and-compile
  (defconst erlang-keywords-regexp (erlang-regexp-opt erlang-keywords 'symbols)))
  
(eval-and-compile
  (defvar erlang-operators
    '("and"
      "andalso"
      "band"
      "bnot"
      "bor"
      "bsl"
      "bsr"
      "bxor"
      "div"
      "not"
      "or"
      "orelse"
      "rem"
      "xor")
    "Erlang operators"))
;; What about these?
;; '+' '-' '*' '/' '>', '>=', '<', '=<', '=:=', '==', '=/=', '/='

(eval-and-compile
  (defconst erlang-operators-regexp (erlang-regexp-opt erlang-operators 'symbols)))
  

(eval-and-compile
  (defvar erlang-guards
    '("is_atom"
      "is_binary"
      "is_bitstring"
      "is_boolean"
      "is_float"
      "is_function"
      "is_integer"
      "is_list"
      "is_number"
      "is_pid"
      "is_port"
      "is_record"
      "is_reference"
      "is_tuple"
      "atom"
      "binary"
      "bitstring"
      "boolean"
      ;;"float"	; Not included to avoid clashes with the bif float/1
      "function"
      "integer"
      "list"
      "number"
      "pid"
      "port"
      "record"
      "reference"
      "tuple")
    "Erlang guards"))

(eval-and-compile
  (defconst erlang-guards-regexp (erlang-regexp-opt erlang-guards 'symbols)))

(eval-and-compile
  (defvar erlang-predefined-types
    '("any"
      "arity"
      "boolean"
      "byte"
      "char"
      "cons"
      "deep_string"
      "iolist"
      "maybe_improper_list"
      "module"
      "mfa"
      "nil"
      "neg_integer"
      "none"
      "non_neg_integer"
      "nonempty_list"
      "nonempty_improper_list"
      "nonempty_maybe_improper_list"
      "no_return"
      "pos_integer"
      "string"
      "term"
      "timeout")
    "Erlang type specs types"))

(eval-and-compile
  (defconst erlang-predefined-types-regexp 
    (erlang-regexp-opt erlang-predefined-types 'symbols)))


(eval-and-compile
  (defvar erlang-int-bifs
    '("abs"
      "adler32"
      "adler32_combine"
      "alive"
      "apply"
      "atom_to_binary"
      "atom_to_list"
      "binary_to_atom"
      "binary_to_existing_atom"
      "binary_to_list"
      "binary_to_term"
      "bit_size"
      "bitstring_to_list"
      "byte_size"
      "check_process_code"
      "contact_binary"
      "crc32"
      "crc32_combine"
      "date"
      "decode_packet"
      "delete_module"
      "disconnect_node"
      "element"
      "erase"
      "exit"
      "float"
      "float_to_list"
      "garbage_collect"
      "get"
      "get_keys"
      "group_leader"
      "halt"
      "hd"
      "integer_to_list"
      "internal_bif"
      "iolist_size"
      "iolist_to_binary"
      "is_alive"
      "is_atom"
      "is_binary"
      "is_bitstring"
      "is_boolean"
      "is_float"
      "is_function"
      "is_integer"
      "is_list"
      "is_number"
      "is_pid"
      "is_port"
      "is_process_alive"
      "is_record"
      "is_reference"
      "is_tuple"
      "length"
      "link"
      "list_to_atom"
      "list_to_binary"
      "list_to_bitstring"
      "list_to_existing_atom"
      "list_to_float"
      "list_to_integer"
      "list_to_pid"
      "list_to_tuple"
      "load_module"
      "make_ref"
      "module_loaded"
      "monitor_node"
      "node"
      "node_link"
      "node_unlink"
      "nodes"
      "notalive"
      "now"
      "open_port"
      "pid_to_list"
      "port_close"
      "port_command"
      "port_connect"
      "port_control"
      "pre_loaded"
      "process_flag"
      "process_info"
      "processes"
      "purge_module"
      "put"
      "register"
      "registered"
      "round"
      "self"
      "setelement"
      "size"
      "spawn"
      "spawn_link"
      "spawn_monitor"
      "spawn_opt"
      "split_binary"
      "statistics"
      "term_to_binary"
      "time"
      "throw"
      "tl"
      "trunc"
      "tuple_size"
      "tuple_to_list"
      "unlink"
      "unregister"
      "whereis")
    "Erlang built-in functions (BIFs)"))

(eval-and-compile
  (defconst erlang-int-bif-regexp (erlang-regexp-opt erlang-int-bifs 'symbols)))
  

(eval-and-compile
  (defvar erlang-ext-bifs
    '("append_element"
      "bump_reductions"
      "cancel_timer"
      "demonitor"
      "display"
      "fun_info"
      "fun_to_list"
      "function_exported"
      "get_cookie"
      "get_stacktrace"
      "hash"
      "integer_to_list"
      "is_builtin"
      "list_to_integer"
      "loaded"
      "localtime"
      "localtime_to_universaltime"
      "make_tuple"
      "max"
      "md5"
      "md5_final"
      "md5_init"
      "md5_update"
      "memory"
      "min"
      "monitor"
      "monitor_node"
      "phash"
      "phash2"
      "port_call"
      "port_info"
      "port_to_list"
      "ports"
      "process_display"
      "read_timer"
      "ref_to_list"
      "resume_process"
      "send"
      "send_after"
      "send_nosuspend"
      "set_cookie"
      "start_timer"
      "suspend_process"
      "system_flag"
      "system_info"
      "system_monitor"
      "system_profile"
      "trace"
      "trace_delivered"
      "trace_info"
      "trace_pattern"
      "universaltime"
      "universaltime_to_localtime"
      "yield")
    "Erlang built-in functions (BIFs) that needs erlang: prefix"))

(eval-and-compile
  (defconst erlang-ext-bif-regexp
    (erlang-regexp-opt (append erlang-int-bifs erlang-ext-bifs) 'symbols)))


(defvar erlang-defun-prompt-regexp (concat "^" erlang-atom-regexp "\\s *(")
  "Regexp which should match beginning of a clause.")

(defvar erlang-file-name-extension-regexp "\\.[eh]rl$"
  "*Regexp which should match an Erlang file name.

This regexp is used when an Erlang module name is extracted from the
name of an Erlang source file.

The regexp should only match the section of the file name which should
be excluded from the module name.

To match all files set this variable to \"\\\\(\\\\..*\\\\|\\\\)$\".
The matches all except the extension.  This is useful if the Erlang
tags system should interpret tags on the form `module:tag' for
files written in other languages than Erlang.")

(defvar erlang-inferior-shell-split-window t
  "*If non-nil, when starting an inferior shell, split windows.
If nil, the inferior shell replaces the window. This is the traditional
behaviour.")

(defconst inferior-erlang-use-cmm (boundp 'minor-mode-overriding-map-alist)
  "Non-nil means use `compilation-minor-mode' in Erlang shell.")

(defvar erlang-mode-map
  (let ((map (make-sparse-keymap)))
    (unless (boundp 'indent-line-function)
      (define-key map "\t"        'erlang-indent-command))
    (define-key map ";"	      'erlang-electric-semicolon)
    (define-key map ","	      'erlang-electric-comma)
    (define-key map "<"         'erlang-electric-lt)
    (define-key map ">"         'erlang-electric-gt)
    (define-key map "\C-m"      'erlang-electric-newline)
    (if (not (boundp 'delete-key-deletes-forward))
        (define-key map "\177" 'backward-delete-char-untabify)
      (define-key map [(backspace)] 'backward-delete-char-untabify))
    ;;(unless (boundp 'fill-paragraph-function)
    (define-key map "\M-q"      'erlang-fill-paragraph)
    (unless (boundp 'beginning-of-defun-function)
      (define-key map "\M-\C-a"   'erlang-beginning-of-function)
      (define-key map "\M-\C-e"   'erlang-end-of-function)
      (define-key map '(meta control h)   'erlang-mark-function))	; Xemacs
    (define-key map "\M-\t"     'erlang-complete-tag)
    (define-key map "\C-c\M-\t" 'tempo-complete-tag)
    (define-key map "\M-+"      'erlang-find-next-tag)
    (define-key map "\C-c\M-a"  'erlang-beginning-of-clause)
    (define-key map "\C-c\M-b"  'tempo-backward-mark)
    (define-key map "\C-c\M-e"  'erlang-end-of-clause)
    (define-key map "\C-c\M-f"  'tempo-forward-mark)
    (define-key map "\C-c\M-h"  'erlang-mark-clause)
    (define-key map "\C-c\C-c"  'comment-region)
    (define-key map "\C-c\C-j"  'erlang-generate-new-clause)
    (define-key map "\C-c\C-k"  'erlang-compile)
    (define-key map "\C-c\C-l"  'erlang-compile-display)
    (define-key map "\C-c\C-s"  'erlang-show-syntactic-information)
    (define-key map "\C-c\C-q"  'erlang-indent-function)
    (define-key map "\C-c\C-u"  'erlang-uncomment-region)
    (define-key map "\C-c\C-y"  'erlang-clone-arguments)
    (define-key map "\C-c\C-a"  'erlang-align-arrows)
    (define-key map "\C-c\C-z"  'erlang-shell-display)
    (unless inferior-erlang-use-cmm
      (define-key map "\C-x`"    'erlang-next-error))
    map)
  "*Keymap used in Erlang mode.")
(defvar erlang-mode-abbrev-table nil
  "Abbrev table in use in Erlang-mode buffers.")
(defvar erlang-mode-syntax-table nil
  "Syntax table in use in Erlang-mode buffers.")



(defvar erlang-skel-file "erlang-skels"
  "The type of erlang-skeletons that should be used, default
   uses edoc type, for the old type, standard comments,
   set \"erlang-skels-old\" in your .emacs and restart.

   Or define your own and set the variable to that file.")

;; Tempo skeleton templates:
(load erlang-skel-file)

;; Font-lock variables

;; The next few variables define different Erlang font-lock patterns.
;; They could be appended to form a custom font-lock appearance.
;;
;; The function `erlang-font-lock-set-face' could be used to change
;; the face of a pattern.
;;
;; Note that Erlang strings and atoms are highlighted with using
;; syntactic analysis.

(defvar erlang-font-lock-keywords-function-header
  (list
   (list (concat "^" erlang-atom-regexp "\\s-*(")
	 1 'font-lock-function-name-face t))
  "Font lock keyword highlighting a function header.")

(defvar erlang-font-lock-keywords-int-bifs
  (list
   (list (concat erlang-int-bif-regexp "\\s-*(")
         1 'font-lock-builtin-face))
  "Font lock keyword highlighting built in functions.")

(defvar erlang-font-lock-keywords-ext-bifs
  (list
   (list (concat "\\<\\(erlang\\)\\s-*:\\s-*" erlang-ext-bif-regexp "\\s-*(")
	 '(1 'font-lock-builtin-face)
	 '(2 'font-lock-builtin-face)))
  "Font lock keyword highlighting built in functions.")

(defvar erlang-font-lock-keywords-int-function-calls
  (list
   (list (concat erlang-atom-regexp "\\s-*(")
         1 'font-lock-type-face))
  "Font lock keyword highlighting an internal function call.")

(defvar erlang-font-lock-keywords-ext-function-calls
  (list
   (list (concat erlang-atom-regexp "\\s-*:\\s-*"
                 erlang-atom-regexp "\\s-*(")
         '(1 'font-lock-type-face)
	 '(2 'font-lock-type-face)))
  "Font lock keyword highlighting an external function call.")

(defvar erlang-font-lock-keywords-fun-n
  (list
   (list (concat "\\(" erlang-atom-regexp "/[0-9]+\\)")
         1 'font-lock-type-face))
  "Font lock keyword highlighting a fun descriptor in F/N format.")

(defvar erlang-font-lock-keywords-operators
  (list
   (list erlang-operators-regexp
         1 'font-lock-builtin-face))
  "Font lock keyword highlighting Erlang operators.")

(defvar erlang-font-lock-keywords-dollar
  (list
   (list "\\(\\$\\([^\\]\\|\\\\\\([^0-7^\n]\\|[0-7]+\\|\\^[a-zA-Z]\\)\\)\\)"
	 1 'font-lock-constant-face))
  "Font lock keyword highlighting numbers in ASCII form (e.g. $A).")

(defvar erlang-font-lock-keywords-arrow
  (list
   (list "->\\(\\s \\|$\\)" 1 'font-lock-function-name-face))
  "Font lock keyword highlighting clause arrow.")

(defvar erlang-font-lock-keywords-lc
  (list
   (list "\\(<-\\|<=\\|||\\)\\(\\s \\|$\\)" 1 'font-lock-keyword-face))
  "Font lock keyword highlighting list comprehension operators.")

(defvar erlang-font-lock-keywords-keywords
  (list
   (list erlang-keywords-regexp 1 'font-lock-keyword-face))
  "Font lock keyword highlighting Erlang keywords.")

(defvar erlang-font-lock-keywords-attr
  (list
   (list (concat "^\\(-" erlang-atom-regexp "\\)\\(\\s-\\|\\.\\|(\\)")	 
	 1 (if (boundp 'font-lock-preprocessor-face)
	       'font-lock-preprocessor-face
	     'font-lock-constant-face)))
  "Font lock keyword highlighting attributes.")

(defvar erlang-font-lock-keywords-quotes
  (list
   (list "`\\([-+a-zA-Z0-9_:*][-+a-zA-Z0-9_:*]+\\)'"
	 1
	 'font-lock-keyword-face
	 t))
  "Font lock keyword highlighting words in single quotes in comments.

This is not the highlighting of Erlang strings and atoms, which
are highlighted by syntactic analysis.")

(defvar erlang-font-lock-keywords-guards
  (list
   (list (concat "[^:]" erlang-guards-regexp "\\s-*(")
	 1 'font-lock-builtin-face))
  "Font lock keyword highlighting guards.")

(defvar erlang-font-lock-keywords-predefined-types
  (list
   (list (concat "[^:]" erlang-predefined-types-regexp "\\s-*(")
	 1 'font-lock-builtin-face))
  "Font lock keyword highlighting predefined types.")


(defvar erlang-font-lock-keywords-macros
  (list
   (list (concat "?\\s-*\\(" erlang-atom-regexp
		 "\\|" erlang-variable-regexp "\\)")
	 1 'font-lock-constant-face)
   (list (concat "^\\(-\\(?:define\\|ifn?def\\)\\)\\s-*(\\s-*\\(" erlang-atom-regexp
		 "\\|" erlang-variable-regexp "\\)")
	 (if (boundp 'font-lock-preprocessor-face)
	     (list 1 'font-lock-preprocessor-face t)
	   (list 1 'font-lock-constant-face t))
	 (list 3 'font-lock-type-face t t))
   (list "^-e\\(lse\\|ndif\\)\\>" 0 'font-lock-preprocessor-face t))
  "Font lock keyword highlighting macros.
This must be placed in front of `erlang-font-lock-keywords-vars'.")

(defvar erlang-font-lock-keywords-records
  (list
   (list (concat "#\\s *" erlang-atom-regexp)
         1 'font-lock-type-face)
   ;; Don't highlight numerical constants.
   (list (if erlang-regexp-modern-p
	     "\\_<[0-9]+#\\([0-9a-zA-Z]+\\)"
	   "\\<[0-9]+#\\([0-9a-zA-Z]+\\)")
         1 nil t)
   (list (concat "^-record\\s-*(\\s-*" erlang-atom-regexp)
         1 'font-lock-type-face))
  "Font lock keyword highlighting Erlang records.
This must be placed in front of `erlang-font-lock-keywords-vars'.")

(defvar erlang-font-lock-keywords-vars
  (list
   (list (concat "[^#]" erlang-variable-regexp)	; no numerical constants
	 1 'font-lock-variable-name-face))
  "Font lock keyword highlighting Erlang variables.
Must be preceded by `erlang-font-lock-keywords-macros' to work properly.")

(defvar erlang-font-lock-descr-string
  "Font-lock keywords used by Erlang Mode.

There exists three levels of Font Lock keywords for Erlang:
  `erlang-font-lock-keywords-1' - Function headers and reserved keywords.
  `erlang-font-lock-keywords-2' - Bifs, guards and `single quotes'.
  `erlang-font-lock-keywords-3' - Variables, macros and records.
  `erlang-font-lock-keywords-4' - Function names, Funs, LCs (not Atoms)

To use a specific level, please set the variable
`font-lock-maximum-decoration' to the appropriate level.  Note that the
variable must be set before Erlang mode is activated.

Example:
    (setq font-lock-maximum-decoration 2)")

(defvar erlang-font-lock-keywords-1
  (append erlang-font-lock-keywords-function-header
	  erlang-font-lock-keywords-dollar
	  erlang-font-lock-keywords-arrow
	  erlang-font-lock-keywords-keywords
	  )
  ;; DocStringOrig: erlang-font-lock-keywords
  erlang-font-lock-descr-string)

(defvar erlang-font-lock-keywords-2
  (append erlang-font-lock-keywords-1
	  erlang-font-lock-keywords-int-bifs
	  erlang-font-lock-keywords-ext-bifs
	  erlang-font-lock-keywords-attr
	  erlang-font-lock-keywords-quotes
	  erlang-font-lock-keywords-guards
	  )
  ;; DocStringCopy: erlang-font-lock-keywords
  erlang-font-lock-descr-string)

(defvar erlang-font-lock-keywords-3
  (append erlang-font-lock-keywords-2
	  erlang-font-lock-keywords-operators
	  erlang-font-lock-keywords-macros
	  erlang-font-lock-keywords-records
	  erlang-font-lock-keywords-vars
	  erlang-font-lock-keywords-predefined-types
	  )
  ;; DocStringCopy: erlang-font-lock-keywords
  erlang-font-lock-descr-string)

(defvar erlang-font-lock-keywords-4
  (append erlang-font-lock-keywords-3
          erlang-font-lock-keywords-int-function-calls
	  erlang-font-lock-keywords-ext-function-calls
	  erlang-font-lock-keywords-fun-n
          erlang-font-lock-keywords-lc
	  )
  ;; DocStringCopy: erlang-font-lock-keywords
  erlang-font-lock-descr-string)

(defvar erlang-font-lock-keywords erlang-font-lock-keywords-4
  ;; DocStringCopy: erlang-font-lock-keywords
  erlang-font-lock-descr-string)

(defvar erlang-font-lock-syntax-table nil
  "Syntax table used by Font Lock mode.

The difference between this and the standard Erlang Mode
syntax table is that `_' is treated as part of words by
this syntax table.

Unfortunately, XEmacs hasn't got support for a special Font
Lock syntax table.  The effect is that `apply' in the atom
`foo_apply' will be highlighted as a bif.")


;;; Avoid errors while compiling this file.

;; `eval-when-compile' is not defined in Emacs 18.  We define it as a
;; no-op.
(or (fboundp 'eval-when-compile)
    (defmacro eval-when-compile (&rest rest) nil))

;; These umm...functions are new in Emacs 20. And, yes, until version
;; 19.27 Emacs backquotes were this ugly.

(or (fboundp 'unless)
    (defmacro unless (condition &rest body)
      "(unless CONDITION BODY...): If CONDITION is false, do BODY, else return nil."
      `((if (, condition) nil ,@body))))

(or (fboundp 'when)
    (defmacro when (condition &rest body)
      "(when CONDITION BODY...): If CONDITION is true, do BODY, else return nil."
      `((if (, condition) (progn ,@body) nil))))

(or (fboundp 'char-before)
    (defmacro char-before (&optional pos)
      "Return the character in the current buffer just before POS."
      `( (char-after (1- (or ,pos (point)))))))

;; defvar some obsolete variables, which we still support for
;; backwards compatibility reasons.
(eval-when-compile
  (defvar comment-indent-hook)
  (defvar dabbrev-case-fold-search)
  (defvar tempo-match-finder)
  (defvar compilation-menu-map)
  (defvar next-error-last-buffer))

(eval-when-compile
  (if (or (featurep 'bytecomp)
	  (featurep 'byte-compile))
      (progn
	(cond ((string-match "Lucid\\|XEmacs" emacs-version)
	       (put 'comment-indent-hook 'byte-obsolete-variable nil)
	       ;; Do not warn for unused variables
	       ;; when compiling under XEmacs.
	       (setq byte-compile-warnings
		     '(free-vars unresolved callargs redefine))))
	(require 'comint)
	(require 'tempo)
	(require 'compile))))


(defun erlang-version ()
  "Return the current version of Erlang mode."
  (interactive)
  (if (interactive-p)
      (message "Erlang mode version %s, written by Anders Lindgren"
	       erlang-version))
  erlang-version)


;;;###autoload
(defun erlang-mode ()
  "Major mode for editing Erlang source files in Emacs.
It knows about syntax and comment, it can indent code, it is capable
of fontifying the source file, the TAGS commands are aware of Erlang
modules, and the Erlang man pages can be accessed.

Should this module, \"erlang.el\", be installed properly, Erlang mode
is activated whenever an Erlang source or header file is loaded into
Emacs.  To indicate this, the mode line should contain the word
\"Erlang\".

The main feature of Erlang mode is indentation, press TAB and the
current line will be indented correctly.

Comments starting with only one `%' are indented to the column stored
in the variable `comment-column'.  Comments starting with two `%':s
are indented with the same indentation as code.  Comments starting
with at least three `%':s are indented to the first column.

However, Erlang mode contains much more, this is a list of the most
useful commands:
     TAB     - Indent the line.
     C-c C-q - Indent current function.
     M-;     - Create a comment at the end of the line.
     M-q     - Fill a comment, i.e. wrap lines so that they (hopefully)
		 will look better.
     M-a     - Goto the beginning of an Erlang clause.
     M-C-a   - Ditto for function.
     M-e     - Goto the end of an Erlang clause.
     M-C-e   - Ditto for function.
     M-h     - Mark current Erlang clause.
     M-C-h   - Ditto for function.
     C-c C-z - Start, or switch to, an inferior Erlang shell.
     C-c C-k - Compile current file.
     C-x `   - Next error.
     ,       - Electric comma.
     ;       - Electric semicolon.

Erlang mode check the name of the file against the module name when
saving, whenever a mismatch occurs Erlang mode offers to modify the
source.

The variable `erlang-electric-commands' controls the electric
commands.  To deactivate all of them, set it to nil.

There exists a large number of commands and variables in the Erlang
module.  Please press `M-x apropos RET erlang RET' to see a complete
list.  Press `C-h f name-of-function RET' and `C-h v name-of-variable
RET'to see the full description of functions and variables,
respectively.

On entry to this mode the contents of the hook `erlang-mode-hook' is
executed.

Please see the beginning of the file `erlang.el' for more information
and examples of hooks.

Other commands:
\\{erlang-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'erlang-mode)
  (setq mode-name "Erlang")
  (erlang-syntax-table-init)
  (use-local-map erlang-mode-map)
  (erlang-electric-init)
  (erlang-menu-init)
  (erlang-mode-variables)
  (erlang-check-module-name-init)
  (erlang-add-compilation-alist erlang-error-regexp-alist)
  (erlang-man-init)
  (erlang-tags-init)
  (erlang-font-lock-init)
  (erlang-skel-init)
  (tempo-use-tag-list 'erlang-tempo-tags)
  (run-hooks 'erlang-mode-hook)
  (if (zerop (buffer-size))
      (run-hooks 'erlang-new-file-hook))
  ;; Doesn't exist in Emacs v21.4; required by Emacs v23.
  (if (boundp 'after-change-major-mode-hook)
      (run-hooks 'after-change-major-mode-hook)))


(defun erlang-syntax-table-init ()
  (if (null erlang-mode-syntax-table)
      (let ((table (make-syntax-table)))
	(modify-syntax-entry ?\n ">" table)
	(modify-syntax-entry ?\" "\"" table)
	(modify-syntax-entry ?# "." table)
;;	(modify-syntax-entry ?$ "\\" table)   ;; Creates problems with indention afterwards
;;	(modify-syntax-entry ?$ "'" table)    ;; Creates syntax highlighting and indention problems
	(modify-syntax-entry ?$ "/" table)    ;; Misses the corner case "string that ends with $" 
	                                      ;; we have to live with that for now..it is the best alternative
	                                      ;; that can be worked around with "string hat ends with \$" 
	(modify-syntax-entry ?% "<" table)
	(modify-syntax-entry ?& "." table)
	(modify-syntax-entry ?\' "\"" table)
	(modify-syntax-entry ?* "." table)
	(modify-syntax-entry ?+ "." table)
	(modify-syntax-entry ?- "." table)
	(modify-syntax-entry ?/ "." table)
	(modify-syntax-entry ?: "." table)
	(modify-syntax-entry ?< "." table)
	(modify-syntax-entry ?= "." table)
	(modify-syntax-entry ?> "." table)
	(modify-syntax-entry ?\\ "\\" table)
	(modify-syntax-entry ?_ "_" table)
	(modify-syntax-entry ?| "." table)
	(modify-syntax-entry ?^ "'" table)
	
	;; Pseudo bit-syntax: Latin1 double angle quotes as parens.
	;;(modify-syntax-entry ?\253 "(?\273" table)
	;;(modify-syntax-entry ?\273 ")?\253" table)

	(setq erlang-mode-syntax-table table)))

  (set-syntax-table erlang-mode-syntax-table))


(defun erlang-electric-init ()
  ;; Set up electric character functions to work with
  ;; delsel/pending-del mode. Also, set up text properties for bit
  ;; syntax handling.
  (mapc #'(lambda (cmd)
	    (put cmd 'delete-selection t) ;for delsel (Emacs)
	    (put cmd 'pending-delete t)) ;for pending-del (XEmacs)
	'(erlang-electric-semicolon
	  erlang-electric-comma
	  erlang-electric-gt))
  
  (put 'bitsyntax-open-outer 'syntax-table '(4 . ?>))
  (put 'bitsyntax-open-outer 'rear-nonsticky '(category))
  (put 'bitsyntax-open-inner 'rear-nonsticky '(category))
  (put 'bitsyntax-close-inner 'rear-nonsticky '(category))
  (put 'bitsyntax-close-outer 'syntax-table '(5 . ?<))
  (put 'bitsyntax-close-outer 'rear-nonsticky '(category))
  (make-local-variable 'parse-sexp-lookup-properties)
  (setq parse-sexp-lookup-properties 't))


(defun erlang-mode-variables ()
  (or erlang-mode-abbrev-table
      (define-abbrev-table 'erlang-mode-abbrev-table ()))
  (setq local-abbrev-table erlang-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'defun-prompt-regexp)
  (setq defun-prompt-regexp erlang-defun-prompt-regexp)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "%+\\s *")
  (make-local-variable 'comment-column)
  (setq comment-column 48)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'erlang-indent-command)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'erlang-indent-region)
  (set (make-local-variable 'comment-indent-function) 'erlang-comment-indent)
  (if (<= erlang-emacs-major-version 18)
      (set (make-local-variable 'comment-indent-hook) 'erlang-comment-indent))
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'dabbrev-case-fold-search) nil)
  (set (make-local-variable 'imenu-prev-index-position-function)
       'erlang-beginning-of-function)
  (set (make-local-variable 'imenu-extract-index-name-function)
       'erlang-get-function-name-and-arity)
  (set (make-local-variable 'tempo-match-finder)
       "[^-a-zA-Z0-9_]\\([-a-zA-Z0-9_]*\\)\\=")
  (set (make-local-variable 'beginning-of-defun-function)
       'erlang-beginning-of-function)
  (set (make-local-variable 'end-of-defun-function) 'erlang-end-of-function)
  (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil)
  (set (make-local-variable 'fill-paragraph-function) 'erlang-fill-paragraph)
  (set (make-local-variable 'comment-add) 1)
  (set (make-local-variable 'outline-regexp) "[[:lower:]0-9_]+ *(.*) *-> *$")
  (set (make-local-variable 'outline-level) (lambda () 1))
  (set (make-local-variable 'add-log-current-defun-function)
       'erlang-current-defun))


;; Compilation.
;;
;; The following code is compatible with the standard package `compilation',
;; making it possible to go to errors using `erlang-next-error' (or just
;; `next-error' in Emacs 21).
;;
;; The normal `compile' command works of course.  For best result, please
;; execute `make' with the `-w' flag.
;;
;; Please see the variables named `compiling-..' above.

(defun erlang-add-compilation-alist (alist)
  (require 'compile)
  (cond ((boundp 'compilation-error-regexp-alist) ; Emacs 19
	 (while alist
	   (or (assoc (car (car alist)) compilation-error-regexp-alist)
	       (setq compilation-error-regexp-alist
		     (cons (car alist) compilation-error-regexp-alist)))
	   (setq alist (cdr alist))))
	((boundp 'compilation-error-regexp)
	 ;; Emacs 18,  Only one regexp is allowed.
	 (funcall (symbol-function 'set)
		  'compilation-error-regexp (car (car alist))))))

(defun erlang-font-lock-init ()
  "Initialize Font Lock for Erlang mode."
  (or erlang-font-lock-syntax-table
      (setq erlang-font-lock-syntax-table
	    (let ((table (copy-syntax-table erlang-mode-syntax-table)))
	      (modify-syntax-entry ?_ "w" table)
	      table)))
  (set (make-local-variable 'font-lock-syntax-table)
       erlang-font-lock-syntax-table)
  (set (make-local-variable 'font-lock-beginning-of-syntax-function)
       'erlang-beginning-of-clause)
  (make-local-variable 'font-lock-keywords)
  (let ((level (cond ((boundp 'font-lock-maximum-decoration)
		      (symbol-value 'font-lock-maximum-decoration))
		     ((boundp 'font-lock-use-maximal-decoration)
		      (symbol-value 'font-lock-use-maximal-decoration))
		     (t nil))))
    (if (consp level)
	(setq level (cdr-safe (or (assq 'erlang-mode level)
				  (assq t level)))))
    ;; `level' can here be:
    ;;      A number - The fontification level
    ;;      nil      - Use the default
    ;;      t        - Use maximum
    (cond ((eq level nil)
	   (set 'font-lock-keywords erlang-font-lock-keywords))
	  ((eq level 1)
	   (set 'font-lock-keywords erlang-font-lock-keywords-1))
	  ((eq level 2)
	   (set 'font-lock-keywords erlang-font-lock-keywords-2))
          ((eq level 3)
           (set 'font-lock-keywords erlang-font-lock-keywords-3))
          (t
           (set 'font-lock-keywords erlang-font-lock-keywords-4))))

  ;; Modern font-locks can handle the above much more elegantly:
  (set (make-local-variable 'font-lock-defaults)
       '((erlang-font-lock-keywords erlang-font-lock-keywords-1
				    erlang-font-lock-keywords-2 
				    erlang-font-lock-keywords-3 
				    erlang-font-lock-keywords-4)
	 nil nil ((?_ . "w")) erlang-beginning-of-clause
	 (font-lock-mark-block-function . erlang-mark-clause)
         (font-lock-syntactic-keywords
          ;; A dollar sign right before the double quote that ends a
          ;; string is not a character escape.
          ;;
          ;; And a "string" has with a double quote not escaped by a
          ;; dollar sign, any number of non-backslash non-newline
          ;; characters or escaped backslashes, a dollar sign
          ;; (otherwise we wouldn't care) and a double quote.  This
          ;; doesn't match multi-line strings, but this is probably
          ;; the best we can get, since while font-locking we don't
          ;; know whether matching started inside a string: limiting
          ;; search to a single line keeps things sane.
          . (("\\(?:^\\|[^$]\\)\"\\(?:[^\"\n]\\|\\\\\"\\)*\\(\\$\\)\"" 1 "w")
             ;; And the dollar sign in $\" escapes two characters, not
             ;; just one.
             ("\\(\\$\\)\\\\\\\"" 1 "'"))))))



;; Useful when defining your own keywords.
(defun erlang-font-lock-set-face (ks &rest faces)
  "Replace the face components in a list of keywords.

The first argument, KS, is a list of keywords.  The rest of the
arguments are expressions to replace the face information with.  The
first expression replaces the face of the first keyword, the second
expression the second keyword etc.

Should an expression be nil, the face of the corresponding keyword is
not changed.

Should fewer expressions than keywords be given, the last expression
is used for all remaining keywords.

Normally, the expressions are just atoms representing the new face.
They could however be more complex, returning different faces in
different situations.

This function only handles keywords with elements on the forms:
  (REGEXP NUMBER FACE)
  (REGEXP NUMBER FACE OVERWRITE)

This could be used when defining your own special font-lock setup, e.g:

\(setq my-font-lock-keywords
      (append erlang-font-lock-keywords-function-header
              erlang-font-lock-keywords-dollar
              (erlang-font-lock-set-face
               erlang-font-lock-keywords-macros 'my-neon-green-face)
              (erlang-font-lock-set-face
               erlang-font-lock-keywords-lc 'my-deep-red 'my-light-red)
              erlang-font-lock-keywords-attr))

For a more elaborate example, please see the beginning of the file
`erlang.el'."
  (let ((res '()))
    (while ks
      (let* ((regexp (car (car ks)))
	     (number (car (cdr (car ks))))
	     (new-face (if (and faces (car faces))
			   (car faces)
			 (car (cdr (cdr (car ks))))))
	     (overwrite (car (cdr (cdr (cdr (car ks))))))
	     (new-keyword (list regexp number new-face)))
	(if overwrite (nconc new-keyword (list overwrite)))
	(setq res (cons new-keyword res))
	(setq ks (cdr ks))
	(if (and faces (cdr faces))
	    (setq faces (cdr faces)))))
    (nreverse res)))


(defun erlang-font-lock-level-0 ()
  ;; DocStringOrig: font-cmd
  "Unfontify current buffer."
  (interactive)
  (font-lock-mode 0))


(defun erlang-font-lock-level-1 ()
  ;; DocStringCopy: font-cmd
  "Fontify current buffer at level 1.
This highlights function headers, reserved keywords, strings and comments."
  (interactive)
  (require 'font-lock)
  (set 'font-lock-keywords erlang-font-lock-keywords-1)
  (font-lock-mode 1)
  (funcall (symbol-function 'font-lock-fontify-buffer)))


(defun erlang-font-lock-level-2 ()
  ;; DocStringCopy: font-cmd
  "Fontify current buffer at level 2.
This highlights level 1 features (see `erlang-font-lock-level-1')
plus bifs, guards and `single quotes'."
  (interactive)
  (require 'font-lock)
  (set 'font-lock-keywords erlang-font-lock-keywords-2)
  (font-lock-mode 1)
  (funcall (symbol-function 'font-lock-fontify-buffer)))


(defun erlang-font-lock-level-3 ()
  ;; DocStringCopy: font-cmd
  "Fontify current buffer at level 3.
This highlights level 2 features (see `erlang-font-lock-level-2')
plus variables, macros and records."
  (interactive)
  (require 'font-lock)
  (set 'font-lock-keywords erlang-font-lock-keywords-3)
  (font-lock-mode 1)
  (funcall (symbol-function 'font-lock-fontify-buffer)))

(defun erlang-font-lock-level-4 ()
  ;; DocStringCopy: font-cmd
  "Fontify current buffer at level 4.
This highlights level 3 features (see `erlang-font-lock-level-2')
plus variables, macros and records."
  (interactive)
  (require 'font-lock)
  (set 'font-lock-keywords erlang-font-lock-keywords-4)
  (font-lock-mode 1)
  (funcall (symbol-function 'font-lock-fontify-buffer)))


(defun erlang-menu-init ()
  "Init menus for Erlang mode.

The variable `erlang-menu-items' contain a description of the Erlang
mode menu.  Normally, the list contains atoms, representing variables
bound to pieces of the menu.

Personal extensions could be added to `erlang-menu-personal-items'.

This function should be called if any variable describing the
menu configuration is changed."
  (erlang-menu-install "Erlang" erlang-menu-items erlang-mode-map t))


(defun erlang-menu-install (name items keymap &optional popup)
  "Install a menu in Emacs or XEmacs based on an abstract description.

NAME is the name of the menu.

ITEMS is a list.  The elements are either nil representing a horizontal
line or a list with two or three elements.  The first is the name of
the menu item, the second the function to call, or a submenu, on the
same same form as ITEMS.  The third optional element is an expression
which is evaluated every time the menu is displayed.  Should the
expression evaluate to nil the menu item is ghosted.

KEYMAP is the keymap to add to menu to.  (When using XEmacs, the menu
will only be visible when this menu is the global, the local, or an
activate minor mode keymap.)

If POPUP is non-nil, the menu is bound to the XEmacs `mode-popup-menu'
variable, i.e. it will popup when pressing the right mouse button.

Please see the variable `erlang-menu-base-items'."
  (cond (erlang-xemacs-p
	 (let ((menu (erlang-menu-xemacs name items keymap)))
	   ;; We add the menu to the global menubar.
	   ;;(funcall (symbol-function 'set-buffer-menubar)
	   ;;         (symbol-value 'current-menubar))
	   (funcall (symbol-function 'add-submenu) nil menu)
	   (setcdr erlang-xemacs-popup-menu (cdr menu))
	   (if (and popup (boundp 'mode-popup-menu))
	       (funcall (symbol-function 'set)
			'mode-popup-menu erlang-xemacs-popup-menu))))
	((>= erlang-emacs-major-version 19)
	 (define-key keymap (vector 'menu-bar (intern name))
	   (erlang-menu-make-keymap name items)))
	(t nil)))


(defun erlang-menu-make-keymap (name items)
  "Build a menu for Emacs 19."
  (let ((menumap (funcall (symbol-function 'make-sparse-keymap)
			  name))
	(count 0)
	id def first second third)
    (setq items (reverse items))
    (while items
      ;; Replace any occurrence of atoms by their value.
      (while (and items (atom (car items)) (not (null (car items))))
	(if (and (boundp (car items))
		 (listp (symbol-value (car items))))
	    (setq items (append (reverse (symbol-value (car items)))
				(cdr items)))
	  (setq items (cdr items))))
      (setq first (car-safe (car items)))
      (setq second (car-safe (cdr-safe (car items))))
      (setq third (car-safe (cdr-safe (cdr-safe (car items)))))
      (cond ((null first)
	     (setq count (+ count 1))
	     (setq id (intern (format "separator-%d" count)))
	     (setq def '("--" . nil)))
	    ((and (consp second) (eq (car second) 'lambda))
	     (setq count (+ count 1))
	     (setq id (intern (format "lambda-%d" count)))
	     (setq def (cons first second)))
	    ((symbolp second)
	     (setq id second)
	     (setq def (cons first second)))
	    (t
	     (setq count (+ count 1))
	     (setq id (intern (format "submenu-%d" count)))
	     (setq def (erlang-menu-make-keymap first second))))
      (define-key menumap (vector id) def)
      (if third
	  (put id 'menu-enable third))
      (setq items (cdr items)))
    (cons name menumap)))


(defun erlang-menu-xemacs (name items &optional keymap)
  "Build a menu for XEmacs."
  (let ((res '())
	first second third entry)
    (while items
      ;; Replace any occurrence of atoms by their value.
      (while (and items (atom (car items)) (not (null (car items))))
	(if (and (boundp (car items))
		 (listp (symbol-value (car items))))
	    (setq items (append (reverse (symbol-value (car items)))
				(cdr items)))
	  (setq items (cdr items))))
      (setq first (car-safe (car items)))
      (setq second (car-safe (cdr-safe (car items))))
      (setq third (car-safe (cdr-safe (cdr-safe (car items)))))
      (cond ((null first)
	     (setq res (cons "------" res)))
	    ((symbolp second)
	     (setq res (cons (vector first second (or third t)) res)))
	    ((and (consp second) (eq (car second) 'lambda))
	     (setq res (cons (vector first (list 'call-interactively second)
				     (or third t)) res)))
	    (t
	     (setq res (cons (cons first
				   (cdr (erlang-menu-xemacs
					 first second)))
			     res))))
      (setq items (cdr items)))
    (setq res (reverse res))
    ;; When adding a menu to a minor-mode keymap under Emacs,
    ;; it disappears when the mode is disabled.  The expression
    ;; generated below imitates this behaviour.
    ;; (This could be expressed much clearer using backquotes,
    ;; but I don't want to pull in every package.)
    (if keymap
	(let ((expr (list 'or
			  (list 'eq keymap 'global-map)
			  (list 'eq keymap (list 'current-local-map))
			  (list 'symbol-value
				(list 'car-safe
				      (list 'rassq
					    keymap
					    'minor-mode-map-alist))))))
	  (setq res (cons ':included (cons expr res)))))
    (cons name res)))


(defun erlang-menu-substitute (items alist)
  "Substitute functions in menu described by ITEMS.

The menu ITEMS is updated destructively.

ALIST is list of pairs where the car is the old function and cdr the new."
  (let (first second pair)
    (while items
      (setq first (car-safe (car items)))
      (setq second (car-safe (cdr-safe (car items))))
      (cond ((null first))
	    ((symbolp second)
	     (setq pair (and second (assq second alist)))
	     (if pair
		 (setcar (cdr (car items)) (cdr pair))))
	    ((and (consp second) (eq (car second) 'lambda)))
	    (t
	     (erlang-menu-substitute second alist)))
      (setq items (cdr items)))))


(defun erlang-menu-add-above (entry above items)
  "Add menu ENTRY above menu entry ABOVE in menu ITEMS.
Do nothing if the items already should be in the menu.
Should ABOVE not be in the list, the entry is added at
the bottom of the menu.

The new menu is returned.  No guarantee is given that the original
menu is left unchanged.

The equality test is performed by `eq'.

Example:  (erlang-menu-add-above 'my-erlang-menu-items
                                 'erlang-menu-man-items)"
  (erlang-menu-add-below entry above items t))


(defun erlang-menu-add-below (entry below items &optional above-p)
  "Add menu ENTRY below menu items BELOW in the Erlang menu.
Do nothing if the items already should be in the menu.
Should BELOW not be in the list, items is added at the bottom
of the menu.

The new menu is returned.  No guarantee is given that the original
menu is left unchanged.

The equality test is performed by `eq'.

Example:

\(setq erlang-menu-items
      (erlang-menu-add-below 'my-erlang-menu-items
	 	             'erlang-menu-base-items
                             erlang-menu-items))"
  (if (memq entry items)
      items				; Return the original menu.
    (let ((head '())
	  (done nil)
	  res)
      (while (not done)
	(cond ((null items)
	       (setq res (append head (list entry)))
	       (setq done t))
	      ((eq below (car items))
	       (setq res
		     (if above-p
			 (append head (cons entry items))
		       (append head (cons (car items)
					  (cons entry (cdr items))))))
	       (setq done t))
	      (t
	       (setq head (append head (list (car items))))
	       (setq items (cdr items)))))
      res)))

(defun erlang-menu-delete (entry items)
  "Delete ENTRY from menu ITEMS.

The new menu is returned.  No guarantee is given that the original
menu is left unchanged."
  (delq entry items))

;; Man code:

(defun erlang-man-init ()
  "Add menus containing the manual pages of the Erlang.

The variable `erlang-man-dirs' contains entries describing
the location of the manual pages."
  (interactive)
  (if erlang-man-inhibit
      ()
    (setq erlang-menu-man-items
	  '(nil
	    ("Man - Function" erlang-man-function)))
    (if erlang-man-dirs
	(setq erlang-menu-man-items
	      (append erlang-menu-man-items
		      (erlang-man-make-top-menu erlang-man-dirs))))
    (setq erlang-menu-items
	  (erlang-menu-add-above 'erlang-menu-man-items
				 'erlang-menu-version-items
				 erlang-menu-items))
    (erlang-menu-init)))


(defun erlang-man-uninstall ()
  "Remove the man pages from the Erlang mode."
  (interactive)
  (setq erlang-menu-items
	(erlang-menu-delete 'erlang-menu-man-items erlang-menu-items))
  (erlang-menu-init))


;; The man menu is a hierarchal structure, with the manual sections
;; at the top, described by `erlang-man-dirs'.  The next level could
;; either be the manual pages if not to many, otherwise it is an index
;; menu whose submenus will contain up to `erlang-man-max-menu-size'
;; manual pages.

(defun erlang-man-make-top-menu (dir-list)
  "Create one menu entry per element of DIR-LIST.
The format is described in the documentation of `erlang-man-dirs'."
  (let ((menu '())
	dir)
    (while dir-list
      (setq dir (cond ((nth 2 (car dir-list))
		       ;; Relative to `erlang-root-dir'.
		       (and (stringp erlang-root-dir)
			    (concat erlang-root-dir (nth 1 (car dir-list)))))
		      (t
		       ;; Absolute
		       (nth 1 (car dir-list)))))
      (if (and dir
	       (file-readable-p dir))
	  (setq menu (cons (list (car (car dir-list))
				 (erlang-man-make-middle-menu
				  (erlang-man-get-files dir)))
			   menu)))
      (setq dir-list (cdr dir-list)))
    ;; Should no menus be found, generate a menu item which
    ;; will display a help text, when selected.
    (if menu
	(nreverse menu)
      '(("Man Pages"
	 (("Error! Why?" erlang-man-describe-error)))))))


;; Should the menu be to long, let's split it into a number of
;; smaller menus.  Warning, this code contains beautiful
;; destructive operations!
(defun erlang-man-make-middle-menu (filelist)
  "Create the second level menu from FILELIST.

Should the list be longer than `erlang-man-max-menu-size', a tree of
menus is created."
  (if (<= (length filelist) erlang-man-max-menu-size)
      (erlang-man-make-menu filelist)
    (let ((menu '())
	  (filelist (copy-sequence filelist))
	  segment submenu pair)
      (while filelist
	(setq pair (nthcdr (- erlang-man-max-menu-size 1) filelist))
	(setq segment filelist)
	(if (null pair)
	    (setq filelist nil)
	  (setq filelist (cdr pair))
	  (setcdr pair nil))
	(setq submenu (erlang-man-make-menu segment))
	(setq menu (cons (list (concat (car (car submenu))
				       " -- "
				       (car (car (reverse submenu))))
			       submenu)
			 menu)))
      (nreverse menu))))


(defun erlang-man-make-menu (filelist)
  "Make a leaf menu based on FILELIST."
  (let ((menu '())
	item)
    (while filelist
      (setq item (erlang-man-make-menu-item (car filelist)))
      (if item
	  (setq menu (cons item menu)))
      (setq filelist (cdr filelist)))
    (nreverse menu)))


(defun erlang-man-make-menu-item (file)
  "Create a menu item containing the name of the man page."
  (and (string-match ".+/\\([^/]+\\)\\.\\([124-9]\\|3\\(erl\\)?\\)\\(\\.gz\\)?$" file)
       (let ((page (substring file (match-beginning 1) (match-end 1))))
	 (list (capitalize page)
	       (list 'lambda '()
		     '(interactive)
		     (list 'funcall 'erlang-man-display-function
			   file))))))


(defun erlang-man-get-files (dir)
  "Return files in directory DIR."
  (directory-files dir t ".+\\.\\([124-9]\\|3\\(erl\\)?\\)\\(\\.gz\\)?\\'"))


(defun erlang-man-module (&optional module)
  "Find manual page for MODULE, defaults to module of function under point.
This function is aware of imported functions."
  (interactive
   (list (let* ((mod (car-safe (erlang-get-function-under-point)))
		(input (read-string
			(format "Manual entry for module%s: "
				(if (or (null mod) (string= mod ""))
				    ""
				  (format " (default %s)" mod))))))
	   (if (string= input "")
	       mod
	     input))))
  (or module (setq module (car (erlang-get-function-under-point))))
  (if (or (null module) (string= module ""))
      (error "No Erlang module name given"))
  (let ((dir-list erlang-man-dirs)
	(pat (concat "/" (regexp-quote module) "\\.\\([124-9]\\|3\\(erl\\)?\\)\\(\\.gz\\)?$"))
	(file nil)
	file-list)
    (while (and dir-list (null file))
      (setq file-list (erlang-man-get-files
		       (if (nth 2 (car dir-list))
			   (concat erlang-root-dir (nth 1 (car dir-list)))
			 (nth 1 (car dir-list)))))
      (while (and file-list (null file))
	(if (string-match pat (car file-list))
	    (setq file (car file-list)))
	(setq file-list (cdr file-list)))
      (setq dir-list (cdr dir-list)))
    (if file
	(funcall erlang-man-display-function file)
      (error "No manual page for module %s found" module))))


;; Warning, the function `erlang-man-function' is a hack!
;; It links itself into the man code in a non-clean way.  I have
;; chosen to keep it since it provides a very useful functionality
;; which is not possible to achieve using a clean approach.
;;   / AndersL

(defvar erlang-man-function-name nil
  "Name of function for last `erlang-man-function' call.
Used for communication between `erlang-man-function' and the
patch to `Man-notify-when-ready'.")

(defun erlang-man-function (&optional name)
  "Find manual page for NAME, where NAME is module:function.
The entry for `function' is displayed.

This function is aware of imported functions."
  (interactive
   (list (let* ((mod-func (erlang-get-function-under-point))
		(mod (car-safe mod-func))
		(func (nth 1 mod-func))
		(input (read-string
			(format
			 "Manual entry for `module:func' or `module'%s: "
			 (if (or (null mod) (string= mod ""))
			     ""
			   (format " (default %s:%s)" mod func))))))
	   (if (string= input "")
	       (if (and mod func)
		   (concat mod ":" func)
		 mod)
	     input))))
  ;; Emacs 18 doesn't provide `man'...
  (condition-case nil
      (require 'man)
    (error nil))
  (let ((modname nil)
	(funcname nil))
    (cond ((null name)
	   (let ((mod-func (erlang-get-function-under-point)))
	     (setq modname (car-safe mod-func))
	     (setq funcname (nth 1 mod-func))))
	  ((string-match ":" name)
	   (setq modname (substring name 0 (match-beginning 0)))
	   (setq funcname (substring name (match-end 0) nil)))
	  ((stringp name)
	   (setq modname name)))
    (if (or (null modname) (string= modname ""))
	(error "No Erlang module name given"))
    (cond ((fboundp 'Man-notify-when-ready)
	   ;; Emacs 19:  The man command could possibly start an
	   ;; asynchronous process, i.e. we must hook ourselves into
	   ;; the system to be activated when the man-process
	   ;; terminates.
	   (if (null funcname)
	       ()
	     (erlang-man-patch-notify)
	     (setq erlang-man-function-name funcname))
	   (condition-case nil
	       (erlang-man-module modname)
	     (error (setq erlang-man-function-name nil))))
	  (t
	   (erlang-man-module modname)
	   (if funcname
	       (erlang-man-find-function
		(or (get-buffer "*Manual Entry*") ; Emacs 18
		    (current-buffer))	; XEmacs
		funcname))))))


;; Should the defadvice be at the top level, the package `advice' would
;; be required.  Now it is only required when this functionality
;; is used.  (Emacs 19 specific.)
(defun erlang-man-patch-notify ()
  "Patch the function `Man-notify-when-ready' to search for function.
The variable `erlang-man-function-name' is assumed to be bound to
the function name, or to nil.

The reason for patching a function is that under Emacs 19, the man
command is executed asynchronously."
  (condition-case nil
      (require 'advice)
    ;; This should never happened since this is only called when
    ;; running under Emacs 19.
    (error (error (concat "This command needs the package `advice', "
			  "please upgrade your Emacs."))))
  (require 'man)
  (defadvice Man-notify-when-ready
    (after erlang-Man-notify-when-ready activate)
    "Set point at the documentation of the function name in
`erlang-man-function-name' when the man page is displayed."
    (if erlang-man-function-name
	(erlang-man-find-function (ad-get-arg 0) erlang-man-function-name))
    (setq erlang-man-function-name nil)))


(defun erlang-man-find-function (buf func)
  "Find manual page for function in `erlang-man-function-name' in buffer BUF."
  (if func
      (let ((win (get-buffer-window buf)))
	(if win
	    (progn
	      (set-buffer buf)
	      (goto-char (point-min))
	      (if (re-search-forward
		   (concat "^[ \t]+" func " ?(")
		   (point-max) t)
		  (progn
		    (forward-word -1)
		    (set-window-point win (point)))
		(message "Could not find function `%s'" func)))))))


(defun erlang-man-display (file)
  "Display FILE as a `man' file.
This is the default manual page display function.
The variables `erlang-man-display-function' contains the function
to be used."
  ;; Emacs 18 doesn't `provide' man.
  (condition-case nil
      (require 'man)
    (error nil))
  (if file
      (let ((process-environment (copy-sequence process-environment)))
	(if (string-match "\\(.*\\)/man[^/]*/\\([^.]+\\)\\.\\([124-9]\\|3\\(erl\\)?\\)\\(\\.gz\\)?$" file)
	    (let ((dir (substring file (match-beginning 1) (match-end 1)))
		  (page (substring file (match-beginning 2) (match-end 2))))
	      (if (fboundp 'setenv)
		  (setenv "MANPATH" dir)
		;; Emacs 18
		(setq process-environment (cons (concat "MANPATH=" dir)
						process-environment)))
	      (cond ((not (and (not erlang-xemacs-p)
			       (= erlang-emacs-major-version 19)
			       (< erlang-emacs-minor-version 29)))
		     (manual-entry page))
		    (t
		     ;; Emacs 19.28 and earlier versions of 19:
		     ;; The manual-entry command unconditionally prompts
		     ;; the user :-(
		     (funcall (symbol-function 'Man-getpage-in-background)
			      page))))
	  (error "Can't find man page for %s\n" file)))))


(defun erlang-man-describe-error ()
  "Describe why the manual pages weren't found."
  (interactive)
  (with-output-to-temp-buffer "*Erlang Man Error*"
    (princ "Normally, this menu should contain Erlang manual pages.

In order to find the manual pages, the variable `erlang-root-dir'
should be bound to the name of the directory containing the Erlang
installation.  The name should not include the final slash.

Practically, you should add a line on the following form to
your ~/.emacs, or ask your system administrator to add it to
the site init file:
    (setq erlang-root-dir \"/the/erlang/root/dir/goes/here\")

For example:
    (setq erlang-root-dir \"/usr/local/erlang\")

After installing the line, kill and restart Emacs, or restart Erlang
mode with the command `M-x erlang-mode RET'.")))

;; Skeleton code:

;; This code is based on the package `tempo' which is part of modern
;; Emacsen.  (GNU Emacs 19.25 (?) and XEmacs 19.14.)

(defun erlang-skel-init ()
  "Generate the skeleton functions and menu items.
The variable `erlang-skel' contains the name and descriptions of
all skeletons.

The skeleton routines are based on the `tempo' package.  Should this
package not be present, this function does nothing."
  (interactive)
  (condition-case nil
      (require 'tempo)
    (error t))
  (if (featurep 'tempo)
      (let ((skel erlang-skel)
	    (menu '()))
	(while skel
	  (cond ((null (car skel))
		 (setq menu (cons nil menu)))
		(t
		 (funcall (symbol-function 'tempo-define-template)
			  (concat "erlang-" (nth 1 (car skel)))
			  ;; The tempo template used contains an `include'
			  ;; function call only, hence changes to the
			  ;; variables describing the templates take effect
			  ;; immdiately.
			  (list (list 'erlang-skel-include (nth 2 (car skel))))
			  (nth 1 (car skel))
			  (car (car skel))
			  'erlang-tempo-tags)
		 (setq menu (cons (erlang-skel-make-menu-item
				   (car skel)) menu))))
	  (setq skel (cdr skel)))
	(setq erlang-menu-skel-items
	      (list nil (list "Skeletons" (nreverse menu))))
	(setq erlang-menu-items
	      (erlang-menu-add-above 'erlang-menu-skel-items
				     'erlang-menu-version-items
				     erlang-menu-items))
	(erlang-menu-init))))

(defun erlang-skel-make-menu-item (skel)
  (let ((func (intern (concat "tempo-template-erlang-" (nth 1 skel)))))
    (cond ((null (nth 3 skel))
	   (list (car skel) func))
	  (t
	   (list (car skel)
		 (list 'lambda '()
		       '(interactive)
		       (list 'funcall
			     (list 'quote (nth 3 skel))
			     (list 'quote func))))))))

;; Functions designed to be added to the skeleton menu.
;; (Not normally used)
(defun erlang-skel-insert (func)
  "Insert skeleton generated by FUNC and goto first tempo mark."
  (save-excursion (funcall func))
  (funcall (symbol-function 'tempo-forward-mark)))

(defun erlang-skel-header (func)
  "Insert the header generated by FUNC at the beginning of the buffer."
  (goto-char (point-min))
  (save-excursion (funcall func))
  (funcall (symbol-function 'tempo-forward-mark)))


;; Functions used inside the skeleton descriptions.
(defun erlang-skel-skip-blank ()
  (skip-chars-backward " \t")
  nil)

(defun erlang-skel-include (&rest args)
  "Include a template inside another template.

Example of use, assuming that `erlang-skel-func' is defined:

 (defvar foo-skeleton '(\"%%% New function:\"
                        (erlang-skel-include erlang-skel-func)))

Technically, this function returns the `tempo' attribute`(l ...)' which
can contain other `tempo' attributes.  Please see the function
`tempo-define-template' for a description of the `(l ...)' attribute."
  (let ((res '())
	entry)
    (while args
      (setq entry (car args))
      (while entry
	(setq res (cons (car entry) res))
	(setq entry (cdr entry)))
      (setq args (cdr args)))
    (cons 'l (nreverse res))))

(defvar erlang-skel-separator-length 70)

(defun erlang-skel-separator (&optional percent)
  "Return a comment separator."
  (let ((percent (or percent 3)))
    (concat (make-string percent ?%) 
	    (make-string (- erlang-skel-separator-length percent) ?-) 
	    "\n")))

(defun erlang-skel-double-separator (&optional percent)
  "Return a comment separator."
  (let ((percent (or percent 3)))
    (concat (make-string percent ?%) 
	    (make-string (- erlang-skel-separator-length percent) ?=) 
	    "\n")))

(defun erlang-skel-dd-mmm-yyyy ()
  "Return the current date as a string in \"DD Mon YYYY\" form.
The first character of DD is space if the value is less than 10."
  (let ((date (current-time-string)))
    (format "%2d %s %s"
	    (erlang-string-to-int (substring date 8 10))
	    (substring date 4 7)
	    (substring date -4))))

;; Indentation code:

(defun erlang-indent-command (&optional whole-exp)
  "Indent current line as Erlang code.
With argument, indent any additional lines of the same clause
rigidly along with this one."
  (interactive "P")
  (if whole-exp
      ;; If arg, always indent this line as Erlang
      ;; and shift remaining lines of clause the same amount.
      (let ((shift-amt (erlang-indent-line))
	    beg end)
	(save-excursion
	  (if erlang-tab-always-indent
	      (beginning-of-line))
	  (setq beg (point))
	  (erlang-end-of-clause 1)
	  (setq end (point))
	  (goto-char beg)
	  (forward-line 1)
	  (setq beg (point)))
	(if (> end beg)
	    (indent-code-rigidly beg end shift-amt "\n")))
    (if (and (not erlang-tab-always-indent)
	     (save-excursion
	       (skip-chars-backward " \t")
	       (not (bolp))))
	(insert-tab)
      (erlang-indent-line))))


(defun erlang-indent-line ()
  "Indent current line as Erlang code.
Return the amount the indentation changed by."
  (let ((pos (- (point-max) (point)))
	indent beg
	shift-amt)
    (beginning-of-line 1)
    (setq beg (point))
    (skip-chars-forward " \t")
    (cond ((looking-at "%")
	   (setq indent (funcall comment-indent-function))
	   (setq shift-amt (- indent (current-column))))
	  (t
	   (setq indent (erlang-calculate-indent))
	   (cond ((null indent)
		  (setq indent (current-indentation)))
		 ((eq indent t)
		  ;; This should never occur here.
		  (error "Erlang mode error"))
		 ;;((= (char-syntax (following-char)) ?\))
		 ;; (setq indent (1- indent)))
		 )
	   (setq shift-amt (- indent (current-column)))))
    (if (zerop shift-amt)
	nil
      (delete-region beg (point))
      (indent-to indent))
    ;; If initial point was within line's indentation, position
    ;; after the indentation. Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos)))
    shift-amt))


(defun erlang-indent-region (beg end)
  "Indent region of Erlang code.

This is automagically called by the user level function `indent-region'."
  (interactive "r")
  (save-excursion
    (let ((case-fold-search nil)
	  (continue t)
	  (from-end (- (point-max) end))
	  indent-point;; The beginning of the current line
	  indent;; The indent amount
	  state)
      (goto-char beg)
      (beginning-of-line)
      (setq indent-point (point))
      (erlang-beginning-of-clause)
      ;; Parse the Erlang code from the beginning of the clause to
      ;; the beginning of the region.
      (while (< (point) indent-point)
	(setq state (erlang-partial-parse (point) indent-point state)))
      ;; Indent every line in the region
      (while continue
	(goto-char indent-point)
	(skip-chars-forward " \t")
	(cond ((looking-at "%")
	       ;; Do not use our stack to help the user to customize
	       ;; comment indentation.
	       (setq indent (funcall comment-indent-function)))
	      ((looking-at "$")
	       ;; Don't indent empty lines.
	       (setq indent 0))
	      (t
	       (setq indent
		     (save-excursion
		       (erlang-calculate-stack-indent (point) state)))
	       (cond ((null indent)
		      (setq indent (current-indentation)))
		     ((eq indent t)
		      ;; This should never occur here.
		      (error "Erlang mode error"))
		     ;;((= (char-syntax (following-char)) ?\))
		     ;; (setq indent (1- indent)))
		     )))
	(if (zerop (- indent (current-column)))
	    nil
	  (delete-region indent-point (point))
	  (indent-to indent))
	;; Find the next line in the region
	(goto-char indent-point)
	(save-excursion
	  (forward-line 1)
	  (setq indent-point (point)))
	(if (>= from-end (- (point-max) indent-point))
	    (setq continue nil)
	  (while (< (point) indent-point)
	    (setq state (erlang-partial-parse
			 (point) indent-point state))))))))


(defun erlang-indent-current-buffer ()
  "Indent current buffer as Erlang code."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (erlang-indent-region (point-min) (point-max)))))


(defun erlang-indent-function ()
  "Indent current Erlang function."
  (interactive)
  (save-excursion
    (let ((end (progn (erlang-end-of-function 1) (point)))
	  (beg (progn (erlang-beginning-of-function 1) (point))))
      (erlang-indent-region beg end))))


(defun erlang-indent-clause ()
  "Indent current Erlang clause."
  (interactive)
  (save-excursion
    (let ((end (progn (erlang-end-of-clause 1) (point)))
	  (beg (progn (erlang-beginning-of-clause 1) (point))))
      (erlang-indent-region beg end))))


(defmacro erlang-push (x stack) (list 'setq stack (list 'cons x stack)))
(defmacro erlang-pop (stack) (list 'setq stack (list 'cdr stack)))
;; Would much prefer to make caddr a macro but this clashes.
(defun erlang-caddr (x) (car (cdr (cdr x))))


(defun erlang-calculate-indent (&optional parse-start)
  "Compute appropriate indentation for current line as Erlang code.
Return nil if line starts inside string, t if in a comment."
  (save-excursion
    (let ((indent-point (point))
	  (case-fold-search nil)
	  (state nil))
      (if parse-start
	  (goto-char parse-start)
	(erlang-beginning-of-clause))
      (while (< (point) indent-point)
	(setq state (erlang-partial-parse (point) indent-point state)))
      (erlang-calculate-stack-indent indent-point state))))

(defun erlang-show-syntactic-information ()
  "Show syntactic information for current line."

  (interactive)

  (save-excursion
    (let ((starting-point (point))
	  (case-fold-search nil)
	  (state nil))
      (erlang-beginning-of-clause)
      (while (< (point) starting-point)
	(setq state (erlang-partial-parse (point) starting-point state)))
      (message "%S" state))))


(defun erlang-partial-parse (from to &optional state)
  "Parse Erlang syntax starting at FROM until TO, with an optional STATE.
Value is list (stack token-start token-type in-what)."
  (goto-char from)			; Start at the beginning
  (erlang-skip-blank to)
  (let ((cs (char-syntax (following-char)))
	(stack (car state))
	(token (point))
	in-what)
    (cond 
     
     ;; Done: Return previous state.
     ((>= token to)
      (setq token (nth 1 state))
      (setq cs (nth 2 state))
      (setq in-what (nth 3 state)))

     ;; Word constituent: check and handle keywords.
     ((= cs ?w)
      (cond ((looking-at "\\(end\\|after\\)[^_a-zA-Z0-9]")
	     ;; Must pop top icr layer, `after' will push a new
	     ;; layer next.
	     (progn
	       (while (and stack (eq (car (car stack)) '->))
		 (erlang-pop stack))
	       (if (and stack (memq (car (car stack)) '(icr begin fun try)))
		   (erlang-pop stack))))
	    ((looking-at "catch.*of")
	     t)
	    ((looking-at "catch\\s *\\($\\|%\\|.*->\\)")
	     ;; Must pop top icr layer, `catch' in try/catch
	     ;;will push a new layer next.
	     (progn
	       (while (and stack (eq (car (car stack)) '->))
		 (erlang-pop stack))
	       (if (and stack (memq (car (car stack)) '(icr begin try)))
		   (erlang-pop stack))))
	    )  
      (cond ((looking-at "\\(if\\|case\\|receive\\)[^_a-zA-Z0-9]")
	     ;; Must push a new icr (if/case/receive) layer.
	     (erlang-push (list 'icr token (current-column)) stack))
	    ((looking-at "\\(try\\|after\\)[^_a-zA-Z0-9]")
	     ;; Must handle separately, try catch or try X of -> catch
	     ;; same for `after', it could be 
	     ;; receive after Time -> X end, or 
	     ;; try after X end
	     (erlang-push (list 'try token (current-column)) stack))
	    ((looking-at "\\(of\\)[^_a-zA-Z0-9]")
	     ;; Must handle separately, try X of -> catch
	     (if (and stack (eq (car (car stack)) 'try))
		 (let ((try-column (nth 2 (car stack)))
		       (try-pos (nth 1 (car stack))))
		   (erlang-pop stack)
		   (erlang-push (list 'icr try-pos try-column) stack))))
	    
	    ((looking-at "\\(fun\\)[^_a-zA-Z0-9]")
	     ;; Push a new layer if we are defining a `fun'
	     ;; expression, not when we are refering an existing
	     ;; function.  'fun's defines are only indented one level now.
	     (if (save-excursion
		   (goto-char (match-end 1))
		   (erlang-skip-blank to)
		   (eq (following-char) ?\())
		 (erlang-push (list 'fun token (current-column)) stack)))
	    ((looking-at "\\(begin\\|query\\)[^_a-zA-Z0-9]")
	     (erlang-push (list 'begin token (current-column)) stack))
	    ;; Normal when case
	    ;;((looking-at "when\\s ")
	    ;;((looking-at "when\\s *\\($\\|%\\)")
	    ((looking-at "when[^_a-zA-Z0-9]")
	     (erlang-push (list 'when token (current-column)) stack))
	    ((looking-at "catch.*of")
	     t)
	    ((looking-at "catch\\s *\\($\\|%\\|.*->\\)")
	     (erlang-push (list 'icr token (current-column)) stack))
	    ;;(erlang-push (list '-> token (current-column)) stack))
	    ;;((looking-at "^of$") 
	    ;; (erlang-push (list 'icr token (current-column)) stack)
	    ;;(erlang-push (list '-> token (current-column)) stack))
	    )
      (forward-sexp 1))
      ;; String: Try to skip over it. (Catch error if not complete.)
      ((= cs ?\")
       (condition-case nil
	   (progn
	    (forward-sexp 1)
	    (if (> (point) to)
		(progn
		  (setq in-what 'string)
		  (goto-char to))))
	(error
	 (setq in-what 'string)
	 (goto-char to))))

     ;; Expression prefix e.i. $ or ^ (Note ^ can be in the character
     ;; literal $^ or part of string and $ outside of a string denotes
     ;; a character literal)
     ((= cs ?')
      (cond 
       ((= (following-char) ?\") ;; $ or ^ was the last char in a string
	(forward-char 1))
       (t
	;; Maybe a character literal, quote the next char to avoid
	;; situations as $" being seen as the begining of a string.
	;; Note the quoting something in the middle of a string is harmless.
	(quote (following-char)) 
	(forward-char 1))))

     ;; Symbol constituent or punctuation
     
     ((memq cs '(?. ?_))
      (cond 
       
       ;; Clause end
       ((= (following-char) ?\;)
	(if (eq (car (car (last stack))) 'spec)
	    (while (memq (car (car stack)) '(when ::))
	      (erlang-pop stack)))
	(if (and stack (eq (car (car stack)) '->))
	    (erlang-pop stack))
	(forward-char 1))
       
       ;; Parameter separator
       ((looking-at ",")
	(forward-char 1)
	(if (and stack (eq (car (car stack)) '::))
	    ;; Type or spec
	    (erlang-pop stack)))

       ;; Function end
       ((looking-at "\\.\\(\\s \\|\n\\|\\s<\\)")
	(setq stack nil)
	(forward-char 1))
       
       ;; Function head
       ((looking-at "->")
	(if (and stack (eq (car (car stack)) 'when))
	    (erlang-pop stack))
	(erlang-push (list '-> token (current-column)) stack)
	(forward-char 2)) 
       
       ;; List-comprehension divider
       ((looking-at "||")
	(erlang-push (list '|| token (current-column)) stack)
	(forward-char 2))

       ;; Bit-syntax open paren
       ((looking-at "<<")
	(erlang-push (list '<< token (current-column)) stack)
	(forward-char 2))
       
       ;; Bbit-syntax close paren
       ((looking-at ">>")
	(while (memq (car (car stack)) '(|| ->))
	  (erlang-pop stack))
	(cond ((eq (car (car stack)) '<<)
	       (erlang-pop stack))
	      ((memq (car (car stack)) '(icr begin fun)) 
	       (error "Missing `end'"))
	      (t
	       (error "Unbalanced parentheses")))
	(forward-char 2))
       
       ;; Macro
       ((= (following-char) ??)
	;; Skip over the ? 
	(forward-char 1)
	)

       ;; Type spec's
       ((looking-at "-type\\s \\|-opaque\\s ")
	(if stack
	    (forward-char 1)
	  (erlang-push (list 'icr token (current-column)) stack)
	  (forward-char 6)))
       ((looking-at "-spec\\s ")
	(if stack
	    (forward-char 1)
	  (forward-char 6) 
	  (skip-chars-forward "^(\n")
	  (erlang-push (list 'spec (point) (current-column)) stack)
	  ))

       ;; Type spec delimiter
       ((looking-at "::")
	(erlang-push (list ':: token (current-column)) stack)
	(forward-char 2))
       
       ;; Don't follow through in the clause below 
       ;; '|' don't need spaces around it 
       ((looking-at "|")
	(forward-char 1))
       
       ;; Other punctuation: Skip over it and any following punctuation
       ((= cs ?.)
	;; Skip over all characters in the operand.
	(skip-syntax-forward "."))
       
       ;; Other char: Skip over it.
       (t
	(forward-char 1))))
     
     ;; Open parenthesis
     ((= cs ?\()
      (erlang-push (list '\( token (current-column)) stack)
      (forward-char 1))
     
     ;; Close parenthesis
     ((= cs ?\))
      (while (memq (car (car stack)) '(|| -> :: when))
	(erlang-pop stack))
      (cond ((eq (car (car stack)) '\()
	     (erlang-pop stack)
	     (if (and (eq (car (car stack)) 'fun) 
		      (or (eq (car (car (last stack))) 'spec)
			  (eq (car (car (cdr stack))) '::))) ;; -type()
		 ;; Inside fun type def ') closes fun definition
		 (erlang-pop stack)))
	    ((eq (car (car stack)) 'icr)
	     (erlang-pop stack)  
	     ;; Normal catch not try-catch might have caused icr
	     ;; and then incr should be removed and is not an error.
	     (if (eq (car (car stack)) '\() 
	     	 (erlang-pop stack) 
	       (error "Missing `end'")
	       ))
	    ((eq (car (car stack)) 'begin) 
	     (error "Missing `end'"))
	    (t
	     (error "Unbalanced parenthesis"))
	    )
      (forward-char 1)) 
    
     ;; Character quote: Skip it and the quoted char.
     ((= cs ?/)
      (forward-char 2))
     
     ;; Character escape: Skip it and the escape sequence.
     ((= cs ?\\)
      (forward-char 1)
      (skip-syntax-forward "w"))

     ;; Everything else
     (t
      (forward-char 1)))
    (list stack token cs in-what)))

(defun erlang-calculate-stack-indent (indent-point state)
  "From the given last position and state (stack) calculate indentation.
Return nil if inside string, t if in a comment."
  (let* ((stack (and state (car state)))
         (token (nth 1 state))
         (stack-top (and stack (car stack))))
    (cond ((null state)                 ;No state
           0)
          ((nth 3 state)
           ;; Return nil or t.
           (eq (nth 3 state) 'comment))
          ((null stack)
           (if (looking-at "when[^_a-zA-Z0-9]")
               erlang-indent-guard
             0))
          ((eq (car stack-top) '\()
           ;; Element of list, tuple or part of an expression,
           (cond ((null erlang-argument-indent)
		  ;; indent to next column.
		  (1+ (nth 2 stack-top)))
		 ((= (char-syntax (following-char)) ?\))
		  (goto-char (nth 1 stack-top))
		  (cond ((looking-at "[({]\\s *\\($\\|%\\)")
		   	 ;; Line ends with parenthesis.
			 (let ((previous (erlang-indent-find-preceding-expr))
			       (stack-pos (nth 2 stack-top)))
			   (if (>= previous stack-pos) stack-pos
			     (- (+ previous erlang-argument-indent) 1))))
		   	(t
		   	 (nth 2 stack-top))))
		 (t 
		  (goto-char (nth 1 stack-top))
		  (let ((base (cond ((looking-at "[({]\\s *\\($\\|%\\)")
				     ;; Line ends with parenthesis.
				     (erlang-indent-parenthesis (nth 2 stack-top)))
				    (t
				     ;; Indent to the same column as the first
				     ;; argument.
				     (goto-char (1+ (nth 1 stack-top)))
				     (skip-chars-forward " \t")
				     (current-column)))))
		    (erlang-indent-standard indent-point token base 't)))))
	  ;;
	  ((eq (car stack-top) '<<)
	   ;; Element of binary (possible comprehension) expression,
	   (cond ((null erlang-argument-indent)
		  ;; indent to next column.
		  (+ 2 (nth 2 stack-top)))
		 ((looking-at "\\(>>\\)[^_a-zA-Z0-9]")
		  (nth 2 stack-top))
		 (t 
		  (goto-char (nth 1 stack-top))
		  ;; Indent to the same column as the first
		  ;; argument.
		  (goto-char (+ 2 (nth 1 stack-top)))
		  (skip-chars-forward " \t")
		  (current-column))))
	  
          ((memq (car stack-top) '(icr fun spec))
           ;; The default indentation is the column of the option
           ;; directly following the keyword. (This does not apply to
           ;; `case'.)  Should no option be on the same line, the
           ;; indentation is the indentation of the keyword +
           ;; `erlang-indent-level'.
           ;;
           ;; `after' should be indented to the same level as the
           ;; corresponding receive.
           (cond ((looking-at "\\(after\\|of\\)\\($\\|[^_a-zA-Z0-9]\\)")
		  (nth 2 stack-top))
		 ((looking-at "when[^_a-zA-Z0-9]")
		  ;; Handling one when part
		  (+ (nth 2 stack-top) erlang-indent-level erlang-indent-guard))
		 (t
		  (save-excursion
		    (goto-char (nth 1 stack-top))
		    (if (looking-at "case[^_a-zA-Z0-9]")
			(+ (nth 2 stack-top) erlang-indent-level)
		      (skip-chars-forward "a-z")
		      (skip-chars-forward " \t")
		      (if (memq (following-char) '(?% ?\n))
			  (+ (nth 2 stack-top) erlang-indent-level)
			(current-column))))))
           )
	  ((and (eq (car stack-top) '||) (looking-at "\\(]\\|>>\\)[^_a-zA-Z0-9]"))
	   (nth 2 (car (cdr stack))))
          ;; Real indentation, where operators create extra indentation etc.
          ((memq (car stack-top) '(-> || try begin))
	   (if (looking-at "\\(of\\)[^_a-zA-Z0-9]")
	       (nth 2 stack-top)
	     (goto-char (nth 1 stack-top))
	     ;; Check if there is more code after the '->' on the
	     ;; same line. If so use this indentation as base, else
	     ;; use parent indentation + 2 * level as base.
	     (let ((off erlang-indent-level)
		   (skip 2))
	       (cond ((null (cdr stack))) ; Top level in function.
		     ((eq (car stack-top) 'begin)
		      (setq skip 5))
		     ((eq (car stack-top) 'try)
		      (setq skip 5))
		     ((eq (car stack-top) '->)
		      ;; If in fun definition use standard indent level not double
		      ;;(if (not (eq (car (car (cdr stack))) 'fun))
		      ;; Removed it made multi clause fun's look to bad
		      (setq off (* 2 erlang-indent-level)))) ;; ) 
	       (let ((base (erlang-indent-find-base stack indent-point off skip)))
		 ;; Special cases
		 (goto-char indent-point)
		 (cond ((looking-at "\\(end\\|after\\)\\($\\|[^_a-zA-Z0-9]\\)")
			(if (eq (car stack-top) '->)
			    (erlang-pop stack))
			(if stack
			    (erlang-caddr (car stack))
			  0))
		       ((looking-at "catch\\($\\|[^_a-zA-Z0-9]\\)")
			;; Are we in a try
			(let ((start (if (eq (car stack-top) '->)
					 (car (cdr stack))
				       stack-top)))
			  (if (null start) nil
			    (goto-char (nth 1 start)))
			  (cond ((looking-at "try\\($\\|[^_a-zA-Z0-9]\\)")
				 (progn
				   (if (eq (car stack-top) '->)
				       (erlang-pop stack))
				   (if stack
				       (erlang-caddr (car stack))
				     0)))
				(t (erlang-indent-standard indent-point token base 'nil))))) ;; old catch
		       (t 
			(erlang-indent-standard indent-point token base 'nil)
			))))
	     ))
	  ((eq (car stack-top) 'when)
	   (goto-char (nth 1 stack-top))
	   (if (looking-at "when\\s *\\($\\|%\\)")
	       (progn
		 (erlang-pop stack)
		 (if (and stack (memq (nth 0 (car stack)) '(icr fun)))
		     (progn
		       (goto-char (nth 1 (car stack)))
		       (+ (nth 2 (car stack)) erlang-indent-guard
			  ;; receive XYZ    or    receive
			  ;;                          XYZ
			  ;; This if thing does not seem to be needed
			  ;;(if (looking-at "[a-z]+\\s *\\($\\|%\\)")
			  ;;    erlang-indent-level
			  ;;  (* 2 erlang-indent-level))))
			  (* 2 erlang-indent-level)))
			  ;;erlang-indent-level))
		   (+ erlang-indent-level erlang-indent-guard)))
             ;; "when" is followed by code, let's indent to the same
             ;; column.
             (forward-char 4)           ; Skip "when"
             (skip-chars-forward " \t")
             (current-column)))
	  ;; Type and Spec indentation
	  ((eq (car stack-top) '::)
	   (if (looking-at "}")
	       ;; Closing record definition with types
	       ;; pop stack and recurse
	       (erlang-calculate-stack-indent indent-point
					      (cons (erlang-pop stack) (cdr state)))
	     (cond ((null erlang-argument-indent)
		    ;; indent to next column.
		    (+ 2 (nth 2 stack-top)))
		   ((looking-at "::[^_a-zA-Z0-9]")
		    (nth 2 stack-top))
		   (t
		    (let ((start-alternativ (if (looking-at "|") 2 0)))
		      (goto-char (nth 1 stack-top))
		      (- (cond ((looking-at "::\\s *\\($\\|%\\)")
				;; Line ends with ::
				(if (eq (car (car (last stack))) 'spec)
				  (+ (erlang-indent-find-preceding-expr 1)
				     erlang-argument-indent)
				  (+ (erlang-indent-find-preceding-expr 2)
				     erlang-argument-indent)))
			       (t
				;; Indent to the same column as the first
				;; argument.
				(goto-char (+ 2 (nth 1 stack-top)))
				(skip-chars-forward " \t")
				(current-column))) start-alternativ))))))
	  )))

(defun erlang-indent-standard (indent-point token base inside-parenthesis)
  "Standard indent when in blocks or tuple or arguments.
   Look at last thing to see in what state we are, move relative to the base."
  (goto-char token)  
  (cond ((looking-at "||\\|,\\|->\\||")
	 base)
	((erlang-at-keyword)
	 (+ (current-column) erlang-indent-level))
	((or (= (char-syntax (following-char)) ?.)
	     (erlang-at-operator))
	 (+ base erlang-indent-level))
	(t
	 (goto-char indent-point)
	 (cond ((memq (following-char) '(?\( ))
		;; Function application.
		(+ (erlang-indent-find-preceding-expr)
		   erlang-argument-indent))
	       ;; Empty line, or end; treat it as the end of
	       ;; the block.  (Here we have a choice: should
	       ;; the user be forced to reindent continued
	       ;; lines, or should the "end" be reindented?)
	       
	       ;; Avoid treating comments a continued line.
	       ((= (following-char) ?%)
		base)
	       ;; Continued line (e.g. line beginning
	       ;; with an operator.)
	       (t 
		(if (or (erlang-at-operator) (not inside-parenthesis)) 
		    (+ base erlang-indent-level)
		  base))))))

(defun erlang-indent-find-base (stack indent-point &optional offset skip)
  "Find the base column for current stack."
  (or skip (setq skip 2))
  (or offset (setq offset erlang-indent-level))
  (save-excursion
    (let* ((stack-top (car stack)))
      (goto-char (nth 1 stack-top))
      (if (< skip (- (point-max) (point)))
	  (progn 
	    (forward-char skip)
	    (if (looking-at "\\s *\\($\\|%\\)")
		(progn
		  (if (memq (car stack-top) '(-> ||))
		      (erlang-pop stack))
		  ;; Take parent identation + offset,
		  ;; else just erlang-indent-level if no parent
		  (if stack
		      (+ (erlang-caddr (car stack))
			 offset)
		    erlang-indent-level))
	      (erlang-skip-blank indent-point)
	      (current-column)))
	(+ (current-column) skip)))))


;; Does not handle `begin' .. `end'.
(defun erlang-indent-find-preceding-expr (&optional arg)
  "Return the first column of the preceding expression.
This assumes that the preceding expression is either simple
\(i.e. an atom) or parenthesized."
  (save-excursion
    (or arg (setq arg 1))
    (forward-sexp (- arg))
    (let ((col (current-column)))
      (skip-chars-backward " \t")
      ;; Needed to match the colon in "'foo':'bar'".
      (if (not (memq (preceding-char) '(?# ?:)))
          col
        ;; Special hack to handle: (note line break)
        ;; [#myrecord{
        ;;  foo = foo}]
        (or
         (ignore-errors
           (backward-char 1)
           (forward-sexp -1)
           (current-column))
         col)))))

(defun erlang-indent-parenthesis (stack-position) 
  (let ((previous (erlang-indent-find-preceding-expr)))
    (if (> previous stack-position)
	(+ stack-position erlang-argument-indent)
      (+ previous erlang-argument-indent))))

(defun erlang-skip-blank (&optional lim)
  "Skip over whitespace and comments until limit reached."
  (or lim (setq lim (point-max)))
  (let (stop)
    (while (and (not stop) (< (point) lim))
      (cond ((= (following-char) ?%)
	     (skip-chars-forward "^\n" lim))
	    ((= (following-char) ?\n)
	     (skip-chars-forward "\n" lim))
	    ((looking-at "\\s ")
	     (if (re-search-forward "\\S " lim 'move)
		 (forward-char -1)))
	    (t
	     (setq stop t))))
    stop))

(defun erlang-at-keyword ()
  "Are we looking at an Erlang keyword which will increase indentation?"
  (looking-at (concat "\\(when\\|if\\|fun\\|case\\|begin\\|query\\|"
		      "of\\|receive\\|after\\|catch\\|try\\)[^_a-zA-Z0-9]")))

(defun erlang-at-operator ()
  "Are we looking at an Erlang operator?"
  (looking-at
   "\\(bnot\\|div\\|mod\\|band\\|bor\\|bxor\\|bsl\\|bsr\\)[^_a-zA-Z0-9]"))

(defun erlang-comment-indent ()
  "Compute Erlang comment indentation.

Used both by `indent-for-comment' and the Erlang specific indentation
commands."
  (cond ((looking-at "%%%") 0)
	((looking-at "%%")
	 (or (erlang-calculate-indent)
	     (current-indentation)))
	(t
	 (save-excursion
	   (skip-chars-backward " \t")
	   (max (if (bolp) 0 (1+ (current-column)))
		comment-column)))))

;;; Erlang movement commands

;; All commands below work as movement commands.  I.e. if the point is
;; at the end of the clause, and the command `erlang-end-of-clause' is
;; executed, the point is moved to the end of the NEXT clause.  (This
;; mimics the behaviour of `end-of-defun'.)
;;
;; Personally I would like to rewrite them to be "pure", and add a set
;; of movement functions, like `erlang-next-clause',
;; `erlang-previous-clause', and the same for functions.
;;
;; The current implementation makes it hopeless to use the functions as
;; subroutines in more complex commands.   /andersl

(defun erlang-beginning-of-clause (&optional arg)
  "Move backward to previous start of clause.
With argument, do this that many times.
Return t unless search stops due to end of buffer."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      ;; Step back to the end of the previous line, unless we are at
      ;; the beginning of the buffer.  The reason for this move is
      ;; that the regexp below includes the last character of the
      ;; previous line.
      (if (bobp)
	  (or (looking-at "\n")
	      (forward-char 1))
	(forward-char -1)
	(if (looking-at "\\`\n")
	    (forward-char 1))))
  ;; The regexp matches a function header that isn't
  ;; included in a string.
  (and (re-search-forward "\\(\\`\\|\\`\n\\|[^\\]\n\\)\\(-?[a-z]\\|'\\|-\\)"
			  nil 'move (- arg))
       (let ((beg (match-beginning 2)))
	 (and beg (goto-char beg))
	 t)))

(defun erlang-end-of-clause (&optional arg)
  "Move to the end of the current clause.
With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (while (and (looking-at "[ \t]*[%\n]")
	      (zerop (forward-line 1))))
  ;; Move to the next clause.
  (erlang-beginning-of-clause (- arg))
  (beginning-of-line);; Just to be sure...
  (let ((continue t))
    (while (and (not (bobp)) continue)
      (forward-line -1)
      (skip-chars-forward " \t")
      (if (looking-at "[%\n]")
	  nil
	(end-of-line)
	(setq continue nil)))))

(defun erlang-mark-clause ()
  "Put mark at end of clause, point at beginning."
  (interactive)
  (push-mark (point))
  (erlang-end-of-clause 1)
  ;; Sets the region. In Emacs 19 and XEmacs, we want to activate
  ;; the region.
  (condition-case nil
      (push-mark (point) nil t)
    (error (push-mark (point))))
  (erlang-beginning-of-clause 1)
  ;; The above function deactivates the mark.
  (if (boundp 'deactivate-mark)
      (funcall (symbol-function 'set) 'deactivate-mark nil)))

(defun erlang-beginning-of-function (&optional arg)
  "Move backward to previous start of function.
With positive argument, do this that many times.
With negative argument, search forward.

Return t unless search stops due to end of buffer."
  (interactive "p")
  (or arg (setq arg 1))
  (cond
   ;; Search backward
   ((> arg 0)
    (while (and (> arg 0)
		(and (erlang-beginning-of-clause 1)
		     (let ((start (point))
			   (name (erlang-name-of-function))
			   (arity (erlang-get-function-arity)))
		       ;; Note: "arity" is nil for e.g. "-import", hence
		       ;; two "-import" clauses are not considered to
		       ;; be part of the same function.
		       (while (and (erlang-beginning-of-clause 1)
				   (string-equal name
						 (erlang-name-of-function))
				   arity
				   (equal arity
					  (erlang-get-function-arity)))
			 (setq start (point)))
		       (goto-char start)
		       t)))
      (setq arg (1- arg))))
   ;; Search forward
   ((< arg 0)
    (end-of-line)
    (erlang-beginning-of-clause 1)
    ;; Step -arg functions forward.
    (while (and (< arg 0)
		;; Step one function forward, or stop if the end of
		;; the buffer was reached.  Return t if we found the
		;; function.
		(let ((name (erlang-name-of-function))
		      (arity (erlang-get-function-arity))
		      (found (erlang-beginning-of-clause -1)))
		  (while (and found
			      (string-equal name (erlang-name-of-function))
			      arity
			      (equal arity
				     (erlang-get-function-arity)))
		    (setq found (erlang-beginning-of-clause -1)))
		  found))
      (setq arg (1+ arg)))))
  (zerop arg))


(defun erlang-end-of-function (&optional arg)
  "Move forward to next end of function.

With argument, do this that many times.
With negative argument go towards the beginning of the buffer."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((first t))
    ;; Forward
    (while (and (> arg 0) (< (point) (point-max)))
      (let ((pos (point)))
	(while (progn
		 (if (and first
			  (progn
			    (forward-char 1)
			    (erlang-beginning-of-clause 1)))
		     nil
		   (or (bobp) (forward-char -1))
		   (erlang-beginning-of-clause -1))
		 (setq first nil)
		 (erlang-pass-over-function)
		 (skip-chars-forward " \t")
		 (if (looking-at "[%\n]")
		     (forward-line 1))
		 (<= (point) pos))))
      (setq arg (1- arg)))
    ;; Backward
    (while (< arg 0)
      (let ((pos (point)))
	(erlang-beginning-of-clause 1)
	(erlang-pass-over-function)
	(forward-line 1)
	(if (>= (point) pos)
	    (if (erlang-beginning-of-function 2)
		(progn
		  (erlang-pass-over-function)
		  (skip-chars-forward " \t")
		  (if (looking-at "[%\n]")
		      (forward-line 1)))
	      (goto-char (point-min)))))
      (setq arg (1+ arg)))))

(eval-and-compile
  (if (default-boundp 'beginning-of-defun-function)
      (defalias 'erlang-mark-function 'mark-defun)
    (defun erlang-mark-function ()
      "Put mark at end of function, point at beginning."
      (interactive)
      (push-mark (point))
      (erlang-end-of-function 1)
      ;; Sets the region. In Emacs 19 and XEmacs, we want to activate
      ;; the region.
      (condition-case nil
	  (push-mark (point) nil t)
	(error (push-mark (point))))
      (erlang-beginning-of-function 1)
      ;; The above function deactivates the mark.
      (if (boundp 'deactivate-mark)
	  (funcall (symbol-function 'set) 'deactivate-mark nil)))))

(defun erlang-pass-over-function ()
  (while (progn
	   (erlang-skip-blank)
	   (and (not (looking-at "\\.\\(\\s \\|\n\\|\\s<\\)"))
		(not (eobp))))
    (forward-sexp 1))
  (if (not (eobp))
      (forward-char 1)))

(defun erlang-name-of-function ()
  (save-excursion
    ;; Skip over attribute leader.
    (if (looking-at "-[ \t]*")
	(re-search-forward "-[ \t]*" nil 'move))
    (let ((start (point)))
      (forward-sexp 1)
      (buffer-substring start (point)))))


;;; Miscellaneous

(defun erlang-fill-paragraph (&optional justify)
  "Like \\[fill-paragraph], but handle Erlang comments.
If any of the current line is a comment, fill the comment or the
paragraph of it that point is in, preserving the comment's indentation
and initial `%':s."
  (interactive "P")
  (let ((has-comment nil)
	;; If has-comment, the appropriate fill-prefix for the comment.
	comment-fill-prefix)
    ;; Figure out what kind of comment we are looking at.
    (save-excursion
      (beginning-of-line)
      (cond
       ;; Find the command prefix.
       ((looking-at (concat "\\s *" comment-start-skip))
	(setq has-comment t)
	(setq comment-fill-prefix (buffer-substring (match-beginning 0)
						    (match-end 0))))
       ;; A line with some code, followed by a comment?  Remember that the
       ;; % which starts the comment shouldn't be part of a string or
       ;; character.
       ((progn
	  (while (not (looking-at "%\\|$"))
	    (skip-chars-forward "^%\n\"\\\\")
	    (cond
	     ((eq (char-after (point)) ?\\) (forward-char 2))
	     ((eq (char-after (point)) ?\") (forward-sexp 1))))
	  (looking-at comment-start-skip))
	(setq has-comment t)
	(setq comment-fill-prefix
	      (concat (make-string (current-column) ? )
		      (buffer-substring (match-beginning 0) (match-end 0)))))))
    (if (not has-comment)
	(fill-paragraph justify)
      ;; Narrow to include only the comment, and then fill the region.
      (save-restriction
	(narrow-to-region
	 ;; Find the first line we should include in the region to fill.
	 (save-excursion
	   (while (and (zerop (forward-line -1))
		       (looking-at "^\\s *%")))
	   ;; We may have gone to far.  Go forward again.
	   (or (looking-at "^\\s *%")
	       (forward-line 1))
	   (point))
	 ;; Find the beginning of the first line past the region to fill.
	 (save-excursion
	   (while (progn (forward-line 1)
			 (looking-at "^\\s *%")))
	   (point)))
	;; Lines with only % on them can be paragraph boundaries.
	(let ((paragraph-start (concat paragraph-start "\\|^[ \t%]*$"))
	      (paragraph-separate (concat paragraph-start "\\|^[ \t%]*$"))
	      (fill-prefix comment-fill-prefix))
	  (fill-paragraph justify))))))


(defun erlang-uncomment-region (beg end)
  "Uncomment all commented lines in the region."
  (interactive "r")
  (uncomment-region beg end))


(defun erlang-generate-new-clause ()
  "Create additional Erlang clause header.

Parses the source file for the name of the current Erlang function.
Create the header containing the name, A pair of parentheses,
and an arrow. The space between the function name and the
first parenthesis is preserved.  The point is placed between
the parentheses."
  (interactive)
  (let ((name (save-excursion
		(and (erlang-beginning-of-clause)
		     (erlang-get-function-name t))))
	(arrow (save-excursion
		 (and (erlang-beginning-of-clause)
		      (erlang-get-function-arrow)))))
    (if (or (null arrow) (null name))
	(error "Can't find name of current Erlang function"))
    (if (and (bolp) (eolp))
	nil
      (end-of-line)
      (newline))
    (insert name)
    (save-excursion
      (insert ") " arrow))
    (if erlang-new-clause-with-arguments
	(erlang-clone-arguments))))


(defun erlang-clone-arguments ()
  "Insert, at the point, the argument list of the previous clause.

The mark is set at the beginning of the inserted text, the point
at the end."
  (interactive)
  (let ((args (save-excursion
		(beginning-of-line)
		(and (erlang-beginning-of-clause)
		     (erlang-get-function-arguments))))
	(p (point)))
    (if (null args)
	(error "Can't clone argument list"))
    (insert args)
    (set-mark p)))

;;; Information retrieval functions.

(defun erlang-buffer-substring (beg end)
  "Like `buffer-substring-no-properties'.
Although, this function works on all versions of Emacs."
  (if (fboundp 'buffer-substring-no-properties)
      (funcall (symbol-function 'buffer-substring-no-properties) beg end)
    (buffer-substring beg end)))


(defun erlang-get-module ()
  "Return the name of the module as specified by `-module'.

Return nil if file contains no `-module' attribute."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((md (match-data)))
	(unwind-protect
	    (if (re-search-forward
		 (eval-when-compile
		   (concat "^-module\\s *(\\s *\\(\\("
			   erlang-atom-regexp
			   "\\)?\\)\\s *)\\s *\\."))
		 (point-max) t)
		(erlang-remove-quotes
		 (erlang-buffer-substring (match-beginning 1)
					  (match-end 1)))
	      nil)
	  (store-match-data md))))))


(defun erlang-get-module-from-file-name (&optional file)
  "Extract the module name from a file name.

First, the directory part is removed.  Second, the part of the file name
matching `erlang-file-name-extension-regexp' is removed.

Should the match fail, nil is returned.

By modifying `erlang-file-name-extension-regexp' to match files other
than Erlang source files, Erlang specific functions could be applied on
non-Erlang files.  Most notably; the support for Erlang modules in the
tags system could be used by files written in other languages."
  (or file (setq file buffer-file-name))
  (if (null file)
      nil
    (setq file (file-name-nondirectory file))
    (if (string-match erlang-file-name-extension-regexp file)
	(substring file 0 (match-beginning 0))
      nil)))


;; Used by `erlang-get-export' and `erlang-get-import'.

(defun erlang-get-function-arity-list ()
  "Parse list of `function/arity' as used by `-import' and `-export'.

Point must be before the opening bracket.  When the
function returns the point will be placed after the closing bracket.

The function does not return an error if the list is incorrectly
formatted.

Return list of (function . arity).  The order of the returned list
corresponds to the order of the parsed Erlang list."
  (let ((res '()))
    (erlang-skip-blank)
    (forward-char 1)
    (if (not (eq (preceding-char) ?\[))
	'()				; Not looking at an Erlang list.
      (while				; Note: `while' has no body.
	  (progn
	    (erlang-skip-blank)
	    (and (looking-at (eval-when-compile
			       (concat erlang-atom-regexp "/\\([0-9]+\\)\\>")))
		 (progn
		   (setq res (cons
			      (cons
			       (erlang-remove-quotes
				(erlang-buffer-substring
				 (match-beginning 1) (match-end 1)))
			       (erlang-string-to-int
				(erlang-buffer-substring
				 (match-beginning
				  (+ 1 erlang-atom-regexp-matches))
				 (match-end
				  (+ 1 erlang-atom-regexp-matches)))))
			      res))
		   (goto-char (match-end 0))
		   (erlang-skip-blank)
		   (forward-char 1)
		   ;; Test if there are more exported functions.
		   (eq (preceding-char) ?,))))))
    (nreverse res)))


;;;  Note that `-export' and the open parenthesis must be written on
;;;  the same line.

(defun erlang-get-export ()
  "Return a list of `(function . arity)' as specified by `-export'."
  (save-excursion
    (goto-char (point-min))
    (let ((md (match-data))
	  (res '()))
      (unwind-protect
	  (progn
	    (while (re-search-forward "^-export\\s *(" (point-max) t)
	      (erlang-skip-blank)
	      (setq res (nconc res (erlang-get-function-arity-list))))
	    res)
	(store-match-data md)))))


(defun erlang-get-import ()
  "Parse an Erlang source file for imported functions.

Return an alist with module name as car part and list of conses containing
function and arity as cdr part."
  (save-excursion
    (goto-char (point-min))
    (let ((md (match-data))
	  (res '()))
      (unwind-protect
	  (progn
	    (while (re-search-forward "^-import\\s *(" (point-max) t)
	      (erlang-skip-blank)
	      (if (looking-at erlang-atom-regexp)
		  (let ((module (erlang-remove-quotes
				 (erlang-buffer-substring
				  (match-beginning 0)
				  (match-end 0)))))
		    (goto-char (match-end 0))
		    (erlang-skip-blank)
		    (if (eq (following-char) ?,)
			(progn
			  (forward-char 1)
			  (erlang-skip-blank)
			  (let ((funcs (erlang-get-function-arity-list))
				(pair (assoc module res)))
			    (if pair
				(setcdr pair (nconc (cdr pair) funcs))
			      (setq res (cons (cons module funcs)
					      res)))))))))
	    (nreverse res))
	(store-match-data md)))))


(defun erlang-get-function-name (&optional arg)
  "Return name of current function, or nil.

If optional argument is non-nil, everything up to and including
the first `(' is returned.

Normally used in conjunction with `erlang-beginning-of-clause', e.g.:
              (save-excursion
                (if (not (eobp)) (forward-char 1))
		(and (erlang-beginning-of-clause)
		     (erlang-get-function-name t)))"
  (let ((n (if arg 0 1)))
    (and (looking-at (eval-when-compile
		       (concat "^" erlang-atom-regexp "\\s *(")))
	 (erlang-buffer-substring (match-beginning n) (match-end n)))))


(defun erlang-get-function-arrow ()
  "Return arrow of current function, could be \"->\" or nil.

Normally used in conjunction with `erlang-beginning-of-clause', e.g.:
              (save-excursion
                (if (not (eobp)) (forward-char 1))
		(and (erlang-beginning-of-clause)
		     (erlang-get-function-arrow)))"
  (and 
   (save-excursion
     (re-search-forward "->" (point-max) t)
     (erlang-buffer-substring (- (point) 2) (+ (point) 1)))))

(defun erlang-get-function-arity ()
  "Return the number of arguments of function at point, or nil."
  (and (looking-at (eval-when-compile
		     (concat "^" erlang-atom-regexp "\\s *(")))
       (save-excursion
	 (goto-char (match-end 0))
	 (condition-case nil
	     (let ((res 0)
		   (cont t))
	       (while cont
		 (cond ((eobp)
			(setq res nil)
			(setq cont nil))
		       ((looking-at "\\s *)")
			(setq cont nil))
		       ((looking-at "\\s *\\($\\|%\\)")
			(forward-line 1))
		       ((looking-at "\\s *,")
			(setq res (+ 1 res))
			(goto-char (match-end 0)))
		       (t
			(when (zerop res)
			  (setq res (+ 1 res)))
			(forward-sexp 1))))
	       res)
	   (error nil)))))

(defun erlang-get-function-name-and-arity ()
  "Return the name and arity of the function at point, or nil.
The return value is a string of the form \"foo/1\"."
  (let ((name (erlang-get-function-name))
        (arity (erlang-get-function-arity)))
    (and name arity (format "%s/%d" name arity))))

(defun erlang-get-function-arguments ()
  "Return arguments of current function, or nil."
  (if (not (looking-at (eval-when-compile
			 (concat "^" erlang-atom-regexp "\\s *("))))
      nil
    (save-excursion
      (condition-case nil
	  (let ((start (match-end 0)))
	    (goto-char (- start 1))
	    (forward-sexp)
	    (erlang-buffer-substring start (- (point) 1)))
	(error nil)))))


(defun erlang-get-function-under-point ()
  "Return the module and function under the point, or nil.

Should no explicit module name be present at the point, the
list of imported functions is searched.

The following could be returned:
   (\"module\"  \"function\")    -- Both module and function name found.
   (nil       \"function\")    -- No module name was found.
   nil                       -- No function name found

In the future the list may contain more elements."
  (save-excursion
    (let ((md (match-data))
	  (res nil))
      (if (eq (char-syntax (following-char)) ? )
	  (skip-chars-backward " \t"))
      (skip-chars-backward "a-zA-Z0-9_:'")
      (cond ((looking-at (eval-when-compile
			   (concat erlang-atom-regexp ":" erlang-atom-regexp)))
	     (setq res (list
			(erlang-remove-quotes
			 (erlang-buffer-substring
			  (match-beginning 1) (match-end 1)))
			(erlang-remove-quotes
			 (erlang-buffer-substring
			  (match-beginning (1+ erlang-atom-regexp-matches))
			  (match-end (1+ erlang-atom-regexp-matches)))))))
	    ((looking-at erlang-atom-regexp)
	     (let ((fk (erlang-remove-quotes
			(erlang-buffer-substring
			 (match-beginning 0) (match-end 0))))
		   (mod nil)
		   (imports (erlang-get-import)))
	       (while (and imports (null mod))
		 (if (assoc fk (cdr (car imports)))
		     (setq mod (car (car imports)))
		   (setq imports (cdr imports))))
	       (setq res (list mod fk)))))
      (store-match-data md)
      res)))


;; TODO: Escape single quotes inside the string without
;; replace-regexp-in-string.
(defun erlang-add-quotes-if-needed (str)
  "Return STR, possibly with quotes."
  (let ((case-fold-search nil)) ; force string matching to be case sensitive
    (if (and (stringp str)
	     (not (string-match (eval-when-compile
				  (concat "\\`" erlang-atom-regexp "\\'")) str)))
	(progn (if (fboundp 'replace-regexp-in-string)
		   (setq str (replace-regexp-in-string "'" "\\'" str t t )))
	       (concat "'" str "'"))
      str)))


(defun erlang-remove-quotes (str)
  "Return STR without quotes, if present."
  (let ((md (match-data)))
    (prog1
	(if (string-match "\\`'\\(.*\\)'\\'" str)
	    (substring str 1 -1)
	  str)
      (store-match-data md))))


;;; Check module name

;; The function `write-file', bound to C-x C-w, calls
;; `set-visited-file-name' which clears the hook.  :-(
;; To make sure that the hook always is present, we advise
;; `set-visited-file-name'.
(defun erlang-check-module-name-init ()
  "Initialize the functionality to compare file and module names.

Unless we have `before-save-hook', we redefine the function
`set-visited-file-name' since it clears the variable
`local-write-file-hooks'.  The original function definition is
stored in `erlang-orig-set-visited-file-name'."
  (if (boundp 'before-save-hook)
      ;; If we have that, `make-local-hook' is obsolete.
      (add-hook 'before-save-hook 'erlang-check-module-name nil t)
    (require 'advice)
    (unless (ad-advised-definition-p 'set-visited-file-name)
      (defadvice set-visited-file-name (after erlang-set-visited-file-name
					      activate)
	(if (eq major-mode 'erlang-mode)
	    (add-hook 'local-write-file-hooks 'erlang-check-module-name))))
    (add-hook 'local-write-file-hooks 'erlang-check-module-name)))


(defun erlang-check-module-name ()
  "If the module name doesn't match file name, ask for permission to change.

The variable `erlang-check-module-name' controls the behaviour of this
function.  It it is nil, this function does nothing.  If it is t, the
source is silently changed.  If it is set to the atom `ask', the user
is prompted.

This function is normally placed in the hook `local-write-file-hooks'."
  (if erlang-check-module-name
      (let ((mn (erlang-add-quotes-if-needed 
		 (erlang-get-module)))
	    (fn (erlang-add-quotes-if-needed 
		 (erlang-get-module-from-file-name (buffer-file-name)))))
	(if (and (stringp mn) (stringp fn))
	    (or (string-equal mn fn)
		(if (or (eq erlang-check-module-name t)
			(y-or-n-p
			 "Module does not match file name. Modify source? "))
		    (save-excursion
		      (save-restriction
			(widen)
			(goto-char (point-min))
			(if (re-search-forward
			     (eval-when-compile
			       (concat "^-module\\s *(\\s *\\(\\("
				       erlang-atom-regexp
				       "\\)?\\)\\s *)\\s *\\."))
			     (point-max) t)
			    (progn
			      (goto-char (match-beginning 1))
			      (delete-region (match-beginning 1)
					     (match-end 1))
			      (insert fn))))))))))
  ;; Must return nil since it is added to `local-write-file-hook'.
  nil)


;;; Electric functions.

(defun erlang-electric-semicolon (&optional arg)
  "Insert a semicolon character and possibly a prototype for the next line.

The variable `erlang-electric-semicolon-criteria' states a criterion,
when fulfilled a newline is inserted, the next line is indented and a
prototype for the next line is inserted.  Normally the prototype
consists of \" ->\".  Should the semicolon end the clause a new clause
header is generated.

The variable `erlang-electric-semicolon-insert-blank-lines' controls
the number of blank lines inserted between the current line and new
function header.

Behaves just like the normal semicolon when supplied with a
numerical arg, point is inside string or comment, or when there are
non-whitespace characters following the point on the current line."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (if (or arg
	  (and (listp erlang-electric-commands)
	       (not (memq 'erlang-electric-semicolon
			  erlang-electric-commands)))
	  (erlang-in-literal)
	  (not (looking-at "\\s *\\(%.*\\)?$"))
	  (null (erlang-test-criteria-list
		 erlang-electric-semicolon-criteria)))
      (setq erlang-electric-newline-inhibit nil)
    (setq erlang-electric-newline-inhibit t)
    (undo-boundary)
    (erlang-indent-line)
    (end-of-line)
    (newline)
    (if (condition-case nil
	    (progn (erlang-indent-line) t)
	  (error (if (bolp) (delete-backward-char 1))))
	(if (not (bolp))
	    (save-excursion
	      (insert " ->"))
	  (condition-case nil
	      (progn
		(erlang-generate-new-clause)
		(if erlang-electric-semicolon-insert-blank-lines
		    (save-excursion
		      (beginning-of-line)
		      (newline
		       erlang-electric-semicolon-insert-blank-lines))))
	    (error (if (bolp) (delete-backward-char 1))))))))


(defun erlang-electric-comma (&optional arg)
  "Insert a comma character and possibly a new indented line.
The variable `erlang-electric-comma-criteria' states a criterion,
when fulfilled a newline is inserted and the next line is indented.

Behaves just like the normal comma when supplied with a
numerical arg, point is inside string or comment, or when there are
non-whitespace characters following the point on the current line."
  (interactive "P")

  (self-insert-command (prefix-numeric-value arg))

  (if (or arg
	  (and (listp erlang-electric-commands)
	       (not (memq 'erlang-electric-comma erlang-electric-commands)))
	  (erlang-in-literal)
	  (not (looking-at "\\s *\\(%.*\\)?$"))
	  (null (erlang-test-criteria-list
		 erlang-electric-comma-criteria)))
      (setq erlang-electric-newline-inhibit nil)
    (setq erlang-electric-newline-inhibit t)
    (undo-boundary)
    (erlang-indent-line)
    (end-of-line)
    (newline)
    (condition-case nil
	(erlang-indent-line)
      (error (if (bolp) (delete-backward-char 1))))))

(defun erlang-electric-lt (&optional arg)
  "Insert a less-than sign, and optionally mark it as an open paren."
  
  (interactive "p")

  (self-insert-command arg)

  ;; Was this the second char in bit-syntax open (`<<')?
  (unless (< (point) 2)
    (save-excursion
      (backward-char 2)
      (when (and (eq (char-after (point)) ?<)
		 (not (eq (get-text-property (point) 'category)
			  'bitsyntax-open-inner)))
	;; Then mark the two chars...
	(put-text-property (point) (1+ (point)) 
			   'category 'bitsyntax-open-outer)
	(forward-char 1)
	(put-text-property (point) (1+ (point))
			   'category 'bitsyntax-open-inner)
	;;...and unmark any subsequent less-than chars.
	(forward-char 1)
	(while (eq (char-after (point)) ?<)
	  (remove-text-properties (point) (1+ (point))
				  '(category nil))
	  (forward-char 1))))))

(defun erlang-after-bitsyntax-close ()
  "Return t if point is immediately after a bit-syntax close parenthesis (`>>')."
  (and (>= (point) 2)
       (save-excursion
	 (backward-char 2)
	 (and (eq (char-after (point)) ?>)
	      (not (eq (get-text-property (point) 'category)
		       'bitsyntax-close-outer))))))
	 
(defun erlang-after-arrow ()
  "Return true if point is immediately after a function arrow (`->')."
  (and (>= (point) 2)
       (and 
	(save-excursion
	  (backward-char)
	  (eq (char-before (point)) ?-))
	(or (not (listp erlang-electric-commands))
	    (memq 'erlang-electric-gt
		  erlang-electric-commands))
	(not (erlang-in-literal))
	(looking-at "\\s *\\(%.*\\)?$")
	(erlang-test-criteria-list erlang-electric-arrow-criteria))))


(defun erlang-electric-gt (&optional arg)
  "Insert a greater-than sign, and optionally mark it as a close paren."
  
  (interactive "p")

  (self-insert-command arg)

  (cond
   ;; Did we just write a bit-syntax close (`>>')?
   ((erlang-after-bitsyntax-close)
    (save-excursion
      ;; Then mark the two chars...
      (backward-char 2)
      (put-text-property (point) (1+ (point)) 
			 'category 'bitsyntax-close-inner)
      (forward-char)
      (put-text-property (point) (1+ (point))
			 'category 'bitsyntax-close-outer)
      ;;...and unmark any subsequent greater-than chars.
      (forward-char)
      (while (eq (char-after (point)) ?>)
	(remove-text-properties (point) (1+ (point))
				'(category nil))
	(forward-char))))

   ;; Did we just write a function arrow (`->')?
   ((erlang-after-arrow)
    (let ((erlang-electric-newline-inhibit t))
      (undo-boundary)
      (end-of-line)
      (newline)
      (condition-case nil
	  (erlang-indent-line)
	(error (if (bolp) (delete-backward-char 1))))))

   ;; Then it's just a plain greater-than.
   (t
    nil)))
    

(defun erlang-electric-arrow\ off (&optional arg)
  "Insert a '>'-sign and possibly a new indented line.

This command is only `electric' when the `>' is part of an `->' arrow.
The variable `erlang-electric-arrow-criteria' states a sequence of
criteria, which decides when a newline should be inserted and the next
line indented.

It behaves just like the normal greater than sign when supplied with a
numerical arg, point is inside string or comment, or when there are
non-whitespace characters following the point on the current line.

After being split/merged into `erlang-after-arrow' and
`erlang-electric-gt', it is now unused and disabled."
  (interactive "P")
  (let ((prec (preceding-char)))
    (self-insert-command (prefix-numeric-value arg))
    (if (or arg
	    (and (listp erlang-electric-commands)
		 (not (memq 'erlang-electric-arrow
			    erlang-electric-commands)))
	    (not (eq prec ?-))
	    (erlang-in-literal)
	    (not (looking-at "\\s *\\(%.*\\)?$"))
	    (null (erlang-test-criteria-list
		   erlang-electric-arrow-criteria)))
	(setq erlang-electric-newline-inhibit nil)
      (setq erlang-electric-newline-inhibit t)
      (undo-boundary)
      (end-of-line)
      (newline)
      (condition-case nil
	  (erlang-indent-line)
	(error (if (bolp) (delete-backward-char 1)))))))


(defun erlang-electric-newline (&optional arg)
  "Break line at point and indent, continuing comment if within one.
The variable `erlang-electric-newline-criteria' states a criterion,
when fulfilled a newline is inserted and the next line is indented.

Should the current line begin with a comment, and the variable
`comment-multi-line' be non-nil, a new comment start is inserted.

Should the previous command be another electric command we assume that
the user pressed newline out of old habit, hence we will do nothing."
  (interactive "P")
  (cond ((and (not arg)
	      erlang-electric-newline-inhibit
	      (memq last-command erlang-electric-newline-inhibit-list))
	 ())				; Do nothing!
	((or arg
	     (and (listp erlang-electric-commands)
		  (not (memq 'erlang-electric-newline
			     erlang-electric-commands)))
	     (null (erlang-test-criteria-list
		    erlang-electric-newline-criteria)))
	 (newline (prefix-numeric-value arg)))
	(t
	 (if (and comment-multi-line
		  (save-excursion
		    (beginning-of-line)
		    (looking-at (concat "\\s *" comment-start-skip))))
	     (let ((str (buffer-substring
			 (or (match-end 1) (match-beginning 0))
			 (min (match-end 0) (point)))))
	       (newline)
	       (undo-boundary)
	       (insert str))
	   (newline)
	   (undo-boundary)
	   (indent-according-to-mode)))))


(defun erlang-test-criteria-list (criteria)
  "Given a list of criterion functions, test if criteria are fulfilled.

Each element in the criteria list can a function returning nil, t or
the atom `stop'.  t means that the criterion is fulfilled, `stop' means
that it isn't fulfilled and that the search should stop,
and nil means continue searching.

Should the list contain the atom t the criterion is assumed to be
fulfilled, unless preceded by a function returning `stop', of course.

Should the argument be the atom t instead of a list, the criterion is
assumed to be trivially true.

Should all functions return nil, the criteria are assumed not to be
fulfilled.

Return t if criteria fulfilled, nil otherwise."
  (if (eq criteria t)
      t
    (save-excursion
      (let ((answer nil))
	(while (and criteria (null answer))
	  (if (eq (car criteria) t)
	      (setq answer t)
	    (setq answer (funcall (car criteria))))
	  (setq criteria (cdr criteria)))
	(if (and answer (not (eq answer 'stop)))
	    t
	  nil)))))


(defun erlang-in-literal (&optional lim)
  "Test if point is in string, quoted atom or comment.

Return one of the three atoms `atom', `string', and `comment'.
Should the point be inside none of the above mentioned types of
context, nil is returned."
  (save-excursion
    (let* ((lim (or lim (save-excursion
			  (erlang-beginning-of-clause)
			  (point))))
	   (state (if (fboundp 'syntax-ppss) ; post Emacs 21.3
		      (funcall (symbol-function 'syntax-ppss))
		    (parse-partial-sexp lim (point)))))
      (cond
       ((eq (nth 3 state) ?') 'atom)
       ((nth 3 state) 'string)
       ((nth 4 state) 'comment)
       (t nil)))))


(defun erlang-at-end-of-function-p ()
  "Test if point is at end of an Erlang function.

This function is designed to be a member of a criteria list."
  (eq (save-excursion (erlang-skip-blank) (point))
      (save-excursion
	(erlang-beginning-of-function -1) (point))))


(defun erlang-at-end-of-clause-p ()
  "Test if point is at end of an Erlang clause.

This function is designed to be a member of a criteria list."
  (eq (save-excursion (erlang-skip-blank) (point))
      (save-excursion
	(erlang-beginning-of-clause -1) (point))))


(defun erlang-stop-when-inside-argument-list ()
  "Return `stop' if inside parenthesis list, nil otherwise.

Knows about the list comprehension syntax.  When the point is
after `||', `stop' is not returned.

This function is designed to be a member of a criteria list."
  (save-excursion
    (condition-case nil
	(let ((orig-point (point))
	      (state nil))
	  (up-list -1)
	  (if (not (eq (following-char) ?\[))
	      'stop
	    ;; Do not return `stop' when inside a list comprehension
	    ;; construction.  (The point must be after `||').
	    (while (< (point) orig-point)
	      (setq state (erlang-partial-parse (point) orig-point state)))
	    (if (and (car state) (eq (car (car (car state))) '||))
		nil
	      'stop)))
      (error 
       nil))))


(defun erlang-stop-when-at-guard ()
  "Return `stop' when at function guards.

This function is designed to be a member of a criteria list."
  (save-excursion
    (beginning-of-line)
    (if (and (looking-at (eval-when-compile
			   (concat "^" erlang-atom-regexp "\\s *(")))
	     (not (looking-at
		   (eval-when-compile
		     (concat "^" erlang-atom-regexp ".*->")))))
	'stop
      nil)))


(defun erlang-stop-when-in-type-spec ()
  "Return `stop' when in a type spec line.

This function is designed to be a member of a criteria list."
  (save-excursion
    (beginning-of-line)
    (when (save-match-data (looking-at "-\\(spec\\|type\\)"))
      'stop)))


(defun erlang-next-lines-empty-p ()
  "Return non-nil if next lines are empty.

The variable `erlang-next-lines-empty-threshold' contains the number
of lines required to be empty.

A line containing only spaces and tabs is considered empty.

This function is designed to be a member of a criteria list."
  (and erlang-next-lines-empty-threshold
       (save-excursion
	 (let ((left erlang-next-lines-empty-threshold)
	       (cont t))
	   (while (and cont (> left 0))
	     (forward-line 1)
	     (setq cont (looking-at "\\s *$"))
	     (setq left (- left 1)))
	   cont))))


(defun erlang-at-keyword-end-p ()
  "Test if next readable token is the keyword end.

This function is designed to be a member of a criteria list."
  (save-excursion
    (erlang-skip-blank)
    (looking-at "end[^_a-zA-Z0-9]")))


;; Erlang tags support which is aware of erlang modules.
;;
;; Not yet implemented under XEmacs.  (Hint:  The Emacs 19 etags
;; package works under XEmacs.)

(eval-when-compile
  (if (or (featurep 'bytecomp)
	  (featurep 'byte-compile))
      (progn
	(require 'etags))))


;; Variables:

(defvar erlang-tags-function-alist
  '((find-tag                 . erlang-find-tag)
    (find-tag-other-window    . erlang-find-tag-other-window)
    (find-tag-regexp          . erlang-find-tag-regexp)
    (find-tag-other-frame     . erlang-find-tag-other-frame))
  "Alist of old tags commands and the replacement functions.")

(defvar erlang-tags-installed nil
  "Non-nil when the Erlang tags system is installed.")
(defvar erlang-tags-file-list '()
  "List of files in tag list. Used when finding tag on form `module:'.")
(defvar erlang-tags-completion-table nil
  "Like `tags-completion-table', this table contains `tag' and `module:tag'.")
(defvar erlang-tags-buffer-installed-p nil
  "Non-nil when Erlang module recognising functions installed.")
(defvar erlang-tags-buffer-list '()
  "Temporary list of buffers.")
(defvar erlang-tags-orig-completion-table nil
  "Temporary storage for `tags-completion-table'.")
(defvar erlang-tags-orig-tag-order nil
  "Temporary storage for `find-tag-tag-order'.")
(defvar erlang-tags-orig-regexp-tag-order nil
  "Temporary storage for `find-tag-regexp-tag-order'.")
(defvar erlang-tags-orig-search-function nil
  "Temporary storage for `find-tag-search-function'.")
(defvar erlang-tags-orig-regexp-search-function nil
  "Temporary storage for `find-tag-regexp-search-function'.")
(defvar erlang-tags-orig-format-hooks nil
  "Temporary storage for `tags-table-format-hooks'.") ;v19
(defvar erlang-tags-orig-format-functions nil
  "Temporary storage for `tags-table-format-functions'.") ;v > 19

(defun erlang-tags-init ()
  "Install an alternate version of tags, aware of Erlang modules.

After calling this function, the tags functions are aware of
Erlang modules.  Tags can be entered on the for `module:tag' as well
as on the old form `tag'.

In the completion list, `module:tag' and `module:' shows up.

Call this function from an appropriate init file, or add it to
Erlang mode hook with the commands:
    (add-hook 'erlang-mode-hook 'erlang-tags-init)
    (add-hook 'erlang-shell-mode-hook 'erlang-tags-init)

This function only works under Emacs 18 and Emacs 19.  Currently, It
is not implemented under XEmacs.  (Hint: The Emacs 19 etags module
works under XEmacs.)"
  (interactive)
  (cond ((= erlang-emacs-major-version 18)
	 (require 'tags)
	 (erlang-tags-define-keys (current-local-map))
	 (setq erlang-tags-installed t))
	(t
	 (require 'etags)
	 ;; Test on a function available in the Emacs 19 version
	 ;; of tags but not in the XEmacs version.
	 (if (not (fboundp 'find-tag-noselect))
	     ()
	   (erlang-tags-define-keys (current-local-map))
	   (setq erlang-tags-installed t)))))


;; Set all keys bound to `find-tag' et.al. in the global map and the
;; menu to `erlang-find-tag' et.al. in `map'.
;;
;; The function `substitute-key-definition' does not work properly
;; in all version of Emacs.

(defun erlang-tags-define-keys (map)
  "Bind tags commands to keymap MAP aware of Erlang modules."
  (let ((alist erlang-tags-function-alist))
    (while alist
      (let* ((old (car (car alist)))
	     (new (cdr (car alist)))
	     (keys (append (where-is-internal old global-map))))
	(while keys
	  (define-key map (car keys) new)
	  (setq keys (cdr keys))))
      (setq alist (cdr alist))))
  ;; Update the menu.
  (erlang-menu-substitute erlang-menu-base-items erlang-tags-function-alist)
  (erlang-menu-init))


;; There exists a variable `find-tag-default-function'.  It is not used
;; since `complete-tag' uses it to get current word under point.  In that
;; situation we don't want the module to be prepended.

(defun erlang-find-tag-default ()
  "Return the default tag.
Search `-import' list of imported functions.
Single quotes are been stripped away."
  (let ((mod-func (erlang-get-function-under-point)))
    (cond ((null mod-func)
	   nil)
	  ((null (car mod-func))
	   (nth 1 mod-func))
	  (t
	   (concat (car mod-func) ":" (nth 1 mod-func))))))


;; Return `t' since it is used inside `tags-loop-form'.
;;;###autoload
(defun erlang-find-tag (modtagname &optional next-p regexp-p)
  "Like `find-tag'.  Capable of retrieving Erlang modules.

Tags can be given on the forms `tag', `module:', `module:tag'."
  (interactive (erlang-tag-interactive "Find `module:tag' or `tag': "))
  (switch-to-buffer (erlang-find-tag-noselect modtagname next-p regexp-p))
  t)


;; Code mainly from `find-tag-other-window' in `etags.el'.
;;;###autoload
(defun erlang-find-tag-other-window (tagname &optional next-p regexp-p)
  "Like `find-tag-other-window' but aware of Erlang modules."
  (interactive (erlang-tag-interactive
		"Find `module:tag' or `tag' other window: "))

  ;; This is to deal with the case where the tag is found in the
  ;; selected window's buffer; without this, point is moved in both
  ;; windows.  To prevent this, we save the selected window's point
  ;; before doing find-tag-noselect, and restore it afterwards.
  (let* ((window-point (window-point (selected-window)))
	 (tagbuf (erlang-find-tag-noselect tagname next-p regexp-p))
	 (tagpoint (progn (set-buffer tagbuf) (point))))
    (set-window-point (prog1
			  (selected-window)
			(switch-to-buffer-other-window tagbuf)
			;; We have to set this new window's point; it
			;; might already have been displaying a
			;; different portion of tagbuf, in which case
			;; switch-to-buffer-other-window doesn't set
			;; the window's point from the buffer.
			(set-window-point (selected-window) tagpoint))
		      window-point)))


(defun erlang-find-tag-other-frame (tagname &optional next-p)
  "Like `find-tag-other-frame' but aware of Erlang modules."
  (interactive (erlang-tag-interactive
		"Find `module:tag' or `tag' other frame: "))
  (let ((pop-up-frames t))
    (erlang-find-tag-other-window tagname next-p)))


(defun erlang-find-tag-regexp (regexp &optional next-p other-window)
  "Like `find-tag-regexp' but aware of Erlang modules."
  (interactive (if (fboundp 'find-tag-regexp)
		   (erlang-tag-interactive
		    "Find `module:regexp' or `regexp': ")
		 (error "This version of Emacs can't find tags by regexps")))
  (funcall (if other-window
	       'erlang-find-tag-other-window
	     'erlang-find-tag)
	   regexp next-p t))


;; Just like C-u M-.  This could be added to the menu.
(defun erlang-find-next-tag ()
  "Find next tag, like \\[find-tag] with prefix arg."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (if erlang-tags-installed
	(call-interactively 'erlang-find-tag)
      (call-interactively 'find-tag))))


;; Mimics `find-tag-noselect' found in `etags.el', but uses `find-tag' to
;; be compatible with `tags.el'.
;;
;; Handles three cases:
;; * `module:'  Loop over all possible file names.  Stop if a file-name
;;              without extension and directory matches the module.
;;
;; * `module:tag'
;;		Emacs 19: Replace test functions with functions aware of
;;	        Erlang modules.  Tricky because the etags system wasn't
;;		built for these kind of operations...
;;
;;              Emacs 18: We loop over `find-tag' until we find a file
;;              whose module matches the requested module.  The
;;              drawback is that a lot of files could be loaded into
;;              Emacs.
;;
;; * `tag'      Just give it to `find-tag'.

(defun erlang-find-tag-noselect (modtagname &optional next-p regexp-p)
  "Like `find-tag-noselect' but aware of Erlang modules."
  (interactive (erlang-tag-interactive "Find `module:tag' or `tag': "))
  (or modtagname
      (setq modtagname (symbol-value 'last-tag)))
  (funcall (symbol-function 'set) 'last-tag modtagname)
  ;; `tags.el' uses this variable to record how M-, would
  ;; know where to restart a tags command.
  (if (boundp 'tags-loop-form)
      (funcall (symbol-function 'set)
	       'tags-loop-form '(erlang-find-tag nil t)))
  (save-window-excursion
    (cond
     ((string-match ":$" modtagname)
      ;; Only the module name was given.  Read all files whose file name
      ;; match.
      (let ((modname (substring modtagname 0 (match-beginning 0)))
	    (file nil))
	(if (not next-p)
	    (save-excursion
	      (visit-tags-table-buffer)
	      (setq erlang-tags-file-list
		    (funcall (symbol-function 'tags-table-files)))))
	(while (null file)
	  (or erlang-tags-file-list
	      (save-excursion
		(if (and (featurep 'etags)
			 (funcall
			  (symbol-function 'visit-tags-table-buffer) 'same)
			 (funcall
			  (symbol-function 'visit-tags-table-buffer) t))
		    (setq erlang-tags-file-list
			  (funcall (symbol-function 'tags-table-files)))
		  (error "No %stags containing %s" (if next-p "more " "")
			 modtagname))))
	  (if erlang-tags-file-list
	      (let ((this-module (erlang-get-module-from-file-name
				  (car erlang-tags-file-list))))
		(if (and (stringp this-module)
			 (string= modname this-module))
		    (setq file (car erlang-tags-file-list)))
		(setq erlang-tags-file-list (cdr erlang-tags-file-list)))))
	(set-buffer (or (get-file-buffer file)
			(find-file-noselect file)))))

     ((string-match ":" modtagname)
      (if (boundp 'find-tag-tag-order)
	  ;; Method one: Add module-recognising functions to the
	  ;; list of order functions.  However, the tags system
	  ;; from Emacs 18, and derives thereof (read: XEmacs)
	  ;; hasn't got this feature.
	  (progn
	    (erlang-tags-install-module-check)
	    (unwind-protect
		(funcall (symbol-function 'find-tag)
			 modtagname next-p regexp-p)
	      (erlang-tags-remove-module-check)))
	;; Method two: Call the tags system until a file matching
	;; the module is found.  This could result in that many
	;; files are read. (e.g. The tag "foo:file" will take a
	;; while to process.)
	(let* ((modname (substring modtagname 0 (match-beginning 0)))
	       (tagname (substring modtagname (match-end 0) nil))
	       (last-tag tagname)
	       file)
	  (while
	      (progn
		(funcall (symbol-function 'find-tag) tagname next-p regexp-p)
		(setq next-p t)
		;; Determine the module form the file name.  (The
		;; alternative, to check `-module', would make this
		;; code useless for non-Erlang programs.)
		(setq file (erlang-get-module-from-file-name buffer-file-name))
		(not (and (stringp file)
			  (string= modname file))))))))
     (t
      (funcall (symbol-function 'find-tag) modtagname next-p regexp-p)))
    (current-buffer)))			; Return the new buffer.


;; Process interactive arguments for erlang-find-tag-*.
;;
;; Negative arguments work only for `etags', not `tags'.  This is not
;; a problem since negative arguments means step back into the
;; history list, a feature not implemented in `tags'.

(defun erlang-tag-interactive (prompt)
  (condition-case nil
      (require 'etags)
    (error
     (require 'tags)))
  (if current-prefix-arg
      (list nil (if (< (prefix-numeric-value current-prefix-arg) 0)
		    '-
		  t))
    (let* ((default (erlang-find-tag-default))
	   (prompt (if default
		       (format "%s(default %s) " prompt default)
		     prompt))
	   (spec (if (featurep 'etags)
		     (completing-read prompt 'erlang-tags-complete-tag)
		   (read-string prompt))))
      (list (if (equal spec "")
		(or default (error "There is no default tag"))
	      spec)))))


;; Search tag functions which are aware of Erlang modules.  The tactic
;; is to store new search functions into the local variables of the
;; TAGS buffers.  The variables are restored directly after the
;; search.  The situation is complicated by the fact that new TAGS
;; files can be loaded during the search.
;;

(defun erlang-tags-install-module-check ()
  "Install our own tag search functions."
  ;; Make sure our functions are installed in TAGS files loaded
  ;; into Emacs while searching.
  (cond
   ((>= erlang-emacs-major-version 20)
    (setq erlang-tags-orig-format-functions
          (symbol-value 'tags-table-format-functions))
    (funcall (symbol-function 'set) 'tags-table-format-functions
             (cons 'erlang-tags-recognize-tags-table
                   erlang-tags-orig-format-functions))
    (setq erlang-tags-buffer-list '())
    )
   (t
    (setq erlang-tags-orig-format-hooks
          (symbol-value 'tags-table-format-hooks))
    (funcall (symbol-function 'set) 'tags-table-format-hooks
	     (cons 'erlang-tags-recognize-tags-table
		   erlang-tags-orig-format-hooks))
    (setq erlang-tags-buffer-list '())
   ))
   
  ;; Install our functions in the TAGS files already resident.
  (save-excursion
    (let ((files (symbol-value 'tags-table-computed-list)))
      (while files
	(if (stringp (car files))
	    (if (get-file-buffer (car files))
		(progn
		  (set-buffer (get-file-buffer (car files)))
		  (erlang-tags-install-local))))
	(setq files (cdr files))))))


(defun erlang-tags-install-local ()
  "Install our tag search functions in current buffer."
  (if erlang-tags-buffer-installed-p
      ()
    ;; Mark this buffer as "installed" and record.
    (set (make-local-variable 'erlang-tags-buffer-installed-p) t)
    (setq erlang-tags-buffer-list
	  (cons (current-buffer) erlang-tags-buffer-list))

    ;; Save the original values.
    (set (make-local-variable 'erlang-tags-orig-tag-order)
	 (symbol-value 'find-tag-tag-order))
    (set (make-local-variable 'erlang-tags-orig-regexp-tag-order)
	 (symbol-value 'find-tag-regexp-tag-order))
    (set (make-local-variable 'erlang-tags-orig-search-function)
	 (symbol-value 'find-tag-search-function))
    (set (make-local-variable 'erlang-tags-orig-regexp-search-function)
	 (symbol-value 'find-tag-regexp-search-function))

    ;; Install our own functions.
    (set (make-local-variable 'find-tag-search-function)
	 'erlang-tags-search-forward)
    (set (make-local-variable 'find-tag-regexp-search-function)
	 'erlang-tags-regexp-search-forward)
    (set (make-local-variable 'find-tag-tag-order)
	 '(erlang-tag-match-module-p))
    (set (make-local-variable 'find-tag-regexp-tag-order)
	 '(erlang-tag-match-module-regexp-p))))


(defun erlang-tags-remove-module-check ()
  "Remove our own tags search functions."
  (cond
   ((>= erlang-emacs-major-version 20)
    (funcall (symbol-function 'set)
	     'tags-table-format-functions
	     erlang-tags-orig-format-functions)
    )
   (t 
    (funcall (symbol-function 'set)
	     'tags-table-format-hooks
	     erlang-tags-orig-format-hooks)
    ))

  ;; Remove our functions from the TAGS files.  (Note that
  ;; `tags-table-computed-list' need not be the same list as when
  ;; the search was started.)
  (save-excursion
    (let ((buffers erlang-tags-buffer-list))
      (while buffers
	(if (buffer-name (car buffers))
	    (progn
	      (set-buffer (car buffers))
	      (erlang-tags-remove-local)))
	(setq buffers (cdr buffers))))))


(defun erlang-tags-remove-local ()
  "Remove our tag search functions from current buffer."
  (if (null erlang-tags-buffer-installed-p)
      ()
    (funcall (symbol-function 'set) 'erlang-tags-buffer-installed-p nil)
    (funcall (symbol-function 'set)
	     'find-tag-tag-order erlang-tags-orig-tag-order)
    (funcall (symbol-function 'set)
	     'find-tag-regexp-tag-order erlang-tags-orig-regexp-tag-order)
    (funcall (symbol-function 'set)
	     'find-tag-search-function erlang-tags-orig-search-function)
    (funcall (symbol-function 'set)
	     'find-tag-regexp-search-function
	     erlang-tags-orig-regexp-search-function)))


(defun erlang-tags-recognize-tags-table ()
  "Install our functions in all loaded TAGS files.

This function is added to `tags-table-format-hooks/functions' when searching
for a tag on the form `module:tag'."
  (if (null (funcall (symbol-function 'etags-recognize-tags-table)))
      nil
    (erlang-tags-install-local)
    t))


(defun erlang-tags-search-forward (tag &optional bound noerror count)
  "Forward search function, aware of Erlang module prefix."
  (if (string-match ":" tag)
      (setq tag (substring tag (match-end 0) nil)))
  ;; Avoid unintended recursion.
  (if (eq erlang-tags-orig-search-function 'erlang-tags-search-forward)
      (search-forward tag bound noerror count)
    (funcall erlang-tags-orig-search-function tag bound noerror count)))


(defun erlang-tags-regexp-search-forward (tag &optional bound noerror count)
  "Forward regexp search function, aware of Erlang module prefix."
  (if (string-match ":" tag)
      (setq tag (substring tag (match-end 0) nil)))
  (if (eq erlang-tags-orig-regexp-search-function
	  'erlang-tags-regexp-search-forward)
      (re-search-forward tag bound noerror count)
    (funcall erlang-tags-orig-regexp-search-function
	     tag bound noerror count)))


;; t if point is at a tag line that matches TAG, containing
;; module information.  Assumes that all other order functions
;; are stored in `erlang-tags-orig-[regex]-tag-order'.

(defun erlang-tag-match-module-p (tag)
  (erlang-tag-match-module-common-p tag erlang-tags-orig-tag-order))

(defun erlang-tag-match-module-regexp-p (tag)
  (erlang-tag-match-module-common-p tag erlang-tags-orig-regexp-tag-order))

(defun erlang-tag-match-module-common-p (tag order)
  (let ((mod nil)
	(found nil))
    (if (string-match ":" tag)
	(progn
	  (setq mod (substring tag 0 (match-beginning 0)))
	  (setq tag (substring tag (match-end 0) nil))))
    (while (and order (not found))
      (setq found
	    (and (not (memq (car order)
			    '(erlang-tag-match-module-p
			      erlang-tag-match-module-regexp-p)))
		 (funcall (car order) tag)))
      (setq order (cdr order)))
    (and found
	 (or (null mod)
	     (string= mod (erlang-get-module-from-file-name
			   (file-of-tag)))))))


;;; Tags completion, Emacs 19 `etags' specific.
;;;
;;; The basic idea is to create a second completion table `erlang-tags-
;;; completion-table' containing all normal tags plus tags on the form
;;; `module:tag'.


(defun erlang-complete-tag ()
  "Perform tags completion on the text around point.
Completes to the set of names listed in the current tags table.

Should the Erlang tags system be installed this command knows
about Erlang modules."
  (interactive)
  (condition-case nil
      (require 'etags)
    (error nil))
  (cond ((and erlang-tags-installed
	      (fboundp 'complete-tag))	; Emacs 19
	 (let ((orig-tags-complete-tag (symbol-function 'tags-complete-tag)))
	   (fset 'tags-complete-tag
	     (symbol-function 'erlang-tags-complete-tag))
	   (unwind-protect
	       (funcall (symbol-function 'complete-tag))
	     (fset 'tags-complete-tag orig-tags-complete-tag))))
	((fboundp 'complete-tag)	; Emacs 19
	 (funcall (symbol-function 'complete-tag)))
	((fboundp 'tag-complete-symbol)	; XEmacs
	 (funcall (symbol-function 'tag-complete-symbol)))
	(t
	 (error "This version of Emacs can't complete tags"))))


;; Based on `tags-complete-tag', but this one uses
;; `erlang-tags-completion-table' instead of `tags-completion-table'.
;;
;; This is the entry-point called by system function `completing-read'.
(defun erlang-tags-complete-tag (string predicate what)
  (save-excursion
    ;; If we need to ask for the tag table, allow that.
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (if (eq what t)
	(all-completions string (erlang-tags-completion-table) predicate)
      (try-completion string (erlang-tags-completion-table) predicate))))


;; `tags-completion-table' calls itself recursively, make it
;; call our own wedge instead.  Note that the recursive call
;; is very rare;  it only occurs when a tags-file contains
;; `include'-statements.
(defun erlang-tags-completion-table ()
  "Build completion table.  Tags on the form `tag' or `module:tag'."
  (setq erlang-tags-orig-completion-table
	(symbol-function 'tags-completion-table))
  (fset 'tags-completion-table
	(symbol-function 'erlang-tags-completion-table-1))
  (unwind-protect
      (erlang-tags-completion-table-1)
    (fset 'tags-completion-table
	  erlang-tags-orig-completion-table)))


(defun erlang-tags-completion-table-1 ()
  (make-local-variable 'erlang-tags-completion-table)
  (or erlang-tags-completion-table
      (let ((tags-completion-table nil)
	    (tags-completion-table-function
	     'erlang-etags-tags-completion-table))
	(funcall erlang-tags-orig-completion-table)
	(setq erlang-tags-completion-table tags-completion-table))))


;; Based on `etags-tags-completion-table'.  The difference is that we
;; add three symbols to the vector, the tag, module: and module:tag.
;; The module is extracted from the file name of a tag.  (This one
;; only works if we are looking at an `etags' file. However, this is
;; the only format supported by Emacs, so far.)
(defun erlang-etags-tags-completion-table ()
  (let ((table (make-vector 511 0))
	(file nil))
    (save-excursion
      (goto-char (point-min))
      ;; This monster regexp matches an etags tag line.
      ;;   \1 is the string to match;
      ;;   \2 is not interesting;
      ;;   \3 is the guessed tag name; XXX guess should be better eg DEFUN
      ;;   \4 is not interesting;
      ;;   \5 is the explicitly-specified tag name.
      ;;   \6 is the line to start searching at;
      ;;   \7 is the char to start searching at.
      (while (progn
	       (while (and
		       (eq (following-char) ?\f)
		       (looking-at "\f\n\\([^,\n]*\\),.*\n"))
		 (setq file (buffer-substring
			     (match-beginning 1) (match-end 1)))
		 (goto-char (match-end 0)))
	       (re-search-forward
		"\
^\\(\\([^\177]+[^-a-zA-Z0-9_$\177]+\\)?\\([-a-zA-Z0-9_$?:]+\\)\
\[^-a-zA-Z0-9_$?:\177]*\\)\177\\(\\([^\n\001]+\\)\001\\)?\
\\([0-9]+\\)?,\\([0-9]+\\)?\n"
		nil t))
	(let ((tag (if (match-beginning 5)
		       ;; There is an explicit tag name.
		       (buffer-substring (match-beginning 5) (match-end 5))
		     ;; No explicit tag name.  Best guess.
		     (buffer-substring (match-beginning 3) (match-end 3))))
	      (module (and file
			   (erlang-get-module-from-file-name file))))
	  (intern tag table)
	  (if (stringp module)
	      (progn
		(intern (concat module ":" tag) table)
		;; Only the first one will be stored in the table.
		(intern (concat module ":") table))))))
    table))

;;;
;;; Prepare for other methods to run an Erlang slave process.
;;;

(defvar erlang-shell-function 'inferior-erlang
  "Command to execute start a new Erlang shell.

Change this variable to use your favorite
Erlang compilation package.")

(defvar erlang-shell-display-function 'inferior-erlang-run-or-select
  "Command to execute to display Erlang shell.

Change this variable to use your favorite
Erlang compilation package.")

(defvar erlang-compile-function 'inferior-erlang-compile
  "Command to execute to compile current buffer.

Change this variable to use your favorite
Erlang compilation package.")

(defvar erlang-compile-erlang-function "c"
  "Erlang function to call to compile an erlang file.")

(defvar erlang-compile-display-function 'inferior-erlang-run-or-select
  "Command to execute to view last compilation.

Change this variable to use your favorite
Erlang compilation package.")

(defvar erlang-next-error-function 'inferior-erlang-next-error
  "Command to execute to go to the next error.

Change this variable to use your favorite Erlang compilation
package.  Not used in Emacs 21.")


;;;###autoload
(defun erlang-shell ()
  "Start a new Erlang shell.

The variable `erlang-shell-function' decides which method to use,
default is to start a new Erlang host.  It is possible that, in the
future, a new shell on an already running host will be started."
  (interactive)
  (call-interactively erlang-shell-function))


;;;###autoload (autoload 'run-erlang "erlang" "Start a new Erlang shell." t)

;; It is customary for Emacs packages to supply a function on this
;; form, even though it violates the `erlang-*' name convention.
(defalias 'run-erlang 'erlang-shell)


(defun erlang-shell-display ()
  "Display an Erlang shell, or start a new."
  (interactive)
  (call-interactively erlang-shell-display-function))


;;;###autoload
(defun erlang-compile ()
  "Compile Erlang module in current buffer."
  (interactive)
  (call-interactively erlang-compile-function))


(defun erlang-compile-display ()
  "Display compilation output."
  (interactive)
  (call-interactively erlang-compile-display-function))


(defun erlang-next-error ()
  "Display next error message from the latest compilation."
  (interactive)
  (call-interactively erlang-next-error-function))



;;;
;;; Erlang Shell Mode -- Major mode used for Erlang shells.
;;;

;; This mode is designed to be implementation independent,
;; e.g. it does not assume that we are running an inferior
;; Erlang, there exists a lot of other possibilities.


(defvar erlang-shell-buffer-name "*erlang*"
  "The name of the Erlang link shell buffer.")

(defvar erlang-shell-mode-map nil
  "Keymap used by Erlang shells.")


(defvar erlang-shell-mode-hook nil
  "*User functions to run when an Erlang shell is started.

This hook is used to change the behaviour of Erlang mode.  It is
normally used by the user to personalise the programming environment.
When used in a site init file, it could be used to customise Erlang
mode for all users on the system.

The function added to this hook is run every time a new Erlang
shell is started.

See also `erlang-load-hook', a hook which is run once, when Erlang
mode is loaded, and `erlang-mode-hook' which is run every time a new
Erlang source file is loaded into Emacs.")


(defvar erlang-input-ring-file-name "~/.erlang_history"
  "*When non-nil, file name used to store Erlang shell history information.")


(defun erlang-shell-mode ()
  "Major mode for interacting with an Erlang shell.

We assume that we already are in Comint mode.

The following special commands are available:
\\{erlang-shell-mode-map}"
  (interactive)
  (setq major-mode 'erlang-shell-mode)
  (setq mode-name "Erlang Shell")
  (erlang-mode-variables)
  (if erlang-shell-mode-map
      nil
    (setq erlang-shell-mode-map (copy-keymap comint-mode-map))
    (erlang-shell-mode-commands erlang-shell-mode-map))
  (use-local-map erlang-shell-mode-map)
  (unless inferior-erlang-use-cmm
    ;; This was originally not a marker, but it needs to be, at least
    ;; in Emacs 21, and should be backwards-compatible.  Otherwise,
    ;; would need to test whether compilation-parsing-end is a marker
    ;; after requiring `compile'.
    (set (make-local-variable 'compilation-parsing-end) (copy-marker 1))
    (set (make-local-variable 'compilation-error-list) nil)
    (set (make-local-variable 'compilation-old-error-list) nil))
  ;; Needed when compiling directly from the Erlang shell.
  (setq compilation-last-buffer (current-buffer))
  (erlang-add-compilation-alist erlang-error-regexp-alist)
  (setq comint-prompt-regexp "^[^>=]*> *")
  (setq comint-eol-on-send t)
  (setq comint-input-ignoredups t)
  (setq comint-scroll-show-maximum-output t)
  (setq comint-scroll-to-bottom-on-output t)
  ;; In Emacs 19.30, `add-hook' has got a `local' flag, use it.  If
  ;; the call fails, just call the normal `add-hook'.
  (condition-case nil
      (progn
        (add-hook 'comint-output-filter-functions
		  'inferior-erlang-strip-delete nil t)
        (add-hook 'comint-output-filter-functions
		  'inferior-erlang-strip-ctrl-m nil t))
    (error
     (funcall (symbol-function 'make-local-hook)
	      'comint-output-filter-functions) ; obsolete as of Emacs 21.1
     (add-hook 'comint-output-filter-functions 'inferior-erlang-strip-delete)
     (add-hook 'comint-output-filter-functions 'inferior-erlang-strip-ctrl-m)))
  ;; Some older versions of comint don't have an input ring.
  (if (fboundp 'comint-read-input-ring)
      (progn
	(setq comint-input-ring-file-name erlang-input-ring-file-name)
	(comint-read-input-ring t)
	(make-local-variable 'kill-buffer-hook)
	(add-hook 'kill-buffer-hook 'comint-write-input-ring)))
  ;; At least in Emacs 21, we need to be in `compilation-minor-mode'
  ;; for `next-error' to work.  We can avoid it clobbering the shell
  ;; keys thus.
  (when inferior-erlang-use-cmm
    (compilation-minor-mode 1)
    (set (make-local-variable 'minor-mode-overriding-map-alist)
	 `((compilation-minor-mode
	    . ,(let ((map (make-sparse-keymap)))
		 ;; It would be useful to put keymap properties on the
		 ;; error lines so that we could use RET and mouse-2
		 ;; on them directly.
		 (when (boundp 'compilation-skip-threshold) ; new compile.el
		   (define-key map [mouse-2] #'erlang-mouse-2-command)
		   (define-key map "\C-m" #'erlang-RET-command))
		 (if (boundp 'compilation-menu-map)
		     (define-key map [menu-bar compilation]
		       (cons "Errors" compilation-menu-map)))
		 map)))))
  (run-hooks 'erlang-shell-mode-hook))


(defun erlang-mouse-2-command (event)
  "Command bound to `mouse-2' in inferior Erlang buffer.
Selects Comint or Compilation mode command as appropriate."
  (interactive "e")
  (if (save-window-excursion
	(save-excursion
	  (mouse-set-point event)
	  (consp (get-text-property (line-beginning-position) 'message))))
      (call-interactively (lookup-key compilation-mode-map [mouse-2]))
    (call-interactively (lookup-key comint-mode-map [mouse-2]))))

(defun erlang-RET-command ()
  "Command bound to `RET' in inferior Erlang buffer.
Selects Comint or Compilation mode command as appropriate."
  (interactive)
  (if (consp (get-text-property (line-beginning-position) 'message))
      (call-interactively (lookup-key compilation-mode-map "\C-m"))
    (call-interactively (lookup-key comint-mode-map "\C-m"))))

(defun erlang-shell-mode-commands (map)
  (define-key map "\M-\t"    'erlang-complete-tag)
  (define-key map "\C-a"     'comint-bol) ; Normally the other way around.
  (define-key map "\C-c\C-a" 'beginning-of-line)
  (define-key map "\C-d"     nil)	; Was `comint-delchar-or-maybe-eof'
  (define-key map "\M-\C-m"  'compile-goto-error)
  (unless inferior-erlang-use-cmm
    (define-key map "\C-x`"    'erlang-next-error)))

;;;
;;; Inferior Erlang -- Run an Erlang shell as a subprocess.
;;;

(defvar inferior-erlang-display-buffer-any-frame nil
  "*When nil, `inferior-erlang-display-buffer' use only selected frame.
When t, all frames are searched.  When 'raise, the frame is raised.")

(defvar inferior-erlang-shell-type 'newshell
  "The type of Erlang shell to use.

When this variable is set to the atom `oldshell', the old shell is used.
When set to `newshell' the new shell is used.  Should the variable be
nil, the default shell is used.

This variable influence the setting of other variables.")

(defvar inferior-erlang-machine "erl"
  "*The name of the Erlang shell.")

(defvar inferior-erlang-machine-options '()
  "*The options used when activating the Erlang shell.

This must be a list of strings.")

(defvar inferior-erlang-process-name "inferior-erlang"
  "The name of the inferior Erlang process.")

(defvar inferior-erlang-buffer-name erlang-shell-buffer-name
  "The name of the inferior Erlang buffer.")

(defvar inferior-erlang-prompt-timeout 60
  "*Number of seconds before `inferior-erlang-wait-prompt' timeouts.

The time specified is waited after every output made by the inferior
Erlang shell.  When this variable is t, we assume that we always have
a prompt.  When nil, we will wait forever, or until \\[keyboard-quit].")

(defvar inferior-erlang-process nil
  "Process of last invoked inferior Erlang, or nil.")

(defvar inferior-erlang-buffer nil
  "Buffer of last invoked inferior Erlang, or nil.")

;; Enable uniquifying Erlang shell buffers based on directory name.
(eval-after-load "uniquify"
  '(add-to-list 'uniquify-list-buffers-directory-modes 'erlang-shell-mode))

;;;###autoload
(defun inferior-erlang (&optional command)
  "Run an inferior Erlang.
With prefix command, prompt for command to start Erlang with.

This is just like running Erlang in a normal shell, except that
an Emacs buffer is used for input and output.
\\<comint-mode-map>
The command line history can be accessed with  \\[comint-previous-input]  and  \\[comint-next-input].
The history is saved between sessions.

Entry to this mode calls the functions in the variables
`comint-mode-hook' and `erlang-shell-mode-hook' with no arguments.

The following commands imitate the usual Unix interrupt and
editing control characters:
\\{erlang-shell-mode-map}"
  (interactive
   (when current-prefix-arg
     (list (if (fboundp 'read-shell-command)
               ;; `read-shell-command' is a new function in Emacs 23.
	       (read-shell-command "Erlang command: ")
	     (read-string "Erlang command: ")))))
  (require 'comint)
  (let (cmd opts)
    (if command
        (setq cmd "sh"
              opts (list "-c" command))
      (setq cmd inferior-erlang-machine
            opts inferior-erlang-machine-options)
      (cond ((eq inferior-erlang-shell-type 'oldshell)
             (setq opts (cons "-oldshell" opts)))
            ((eq inferior-erlang-shell-type 'newshell)
             (setq opts (append '("-newshell" "-env" "TERM" "vt100") opts)))))

    ;; Using create-file-buffer and list-buffers-directory in this way
    ;; makes uniquify give each buffer a unique name based on the
    ;; directory.
    (let ((fake-file-name (expand-file-name inferior-erlang-buffer-name default-directory)))
      (setq inferior-erlang-buffer (create-file-buffer fake-file-name))
      (apply 'make-comint-in-buffer
             inferior-erlang-process-name
             inferior-erlang-buffer
             cmd
             nil opts)
      (with-current-buffer inferior-erlang-buffer
        (setq list-buffers-directory fake-file-name))))

  (setq inferior-erlang-process
	(get-buffer-process inferior-erlang-buffer))
  (if (> 21 erlang-emacs-major-version)	; funcalls to avoid compiler warnings
      (funcall (symbol-function 'set-process-query-on-exit-flag) 
	       inferior-erlang-process nil)
    (funcall (symbol-function 'process-kill-without-query) inferior-erlang-process))
  (if erlang-inferior-shell-split-window
      (switch-to-buffer-other-window inferior-erlang-buffer)
    (switch-to-buffer inferior-erlang-buffer)) 
  (if (and (not (eq system-type 'windows-nt))
	   (eq inferior-erlang-shell-type 'newshell))
      (setq comint-process-echoes t))
  (erlang-shell-mode))


(defun inferior-erlang-run-or-select ()
  "Switch to an inferior Erlang buffer, possibly starting new process."
  (interactive)
  (if (null (inferior-erlang-running-p))
      (inferior-erlang)
    (inferior-erlang-display-buffer t)))


(defun inferior-erlang-display-buffer (&optional select)
  "Make the inferior Erlang process visible.
The window is returned.

Should `inferior-erlang-display-buffer-any-frame' be nil the buffer is
displayed in the current frame.  Should it be non-nil, and the buffer
already is visible in any other frame, no new window will be created.
Should it be the atom 'raise, the frame containing the window will
be raised.

Should the optional argument SELECT be non-nil, the window is
selected.  Should the window be in another frame, that frame is raised.

Note, should the mouse pointer be places outside the raised frame, that
frame will become deselected before the next command."
  (interactive)
  (or (inferior-erlang-running-p)
      (error "No inferior Erlang process is running"))
  (let ((win (inferior-erlang-window
	      inferior-erlang-display-buffer-any-frame))
	(frames-p (fboundp 'selected-frame)))
    (if (null win)
	(let ((old-win (selected-window)))
	  (save-excursion
	    (switch-to-buffer-other-window inferior-erlang-buffer)
	    (setq win (selected-window)))
	  (select-window old-win))
      (if (and window-system
	       frames-p
	       (or select
		   (eq inferior-erlang-display-buffer-any-frame 'raise))
	       (not (eq (selected-frame) (window-frame win))))
	  (raise-frame (window-frame win))))
    (if select
	(select-window win))
    (sit-for 0)
    win))


(defun inferior-erlang-running-p ()
  "Non-nil when an inferior Erlang is running."
  (and inferior-erlang-process
       (memq (process-status inferior-erlang-process) '(run open))
       inferior-erlang-buffer
       (buffer-name inferior-erlang-buffer)))


(defun inferior-erlang-window (&optional all-frames)
  "Return the window containing the inferior Erlang, or nil."
  (and (inferior-erlang-running-p)
       (if (and all-frames (>= erlang-emacs-major-version 19))
	   (get-buffer-window inferior-erlang-buffer t)
	 (get-buffer-window inferior-erlang-buffer))))


(defun inferior-erlang-wait-prompt ()
  "Wait until the inferior Erlang shell prompt appears."
  (if (eq inferior-erlang-prompt-timeout t)
      ()
    (or (inferior-erlang-running-p)
	(error "No inferior Erlang shell is running"))
    (save-excursion
      (set-buffer inferior-erlang-buffer)
      (let ((msg nil))
	(while (save-excursion
		 (goto-char (process-mark inferior-erlang-process))
		 (forward-line 0)
		 (not (looking-at comint-prompt-regexp)))
	  (if msg
	      ()
	    (setq msg t)
	    (message "Waiting for Erlang shell prompt (press C-g to abort)."))
	  (or (accept-process-output inferior-erlang-process
				     inferior-erlang-prompt-timeout)
	      (error "No Erlang shell prompt before timeout")))
	(if msg (message ""))))))

(defun inferior-erlang-send-empty-cmd-unless-already-at-prompt ()
  "If not already at a prompt, try to send an empty cmd to get a prompt.
The empty command resembles hitting RET. This is useful in some
situations, for instance if a crash or error report from sasl
has been printed after the last prompt."
  (save-excursion
    (set-buffer inferior-erlang-buffer)
    (if (> (point-max) 1)
	;; make sure we get a prompt if buffer contains data
	(if (save-excursion
	      (goto-char (process-mark inferior-erlang-process))
	      (forward-line 0)
	      (not (looking-at comint-prompt-regexp)))
	    (inferior-erlang-send-command "")))))

(autoload 'comint-send-input "comint")

(defun inferior-erlang-send-command (cmd &optional hist)
  "Send command CMD to the inferior Erlang.

The contents of the current command line (if any) will
be placed at the next prompt.

If optional second argument is non-nil the command is inserted into
the history list.

Return the position after the newly inserted command."
  (or (inferior-erlang-running-p)
      (error "No inferior Erlang process is running"))
  (let ((old-buffer (current-buffer))
	(insert-point (marker-position (process-mark inferior-erlang-process)))
	(insert-length (if comint-process-echoes
			   0
			 (1+ (length cmd)))))
    (set-buffer inferior-erlang-buffer)
    (goto-char insert-point)
    (insert cmd)
    ;; Strange things happened if `comint-eol-on-send' is declared
    ;; in the `let' expression above, but setq:d here. The
    ;; `set-buffer' statement obviously makes the buffer local
    ;; instance of `comint-eol-on-send' shadow this one.
    ;; I'm considering this a bug in Elisp.
    ;;
    ;; This was previously cautioned against in the Lisp manual.  It
    ;; has been sorted out in Emacs 21.  -- fx
    (let ((comint-eol-on-send nil)
	  (comint-input-filter (if hist comint-input-filter 'ignore)))
      (if (and (not erlang-xemacs-p)
	       (>= emacs-major-version 22))
	  (comint-send-input nil t)
	(comint-send-input)))
    ;; Adjust all windows whose points are incorrect.
    (if (null comint-process-echoes)
	(walk-windows
	 (function
	  (lambda (window)
	    (if (and (eq (window-buffer window) inferior-erlang-buffer)
		     (= (window-point window) insert-point))
		(set-window-point window
				  (+ insert-point insert-length)))))
	 nil t))
    (set-buffer old-buffer)
    (+ insert-point insert-length)))


(defun inferior-erlang-strip-delete (&optional s)
  "Remove `^H' (delete) and the characters it was supposed to remove."
  (interactive)
  (if (and (boundp 'comint-last-input-end)
	   (boundp 'comint-last-output-start))
      (save-excursion
	(goto-char
	 (if (interactive-p)
	     (symbol-value 'comint-last-input-end)
	   (symbol-value 'comint-last-output-start)))
	(while (progn (skip-chars-forward "^\C-h")
		      (not (eq (point) (point-max))))
	  (delete-char 1)
	  (or (bolp)
	      (backward-delete-char 1))))))


;; Basically `comint-strip-ctrl-m', with a few extra checks.
(defun inferior-erlang-strip-ctrl-m (&optional string)
  "Strip trailing `^M' characters from the current output group."
  (interactive)
  (if (and (boundp 'comint-last-input-end)
	   (boundp 'comint-last-output-start))
      (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
	(save-excursion
	  (goto-char
	   (if (interactive-p)
	       (symbol-value 'comint-last-input-end)
	     (symbol-value 'comint-last-output-start)))
	  (while (re-search-forward "\r+$" pmark t)
	    (replace-match "" t t))))))


(defun inferior-erlang-compile (arg)
  "Compile the file in the current buffer.

With prefix arg, compiles for debug.

Should Erlang return `{error, nofile}' it could not load the object
module after completing the compilation.  This is due to a bug in the
compile command `c' when using the option `outdir'.

There exists two workarounds for this bug:

  1) Place the directory in the Erlang load path.

  2) Set the Emacs variable `erlang-compile-use-outdir' to nil.
     To do so, place the following line in your `~/.emacs'-file:
        (setq erlang-compile-use-outdir nil)"
  (interactive "P")
  (save-some-buffers)
  (inferior-erlang-prepare-for-input)
  (let* ((dir (inferior-erlang-compile-outdir))
;;; (file (file-name-nondirectory (buffer-file-name)))
	 (noext (substring (buffer-file-name) 0 -4))
	 (opts (append (list (cons 'outdir dir))
		       (if current-prefix-arg
			   (list 'debug_info 'export_all))
		       erlang-compile-extra-opts))
	 end)
    (save-excursion
      (set-buffer inferior-erlang-buffer)
      (compilation-forget-errors))
    (setq end (inferior-erlang-send-command
	       (inferior-erlang-compute-compile-command noext opts)
	       nil))
    (sit-for 0)
    (inferior-erlang-wait-prompt)
    (save-excursion
      (set-buffer inferior-erlang-buffer)
      (setq compilation-error-list nil)
      (set-marker compilation-parsing-end end))
    (setq compilation-last-buffer inferior-erlang-buffer)))

(defun inferior-erlang-prepare-for-input (&optional no-display)
  "Create an inferior erlang buffer if needed and ready it for input.
The buffer is displayed, according to `inferior-erlang-display-buffer'
unless the optional NO-DISPLAY is non-nil."
  (or (inferior-erlang-running-p)
      (save-excursion
	(inferior-erlang)))
  (or (inferior-erlang-running-p)
      (error "Error starting inferior Erlang shell"))
  (if (not no-display)
      (inferior-erlang-display-buffer))
  (inferior-erlang-send-empty-cmd-unless-already-at-prompt)
  (sit-for 0)
  (inferior-erlang-wait-prompt))

(defun inferior-erlang-compile-outdir ()
  "Return the directory to compile the current buffer into."
  (let* ((buffer-dir (directory-file-name
		      (file-name-directory (buffer-file-name))))
	 (parent-dir (directory-file-name
		      (file-name-directory buffer-dir)))
         (ebin-dir (concat (file-name-as-directory parent-dir) "ebin"))
	 (buffer-dir-base-name (file-name-nondirectory 
				(expand-file-name
				 (concat (file-name-as-directory buffer-dir)
					 ".")))))
    (if (and (string= buffer-dir-base-name "src")
	     (file-directory-p ebin-dir))
	(file-name-as-directory ebin-dir)
      (file-name-as-directory buffer-dir))))

(defun inferior-erlang-compute-compile-command (module-name opts)
  (let ((ccfn erlang-compile-command-function-alist)
	(res (inferior-erlang-compute-erl-compile-command module-name opts))
	ccfn-entry
	done)
    (if (not (null (buffer-file-name)))
	(while (and (not done) (not (null ccfn)))
	  (setq ccfn-entry (car ccfn))
	  (setq ccfn (cdr ccfn))
	  (if (string-match (car ccfn-entry) (buffer-file-name))
	      (let ((c-fn (cdr ccfn-entry)))
		(setq done t)
		(if (not (null c-fn))
		    (setq result (funcall c-fn module-name opts)))))))
    result))

(defun inferior-erlang-compute-erl-compile-command (module-name opts)
  (let* ((out-dir-opt (assoc 'outdir opts))
	 (out-dir     (cdr out-dir-opt)))
    (if erlang-compile-use-outdir
	(format "%s(\"%s\"%s)."
		erlang-compile-erlang-function
		module-name
		(inferior-erlang-format-comma-opts opts))
      (let (;; Hopefully, noone else will ever use these...
	    (tmpvar "Tmp7236")
	    (tmpvar2 "Tmp8742"))
	(format
	 (concat
	  "f(%s), {ok, %s} = file:get_cwd(), "
	  "file:set_cwd(\"%s\"), "
	  "%s = %s(\"%s\"%s), file:set_cwd(%s), f(%s), %s.")
	 tmpvar2 tmpvar
	 out-dir
	 tmpvar2
	 erlang-compile-erlang-function
	 module-name (inferior-erlang-format-comma-opts
		      (remq out-dir-opt opts))
	 tmpvar tmpvar tmpvar2)))))

(defun inferior-erlang-compute-leex-compile-command (module-name opts)
  (let ((file-name        (buffer-file-name))
	(erl-compile-expr (inferior-erlang-remove-any-trailing-dot
			   (inferior-erlang-compute-erl-compile-command
			    module-name opts))))
    (format (concat "f(LErr1__), f(LErr2__), "
		    "case case leex:file(\"%s\", [%s]) of"
		    " ok -> ok;"
		    " {ok,_} -> ok;"
		    " {ok,_,_} -> ok;"
		    " LErr1__ -> LErr1__ "
		    "end of"
		    " ok -> %s;"
		    " LErr2__ -> LErr2__ "
		    "end.")
	    file-name
	    (inferior-erlang-format-comma-opts erlang-leex-compile-opts)
	    erl-compile-expr)))

(defun inferior-erlang-compute-yecc-compile-command (module-name opts)
  (let ((file-name        (buffer-file-name))
	(erl-compile-expr (inferior-erlang-remove-any-trailing-dot
			   (inferior-erlang-compute-erl-compile-command
			    module-name opts))))
    (format (concat "f(YErr1__), f(YErr2__), "
		    "case case yecc:file(\"%s\", [%s]) of"
		    " {ok,_} -> ok;"
		    " {ok,_,_} -> ok;"
		    " YErr1__ -> YErr1__ "
		    "end of"
		    " ok -> %s;"
		    " YErr2__ -> YErr2__ "
		    "end.")
	    file-name
	    (inferior-erlang-format-comma-opts erlang-yecc-compile-opts)
	    erl-compile-expr)))

(defun inferior-erlang-remove-any-trailing-dot (str)
  (if (string= (substring str -1) ".")
      (substring str 0 (1- (length str)))
    str))

(defun inferior-erlang-format-comma-opts (opts)
  (if (null opts)
      ""
    (concat ", " (inferior-erlang-format-opts opts))))

(defun inferior-erlang-format-opts (opts)
  (concat "[" (inferior-erlang-string-join (mapcar 'inferior-erlang-format-opt
						   opts)
					   ", ")
	  "]"))

(defun inferior-erlang-format-opt (opt)
  (cond ((stringp opt) (concat "\"" opt "\""))
	((atom opt)    (format "%s" opt))
	((consp opt)   (concat "{" (inferior-erlang-string-join
				    (mapcar 'inferior-erlang-format-opt
					    (list (car opt) (cdr opt)))
				    ", ")
			       "}"))
	(t (error (format "Unexpected opt %s" opt)))))

(defun inferior-erlang-string-join (strs sep)
  (let ((result (or (car strs) "")))
    (setq strs (cdr strs))
    (while strs
      (setq result (concat result sep (car strs)))
      (setq strs (cdr strs)))
    result))

;; `next-error' only accepts buffers with major mode `compilation-mode'
;; or with the minor mode `compilation-minor-mode' activated.
;; (To activate the minor mode is out of the question, since it will
;; ruin the inferior Erlang keymap.)
;; This is done differently in Emacs 21.
(defun inferior-erlang-next-error (&optional argp)
  "Just like `next-error'.
Capable of finding error messages in an inferior Erlang buffer."
  (interactive "P")
  (let ((done nil)
	(buf (or (and (boundp 'next-error-last-buffer)
		      next-error-last-buffer)
		 (and (boundp 'compilation-last-buffer)
		      compilation-last-buffer))))
    (if (and (bufferp buf)
	     (save-excursion
	       (set-buffer buf)
	       (and (eq major-mode 'erlang-shell-mode)
		    (setq major-mode 'compilation-mode))))
	(unwind-protect
	    (progn
	      (setq done t)
	      (next-error argp))
	  (save-excursion
	    (set-buffer buf)
	    (setq major-mode 'erlang-shell-mode))))
    (or done
	(next-error argp))))


(defun inferior-erlang-change-directory (&optional dir)
  "Make the inferior Erlang change directory.
The default is to go to the directory of the current buffer."
  (interactive)
  (or dir (setq dir (file-name-directory (buffer-file-name))))
  (or (inferior-erlang-running-p)
      (error "No inferior Erlang is running"))
  (inferior-erlang-display-buffer)
  (inferior-erlang-send-empty-cmd-unless-already-at-prompt)
  (inferior-erlang-wait-prompt)
  (inferior-erlang-send-command (format "cd('%s')." dir) nil))

(defun erlang-align-arrows (start end)
  "Align arrows (\"->\") in function clauses from START to END.
When called interactively, aligns arrows after function clauses inside
the region.

With a prefix argument, aligns all arrows, not just those in function
clauses.

Example:

sum(L) -> sum(L, 0).
sum([H|T], Sum) -> sum(T, Sum + H);
sum([], Sum) -> Sum.

becomes:

sum(L)          -> sum(L, 0).
sum([H|T], Sum) -> sum(T, Sum + H);
sum([], Sum)    -> Sum."
  (interactive "r")
  (save-excursion
    (let (;; regexp for matching arrows. without a prefix argument,
	  ;; the regexp matches function heads. With a prefix, it
	  ;; matches any arrow.
	  (re (if current-prefix-arg
		  "^.*\\(\\)->"
		(eval-when-compile
		  (concat "^" erlang-atom-regexp ".*\\(\\)->"))))
	  ;; part of regexp matching directly before the arrow
	  (arrow-match-pos (if current-prefix-arg
			       1
			     (1+ erlang-atom-regexp-matches)))
	  ;; accumulator for positions where arrows are found, ordered
	  ;; by buffer position (from greatest to smallest)
	  (arrow-positions '())
	  ;; accumulator for longest distance from start of line to arrow
	  (most-indent 0)
	  ;; marker to track the end of the region we're aligning
	  (end-marker (progn (goto-char end)
			     (point-marker))))
      ;; Pass 1: Find the arrow positions, adjust the whitespace
      ;; before each arrow to one space, and find the greatest
      ;; indentation level.
      (goto-char start)
      (while (re-search-forward re end-marker t)
	(goto-char (match-beginning arrow-match-pos))
	(just-one-space)		; adjust whitespace
	(setq arrow-positions (cons (point) arrow-positions))
	(setq most-indent (max most-indent (erlang-column-number))))
      (set-marker end-marker nil)	; free the marker
      ;; Pass 2: Insert extra padding so that all arrow indentation is
      ;; equal. This is done last-to-first by buffer position, so that
      ;; inserting spaces before one arrow doesn't change the
      ;; positions of the next ones.
      (mapc (lambda (arrow-pos)
	      (goto-char arrow-pos)
	      (let* ((pad (- most-indent (erlang-column-number))))
		(when (> pad 0)
		  (insert-char ?\  pad))))
	    arrow-positions))))

(defun erlang-column-number ()
  "Return the column number of the current position in the buffer.
Tab characters are counted by their visual width."
  (string-width (buffer-substring (line-beginning-position) (point))))

(defun erlang-current-defun ()
  "`add-log-current-defun-function' for Erlang."
  (save-excursion
    (erlang-beginning-of-function)
    (if (looking-at "[a-z0-9_]+")
	(match-string 0))))

;; Aliases for backward compatibility with older versions of Erlang Mode.
;;
;; Unfortuantely, older versions of Emacs doesn't have `defalias' and
;; `make-obsolete' so we have to define our own `obsolete' function.

(defun erlang-obsolete (sym newdef)
  "Make the obsolete function SYM refer to the defined function NEWDEF.

Simplified version of a combination `defalias' and `make-obsolete',
it assumes that NEWDEF is loaded."
  (defalias sym (symbol-function newdef))
  (if (fboundp 'make-obsolete)
      (make-obsolete sym newdef)))


(erlang-obsolete 'calculate-erlang-indent 'erlang-calculate-indent)
(erlang-obsolete 'calculate-erlang-stack-indent
		 'erlang-calculate-stack-indent)
(erlang-obsolete 'at-erlang-keyword 'erlang-at-keyword)
(erlang-obsolete 'at-erlang-operator 'erlang-at-operator)
(erlang-obsolete 'beginning-of-erlang-clause 'erlang-beginning-of-clause)
(erlang-obsolete 'end-of-erlang-clause 'erlang-end-of-clause)
(erlang-obsolete 'mark-erlang-clause 'erlang-mark-clause)
(erlang-obsolete 'beginning-of-erlang-function 'erlang-beginning-of-function)
(erlang-obsolete 'end-of-erlang-function 'erlang-end-of-function)
(erlang-obsolete 'mark-erlang-function 'erlang-mark-function)
(erlang-obsolete 'pass-over-erlang-clause 'erlang-pass-over-function)
(erlang-obsolete 'name-of-erlang-function 'erlang-name-of-function)


;; Fixme: shouldn't redefine `set-visited-file-name' anyhow -- see above.
(defconst erlang-unload-hook
  (list (lambda ()
	  (defalias 'set-visited-file-name
	    'erlang-orig-set-visited-file-name)
	  (when (featurep 'advice)
	    (ad-unadvise 'Man-notify-when-ready)
	    (ad-unadvise 'set-visited-file-name)))))


(defun erlang-string-to-int (string) 
  (if (fboundp 'string-to-number)
      (string-to-number string)
    (funcall (symbol-function 'string-to-int) string)))

;; The end...

(provide 'erlang)

(run-hooks 'erlang-load-hook)

;; Local variables:
;; coding: iso-8859-1
;; End:

;;; erlang.el ends here
