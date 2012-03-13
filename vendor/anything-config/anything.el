;;; anything.el --- open anything / QuickSilver-like candidate-selection framework

;; Copyright (C) 2007              Tamas Patrovics
;;               2008 ~ 2012       rubikitch <rubikitch@ruby-lang.org>
;;               2011 ~ 2012       Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; Author: Tamas Patrovics

;; Maintainers: rubikitch <rubikitch@ruby-lang.org>
;;              Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; Keywords: files, frames, help, matching, outlines,
;;           processes, tools, convenience, anything

;; X-URL: <http://repo.or.cz/w/anything-config.git>

;; Site: <http://www.emacswiki.org/cgi-bin/emacs/Anything>

;; MailingList: <https://groups.google.com/group/emacs-anything?hl=en>


;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the

;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Autodoc documentation:
;;  ---------------------

;;  * Commands defined here are:
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "anything" :docstring t)
;; `anything-open-last-log'
;; Open anything log file of last anything session.
;; `anything'
;; Main function to execute anything sources.
;; `anything-resume'
;; Resurrect previously invoked `anything'.
;; `anything-resume-window-only'
;; 
;; `anything-at-point'
;; Call anything with symbol at point as initial input.
;; `anything-force-update'
;; Force recalculation and update of candidates.
;; `anything-select-action'
;; Select an action for the currently selected candidate.
;; `anything-previous-line'
;; Move selection to the previous line.
;; `anything-next-line'
;; Move selection to the next line.
;; `anything-previous-page'
;; Move selection back with a pageful.
;; `anything-next-page'
;; Move selection forward with a pageful.
;; `anything-beginning-of-buffer'
;; Move selection at the top.
;; `anything-end-of-buffer'
;; Move selection at the bottom.
;; `anything-previous-source'
;; Move selection to the previous source.
;; `anything-next-source'
;; Move selection to the next source.
;; `anything-select-with-prefix-shortcut'
;; Invoke default action with prefix shortcut.
;; `anything-select-with-digit-shortcut'
;; Invoke default action with digit/alphabet shortcut.
;; `anything-confirm-and-exit-minibuffer'
;; Maybe ask for confirmation when exiting anything.
;; `anything-exit-minibuffer'
;; Select the current candidate by exiting the minibuffer.
;; `anything-keyboard-quit'
;; Quit minibuffer in anything.
;; `anything-help'
;; Help of `anything'.
;; `anything-debug-output'
;; Show all anything-related variables at this time.
;; `anything-delete-current-selection'
;; Delete the currently selected item.
;; `anything-delete-minibuffer-contents'
;; Same as `delete-minibuffer-contents' but this is a command.
;; `anything-toggle-resplit-window'
;; Toggle resplit anything window, vertically or horizontally.
;; `anything-narrow-window'
;; Narrow anything window.
;; `anything-enlarge-window'
;; Enlarge anything window.
;; `anything-select-2nd-action'
;; Select the 2nd action for the currently selected candidate.
;; `anything-select-3rd-action'
;; Select the 3rd action for the currently selected candidate.
;; `anything-select-4th-action'
;; Select the 4th action for the currently selected candidate.
;; `anything-select-2nd-action-or-end-of-line'
;; Select the 2nd action for the currently selected candidate.
;; `anything-execute-persistent-action'
;; Perform the associated action ATTR without quitting anything.
;; `anything-scroll-other-window'
;; Scroll other window (not *Anything* window) upward.
;; `anything-scroll-other-window-down'
;; Scroll other window (not *Anything* window) downward.
;; `anything-toggle-visible-mark'
;; Toggle anything visible mark at point.
;; `anything-display-all-visible-marks'
;; Show all `anything' visible marks strings.
;; `anything-next-visible-mark'
;; Move next anything visible mark.
;; `anything-prev-visible-mark'
;; Move previous anything visible mark.
;; `anything-yank-selection'
;; Set minibuffer contents to current selection.
;; `anything-kill-selection-and-quit'
;; Store current selection to kill ring.
;; `anything-follow-mode'
;; If this mode is on, persistent action is executed everytime the cursor is moved.
;; `anything-migrate-sources'
;; Help to migrate to new `anything' way.
;; `anything-describe-anything-attribute'
;; Display the full documentation of ANYTHING-ATTRIBUTE.
;; `anything-send-bug-report'
;; Send a bug report of anything.el.
;; `anything-send-bug-report-from-anything'
;; Send a bug report of anything.el in anything session.

;;  * Variables defined here are:
;; [EVAL] (autodoc-document-lisp-buffer :type 'internal-variable :prefix "anything-" :docstring t)
;; `anything-version'
;; Not documented.
;; `anything-sources'
;; A list of sources to use with `anything'.
;; `anything-type-attributes'
;; It's a list of                                      (TYPE ATTRIBUTES ...).
;; `anything-enable-shortcuts'
;; *Whether to use digit/alphabet shortcut to select the first nine matches.
;; `anything-shortcut-keys-alist'
;; Not documented.
;; `anything-display-source-at-screen-top'
;; *Display candidates at the top of screen.
;; `anything-candidate-number-limit'
;; Apply candidate-number-limit attribute value.
;; `anything-idle-delay'
;; *Be idle for this many seconds, before updating in delayed sources.
;; `anything-input-idle-delay'
;; Be idle for this many seconds, before updating.
;; `anything-samewindow'
;; Use current window to show the candidates.
;; `anything-source-filter'
;; A list of source names to be displayed.
;; `anything-map'
;; Keymap for anything.
;; `anything-header-face'
;; *Face for header lines in the anything buffer.
;; `anything-selection-face'
;; *Face for currently selected item.
;; `anything-buffer'
;; Buffer showing completions.
;; `anything-action-buffer'
;; Buffer showing actions.
;; `anything-selection-overlay'
;; Overlay used to highlight the currently selected item.
;; `anything-digit-overlays'
;; Overlays for digit shortcuts.  See `anything-enable-shortcuts'.
;; `anything-candidate-cache'
;; Holds the available candidate withing a single anything invocation.
;; `anything-pattern'
;; Not documented.
;; `anything-input'
;; Not documented.
;; `anything-async-processes'
;; List of information about asynchronous processes managed by anything.
;; `anything-digit-shortcut-count'
;; Number of digit shortcuts shown in the anything buffer.
;; `anything-before-initialize-hook'
;; Run before anything initialization.
;; `anything-after-initialize-hook'
;; Run after anything initialization.
;; `anything-update-hook'
;; Run after the anything buffer was updated according the new input pattern.
;; `anything-after-update-hook'
;; Run after the anything buffer was updated according the new input pattern.
;; `anything-cleanup-hook'
;; Run after anything minibuffer is closed.
;; `anything-select-action-hook'
;; Run when opening the action buffer.
;; `anything-before-action-hook'
;; Run before executing action.
;; `anything-after-action-hook'
;; Run after executing action.
;; `anything-after-persistent-action-hook'
;; Run after executing persistent action.
;; `anything-move-selection-before-hook'
;; Run before moving selection in `anything-buffer'.
;; `anything-move-selection-after-hook'
;; Run after moving selection in `anything-buffer'.
;; `anything-restored-variables'
;; Variables which are restored after `anything' invocation.
;; `anything-saved-selection'
;; Value of the currently selected object when the action list is shown.
;; `anything-current-prefix-arg'
;; Record `current-prefix-arg' when exiting minibuffer.
;; `anything-candidate-separator'
;; Candidates separator of `multiline' source.
;; `anything-current-buffer'
;; Current buffer when `anything' is invoked.
;; `anything-buffer-file-name'
;; Variable `buffer-file-name' when `anything' is invoked.
;; `anything-saved-action'
;; Saved value of the currently selected action by key.
;; `anything-last-sources'
;; OBSOLETE!! Sources of previously invoked `anything'.
;; `anything-saved-current-source'
;; Value of the current source when the action list is shown.
;; `anything-compiled-sources'
;; Compiled version of `anything-sources'.
;; `anything-in-persistent-action'
;; Flag whether in persistent-action or not.
;; `anything-quick-update'
;; If non-nil, suppress displaying sources which are out of screen at first.
;; `anything-last-sources-local'
;; Buffer local value of `anything-sources'.
;; `anything-last-buffer'
;; `anything-buffer' of previously `anything' session.
;; `anything-save-configuration-functions'
;; The functions used to restore/save window or frame configurations.
;; `anything-persistent-action-use-special-display'
;; If non-nil, use `special-display-function' in persistent action.
;; `anything-execute-action-at-once-if-one'
;; Execute default action and exit when only one candidate is remaining.
;; `anything-quit-if-no-candidate'
;; Quit when there is no candidates when non--nil.
;; `anything-scroll-amount'
;; Scroll amount when scrolling other window in an anything session.
;; `anything-display-function'
;; Function to display *anything* buffer.
;; `anything-delayed-init-executed'
;; Not documented.
;; `anything-mode-line-string'
;; Help string displayed in mode-line in `anything'.
;; `anything-help-message'
;; Detailed help message string for `anything'.
;; `anything-source-in-each-line-flag'
;; Non-nil means add anything-source text-property in each candidate.
;; `anything-debug-forms'
;; Forms to show in `anything-debug-output'.
;; `anything-debug'
;; If non-nil, write log message into *Anything Log* buffer.
;; `anything-test-candidate-list'
;; Not documented.
;; `anything-test-mode'
;; Not documented.
;; `anything-source-name'
;; Not documented.
;; `anything-candidate-buffer-alist'
;; Not documented.
;; `anything-check-minibuffer-input-timer'
;; Not documented.
;; `anything-match-hash'
;; Not documented.
;; `anything-cib-hash'
;; Not documented.
;; `anything-tick-hash'
;; Not documented.
;; `anything-issued-errors'
;; Not documented.
;; `anything-shortcut-keys'
;; Not documented.
;; `anything-once-called-functions'
;; Not documented.
;; `anything-follow-mode'
;; If this mode is on, persistent action is executed everytime the cursor is moved.
;; `anything-let-variables'
;; Not documented.
;; `anything-split-window-state'
;; Not documented.
;; `anything-selection-point'
;; Not documented.
;; `anything-last-log-file'
;; Not documented.
;; `anything-compile-source-functions'
;; Functions to compile elements of `anything-sources' (plug-in).
;; `anything-quit'
;; Not documented.
;; `anything-additional-attributes'
;; List of all `anything' attributes.
;; `anything-buffers'
;; All of `anything-buffer' in most recently used order.
;; `anything-current-position'
;; Restore or save current position in `anything-current-buffer'.
;; `anything-last-frame-or-window-configuration'
;; Used to store window or frame configuration when anything start.
;; `anything-reading-pattern'
;; Whether in `read-string' in anything or not.
;; `anything-compile-source-functions-default'
;; Plug-ins this file provides.
;; `anything-input-local'
;; Not documented.
;; `anything-process-delayed-sources-timer'
;; Not documented.
;; `anything-mode-line-string-real'
;; Not documented.
;; `anything-exit-status'
;; Flag to inform whether anything have exited or quitted.
;; `anything-minibuffer-confirm-state'
;; Not documented.
;; `anything-types'
;; Not documented.
;; `anything-orig-enable-shortcuts'
;; Not documented.
;; `anything-persistent-action-display-window'
;; Return the window that will be used for presistent action.
;; `anything-visible-mark-face'
;; Not documented.
;; `anything-visible-mark-overlays'
;; Not documented.
;; `anything-marked-candidates'
;; Return marked candidates of current source if any.
;; `anything-maintainer-mail-address'
;; Not documented.
;; `anything-bug-report-salutation'
;; Not documented.
;; `anything-no-dump-variables'
;; Variables not to dump in bug report.

;;  *** END auto-documentation

;; [EVAL] (autodoc-update-all)


;;; Commentary:

;;
;; Start with M-x anything, narrow the list by typing some pattern,
;; select with up/down/pgup/pgdown/C-p/C-n/C-v/M-v, choose with enter,
;; left/right moves between sources.  With TAB actions can be selected
;; if the selected candidate has more than one possible action.
;;
;; Note that anything.el provides only the framework and some example
;; configurations for demonstration purposes.  See anything-config.el
;; for practical, polished, easy to use configurations which can be
;; used to assemble a custom personalized configuration.
;;
;; NOTE: What you find on Emacswiki is mostly deprecated and not maintained,
;;       don't complain if you use such code or configuration and something
;;       doesn't work.
;;
;; Here is Japanese translation of `anything-sources' attributes.  Thanks.
;; http://d.hatena.ne.jp/sirocco634/20091012/1255336649


;;; Bug Report:
;;
;; If you have problems, send a bug report via C-c C-x C-b in anything session (best)
;; or M-x anything-send-bug-report outside anything session.
;; I implemented bug report feature because I want to know your current state.
;; It helps me to solve problems easily.
;; The step is:
;;  0) Setup mail in Emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "Your Full Name")
;;       (setq smtpmail-smtp-server "your.smtp.server.jp")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) Be sure to use the LATEST version of anything.el.
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) Use Lisp version instead of compiled one: (load "anything.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) Type C-c C-x C-b (anything session, best!)
;;     or M-x anything-send-bug-report (outside)
;;     then M-x insert-buffer *Backtrace* (if you got error)
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;
;;  You can also just report bug to:
;;  https://groups.google.com/group/emacs-anything?hl=en


;; You can extend `anything' by writing plug-ins. As soon as
;; `anything' is invoked, `anything-sources' is compiled into basic
;; attributes, then compiled one is used during invocation.
;;
;; The oldest built-in plug-in is `type' attribute: appends
;; appropriate element of `anything-type-attributes'. Second built-in
;; plug-in is `candidates-in-buffer': selecting a line from candidates
;; buffer.
;;
;; To write a plug-in:
;; 1. Define a compiler: anything-compile-source--*
;; 2. Add compier function to `anything-compile-source-functions'.
;; 3. (optional) Write helper functions.
;
;; Tested on Emacs 22/23/24.
;;
;;
;; Thanks to Vagn Johansen for ideas.
;; Thanks to Stefan Kamphausen for fixes and XEmacs support.
;; Thanks to Tassilo Horn for fixes.
;; Thanks to Drew Adams for various fixes
;; Thanks to IMAKADO for candidates-in-buffer idea.
;; Thanks to Tomohiro MATSUYAMA for multiline patch.
;;

;;; (@* "Index")

;;  If you have library `linkd.el', load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections  Linkd mode will
;;  highlight this Index.  You can get `linkd.el' here:
;;  http://www.emacswiki.org/cgi-bin/wiki/download/linkd.el
;;


;;; (@* "Tips")

;;
;; `anything' accepts keyword arguments. See docstring.
;; [EVAL IT] (describe-function 'anything)

;;
;; `anything-enable-shortcuts' enables us to select candidate easily.
;; If 'prefix then they can be selected using <prefix-key> <alnum>.
;; The prefix key is `anything-select-with-prefix-shortcut'.
;; If the <prefix-key> is a letter, pressing twice inputs the letter itself.
;; e.g.
;;  (setq anything-enable-shortcuts 'prefix)
;;  (define-key anything-map \"@\" 'anything-select-with-prefix-shortcut)

;;
;; You can edit current selection using `anything-edit-current-selection'.
;; It is useful after persistent-action.

;;
;; For `anything' users, setting `anything-sources' directly and
;; invoke M-x anything is obsolete way for now. Try M-x
;; `anything-migrate-sources'!

;;
;; If you want to create anything sources, yasnippet would help you.
;; http://yasnippet.googlecode.com/
;;
;; Then get the snippet from
;; http://www.emacswiki.org/cgi-bin/wiki/download/anything-source.yasnippet
;;
;; Put it in ~/.emacs.d/plugins/yasnippet/snippets/text-mode/emacs-lisp-mode/


;;
;; `anything-interpret-value' is useful function to interpret value
;; like `candidates' attribute.
;;
;; (anything-interpret-value "literal")            ; => "literal"
;; (anything-interpret-value (lambda () "lambda")) ; => "lambda"
;; (let ((source '((name . "lambda with source name"))))
;;   (anything-interpret-value
;;    (lambda () anything-source-name)
;;    source))                             ; => "lambda with source name"
;; (flet ((f () "function symbol"))
;;   (anything-interpret-value 'f))        ; => "function symbol"
;; (let ((v "variable symbol"))
;;   (anything-interpret-value 'v))        ; => "variable symbol"
;; (anything-interpret-value 'unbounded-1) ; error

;;
;; Now symbols are acceptable as candidates. So you do not have to use
;; `symbol-name' function. The source is much simpler. For example,
;; `apropos-internal' returns a list of symbols.
;;
;;   (anything
;;    '(((name . "Commands")
;;       (candidates . (lambda () (apropos-internal anything-pattern 'commandp)))
;;       (volatile)
;;       (action . describe-function))))

;;
;; To mark a candidate, press C-SPC as normal Emacs marking. To go to
;; marked candidate, press M-[ or M-].

;;
;; `anything-map' is now Emacs-standard key bindings by default.
;;
;; There are many `anything' applications, using `anything' for
;; selecting candidate. In this case, if there is one candidate or no
;; candidate, popping up *anything* buffer is irritating. If one
;; candidate, you want to select it at once. If no candidate, you want
;; to quit `anything'. Set `anything-execute-action-at-once-if-one'
;; and `anything-quit-if-no-candidate' to non-nil to remedy it. Note
;; that setting these variables GLOBALLY is bad idea because of
;; delayed sources. These are meant to be let-binded.
;;
;; ex.
;; (let ((anything-execute-action-at-once-if-one t)
;;       (anything-quit-if-no-candidate (lambda () (message "No candidate"))))
;;    (anything temporary-sources input))

;;
;; `set-frame-configuration' arises flickering. If you hate
;; flickering, eval:
;; (setq anything-save-configuration-functions
;;    '(set-window-configuration . current-window-configuration))
;; at the cost of restoring frame configuration (only window configuration).

;;
;; `anything-delete-current-selection' deletes the current line.
;; It is useful when deleting a candidate in persistent action.
;; eg. `kill-buffer'.
;;
;; [EVAL IT] (describe-function 'anything-delete-current-selection)

;;
;; `anything-attr' gets the attribute. `anything-attrset' sets the
;; attribute. `anything-attr-defined' tests whether the attribute is
;; defined. They handles source-local variables.
;;
;; [EVAL IT] (describe-function 'anything-attr)
;; [EVAL IT] (describe-function 'anything-attrset)
;; [EVAL IT] (describe-function 'anything-attr-defined)

;;
;; `anything-sources' accepts many attributes to make your life easier.
;; Now `anything-sources' accepts a list of symbols.
;;
;; [EVAL IT] (describe-variable 'anything-sources)

;;
;; `anything' has optional arguments. Now you do not have to let-bind
;; `anything-sources'.
;;
;; [EVAL IT] (describe-function 'anything)

;;
;; `anything-resume' resumes last `anything' session. Now you do not
;; have to retype pattern.
;;
;; [EVAL IT] (describe-function 'anything-resume)

;;
;; `anything-execute-persistent-action' executes action without
;; quitting `anything'. When popping up a buffer in other window by
;; persistent action, you can scroll with `anything-scroll-other-window' and
;; `anything-scroll-other-window-down'. See also `anything-sources' docstring.
;;
;; [EVAL IT] (describe-function 'anything-execute-persistent-action)
;; [EVAL IT] (describe-variable 'anything-sources)

;;
;; `anything-select-2nd-action', `anything-select-3rd-action' and
;; `anything-select-4th-action' select other than default action
;; without pressing Tab.

;;
;; Using `anything-candidate-buffer' and the candidates-in-buffer
;; attribute is much faster than traditional "candidates and match"
;; way. And `anything-current-buffer-is-modified' avoids to
;; recalculate candidates for unmodified buffer. See docstring of
;; them.
;;
;; [EVAL IT] (describe-function 'anything-candidate-buffer)
;; [EVAL IT] (describe-function 'anything-candidates-in-buffer)
;; [EVAL IT] (describe-function 'anything-current-buffer-is-modified)

;;
;; `anything-current-buffer' and `anything-buffer-file-name' stores
;; `(current-buffer)' and `buffer-file-name' in the buffer `anything'
;; is invoked. Use them freely.
;;
;; [EVAL IT] (describe-variable 'anything-current-buffer)
;; [EVAL IT] (describe-variable 'anything-buffer-file-name)

;;
;; `anything-completing-read' and `anything-read-file-name' are
;; experimental implementation. If you are curious, type M-x
;; anything-read-string-mode. It is a minor mode and toggles on/off.

;;
;; Use `anything-test-candidates' to test your handmade anything
;; sources. It simulates contents of *anything* buffer with pseudo
;; `anything-sources' and `anything-pattern', without side-effect. So
;; you can unit-test your anything sources! Let's TDD!
;;
;; [EVAL IT] (describe-function 'anything-test-candidates)
;;
;; For anything developpers:
;;
;; There are many unit-testing framework in Emacs Lisp. See the EmacsWiki.
;; http://www.emacswiki.org/cgi-bin/emacs/UnitTesting
;; There is an unit-test by Emacs Lisp Expectations in developper-tools directory.
;; http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el
;; http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el

;;
;; If you want to create anything sources, see anything-config.el.
;; It is huge collection of sources. You can learn from examples.


;; (@* "TODO")
;;
;;   - process status indication
;;
;;   - async sources doesn't honor digit-shortcut-count
;;
;;   - anything-candidate-number-limit can't be nil everywhere

;; (@* "HISTORY")
;;
;;  Change log of this file is found at
;;  http://repo.or.cz/w/anything-config.git/history/master:/anything.el
;;
;;  Change log of this project is found at
;;  http://repo.or.cz/w/anything-config.git?a=shortlog
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Code:

(require 'cl)

(defvar anything-version "1.3.9")

;; (@* "User Configuration")


;;; Variables
;;
;;
;; [DEPRECATED]
;; A default value is provided in anything-config.el
(defvar anything-sources nil
  "A list of sources to use with `anything'.
It is deprecated, you should not use this.
Use instead individual sources or list of sources of your choice.")

;; Default values are provided in anything-config.el.
(defvar anything-type-attributes nil
  "It's a list of \(TYPE ATTRIBUTES ...\).
ATTRIBUTES are the same as attributes for `anything-sources'.
TYPE connects the value to the appropriate sources.
Don't set this directly, use instead `define-anything-type-attribute'.

This allows specifying common attributes for several sources.
For example, sources which provide files can specify
common attributes with a `file' type.")

(defvaralias 'anything-enable-digit-shortcuts 'anything-enable-shortcuts
  "Same as `anything-enable-shortcuts'.
Alphabet shortcuts are available now in `anything-enable-shortcuts'.
`anything-enable-digit-shortcuts' is retained for compatibility.")

(defvar anything-enable-shortcuts nil
  "*Whether to use digit/alphabet shortcut to select the first nine matches.
If t then they can be selected using Ctrl+<number>.

If 'prefix then they can be selected using <prefix-key> <alnum>.
The prefix key is `anything-select-with-prefix-shortcut'.
If the <prefix-key> is a letter, pressing twice inputs the letter itself.
e.g.
 (setq anything-enable-shortcuts 'prefix)
 (define-key anything-map \"@\" 'anything-select-with-prefix-shortcut)

If 'alphabet then they can be selected using Shift+<alphabet> (deprecated).
It is not recommended because you cannot input capital letters in pattern.

Keys (digit/alphabet) are listed in `anything-shortcut-keys-alist'.")

(defvar anything-shortcut-keys-alist
  '((alphabet . "asdfghjklzxcvbnmqwertyuiop")
    (prefix   . "asdfghjklzxcvbnmqwertyuiop1234567890")
    (t        . "123456789")))

(defvar anything-display-source-at-screen-top t
  "*Display candidates at the top of screen.
This happen when using `anything-next-source' and `anything-previous-source'.")

(defvar anything-candidate-number-limit 50
  "*Limit candidate number globally.
Do not show more candidates than this limit from individual sources.
It is usually pointless to show hundreds of matches
when the pattern is empty, because it is much simpler to type a
few characters to narrow down the list of potential candidates.

Set it to nil if you don't want this limit.")

(defvar anything-idle-delay 0.3
  "*Be idle for this many seconds, before updating in delayed sources.
This is useful for sources involving heavy operations
\(like launching external programs\), so that candidates
from the source are not retrieved unnecessarily if the user keeps typing.

It also can be used to declutter the results anything displays,
so that results from certain sources are not shown with every
character typed, only if the user hesitates a bit.")

(defvar anything-input-idle-delay 0.3
  "Be idle for this many seconds, before updating.

Unlike `anything-idle-delay', it is also effective for non-delayed sources.
If nil, candidates are collected immediately.

Note:  If this value is too low compared to `anything-idle-delay',
you may have duplicated sources when using multiples sources.
Safe value is always >= `anything-idle-delay'.
Default settings are equal value for both.")

(defvar anything-samewindow nil
  "Use current window to show the candidates.
If t then Anything doesn't pop up a new window.")

(defvar anything-source-filter nil
  "A list of source names to be displayed.
Other sources won't appear in the search results.
If nil then there is no filtering.
See also `anything-set-source-filter'.")


(defvar anything-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "<down>")          'anything-next-line)
    (define-key map (kbd "<up>")            'anything-previous-line)
    (define-key map (kbd "C-n")             'anything-next-line)
    (define-key map (kbd "C-p")             'anything-previous-line)
    (define-key map (kbd "<prior>")         'anything-previous-page)
    (define-key map (kbd "<next>")          'anything-next-page)
    (define-key map (kbd "M-v")             'anything-previous-page)
    (define-key map (kbd "C-v")             'anything-next-page)
    (define-key map (kbd "M-<")             'anything-beginning-of-buffer)
    (define-key map (kbd "M->")             'anything-end-of-buffer)
    (define-key map (kbd "C-g")             'anything-keyboard-quit)
    (define-key map (kbd "<right>")         'anything-next-source)
    (define-key map (kbd "<left>")          'anything-previous-source)
    (define-key map (kbd "<RET>")           'anything-exit-minibuffer)
    (define-key map (kbd "C-1")             'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-2")             'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-3")             'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-4")             'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-5")             'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-6")             'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-7")             'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-8")             'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-9")             'anything-select-with-digit-shortcut)
    (loop for c from ?A to ?Z do
          (define-key map (make-string 1 c) 'anything-select-with-digit-shortcut))
    (define-key map (kbd "C-i")             'anything-select-action)
    (define-key map (kbd "C-z")             'anything-execute-persistent-action)
    (define-key map (kbd "C-e")             'anything-select-2nd-action-or-end-of-line)
    (define-key map (kbd "C-j")             'anything-select-3rd-action)
    (define-key map (kbd "C-o")             'anything-next-source)
    (define-key map (kbd "C-M-v")           'anything-scroll-other-window)
    (define-key map (kbd "M-<next>")        'anything-scroll-other-window)
    (define-key map (kbd "C-M-y")           'anything-scroll-other-window-down)
    (define-key map (kbd "C-M-S-v")         'anything-scroll-other-window-down)
    (define-key map (kbd "M-<prior>")       'anything-scroll-other-window-down)
    (define-key map (kbd "<C-M-down>")      'anything-scroll-other-window)
    (define-key map (kbd "<C-M-up>")        'anything-scroll-other-window-down)
    (define-key map (kbd "C-SPC")           'anything-toggle-visible-mark)
    (define-key map (kbd "M-SPC")           'anything-toggle-visible-mark)
    (define-key map (kbd "M-[")             'anything-prev-visible-mark)
    (define-key map (kbd "M-]")             'anything-next-visible-mark)
    (define-key map (kbd "C-k")             'anything-delete-minibuffer-contents)

    (define-key map (kbd "C-r")             'undefined)
    (define-key map (kbd "C-t")             'anything-toggle-resplit-window)
    (define-key map (kbd "C-}")             'anything-narrow-window)
    (define-key map (kbd "C-{")             'anything-enlarge-window)
    
    (define-key map (kbd "C-c C-d")         'anything-delete-current-selection)
    (define-key map (kbd "C-c C-y")         'anything-yank-selection)
    (define-key map (kbd "C-c C-k")         'anything-kill-selection-and-quit)
    (define-key map (kbd "C-c C-f")         'anything-follow-mode)
    (define-key map (kbd "C-c C-u")         'anything-force-update)
    (define-key map (kbd "M-p")             'previous-history-element)
    (define-key map (kbd "M-n")             'next-history-element)
    ;; Debugging command
    (define-key map "\C-c\C-x\C-d"          'anything-debug-output)
    (define-key map "\C-c\C-x\C-m"          'anything-display-all-visible-marks)
    (define-key map "\C-c\C-x\C-b"          'anything-send-bug-report-from-anything)
    ;; Use `describe-mode' key in `global-map'.
    (define-key map [f1] nil) ; Allow to eval keymap whithout errors.
    (dolist (k (where-is-internal 'describe-mode global-map))
      (define-key map k 'anything-help))
    map)
  "Keymap for anything.")


(defgroup anything nil
  "Open anything."
  :prefix "anything-" :group 'convenience)

(defface anything-header
    '((t (:inherit header-line)))
  "Face for header lines in the anything buffer."
  :group 'anything)

(defvar anything-header-face 'anything-header
  "*Face for header lines in the anything buffer.")

(defface anything-candidate-number
    '((t (:background "Yellow" :foreground "black")))
  "Face for candidate number in mode-line." :group 'anything)

(defvar anything-selection-face 'highlight
  "*Face for currently selected item.")

(defvar anything-buffer "*anything*"
  "Buffer showing completions.")

(defvar anything-action-buffer "*anything action*"
  "Buffer showing actions.")

(defvar anything-selection-overlay nil
  "Overlay used to highlight the currently selected item.")

(defvar anything-digit-overlays nil
  "Overlays for digit shortcuts.  See `anything-enable-shortcuts'.")

(defvar anything-candidate-cache nil
  "Holds the available candidate withing a single anything invocation.")

(defvar anything-pattern
  "The input pattern used to update the anything buffer.")

(defvar anything-input
  "The input typed in the candidates panel.")

(defvar anything-async-processes nil
  "List of information about asynchronous processes managed by anything.")

(defvar anything-digit-shortcut-count 0
  "Number of digit shortcuts shown in the anything buffer.")

(defvar anything-before-initialize-hook nil
  "Run before anything initialization.
This hook is run before init functions in `anything-sources'.")

(defvar anything-after-initialize-hook nil
  "Run after anything initialization.
Global variables are initialized and the anything buffer is created.
But the anything buffer has no contents.")

(defvar anything-update-hook nil
  "Run after the anything buffer was updated according the new input pattern.
This hook is run at the beginning of buffer.
The first candidate is selected after running this hook.
See also `anything-after-update-hook'.")

(defvar anything-after-update-hook nil
  "Run after the anything buffer was updated according the new input pattern.
This is very similar to `anything-update-hook' but selection is not moved.
It is useful to select a particular object instead of the first one.")

(defvar anything-cleanup-hook nil
  "Run after anything minibuffer is closed.
IOW this hook is executed BEFORE performing action.")

(defvar anything-select-action-hook nil
  "Run when opening the action buffer.")

(defvar anything-before-action-hook nil
  "Run before executing action.
Contrarily to `anything-cleanup-hook',
this hook run before anything minibuffer is closed
and before performing action.")

(defvar anything-after-action-hook nil
  "Run after executing action.")

(defvar anything-after-persistent-action-hook nil
  "Run after executing persistent action.")

(defvar anything-move-selection-before-hook nil
  "Run before moving selection in `anything-buffer'.")

(defvar anything-move-selection-after-hook nil
  "Run after moving selection in `anything-buffer'.")

(defvar anything-restored-variables
  '(anything-candidate-number-limit
    anything-source-filter
    anything-source-in-each-line-flag
    anything-map
    anything-sources)
  "Variables which are restored after `anything' invocation.")

(defvar anything-saved-selection nil
  "Value of the currently selected object when the action list is shown.")

(defvar anything-current-prefix-arg nil
  "Record `current-prefix-arg' when exiting minibuffer.")

(defvar anything-candidate-separator
  "--------------------"
  "Candidates separator of `multiline' source.")

(defvar anything-current-buffer nil
  "Current buffer when `anything' is invoked.")

(defvar anything-buffer-file-name nil
  "Variable `buffer-file-name' when `anything' is invoked.")

(defvar anything-saved-action nil
  "Saved value of the currently selected action by key.")

(defvar anything-last-sources nil
  "OBSOLETE!! Sources of previously invoked `anything'.")

(defvar anything-saved-current-source nil
  "Value of the current source when the action list is shown.")

(defvar anything-compiled-sources nil
  "Compiled version of `anything-sources'.")

(defvar anything-in-persistent-action nil
  "Flag whether in persistent-action or not.")

(defvar anything-quick-update nil
  "If non-nil, suppress displaying sources which are out of screen at first.
They are treated as delayed sources at this input.
This flag makes `anything' a bit faster with many sources.")

(defvar anything-last-sources-local nil
  "Buffer local value of `anything-sources'.")

(defvar anything-last-buffer nil
  "`anything-buffer' of previously `anything' session.")

(defvar anything-save-configuration-functions
  '(set-window-configuration . current-window-configuration)
  "The functions used to restore/save window or frame configurations.
It is a pair where the car is the function to restore window or frame config,
and the cdr is the function to save the window or frame config.

If you want to save and restore frame configuration, set this variable to
 '\(set-frame-configuration . current-frame-configuration\)

Older version saves/restores frame configuration, but the default is changed now
because flickering can occur in some environment. ")

(defvar anything-persistent-action-use-special-display nil
  "If non-nil, use `special-display-function' in persistent action.")

(defvar anything-execute-action-at-once-if-one nil
  "Execute default action and exit when only one candidate is remaining.
It is useful for `anything' applications.")

(defvar anything-quit-if-no-candidate nil
  "Quit when there is no candidates when non--nil.
This variable accepts a function, which is executed if no candidate.

It is useful for `anything' applications.")

(defvar anything-scroll-amount nil
  "Scroll amount when scrolling other window in an anything session.
It is used by `anything-scroll-other-window'
and `anything-scroll-other-window-down'.

If you prefer scrolling line by line, set this value to 1.")

(defvar anything-display-function 'anything-default-display-buffer
  "Function to display *anything* buffer.
It is `anything-default-display-buffer' by default,
which affects `anything-samewindow'.")

(defvar anything-delayed-init-executed nil)

(defvar anything-mode-line-string "\\<anything-map>\\[anything-help]:help \
\\[anything-select-action]:Acts \
\\[anything-exit-minibuffer]/\\[anything-select-2nd-action-or-end-of-line]/\
\\[anything-select-3rd-action]:NthAct \
\\[anything-send-bug-report-from-anything]:BugReport"
  "Help string displayed in mode-line in `anything'.
It can be a string or a list of two args, in this case,
first arg is a string that will be used as name for candidates number,
second arg any string to display in mode line.
If nil, use default `mode-line-format'.")

(defvar anything-help-message
  "\\<anything-map>The keys that are defined for `anything' are:
       \\{anything-map}"
  "Detailed help message string for `anything'.
It also accepts function or variable symbol.")

(defvar anything-source-in-each-line-flag nil
  "Non-nil means add anything-source text-property in each candidate.
experimental feature.")

(defvaralias 'anything-debug-variables 'anything-debug-forms)

(defvar anything-debug-forms nil
  "Forms to show in `anything-debug-output'.
Otherwise all variables started with `anything-' are shown.
It is useful for debug.")

(defvar anything-debug nil
  "If non-nil, write log message into *Anything Log* buffer.
If `debug-on-error' is non-nil, write log message regardless of this variable.
It is disabled by default because *Anything Log* grows quickly.")

(defcustom anything-local-map-override-anything-map t
  "Override `anything-map' keys with the corresponding ones in source local map.
When non--nil keys in source local map will override same keys in `anything-map'
otherwise same keys in `anything-map' will take precedence."
  :group 'anything
  :type  'boolean)


;; (@* "Internal Variables")
(defvar anything-test-candidate-list nil)
(defvar anything-test-mode nil)
(defvar anything-source-name nil)
(defvar anything-candidate-buffer-alist nil)
(defvar anything-check-minibuffer-input-timer nil)
(defvar anything-match-hash (make-hash-table :test 'equal))
(defvar anything-cib-hash (make-hash-table :test 'equal))
(defvar anything-tick-hash (make-hash-table :test 'equal))
(defvar anything-issued-errors nil)
(defvar anything-shortcut-keys nil)
(defvar anything-once-called-functions nil)
(defvar anything-follow-mode nil)
(defvar anything-let-variables nil)
(defvar anything-split-window-state nil)
(defvar anything-selection-point nil)
(defvar anything-alive-p nil)


;; (@* "Utility: logging")
(defun anything-log (format-string &rest args)
  "Log message if `debug-on-error' or `anything-debug' is non-nil.
Messages are written to the *Anything Log* buffer.

Argument FORMAT-STRING is a string to use with `format'.
Use optional arguments ARGS like in `format'."
  (when (or debug-on-error anything-debug)
    (with-current-buffer (get-buffer-create "*Anything Log*")
      (buffer-disable-undo)
      (set (make-local-variable 'inhibit-read-only) t)
      (goto-char (point-max))
      (insert (let ((tm (current-time)))
                (format "%s.%06d (%s) %s\n"
                        (format-time-string "%H:%M:%S" tm)
                        (nth 2 tm)
                        (anything-log-get-current-function)
                        (apply #'format (cons format-string args))))))))

(defmacro anything-log-eval (&rest exprs)
  "Write each EXPRS evaluation result to the *Anything Log* buffer."
  `(anything-log-eval-internal ',exprs))

(defun anything-log-run-hook (hook)
  "Run HOOK like `run-hooks' but write these actions to anything log buffer."
  (anything-log "executing %s" hook)
  (when (boundp hook)
    (anything-log-eval (symbol-value hook))
    (anything-log-eval (default-value hook)))
  (run-hooks hook)
  (anything-log "executed %s" hook))

(defun anything-log-eval-internal (exprs)
  "Eval EXPRS and write results to anything log buffer."
  (dolist (expr exprs)
    (condition-case err
        (anything-log "%S = %S" expr (eval expr))
      (error (anything-log "%S = ERROR!" expr)))))

(defun anything-log-get-current-function ()
  "Get function name calling `anything-log'.
The original idea is from `tramp-debug-message'."
  (loop with exclude-func-re = "^anything-\\(?:interpret\\|log\\|.*funcall\\)"
        for btn from 1 to 40            ;avoid inf-loop
        for btf = (second (backtrace-frame btn))
        for fn  = (if (symbolp btf) (symbol-name btf) "")
        if (and (string-match "^anything" fn)
                (not (string-match exclude-func-re fn)))
        return fn))

(defun anything-log-error (&rest args)
  "Accumulate error messages into `anything-issued-errors'.
ARGS are args given to `format'."
  (apply 'anything-log (concat "ERROR: " (car args)) (cdr args))
  (let ((msg (apply 'format args)))
    (unless (member msg anything-issued-errors)
      (add-to-list 'anything-issued-errors msg))))

(defvar anything-last-log-file nil)
(defun anything-log-save-maybe ()
  "May be save log buffer to `anything-last-log-file'."
  (when (stringp anything-debug)
    (let ((logdir (expand-file-name (format-time-string "%Y%m%d")
                                    anything-debug)))
      (make-directory logdir t)
      (with-current-buffer (get-buffer-create "*Anything Log*")
        (write-region (point-min) (point-max)
                      (setq anything-last-log-file
                            (expand-file-name (format-time-string "%Y%m%d-%H%M%S")
                                              logdir))
                      nil 'silent)
        (erase-buffer)))))

(defun anything-open-last-log ()
  "Open anything log file of last anything session."
  (interactive)
  (if anything-last-log-file
      (view-file anything-last-log-file)
      (switch-to-buffer "*Anything Log*")))

(defun anything-print-error-messages ()
  "Print error messages in `anything-issued-errors'."
  (message "%s" (mapconcat 'identity (reverse anything-issued-errors) "\n")))

;; (anything-log "test")
;; (switch-to-buffer-other-window "*Anything Log*")


;; (@* "Programming Tools")
(defmacro anything-aif (test-form then-form &rest else-forms)
  "Like `if' but set the result of TEST-FORM in a temprary variable called `it'.
THEN-FORM and ELSE-FORMS are then excuted just like in `if'."
  (declare (indent 2) (debug t))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defun anything-mklist (obj)
  "If OBJ is a list \(but not lambda\), return itself.
Otherwise make a list with one element."
  (if (and (listp obj) (not (functionp obj)))
      obj
      (list obj)))


;; (@* "Anything API")

(defun anything-buffer-get ()
  "Return `anything-action-buffer' if shown otherwise `anything-buffer'."
  (if (anything-action-window)
      anything-action-buffer
      anything-buffer))

(defun anything-window ()
  "Window of `anything-buffer'."
  (get-buffer-window (anything-buffer-get) 'visible))

(defun anything-action-window ()
  "Window of `anything-action-buffer'."
  (get-buffer-window anything-action-buffer 'visible))

(defmacro with-anything-window (&rest body)
  "Be sure BODY is excuted in the anything window."
  (declare (indent 0) (debug t))
  `(if anything-test-mode
       (with-current-buffer (anything-buffer-get)
         ,@body)
       (with-selected-window (anything-window)
         ,@body)))

(defmacro with-anything-current-buffer (&rest body)
  "Eval BODY inside `anything-current-buffer'."
  (declare (indent 0) (debug t))
  `(with-current-buffer anything-current-buffer
     ,@body))

(defmacro with-anything-restore-variables(&rest body)
  "Restore `anything-restored-variables' after executing BODY.
`post-command-hook' is handled specially."
  (declare (indent 0) (debug t))
  `(let ((--orig-vars (mapcar (lambda (v)
                                (cons v (symbol-value v)))
                              anything-restored-variables))
         (--post-command-hook-pair (cons post-command-hook
                                         (default-value 'post-command-hook))))
     (setq post-command-hook '(t))
     (setq-default post-command-hook nil)
     (unwind-protect (progn ,@body)
       (loop for (var . value) in --orig-vars
             do (set var value))
       (setq post-command-hook (car --post-command-hook-pair))
       (setq-default post-command-hook (cdr --post-command-hook-pair))
       (anything-log "restore variables"))))

(defun* anything-attr (attribute-name &optional
                                      (src (anything-get-current-source)))
  "Get the value of ATTRIBUTE-NAME of SRC.
if SRC is omitted, use current source.
It is useful to write your sources."
  (anything-aif (assq attribute-name src)
      (cdr it)))

(defun* anything-attr* (attribute-name
                        &optional (src (anything-get-current-source)))
  "Pass the value of ATTRIBUTE-NAME of SRC to `anything-interpret-value'.
if SRC is omitted, use current source.
It is useful to write your sources."
  (anything-interpret-value (anything-attr attribute-name src)))

(defun* anything-attr-defined (attribute-name
                               &optional (src (anything-get-current-source)))
  "Return non-nil if ATTRIBUTE-NAME of SRC is defined.
if SRC is omitted, use current source.
It is useful to write your sources."
  (and (assq attribute-name src) t))

(defun* anything-attrset (attribute-name value
                                         &optional
                                         (src (anything-get-current-source)))
  "Set the value of ATTRIBUTE-NAME of SRC to VALUE.
if SRC is omitted, use current source.
It is useful to write your sources."
  (anything-aif (assq attribute-name src)
      (setcdr it value)
    (setcdr src (cons (cons attribute-name value) (cdr src))))
  value)

(defun anything-set-source-filter (sources)
  "Set the value of `anything-source-filter' to SOURCES and update.

This function sets a filter for anything sources and it may be
called while anything is running. It can be used to toggle
displaying of sources dinamically. For example, additional keys
can be bound into `anything-map' to display only the file-related
results if there are too many matches from other sources and
you're after files only:

Shift+F shows only file results from some sources:

\(define-key anything-map \"F\" 'anything-my-show-files-only)

\(defun anything-my-show-files-only ()
  (interactive)
  (anything-set-source-filter '(\"File Name History\"
                                  \"Files from Current Directory\")))

Shift+A shows all results:

\(define-key anything-map \"A\" 'anything-my-show-all)

\(defun anything-my-show-all ()
  (interactive)
  (anything-set-source-filter nil))

Note that you have to prefix the functions with anything- prefix,
otherwise they won't be bound when Anything is used under
Iswitchb. The -my- part is added to avoid collisions with
existing Anything function names."
  (unless (and (listp sources)
               (loop for name in sources always (stringp name)))
    (error "Invalid data in `anything-set-source-filter': %S" sources))
  (setq anything-source-filter sources)
  (anything-log-eval anything-source-filter)
  (anything-update))

(defun anything-set-sources (sources &optional no-init no-update)
  "Set SOURCES during `anything' invocation.
If NO-INIT is non-nil, skip executing init functions of SOURCES.
If NO-UPDATE is non-nil, skip executing `anything-update'."
  (with-current-buffer anything-buffer
    (setq anything-compiled-sources nil
          anything-sources sources
          anything-last-sources-local sources)
    (anything-log-eval anything-compiled-sources anything-sources))
  (unless no-init (anything-funcall-foreach 'init))
  (unless no-update (anything-update)))

(defvar anything-compile-source-functions
  '(anything-compile-source--type
    anything-compile-source--dummy
    anything-compile-source--disable-shortcuts
    anything-compile-source--candidates-in-buffer)
  "Functions to compile elements of `anything-sources' (plug-in).")

(defun anything-get-sources ()
  "Return compiled `anything-sources', which is memoized.

Attributes:

- type
  `anything-type-attributes' are merged in.
- candidates-buffer
  candidates, volatile and match attribute are created."
  (cond
    ;; action
    ((anything-action-window)
     anything-sources)
    ;; memoized
    (anything-compiled-sources)
    ;; first time
    (t
     (prog1
         (setq anything-compiled-sources
               (anything-compile-sources
                anything-sources anything-compile-source-functions))
       (anything-log-eval anything-compiled-sources)))))

(defun* anything-get-selection (&optional (buffer nil buffer-s) (force-display-part))
  "Return the currently selected item or nil.
if BUFFER is nil or unspecified, use anything-buffer as default value.
If FORCE-DISPLAY-PART is non-nil, return the display string."
  (setq buffer (if (and buffer buffer-s) buffer anything-buffer))
  (unless (anything-empty-buffer-p buffer)
    (with-current-buffer buffer
      (let ((selection
             (or (and (not force-display-part)
                      (get-text-property (overlay-start
                                          anything-selection-overlay)
                                         'anything-realvalue))
                 (let ((disp (buffer-substring-no-properties
                              (overlay-start anything-selection-overlay)
                              (1- (overlay-end anything-selection-overlay))))
                       (source (anything-get-current-source)))
                   (anything-aif (and (not force-display-part)
                                      (assoc-default 'display-to-real source))
                       (anything-funcall-with-source source it disp)
                     disp)))))
        (unless (equal selection "")
          (anything-log-eval selection)
          selection)))))

(defun anything-get-action ()
  "Return the associated action for the selected candidate.
It is a function symbol \(sole action\) or list
of \(action-display . function\)."
  (unless (anything-empty-buffer-p (anything-buffer-get))
    (anything-aif (anything-attr 'action-transformer)
        (anything-composed-funcall-with-source
         (anything-get-current-source) it
         (anything-attr 'action) (anything-get-selection))
      (anything-attr 'action))))

(defun anything-get-current-source ()
  "Return the source for the current selection.
Use it in init, candidates, action, candidate-transformer,
filtered-candidate-transformer functions."
  (declare (special source))
  ;; The name `anything-get-current-source' should be used in init function etc.
  (if (and (boundp 'anything-source-name) (stringp anything-source-name))
      source
      (with-current-buffer (anything-buffer-get)
        (or
         ;; This happen only when `anything-source-in-each-line-flag'
         ;; is non--nil and there is candidates in buffer.
         (get-text-property (point) 'anything-source)
         ;; Return nil when no--candidates.
         (block exit
           ;; This goto-char shouldn't be necessary, but point is moved to
           ;; point-min somewhere else which shouldn't happen.
           (goto-char (overlay-start anything-selection-overlay))
           (let* ((header-pos (or (anything-get-previous-header-pos)
                                  (anything-get-next-header-pos)))
                  (source-name
                   (save-excursion
                     (unless header-pos
                       (return-from exit nil))
                     (goto-char header-pos)
                     (anything-current-line-contents))))
             (loop for source in (anything-get-sources) thereis
                   (and (equal (assoc-default 'name source) source-name)
                        source))))))))

(defun anything-buffer-is-modified (buffer)
  "Return non-nil when BUFFER is modified since `anything' was invoked."
  (let* ((b (get-buffer buffer))
         (key (concat (buffer-name b) "/" (anything-attr 'name)))
         (source-tick (or (gethash key anything-tick-hash) 0))
         (buffer-tick (buffer-chars-modified-tick b))
         (modifiedp (/= source-tick buffer-tick)))
    (puthash key buffer-tick anything-tick-hash)
    (anything-log-eval buffer modifiedp)
    modifiedp))

(defun anything-current-buffer-is-modified ()
  "Check if `anything-current-buffer' is modified since `anything' was invoked."
  (anything-buffer-is-modified anything-current-buffer))

(defvar anything-quit nil)
(defun anything-run-after-quit (function &rest args)
  "Perform an action after quitting `anything'.
The action is to call FUNCTION with arguments ARGS."
  (setq anything-quit t)
  (anything-kill-async-processes)
  (anything-log-eval function args)
  (apply 'run-with-idle-timer 0 nil function args)
  (anything-exit-minibuffer))


(defun define-anything-type-attribute (type definition &optional doc)
  "Register type attribute of TYPE as DEFINITION with DOC.
DOC is displayed in `anything-type-attributes' docstring.

Use this function is better than setting `anything-type-attributes' directly."
  (loop for i in definition do
        ;; without `ignore-errors', error at emacs22
        (ignore-errors (setf i (delete nil i))))
  (anything-add-type-attribute type definition)
  (and doc (anything-document-type-attribute type doc))
  nil)

(defvaralias 'anything-attributes 'anything-additional-attributes)
(defvar anything-additional-attributes nil
  "List of all `anything' attributes.")

(defun anything-document-attribute (attribute short-doc &optional long-doc)
  "Register ATTRIBUTE documentation introduced by plug-in.
SHORT-DOC is displayed beside attribute name.
LONG-DOC is displayed below attribute name and short documentation."
  (if long-doc
      (setq short-doc (concat "(" short-doc ")"))
      (setq long-doc short-doc
            short-doc ""))
  (add-to-list 'anything-additional-attributes attribute t)
  (put attribute 'anything-attrdoc
       (concat "- " (symbol-name attribute)
               " " short-doc "\n\n" long-doc "\n")))

(put 'anything-document-attribute 'lisp-indent-function 2)

(defun anything-interpret-value (value &optional source)
  "Interpret VALUE as variable, function or literal.
If VALUE is a function, call it with no arguments and return the value.
If SOURCE is `anything' source, `anything-source-name' is source name.

If VALUE is a variable, return the value.

If VALUE is a symbol, but it is not a function or a variable, cause an error.

Otherwise, return VALUE itself."
  (cond ((and source (functionp value))
         (anything-funcall-with-source source value))
        ((functionp value)
         (funcall value))
        ((and (symbolp value) (boundp value))
         (symbol-value value))
        ((symbolp value)
         (error "anything-interpret-value: Symbol must be a function or a variable"))
        (t
         value)))

(defun anything-once (function &rest args)
  "Ensure FUNCTION with ARGS to be called once in `anything' session."
  (let ((spec (cons function args)))
    (unless (member spec anything-once-called-functions)
      (apply function args)
      (push spec anything-once-called-functions))))


;; (@* "Core: API helper")
(defun* anything-empty-buffer-p (&optional (buffer anything-buffer))
  "Check if BUFFER have candidates.
Default value for BUFFER is `anything-buffer'."
  (zerop (buffer-size (and buffer (get-buffer buffer)))))

(defun anything-let-eval-varlist (varlist)
  "Return the list of pairs VARLIST with each cdr of pair evaluated.
If VARLIST contain single elements, those are returned
as a list of one element."
  (mapcar (lambda (pair)
            (if (listp pair)
                (cons (car pair) (eval (cadr pair)))
                (cons pair nil)))
          varlist))

;; [NOT USED]
;; (defun anything-let*-eval-varlist (varlist)
;;   (let ((vars (mapcar (lambda (pair)
;;                         (or (car-safe pair) pair))
;;                       varlist)))
;;     (eval `(let ,vars
;;              ,@(mapcar (lambda (pair)
;;                          (if (listp pair)
;;                              `(setq ,(car pair) ,(cadr pair))
;;                            `(setq ,pair nil)))
;;                        varlist)
;;              (mapcar (lambda (v)
;;                        (cons v (symbol-value v)))
;;                      ',vars)))))

(defun anything-let-internal (binding bodyfunc)
  "Set BINDING to anything buffer-local variables and Evaluate BODYFUNC.
BINDING is a list of \(VARNAME . VALUE\) pair."
  (setq anything-let-variables binding)
  (unwind-protect
       (funcall bodyfunc)
    (setq anything-let-variables nil)))


;; (@* "Core: tools")
(defun anything-current-line-contents ()
  "Current line string without properties."
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun anything-funcall-with-source (source func &rest args)
  "Call from SOURCE FUNC list or single function FUNC with ARGS.
FUNC can be a symbol or a list of functions.
Return the result of last function call."
  (let ((anything-source-name (assoc-default 'name source))
        result)
    (anything-log-eval anything-source-name func args)
    (dolist (func (if (functionp func) (list func) func) result)
      (setq result (apply func args)))))

(defun anything-funcall-foreach (sym)
  "Call the function SYM for each source if any."
  (dolist (source (anything-get-sources))
    (anything-aif (assoc-default sym source)
        (anything-funcall-with-source source it))))

(defun anything-normalize-sources (sources)
  "If SOURCES is only one source, make a list of one element."
  (cond ((or (and sources
                  (symbolp sources))
             (and (listp sources) (assq 'name sources)))
         (list sources))
        (sources)
        (t anything-sources)))

(defun anything-approximate-candidate-number (&optional in-current-source)
  "Return approximate number of candidates in `anything-buffer'.
If IN-CURRENT-SOURCE is provided return number of candidates
in the source where point is.
It is used to check if candidate number is 0, 1, or 2+."
  (with-current-buffer anything-buffer
    (save-excursion
      (if in-current-source
          (goto-char (anything-get-previous-header-pos))
          (goto-char (point-min)))
      (forward-line 1)
      (let ((count-multi 1))
        (if (anything-pos-multiline-p)
            (save-excursion
              (loop while (and (not (if in-current-source
                                        (save-excursion
                                          (forward-line 2)
                                          (or (anything-pos-header-line-p) (eobp)))
                                        (eobp)))
                               (search-forward anything-candidate-separator nil t))
                    do (incf count-multi)
                    finally return count-multi))
            (save-excursion
              (loop with ln = 0
                    while (not (if in-current-source
                                   (or (anything-pos-header-line-p) (eobp))
                                   (eobp)))
                    unless (anything-pos-header-line-p)
                    do (incf ln)
                    do (forward-line 1) finally return ln)))))))

(defmacro with-anything-quittable (&rest body)
  "If an error occur in execution of BODY, quit anything safely."
  (declare (indent 0) (debug t))
  `(let (inhibit-quit)
     (condition-case v
         (progn ,@body)
       (quit (setq anything-quit t)
             (exit-minibuffer)
             (keyboard-quit)))))

(defun anything-compose (arg-lst func-lst)
  "Apply arguments specified in ARG-LST with each function of FUNC-LST.
The result of each function will be the new `car' of ARG-LST.

This function allows easy sequencing of transformer functions."
  (dolist (func func-lst)
    (setcar arg-lst (apply func arg-lst)))
  (car arg-lst))

(defun anything-composed-funcall-with-source (source funcs &rest args)
  "With SOURCE apply `anything-funcall-with-source' with each FUNCS and ARGS.
This is used in transformers to modify candidates list."
  (if (functionp funcs)
      (apply 'anything-funcall-with-source source funcs args)
      (apply 'anything-funcall-with-source source
             (lambda (&rest args)
               (anything-compose args funcs))
             args)))

(defun anything-new-timer (variable timer)
  "Give VARIABLE value to TIMER and cancel old timer."
  (anything-aif (symbol-value variable)
      (cancel-timer it))
  (set variable timer))


;; (@* "Core: entry point")
(defconst anything-argument-keys
  '(:sources :input :prompt :resume :preselect :buffer :keymap :default :history))

;;;###autoload
(defun anything (&rest plist)
  "Main function to execute anything sources.

Keywords supported:
:sources :input :prompt :resume :preselect :buffer :keymap :default :history
Extra keywords are supported and can be added, see below.

When call interactively with no arguments deprecated `anything-sources'
will be used if non--nil.

PLIST is a list like \(:key1 val1 :key2 val2 ...\) or
\(&optional sources input prompt resume preselect buffer keymap default history\).

Basic keywords are the following:

\:sources

Temporary value of `anything-sources'.  It also accepts a
symbol, interpreted as a variable of an anything source.  It
also accepts an alist representing an anything source, which is
detected by \(assq 'name ANY-SOURCES\)

\:input

Temporary value of `anything-pattern', ie. initial input of minibuffer.

\:prompt

Prompt other than \"pattern: \".

\:resume

If t, Resurrect previously instance of `anything'.  Skip the initialization.
If 'noresume, this instance of `anything' cannot be resumed.

\:preselect

Initially selected candidate.  Specified by exact candidate or a regexp.

\:buffer

`anything-buffer' instead of *anything*.

\:keymap

`anything-map' for current `anything' session.

\:default

A default argument that will be inserted in minibuffer \
with \\<minibuffer-local-map>\\[next-history-element].
When nil of not present `thing-at-point' will be used instead.

\:history

By default all minibuffer input is pushed to `minibuffer-history',
if an argument HISTORY is provided, input will be pushed to HISTORY.
History element should be a symbol.

Of course, conventional arguments are supported, the two are same.

\(anything :sources sources :input input :prompt prompt :resume resume
           :preselect preselect :buffer buffer :keymap keymap :default default
           :history history\)
\(anything sources input prompt resume preselect buffer keymap default history\)

Other keywords are interpreted as local variables of this anything session.
The `anything-' prefix can be omitted.  For example,

\(anything :sources 'anything-c-source-buffers
           :buffer \"*buffers*\" :candidate-number-limit 10\)

means starting anything session with `anything-c-source-buffers'
source in *buffers* buffer and set variable `anything-candidate-number-limit'
to 10 as session local variable."
  (interactive)
  (if (keywordp (car plist))
      (anything-let-internal
       (anything-parse-keys plist)
       (lambda ()
         (apply 'anything
                (mapcar (lambda (key) (plist-get plist key))
                        anything-argument-keys))))
      (apply 'anything-internal plist)))

(defun anything-parse-keys (keys)
  "Parse the KEYS arguments of `anything'.
Return only the keys that are not in `anything-argument-keys'.
It is used to set local variables via `anything-let-internal'.
This allow to add arguments that are not part of `anything-argument-keys',
but are valid anything attributes.
i.e :candidate-number-limit will be bound to `anything-candidate-number-limit'
in source."
  ;; (anything-parse-keys '(:sources ((name . "test")
  ;;                                  (candidates . (a b c)))
  ;;                        :buffer "toto"
  ;;                        :candidate-number-limit 4))
  ;; ==> ((anything-candidate-number-limit . 4))
  (loop for (key value) on keys by #'cddr
        for symname = (substring (symbol-name key) 1)
        for sym = (intern (if (string-match "^anything-" symname)
                              symname
                              (concat "anything-" symname)))
        unless (memq key anything-argument-keys)
        collect (cons sym value)))

;;; (@* "Core: entry point helper")
(defun anything-internal (&optional
                            any-sources any-input
                            any-prompt any-resume
                            any-preselect any-buffer
                            any-keymap any-default any-history)
  "The internal anything function called by `anything'.
For ANY-SOURCES ANY-INPUT ANY-PROMPT ANY-RESUME ANY-PRESELECT ANY-BUFFER and
ANY-KEYMAP ANY-DEFAULT ANY-HISTORY See `anything'."
  (anything-log (concat "[Start session] " (make-string 41 ?+)))
  (anything-log-eval any-prompt any-preselect
                     any-buffer any-keymap any-default)
  (let ((old-overridding-local-map overriding-local-map))
    (unwind-protect
         (condition-case v
             (let ( ;; It is needed because `anything-source-name' is non-nil
                   ;; when `anything' is invoked by action. Awful global scope.
                   anything-source-name
                   anything-in-persistent-action
                   anything-quit
                   (case-fold-search t)
                   (anything-buffer (or any-buffer anything-buffer))
                   ;; cua-mode ; avoid error when region is selected
                   )
               (with-anything-restore-variables
                 (anything-initialize any-resume any-input any-sources)
                 (anything-display-buffer anything-buffer)
                 (anything-log "show prompt")
                 (unwind-protect
                      (anything-read-pattern-maybe
                       any-prompt any-input any-preselect
                       any-resume any-keymap any-default
                       (when (and any-history (symbolp any-history)) any-history))
                   (anything-cleanup)))
               (prog1 (unless anything-quit
                        (anything-execute-selection-action-1))
                 (anything-log (concat "[End session] " (make-string 41 ?-)))))
           (quit
            (anything-restore-position-on-quit)
            (anything-log (concat "[End session (quit)] " (make-string 34 ?-)))
            nil))
      (setq overriding-local-map old-overridding-local-map)
      (anything-log-save-maybe))))


;;; Anything resume
;;
;;
(defun* anything-resume (&optional
                         (any-buffer anything-last-buffer)
                         buffer-pattern (any-resume t))
  "Resurrect previously invoked `anything'.
Called with a prefix arg, allow choosing among all existing
anything buffers.  i.e choose among various anything sessions."
  (interactive)
  (when (or current-prefix-arg buffer-pattern)
    (setq any-buffer (anything-resume-select-buffer buffer-pattern)))
  (setq anything-compiled-sources nil)
  (anything
   :sources (or (buffer-local-value 'anything-last-sources-local (get-buffer any-buffer))
                anything-last-sources anything-sources)
   :input (buffer-local-value 'anything-input-local (get-buffer any-buffer))
   :resume any-resume
   :buffer any-buffer))

;;; rubikitch: experimental
;;; I use this and check it whether I am convenient.
;;; I may introduce an option to control the behavior.
(defun* anything-resume-window-only (&optional
                                     (any-buffer anything-last-buffer)
                                     buffer-pattern)
  (interactive)
  (anything-resume any-buffer buffer-pattern 'window-only))

(defun anything-resume-p (any-resume)
  "Whether current anything session is resumed or not.
Just check if ANY-RESUME value is t or window-only."
  (memq any-resume '(t window-only)))

(defun anything-resume-select-buffer (input)
  "Resume precedent anything session with initial input INPUT."
  (or (anything :sources '(((name . "Resume anything buffer")
                            (candidates . anything-buffers)
                            (action . identity)))
                :input  input
                :resume 'noresume
                :buffer "*anything resume*")
      (keyboard-quit)))


;;;###autoload
(defun anything-at-point (&optional
                            any-sources any-input
                            any-prompt any-resume
                            any-preselect any-buffer)
  "Call anything with symbol at point as initial input.
ANY-SOURCES ANY-INPUT ANY-PROMPT ANY-RESUME ANY-PRESELECT and ANY-BUFFER
are same args as in `anything'."
  (interactive)
  (anything :sources any-sources
            :input (if current-prefix-arg
                       (concat "\\b" (thing-at-point 'symbol) "\\b"
                               (if (featurep 'anything-match-plugin) " " ""))
                       any-input)
            :prompt any-prompt
            :resume any-resume
            :preselect any-preselect
            :buffer any-buffer))

;;;###autoload
(defun anything-other-buffer (any-sources any-buffer)
  "Simplified interface of `anything' with other `anything-buffer'.
Call `anything' with only ANY-SOURCES and ANY-BUFFER as args."
  (anything :sources any-sources :buffer any-buffer))

(defun anything-nest (&rest same-as-anything)
  "Allow calling `anything' whithin a running anything session."
  (with-anything-window
    (let (anything-current-position
          anything-current-buffer
          (orig-anything-current-buffer anything-current-buffer)
          (orig-anything-buffer anything-buffer)
          (orig-anything-last-frame-or-window-configuration
           anything-last-frame-or-window-configuration)
          anything-pattern
          (anything-buffer (or (getf same-as-anything :buffer)
                               (nth 5 same-as-anything)
                               "*Anything*"))
          anything-sources
          anything-compiled-sources
          (anything-samewindow t)
          (enable-recursive-minibuffers t))
      (unwind-protect
           (apply #'anything same-as-anything)
        (with-current-buffer orig-anything-buffer
          (anything-initialize-overlays orig-anything-buffer)
          (setq anything-buffer (current-buffer))
          (anything-mark-current-line)
          (setq anything-last-frame-or-window-configuration
                orig-anything-last-frame-or-window-configuration)
          (setq cursor-type t)
          (setq anything-current-buffer orig-anything-current-buffer))))))


;;; Initialize
;;
;;
(defvar anything-buffers nil
  "All of `anything-buffer' in most recently used order.")
(defun anything-initialize (any-resume any-input any-sources)
  "Start initialization of `anything' session.
For ANY-RESUME ANY-INPUT and ANY-SOURCES See `anything'."
  (anything-log "start initialization: any-resume=%S any-input=%S" any-resume any-input)
  (anything-frame-or-window-configuration 'save)
  (setq anything-sources (anything-normalize-sources any-sources))
  (anything-log "sources = %S" anything-sources)
  (anything-hooks 'setup)
  (anything-current-position 'save)
  (if (anything-resume-p any-resume)
      (anything-initialize-overlays (anything-buffer-get))
      (anything-initial-setup))
  (unless (eq any-resume 'noresume)
    (anything-recent-push anything-buffer 'anything-buffers)
    (setq anything-last-buffer anything-buffer))
  (when any-input (setq anything-input any-input anything-pattern any-input))
  (and (anything-resume-p any-resume) (anything-funcall-foreach 'resume))
  (anything-log "end initialization"))

(defun anything-execute-selection-action-1 ()
  "Execute current action."
  (anything-log-run-hook 'anything-before-action-hook)
  (unwind-protect
       (anything-execute-selection-action)
    (anything-aif (get-buffer anything-action-buffer)
        (kill-buffer it))
    (anything-log-run-hook 'anything-after-action-hook)))

(defun anything-restore-position-on-quit ()
  "Restore position in `anything-current-buffer' when quitting."
  (anything-current-position 'restore))

(defun anything-recent-push (elt list-var)
  "Add ELT to the value of LIST-VAR as most recently used value."
  (let ((m (member elt (symbol-value list-var))))
    (and m (set list-var (delq (car m) (symbol-value list-var))))
    (push elt (symbol-value list-var))))


;;; (@* "Core: Accessors")
;;; rubikitch: I love to create functions to control variables.
(defvar anything-current-position nil
  "Cons of \(point . window-start\)  when `anything' is invoked.
It is needed to restore position in `anything-current-buffer'
when `anything' is keyboard-quitted.")
(defun anything-current-position (save-or-restore)
  "Restore or save current position in `anything-current-buffer'.
Argument SAVE-OR-RESTORE is one of save or restore."
  (case save-or-restore
    (save
     (anything-log "Save position at %S" (cons (point) (window-start)))
     (setq anything-current-position (cons (point) (window-start))))
    (restore
     (anything-log "Restore position at  %S in buffer %s"
                   anything-current-position
                   (buffer-name (current-buffer)))
     (goto-char (car anything-current-position))
     ;; Fix this position with the NOFORCE arg of `set-window-start'
     ;; otherwise, if there is some other buffer than `anything-current-buffer'
     ;; one, position will be lost.
     (set-window-start (selected-window) (cdr anything-current-position) t))))


;; Internal.
(defvar anything-last-frame-or-window-configuration nil
  "Used to store window or frame configuration when anything start.")
(defun anything-frame-or-window-configuration (save-or-restore)
  "Save or restore last frame or window configuration.
Possible value of SAVE-OR-RESTORE are 'save and 'restore.
window or frame configuration is saved/restored according to values of
`anything-save-configuration-functions'."
  (anything-log-eval anything-save-configuration-functions)
  (case save-or-restore
    (save    (setq anything-last-frame-or-window-configuration
                   (funcall (cdr anything-save-configuration-functions))))
    (restore (funcall (car anything-save-configuration-functions)
                      anything-last-frame-or-window-configuration)
             ;; Restore frame focus.
             (let ((frame (and (listp anything-last-frame-or-window-configuration)
                               (caadr anything-last-frame-or-window-configuration))))
               ;; If `anything-save-configuration-functions' are window functions
               ;; frame should be nil, use current frame.
               (unless (framep frame)
                 (setq frame (selected-frame)))
               (select-frame-set-input-focus frame)))))


;; (@* "Core: Display *anything* buffer")
(defun anything-display-buffer (buf)
  "Display *anything* buffer BUF."
  (let (pop-up-frames)
    (funcall (with-current-buffer buf anything-display-function) buf)))

(defun anything-default-display-buffer (buf)
  "Default function to display BUF.
Where BUF is generally `anything-buffer'.
It use `switch-to-buffer' or `pop-to-buffer' depending of value of
`anything-samewindow'."
  (funcall (if anything-samewindow 'switch-to-buffer 'pop-to-buffer) buf))


;; (@* "Core: initialize")
(defun anything-initial-setup ()
  "Initialize anything settings and set up the anything buffer."
  (anything-log-run-hook 'anything-before-initialize-hook)
  (setq anything-current-prefix-arg nil)
  (setq anything-once-called-functions nil)
  (setq anything-delayed-init-executed nil)
  (setq anything-alive-p t)
  (setq anything-current-buffer
        (if (minibuffer-window-active-p (minibuffer-window))
            ;; If minibuffer is active be sure to use it's buffer
            ;; as `anything-current-buffer'.
            (window-buffer (active-minibuffer-window))
            (current-buffer)))
  (setq anything-buffer-file-name buffer-file-name)
  (setq anything-issued-errors nil)
  (setq anything-compiled-sources nil)
  (setq anything-saved-current-source nil)
  (if (or (not split-width-threshold)
          (and (integerp split-width-threshold)
               (>= split-width-threshold (+ (frame-width) 4))))
      (setq anything-split-window-state 'vertical)
      (setq anything-split-window-state 'horizontal))
  ;; Call the init function for sources where appropriate
  (anything-funcall-foreach 'init)
  (setq anything-pattern "")
  (setq anything-input "")
  (setq anything-candidate-cache nil)
  (setq anything-last-sources anything-sources)
  (anything-create-anything-buffer)
  (anything-log-run-hook 'anything-after-initialize-hook))

(defvar anything-reading-pattern nil
  "Whether in `read-string' in anything or not.")

(defun anything-read-pattern-maybe (any-prompt any-input
                                    any-preselect any-resume any-keymap
                                    any-default any-history)
  "Read pattern with prompt ANY-PROMPT and initial input ANY-INPUT.
For ANY-PRESELECT ANY-RESUME ANY-KEYMAP, See `anything'."
  (if (anything-resume-p any-resume)
      (anything-mark-current-line t)
      (anything-update any-preselect))
  (with-current-buffer (anything-buffer-get)
    (let ((src-keymap (assoc-default 'keymap (anything-get-current-source))))
      ;; Startup with the first keymap found either in current source
      ;; or anything arg, otherwise use global value of `anything-map'.
      ;; This map will be used as a `minibuffer-local-map'.
      ;; Maybe it will be overriden when changing source
      ;; by `anything-maybe-update-keymap'.
      (unless anything-local-map-override-anything-map
        (anything-aif (or src-keymap any-keymap)
            (ignore-errors
              (set-keymap-parent it anything-map))))
      (set (make-local-variable 'anything-map)
           (or src-keymap any-keymap anything-map))
      (anything-log-eval (anything-approximate-candidate-number)
                         anything-execute-action-at-once-if-one
                         anything-quit-if-no-candidate)
      (cond ((and anything-execute-action-at-once-if-one
                  (= (anything-approximate-candidate-number) 1))
             (ignore))
            ((and anything-quit-if-no-candidate
                  (= (anything-approximate-candidate-number) 0))
             (setq anything-quit t)
             (and (functionp anything-quit-if-no-candidate)
                  (funcall anything-quit-if-no-candidate)))
            (t
             (let ((anything-reading-pattern t)
                   (tap (or any-default
                            (with-anything-current-buffer
                              (thing-at-point 'symbol)))))
               (read-from-minibuffer (or any-prompt "pattern: ")
                                     any-input anything-map
                                     nil any-history tap)))))))
               
(defun anything-maybe-update-keymap ()
  "Handle differents keymaps in multiples sources.
This function is meant to be run in `anything-move-selection-after-hook'.
It will override `anything-map' with the keymap attribute of current source
if some when multiples sources are present."
  (with-anything-window
    (let ((kmap (assoc-default 'keymap (anything-get-current-source))))
      (when kmap
        (and (not anything-local-map-override-anything-map)
             (ignore-errors (set-keymap-parent kmap (default-value 'anything-map))))
        (setq overriding-local-map kmap)))))
(add-hook 'anything-move-selection-after-hook 'anything-maybe-update-keymap)

(defun anything-create-anything-buffer (&optional test-mode)
  "Create newly created `anything-buffer'.
If TEST-MODE is non-nil, clear `anything-candidate-cache'."
  (when test-mode
    (setq anything-candidate-cache nil))
  (with-current-buffer (get-buffer-create anything-buffer)
    (anything-log "kill local variables: %S" (buffer-local-variables))
    (kill-all-local-variables)
    (set (make-local-variable 'inhibit-read-only) t)
    (buffer-disable-undo)
    (erase-buffer)
    (set (make-local-variable 'inhibit-read-only) t)
    (set (make-local-variable 'anything-last-sources-local) anything-sources)
    (set (make-local-variable 'anything-follow-mode) nil)
    (set (make-local-variable 'anything-display-function) anything-display-function)
    (set (make-local-variable 'anything-selection-point) nil)
    (anything-initialize-persistent-action)
    (anything-log-eval anything-display-function anything-let-variables)
    (loop for (var . val) in anything-let-variables
          do (set (make-local-variable var) val))
    (setq cursor-type nil)
    (setq mode-name "Anything"))
  (anything-initialize-overlays anything-buffer)
  (get-buffer anything-buffer))

(defun anything-initialize-overlays (buffer)
  "Initialize anything overlays in BUFFER."
  (anything-log "overlay setup")
  (if anything-selection-overlay
      ;; make sure the overlay belongs to the anything buffer if
      ;; it's newly created
      (move-overlay anything-selection-overlay (point-min) (point-min)
                    (get-buffer buffer))

      (setq anything-selection-overlay
            (make-overlay (point-min) (point-min) (get-buffer buffer)))
      (overlay-put anything-selection-overlay 'face anything-selection-face))

  (cond (anything-enable-shortcuts
         (setq anything-shortcut-keys
               (assoc-default anything-enable-shortcuts anything-shortcut-keys-alist))
         (unless anything-digit-overlays
           (setq anything-digit-overlays
                 (loop for key across anything-shortcut-keys
                       for overlay = (make-overlay (point-min) (point-min)
                                                   (get-buffer buffer))
                       do (overlay-put overlay 'before-string
                                       (format "%s - " (upcase (make-string 1 key))))
                       collect overlay))))
        (anything-digit-overlays
         (mapc 'delete-overlay anything-digit-overlays)
         (setq anything-digit-overlays nil))))

(defun anything-hooks (setup-or-cleanup)
  "Add or remove hooks according to SETUP-OR-CLEANUP value.
if SETUP-OR-CLEANUP value is setup add hooks, any other value
will remove hooks.
hooks concerned are `post-command-hook' and `minibuffer-setup-hook'."
  (let ((hooks '((post-command-hook anything-check-minibuffer-input)
                 (minibuffer-setup-hook anything-print-error-messages))))
    (if (eq setup-or-cleanup 'setup)
        (dolist (args hooks) (apply 'add-hook args))
        (dolist (args (reverse hooks)) (apply 'remove-hook args)))))


;; (@* "Core: clean up")
;;; TODO move
(defun anything-cleanup ()
  "Clean up the mess when anything exit or quit."
  (anything-log "start cleanup")
  (with-current-buffer anything-buffer
    ;; rubikitch: I think it is not needed.
    ;; thierry: If you end up for any reasons (error etc...)
    ;; with an anything-buffer staying around (visible),
    ;; You will have no cursor in this buffer when switching to it,
    ;; so I think this is needed.
    (setq cursor-type t)
    ;; Call burry-buffer whithout arg
    ;; to be sure anything-buffer is removed from window.
    (bury-buffer)
    ;; Be sure we call this from anything-buffer.
    (anything-funcall-foreach 'cleanup))
  (anything-new-timer 'anything-check-minibuffer-input-timer nil)
  (anything-kill-async-processes)
  (anything-log-run-hook 'anything-cleanup-hook)
  (anything-hooks 'cleanup)
  (anything-frame-or-window-configuration 'restore)
  (setq anything-alive-p nil)
  ;; This is needed in some cases where last input
  ;; is yielded infinitely in minibuffer after anything session.
  (anything-clean-up-minibuffer))

(defun anything-clean-up-minibuffer ()
  "Remove contents of minibuffer."
  (let ((miniwin (minibuffer-window)))
    ;; Clean only current minibuffer used by anything.
    ;; i.e The precedent one is active.
    (unless (minibuffer-window-active-p miniwin)
      (with-current-buffer (window-buffer miniwin)
        (delete-minibuffer-contents)))))


;; (@* "Core: input handling")
(defun anything-check-minibuffer-input ()
  "Extract input string from the minibuffer and check if it needs to be handled."
  (let ((delay (with-current-buffer anything-buffer
                 (and anything-input-idle-delay
                      (max anything-input-idle-delay 0.1)))))
    (if (or (not delay) (anything-action-window))
        (anything-check-minibuffer-input-1)
        (anything-new-timer
         'anything-check-minibuffer-input-timer
         (run-with-idle-timer delay nil 'anything-check-minibuffer-input-1)))))

(defun anything-check-minibuffer-input-1 ()
  "Check minibuffer content."
  (with-anything-quittable
    (with-selected-window (or (active-minibuffer-window) (minibuffer-window))
      (anything-check-new-input (minibuffer-contents)))))

(defun anything-check-new-input (input)
  "Check INPUT string and update the anything buffer if necessary."
  (unless (equal input anything-pattern)
    (setq anything-pattern input)
    (unless (anything-action-window)
      (setq anything-input anything-pattern))
    (anything-log-eval anything-pattern anything-input)
    (anything-update)))


;; (@* "Core: source compiler")
(defvar anything-compile-source-functions-default anything-compile-source-functions
  "Plug-ins this file provides.")
(defun anything-compile-sources (sources funcs)
  "Compile SOURCES with FUNCS.
See `anything-compile-source-functions'.
Anything plug-ins are realized by this function."
  (mapcar
   (lambda (source)
     (loop with source = (if (listp source) source (symbol-value source))
           for f in funcs
           do (setq source (funcall f source))
           finally (return source)))
   sources))


;; (@* "Core: plug-in attribute documentation hack")

;; `anything-document-attribute' is public API.
(defadvice documentation-property (after anything-document-attribute activate)
  "Display plug-in attributes' documentation as `anything-sources' docstring."
  (when (eq (ad-get-arg 0) 'anything-sources)
    (setq ad-return-value
          (concat ad-return-value "\n"
                  (mapconcat (lambda (sym) (get sym 'anything-attrdoc))
                             anything-additional-attributes
                             "\n")))))
;; (describe-variable 'anything-sources)
;; (documentation-property 'anything-sources 'variable-documentation)
;; (progn (ad-disable-advice 'documentation-property 'after 'anything-document-attribute) (ad-update 'documentation-property))


;; (@* "Core: all candidates")
(defun anything-process-delayed-init (source)
  "Initialize delayed SOURCE."
  (let ((name (assoc-default 'name source)))
    (unless (member name anything-delayed-init-executed)
      (anything-aif (assoc-default 'delayed-init source)
          (with-current-buffer anything-current-buffer
            (anything-funcall-with-source source it)
            (dolist (f (if (functionp it) (list it) it))
              (add-to-list 'anything-delayed-init-executed name)))))))

(defun anything-get-candidates (source)
  "Retrieve and return the list of candidates from SOURCE."
  (anything-process-delayed-init source)
  (let* ((candidate-source (assoc-default 'candidates source))
         (type-error (lambda ()
                       (error (concat "Candidates must either be a function, "
                                      " a variable or a list: %s")
                              candidate-source)))
         (candidates (condition-case err
                         (anything-interpret-value candidate-source source)
                       (error (funcall type-error)))))
    (cond ((processp candidates) candidates)
          ((listp candidates) (anything-transform-candidates candidates source))
          (t (funcall type-error)))))


(defun anything-get-cached-candidates (source)
  "Return the cached value of candidates for SOURCE.
Cache the candidates if there is not yet a cached value."
  (let* ((name (assoc-default 'name source))
         (candidate-cache (assoc name anything-candidate-cache)))
    (cond (candidate-cache
           (anything-log "use cached candidates")
           (cdr candidate-cache))
          (t
           (anything-log "calculate candidates")
           (let ((candidates (anything-get-candidates source)))
             (cond ((processp candidates)
                    (push (cons candidates
                                (append source
                                        (list (cons 'item-count 0)
                                              (cons 'incomplete-line ""))))
                          anything-async-processes)
                    (set-process-filter candidates 'anything-output-filter)
                    (setq candidates nil))
                   ((not (assoc 'volatile source))
                    (setq candidate-cache (cons name candidates))
                    (push candidate-cache anything-candidate-cache)))
             candidates)))))


;;; (@* "Core: candidate transformers")
(defun anything-transform-mapcar (function args)
  "`mapcar' for candidate-transformer.

ARGS is (cand1 cand2 ...) or ((disp1 . real1) (disp2 . real2) ...)

\(anything-transform-mapcar 'upcase '(\"foo\" \"bar\"))
=> (\"FOO\" \"BAR\")
\(anything-transform-mapcar 'upcase '((\"1st\" . \"foo\") (\"2nd\" . \"bar\")))
=> ((\"1st\" . \"FOO\") (\"2nd\" . \"BAR\"))
"
  (loop for arg in args
        if (consp arg)
        collect (cons (car arg) (funcall function (cdr arg)))
        else
        collect (funcall function arg)))

(defun anything-process-candidate-transformer (candidates source)
  "Execute candidate-transformer function on all CANDIDATES of SOURCE."
  (anything-aif (assoc-default 'candidate-transformer source)
      (anything-composed-funcall-with-source source it candidates)
    candidates))

(defun anything-process-filtered-candidate-transformer (candidates source)
  "Execute filtered-candidate-transformer function on all CANDIDATES of SOURCE."
  (anything-aif (assoc-default 'filtered-candidate-transformer source)
      (anything-composed-funcall-with-source source it candidates source)
    candidates))

(defun anything-process-filtered-candidate-transformer-maybe (candidates source process-p)
  "Execute filtered-candidate-transformer function on all CANDIDATES of SOURCE.
This happen if PROCESS-P is non-nil."
  (if process-p
      (anything-process-filtered-candidate-transformer candidates source)
      candidates))

(defun anything-process-real-to-display (candidates source)
  "Execute real-to-display function on all CANDIDATES of SOURCE."
  (anything-aif (assoc-default 'real-to-display source)
      (setq candidates (anything-funcall-with-source
                        source 'mapcar
                        (lambda (cand_)
                          (if (consp cand_)
                              ;; override DISPLAY from candidate-transformer
                              (cons (funcall it (cdr cand_)) (cdr cand_))
                              (cons (funcall it cand_) cand_)))
                        candidates))
    candidates))

(defun anything-transform-candidates (candidates source &optional process-p)
  "Transform CANDIDATES of SOURCE according to candidate transformers.
This happen if PROCESS-P is non-nil."
  (anything-process-real-to-display
   (anything-process-filtered-candidate-transformer-maybe
    (anything-process-candidate-transformer candidates source) source process-p)
   source))


;; (@* "Core: narrowing candidates")
(defun anything-candidate-number-limit (source)
  "Apply candidate-number-limit attribute value.
This overhide variable `anything-candidate-number-limit'.

e.g:
If \(candidate-number-limit\) is in SOURCE, show all candidates in SOURCE.
If \(candidate-number-limit . 123\) is in SOURCE limit candidate to 123."
  (anything-aif (assq 'candidate-number-limit source)
      (or (cdr it) 99999999)
    (or anything-candidate-number-limit 99999999)))

;; FIXME: Why a defconst here
(defconst anything-default-match-functions
  (list (lambda (candidate)
          (string-match anything-pattern candidate)))
  "Default functions to match candidates according to `anything-pattern'.")

(defun anything-compute-matches (source)
  "Compute matched results from SOURCE according to its settings."
  (if debug-on-error
      (anything-compute-matches-internal source)
      (condition-case v
          (anything-compute-matches-internal source)
        (error (anything-log-error
                "anything-compute-matches: error when processing source: %s"
                (assoc-default 'name source))
               nil))))

(defun anything-candidate-get-display (candidate)
  "Get searched display part from CANDIDATE.
CANDIDATE is a string, a symbol, or \(DISPLAY . REAL\) cons cell."
  (format "%s" (or (car-safe candidate) candidate)))

(defun anything-process-pattern-transformer (pattern source)
  "Execute pattern-transformer attribute PATTERN function in SOURCE."
  (anything-aif (assoc-default 'pattern-transformer source)
      (anything-composed-funcall-with-source source it pattern)
    pattern))

(defun anything-match-functions (source)
  (or (assoc-default 'match source)
      anything-default-match-functions))

(defmacro anything-accumulate-candidates-internal (cand newmatches
                                                   hash item-count limit)
  "Internal, add CAND into NEWMATCHES.
Use HASH to uniq NEWMATCHES.
Argument ITEM-COUNT count the matches.
if ITEM-COUNT reaches LIMIT, exit from inner loop."
  `(unless (gethash ,cand ,hash)
     (puthash ,cand t ,hash)
     (push ,cand ,newmatches)
     (incf ,item-count)
     (when (= ,item-count ,limit)
       (setq exit t)
       (return))))

(defun anything-take-first-elements (seq n)
  (if (> (length seq) n)
      (setq seq (subseq seq 0 n))
      seq))

(defun anything-match-from-candidates (cands matchfns limit)
  (let (matches)
    (condition-case nil
        (let ((item-count 0) exit)
          (clrhash anything-match-hash)
          (dolist (match matchfns)
            (let (newmatches)
              (dolist (candidate cands)
                (when (funcall match (anything-candidate-get-display candidate))
                  (anything-accumulate-candidates-internal
                   candidate newmatches anything-match-hash item-count limit)))
              (setq matches (append matches (reverse newmatches)))
              (if exit (return)))))
      (invalid-regexp (setq matches nil)))
    matches))

(defun anything-compute-matches-internal (source)
  (save-current-buffer
    (let ((matchfns (anything-match-functions source))
          (anything-source-name (assoc-default 'name source))
          (limit (anything-candidate-number-limit source))
          (anything-pattern (anything-process-pattern-transformer
                             anything-pattern source)))
      (anything-process-filtered-candidate-transformer
       (if (or (equal anything-pattern "") (equal matchfns '(identity)))
           (anything-take-first-elements
            (anything-get-cached-candidates source) limit)
           (anything-match-from-candidates
            (anything-get-cached-candidates source) matchfns limit))
       source))))

;; (anything '(((name . "error")(candidates . (lambda () (hage))) (action . identity))))

(defun anything-process-source (source)
  "Display matched results from SOURCE according to its settings."
  (anything-log-eval (assoc-default 'name source))
  (if (assq 'direct-insert-match source) ;experimental
      (anything-process-source--direct-insert-match source)
      (let ((matches (anything-compute-matches source)))
        (when matches
          (when anything-test-mode
            (setq anything-test-candidate-list
                  `(,@anything-test-candidate-list
                    (,(assoc-default 'name source)
                      ,matches))))
          (anything-insert-header-from-source source)
          (if (not (assq 'multiline source))
              (mapc 'anything-insert-match-with-digit-overlay matches)
              (let ((start (point)) separate)
                (dolist (match matches)
                  (if separate
                      (anything-insert-candidate-separator)
                      (setq separate t))
                  (anything-insert-match-with-digit-overlay match))
                (put-text-property start (point) 'anything-multiline t)))))))

(defun anything-insert-match-with-digit-overlay (match)
  (declare (special source))
  (anything-put-digit-overlay-maybe)
  (anything-insert-match match 'insert source))

(defun anything-put-digit-overlay-maybe ()
  (when (and anything-enable-shortcuts
             (not (eq anything-digit-shortcut-count
                      (length anything-digit-overlays))))
    (move-overlay (nth anything-digit-shortcut-count
                       anything-digit-overlays)
                  (point-at-bol)
                  (point-at-bol))
    (incf anything-digit-shortcut-count)))

(defun anything-process-source--direct-insert-match (source)
  "[EXPERIMENTAL] Insert candidates from `anything-candidate-buffer' in SOURCE."
  (anything-log-eval (assoc-default 'name source))
  (let ((anything-source-name (assoc-default 'name source))
        content-buf)
    (funcall (assoc-default 'candidates source))
    (setq content-buf (anything-candidate-buffer))
    (unless (anything-empty-buffer-p content-buf)
      (anything-insert-header-from-source source)
      (insert-buffer-substring content-buf)
      ;; TODO call anything-put-digit-overlay-maybe with loop
      )))

(defun anything-process-delayed-sources (delayed-sources &optional preselect)
  "Process anything DELAYED-SOURCES.
Move selection to string or regexp PRESELECT if non--nil.
This function is called in `anything-process-delayed-sources-timer'
when emacs is idle for `anything-idle-delay'."
  (with-anything-quittable
    (anything-log-eval (mapcar (lambda (s) (assoc-default 'name s)) delayed-sources))
    (with-current-buffer anything-buffer
      (save-excursion
        (goto-char (point-max))
        (mapc 'anything-process-source delayed-sources)
        (when (and (not (anything-empty-buffer-p))
                   ;; No selection yet.
                   (= (overlay-start anything-selection-overlay)
                      (overlay-end anything-selection-overlay)))
          (anything-update-move-first-line 'without-hook)))
      (when preselect (anything-preselect preselect))
      (save-excursion
        (goto-char (point-min))
        (anything-log-run-hook 'anything-update-hook))
      (anything-log-run-hook 'anything-after-update-hook))))


;; (@* "Core: *anything* buffer contents")
(defvar anything-input-local nil)
(defvar anything-process-delayed-sources-timer nil)
(defun anything-update (&optional preselect)
  "Update candidates list in `anything-buffer' according to `anything-pattern'.
Argument PRESELECT is a string or regexp used to move selection to a particular
place once updating is done.  It should be used on single source because search
is done on whole `anything-buffer' and not on current source."
  (anything-log "start update")
  (setq anything-digit-shortcut-count 0)
  (anything-kill-async-processes)
  (with-current-buffer (anything-buffer-get)
    (set (make-local-variable 'anything-input-local) anything-pattern)
    (erase-buffer)
    (when anything-enable-shortcuts
      (mapc 'delete-overlay anything-digit-overlays))
    (let (delayed-sources
          normal-sources)
      (unwind-protect ; Process normal sources and store delayed one's.
           (setq delayed-sources
                 (loop for source in (remove-if-not 'anything-update-source-p
                                                    (anything-get-sources))
                       if (anything-delayed-source-p source)
                       collect source
                       else do (progn (push source normal-sources)
                                      (anything-process-source source))))
        (anything-log-eval
         (mapcar (lambda (s) (assoc-default 'name s)) delayed-sources))
        (if anything-test-mode ; Need only to process sources.
            (mapc 'anything-process-source delayed-sources)
            (cond ((and preselect delayed-sources normal-sources)
                   ;; Preselection run here when there is
                   ;; normal AND delayed sources.
                   (anything-log "Update preselect candidate %s" preselect)
                   (anything-preselect preselect))
                  (delayed-sources ; Preselection and hooks will run later.
                   (anything-update-move-first-line 'without-hook))
                  (t ; No delayed sources, run the hooks now.
                   (anything-update-move-first-line)
                   (anything-log-run-hook 'anything-after-update-hook)
                   (when preselect
                     (anything-log "Update preselect candidate %s" preselect)
                     (anything-preselect preselect))))
            (when delayed-sources
              (anything-new-timer
               'anything-process-delayed-sources-timer
               (run-with-idle-timer
                ;; Be sure anything-idle-delay is >
                ;; to anything-input-idle-delay
                ;; otherwise use value of anything-input-idle-delay
                ;; or 0.1 if == to 0.
                (max anything-idle-delay anything-input-idle-delay 0.1) nil
                'anything-process-delayed-sources delayed-sources preselect))))
        (anything-log "end update")))))

(defun anything-update-source-p (source)
  "Wheter SOURCE need updating or not."
  (and (or (not anything-source-filter)
           (member (assoc-default 'name source) anything-source-filter))
       (>= (length anything-pattern)
           (anything-aif (assoc 'requires-pattern source)
               (or (cdr it) 1)
             0))))

(defun anything-delayed-source-p (source)
  "Wheter SOURCE is a delayed source or not."
  (or (assoc 'delayed source)
      (and anything-quick-update
           (< (window-height (get-buffer-window (current-buffer)))
              (line-number-at-pos (point-max))))))

(defun anything-update-move-first-line (&optional without-hook)
  "Goto first line of `anything-buffer'."
  (goto-char (point-min))
  (unless without-hook
    (save-excursion (anything-log-run-hook 'anything-update-hook)))
  (anything-next-line))

(defun anything-force-update (&optional preselect)
  "Force recalculation and update of candidates.
If current source has `update' attribute, a function without argument,
call it before update."
  (interactive)
  (let ((source    (anything-get-current-source))
        (selection (anything-get-selection nil t)))
    (when source
      (mapc 'anything-force-update--reinit
            (anything-get-sources)))
    (anything-update preselect)
    ;; If preselect arg exists, `anything-update' should
    ;; have moved to selection, otherwise do it now.
    (unless preselect
      (anything-keep-selection (assoc-default 'name source) selection))
    (with-anything-window (recenter))))

(defun anything-force-update--reinit (source)
  "Reinit SOURCE by calling his update and/or init functions."
  (anything-aif (anything-funcall-with-source
                 source 'anything-candidate-buffer)
      (kill-buffer it))
  (dolist (attr '(update init))
    (anything-aif (assoc-default attr source)
        (anything-funcall-with-source source it)))
  (anything-remove-candidate-cache source))

(defun anything-keep-selection (source selection)
  "Switch to SOURCE and goto SELECTION."
  (when (and source selection)
    (with-anything-window
      (anything-goto-source source)
      (forward-char -1)
      (if (search-forward selection nil t)
          (forward-line 0)
          (goto-char (point-min))
          (forward-line 1))
      (anything-mark-current-line))))

(defun anything-remove-candidate-cache (source)
  "Remove SOURCE from `anything-candidate-cache'."
  (setq anything-candidate-cache
        (delete (assoc (assoc-default 'name source)
                       anything-candidate-cache)
                anything-candidate-cache)))

(defun anything-insert-match (match insert-function source)
  "Insert MATCH into `anything-buffer' with INSERT-FUNCTION for SOURCE.
If MATCH is a list then insert the string intended to appear on the display
and store the real value in a text property."
  (let ((start     (point-at-bol (point)))
        (dispvalue (or (car-safe match) match))
        (realvalue (cdr-safe match)))
    (setq dispvalue
          (cond ((symbolp dispvalue) (symbol-name dispvalue))
                ((numberp dispvalue) (number-to-string dispvalue))
                (t dispvalue)))
    (when (stringp dispvalue)
      (funcall insert-function dispvalue)
      ;; Some sources with candidates-in-buffer have already added
      ;; 'anything-realvalue property when creating candidate buffer.
      (unless (get-text-property start 'anything-realvalue)
        (and realvalue
             (put-text-property start (point-at-eol)
                                'anything-realvalue realvalue)))
      (when anything-source-in-each-line-flag
        (put-text-property start (point-at-eol) 'anything-source source))
      (funcall insert-function "\n"))))

(defun anything-insert-header-from-source (source)
  "Insert SOURCE name in `anything-buffer' header.
Maybe insert by overlay additional info after source name if SOURCE have
header-name attribute."
  (let ((name (assoc-default 'name source)))
    (anything-insert-header
     name
     (anything-aif (assoc-default 'header-name source)
         (anything-funcall-with-source source it name)))))

(defun anything-insert-header (name &optional display-string)
  "Insert header of source NAME into the anything buffer.
If DISPLAY-STRING is non--nil and a string, display this additional info
after the source name by overlay."
  (unless (bobp)
    (let ((start (point)))
      (insert "\n")
      (put-text-property start (point) 'anything-header-separator t)))
  (let ((start (point)))
    (insert name)
    (put-text-property (point-at-bol)
                       (point-at-eol) 'anything-header t)
    (when display-string
      (overlay-put (make-overlay (point-at-bol) (point-at-eol))
                   'display display-string))
    (insert "\n")
    (put-text-property start (point) 'face anything-header-face)))

(defun anything-insert-candidate-separator ()
  "Insert separator of candidates into the anything buffer."
  (insert anything-candidate-separator)
  (put-text-property (point-at-bol)
                     (point-at-eol) 'anything-candidate-separator t)
  (insert "\n"))


;; (@* "Core: async process")
(defun anything-output-filter (process string)
  "From PROCESS process output STRING."
  (anything-output-filter-1 (assoc process anything-async-processes) string))

(defun anything-output-filter-1 (process-assoc string)
  (anything-log-eval string)
  (with-current-buffer anything-buffer
    (let ((source (cdr process-assoc)))
      (save-excursion
        (anything-aif (assoc-default 'insertion-marker source)
            (goto-char it)
          (goto-char (point-max))
          (anything-insert-header-from-source source)
          (setcdr process-assoc
                  (append source `((insertion-marker . ,(point-marker))))))
        (anything-output-filter--process-source
         (car process-assoc) string source
         (anything-candidate-number-limit source))))
    (anything-output-filter--post-process)))

(defun anything-output-filter--process-source (process string source limit)
  (dolist (candidate (anything-transform-candidates
                      (anything-output-filter--collect-candidates
                       (split-string string "\n")
                       (assoc 'incomplete-line source))
                      source t))
    (if (not (assq 'multiline source))
        (anything-insert-match candidate 'insert-before-markers source)
        (let ((start (point)))
          (anything-insert-candidate-separator)
          (anything-insert-match candidate 'insert-before-markers source)
          (put-text-property start (point) 'anything-multiline t)))
    (incf (cdr (assoc 'item-count source)))
    (when (>= (assoc-default 'item-count source) limit)
      (anything-kill-async-process process)
      (return))))

(defun anything-output-filter--collect-candidates (lines incomplete-line-info)
  (anything-log-eval (cdr incomplete-line-info))
  (butlast
   (loop for line in lines collect
         (if (cdr incomplete-line-info)
             (prog1
                 (concat (cdr incomplete-line-info) line)
               (setcdr incomplete-line-info nil))
             line)
         finally (setcdr incomplete-line-info line))))

(defun anything-output-filter--post-process ()
  (anything-log-run-hook 'anything-update-hook)
  (anything-aif (get-buffer-window anything-buffer 'visible)
      (save-selected-window
        (select-window it)
        (anything-skip-noncandidate-line 'next)
        (anything-mark-current-line))))

(defun anything-kill-async-processes ()
  "Kill all known asynchronous processes of `anything-async-processes'."
  (mapc 'anything-kill-async-process (mapcar 'car anything-async-processes))
  (setq anything-async-processes nil))

(defun anything-kill-async-process (process)
  "Kill PROCESS and detach the associated functions."
  (set-process-filter process nil)
  (delete-process process))


;; (@* "Core: action")
(defun anything-execute-selection-action (&optional
                                            selection action
                                            preserve-saved-action)
  "If a candidate SELECTION is present then perform the associated ACTION on it.
If PRESERVE-SAVED-ACTION is non-nil don't save action."
  (anything-log "executing action")
  (setq action (anything-get-default-action
                (or action
                    anything-saved-action
                    (if (get-buffer anything-action-buffer)
                        (anything-get-selection anything-action-buffer)
                        (anything-get-action)))))
  (let ((source (or anything-saved-current-source
                    (anything-get-current-source))))
    (setq selection (or selection
                        (anything-get-selection)
                        (and (assoc 'accept-empty source) "")))
    (unless preserve-saved-action (setq anything-saved-action nil))
    (if (and selection action)
        (anything-funcall-with-source
         source action
         (anything-coerce-selection selection source)))))

(defun anything-coerce-selection (selection source)
  "Apply coerce attribute function to SELECTION in SOURCE.
Coerce source with coerce function."
  (anything-aif (assoc-default 'coerce source)
      (anything-funcall-with-source source it selection)
    selection))

(defun anything-get-default-action (action)
  "Get the first ACTION value of action list in source."
  (if (and (listp action) (not (functionp action)))
      (cdar action)
      action))

(defun anything-select-action ()
  "Select an action for the currently selected candidate.
If action buffer is selected, back to the anything buffer."
  (interactive)
  (anything-log-run-hook 'anything-select-action-hook)
  (cond ((get-buffer-window anything-action-buffer 'visible)
         (set-window-buffer (get-buffer-window anything-action-buffer)
                            anything-buffer)
         (kill-buffer anything-action-buffer)
         (anything-set-pattern anything-input 'noupdate))
        (t
         (setq anything-saved-selection (anything-get-selection))
         (unless anything-saved-selection
           (error "Nothing is selected"))
         (setq anything-saved-current-source (anything-get-current-source))
         (let ((actions (anything-get-action)))
           (if (functionp actions)
               (message "Sole action: %s" actions)
               (anything-show-action-buffer actions)
               (anything-delete-minibuffer-contents)
               (setq anything-pattern 'dummy) ; so that it differs from the previous one
               (anything-check-minibuffer-input))))))

(defun anything-show-action-buffer (actions)
  (with-current-buffer (get-buffer-create anything-action-buffer)
    (erase-buffer)
    (buffer-disable-undo)
    (set-window-buffer (get-buffer-window anything-buffer) anything-action-buffer)
    (set (make-local-variable 'anything-sources)
         `(((name . "Actions")
            (volatile)
            (candidates . ,actions)
            (candidate-number-limit))))
    (set (make-local-variable 'anything-source-filter) nil)
    (set (make-local-variable 'anything-selection-overlay) nil)
    (set (make-local-variable 'anything-digit-overlays) nil)
    (anything-initialize-overlays anything-action-buffer)))


;; (@* "Core: selection")
(defun anything-move-selection-common (move-func unit direction)
  "Move the selection marker to a new position wit function MOVE-FUNC.
It is determined by UNIT and DIRECTION."
  (unless (or (anything-empty-buffer-p (anything-buffer-get))
              (not (anything-window)))
    (with-anything-window
      (anything-log-run-hook 'anything-move-selection-before-hook)
      (funcall move-func)
      (anything-skip-noncandidate-line direction)
      (anything-display-source-at-screen-top-maybe unit)
      (when (anything-get-previous-header-pos)
        (anything-mark-current-line))
      (anything-display-mode-line (anything-get-current-source))
      (anything-log-run-hook 'anything-move-selection-after-hook))))

(defun anything-display-source-at-screen-top-maybe (unit)
  (when (and anything-display-source-at-screen-top (eq unit 'source))
    (set-window-start (selected-window)
                      (save-excursion (forward-line -1) (point)))))

(defun anything-skip-noncandidate-line (direction)
  (anything-skip-header-and-separator-line direction)
  (and (bobp) (forward-line 1))     ;skip first header
  (and (eobp) (forward-line -1)))   ;avoid last empty line


(defun anything-skip-header-and-separator-line (direction)
  (while (and (not (bobp))
              (or (anything-pos-header-line-p)
                  (anything-pos-candidate-separator-p)))
    (forward-line (if (and (eq direction 'previous)
                           (not (eq (point-at-bol) (point-min))))
                      -1 1))))

(defvar anything-mode-line-string-real nil)
(defun anything-display-mode-line (source)
  (set (make-local-variable 'anything-mode-line-string)
       (anything-interpret-value (or (assoc-default 'mode-line source)
                                     (default-value 'anything-mode-line-string))
                                 source))
  (if anything-mode-line-string
      (setq mode-line-format
            '(" " mode-line-buffer-identification " "
              (line-number-mode "L%l") " " (anything-follow-mode "(F) ")
              (:eval (anything-show-candidate-number
                      (when (listp anything-mode-line-string)
                        (car anything-mode-line-string))))
              " " anything-mode-line-string-real "-%-")
            anything-mode-line-string-real
            (substitute-command-keys (if (listp anything-mode-line-string)
                                         (cadr anything-mode-line-string)
                                         anything-mode-line-string)))
      (setq mode-line-format
            (default-value 'mode-line-format)))
  (setq header-line-format
        (anything-interpret-value (assoc-default 'header-line source) source)))

(defun anything-show-candidate-number (&optional name)
  "Used to display candidate number in mode-line.
You can specify NAME of candidates e.g \"Buffers\" otherwise
it is \"Candidate\(s\)\" by default."
  (propertize
   (format "[%s %s]"
           (anything-approximate-candidate-number 'in-current-source)
           (or name "Candidate(s)"))
   'face 'anything-candidate-number))

(defun anything-previous-line ()
  "Move selection to the previous line."
  (interactive)
  (anything-move-selection-common
   (lambda ()
     (if (not (anything-pos-multiline-p))
         (forward-line -1)      ;double forward-line is meaningful
         (forward-line -1)        ;because evaluation order is important
         (anything-skip-header-and-separator-line 'previous)
         (let ((header-pos (anything-get-previous-header-pos))
               (separator-pos (anything-get-previous-candidate-separator-pos)))
           (when header-pos
             (goto-char (if (or (null separator-pos) (< separator-pos header-pos))
                            header-pos ; first candidate
                            separator-pos))
             (forward-line 1)))))
   'line 'previous))

(defun anything-next-line ()
  "Move selection to the next line."
  (interactive)
  (anything-move-selection-common
   (lambda ()
     (if (not (anything-pos-multiline-p))
         (forward-line 1)
         (let ((header-pos (anything-get-next-header-pos))
               (separator-pos (anything-get-next-candidate-separator-pos)))
           (cond ((and separator-pos
                       (or (null header-pos) (< separator-pos header-pos)))
                  (goto-char separator-pos))
                 (header-pos
                  (goto-char header-pos))))))
   'line 'next))

(defun anything-previous-page ()
  "Move selection back with a pageful."
  (interactive)
  (anything-move-selection-common
   (lambda ()
     (condition-case nil
         (scroll-down)
       (beginning-of-buffer (goto-char (point-min)))))
   'page 'previous))

(defun anything-next-page ()
  "Move selection forward with a pageful."
  (interactive)
  (anything-move-selection-common
   (lambda ()
     (condition-case nil
         (scroll-up)
       (end-of-buffer (goto-char (point-max)))))
   'page 'next))

(defun anything-beginning-of-buffer ()
  "Move selection at the top."
  (interactive)
  (anything-move-selection-common
   (lambda () (goto-char (point-min)))
   'edge 'previous))

(defun anything-end-of-buffer ()
  "Move selection at the bottom."
  (interactive)
  (anything-move-selection-common
   (lambda () (goto-char (point-max)))
   'edge 'next))

(defun anything-previous-source ()
  "Move selection to the previous source."
  (interactive)
  (anything-move-selection-common
   (lambda ()
     (forward-line -1)
     (if (bobp)
         (goto-char (point-max))
         (anything-skip-header-and-separator-line 'previous))
     (goto-char (anything-get-previous-header-pos))
     (forward-line 1))
   'source 'previous))

(defun anything-next-source ()
  "Move selection to the next source."
  (interactive)
  (anything-move-selection-common
   (lambda ()
     (goto-char (or (anything-get-next-header-pos) (point-min))))
   'source 'next))

(defun anything-goto-source (source-or-name)
  "Move the selection to the source SOURCE-OR-NAME."
  (anything-move-selection-common
   (lambda ()
     (goto-char (point-min))
     (let ((name (if (stringp source-or-name) source-or-name
                     (assoc-default 'name source-or-name))))
       (condition-case err
           (while (not (string= name (anything-current-line-contents)))
             (goto-char (anything-get-next-header-pos)))
         (error (message "")))))
   'source 'next))

(defun anything-mark-current-line (&optional resumep)
  "Move `anything-selection-overlay' to current line.
Note that this is not related with visibles marks, which are used
to mark candidates."
  (with-anything-window
    (when resumep
      (goto-char anything-selection-point))
    (move-overlay
     anything-selection-overlay (point-at-bol)
     (if (anything-pos-multiline-p)
         (let ((header-pos (anything-get-next-header-pos))
               (separator-pos (anything-get-next-candidate-separator-pos)))
           (or (and (null header-pos) separator-pos)
               (and header-pos separator-pos (< separator-pos header-pos)
                    separator-pos)
               header-pos
               (point-max)))
       (1+ (point-at-eol))))
    (setq anything-selection-point (overlay-start anything-selection-overlay)))
  (anything-follow-execute-persistent-action-maybe))

(defun anything-this-command-key ()
  (event-basic-type (elt (this-command-keys-vector) 0)))
;; (progn (read-key-sequence "Key: ") (p (anything-this-command-key)))

(defun anything-select-with-shortcut-internal (types get-key-func)
  (if (memq anything-enable-shortcuts types)
      (save-selected-window
        (select-window (anything-window))
        (let* ((key (funcall get-key-func))
               (overlay (ignore-errors (nth (position key anything-shortcut-keys)
                                            anything-digit-overlays))))
          (if (not (and overlay (overlay-buffer overlay)))
              (when (numberp key)
                (select-window (minibuffer-window))
                (self-insert-command 1))
              (goto-char (overlay-start overlay))
              (anything-mark-current-line)
              (anything-exit-minibuffer))))
      (self-insert-command 1)))

(defun anything-select-with-prefix-shortcut ()
  "Invoke default action with prefix shortcut."
  (interactive)
  (anything-select-with-shortcut-internal
   '(prefix)
   (lambda () (read-event "Select shortcut key: "))))

(defun anything-select-with-digit-shortcut ()
  "Invoke default action with digit/alphabet shortcut."
  (interactive)
  (anything-select-with-shortcut-internal
   '(alphabet t) 'anything-this-command-key))

;; (setq anything-enable-shortcuts 'prefix)
;; (define-key anything-map "@" 'anything-select-with-prefix-shortcut)
;; (define-key anything-map (kbd "<f18>") 'anything-select-with-prefix-shortcut)

(defvar anything-exit-status 0
  "Flag to inform whether anything have exited or quitted.
Exit with 0 mean anything have exited executing an action.
Exit with 1 mean anything have quitted with \\[keyboard-quit]
It is useful for example to restore a window config if anything abort
in special cases.
See `anything-exit-minibuffer' and `anything-keyboard-quit'.")

(defvar anything-minibuffer-confirm-state nil)
(defun anything-confirm-and-exit-minibuffer ()
  "Maybe ask for confirmation when exiting anything.
It is similar to `minibuffer-complete-and-exit' adapted to anything.
If `minibuffer-completion-confirm' value is 'confirm,
send in minibuffer confirm message and exit on next hit.
If `minibuffer-completion-confirm' value is t,
don't exit and send message 'no match'."
  (interactive)
  (let ((empty-buffer-p (with-current-buffer anything-buffer
                          (eq (point-min) (point-max)))))
      (cond ((and empty-buffer-p
                  (eq minibuffer-completion-confirm 'confirm))
             (setq anything-minibuffer-confirm-state
                   'confirm)
             (setq minibuffer-completion-confirm nil)
             (minibuffer-message " [confirm]"))
            ((and empty-buffer-p
                  (eq minibuffer-completion-confirm t))
             (minibuffer-message " [No match]"))
            (t
             (setq anything-minibuffer-confirm-state nil)
             (anything-exit-minibuffer)))))
(add-hook 'anything-after-update-hook 'anything-confirm-and-exit-hook)

(defun anything-confirm-and-exit-hook ()
  "Restore `minibuffer-completion-confirm' when anything update."
  (unless (or (eq minibuffer-completion-confirm t)
              (not anything-minibuffer-confirm-state))
    (setq minibuffer-completion-confirm
          anything-minibuffer-confirm-state)))

(defun anything-exit-minibuffer ()
  "Select the current candidate by exiting the minibuffer."
  (interactive)
  (unless anything-current-prefix-arg
    (setq anything-current-prefix-arg current-prefix-arg))
  (setq anything-exit-status 0)
  (exit-minibuffer))

(defun anything-keyboard-quit ()
  "Quit minibuffer in anything.
If action buffer is displayed, kill it."
  (interactive)
  (when (get-buffer-window anything-action-buffer 'visible)
    (kill-buffer anything-action-buffer))
  (setq anything-exit-status 1)
  (abort-recursive-edit))

(defun anything-get-next-header-pos ()
  "Return the position of the next header from point."
  (next-single-property-change (point) 'anything-header))

(defun anything-get-previous-header-pos ()
  "Return the position of the previous header from point."
  (previous-single-property-change (point) 'anything-header))

(defun anything-pos-multiline-p ()
  "Return non-nil if the current position is in the multiline source region."
  (get-text-property (point) 'anything-multiline))

(defun anything-get-next-candidate-separator-pos ()
  "Return the position of the next candidate separator from point."
  (next-single-property-change (point) 'anything-candidate-separator))

(defun anything-get-previous-candidate-separator-pos ()
  "Return the position of the previous candidate separator from point."
  (previous-single-property-change (point) 'anything-candidate-separator))

(defun anything-pos-header-line-p ()
  "Return t if the current line is a header line."
  (or (get-text-property (point-at-bol) 'anything-header)
      (get-text-property (point-at-bol) 'anything-header-separator)))

(defun anything-pos-candidate-separator-p ()
  "Return t if the current line is a candidate separator."
  (get-text-property (point-at-bol) 'anything-candidate-separator))


;; (@* "Core: help")
(defun anything-help-internal (bufname insert-content-fn)
  "Show long message during `anything' session in BUFNAME.
INSERT-CONTENT-FN is the text to be displayed in BUFNAME."
  (save-window-excursion
    (select-window (anything-window))
    (delete-other-windows)
    (switch-to-buffer (get-buffer-create bufname))
    (erase-buffer)
    (funcall insert-content-fn)
    (setq mode-line-format "%b (SPC,C-v:NextPage  b,M-v:PrevPage  other:Exit)")
    (setq cursor-type nil)
    (goto-char 1)
    (anything-help-event-loop)))

(defun anything-help-event-loop ()
  (ignore-errors
    (loop for event = (read-event) do
          (case event
            ((?\C-v ? )  (scroll-up))
            ((?\M-v ?b) (scroll-down))
            (t (return))))))

(defun anything-help ()
  "Help of `anything'."
  (interactive)
  (anything-help-internal
   " *Anything Help*"
   (lambda ()
     (insert (substitute-command-keys
              (anything-interpret-value (or (assoc-default
                                             'help-message
                                             (anything-get-current-source))
                                            anything-help-message))))
     (org-mode))))

(defun anything-debug-output ()
  "Show all anything-related variables at this time."
  (interactive)
  (anything-help-internal " *Anything Debug*" 'anything-debug-output-function))

(defun anything-debug-output-function (&optional vars)
  (message "Calculating all anything-related values...")
  (insert "If you debug some variables or forms, set `anything-debug-forms'
to a list of forms.\n\n")
  (dolist (v (or vars
                 anything-debug-forms
                 (apropos-internal "^anything-" 'boundp)))
    (insert "** "
            (pp-to-string v) "\n"
            (pp-to-string (with-current-buffer anything-buffer (eval v))) "\n"))
  (message "Calculating all anything-related values...Done"))


;; (@* "Core: misc")
(defun anything-kill-buffer-hook ()
  "Remove tick entry from `anything-tick-hash' when killing a buffer."
  (loop for key being the hash-keys in anything-tick-hash
        if (string-match (format "^%s/" (regexp-quote (buffer-name))) key)
        do (remhash key anything-tick-hash)))
(add-hook 'kill-buffer-hook 'anything-kill-buffer-hook)

(defun anything-preselect (candidate-or-regexp)
  "Move `anything-selection-overlay' to CANDIDATE-OR-REGEXP on startup."
  (with-anything-window
    (when candidate-or-regexp
      (goto-char (point-min))
      ;; go to first candidate of first source
      (forward-line 1)
      (let ((start (point)))
        (or (re-search-forward
             (concat "^" (regexp-quote candidate-or-regexp) "$") nil t)
            (re-search-forward candidate-or-regexp nil t)
            (search-forward candidate-or-regexp nil t)
            (goto-char start))))
    (anything-mark-current-line)))

(defun anything-delete-current-selection ()
  "Delete the currently selected item."
  (interactive)
  (with-anything-window
    (cond ((anything-pos-multiline-p)
           (anything-aif (anything-get-next-candidate-separator-pos)
               (delete-region (point-at-bol)
                              (1+ (progn (goto-char it) (point-at-eol))))
             ;; last candidate
             (goto-char (anything-get-previous-candidate-separator-pos))
             (delete-region (point-at-bol) (point-max)))
           (when (anything-end-of-source-p)
             (goto-char (or (anything-get-previous-candidate-separator-pos)
                            (point-min)))
             (forward-line 1)))
          (t
           (delete-region (point-at-bol) (1+ (point-at-eol)))
           (when (anything-end-of-source-p) (forward-line -1))))
    (anything-mark-current-line)))

(defun anything-end-of-source-p ()
  "Return non--nil if we are at eob or end of source."
  (save-excursion
    (forward-line 1)
    (or (eq (point-at-bol) (point-at-eol))
        (anything-pos-header-line-p)
        (eobp))))

(defun anything-edit-current-selection-internal (func)
  (with-anything-window
    (beginning-of-line)
    (let ((realvalue (get-text-property (point) 'anything-realvalue)))
      (funcall func)
      (beginning-of-line)
      (and realvalue
           (put-text-property (point) (point-at-eol)
                              'anything-realvalue realvalue))
      (anything-mark-current-line))))

(defmacro anything-edit-current-selection (&rest forms)
  "Evaluate FORMS at current selection in the anything buffer.
You can edit the line."
  (declare (indent 0) (debug t))
  `(anything-edit-current-selection-internal
    (lambda () ,@forms)))

(defun anything-set-pattern (pattern &optional noupdate)
  "Set minibuffer contents to PATTERN.
if optional NOUPDATE is non-nil, anything buffer is not changed."
  (with-selected-window (or (active-minibuffer-window) (minibuffer-window))
    (delete-minibuffer-contents)
    (insert pattern))
  (when noupdate
    (setq anything-pattern pattern)
    (anything-hooks 'cleanup)
    (run-with-idle-timer 0 nil 'anything-hooks 'setup)))

(defun anything-delete-minibuffer-contents ()
  "Same as `delete-minibuffer-contents' but this is a command."
  (interactive)
  (anything-set-pattern ""))
(defalias 'anything-delete-minibuffer-content 'anything-delete-minibuffer-contents)


;;; Plugins
;;
;; (@* "Built-in plug-in: type")
(defun anything-compile-source--type (source)
  (anything-aif (assoc-default 'type source)
      (append source (assoc-default it anything-type-attributes) nil)
    source))

;; `define-anything-type-attribute' is public API.

(defun anything-add-type-attribute (type definition)
  (anything-aif (assq type anything-type-attributes)
      (setq anything-type-attributes (delete it anything-type-attributes)))
  (push (cons type definition) anything-type-attributes))

(defvar anything-types nil)
(defun anything-document-type-attribute (type doc)
  (add-to-list 'anything-types type t)
  (put type 'anything-typeattrdoc
       (concat "- " (symbol-name type) "\n\n" doc "\n")))

(defadvice documentation-property (after anything-document-type-attribute activate)
  "Display type attributes' documentation as `anything-type-attributes' docstring."
  (when (eq (ad-get-arg 0) 'anything-type-attributes)
    (setq ad-return-value
          (concat ad-return-value "\n\n++++ Types currently defined ++++\n"
                  (mapconcat (lambda (sym) (get sym 'anything-typeattrdoc))
                             anything-types "\n")))))

;; (@* "Built-in plug-in: dummy")
(defun anything-dummy-candidate (candidate source)
  "Use `anything-pattern' as CANDIDATE in SOURCE."
  ;; `source' is defined in filtered-candidate-transformer
  (list anything-pattern))

(defun anything-compile-source--dummy (source)
  (if (assoc 'dummy source)
      (append source
              '((candidates "dummy")
                (accept-empty)
                (match identity)
                (filtered-candidate-transformer . anything-dummy-candidate)
                (disable-shortcuts)
                (volatile)))
      source))

;; (@* "Built-in plug-in: disable-shortcuts")
(defvar anything-orig-enable-shortcuts nil)
(defun anything-save-enable-shortcuts ()
  (anything-once
   (lambda ()
     (setq anything-orig-enable-shortcuts anything-enable-shortcuts
           anything-enable-shortcuts nil))))

(defun anything-compile-source--disable-shortcuts (source)
  (if (assoc 'disable-shortcuts source)
      (append `((init ,@(anything-mklist (assoc-default 'init source))
                      anything-save-enable-shortcuts)
                (resume ,@(anything-mklist (assoc-default 'resume source))
                        anything-save-enable-shortcuts)
                (cleanup ,@(anything-mklist (assoc-default 'cleanup source))
                         (lambda () (setq anything-enable-shortcuts
                                          anything-orig-enable-shortcuts))))
              source)
      source))

;; (@* "Built-in plug-in: candidates-in-buffer")
(defun anything-candidates-in-buffer ()
  "Get candidates from the candidates buffer according to `anything-pattern'.

BUFFER is `anything-candidate-buffer' by default.  Each
candidate must be placed in one line.  This function is meant to
be used in candidates-in-buffer or candidates attribute of an
anything source.  Especially fast for many (1000+) candidates.

eg.
 '((name . \"many files\")
   (init . (lambda () (with-current-buffer (anything-candidate-buffer 'local)
                        (insert-many-filenames))))
   (search re-search-forward)  ; optional
   (candidates-in-buffer)
   (type . file))

+===============================================================+
| The new way of making and narrowing candidates: Using buffers |
+===============================================================+

By default, `anything' makes candidates by evaluating the
candidates function, then narrows them by `string-match' for each
candidate.

But this way is very slow for many candidates. The new way is
storing all candidates in a buffer and narrowing them by
`re-search-forward'. Search function is customizable by search
attribute. The important point is that buffer processing is MUCH
FASTER than string list processing and is the Emacs way.

The init function writes all candidates to a newly-created
candidate buffer.  The candidates buffer is created or specified
by `anything-candidate-buffer'.  Candidates are stored in a line.

The candidates function narrows all candidates, IOW creates a
subset of candidates dynamically. It is the task of
`anything-candidates-in-buffer'.  As long as
`anything-candidate-buffer' is used,`(candidates-in-buffer)' is
sufficient in most cases.

Note that `(candidates-in-buffer)' is shortcut of three attributes:
  (candidates . anything-candidates-in-buffer)
  (volatile)
  (match identity)
And `(candidates-in-buffer . func)' is shortcut of three attributes:
  (candidates . func)
  (volatile)
  (match identity)
The expansion is performed in `anything-get-sources'.

The candidates-in-buffer attribute implies the volatile attribute.
The volatile attribute is needed because `anything-candidates-in-buffer'
creates candidates dynamically and need to be called everytime
`anything-pattern' changes.

Because `anything-candidates-in-buffer' plays the role of `match' attribute
function, specifying `(match identity)' makes the source slightly faster.

To customize `anything-candidates-in-buffer' behavior, use search,
get-line and search-from-end attributes. See also `anything-sources' docstring."
  (declare (special source))
  (anything-candidates-in-buffer-1
   (anything-candidate-buffer)
   anything-pattern
   (or (assoc-default 'get-line source)
       #'buffer-substring-no-properties)
   ;; use external variable `source'.
   (or (assoc-default 'search source)
       (if (assoc 'search-from-end source)
           '(anything-candidates-in-buffer-search-from-end)
           '(anything-candidates-in-buffer-search-from-start)))
   (anything-candidate-number-limit source)
   (assoc 'search-from-end source)))

(defun anything-candidates-in-buffer-search-from-start (pattern)
  "Search PATTERN with `re-search-forward' with bound and noerror args."
  (re-search-forward pattern nil t))

(defun anything-candidates-in-buffer-search-from-end (pattern)
  "Search PATTERN with `re-search-backward' with bound and noerror args."
  (re-search-backward pattern nil t))

(defun anything-candidates-in-buffer-1 (buffer pattern get-line-fn
                                        search-fns limit search-from-end)
  ;; buffer == nil when candidates buffer does not exist.
  (when buffer
    (with-current-buffer buffer
      (let ((start-point (if search-from-end (point-max) (point-min)))
            (endp (if search-from-end #'bobp #'eobp)))
        (goto-char (1- start-point))
        (if (string= pattern "")
            (anything-initial-candidates-from-candidate-buffer
             endp get-line-fn limit search-from-end)
            (anything-search-from-candidate-buffer
             pattern get-line-fn search-fns limit search-from-end
             start-point endp))))))

(defun anything-point-is-moved (proc)
  "If point is moved after executing PROC, return t, otherwise nil."
  (/= (point) (progn (funcall proc) (point))))

(defun anything-search-from-candidate-buffer (pattern get-line-fn search-fns
                                              limit search-from-end
                                              start-point endp)
  (let (buffer-read-only
        matches exit newmatches)
    (anything-search-from-candidate-buffer-internal
     (lambda ()
       (clrhash anything-cib-hash)
       (dolist (searcher search-fns)
         (goto-char start-point)
         (setq newmatches nil)
         (loop with item-count = 0
               while (funcall searcher pattern)
               for cand = (funcall get-line-fn (point-at-bol) (point-at-eol))
               do (anything-accumulate-candidates-internal
                   cand newmatches anything-cib-hash item-count limit)
               unless (anything-point-is-moved
                       (lambda ()
                         (if search-from-end
                             (goto-char (1- (point-at-bol)))
                             (forward-line 1))))
               return nil)
         (setq matches (append matches (nreverse newmatches)))
         (if exit (return)))
       (delq nil matches)))))

(defun anything-initial-candidates-from-candidate-buffer (endp get-line-fn limit search-from-end)
  (delq nil (loop with next-line-fn =
                  (if search-from-end
                      (lambda (x) (goto-char (max (1- (point-at-bol)) 1)))
                      #'forward-line)
                  until (funcall endp)
                  for i from 1 to limit
                  collect (funcall get-line-fn (point-at-bol) (point-at-eol))
                  do (funcall next-line-fn 1))))

(defun anything-search-from-candidate-buffer-internal (search-fn)
  (goto-char (point-min))
  (insert "\n")
  (goto-char (point-max))
  (insert "\n")
  (unwind-protect
       (funcall search-fn)
    (goto-char (point-min))
    (delete-char 1)
    (goto-char (1- (point-max)))
    (delete-char 1)

    (set-buffer-modified-p nil)))

(defun anything-candidate-buffer (&optional create-or-buffer)
  "Register and return a buffer containing candidates of current source.
`anything-candidate-buffer' searches buffer-local candidates buffer first,
then global candidates buffer.

Acceptable values of CREATE-OR-BUFFER:

- nil (omit)
  Only return the candidates buffer.
- a buffer
  Register a buffer as a candidates buffer.
- 'global
  Create a new global candidates buffer,
  named \" *anything candidates:SOURCE*\".
- other non-nil value
  Create a new local candidates buffer,
  named \" *anything candidates:SOURCE*ANYTHING-CURRENT-BUFFER\"."
  (let* ((global-bname (format " *anything candidates:%s*"
                               anything-source-name))
         (local-bname (format " *anything candidates:%s*%s"
                              anything-source-name
                              (buffer-name anything-current-buffer))))
    (flet ((register-func ()
             (setq anything-candidate-buffer-alist
                   (cons (cons anything-source-name create-or-buffer)
                         (delete (assoc anything-source-name
                                        anything-candidate-buffer-alist)
                                 anything-candidate-buffer-alist))))
           (kill-buffers-func ()
             (loop for b in (buffer-list)
                   if (string-match (format "^%s" (regexp-quote global-bname))
                                    (buffer-name b))
                   do (kill-buffer b)))
           (create-func ()
             (with-current-buffer
                 (get-buffer-create (if (eq create-or-buffer 'global)
                                        global-bname
                                        local-bname))
               (buffer-disable-undo)
               (erase-buffer)
               (font-lock-mode -1)))
           (return-func ()
             (or (get-buffer local-bname)
                 (get-buffer global-bname)
                 (anything-aif (assoc-default anything-source-name
                                              anything-candidate-buffer-alist)
                     (and (buffer-live-p it) it)))))
      (when create-or-buffer
        (register-func)
        (unless (bufferp create-or-buffer)
          (and (eq create-or-buffer 'global) (kill-buffers-func))
          (create-func)))
      (return-func))))

(defun anything-compile-source--candidates-in-buffer (source)
  (anything-aif (assoc 'candidates-in-buffer source)
      (append source
              `((candidates . ,(or (cdr it) 'anything-candidates-in-buffer))
                (volatile) (match identity)))
    source))


;; (@* "Utility: resplit anything window")
(defun anything-toggle-resplit-window ()
  "Toggle resplit anything window, vertically or horizontally."
  (interactive)
  (with-anything-window
    (let ((before-height (window-height)))
      (delete-window)
      (set-window-buffer
       (select-window (if (= (window-height) before-height)
                          (prog1
                              (split-window-vertically)
                            (setq anything-split-window-state 'vertical))
                          (setq anything-split-window-state 'horizontal)
                          (split-window-horizontally)))
       anything-buffer))))

;; (@* "Utility: Resize anything window.")
(defun anything-enlarge-window-1 (n)
  "Enlarge or narrow anything window.
If N is positive enlarge, if negative narrow."
  (unless anything-samewindow
    (let ((horizontal-p (eq anything-split-window-state 'horizontal)))
      (with-anything-window
        (enlarge-window n horizontal-p)))))

(defun anything-narrow-window ()
  "Narrow anything window."
  (interactive)
  (anything-enlarge-window-1 -1))

(defun anything-enlarge-window ()
  "Enlarge anything window."
  (interactive)
  (anything-enlarge-window-1 1))

;; (@* "Utility: select another action by key")
(defun anything-select-nth-action (n)
  "Select the N nth action for the currently selected candidate."
  (setq anything-saved-selection (anything-get-selection))
  (unless anything-saved-selection
    (error "Nothing is selected"))
  (setq anything-saved-action (anything-get-nth-action n (anything-get-action)))
  (anything-exit-minibuffer))

(defun anything-get-nth-action (n action)
  (cond ((and (zerop n) (functionp action))
         action)
        ((listp action)
         (or (cdr (elt action n))
             (error "No such action")))
        ((and (functionp action) (< 0 n))
         (error "Sole action"))
        (t
         (error "Error in `anything-select-nth-action'"))))

(defun anything-select-2nd-action ()
  "Select the 2nd action for the currently selected candidate."
  (interactive)
  (anything-select-nth-action 1))

(defun anything-select-3rd-action ()
  "Select the 3rd action for the currently selected candidate."
  (interactive)
  (anything-select-nth-action 2))

(defun anything-select-4th-action ()
  "Select the 4th action for the currently selected candidate."
  (interactive)
  (anything-select-nth-action 3))

(defun anything-select-2nd-action-or-end-of-line ()
  "Select the 2nd action for the currently selected candidate.
This happen when point is at the end of minibuffer.
Otherwise goto the end of minibuffer."
  (interactive)
  (if (eolp)
      (anything-select-nth-action 1)
      (end-of-line)))

;; (@* "Utility: Persistent Action")
(defmacro with-anything-display-same-window (&rest body)
  "Execute BODY in the window used for persistent action.
Make `pop-to-buffer' and `display-buffer' display in the same window."
  (declare (indent 0) (debug t))
  `(let ((display-buffer-function 'anything-persistent-action-display-buffer))
     ,@body))

(defvar anything-persistent-action-display-window nil)
(defun anything-initialize-persistent-action ()
  (set (make-local-variable 'anything-persistent-action-display-window) nil))

(defun* anything-execute-persistent-action (&optional (attr 'persistent-action) onewindow)
  "Perform the associated action ATTR without quitting anything.
ATTR default is 'persistent-action', but it can be anything else.
In this case you have to add this new attribute to your source.
When `anything-samewindow' and ONEWINDOW are non--nil,
the anything window is never split in persistent action."
  (interactive)
  (anything-log "executing persistent-action")
  (with-anything-window
    (save-selected-window
      (anything-select-persistent-action-window onewindow)
      (anything-log-eval (current-buffer))
      (let ((anything-in-persistent-action t))
        (with-anything-display-same-window
          (anything-execute-selection-action
           nil
           (or (assoc-default attr (anything-get-current-source))
               (anything-get-action))
           t)
          (anything-log-run-hook 'anything-after-persistent-action-hook))))))


(defun anything-persistent-action-display-window (&optional onewindow)
  "Return the window that will be used for presistent action.
If ONEWINDOW is non--nil window will not be splitted in persistent action
if `anything-samewindow' is non--nil also."
  (with-anything-window
    (setq anything-persistent-action-display-window
          (cond ((window-live-p anything-persistent-action-display-window)
                 anything-persistent-action-display-window)
                ((and anything-samewindow (one-window-p t) (not onewindow))
                 (split-window))
                ((get-buffer-window anything-current-buffer))
                (t
                 (next-window (selected-window) 1))))))

(defun anything-select-persistent-action-window (&optional onewindow)
  "Select the window that will be used for persistent action.
See `anything-persistent-action-display-window' for how to use ONEWINDOW."
  (select-window (get-buffer-window (anything-buffer-get)))
  (select-window
   (setq minibuffer-scroll-window
         (anything-persistent-action-display-window onewindow))))

(defun anything-persistent-action-display-buffer (buf &optional not-this-window)
  "Make `pop-to-buffer' and `display-buffer' display in the same window.
If `anything-persistent-action-use-special-display' is non-nil and
BUF is to be displayed by `special-display-function', use it.
Otherwise ignores `special-display-buffer-names' and `special-display-regexps'.
Argument NOT-THIS-WINDOW if present will be used as
second argument of `display-buffer'."
  (let* ((name (buffer-name buf))
         display-buffer-function pop-up-windows pop-up-frames
         (same-window-regexps
          (unless (and anything-persistent-action-use-special-display
                       (or (member name
                                   (mapcar (lambda (x) (or (car-safe x) x))
                                           special-display-buffer-names))
                           (remove-if-not
                            (lambda (x) (string-match (or (car-safe x) x) name))
                            special-display-regexps)))
            '("."))))
    (display-buffer buf not-this-window)))

;; scroll-other-window(-down)? for persistent-action
(defun anything-scroll-other-window-base (command)
  (with-selected-window (anything-persistent-action-display-window)
    (funcall command anything-scroll-amount)))

(defun anything-scroll-other-window ()
  "Scroll other window (not *Anything* window) upward."
  (interactive)
  (anything-scroll-other-window-base 'scroll-up))

(defun anything-scroll-other-window-down ()
  "Scroll other window (not *Anything* window) downward."
  (interactive)
  (anything-scroll-other-window-base 'scroll-down))


;; (@* "Utility: Visible Mark")
(defface anything-visible-mark
    '((((min-colors 88) (background dark))
       (:background "green1" :foreground "black"))
      (((background dark)) (:background "green" :foreground "black"))
      (((min-colors 88)) (:background "green1"))
      (t (:background "green")))
  "Face for visible mark."
  :group 'anything)

(defvar anything-visible-mark-face 'anything-visible-mark)
(defvar anything-visible-mark-overlays nil)

(defun anything-clear-visible-mark ()
  (with-current-buffer (anything-buffer-get)
    (mapc 'delete-overlay anything-visible-mark-overlays)
    (set (make-local-variable 'anything-visible-mark-overlays) nil)))
(add-hook 'anything-after-initialize-hook 'anything-clear-visible-mark)

(defvar anything-marked-candidates nil
  "Marked candadates.  List of \(source . real\) pair.")

(defun anything-this-visible-mark ()
  (loop for o in anything-visible-mark-overlays
        when (equal (point-at-bol) (overlay-start o))
        return o))

(defun anything-delete-visible-mark (overlay)
  (setq anything-marked-candidates
        (remove
         (cons (anything-get-current-source) (anything-get-selection))
         anything-marked-candidates))
  (delete-overlay overlay)
  (setq anything-visible-mark-overlays
        (delq overlay anything-visible-mark-overlays)))

(defun anything-make-visible-mark ()
  (let ((o (make-overlay (point-at-bol) (1+ (point-at-eol)))))
    (overlay-put o 'face   anything-visible-mark-face)
    (overlay-put o 'source (assoc-default 'name (anything-get-current-source)))
    (overlay-put o 'string (buffer-substring (overlay-start o) (overlay-end o)))
    (overlay-put o 'real   (anything-get-selection))
    (add-to-list 'anything-visible-mark-overlays o))
  (push (cons (anything-get-current-source) (anything-get-selection))
        anything-marked-candidates))

(defun anything-toggle-visible-mark ()
  "Toggle anything visible mark at point."
  (interactive)
  (with-anything-window
    (anything-aif (anything-this-visible-mark)
        (anything-delete-visible-mark it)
      (anything-make-visible-mark))
    (anything-next-line)))

(defun anything-display-all-visible-marks ()
  "Show all `anything' visible marks strings."
  (interactive)
  (with-anything-window
    (lexical-let ((overlays (reverse anything-visible-mark-overlays)))
      (anything-run-after-quit
       (lambda ()
         (with-output-to-temp-buffer "*anything visible marks*"
           (dolist (o overlays) (princ (overlay-get o 'string)))))))))

(defun anything-marked-candidates ()
  "Return marked candidates of current source if any.
Otherwise one element list of current selection.

It is analogous to `dired-get-marked-files'."
  (with-current-buffer (anything-buffer-get)
    (let ((cands
           (if anything-marked-candidates
               (loop with current-src = (anything-get-current-source)
                     for (source . real) in (reverse anything-marked-candidates)
                     when (equal current-src source)
                     collect (anything-coerce-selection real source))
               (list (anything-get-selection)))))
      (anything-log-eval cands)
      cands)))

(defun anything-reset-marked-candidates ()
  (with-current-buffer (anything-buffer-get)
    (set (make-local-variable 'anything-marked-candidates) nil)))

(add-hook 'anything-after-initialize-hook 'anything-reset-marked-candidates)
;; (add-hook 'anything-after-action-hook 'anything-reset-marked-candidates)

(defun anything-current-source-name= (name)
  (save-excursion
    (goto-char (anything-get-previous-header-pos))
    (equal name (anything-current-line-contents))))

(defun anything-revive-visible-mark ()
  "Restore marked candidates when anything update display."
  (with-current-buffer anything-buffer
    (dolist (o anything-visible-mark-overlays)
      (goto-char (point-min))
      (while (and (search-forward (overlay-get o 'string) nil t)
                  (anything-current-source-name= (overlay-get o 'source)))
        ;; Calculate real value of candidate.
        ;; It can be nil if candidate have only a display value.
        (let ((real (get-text-property (point-at-bol 0) 'anything-realvalue)))
          (if real
              ;; Check if real value of current candidate is the same
              ;; that the one stored in overlay.
              (and (string= (overlay-get o 'real) real)
                   (move-overlay o (point-at-bol 0) (1+ (point-at-eol 0))))
              (move-overlay o (point-at-bol 0) (1+ (point-at-eol 0)))))))))
(add-hook 'anything-update-hook 'anything-revive-visible-mark)

(defun anything-next-point-in-list (curpos points &optional prev)
  (cond
    ;; rule out special cases
    ((null points)                        curpos)
    ((and prev (< curpos (car points)))   curpos)
    ((< (car (last points)) curpos)
     (if prev (car (last points)) curpos))
    (t
     (nth (if prev
              (loop for pt in points
                    for i from 0
                    if (<= curpos pt)
                    return (1- i))
              (loop for pt in points
                    for i from 0
                    if (< curpos pt)
                    return i))
          points))))

(defun anything-next-visible-mark (&optional prev)
  "Move next anything visible mark.
If PREV is non-nil move to precedent."
  (interactive)
  (with-anything-window
    (ignore-errors
      (goto-char (anything-next-point-in-list
                  (point)
                  (sort (mapcar 'overlay-start anything-visible-mark-overlays) '<)
                  prev)))
    (anything-mark-current-line)))

(defun anything-prev-visible-mark ()
  "Move previous anything visible mark."
  (interactive)
  (anything-next-visible-mark t))

;; (@* "Utility: Selection Paste")
(defun anything-yank-selection ()
  "Set minibuffer contents to current selection."
  (interactive)
  (anything-set-pattern (anything-get-selection nil t)))

(defun anything-kill-selection-and-quit ()
  "Store current selection to kill ring.
You can paste it by typing \\[yank]."
  (interactive)
  (anything-run-after-quit
   (lambda (sel)
     (kill-new sel)
     (message "Killed: %s" sel))
   (anything-get-selection nil t)))


;; (@* "Utility: Automatical execution of persistent-action")
(add-to-list 'minor-mode-alist '(anything-follow-mode " AFollow"))
(defun anything-follow-mode ()
  "If this mode is on, persistent action is executed everytime the cursor is moved."
  (interactive)
  (with-current-buffer anything-buffer
    (setq anything-follow-mode (not anything-follow-mode))
    (message "anything-follow-mode is %s"
             (if anything-follow-mode "enabled" "disabled"))))

(defun anything-follow-execute-persistent-action-maybe ()
  "Execute persistent action in mode `anything-follow-mode'.
This happen after `anything-input-idle-delay' secs."
  (and (not (get-buffer-window anything-action-buffer 'visible))
       (buffer-local-value 'anything-follow-mode
                           (get-buffer-create anything-buffer))
       (sit-for (and anything-input-idle-delay
                     (max anything-input-idle-delay 0.1)))
       (anything-window)
       (anything-get-selection)
       (save-excursion
         (anything-execute-persistent-action))))


;; (@* "Utility: Migrate `anything-sources' to my-anything command")
(defun anything-migrate-sources ()
  "Help to migrate to new `anything' way."
  (interactive)
  (with-current-buffer (get-buffer-create "*anything migrate*")
    (erase-buffer)
    (insert (format "\
Setting `anything-sources' directly is not good because
`anything' is not for one command.  For now, interactive use of
`anything' (M-x anything) is only for demonstration purpose.
So you should define commands calling `anything'.
I help you to migrate to the new way.

The code below is automatically generated from current
`anything-sources' value. You can use the `my-anything' command
now!

Copy and paste it to your .emacs. Then substitute `my-anything'
for `anything' bindings in all `define-key', `local-set-key' and
`global-set-key' calls.

\(defun my-anything ()
  \"Anything command for you.

It is automatically generated by `anything-migrate-sources'.\"
  (interactive)
  (anything-other-buffer
    '%S
    \"*my-anything*\"))
" anything-sources))
    (eval-last-sexp nil)
    (substitute-key-definition 'anything 'my-anything global-map)
    (pop-to-buffer (current-buffer))))


;; (@* "Compatibility")

;; Copied assoc-default from XEmacs version 21.5.12
(unless (fboundp 'assoc-default)
  (defun assoc-default (key alist &optional test default)
    "Find object KEY in a pseudo-alist ALIST.
ALIST is a list of conses or objects.  Each element (or the element's car,
if it is a cons) is compared with KEY by evaluating (TEST (car elt) KEY).
If that is non-nil, the element matches;
then `assoc-default' returns the element's cdr, if it is a cons,
or DEFAULT if the element is not a cons.

If no element matches, the value is nil.
If TEST is omitted or nil, `equal' is used."
    (let (found (tail alist) value)
      (while (and tail (not found))
        (let ((elt (car tail)))
          (when (funcall (or test 'equal) (if (consp elt) (car elt) elt) key)
            (setq found t value (if (consp elt) (cdr elt) default))))
        (setq tail (cdr tail)))
      value)))

;; Function not available in XEmacs,
(unless (fboundp 'minibuffer-contents)
  (defun minibuffer-contents ()
    "Return the user input in a minbuffer as a string.
The current buffer must be a minibuffer."
    (field-string (point-max)))

  (defun delete-minibuffer-contents  ()
    "Delete all user input in a minibuffer.
The current buffer must be a minibuffer."
    (delete-field (point-max))))

;; Function not available in older Emacs (<= 22.1).
(unless (fboundp 'buffer-chars-modified-tick)
  (defun buffer-chars-modified-tick (&optional buffer)
    "Return BUFFER's character-change tick counter.
Each buffer has a character-change tick counter, which is set to the
value of the buffer's tick counter (see `buffer-modified-tick'), each
time text in that buffer is inserted or deleted.  By comparing the
values returned by two individual calls of `buffer-chars-modified-tick',
you can tell whether a character change occurred in that buffer in
between these calls.  No argument or nil as argument means use current
buffer as BUFFER."
    (with-current-buffer (or buffer (current-buffer))
      (if (listp buffer-undo-list)
          (length buffer-undo-list)
          (buffer-modified-tick)))))


;; (@* "CUA workaround")
(defadvice cua-delete-region (around anything-avoid-cua activate)
  (ignore-errors ad-do-it))

(defadvice copy-region-as-kill (around anything-avoid-cua activate)
  (if cua-mode
      (ignore-errors ad-do-it)
      ad-do-it))

;;(@* "Attribute Documentation")
(defun anything-describe-anything-attribute (anything-attribute)
  "Display the full documentation of ANYTHING-ATTRIBUTE.
ANYTHING-ATTRIBUTE should be a symbol."
  (interactive (list (intern
                      (completing-read
                       "Describe anything attribute: "
                       (mapcar 'symbol-name anything-additional-attributes)
                       nil t))))
  (with-output-to-temp-buffer "*Help*"
    (princ (get anything-attribute 'anything-attrdoc))))

(anything-document-attribute 'name "mandatory"
  "  The name of the source. It is also the heading which appears
  above the list of matches from the source. Must be unique.")

(anything-document-attribute 'header-name "optional"
  "  A function returning the display string of the header. Its
  argument is the name of the source. This attribute is useful to
  add an additional information with the source name.")

(anything-document-attribute 'candidates "mandatory if candidates-in-buffer attribute is not provided"
  "  Specifies how to retrieve candidates from the source. It can
  either be a variable name, a function called with no parameters
  or the actual list of candidates.

  The list must be a list whose members are strings, symbols
  or (DISPLAY . REAL) pairs.

  In case of (DISPLAY . REAL) pairs, the DISPLAY string is shown
  in the Anything buffer, but the REAL one is used as action
  argument when the candidate is selected. This allows a more
  readable presentation for candidates which would otherwise be,
  for example, too long or have a common part shared with other
  candidates which can be safely replaced with an abbreviated
  string for display purposes.

  Note that if the (DISPLAY . REAL) form is used then pattern
  matching is done on the displayed string, not on the real
  value.

  If the candidates have to be retrieved asynchronously (for
  example, by an external command which takes a while to run)
  then the function should start the external command
  asynchronously and return the associated process object.
  Anything will take care of managing the process (receiving the
  output from it, killing it if necessary, etc.). The process
  should return candidates matching the current pattern (see
  variable `anything-pattern'.)

  Note that currently results from asynchronous sources appear
  last in the anything buffer regardless of their position in
  `anything-sources'.")

(anything-document-attribute 'action "mandatory if type attribute is not provided"
  "  It is a list of (DISPLAY . FUNCTION) pairs or FUNCTION.
  FUNCTION is called with one parameter: the selected candidate.

  An action other than the default can be chosen from this list
  of actions for the currently selected candidate (by default
  with TAB). The DISPLAY string is shown in the completions
  buffer and the FUNCTION is invoked when an action is
  selected. The first action of the list is the default.")

(anything-document-attribute 'coerce "optional"
  "  It's a function called with one argument: the selected candidate.

  This function is intended for type convertion.
  In normal case, the selected candidate (string) is passed to action function.
  If coerce function is specified, it is called just before action function.

  Example: converting string to symbol
    (coerce . intern)")

(anything-document-attribute 'type "optional if action attribute is provided"
  "  Indicates the type of the items the source returns.

  Merge attributes not specified in the source itself from
  `anything-type-attributes'.

  This attribute is implemented by plug-in.")

(anything-document-attribute 'init "optional"
  "  Function called with no parameters when anything is started. It
  is useful for collecting current state information which can be
  used to create the list of candidates later.

  For example, if a source needs to work with the current
  directory then it can store its value here, because later
  anything does its job in the minibuffer and in the
  `anything-buffer' and the current directory can be different
  there.")

(anything-document-attribute 'delayed-init "optional"
  "  Function called with no parameters before candidate function is
  called.  It is similar with `init' attribute, but its
  evaluation is deferred. It is useful to combine with ")

(anything-document-attribute 'match "optional"
  "  List of functions called with one parameter: a candidate. The
  function should return non-nil if the candidate matches the
  current pattern (see variable `anything-pattern').

  This attribute allows the source to override the default
  pattern matching based on `string-match'. It can be used, for
  example, to implement a source for file names and do the
  pattern matching on the basename of files, since it's more
  likely one is typing part of the basename when searching for a
  file, instead of some string anywhere else in its path.

  If the list contains more than one function then the list of
  matching candidates from the source is constructed by appending
  the results after invoking the first function on all the
  potential candidates, then the next function, and so on. The
  matching candidates supplied by the first function appear first
  in the list of results and then results from the other
  functions, respectively.

  This attribute has no effect for asynchronous sources (see
  attribute `candidates'), since they perform pattern matching
  themselves.")

(anything-document-attribute 'candidate-transformer "optional"
  "  It's a function or a list of functions called with one argument
  when the completion list from the source is built. The argument
  is the list of candidates retrieved from the source. The
  function should return a transformed list of candidates which
  will be used for the actual completion.  If it is a list of
  functions, it calls each function sequentially.

  This can be used to transform or remove items from the list of
  candidates.

  Note that `candidates' is run already, so the given transformer
  function should also be able to handle candidates with (DISPLAY
  . REAL) format.")

(anything-document-attribute 'filtered-candidate-transformer "optional"
  "  It has the same format as `candidate-transformer', except the
  function is called with two parameters: the candidate list and
  the source.

  This transformer is run on the candidate list which is already
  filtered by the current pattern. While `candidate-transformer'
  is run only once, it is run every time the input pattern is
  changed.

  It can be used to transform the candidate list dynamically, for
  example, based on the current pattern.

  In some cases it may also be more efficent to perform candidate
  transformation here, instead of with `candidate-transformer'
  even if this transformation is done every time the pattern is
  changed.  For example, if a candidate set is very large then
  `candidate-transformer' transforms every candidate while only
  some of them will actually be dislpayed due to the limit
  imposed by `anything-candidate-number-limit'.

  Note that `candidates' and `candidate-transformer' is run
  already, so the given transformer function should also be able
  to handle candidates with (DISPLAY . REAL) format.

  This option has no effect for asynchronous sources. (Not yet,
  at least.")

(anything-document-attribute 'action-transformer "optional"
  "  It's a function or a list of functions called with two
  arguments when the action list from the source is
  assembled. The first argument is the list of actions, the
  second is the current selection.  If it is a list of functions,
  it calls each function sequentially.

  The function should return a transformed action list.

  This can be used to customize the list of actions based on the
  currently selected candidate.")

(anything-document-attribute 'pattern-transformer "optional"
  "  It's a function or a list of functions called with one argument
  before computing matches. Its argument is `anything-pattern'.
  Functions should return transformed `anything-pattern'.

  It is useful to change interpretation of `anything-pattern'.")

(anything-document-attribute 'delayed "optional"
  "  Candidates from the source are shown only if the user stops
  typing and is idle for `anything-idle-delay' seconds.")

(anything-document-attribute 'volatile "optional"
  "  Indicates the source assembles the candidate list dynamically,
  so it shouldn't be cached within a single Anything
  invocation. It is only applicable to synchronous sources,
  because asynchronous sources are not cached.")

(anything-document-attribute 'requires-pattern "optional"
  "  If present matches from the source are shown only if the
  pattern is not empty. Optionally, it can have an integer
  parameter specifying the required length of input which is
  useful in case of sources with lots of candidates.")

(anything-document-attribute 'persistent-action "optional"
  "  Function called with one parameter; the selected candidate.

  An action performed by `anything-execute-persistent-action'.
  If none, use the default action.")

(anything-document-attribute 'candidates-in-buffer "optional"
  "  Shortcut attribute for making and narrowing candidates using
  buffers.  This newly-introduced attribute prevents us from
  forgetting to add volatile and match attributes.

  See docstring of `anything-candidates-in-buffer'.

  (candidates-in-buffer) is equivalent of three attributes:
    (candidates . anything-candidates-in-buffer)
    (volatile)
    (match identity)

  (candidates-in-buffer . candidates-function) is equivalent of:
    (candidates . candidates-function)
    (volatile)
    (match identity)

  This attribute is implemented by plug-in.")

(anything-document-attribute 'search "optional"
  "  List of functions like `re-search-forward' or `search-forward'.
  Buffer search function used by `anything-candidates-in-buffer'.
  By default, `anything-candidates-in-buffer' uses `re-search-forward'.
  This attribute is meant to be used with
  (candidates . anything-candidates-in-buffer) or
  (candidates-in-buffer) in short.")

(anything-document-attribute 'search-from-end "optional"
  "  Make `anything-candidates-in-buffer' search from the end of buffer.
  If this attribute is specified, `anything-candidates-in-buffer' uses
  `re-search-backward' instead.")

(anything-document-attribute 'get-line "optional"
  "  A function like `buffer-substring-no-properties' or `buffer-substring'.
  This function converts point of line-beginning and point of line-end,
  which represents a candidate computed by `anything-candidates-in-buffer'.
  By default, `anything-candidates-in-buffer' uses
  `buffer-substring-no-properties'.")

(anything-document-attribute 'display-to-real "optional"
  "  Function called with one parameter; the selected candidate.

  The function transforms the selected candidate, and the result
  is passed to the action function.  The display-to-real
  attribute provides another way to pass other string than one
  shown in Anything buffer.

  Traditionally, it is possible to make candidates,
  candidate-transformer or filtered-candidate-transformer
  function return a list with (DISPLAY . REAL) pairs. But if REAL
  can be generated from DISPLAY, display-to-real is more
  convenient and faster.")

(anything-document-attribute 'real-to-display "optional"
  "  Function called with one parameter; the selected candidate.

  The inverse of display-to-real attribute.

  The function transforms the selected candidate, which is passed
  to the action function, for display.  The real-to-display
  attribute provides the other way to pass other string than one
  shown in Anything buffer.

  Traditionally, it is possible to make candidates,
  candidate-transformer or filtered-candidate-transformer
  function return a list with (DISPLAY . REAL) pairs. But if
  DISPLAY can be generated from REAL, real-to-display is more
  convenient.

  Note that DISPLAY parts returned from candidates /
  candidate-transformer are IGNORED as the name `display-to-real'
  says.")

(anything-document-attribute 'cleanup "optional"
  "  Function called with no parameters when *anything* buffer is closed. It
  is useful for killing unneeded candidates buffer.

  Note that the function is executed BEFORE performing action.")

(anything-document-attribute 'candidate-number-limit "optional"
  "  Override `anything-candidate-number-limit' only for this source.")

(anything-document-attribute 'accept-empty "optional"
  "  Pass empty string \"\" to action function.")

(anything-document-attribute 'disable-shortcuts "optional"
  "  Disable `anything-enable-shortcuts' in current `anything' session.

  This attribute is implemented by plug-in.")

(anything-document-attribute 'dummy "optional"
  "  Set `anything-pattern' to candidate. If this attribute is
  specified, The candidates attribute is ignored.

  This attribute is implemented by plug-in.
  This plug-in implies disable-shortcuts plug-in.")

(anything-document-attribute 'multiline "optional"
  "  Enable to selection multiline candidates.")

(anything-document-attribute 'update "optional"
  (substitute-command-keys
   "  Function called with no parameters when \
\\<anything-map>\\[anything-force-update] is pressed."))

(anything-document-attribute 'mode-line "optional"
  "  source local `anything-mode-line-string'. (included in `mode-line-format')
  It accepts also variable/function name.")

(anything-document-attribute 'header-line "optional"
  "  source local `header-line-format'.
  It accepts also variable/function name. ")

(anything-document-attribute
 'resume "optional"
 "  Function called with no parameters when `anything-resume' is started.")

(anything-document-attribute 'keymap "optional"
  "  Specific keymap for this source.
  It is useful to have a keymap per source when using more than one source.
  Otherwise, a keymap can be set per command with `anything' argument KEYMAP.
  NOTE: when a source have `anything-map' as keymap attr,
  the global value of `anything-map' will override the actual local one.")

(anything-document-attribute 'help-message "optional"
  "  Help message for this source.
  If not present, `anything-help-message' value will be used.")


;; (@* "Bug Report")
(defvar anything-maintainer-mail-address "emacs-anything@googlegroups.com")

(defvar anything-bug-report-salutation
  "Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of anything.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"anything.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.")

(defvar anything-no-dump-variables
  '(anything-candidate-buffer-alist
    anything-digit-overlays
    anything-help-message
    anything-candidate-cache
    )
  "Variables not to dump in bug report.")

(defun anything-dumped-variables-in-bug-report ()
  (let ((hash (make-hash-table)))
    (loop for var in (apropos-internal "anything-" 'boundp)
          for vname = (symbol-name var)
          unless (or (string-match "-map$" vname)
                     (string-match "^anything-c-source-" vname)
                     (string-match "-hash$" vname)
                     (string-match "-face$" vname)
                     (memq var anything-no-dump-variables))
          collect var)))

(defun anything-send-bug-report ()
  "Send a bug report of anything.el."
  (interactive)
  (with-current-buffer (or anything-last-buffer
                           (current-buffer))
    (reporter-submit-bug-report
     anything-maintainer-mail-address
     "anything.el"
     (anything-dumped-variables-in-bug-report)
     nil nil
     anything-bug-report-salutation)))

(defun anything-send-bug-report-from-anything ()
  "Send a bug report of anything.el in anything session."
  (interactive)
  (anything-run-after-quit 'anything-send-bug-report))

;; Debugging function.
(defun* anything-test-candidates
    (sources &optional (input "")
             (compile-source-functions
              anything-compile-source-functions-default))
  "Test helper function for anything.
Given pseudo `anything-sources' and `anything-pattern', returns list like
  ((\"source name1\" (\"candidate1\" \"candidate2\"))
   (\"source name2\" (\"candidate3\" \"candidate4\")))"
  (let ((anything-test-mode t)
        anything-enable-shortcuts
        anything-candidate-cache
        (anything-compile-source-functions compile-source-functions)
        anything-before-initialize-hook
        anything-after-initialize-hook
        anything-update-hook
        anything-test-candidate-list)
    (get-buffer-create anything-buffer)
    (anything-initialize nil input sources)
    (anything-update)
    ;; test-mode spec: select 1st candidate!
    (with-current-buffer anything-buffer
      (forward-line 1)
      (anything-mark-current-line))
    (prog1
        anything-test-candidate-list
      (anything-cleanup))))


;; (@* "Unit Tests")
;; See developer-tools/unit-test-anything.el

(provide 'anything)

;; Local Variables:
;; coding: utf-8
;; End:

;;; anything.el ends here
