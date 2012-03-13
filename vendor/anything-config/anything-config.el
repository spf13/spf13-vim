;;; anything-config.el --- Applications libary for `anything.el'

;; Filename: anything-config.el

;; Description: Applications libary for `anything.el'

;; Author: Tassilo Horn <tassilo@member.fsf.org>

;; Maintainer: Tassilo Horn <tassilo@member.fsf.org>
;;             rubikitch    <rubikitch@ruby-lang.org>
;;             Thierry Volpiatto <thierry.volpiatto@gmail.com>
;; Copyright (C) 2007 ~ 2011, Tassilo Horn, all rights reserved.
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Copyright (C) 2009 ~ 2012, rubikitch, all rights reserved.
;; Copyright (C) 2009 ~ 2012, Thierry Volpiatto, all rights reserved.

;; Created: 2009-02-16 21:38:23

;; X-URL: <http://repo.or.cz/w/anything-config.git>

;; MailingList: <https://groups.google.com/group/emacs-anything?hl=en>

;; Keywords: anything, anything-config

;; Compatibility: GNU Emacs 22 ~ 24

;; Dependencies: `anything.el', `anything-match-plugin.el'.

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Predefined configurations for `anything.el'
;;
;; For quick start, try `anything-for-files' to open files.
;;
;; To configure anything you should define anything command
;; with your favorite sources, like below:
;;
;; (defun my-anything ()
;;   (interactive)
;;   (anything-other-buffer
;;    '(anything-c-source-buffers
;;      anything-c-source-file-name-history
;;      anything-c-source-info-pages
;;      anything-c-source-info-elisp
;;      anything-c-source-man-pages
;;      anything-c-source-locate
;;      anything-c-source-emacs-commands)
;;    " *my-anything*"))
;;
;; Then type M-x my-anything to use sources.
;;
;; Defining own command is better than setup `anything-sources'
;; directly, because you can define multiple anything commands with
;; different sources. Each anything command should have own anything
;; buffer, because M-x anything-resume revives anything command.

;; NOTE: What you find on Emacswiki is mostly deprecated and not maintained,
;;       don't complain if you use such code or configuration and something
;;       doesn't work.


;;; Autodoc documentation:
;;  ---------------------

;;  * Commands defined here are:
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "anything-" :docstring t)
;; `anything-configuration'
;; Customize `anything'.
;; `anything-c-buffer-help'
;; Help command for anything buffers.
;; `anything-ff-help'
;; Help command for `anything-find-files'.
;; `anything-read-file-name-help'
;; Not documented.
;; `anything-generic-file-help'
;; Not documented.
;; `anything-grep-help'
;; Not documented.
;; `anything-pdfgrep-help'
;; Not documented.
;; `anything-etags-help'
;; The help function for etags.
;; `anything-c-ucs-help'
;; Help command for `anything-ucs'.
;; `anything-c-bookmark-help'
;; Help command for bookmarks.
;; `anything-show-this-source-only'
;; Show all candidates of this source.
;; `anything-test-sources'
;; List all anything sources for test.
;; `anything-select-source'
;; [OBSOLETE] Select source.
;; `anything-insert-buffer-name'
;; Insert buffer name.
;; `anything-quit-and-find-file'
;; Drop into `anything-find-files' from `anything'.
;; `anything-mark-all'
;; Mark all visible unmarked candidates in current source.
;; `anything-unmark-all'
;; Unmark all candidates in all sources of current anything session.
;; `anything-toggle-all-marks'
;; Toggle all marks.
;; `anything-buffer-diff-persistent'
;; Toggle diff buffer without quitting anything.
;; `anything-buffer-revert-persistent'
;; Revert buffer without quitting anything.
;; `anything-buffer-save-persistent'
;; Save buffer without quitting anything.
;; `anything-buffer-run-kill-buffers'
;; Run kill buffer action from `anything-c-source-buffers-list'.
;; `anything-buffer-run-grep'
;; Run Grep action from `anything-c-source-buffers-list'.
;; `anything-buffer-run-zgrep'
;; Run Grep action from `anything-c-source-buffers-list'.
;; `anything-buffer-run-query-replace-regexp'
;; Run Query replace regexp action from `anything-c-source-buffers-list'.
;; `anything-buffer-run-query-replace'
;; Run Query replace action from `anything-c-source-buffers-list'.
;; `anything-buffer-switch-other-window'
;; Run switch to other window action from `anything-c-source-buffers-list'.
;; `anything-buffer-switch-other-frame'
;; Run switch to other frame action from `anything-c-source-buffers-list'.
;; `anything-buffer-switch-to-elscreen'
;; Run switch to elscreen  action from `anything-c-source-buffers-list'.
;; `anything-buffer-run-ediff'
;; Run ediff action from `anything-c-source-buffers-list'.
;; `anything-buffer-run-ediff-merge'
;; Run ediff action from `anything-c-source-buffers-list'.
;; `anything-ff-run-toggle-auto-update'
;; Not documented.
;; `anything-ff-run-switch-to-history'
;; Run Switch to history action from `anything-c-source-find-files'.
;; `anything-ff-run-grep'
;; Run Grep action from `anything-c-source-find-files'.
;; `anything-ff-run-pdfgrep'
;; Run Pdfgrep action from `anything-c-source-find-files'.
;; `anything-ff-run-zgrep'
;; Run Grep action from `anything-c-source-find-files'.
;; `anything-ff-run-copy-file'
;; Run Copy file action from `anything-c-source-find-files'.
;; `anything-ff-run-rename-file'
;; Run Rename file action from `anything-c-source-find-files'.
;; `anything-ff-run-byte-compile-file'
;; Run Byte compile file action from `anything-c-source-find-files'.
;; `anything-ff-run-load-file'
;; Run Load file action from `anything-c-source-find-files'.
;; `anything-ff-run-eshell-command-on-file'
;; Run eshell command on file action from `anything-c-source-find-files'.
;; `anything-ff-run-ediff-file'
;; Run Ediff file action from `anything-c-source-find-files'.
;; `anything-ff-run-ediff-merge-file'
;; Run Ediff merge file action from `anything-c-source-find-files'.
;; `anything-ff-run-symlink-file'
;; Run Symlink file action from `anything-c-source-find-files'.
;; `anything-ff-run-hardlink-file'
;; Run Hardlink file action from `anything-c-source-find-files'.
;; `anything-ff-run-delete-file'
;; Run Delete file action from `anything-c-source-find-files'.
;; `anything-ff-run-complete-fn-at-point'
;; Run complete file name action from `anything-c-source-find-files'.
;; `anything-ff-run-switch-to-eshell'
;; Run switch to eshell action from `anything-c-source-find-files'.
;; `anything-ff-run-switch-other-window'
;; Run switch to other window action from `anything-c-source-find-files'.
;; `anything-ff-run-switch-other-frame'
;; Run switch to other frame action from `anything-c-source-find-files'.
;; `anything-ff-run-open-file-externally'
;; Run open file externally command action from `anything-c-source-find-files'.
;; `anything-ff-run-locate'
;; Run locate action from `anything-c-source-find-files'.
;; `anything-ff-run-gnus-attach-files'
;; Run gnus attach files command action from `anything-c-source-find-files'.
;; `anything-ff-run-etags'
;; Run Etags command action from `anything-c-source-find-files'.
;; `anything-ff-run-print-file'
;; Run Print file action from `anything-c-source-find-files'.
;; `anything-ff-run-toggle-basename'
;; Not documented.
;; `anything-find-files-down-one-level'
;; Go down one level like unix command `cd ..'.
;; `anything-ff-properties-persistent'
;; Show properties without quitting anything.
;; `anything-ff-persistent-delete'
;; Delete current candidate without quitting.
;; `anything-ff-run-kill-buffer-persistent'
;; Execute `anything-ff-kill-buffer-fname' whitout quitting.
;; `anything-ff-rotate-left-persistent'
;; Rotate image left without quitting anything.
;; `anything-ff-rotate-right-persistent'
;; Rotate image right without quitting anything.
;; `anything-c-goto-precedent-file'
;; Go to precedent file in anything grep/etags buffers.
;; `anything-c-goto-next-file'
;; Go to precedent file in anything grep/etags buffers.
;; `anything-c-grep-run-persistent-action'
;; Run grep persistent action from `anything-do-grep-1'.
;; `anything-c-grep-run-default-action'
;; Run grep default action from `anything-do-grep-1'.
;; `anything-c-grep-run-other-window-action'
;; Run grep goto other window action from `anything-do-grep-1'.
;; `anything-c-grep-run-save-buffer'
;; Run grep save results action from `anything-do-grep-1'.
;; `anything-yank-text-at-point'
;; Yank text at point in minibuffer.
;; `anything-c-bookmark-run-jump-other-window'
;; Jump to bookmark from keyboard.
;; `anything-c-bookmark-run-delete'
;; Delete bookmark from keyboard.
;; `anything-c-bmkext-run-edit'
;; Run `bmkext-edit-bookmark' from keyboard.
;; `anything-yaoddmuse-cache-pages'
;; Fetch the list of files on emacswiki and create cache file.
;; `anything-eval-new-line-and-indent'
;; Not documented.
;; `anything-call-source-from-anything'
;; Call anything source within `anything' session.
;; `anything-create-from-anything'
;; Run `anything-create' from `anything' as a fallback.
;; `anything-c-ucs-persistent-insert'
;; Not documented.
;; `anything-c-ucs-persistent-forward'
;; Not documented.
;; `anything-c-ucs-persistent-backward'
;; Not documented.
;; `anything-c-ucs-persistent-delete'
;; Not documented.
;; `anything-lisp-completion-at-point'
;; Anything lisp symbol completion at point.
;; `anything-c-complete-file-name-at-point'
;; Complete file name at point.
;; `anything-lisp-completion-at-point-or-indent'
;; First call indent and second call complete lisp symbol.
;; `anything-lisp-completion-or-file-name-at-point'
;; Complete lisp symbol or filename at point.
;; `anything-w32-shell-execute-open-file'
;; Not documented.
;; `anything-c-set-variable'
;; Set value to VAR interactively.
;; `anything-c-adaptive-save-history'
;; Save history information to file given by `anything-c-adaptive-history-file'.
;; `anything-c-reset-adaptative-history'
;; Delete all `anything-c-adaptive-history' and his file.
;; `anything-mini'
;; Preconfigured `anything' lightweight version (buffer -> recentf).
;; `anything-for-files'
;; Preconfigured `anything' for opening files.
;; `anything-recentf'
;; Preconfigured `anything' for `recentf'.
;; `anything-info-at-point'
;; Preconfigured `anything' for searching info at point.
;; `anything-show-kill-ring'
;; Preconfigured `anything' for `kill-ring'.
;; `anything-minibuffer-history'
;; Preconfigured `anything' for `minibuffer-history'.
;; `anything-gentoo'
;; Preconfigured `anything' for gentoo linux.
;; `anything-imenu'
;; Preconfigured `anything' for `imenu'.
;; `anything-google-suggest'
;; Preconfigured `anything' for google search with google suggest.
;; `anything-yahoo-suggest'
;; Preconfigured `anything' for Yahoo searching with Yahoo suggest.
;; `anything-for-buffers'
;; Preconfigured `anything' for buffers.
;; `anything-buffers-list'
;; Preconfigured `anything' to list buffers.
;; `anything-bbdb'
;; Preconfigured `anything' for BBDB.
;; `anything-locate'
;; Preconfigured `anything' for Locate.
;; `anything-w3m-bookmarks'
;; Preconfigured `anything' for w3m bookmark.
;; `anything-firefox-bookmarks'
;; Preconfigured `anything' for firefox bookmark.
;; `anything-colors'
;; Preconfigured `anything' for color.
;; `anything-bookmarks'
;; Preconfigured `anything' for bookmarks.
;; `anything-c-pp-bookmarks'
;; Preconfigured `anything' for bookmarks	(pretty-printed).
;; `anything-c-insert-latex-math'
;; Preconfigured anything for latex math symbols completion.
;; `anything-register'
;; Preconfigured `anything' for Emacs registers.
;; `anything-man-woman'
;; Preconfigured `anything' for Man and Woman pages.
;; `anything-org-keywords'
;; Preconfigured `anything' for org keywords.
;; `anything-emms'
;; Preconfigured `anything' for emms sources.
;; `anything-eev-anchors'
;; Preconfigured `anything' for eev anchors.
;; `anything-bm-list'
;; Preconfigured `anything' for visible bookmarks.
;; `anything-timers'
;; Preconfigured `anything' for timers.
;; `anything-list-emacs-process'
;; Preconfigured `anything' for emacs process.
;; `anything-occur'
;; Preconfigured Anything for Occur source.
;; `anything-browse-code'
;; Preconfigured anything to browse code.
;; `anything-org-headlines'
;; Preconfigured anything to show org headlines.
;; `anything-regexp'
;; Preconfigured anything to build regexps.
;; `anything-c-copy-files-async'
;; Preconfigured anything to copy file list FLIST to DEST asynchronously.
;; `anything-find-files'
;; Preconfigured `anything' for anything implementation of `find-file'.
;; `anything-write-file'
;; Preconfigured `anything' providing completion for `write-file'.
;; `anything-insert-file'
;; Preconfigured `anything' providing completion for `insert-file'.
;; `anything-dired-rename-file'
;; Preconfigured `anything' to rename files from dired.
;; `anything-dired-copy-file'
;; Preconfigured `anything' to copy files from dired.
;; `anything-dired-symlink-file'
;; Preconfigured `anything' to symlink files from dired.
;; `anything-dired-hardlink-file'
;; Preconfigured `anything' to hardlink files from dired.
;; `anything-do-grep'
;; Preconfigured anything for grep.
;; `anything-do-pdfgrep'
;; Preconfigured anything for pdfgrep.
;; `anything-c-etags-select'
;; Preconfigured anything for etags.
;; `anything-filelist'
;; Preconfigured `anything' to open files instantly.
;; `anything-filelist+'
;; Preconfigured `anything' to open files/buffers/bookmarks instantly.
;; `anything-M-x'
;; Preconfigured `anything' for Emacs commands.
;; `anything-manage-advice'
;; Preconfigured `anything' to disable/enable function advices.
;; `anything-bookmark-ext'
;; Preconfigured `anything' for bookmark-extensions sources.
;; `anything-simple-call-tree'
;; Preconfigured `anything' for simple-call-tree. List function relationships.
;; `anything-mark-ring'
;; Preconfigured `anything' for `anything-c-source-mark-ring'.
;; `anything-global-mark-ring'
;; Preconfigured `anything' for `anything-c-source-global-mark-ring'.
;; `anything-all-mark-rings'
;; Preconfigured `anything' for `anything-c-source-global-mark-ring' and `anything-c-source-mark-ring'.
;; `anything-yaoddmuse-emacswiki-edit-or-view'
;; Preconfigured `anything' to edit or view EmacsWiki page.
;; `anything-yaoddmuse-emacswiki-post-library'
;; Preconfigured `anything' to post library to EmacsWiki.
;; `anything-eval-expression'
;; Preconfigured anything for `anything-c-source-evaluation-result'.
;; `anything-eval-expression-with-eldoc'
;; Preconfigured anything for `anything-c-source-evaluation-result' with `eldoc' support. 
;; `anything-calcul-expression'
;; Preconfigured anything for `anything-c-source-calculation-result'.
;; `anything-surfraw'
;; Preconfigured `anything' to search PATTERN with search ENGINE.
;; `anything-call-source'
;; Preconfigured `anything' to call anything source.
;; `anything-execute-anything-command'
;; Preconfigured `anything' to execute preconfigured `anything'.
;; `anything-create'
;; Preconfigured `anything' to do many create actions from STRING.
;; `anything-top'
;; Preconfigured `anything' for top command.
;; `anything-select-xfont'
;; Preconfigured `anything' to select Xfont.
;; `anything-world-time'
;; Preconfigured `anything' to show world time.
;; `anything-apt'
;; Preconfigured `anything' : frontend of APT package manager.
;; `anything-esh-pcomplete'
;; Preconfigured anything to provide anything completion in eshell.
;; `anything-eshell-history'
;; Preconfigured anything for eshell history.
;; `anything-c-run-external-command'
;; Preconfigured `anything' to run External PROGRAM asyncronously from Emacs.
;; `anything-ratpoison-commands'
;; Preconfigured `anything' to execute ratpoison commands.
;; `anything-ucs'
;; Preconfigured anything for `ucs-names' math symbols.
;; `anything-c-apropos'
;; Preconfigured anything to describe commands, functions, variables and faces.
;; `anything-xrandr-set'
;; Not documented.

;;  * User variables defined here:
;; [EVAL] (autodoc-document-lisp-buffer :type 'user-variable :prefix "anything-" :var-value t)
;; `anything-c-adaptive-history-file'
;; Default Value: "~/.emacs.d/anything-c-adaptive-history"
;; `anything-c-adaptive-history-length'
;; Default Value: 50
;; `anything-c-google-suggest-url'
;; Default Value: "http://google.com/complete/search?output=toolbar&q="
;; `anything-c-google-suggest-search-url'
;; Default Value: "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
;; `anything-google-suggest-use-curl-p'
;; Default Value: nil
;; `anything-c-yahoo-suggest-url'
;; Default Value: "http://search.yahooapis.com/WebSearchService/V1/relatedSuggestion?appid=G [...]
;; `anything-c-yahoo-suggest-search-url'
;; Default Value: "http://search.yahoo.com/search?&ei=UTF-8&fr&h=c&p="
;; `anything-c-boring-buffer-regexp'
;; Default Value: "\\	(\\` \\)\\|\\*anything\\|\\*ac-mode\\| \\*Echo Area\\| \\*Minibuf"
;; `anything-c-boring-file-regexp'
;; Default Value: "/\\	(?:\\(?:\\.\\(?:git\\|hg\\|svn\\)\\|CVS\\|_darcs\\)\\)\\(?:/\\|$\\)\\| [...]
;; `anything-kill-ring-threshold'
;; Default Value: 10
;; `anything-c-kill-ring-max-lines-number'
;; Default Value: nil
;; `anything-su-or-sudo'
;; Default Value: "su"
;; `anything-for-files-prefered-list'
;; Default Value:	(anything-c-source-ffap-line anything-c-source-ffap-guesser anything-c-sou [...]
;; `anything-create--actions-private'
;; Default Value: nil
;; `anything-allow-skipping-current-buffer'
;; Default Value: nil
;; `anything-c-enable-eval-defun-hack'
;; Default Value: t
;; `anything-tramp-verbose'
;; Default Value: 0
;; `anything-raise-command'
;; Default Value: nil
;; `anything-command-map-prefix-key'
;; Default Value: "<f5> a"
;; `anything-c-browse-code-regexp-lisp'
;; Default Value: "^ *	(def\\(un\\|subst\\|macro\\|face\\|alias\\|advice\\|struct\\|type\\|th [...]
;; `anything-c-browse-code-regexp-python'
;; Default Value: "\\<def\\>\\|\\<class\\>"
;; `anything-c-browse-code-regexp-alist'
;; Default Value:	((lisp-interaction-mode . "^ *(def\\(un\\|subst\\|macro\\|face\\|alias\\|a [...]
;; `anything-c-external-programs-associations'
;; Default Value: nil
;; `anything-ff-auto-update-initial-value'
;; Default Value: t
;; `anything-c-copy-async-prefered-emacs'
;; Default Value: "emacs"
;; `anything-ff-lynx-style-map'
;; Default Value: t
;; `anything-ff-history-max-length'
;; Default Value: 100
;; `anything-ff-smart-completion'
;; Default Value: t
;; `anything-ff-default-kbsize'
;; Default Value: 1024.0
;; `anything-ff-tramp-not-fancy'
;; Default Value: t
;; `anything-ff-exif-data-program'
;; Default Value: "exiftran"
;; `anything-ff-exif-data-program-args'
;; Default Value: "-d"
;; `anything-c-grep-use-ioccur-style-keys'
;; Default Value: t
;; `anything-c-pdfgrep-default-read-command'
;; Default Value: "xpdf '%f' %p"
;; `anything-c-etags-tag-file-name'
;; Default Value: "TAGS"
;; `anything-c-etags-tag-file-search-limit'
;; Default Value: 10
;; `anything-c-etags-use-regexp-search'
;; Default Value: nil
;; `anything-c-etags-search-regexp'
;; Default Value: "^.+: .+ \\<%s"
;; `anything-c-filelist-file-name'
;; Default Value: nil
;; `anything-c-eldoc-in-minibuffer-show-fn'
;; Default Value: anything-c-show-info-in-mode-line
;; `anything-c-turn-on-show-completion'
;; Default Value: t
;; `anything-c-show-completion-use-special-display'
;; Default Value: t
;; `anything-c-show-completion-min-window-height'
;; Default Value: 7
;; `anything-lisp-completion-or-indent-delay'
;; Default Value: 0.6
;; `anything-c-default-external-file-browser'
;; Default Value: "nautilus"
;; `anything-c-use-adaptative-sorting'
;; Default Value: nil
;; `anything-ff-newfile-prompt-p'
;; Default Value: t
;; `anything-ff-avfs-directory'
;; Default Value: nil
;; `anything-ff-file-compressed-list'
;; Default Value:	("gz" "bz2" "zip" "7z") 
;; `anything-locate-db-file-regexp'
;; Default Value: "m?locate.db$"
;; `anything-c-locate-command'
;; Default Value: nil
;; `anything-c-show-info-in-mode-line-delay'
;; Default Value: 12
;; `anything-c-copy-files-async-log-file'
;; Default Value: "/tmp/dired.log"
;; `anything-ff-printer-list'
;; Default Value: nil
;; `anything-ff-transformer-show-only-basename'
;; Default Value: nil
;; `anything-ff-quick-delete-dont-prompt-for-deletion'
;; Default Value: nil
;; `anything-ff-signal-error-on-dot-files'
;; Default Value: t
;; `anything-completing-read-handlers-alist'
;; Default Value:	((describe-function . anything-completing-read-symbols)  (describe-variabl [...]

;;  * Anything sources defined here:
;; [EVAL] (autodoc-document-lisp-buffer :type 'anything-source :prefix "anything-" :any-sname t)
;; `anything-c-source-regexp'					(Regexp Builder)
;; `anything-c-source-buffers'					(Buffers)
;; `anything-c-source-buffer-not-found'				(Create buffer)
;; `anything-c-source-buffers-list'				(Buffers)
;; `anything-c-source-file-name-history'			(File Name History)
;; `anything-c-source-files-in-current-dir'			(Files from Current Directory)
;; `anything-c-source-files-in-current-dir+'			(Files from Current Directory)
;; `anything-c-source-find-files'				(Find Files)
;; `anything-c-source-write-file'				(Write File)
;; `anything-c-source-insert-file'				(Insert File)
;; `anything-c-source-copy-files'				(Copy Files)
;; `anything-c-source-symlink-files'				(Symlink Files)
;; `anything-c-source-hardlink-files'				(Hardlink Files)
;; `anything-c-source-file-cache'				(File Cache)
;; `anything-c-source-locate'					(Locate)
;; `anything-c-source-recentf'					(Recentf)
;; `anything-c-source-ffap-guesser'				(File at point)
;; `anything-c-source-ffap-line'				(File/Lineno at point)
;; `anything-c-source-files-in-all-dired'			(Files in all dired buffer.)
;; `anything-c-source-filelist'					(FileList)
;; `anything-c-source-info-pages'				(Info Pages)
;; `anything-c-source-man-pages'				(Manual Pages)
;; `anything-c-source-complex-command-history'			(Complex Command History)
;; `anything-c-source-extended-command-history'			(Emacs Commands History)
;; `anything-c-source-emacs-commands'				(Emacs Commands)
;; `anything-c-source-emacs-functions'				(Emacs Functions)
;; `anything-c-source-emacs-functions-with-abbrevs'		(Emacs Functions)
;; `anything-c-source-advice'					(Function Advice)
;; `anything-c-source-emacs-variables'				(Emacs Variables)
;; `anything-c-source-lacarte'					(Lacarte)
;; `anything-c-source-bookmarks'				(Bookmarks)
;; `anything-c-source-bookmark-set'				(Set Bookmark)
;; `anything-c-source-bm'					(Visible Bookmarks)
;; `anything-c-source-bookmarks-ssh'				(Bookmarks-ssh)
;; `anything-c-source-bookmarks-su'				(Bookmarks-root)
;; `anything-c-source-bookmarks-local'				(Bookmarks-Local)
;; `anything-c-source-bmkext-addressbook'			(Bookmark Addressbook)
;; `anything-c-source-bookmark-w3m'				(Bookmark W3m)
;; `anything-c-source-bookmark-images'				(Bookmark Images)
;; `anything-c-source-bookmark-man'				(Bookmark Woman&Man)
;; `anything-c-source-bookmark-gnus'				(Bookmark Gnus)
;; `anything-c-source-bookmark-info'				(Bookmark Info)
;; `anything-c-source-bookmark-files&dirs'			(Bookmark Files&Directories)
;; `anything-c-source-bookmark-su-files&dirs'			(Bookmark Root-Files&Directories)
;; `anything-c-source-bookmark-ssh-files&dirs'			(Bookmark Ssh-Files&Directories)
;; `anything-c-source-firefox-bookmarks'			(Firefox Bookmarks)
;; `anything-c-source-w3m-bookmarks'				(W3m Bookmarks)
;; `anything-c-source-elisp-library-scan'			(Elisp libraries (Scan))
;; `anything-c-source-imenu'					(Imenu)
;; `anything-c-source-ctags'					(Exuberant ctags)
;; `anything-c-source-etags-select'				(Etags)
;; `anything-c-source-semantic'					(Semantic Tags)
;; `anything-c-source-simple-call-tree-functions-callers'	(Function is called by)
;; `anything-c-source-simple-call-tree-callers-functions'	(Function calls)
;; `anything-c-source-commands-and-options-in-file'		(Commands/Options in file)
;; `anything-c-source-customize-face'				(Customize Face)
;; `anything-c-source-colors'					(Colors)
;; `anything-c-source-tracker-search'				(Tracker Search)
;; `anything-c-source-mac-spotlight'				(mdfind)
;; `anything-c-source-picklist'					(Picklist)
;; `anything-c-source-kill-ring'				(Kill Ring)
;; `anything-c-source-mark-ring'				(mark-ring)
;; `anything-c-source-global-mark-ring'				(global-mark-ring)
;; `anything-c-source-register'					(Registers)
;; `anything-c-source-latex-math'				(Latex Math Menu)
;; `anything-c-source-fixme'					(TODO/FIXME/DRY comments)
;; `anything-c-source-rd-headline'				(RD HeadLine)
;; `anything-c-source-oddmuse-headline'				(Oddmuse HeadLine)
;; `anything-c-source-emacs-source-defun'			(Emacs Source DEFUN)
;; `anything-c-source-emacs-lisp-expectations'			(Emacs Lisp Expectations)
;; `anything-c-source-emacs-lisp-toplevels'			(Emacs Lisp Toplevel / Level 4 Comment / Linkd Star)
;; `anything-c-source-yaoddmuse-emacswiki-edit-or-view'		(Yaoddmuse Edit or View (EmacsWiki))
;; `anything-c-source-yaoddmuse-emacswiki-post-library'		(Yaoddmuse Post library (EmacsWiki))
;; `anything-c-source-eev-anchor'				(Anchors)
;; `anything-c-source-org-headline'				(Org HeadLine)
;; `anything-c-source-org-keywords'				(Org Keywords)
;; `anything-c-source-bbdb'					(BBDB)
;; `anything-c-source-evaluation-result'			(Evaluation Result)
;; `anything-c-source-calculation-result'			(Calculation Result)
;; `anything-c-source-google-suggest'				(Google Suggest)
;; `anything-c-source-yahoo-suggest'				(Yahoo Suggest)
;; `anything-c-source-emms-streams'				(Emms Streams)
;; `anything-c-source-emms-dired'				(Music Directory)
;; `anything-c-source-emms-files'				(Emms files)
;; `anything-c-source-jabber-contacts'				(Jabber Contacts)
;; `anything-c-source-call-source'				(Call anything source)
;; `anything-c-source-anything-commands'			(Preconfigured Anything)
;; `anything-c-source-occur'					(Occur)
;; `anything-c-source-browse-code'				(Browse code)
;; `anything-c-source-create'					(Create)
;; `anything-c-source-minibuffer-history'			(Minibuffer History)
;; `anything-c-source-elscreen'					(Elscreen)
;; `anything-c-source-top'					(Top (Press C-c C-u to refresh))
;; `anything-c-source-absolute-time-timers'			(Absolute Time Timers)
;; `anything-c-source-idle-time-timers'				(Idle Time Timers)
;; `anything-c-source-xrandr-change-resolution'			(Change Resolution)
;; `anything-c-source-xfonts'					(X Fonts)
;; `anything-c-source-ucs'					(Ucs names)
;; `anything-c-source-emacs-process'				(Emacs Process)
;; `anything-c-source-time-world'				(Time World List)
;; `anything-c-source-apt'					(APT)
;; `anything-c-source-gentoo'					(Portage sources)
;; `anything-c-source-use-flags'				(Use Flags)
;; `anything-c-source-ratpoison-commands'			(Ratpoison Commands)
;; `anything-c-source-esh'					(Eshell completions)
;; `anything-c-source-eshell-history'				(Eshell history)

;;  *** END auto-documentation

;;; For Maintainers:
;;
;; Install developer-tools/autodoc.el and
;; Evaluate (autodoc-update-all) before commit or run it interactively.
;; This function generates anything-c-source-* / functions / options list.
;;
;; [EVAL IT] (autodoc-update-all)
;;
;; Please write details documentation about function, then others will
;; read code more easier.   -- Andy Stewart
;;


;;; Change log:
;;
;;  Change log of this file is found at
;;  http://repo.or.cz/w/anything-config.git/history/master:/anything-config.el
;;
;;  Change log of this project is found at
;;  http://repo.or.cz/w/anything-config.git?a=shortlog

;;; Contributors:
;;
;;     Tamas Patrovics
;;     Tassilo Horn <tassilo@member.fsf.org>
;;     Vagn Johansen <gonz808@hotmail.com>
;;     Mathias Dahl <mathias.dahl@gmail.com>
;;     Bill Clementson <billclem@gmail.com>
;;     Stefan Kamphausen (see http://www.skamphausen.de for more informations)
;;     Drew Adams <drew.adams@oracle.com>
;;     Jason McBrayer <jmcbray@carcosa.net>
;;     Andy Stewart <lazycat.manatee@gmail.com>
;;     Thierry Volpiatto <thierry.volpiatto@gmail.com>
;;     rubikitch <rubikitch@ruby-lang.org>
;;     Scott Vokes <vokes.s@gmail.com>
;;     Kenichirou Oyama <k1lowxb@gmail.com>


;;; TODO
;;
;; - Fix documentation, now many functions haven't documentations.
;;


;;; Code:

;;; Require
;;
;;
(require 'anything)
(require 'thingatpt)
(require 'ffap)
(require 'cl)
(eval-when-compile (require 'dired))
(require 'dired-aux)
(require 'dired-x)
(require 'tramp)
(require 'grep)
(require 'url)
(require 'xml)
(eval-when-compile (require 'org)) ; Shut up byte compiler about org-directory.
(eval-when-compile (require 'semantic nil t))
(require 'anything-match-plugin)



;;; Declare external functions
;;
;;
(declare-function gnus-dired-attach "ext:gnus-dired.el" (files-to-attach))
(declare-function image-dired-display-image "image-dired.el" (file &optional original-size))
(declare-function image-dired-update-property "image-dired.el" (prop value))
(declare-function woman-file-name-all-completions "woman.el" (topic))
(declare-function Man-getpage-in-background "man.el" (topic))
(declare-function simple-call-tree-analyze "ext:simple-call-tree.el" (&optional test))
(declare-function yaoddmuse-update-pagename "ext:yaoddmuse.el" (&optional unforced))
(declare-function yaoddmuse-get-library-list "ext:yaoddmuse.el" (&optional dirs string))
(declare-function org-get-current-options "ext:org-exp.el")
(declare-function emms-streams "ext:emms-streams")
(declare-function emms-stream-delete-bookmark "ext:emms-streams")
(declare-function emms-stream-add-bookmark "ext:emms-streams" (name url fd type))
(declare-function emms-stream-save-bookmarks-file "ext:emms-streams")
(declare-function emms-stream-quit "ext:emms-streams")
(declare-function with-current-emms-playlist "ext:emms" (&rest body))
(declare-function emms-playlist-tracks-in-region "ext:emms" (beg end))
(declare-function emms-playlist-first "ext:emms")
(declare-function emms-playlist-mode-play-smart "ext:emms-playlist-mode")
(declare-function term-line-mode "term")
(declare-function term-char-mode "term")
(declare-function term-send-input "term")
(declare-function term-send-eof "term")
(declare-function Info-index-nodes "info" (&optional file))
(declare-function Info-goto-node "info" (&optional fork))
(declare-function Info-find-node "info.el" (filename nodename &optional no-going-back))
(declare-function elscreen-find-screen-by-buffer "ext:elscreen.el" (buffer &optional create))
(declare-function elscreen-find-file "ext:elscreen.el" (filename))
(declare-function elscreen-goto "ext:elscreen.el" (screen))
(declare-function semantic-format-tag-summarize "ext:format.el" (tag &optional parent color) t)
(declare-function semantic-tag-components "ext:tag.el" (tag) t)
(declare-function semantic-go-to-tag "ext:tag-file.el" (tag) t)
(declare-function semantic-tag-type "ext:tag-file.el" (tag) t)
(declare-function semantic-tag-class "ext:tag-file.el" (tag) t)
(declare-function bbdb "ext:bbdb-com")
(declare-function bbdb-current-record "ext:bbdb-com")
(declare-function bbdb-redisplay-one-record "ext:bbdb-com")
(declare-function bbdb-record-net "ext:bbdb-com" (string) t)
(declare-function bbdb-current-record "ext:bbdb-com")
(declare-function bbdb-dwim-net-address "ext:bbdb-com")
(declare-function bbdb-records "ext:bbdb-com"
                  (&optional dont-check-disk already-in-db-buffer))
(declare-function eshell-read-aliases-list "em-alias")
(declare-function eshell-send-input "esh-mode" (&optional use-region queue-p no-newline))
(declare-function eshell-bol "esh-mode")
(declare-function eldoc-current-symbol "eldoc")
(declare-function eldoc-get-fnsym-args-string "eldoc" (sym &optional index))
(declare-function eldoc-get-var-docstring "eldoc" (sym))
(declare-function eldoc-fnsym-in-current-sexp "eldoc")
(declare-function find-library-name "find-func.el" (library))
(declare-function adoc-construct "ext:auto-document.el" (buf))
(declare-function adoc-first-line "ext:auto-document.el" (str))
(declare-function adoc-prin1-to-string "ext:auto-document.el" (object))
(declare-function secure-hash "ext:fns.c" (algorithm object &optional start end binary))
(declare-function w32-shell-execute "ext:w32fns.c" (operation document &optional parameters show-flag))
(declare-function undo-tree-restore-state-from-register "ext:undo-tree.el" (register))


;;; compatibility
;;
;;
(unless (fboundp 'window-system)
  (defun window-system (&optional arg)
    window-system))

(unless (fboundp 'make-composed-keymap)
  (defun make-composed-keymap (maps &optional parent)
    "Construct a new keymap composed of MAPS and inheriting from PARENT.
When looking up a key in the returned map, the key is looked in each
keymap of MAPS in turn until a binding is found.
If no binding is found in MAPS, the lookup continues in PARENT, if non-nil.
As always with keymap inheritance, a nil binding in MAPS overrides
any corresponding binding in PARENT, but it does not override corresponding
bindings in other keymaps of MAPS.
MAPS can be a list of keymaps or a single keymap.
PARENT if non-nil should be a keymap."
    `(keymap
      ,@(if (keymapp maps) (list maps) maps)
      ,@parent)))

(unless (fboundp 'apply-partially)
  (defun apply-partially (fun &rest args)
    "Return a function that is a partial application of FUN to ARGS.
ARGS is a list of the first N arguments to pass to FUN.
The result is a new function which does the same as FUN, except that
the first N arguments are fixed at the values with which this function
was called."
    (lexical-let ((fun fun) (args1 args))
      (lambda (&rest args2) (apply fun (append args1 args2))))))


;;; Customize
;;
;;
(defgroup anything-config nil
  "Predefined configurations for `anything.el'."
  :group 'anything)

(defcustom anything-c-adaptive-history-file
  "~/.emacs.d/anything-c-adaptive-history"
  "Path of file where history information is stored."
  :type 'string
  :group 'anything-config)

(defcustom anything-c-adaptive-history-length 50
  "Maximum number of candidates stored for a source."
  :type 'number
  :group 'anything-config)

(defcustom anything-c-google-suggest-url
  "http://google.com/complete/search?output=toolbar&q="
  "URL used for looking up Google suggestions."
  :type 'string
  :group 'anything-config)

(defcustom anything-c-google-suggest-search-url
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
  "URL used for Google searching."
  :type 'string
  :group 'anything-config)

(defcustom anything-google-suggest-use-curl-p nil
  "When non--nil use CURL to get info from `anything-c-google-suggest-url'.
Otherwise `url-retrieve-synchronously' is used."
  :type 'boolean
  :group 'anything-config)

(defcustom anything-c-yahoo-suggest-url
  "http://search.yahooapis.com/WebSearchService/V1/relatedSuggestion?appid=Generic&query="
  "Url used for looking up Yahoo suggestions."
  :type 'string
  :group 'anything-config)

(defcustom anything-c-yahoo-suggest-search-url
  "http://search.yahoo.com/search?&ei=UTF-8&fr&h=c&p="
  "Url used for Yahoo searching."
  :type 'string
  :group 'anything-config)

(defcustom anything-c-boring-buffer-regexp
  (rx (or
       (group bos  " ")
       ;; anything-buffers
       "*anything" "*ac-mode"
       ;; echo area
       " *Echo Area" " *Minibuf"))
  "The regexp that match boring buffers.
Buffer candidates matching this regular expression will be
filtered from the list of candidates if the
`anything-c-skip-boring-buffers' candidate transformer is used, or
they will be displayed with face `file-name-shadow' if
`anything-c-shadow-boring-buffers' is used."
  :type 'string
  :group 'anything-config)
;; (string-match anything-c-boring-buffer-regexp "buf")
;; (string-match anything-c-boring-buffer-regexp " hidden")
;; (string-match anything-c-boring-buffer-regexp " *Minibuf-1*")

(defcustom anything-c-boring-file-regexp
  (rx (or
       ;; Boring directories
       (and "/" (or ".svn" "CVS" "_darcs" ".git" ".hg") (or "/" eol))
       ;; Boring files
       (and line-start  ".#")
       (and (or ".class" ".la" ".o" "~") eol)))
  "The regexp that match boring files.
File candidates matching this regular expression will be
filtered from the list of candidates if the
`anything-c-skip-boring-files' candidate transformer is used, or
they will be displayed with face `file-name-shadow' if
`anything-c-shadow-boring-files' is used."
  :type 'string
  :group 'anything-config)

(defcustom anything-kill-ring-threshold 10
  "Minimum length to be listed by `anything-c-source-kill-ring'."
  :type 'integer
  :group 'anything-config)

(defcustom anything-c-kill-ring-max-lines-number nil
  "Max number of lines displayed per candidate in kill-ring browser.
If nil or zero, don't truncate candidate, show all."
  :type 'integer
  :group 'anything-config)

(defcustom anything-su-or-sudo "su"
  "What command to use for root access."
  :type 'string
  :group 'anything-config)

(defcustom anything-for-files-prefered-list
  '(anything-c-source-ffap-line
    anything-c-source-ffap-guesser
    anything-c-source-buffers-list
    anything-c-source-recentf
    anything-c-source-bookmarks
    anything-c-source-file-cache
    anything-c-source-files-in-current-dir+
    anything-c-source-locate)
  "Your prefered sources to find files."
  :type 'list
  :group 'anything-config)

(defcustom anything-create--actions-private nil
  "User defined actions for `anything-create' / `anything-c-source-create'.
It is a list of (DISPLAY . FUNCTION) pairs like `action'
attribute of `anything-sources'.

It is prepended to predefined pairs."
  :type 'list
  :group 'anything-config)

(defcustom anything-allow-skipping-current-buffer nil
  "Show current buffer or not in anything buffer"
  :type 'boolean
  :group 'anything-config)

(defcustom anything-c-enable-eval-defun-hack t
  "If non-nil, execute `anything' using the source at point when C-M-x is pressed.
This hack is invoked when pressing C-M-x in the form \
 (defvar anything-c-source-XXX ...) or (setq anything-c-source-XXX ...)."
  :type 'boolean
  :group 'anything-config)

(defcustom anything-tramp-verbose 0
  "Just like `tramp-verbose' but specific to anything.
When set to 0 don't show tramp messages in anything.
If you want to have the default tramp messages set it to 3."
  :type 'integer
  :group 'anything-config)

(defcustom anything-raise-command nil
  "A shell command to jump to a window running specific program.
Need external program wmctrl.
This will be use with `format', so use something like \"wmctrl -xa %s\"."
  :type 'string
  :group 'anything-config)

(defun anything-set-anything-command-map-prefix-key (var key)
  "The customize set function for `anything-command-map-prefix-key'."
  (when (boundp var)
    (define-key global-map (read-kbd-macro (symbol-value var)) nil))
  (set var key)
  (define-key global-map
      (read-kbd-macro (symbol-value var)) 'anything-command-map))

(defcustom anything-command-map-prefix-key "C-x c"
  "The prefix key for all `anything-command-map' commands."
  :type  'string
  :set   'anything-set-anything-command-map-prefix-key
  :group 'anything-config)

(defcustom anything-c-browse-code-regexp-lisp
  "^ *\(def\\(un\\|subst\\|macro\\|face\\|alias\\|advice\\|struct\\|\
type\\|theme\\|var\\|group\\|custom\\|const\\|method\\|class\\)"
  "Regexp used to parse lisp buffer when browsing code."
  :type 'string
  :group 'anything-config)

(defcustom anything-c-browse-code-regexp-python
  "\\<def\\>\\|\\<class\\>"
  "Regexp used to parse python buffer when browsing code."
  :type 'string
  :group 'anything-config)

(defcustom anything-c-browse-code-regexp-alist
  `((lisp-interaction-mode . ,anything-c-browse-code-regexp-lisp)
    (emacs-lisp-mode . ,anything-c-browse-code-regexp-lisp)
    (lisp-mode . ,anything-c-browse-code-regexp-lisp)
    (python-mode . ,anything-c-browse-code-regexp-python))
  "Alist to store regexps for browsing code corresponding \
to a specific `major-mode'."
  :type 'list
  :group 'anything-config)

(defcustom anything-c-external-programs-associations nil
  "Alist to store externals programs associated with file extension.
This variable overhide setting in .mailcap file.
e.g : '\(\(\"jpg\" . \"gqview\"\) (\"pdf\" . \"xpdf\"\)\) "
  :type 'list
  :group 'anything-config)

(defcustom anything-ff-auto-update-initial-value t
  "Auto update when only one candidate directory is matched.
This is the default value when starting `anything-find-files'."
  :group 'anything-config
  :type  'boolean)

(defcustom anything-c-copy-async-prefered-emacs "emacs"
  "Path to the emacs you want to use for copying async.
Emacs versions < 24 fail to copy directory due to a bug not fixed
in `copy-directory'."
  :group 'anything-config
  :type 'string)

(defcustom anything-ff-lynx-style-map t
  "Use arrow keys to navigate with `anything-find-files'.
You will have to restart Emacs or reeval `anything-find-files-map'
and `anything-c-read-file-map' for this take effect."
  :group 'anything-config
  :type 'boolean)

(defcustom anything-ff-history-max-length 100
  "Number of elements shown in `anything-find-files' history."
  :group 'anything-config
  :type 'integer)

(defcustom anything-ff-smart-completion t
  "Try to complete filenames smarter when non--nil.
See `anything-ff-transform-fname-for-completion' for more info."
  :group 'anything-config
  :type 'boolean)

(defcustom anything-ff-default-kbsize 1024.0
  "Default Kbsize to use for showing files size.
It is a float, usually 1024.0 but could be 1000.0 on some systems."
  :group 'anything-config
  :type 'float)

(defcustom anything-ff-tramp-not-fancy t
  "No colors when listing remote files when set to non--nil.
This make listing much faster, specially on slow machines."
  :group 'anything-config
  :type  'boolean)

(defcustom anything-ff-exif-data-program "exiftran"
  "Program used to extract exif data of an image file."
  :group 'anything-config
  :type 'string)

(defcustom anything-ff-exif-data-program-args "-d"
  "*Arguments used for `anything-ff-exif-data-program'."
  :group 'anything-config
  :type 'string)

(defcustom anything-c-grep-use-ioccur-style-keys t
  "Use Arrow keys to jump to occurences."
  :group 'anything-config
  :type  'boolean)

(defcustom anything-c-pdfgrep-default-read-command "xpdf '%f' %p"
  "Default command to read pdf files from pdfgrep.
Where '%f' format spec is filename and '%p' is page number"
  :group 'anything-config
  :type  'string)

(defcustom anything-c-etags-tag-file-name "TAGS"
  "Etags tag file name."
  :type  'string
  :group 'anything-config)

(defcustom anything-c-etags-tag-file-search-limit 10
  "The limit level of directory to search tag file.
Don't search tag file deeply if outside this value."
  :type  'number
  :group 'anything-config)

(defcustom anything-c-etags-use-regexp-search nil
  "When non--nil search etags candidates by regexp.
This disable anything-match-plugin when enabled.
When nil search is performed directly on patter and *match-plugin is used
if available.  You can customize `anything-c-etags-search-regexp'."
  :group 'anything-config
  :type  'boolean)

(defcustom anything-c-etags-search-regexp "^.+: .+ \\<%s"
  "Regexp that match tags in an anything etags buffer.
The format spec is replaced by pattern.
This regexp have no effect when `anything-c-etags-use-regexp-search'
is nil."
  :group 'anything-config
  :type  'regexp)

(defcustom anything-c-filelist-file-name nil
  "Filename of file list.
Accept a list of string for multiple files.

This file tend to be very large \(> 100MB\) and recommend to be in ramdisk for speed.
File list is created by make-filelist.rb script.

Usage:
  ruby make-filelist.rb > /tmp/all.filelist

Then
 ;; Assume that /tmp is ramdisk or tmpfs
 \(setq anything-grep-candidates-fast-directory-regexp \"^/tmp/\"\)
 \(setq anything-c-filelist-file-name \"/tmp/all.filelist\"\)
"
  :type 'string
  :group 'anything-config)

(defcustom anything-c-eldoc-in-minibuffer-show-fn
  'anything-c-show-info-in-mode-line
  "A function to display eldoc info.
Should take one arg: the string to display."
  :group 'anything-config
  :type  'symbol)

(defcustom anything-c-turn-on-show-completion t
  "Display candidate in buffer while moving selection when non--nil."
  :group 'anything-config
  :type  'boolean)

(defcustom anything-c-show-completion-use-special-display t
  "A special display will be used in lisp completion if non--nil.
All functions that are wrapped in macro `with-anything-show-completion'
will be affected."
  :group 'anything-config
  :type  'boolean)

(defcustom anything-c-show-completion-min-window-height 7
  "Minimum completion window height used in show completion.
This is used in macro `with-anything-show-completion'."
  :group 'anything-config
  :type  'integer)

(defcustom anything-lisp-completion-or-indent-delay 0.6
  "After this delay `anything-lisp-completion-counter' is reset to 0.
This allow to indent again without completing lisp symbol after this delay.
Default is 0.6 seconds."
  :group 'anything-config
  :type  'number)

(defcustom anything-c-default-external-file-browser "nautilus"
  "Default external file browser for your system.
Directories will be opened externally with it when
opening file externally in `anything-find-files'.
Set to nil if you do not have external file browser
or do not want to use it.
Windows users should set that to \"explorer.exe\"."
  :group 'anything-config
  :type  'string)

(defcustom anything-c-use-adaptative-sorting nil
  "Wheter to use or not adaptative sorting.
Even if a source use it, it will have no effect when set to nil."
  :type 'boolean
  :group 'anything-config)

(defcustom anything-ff-newfile-prompt-p t
  "Whether Prompt or not when creating new file.
This set `ffap-newfile-prompt'."
  :type  'boolean
  :group 'anything-config)


(defcustom anything-ff-avfs-directory nil
  "The default avfs directory, usually '.avfs'.
When this is set you will be able to expand archive filenames with `C-z'
inside an avfs directory mounted with mountavfs.
See <http://sourceforge.net/projects/avf/>."
  :type  'boolean
  :group 'anything-config)

(defcustom anything-ff-file-compressed-list '("gz" "bz2" "zip" "7z")
  "Minimal list of compressed files extension."
  :type  'list
  :group 'anything-config)

(defcustom anything-locate-db-file-regexp "m?locate\.db$"
  "Default regexp to match locate database.
If nil Search in all files."
  :type  'string
  :group 'anything-config)

(defcustom anything-ff-locate-db-filename "locate.db"
  "The basename of the locatedb file you use locally in your directories.
When this is set and `anything' find such a file in the directory from
where you launch locate, it will use this file and will not prompt you
for a db file.
Note that this happen only when locate is launched with a prefix arg."
  :group 'anything-config
  :type 'string)

(defcustom anything-c-locate-command nil
  "A list of arguments for locate program.
If nil it will be calculated when `anything-locate' startup
with these default values for different systems:

Gnu/linux: \"locate -i -r %s\"
berkeley-unix: \"locate -i %s\"
windows-nt: \"es -i -r %s\"
Others: \"locate %s\"

This string will be passed to format so it should end with `%s'.
The \"-r\" option must be the last option."
  :type 'string
  :group 'anything-config)

(defcustom anything-c-show-info-in-mode-line-delay 12
  "Eldoc will show info in mode-line during this delay if user is idle."
  :type  'integer
  :group 'anything-config)

(defcustom anything-c-copy-files-async-log-file "/tmp/dired.log"
  "The file used to communicate with two emacs when copying files async."
  :type  'string
  :group 'anything-config)

(defcustom anything-ff-printer-list nil
  "A list of available printers on your system.
When non--nil let you choose a printer to print file.
Otherwise when nil the variable `printer-name' will be used.
On Unix based systems (lpstat command needed) you don't need to set this,
`anything-ff-find-printers' will find a list of available printers for you."
  :type 'list
  :group 'anything-config)

(defcustom anything-ff-transformer-show-only-basename nil
  "Show only basename of candidates in `anything-find-files'.
This can be toggled at anytime from `anything-find-files' with \
\\<anything-find-files-map>0\\[anything-ff-run-toggle-basename]."
  :type 'boolean
  :group 'anything-config)

(defcustom anything-ff-quick-delete-dont-prompt-for-deletion nil
  "Don't ask in persistent deletion of files when non--nil."
  :group 'anything-config
  :type 'boolean)

(defcustom anything-ff-signal-error-on-dot-files t
  "Signal error when file is `.' or `..' on file deletion when non--nil.
Default is non--nil.
WARNING: Setting this to nil is unsafe and can cause deletion of a whole tree."
  :group 'anything-config
  :type 'boolean)

(defcustom anything-completing-read-handlers-alist
  '((describe-function . anything-completing-read-symbols)
    (describe-variable . anything-completing-read-symbols)
    (debug-on-entry . anything-completing-read-symbols)
    (find-function . anything-completing-read-symbols)
    (trace-function . anything-completing-read-symbols)
    (trace-function-background . anything-completing-read-symbols)
    (find-tag . anything-completing-read-with-cands-in-buffer)
    (ffap-alternate-file . nil))
  "Alist of handlers to replace `completing-read', `read-file-name' in `ac-mode'.
Each entry is a cons cell like \(emacs_command . completing-read_handler\)
where key and value are symbols.

Each key is an Emacs command that use originaly `completing-read'.

Each value maybe an anything function that take same arguments as
`completing-read' plus NAME and BUFFER, where NAME is the name of the new
anything source and BUFFER the name of the buffer we will use.
This function prefix name must start by \"anything\".

See `anything-completing-read-symbols' for example.

If the value of an entry is nil completion will fall back to
emacs vanilla behavior.
e.g If you want to disable anything completion for `describe-function':
\(describe-function . nil\).

Ido is also supported, you can use `ido-completing-read' and
`ido-read-file-name' as value of an entry or just 'ido.
e.g ido completion for `find-file':
\(find-file . ido\)
same as
\(find-file . ido-read-file-name\)
Note that you don't need to enable `ido-mode' for this to work."
  :group 'anything-config
  :type '(alist :key-type symbol :value-type symbol))

(defcustom anything-M-x-requires-pattern 2
  "Value of requires-pattern for `anything-M-x'.
Set it to 0 to disable requires-pattern in `anything-M-x'."
  :group 'anything-config
  :type 'boolean)

;;; Build info-index sources with info-index plug-in.
;;
;;
(defun anything-c-build-info-index-command (name doc source buffer)
  "Define an anything command NAME with documentation DOC.
Arg SOURCE will be an existing anything source named
`anything-c-source-info-<NAME>' and BUFFER a string buffer name."
  (eval (list 'defun name nil doc
              (list 'interactive)
              (list 'anything
                    :sources source
                    :buffer buffer
                    :candidate-number-limit 1000))))

(defun anything-c-define-info-index-sources (var-value &optional commands)
  "Define anything sources named anything-c-source-info-<NAME>.
Sources are generated for all entries of `anything-c-default-info-index-list'.
If COMMANDS arg is non--nil build also commands named `anything-info<NAME>'.
Where NAME is one of `anything-c-default-info-index-list'."
  (loop with symbols = (loop for str in var-value
                             collect
                             (intern (concat "anything-c-source-info-" str)))
        for sym in symbols
        for str in var-value
        do (set sym (list (cons 'name (format "Info index: %s" str))
                          (cons 'info-index str)))
        when commands
        do (let ((com (intern (concat "anything-info-" str))))
	     (anything-c-build-info-index-command com
               (format "Predefined anything for %s info." str) sym
               (format "*anything info %s*" str)))))

(defun anything-info-index-set (var value)
  (set var value)
  (anything-c-define-info-index-sources value t))

(defcustom anything-c-default-info-index-list
  '("elisp" "cl" "org" "gnus" "tramp" "ratpoison"
    "zsh" "bash" "coreutils" "fileutils"
    "find" "sh-utils" "textutils" "libc"
    "make" "automake" "autoconf" "emacs-lisp-intro"
    "emacs" "elib" "eieio" "gauche-refe" "guile"
    "guile-tut" "goops" "screen" "latex" "gawk"
    "sed" "m4" "wget" "binutils" "as" "bfd" "gprof"
    "ld" "diff" "flex" "grep" "gzip" "libtool"
    "texinfo" "info" "gdb" "stabs" "cvsbook" "cvs"
    "bison" "id-utils" "global")
  "Info Manual entries to use for building anything info index commands."
  :group 'anything-config
  :type 'list
  :set 'anything-info-index-set)

(defcustom anything-c-register-max-offset 160
  "Max size of string register entries before truncating."
  :group 'anything-config
  :type 'integer)


;;; General internal variables
;;
;; Some internals variable that need to be loaded
;; here to avoid compiler warnings.
(defvar anything-c-external-commands-list nil
  "A list of all external commands the user can execute.  If this
variable is not set by the user, it will be calculated
automatically.")

(defvar anything-c-show-completion-overlay nil)



;;; Faces
;;
;;
(defface anything-buffer-saved-out
    '((t (:foreground "red")))
  "*Face used for buffer files modified outside of emacs."
  :group 'anything-config)

(defface anything-buffer-not-saved
    '((t (:foreground "Indianred2")))
  "*Face used for buffer files not already saved on disk."
  :group 'anything-config)

(defface anything-ff-prefix
    '((t (:background "yellow" :foreground "black")))
  "*Face used to prefix new file or url paths in `anything-find-files'."
  :group 'anything-config)

(defface anything-ff-executable
    '((t (:foreground "green")))
  "*Face used for executable files in `anything-find-files'."
  :group 'anything-config)

(defface anything-ff-directory
    '((t (:foreground "DarkRed" :background "LightGray")))
  "*Face used for directories in `anything-find-files'."
  :group 'anything-config)

(defface anything-ff-symlink
    '((t (:foreground "DarkOrange")))
  "*Face used for symlinks in `anything-find-files'."
  :group 'anything-config)

(defface anything-ff-invalid-symlink
    '((t (:foreground "black" :background "red")))
  "*Face used for invalid symlinks in `anything-find-files'."
  :group 'anything-config)

(defface anything-ff-file
    '((t (:foreground "CadetBlue" :underline t)))
  "*Face used for file names in `anything-find-files'."
  :group 'anything-config)

(defface anything-grep-match
    '((t (:inherit match)))
  "Face used to highlight grep matches."
  :group 'anything-config)

(defface anything-grep-file
    '((t (:foreground "BlueViolet" :underline t)))
  "Face used to highlight grep results filenames."
  :group 'anything-config)

(defface anything-grep-lineno
    '((t (:foreground "Darkorange1")))
  "Face used to highlight grep number lines."
  :group 'anything-config)

(defface anything-grep-running
    '((t (:foreground "Red")))
  "Face used in mode line when grep is running."
  :group 'anything-config)

(defface anything-grep-finish
    '((t (:foreground "Green")))
  "Face used in mode line when grep is finish."
  :group 'anything-config)

(defface anything-M-x-key-face '((t (:foreground "orange" :underline t)))
  "*Face used in anything-M-x to show keybinding."
  :group 'anything)

(defface anything-bmkext-info
    '((t (:foreground "green")))
  "*Face used for W3m Emacs bookmarks (not w3m bookmarks)."
  :group 'anything)

(defface anything-bmkext-w3m
    '((t (:foreground "yellow")))
  "*Face used for W3m Emacs bookmarks (not w3m bookmarks)."
  :group 'anything)

(defface anything-bmkext-gnus
    '((t (:foreground "magenta")))
  "*Face used for Gnus bookmarks."
  :group 'anything)

(defface anything-bmkext-man
    '((t (:foreground "Orange4")))
  "*Face used for Woman/man bookmarks."
  :group 'anything)

(defface anything-bmkext-no--file
    '((t (:foreground "grey")))
  "*Face used for non--file bookmarks."
  :group 'anything)

(defface anything-bmkext-file
    '((t (:foreground "Deepskyblue2")))
  "*Face used for non--file bookmarks."
  :group 'anything)

(defface anything-bookmarks-su-face '((t (:foreground "red")))
  "Face for su/sudo bookmarks."
  :group 'anything)

(defface anything-w3m-bookmarks-face '((t (:foreground "cyan1" :underline t)))
  "Face for w3m bookmarks" :group 'anything)

(defface anything-emms-playlist
    '((t (:foreground "Springgreen4" :underline t)))
  "*Face used for tracks in current emms playlist."
  :group 'anything)

(defface anything-apt-installed
    '((t (:foreground "green")))
  "*Face used for apt installed candidates."
  :group 'anything)

(defface anything-apt-deinstalled
    '((t (:foreground "DimGray")))
  "*Face used for apt deinstalled candidates."
  :group 'anything)

(defface anything-gentoo-match-face '((t (:foreground "red")))
  "Face for anything-gentoo installed packages."
  :group 'traverse-faces)

(defface anything-lisp-show-completion
    '((t (:background "DarkSlateGray")))
  "*Face used for showing candidates in `anything-lisp-completion'."
  :group 'anything-config)

(defface anything-lisp-completion-info
    '((t (:foreground "red")))
  "*Face used for showing info in `anything-lisp-completion'."
  :group 'anything-config)

(defface anything-overlay-line-face '((t (:background "IndianRed4" :underline t)))
  "Face for source header in the anything buffer." :group 'anything)

;;;###autoload
(defun anything-configuration ()
  "Customize `anything'."
  (interactive)
  (customize-group "anything-config"))



;;; Anything-command-map
;;
;;
;;;###autoload
(defvar anything-command-map)
(define-prefix-command 'anything-command-map)


(define-key anything-command-map (kbd "<SPC>")     'anything-execute-anything-command)
(define-key anything-command-map (kbd "a")         'anything-c-apropos)
(define-key anything-command-map (kbd "e")         'anything-c-etags-select)
(define-key anything-command-map (kbd "l")         'anything-locate)
(define-key anything-command-map (kbd "s")         'anything-surfraw)
(define-key anything-command-map (kbd "r")         'anything-regexp)
(define-key anything-command-map (kbd "w")         'anything-w3m-bookmarks)
(define-key anything-command-map (kbd "x")         'anything-firefox-bookmarks)
(define-key anything-command-map (kbd "#")         'anything-emms)
(define-key anything-command-map (kbd "m")         'anything-man-woman)
(define-key anything-command-map (kbd "t")         'anything-top)
(define-key anything-command-map (kbd "i")         'anything-imenu)
(define-key anything-command-map (kbd "<tab>")     'anything-lisp-completion-at-point)
(define-key anything-command-map (kbd "p")         'anything-list-emacs-process)
(define-key anything-command-map (kbd "C-x r b")   'anything-c-pp-bookmarks)
(define-key anything-command-map (kbd "M-y")       'anything-show-kill-ring)
(define-key anything-command-map (kbd "C-c <SPC>") 'anything-all-mark-rings)
(define-key anything-command-map (kbd "C-x C-f")   'anything-find-files)
(define-key anything-command-map (kbd "f")         'anything-for-files)
(define-key anything-command-map (kbd "C-:")       'anything-eval-expression-with-eldoc)
(define-key anything-command-map (kbd "C-,")       'anything-calcul-expression)
(define-key anything-command-map (kbd "M-x")       'anything-M-x)
(define-key anything-command-map (kbd "C-x C-w")   'anything-write-file)
(define-key anything-command-map (kbd "C-x i")     'anything-insert-file)
(define-key anything-command-map (kbd "M-s o")     'anything-occur)
(define-key anything-command-map (kbd "M-g s")     'anything-do-grep)
(define-key anything-command-map (kbd "c")         'anything-colors)
(define-key anything-command-map (kbd "F")         'anything-select-xfont)
(define-key anything-command-map (kbd "C-c f")     'anything-recentf)
(define-key anything-command-map (kbd "C-c g")     'anything-google-suggest)
(define-key anything-command-map (kbd "h i")       'anything-info-at-point)
(define-key anything-command-map (kbd "h r")       'anything-info-emacs)
(define-key anything-command-map (kbd "h g")       'anything-info-gnus)
(define-key anything-command-map (kbd "C-x C-b")   'anything-buffers-list)
(define-key anything-command-map (kbd "C-c C-b")   'anything-browse-code)
(define-key anything-command-map (kbd "C-x r i")   'anything-register)
(define-key anything-command-map (kbd "C-c C-x")   'anything-c-run-external-command)

;; In Emacs 23.1.50, minibuffer-local-must-match-filename-map was renamed to
;; minibuffer-local-filename-must-match-map.
(defvar minibuffer-local-filename-must-match-map (make-sparse-keymap)) ;; Emacs 23.1.+
(defvar minibuffer-local-must-match-filename-map (make-sparse-keymap)) ;; Older Emacsen
(dolist (map (list minibuffer-local-filename-completion-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-filename-map
                   minibuffer-local-filename-must-match-map
                   minibuffer-local-map
                   minibuffer-local-isearch-map
                   minibuffer-local-must-match-map
                   minibuffer-local-ns-map))
  (define-key map "\C-r" 'anything-minibuffer-history))



;;; Menu
;;
;;
(easy-menu-define nil global-map
  "`anything' menu"
  '("Anything"
    ["All anything commands" anything-execute-anything-command t]
    ["Find any Files/Buffers" anything-for-files t]
    ["Anything Everywhere (Toggle)" ac-mode t]
    "----"
    ("Files:"
     ["Find files" anything-find-files t]
     ["Recent Files" anything-recentf t]
     ["Locate" anything-locate t]
     ["Bookmarks" anything-c-pp-bookmarks t])
    ("Buffers:"
     ["Find buffers" anything-buffers-list t])
    ("Commands:"
     ["Emacs Commands" anything-M-x t]
     ["Externals Commands" anything-c-run-external-command t])
    ("Help:"
     ["Anything Apropos" anything-c-apropos t])
    ("Info:"
     ["Info at point" anything-info-at-point t]
     ["Emacs Manual index" anything-info-emacs t]
     ["Gnus Manual index" anything-info-gnus t])
    ("Org:"
     ["Org keywords" anything-org-keywords t]
     ["Org headlines" anything-org-headlines t])
    ("Tools:"
     ["Occur" anything-occur t]
     ["Grep" anything-do-grep t]
     ["Etags" anything-c-etags-select t]
     ["Lisp complete at point" anything-lisp-completion-at-point t]
     ["Browse Kill ring" anything-show-kill-ring t]
     ["Browse register" anything-register t]
     ["Browse code" anything-browse-code t]
     ["Mark Ring" anything-all-mark-rings t]
     ["Regexp handler" anything-regexp t]
     ["Colors & Faces" anything-colors t]
     ["Show xfonts" anything-select-xfont t]
     ["Ucs Symbols" anything-ucs t]
     ["Imenu" anything-imenu t]
     ["Google Suggest" anything-google-suggest t]
     ["Eval expression" anything-eval-expression-with-eldoc t]
     ["Calcul expression" anything-calcul-expression t]
     ["Man pages" anything-man-woman t]
     ["Top externals process" anything-top t]
     ["Emacs internals process" anything-list-emacs-process t])
    "----"
    ["Prefered Options" anything-configuration t]))

;;; Anything map add ons
;;
;;
(define-key anything-map (kbd "C-x C-f") 'anything-quit-and-find-file)
(define-key anything-map (kbd "M-m")     'anything-toggle-all-marks)
(define-key anything-map (kbd "C-w")     'anything-yank-text-at-point)


;;; Specialized keymaps
;;
;;
(defvar anything-c-buffer-map
  (let ((map (copy-keymap anything-map)))
    (define-key map (kbd "C-c ?")     'anything-c-buffer-help)
    ;; No need to have separate command for grep and zgrep
    ;; as we don't use recursivity for buffers.
    ;; So use zgrep for both as it is capable to handle non--compressed files.
    (define-key map (kbd "M-g s")     'anything-buffer-run-zgrep)
    (define-key map (kbd "C-c o")     'anything-buffer-switch-other-window)
    (define-key map (kbd "C-c C-o")   'anything-buffer-switch-other-frame)
    (define-key map (kbd "C-c =")     'anything-buffer-run-ediff)
    (define-key map (kbd "M-=")       'anything-buffer-run-ediff-merge)
    (define-key map (kbd "C-=")       'anything-buffer-diff-persistent)
    (define-key map (kbd "M-U")       'anything-buffer-revert-persistent)
    (define-key map (kbd "M-D")       'anything-buffer-run-kill-buffers)
    (define-key map (kbd "C-x C-s")   'anything-buffer-save-persistent)
    (define-key map (kbd "C-M-%")     'anything-buffer-run-query-replace-regexp)
    (define-key map (kbd "M-%")       'anything-buffer-run-query-replace)
    (define-key map (kbd "M-m")       'anything-toggle-all-marks)
    (define-key map (kbd "M-a")       'anything-mark-all)
    (when (locate-library "elscreen")
      (define-key map (kbd "<C-tab>") 'anything-buffer-switch-to-elscreen))
    (delq nil map))
  "Keymap for buffer sources in anything.")

(defvar anything-find-files-map
  (let ((map (copy-keymap anything-map)))
    (define-key map (kbd "C-]")           'anything-ff-run-toggle-basename)
    (define-key map (kbd "C-x C-f")       'anything-ff-run-locate)
    (define-key map (kbd "M-g s")         'anything-ff-run-grep)
    (define-key map (kbd "M-g p")         'anything-ff-run-pdfgrep)
    (define-key map (kbd "M-g z")         'anything-ff-run-zgrep)
    (define-key map (kbd "M-.")           'anything-ff-run-etags)
    (define-key map (kbd "M-R")           'anything-ff-run-rename-file)
    (define-key map (kbd "M-C")           'anything-ff-run-copy-file)
    (define-key map (kbd "M-B")           'anything-ff-run-byte-compile-file)
    (define-key map (kbd "M-L")           'anything-ff-run-load-file)
    (define-key map (kbd "M-S")           'anything-ff-run-symlink-file)
    (define-key map (kbd "M-H")           'anything-ff-run-hardlink-file)
    (define-key map (kbd "M-D")           'anything-ff-run-delete-file)
    (define-key map (kbd "M-K")           'anything-ff-run-kill-buffer-persistent)
    (define-key map (kbd "C-d")           'anything-ff-persistent-delete)
    (define-key map (kbd "M-e")           'anything-ff-run-switch-to-eshell)
    (define-key map (kbd "<M-tab>")       'anything-ff-run-complete-fn-at-point)
    (define-key map (kbd "C-c o")         'anything-ff-run-switch-other-window)
    (define-key map (kbd "C-c C-o")       'anything-ff-run-switch-other-frame)
    (define-key map (kbd "C-c C-x")       'anything-ff-run-open-file-externally)
    (define-key map (kbd "M-!")           'anything-ff-run-eshell-command-on-file)
    (define-key map (kbd "C-=")           'anything-ff-run-ediff-file)
    (define-key map (kbd "C-c =")         'anything-ff-run-ediff-merge-file)
    (define-key map (kbd "M-p")           'anything-ff-run-switch-to-history)
    (define-key map (kbd "M-i")           'anything-ff-properties-persistent)
    (define-key map (kbd "C-c ?")         'anything-ff-help)
    (define-key map (kbd "C-}")           'anything-narrow-window)
    (define-key map (kbd "C-{")           'anything-enlarge-window)
    (define-key map (kbd "C-<backspace>") 'anything-ff-run-toggle-auto-update)
    (define-key map (kbd "M-a")           'anything-mark-all)
    (define-key map (kbd "M-m")           'anything-toggle-all-marks)
    (define-key map (kbd "M-u")           'anything-unmark-all)
    (define-key map (kbd "C-c C-a")       'anything-ff-run-gnus-attach-files)
    (define-key map (kbd "C-c p")         'anything-ff-run-print-file)
    ;; Next 2 have no effect if candidate is not an image file.
    (define-key map (kbd "M-l")           'anything-ff-rotate-left-persistent)
    (define-key map (kbd "M-r")           'anything-ff-rotate-right-persistent)
    (define-key map (kbd "C-.")           'anything-find-files-down-one-level)
    (define-key map (kbd "C-l")           'anything-find-files-down-one-level)
    (define-key map (kbd "C-h C-b")       'anything-send-bug-report-from-anything)
    (define-key map (kbd "C-h C-d")       'anything-debug-output)
    (when anything-ff-lynx-style-map
      (define-key map (kbd "<left>")      'anything-find-files-down-one-level)
      (define-key map (kbd "<right>")     'anything-execute-persistent-action))
    (delq nil map))
  "Keymap for `anything-find-files'.")

(defvar anything-c-read-file-map
  (let ((map (copy-keymap anything-map)))
    (define-key map (kbd "C-]")           'anything-ff-run-toggle-basename)
    (define-key map (kbd "C-.")           'anything-find-files-down-one-level)
    (define-key map (kbd "C-l")           'anything-find-files-down-one-level)
    (define-key map (kbd "C-<backspace>") 'anything-ff-run-toggle-auto-update)
    (define-key map (kbd "C-c ?")         'anything-read-file-name-help)
    (when anything-ff-lynx-style-map
      (define-key map (kbd "<left>")      'anything-find-files-down-one-level)
      (define-key map (kbd "<right>")     'anything-execute-persistent-action)
      (define-key map (kbd "C-o")         nil)
      (define-key map (kbd "<M-left>")    'anything-previous-source)
      (define-key map (kbd "<M-right>")   'anything-next-source))
    (delq nil map))
  "Keymap for `anything-c-read-file-name'.")

(defvar anything-generic-files-map
  (let ((map (copy-keymap anything-map)))
    (define-key map (kbd "M-g s")   'anything-ff-run-grep)
    (define-key map (kbd "M-g z")   'anything-ff-run-zgrep)
    (define-key map (kbd "M-g p")   'anything-ff-run-pdfgrep)
    (define-key map (kbd "M-D")     'anything-ff-run-delete-file)
    (define-key map (kbd "C-=")     'anything-ff-run-ediff-file)
    (define-key map (kbd "C-c =")   'anything-ff-run-ediff-merge-file)
    (define-key map (kbd "C-c o")   'anything-ff-run-switch-other-window)
    (define-key map (kbd "M-i")     'anything-ff-properties-persistent)
    (define-key map (kbd "C-c C-x") 'anything-ff-run-open-file-externally)
    (define-key map (kbd "C-w")     'anything-yank-text-at-point)
    (define-key map (kbd "C-c ?")   'anything-generic-file-help)
    map)
  "Generic Keymap for files.")

(defvar anything-c-grep-map
  (let ((map (copy-keymap anything-map)))
    (define-key map (kbd "M-<down>") 'anything-c-goto-next-file)
    (define-key map (kbd "M-<up>")   'anything-c-goto-precedent-file)
    (define-key map (kbd "C-c o")    'anything-c-grep-run-other-window-action)
    (define-key map (kbd "C-w")      'anything-yank-text-at-point)
    (define-key map (kbd "C-x C-s")  'anything-c-grep-run-save-buffer)
    (when anything-c-grep-use-ioccur-style-keys
      (define-key map (kbd "<right>")  'anything-c-grep-run-persistent-action)
      (define-key map (kbd "<left>")  'anything-c-grep-run-default-action))
    (define-key map (kbd "C-c ?")    'anything-grep-help)
    (delq nil map))
  "Keymap used in Grep sources.")

(defvar anything-c-pdfgrep-map
  (let ((map (copy-keymap anything-map)))
    (define-key map (kbd "M-<down>") 'anything-c-goto-next-file)
    (define-key map (kbd "M-<up>")   'anything-c-goto-precedent-file)
    (define-key map (kbd "C-w")      'anything-yank-text-at-point)
    (define-key map (kbd "C-c ?")    'anything-pdfgrep-help)
    map)
  "Keymap used in pdfgrep.")

(defvar anything-c-etags-map
  (let ((map (copy-keymap anything-map)))
    (define-key map (kbd "M-<down>") 'anything-c-goto-next-file)
    (define-key map (kbd "M-<up>")   'anything-c-goto-precedent-file)
    (define-key map (kbd "C-w")      'anything-yank-text-at-point)
    (define-key map (kbd "C-c ?")    'anything-etags-help)
    map)
  "Keymap used in Etags.")

(defvar anything-eval-expression-map
  (let ((map (copy-keymap anything-map)))
    (define-key map (kbd "<C-return>") 'anything-eval-new-line-and-indent)
    (define-key map (kbd "<tab>")      'lisp-indent-line)
    (define-key map (kbd "<C-tab>")    'lisp-complete-symbol)
    (define-key map (kbd "C-p")        'previous-line)
    (define-key map (kbd "C-n")        'next-line)
    (define-key map (kbd "<up>")       'previous-line)
    (define-key map (kbd "<down>")     'next-line)
    (define-key map (kbd "<right>")    'forward-char)
    (define-key map (kbd "<left>")     'backward-char)
    map))

(defvar anything-c-ucs-map
  (let ((map (copy-keymap anything-map)))
    (define-key map (kbd "<C-backspace>") 'anything-c-ucs-persistent-delete)
    (define-key map (kbd "<C-left>")      'anything-c-ucs-persistent-backward)
    (define-key map (kbd "<C-right>")     'anything-c-ucs-persistent-forward)
    (define-key map (kbd "<C-return>")    'anything-c-ucs-persistent-insert)
    (define-key map (kbd "C-c ?")         'anything-c-ucs-help)
    map)
  "Keymap for `anything-ucs'.")

(defvar anything-c-bookmark-map
  (let ((map (copy-keymap anything-map)))
    (define-key map (kbd "C-c o") 'anything-c-bookmark-run-jump-other-window)
    (define-key map (kbd "C-d")   'anything-c-bookmark-run-delete)
    (when (locate-library "bookmark-extensions")
      (define-key map (kbd "M-e") 'anything-c-bmkext-run-edit))
    (define-key map (kbd "C-c ?") 'anything-c-bookmark-help)
    (delq nil map))
  "Generic Keymap for emacs bookmark sources.")

(defvar anything-esh-on-file-map
  (let ((map (copy-keymap anything-map)))
    (define-key map (kbd "C-c ?")    'anything-esh-help)
    map)
  "Keymap for `anything-find-files-eshell-command-on-file'.")

(defvar anything-eshell-history-map
  (let ((map (copy-keymap anything-map)))
    (define-key map (kbd "M-p") 'anything-next-line)
    map)
  "Keymap for `anything-eshell-history'.")

(defvar anything-kill-ring-map
  (let ((map (copy-keymap anything-map)))
    (define-key map (kbd "M-y") 'anything-next-line)
    (define-key map (kbd "M-u") 'anything-previous-line)
    map)
  "Keymap for `anything-show-kill-ring'.")

(defvar anything-occur-map
  (let ((map (copy-keymap anything-map)))
    (define-key map (kbd "C-M-%") 'anything-occur-run-query-replace-regexp)
    map)
  "Keymap for `anything-occur'.")


;;; Embeded documentation.
;;
;;
(defun anything-c-list-preconfigured-anything ()
  "Collect preconfigured anything functions in this file."
  (loop with doc
        with sym
        for entry in (cdr (assoc
                           (file-truename (locate-library "anything-config"))
                           load-history))
        if (and (consp entry)
                (eq (car entry) 'defun)
                (string-match "^Preconfigured.+$"
                              (setq doc (or (documentation (setq sym (cdr entry)))
                                            ""))))
        collect (cons sym (match-string 0 doc))))

(defun anything-c-format-preconfigured-anything ()
  (mapcar (lambda (x) (format "\\[%s] : %s\n" (car x) (cdr x)))
          (anything-c-list-preconfigured-anything)))

;;; Global help message - Used by `anything-help'
;;
;;
(setq anything-help-message
      (lambda ()
        (concat
         "\\<anything-map>"
         "`anything' is QuickSilver-like candidate-selection framework.

Narrow the list by typing some pattern,
Multiple patterns are allowed by splitting by space.
Select with natural Emacs operations, choose with RET.

If you have any problems, press C-c C-x C-b!!
Feel free to send bug reports. I'll fix them.
The steps are described in the beginning of anything.el file.

== Basic Operations ==
C-p, Up: Previous Line
C-n, Down : Next Line
M-v, PageUp : Previous Page
C-v, PageDown : Next Page
Enter : Execute first (default) action / Select
M-< : First Line
M-> : Last Line
M-PageUp, C-M-S-v, C-M-y : Previous Page (other-window)
M-PageDown, C-M-v : Next Page (other-window)

Tab, C-i : Show action list
Left : Previous Source
Right, C-o : Next Source
C-k : Delete pattern
C-z : Persistent Action (Execute action with anything session kept)
C-c C-x C-b: Send a bug report

== Shortcuts For 2nd/3rd Action ==
\\[anything-select-2nd-action-or-end-of-line] : Execute 2nd Action (if the minibuffer cursor is at end of line)
\\[anything-select-3rd-action] : Execute 3rd Action

== Visible Marks ==
Visible marks store candidate. Some actions uses marked candidates.

\\[anything-toggle-visible-mark] : Toggle Visible Mark
\\[anything-prev-visible-mark] : Previous Mark
\\[anything-next-visible-mark] : Next Mark

== Miscellaneous Commands ==
\\[anything-toggle-resplit-window] : Toggle vertical/horizontal split anything window
\\[anything-quit-and-find-file] : Drop into `find-file'
\\[anything-delete-current-selection] : Delete Selected Item (visually)
\\[anything-kill-selection-and-quit] : Set Item Into the kill-ring And Quit
\\[anything-yank-selection] : Yank Selected Item Into Pattern
\\[anything-follow-mode] : Toggle Automatical Execution Of Persistent Action
\\[anything-force-update] : Recalculate And Redisplay Candidates

== Global Commands ==
\\<global-map>\\[anything-resume] revives last `anything' session.
It is very useful, so you should bind any key.

Single source is executed by \\[anything-call-source].

== Preconfigured `anything' ==
Preconfigured `anything' is commands that uses `anything' interface.
You can use them without configuration.

"
         (apply 'concat (anything-c-format-preconfigured-anything))
         "
Enjoy!")))

;;; `anything-buffer-list' help
;;
;;
(defvar anything-c-buffer-help-message
  "== Anything Buffer ==
\nTips:
You can enter a partial name of major-mode (e.g lisp, sh) to narrow down buffers.
Enter then a space and a pattern to narrow down to buffers matching this pattern. 
\nSpecific commands for `anything-buffers-list':
\\<anything-c-buffer-map>
\\[anything-buffer-run-zgrep]\t\t->Grep Buffer(s) works as zgrep too. (C-u grep all buffers but non--file buffers).
\\[anything-buffer-switch-other-window]\t\t->Switch other window.
\\[anything-buffer-switch-other-frame]\t\t->Switch other frame.
\\[anything-buffer-run-query-replace-regexp]\t\t->Query replace regexp in marked buffers.
\\[anything-buffer-run-query-replace]\t\t->Query replace in marked buffers.
\\[anything-buffer-switch-to-elscreen]\t\t->Find buffer in Elscreen.
\\[anything-buffer-run-ediff]\t\t->Ediff current buffer with candidate.  If two marked buffers ediff those buffers.
\\[anything-buffer-run-ediff-merge]\t\t->Ediff merge current buffer with candidate.  If two marked buffers ediff merge those buffers.
\\[anything-buffer-diff-persistent]\t\t->Toggle Diff buffer with saved file without quitting.
\\[anything-buffer-revert-persistent]\t\t->Revert buffer without quitting.
\\[anything-buffer-save-persistent]\t\t->Save buffer without quitting.
\\[anything-buffer-run-kill-buffers]\t\t->Delete marked buffers and quit.
\\[anything-toggle-all-marks]\t\t->Toggle all marks.
\\[anything-mark-all]\t\t->Mark all.
\\[anything-c-buffer-help]\t\t->Display this help.
\n== Anything Map ==
\\{anything-map}")

;;;###autoload
(defun anything-c-buffer-help ()
  "Help command for anything buffers."
  (interactive)
  (let ((anything-help-message anything-c-buffer-help-message))
    (anything-help)))

;;; Find files help (`anything-find-files')
;;
;;
(defvar anything-ff-help-message
  "== Anything Find Files ==
\nTips:
\n- Enter `~/' at end of pattern to quickly reach home directory.
- Enter `/' at end of pattern to quickly reach root of your file system.
- Enter `./' at end of pattern to quickly reach `default-directory' (initial start of session).
- You can complete with partial basename \(e.g \"fb\" will complete \"foobar\"\).
- Use `C-u C-z' to watch an image.
- To browse images directories turn on `anything-follow-mode' and navigate with arrow keys.
- When entered ediff, hitting `C-g' will ask you to use locate to find the file to ediff with.
 
\nSpecific commands for `anything-find-files':
\\<anything-find-files-map>
\\[anything-ff-run-locate]\t\t->Run Locate on basename of candidate (C-u to specify locate db).
\\[anything-ff-run-grep]\t\t->Run Grep (C-u Recursive).
\\[anything-ff-run-pdfgrep]\t\t->Run Pdfgrep on marked files.
\\[anything-ff-run-zgrep]\t\t->Run zgrep (C-u Recursive).
\\[anything-ff-run-etags]\t\t->Run Etags (C-u use thing-at-point `C-u C-u' reload cache)
\\[anything-ff-run-rename-file]\t\t->Rename File (C-u Follow).
\\[anything-ff-run-copy-file]\t\t->Copy File (C-u Follow).
\\[anything-ff-run-byte-compile-file]\t\t->Byte Compile File (C-u Load).
\\[anything-ff-run-load-file]\t\t->Load File.
\\[anything-ff-run-symlink-file]\t\t->Symlink File.
\\[anything-ff-run-hardlink-file]\t\t->Hardlink file.
\\[anything-ff-run-delete-file]\t\t->Delete File.
\\[anything-ff-run-kill-buffer-persistent]\t\t->Kill buffer candidate without quitting.
\\[anything-ff-persistent-delete]\t\t->Delete file without quitting.
\\[anything-ff-run-switch-to-eshell]\t\t->Switch to Eshell.
\\[anything-ff-run-eshell-command-on-file]\t\t->Eshell command on file (C-u Run on all marked files at once).
\\[anything-ff-run-ediff-file]\t\t->Ediff file.
\\[anything-ff-run-ediff-merge-file]\t\t->Ediff merge file.
\\[anything-ff-run-complete-fn-at-point]\t\t->Complete file name at point.
\\[anything-ff-run-switch-other-window]\t\t->Switch other window.
\\[anything-ff-run-switch-other-frame]\t\t->Switch other frame.
\\[anything-ff-run-open-file-externally]\t\t->Open file with external program (C-u to choose).
\\[anything-ff-rotate-left-persistent]\t\t->Rotate Image Left.
\\[anything-ff-rotate-right-persistent]\t\t->Rotate Image Right.
\\[anything-find-files-down-one-level]\t\t->Go down precedent directory.
\\[anything-ff-run-switch-to-history]\t\t->Switch to anything find-files history.
\\[anything-ff-properties-persistent]\t\t->Show file properties in a tooltip.
\\[anything-mark-all]\t\t->Mark all visibles candidates.
\\[anything-ff-run-toggle-auto-update]\t->Toggle auto expansion of directories.
\\[anything-unmark-all]\t\t->Unmark all candidates, visibles and invisibles.
\\[anything-ff-run-gnus-attach-files]\t\t->Gnus attach files to message buffer.
\\[anything-ff-run-print-file]\t\t->Print file, (C-u to refresh printers list).
\\[anything-enlarge-window]\t\t->Enlarge anything window.
\\[anything-narrow-window]\t\t->Narrow anything window.
\\[anything-ff-run-toggle-basename]\t\t->Toggle basename/fullpath.
\\[anything-send-bug-report-from-anything]\t\t->Send Bug report.
\\[anything-ff-help]\t\t->Display this help info.
\n== Anything Map ==
\\{anything-map}")

;;;###autoload
(defun anything-ff-help ()
  "Help command for `anything-find-files'."
  (interactive)
  (let ((anything-help-message anything-ff-help-message))
    (anything-help)))

;;; Help for `anything-c-read-file-name'
;;
;;
(defvar anything-read-file-name-help-message
  "== Anything read file name Map ==\
\nSpecific commands for anything-c-read-file-name:
\\<anything-c-read-file-map>
\\[anything-find-files-down-one-level]\t\t->Go down precedent directory.
\\[anything-ff-run-toggle-auto-update]\t->Toggle auto expansion of directories.
\\[anything-next-source]\t->Goto next source.
\\[anything-previous-source]\t->Goto previous source.
\\[anything-read-file-name-help]\t\t->Display this help info.
\n== Anything Map ==
\\{anything-map}")
  
;;;###autoload
(defun anything-read-file-name-help ()
  (interactive)
  (let ((anything-help-message anything-read-file-name-help-message))
    (anything-help)))

;;; Generic file help - Used by locate.
;;
;;
(defvar anything-generic-file-help-message
  "== Anything Generic files Map ==\
\nSpecific commands for anything locate and others files sources:
\\<anything-generic-files-map>
\\[anything-ff-run-grep]\t\t->Run grep (C-u recurse).
\\[anything-ff-run-pdfgrep]\t\t->Run Pdfgrep on marked files.
\\[anything-ff-run-delete-file]\t\t->Delete file.
\\[anything-ff-run-ediff-file]\t\t->Ediff file.
\\[anything-ff-run-ediff-merge-file]\t\t->Ediff merge file.
\\[anything-ff-run-switch-other-window]\t\t->Switch other window.
\\[anything-ff-properties-persistent]\t\t->Show file properties.
\\[anything-yank-text-at-point]\t\t->Yank text at point.
\\[anything-ff-run-open-file-externally]\t\t->Open file with external program (C-u to choose).
\nLocate tips:
You can add after writing search pattern any of the locate command line options.
e.g -b, -e, -n <number>...etc.
See Man locate for more infos.
\n== Anything Map ==
\\{anything-map}")

;;;###autoload
(defun anything-generic-file-help ()
  (interactive)
  (let ((anything-help-message anything-generic-file-help-message))
    (anything-help)))

;;; Grep help
;;
;;
(defvar anything-grep-help-message
  "== Anything Grep Map ==\
\nAnything Grep tips:
You can start grep with a prefix arg to recurse in subdirectories.
You can use wild card when selecting files (e.g *.el)
You can grep in many differents directories by marking files or wild cards.
You can save your results in a grep-mode buffer, see below.

\nSpecific commands for Anything Grep:
\\<anything-c-grep-map>
\\[anything-c-goto-next-file]\t->Next File.
\\[anything-c-goto-precedent-file]\t\t->Precedent File.
\\[anything-yank-text-at-point]\t\t->Yank Text at point in minibuffer.
\\[anything-c-grep-run-other-window-action]\t\t->Jump other window.
\\[anything-c-grep-run-persistent-action]\t\t->Run persistent action (Same as `C-z').
\\[anything-c-grep-run-default-action]\t\t->Run default action (Same as RET).
\\[anything-c-grep-run-save-buffer]\t\t->Save to a `grep-mode' enabled buffer.
\\[anything-grep-help]\t\t->Show this help.
\n== Anything Map ==
\\{anything-map}")
  
;;;###autoload
(defun anything-grep-help ()
  (interactive)
  (let ((anything-help-message anything-grep-help-message))
    (anything-help)))

;;; Pdf grep help
;;
;;
(defvar anything-pdfgrep-help-message
  "== Anything PdfGrep Map ==\
\nSpecific commands for Pdf Grep:
\\<anything-c-pdfgrep-map>
\\[anything-c-goto-next-file]\t->Next File.
\\[anything-c-goto-precedent-file]\t\t->Precedent File.
\\[anything-yank-text-at-point]\t\t->Yank Text at point in minibuffer.
\\[anything-pdfgrep-help]\t\t->Show this help.
\n== Anything Map ==
\\{anything-map}")
  
;;;###autoload
(defun anything-pdfgrep-help ()
  (interactive)
  (let ((anything-help-message anything-pdfgrep-help-message))
    (anything-help)))

;;; Etags help
;;
;;
(defvar anything-etags-help-message
  "== Anything Etags Map ==\
\nSpecific commands for Etags:
\\<anything-c-etags-map>
\\[anything-c-goto-next-file]\t->Next File.
\\[anything-c-goto-precedent-file]\t\t->Precedent File.
\\[anything-yank-text-at-point]\t\t->Yank Text at point in minibuffer.
\\[anything-etags-help]\t\t->Show this help.
\n== Anything Map ==
\\{anything-map}")

;;;###autoload
(defun anything-etags-help ()
  "The help function for etags."
  (interactive)
  (let ((anything-help-message anything-etags-help-message))
    (anything-help)))

;;; Ucs help
;;
;;
(defvar anything-c-ucs-help-message
  "== Anything Ucs ==
\nSpecific commands for `anything-ucs':
\\<anything-c-ucs-map>
\\[anything-c-ucs-persistent-insert]\t->Insert char.
\\[anything-c-ucs-persistent-forward]\t->Forward char.
\\[anything-c-ucs-persistent-backward]\t->Backward char.
\\[anything-c-ucs-persistent-delete]\t->Delete char backward.
\\[anything-c-ucs-help]\t\t->Show this help.

\n== Anything Map ==
\\{anything-map}")
  
(defun anything-c-ucs-help ()
  "Help command for `anything-ucs'."
  (interactive)
  (let ((anything-help-message anything-c-ucs-help-message))
    (anything-help)))

;;; Bookmark help
;;
;;
(defvar anything-bookmark-help-message
  "== Anything bookmark name Map ==\
\nSpecific commands for bookmarks:
\\<anything-c-bookmark-map>
\\[anything-c-bookmark-run-jump-other-window]\t\t->Jump other window.
\\[anything-c-bookmark-run-delete]\t\t->Delete bookmark.
\\[anything-c-bmkext-run-edit]\t\t->Edit bookmark (only for bmkext).
\\[anything-c-bookmark-help]\t\t->Run this help.
\n== Anything Map ==
\\{anything-map}")

(defun anything-c-bookmark-help ()
  "Help command for bookmarks."
  (interactive)
  (let ((anything-help-message anything-bookmark-help-message))
    (anything-help)))

;;; Eshell command on file help
;;
;;
(defvar anything-c-esh-help-message
  "== Anything eshell on file ==
\nTips:

- Passing extra args after filename:

Normally your command or alias will be called with file as argument.

e.g <command> 'candidate_file'

But you can also pass an argument or more after 'candidate_file' like this:

<command> %s [extra_args]\n

'candidate_file' will be inserted at '%s' and your command will look at this:

<command> 'candidate_file' [args]

- Specify many files as args (marked files):

e.g <command> file1 file2 ...

Please restart and use a prefix arg to call `anything-find-files-eshell-command-on-file'.
Otherwise your command will be called many times like this:

<command> file1 <command> file2 etc...

\nSpecific commands for `anything-find-files-eshell-command-on-file':
\\<anything-esh-on-file-map>
\\[anything-esh-help]\t\t->Display this help.
\n== Anything Map ==
\\{anything-map}")

(defun anything-esh-help ()
  "Help command for `anything-find-files-eshell-command-on-file'."
  (interactive)
  (let ((anything-help-message anything-c-esh-help-message))
    (anything-help)))


;;; Mode line strings
;;
;;
(defvar anything-buffer-mode-line-string
  '("Buffer(s)"
    "\\<anything-c-buffer-map>\
\\[anything-c-buffer-help]:Help, \
\\<anything-map>\
\\[anything-select-action]:Acts,\
\\[anything-exit-minibuffer]/\\[anything-select-2nd-action-or-end-of-line]/\
\\[anything-select-3rd-action]:NthAct,\
\\[anything-send-bug-report-from-anything]:BugReport."
    "String displayed in mode-line in `anything-c-source-buffers-list'"))

(defvar anything-ff-mode-line-string
  "\\<anything-find-files-map>\
\\[anything-ff-help]:Help, \
\\[anything-send-bug-report-from-anything]:BugReport, \
\\<anything-map>\
\\[anything-select-action]:Acts, \
\\[anything-exit-minibuffer]/\\[anything-select-2nd-action-or-end-of-line]/\
\\[anything-select-3rd-action]:NthAct"
  "String displayed in mode-line in `anything-c-source-find-files'")

(defvar anything-read-file-name-mode-line-string
  "\\<anything-c-read-file-map>\
\\[anything-read-file-name-help]:Help, \
\\<anything-map>\
\\[anything-select-action]:Acts,\
\\[anything-exit-minibuffer]/\\[anything-select-2nd-action-or-end-of-line]/\
\\[anything-select-3rd-action]:NthAct"
  "String displayed in mode-line in `anything-c-source-find-files'")

(defvar anything-generic-file-mode-line-string
  "\\<anything-generic-files-map>\
\\[anything-generic-file-help]:Help, \
\\<anything-map>\
\\[anything-select-action]:Acts,\
\\[anything-exit-minibuffer]/\\[anything-select-2nd-action-or-end-of-line]/\
\\[anything-select-3rd-action]:NthAct,\
\\[anything-send-bug-report-from-anything]:BugReport."
  "String displayed in mode-line in Locate.")

(defvar anything-grep-mode-line-string
  "\\<anything-c-grep-map>\
\\[anything-grep-help]:Help,\
\\<anything-map>\
\\[anything-select-action]:Acts,\
\\[anything-exit-minibuffer]/\\[anything-select-2nd-action-or-end-of-line]/\
\\[anything-select-3rd-action]:NthAct,\
\\[anything-send-bug-report-from-anything]:BugReport."
  "String displayed in mode-line in `anything-do-grep'.")

(defvar anything-pdfgrep-mode-line-string
  "\\<anything-c-pdfgrep-map>\
\\[anything-pdfgrep-help]:Help,\
\\<anything-map>\
\\[anything-select-action]:Acts,\
\\[anything-exit-minibuffer]/\\[anything-select-2nd-action-or-end-of-line]/\
\\[anything-select-3rd-action]:NthAct,\
\\[anything-send-bug-report-from-anything]:BugReport."
  "String displayed in mode-line in `anything-do-pdfgrep'.")

(defvar anything-etags-mode-line-string
  "\\<anything-c-etags-map>\
\\[anything-etags-help]:Help,\
\\<anything-map>\
\\[anything-select-action]:Acts,\
\\[anything-exit-minibuffer]/\\[anything-select-2nd-action-or-end-of-line]/\
\\[anything-select-3rd-action]:NthAct,\
\\[anything-send-bug-report-from-anything]:BugReport."
  "String displayed in mode-line in `anything-c-etags-select'.")


(defvar anything-c-ucs-mode-line-string
  "\\<anything-c-ucs-map>\
\\[anything-c-ucs-help]:Help, \
\\<anything-map>\
\\[anything-select-action]:Acts,\
\\[anything-exit-minibuffer]/\\[anything-select-2nd-action-or-end-of-line]/\
\\[anything-select-3rd-action]:NthAct."
  "String displayed in mode-line in `anything-ucs'.")

(defvar anything-bookmark-mode-line-string
  '("Bookmark(s)"
    "\\<anything-c-bookmark-map>\
\\[anything-c-bookmark-help]:Help, \
\\<anything-map>\
\\[anything-select-action]:Acts,\
\\[anything-exit-minibuffer]/\\[anything-select-2nd-action-or-end-of-line]/\
\\[anything-select-3rd-action]:NthAct,\
\\[anything-send-bug-report-from-anything]:BugReport."
    "String displayed in mode-line in `anything-c-source-buffers-list'"))

(defvar anything-occur-mode-line
  "\\<anything-map>\
\\[anything-help]:Help,\
\\<anything-occur-map>\
\\[anything-occur-run-query-replace-regexp]:Query replace regexp,\
\\<anything-map>\
\\[anything-select-action]:Acts,\
\\[anything-exit-minibuffer]/\\[anything-select-2nd-action-or-end-of-line]/\
\\[anything-select-3rd-action]:NthAct,\
\\[anything-send-bug-report-from-anything]:BugReport.")


;;; Utilities Functions
;;
;;
(defun anything-ff-find-printers ()
  "Return a list of available printers on Unix systems."
  (when (executable-find "lpstat")
    (let ((printer-list (with-temp-buffer
                          (call-process "lpstat" nil t nil "-a")
                          (split-string (buffer-string) "\n"))))
      (loop for p in printer-list
            for printer = (car (split-string p))
            when printer
            collect printer))))

;; Shut up byte compiler in emacs24*.
(defun anything-c-switch-to-buffer (buffer-or-name)
  "Same as `switch-to-buffer' whithout warnings at compile time."
  (with-no-warnings
    (switch-to-buffer buffer-or-name)))

(defun* anything-c-position (item seq &key (test 'eq) all)
  "A simple and faster replacement of CL `position'.
Return position of first occurence of ITEM found in SEQ.
Argument SEQ can be a string, in this case ITEM have to be a char.
Argument ALL, if non--nil specify to return a list of positions of
all ITEM found in SEQ."
  (let ((key (if (stringp seq) 'across 'in)))
    (eval
     `(loop for c ,key seq
            for index from 0
            when (funcall test c item)
            if all collect index into ls
            else return index
            finally return ls))))

(defun anything-c-get-pid-from-process-name (process-name)
  "Get pid from running process PROCESS-NAME."
  (loop with process-list = (list-system-processes)
        for pid in process-list
        for process = (assoc-default 'comm (process-attributes pid))
        when (and process (string-match process-name process))
        return pid))

(defun* anything-current-buffer-narrowed-p (&optional
                                            (buffer anything-current-buffer))
  "Check if BUFFER is narrowed.
Default is `anything-current-buffer'."
  (with-current-buffer buffer
    (let ((beg (point-min))
          (end (point-max))
          (total (buffer-size)))
      (or (/= beg 1) (/= end (1+ total))))))

(defun anything-region-active-p ()
  (and transient-mark-mode mark-active (/= (mark) (point))))

(defun anything-goto-char (loc)
  "Go to char, revealing if necessary."
  (goto-char loc)
  (when (or (eq major-mode 'org-mode)
            (and (boundp 'outline-minor-mode)
                 outline-minor-mode))
    (require 'org) ; On some old Emacs versions org may not be loaded.
    (org-reveal)))

(defun anything-goto-line (lineno &optional noanim)
  "Goto LINENO opening only outline headline if needed.
Animation is used unless NOANIM is non--nil."
  (goto-char (point-min))
  (anything-goto-char (point-at-bol lineno))
  (unless noanim
    (anything-match-line-color-current-line)
    (sit-for 0.3)
    (anything-match-line-cleanup)))

(defun anything-show-this-source-only ()
  "Show all candidates of this source."
  (interactive)
  (let (anything-candidate-number-limit)
    (anything-set-source-filter
     (list (assoc-default 'name (anything-get-current-source))))))

;;;###autoload
(defun anything-test-sources ()
  "List all anything sources for test.
The output is sexps which are evaluated by \\[eval-last-sexp]."
  (interactive)
  (with-output-to-temp-buffer "*Anything Test Sources*"
    (mapc (lambda (s) (princ (format ";; (anything '%s)\n" s)))
          (apropos-internal "^anything-c-source" #'boundp))
    (pop-to-buffer standard-output)))

(defun anything-displaying-source-names ()
  "Display sources name."
  (with-current-buffer anything-buffer
    (goto-char (point-min))
    (loop with pos
          while (setq pos (next-single-property-change (point) 'anything-header))
          do (goto-char pos)
          collect (buffer-substring-no-properties (point-at-bol)(point-at-eol))
          do (forward-line 1))))

;; [Obsolete]
(defun anything-select-source ()
  "[OBSOLETE] Select source."
  (interactive)
  (let ((default (assoc-default 'name (anything-get-current-source)))
        (source-names (anything-displaying-source-names))
        (all-source-names (mapcar (lambda (s) (assoc-default 'name s))
                                  (anything-get-sources))))
    (setq anything-candidate-number-limit 9999)
    (anything-aif
        (let (anything-source-filter)
          (anything-nest '(((name . "Anything Source")
                            (candidates . source-names)
                            (action . identity))
                           ((name . "Anything Source (ALL)")
                            (candidates . all-source-names)
                            (action . identity)))
                         nil "Source: " nil
                         default "*anything select source*"))
        (anything-set-source-filter (list it))
      (anything-set-source-filter nil))))

(defun anything-insert-string (str)
  "Insert STR."
  (anything-set-pattern str 'noupdate))

;;;###autoload
(defun anything-insert-buffer-name ()
  "Insert buffer name."
  (interactive)
  (anything-set-pattern
   (with-anything-current-buffer
     (if buffer-file-name (file-name-nondirectory buffer-file-name)
       (buffer-name)))))

(defalias 'anything-insert-symbol 'next-history-element)
(defalias 'anything-insert-selection 'anything-yank-selection)

(defun anything-c-match-on-file-name (candidate)
  "Return non-nil if `anything-pattern' match basename of filename CANDIDATE."
  (string-match anything-pattern (file-name-nondirectory candidate)))

(defun anything-c-match-on-directory-name (candidate)
  "Return non-nil if `anything-pattern' match directory part of CANDIDATE."
  (anything-aif (file-name-directory candidate)
      (string-match anything-pattern it)))

(defun anything-c-match-on-basename (candidate)
  "Return non-nil if `anything-pattern' match basename of filename CANDIDATE."
  (string-match anything-pattern (anything-c-basename candidate)))

(defun anything-c-string-match (candidate)
  "Return non-nil if `anything-pattern' match CANDIDATE.
The match is done with `string-match'."
  (string-match anything-pattern candidate))

(defun anything-c-skip-entries (list regexp)
  "Remove entries which matches REGEXP from LIST."
  (remove-if (lambda (x) (and (stringp x) (string-match regexp x)))
             list))

(defun anything-c-shadow-entries (list regexp)
  "Display elements of LIST matching REGEXP with the `file-name-shadow' face."
  (mapcar (lambda (file)
            ;; Add shadow face property to boring files.
            (let ((face (if (facep 'file-name-shadow)
                            'file-name-shadow
                            ;; fall back to default on XEmacs
                            'default)))
              (if (string-match regexp file)
                  (setq file (propertize file 'face face))))
            file)
          list))

(defsubst anything-c-stringify (str-or-sym)
  "Get string of STR-OR-SYM."
  (if (stringp str-or-sym)
      str-or-sym
      (symbol-name str-or-sym)))

(defsubst anything-c-symbolify (str-or-sym)
  "Get symbol of STR-OR-SYM."
  (if (symbolp str-or-sym)
      str-or-sym
      (intern str-or-sym)))

(defun anything-c-describe-function (func)
  "FUNC is symbol or string."
  (describe-function (anything-c-symbolify func)))

(defun anything-c-describe-variable (var)
  "VAR is symbol or string."
  (describe-variable (anything-c-symbolify var)))

(defun anything-c-find-function (func)
  "FUNC is symbol or string."
  (find-function (anything-c-symbolify func)))

(defun anything-c-find-variable (var)
  "VAR is symbol or string."
  (find-variable (anything-c-symbolify var)))

(defun anything-c-kill-new (candidate &optional replace)
  "CANDIDATE is symbol or string.
See `kill-new' for argument REPLACE."
  (kill-new (anything-c-stringify candidate) replace))

(defun* anything-fast-remove-dups (seq &key (test 'eq))
  "Remove duplicates elements in list SEQ.
This is same as `remove-duplicates' but with memoisation.
It is much faster, especially in large lists.
A test function can be provided with TEST argument key.
Default is `eq'."
  (loop with cont = (make-hash-table :test test)
        for elm in seq
        unless (gethash elm cont)
        do (puthash elm elm cont)
        finally return
        (loop for i being the hash-values in cont collect i)))

(defadvice eval-defun (after anything-source-hack activate)
  "Allow immediate execution of anything source when evaling it.
See `anything-c-enable-eval-defun-hack'."
  (when anything-c-enable-eval-defun-hack
    (let ((varsym (save-excursion
                    (beginning-of-defun)
                    (forward-char 1)
                    (when (memq (read (current-buffer)) '(defvar setq))
                      (read (current-buffer))))))
      (when (string-match "^anything-c-source-" (symbol-name varsym))
        (anything varsym)))))
;; (progn (ad-disable-advice 'eval-defun 'after 'anything-source-hack) (ad-update 'eval-defun))


;; Move this function from anything.el and redefine here
;; to avoid an unneeded defadvice.
(defun anything-quit-and-find-file ()
  "Drop into `anything-find-files' from `anything'.
If current selection is a buffer or a file, `anything-find-files'
from its directory."
  (interactive)
  (anything-run-after-quit
   (lambda (f)
     (if (file-exists-p f)
         (anything-find-files-1 (file-name-directory f)
                                (if anything-ff-transformer-show-only-basename
                                    (anything-c-basename f) f))
         (anything-find-files-1 f)))
   (anything-aif (get-buffer (anything-get-selection))
       (or (buffer-file-name it)
           (car (rassoc it dired-buffers))
           (and (with-current-buffer it
                  (eq major-mode 'org-agenda-mode))
                org-directory
                (expand-file-name org-directory))
           default-directory)
     (let ((sel (anything-get-selection)))
       (cond ((or (file-remote-p sel)
                  (file-exists-p sel))
              (expand-file-name sel))
             ((string-match ffap-url-regexp sel)
              sel)
             (t default-directory))))))


(defmacro* anything-c-walk-directory (directory &key path (directories t) match)
  "Walk through DIRECTORY tree.
PATH can be one of basename, relative, or full.
DIRECTORIES when non--nil (default) return also directories names, otherwise
skip directories names.
MATCH match only filenames matching regexp MATCH."
  `(let (result
         (fn (case ,path
               (basename 'file-name-nondirectory)
               (relative 'file-relative-name)
               (full     'identity)
               (t        'file-name-nondirectory))))
     (labels ((ls-R (dir)
                (loop with ls = (directory-files dir t directory-files-no-dot-files-regexp)
                      for f in ls
                      if (file-directory-p f)
                      do (progn (when ,directories
                                  (push (funcall fn f) result))
                                ;; Don't recurse in directory symlink.
                                (unless (file-symlink-p f)
                                  (ls-R f)))
                      else do 
                      (unless (and ,match (not (string-match ,match (file-name-nondirectory f))))
                        (push (funcall fn f) result)))))
       (ls-R ,directory)
       (nreverse result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Anything Applications ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Anything regexp.
;;
;;
(defvar anything-build-regexp-history nil)
(defun anything-c-query-replace-regexp (candidate)
  "Query replace regexp from `anything-regexp'.
With a prefix arg replace only matches surrounded by word boundaries,
i.e Don't replace inside a word, regexp is surrounded with \\bregexp\\b."
  (let ((regexp (funcall (anything-attr 'regexp))))
    (apply 'query-replace-regexp
           (anything-c-query-replace-args regexp))))

(defun anything-c-kill-regexp-as-sexp (candidate)
  "Kill regexp in a format usable in lisp code."
  (anything-c-regexp-kill-new
   (prin1-to-string (funcall (anything-attr 'regexp)))))

(defun anything-c-kill-regexp (candidate)
  "Kill regexp as it is in `anything-pattern'."
  (anything-c-regexp-kill-new (funcall (anything-attr 'regexp))))

(defun anything-c-query-replace-args (regexp)
  "create arguments of `query-replace-regexp' action in `anything-regexp'."
  (let ((region-only (anything-region-active-p)))
    (list
     regexp
     (query-replace-read-to regexp
                            (format "Query replace %sregexp %s"
                                    (if anything-current-prefix-arg "word " "")
                                    (if region-only "in region " ""))
                            t)
     anything-current-prefix-arg
     (when region-only (region-beginning))
     (when region-only (region-end)))))

(defvar anything-c-source-regexp
  '((name . "Regexp Builder")
    (init . (lambda ()
              (anything-candidate-buffer anything-current-buffer)))
    (candidates-in-buffer)
    (get-line . anything-c-regexp-get-line)
    (persistent-action . anything-c-regexp-persistent-action)
    (persistent-help . "Show this line")
    (multiline)
    (delayed)
    (requires-pattern . 2)
    (mode-line . "Press TAB to select action.")
    (regexp . (lambda () anything-input))
    (action . (("Kill Regexp as sexp" . anything-c-kill-regexp-as-sexp)
               ("Query Replace Regexp (C-u Not inside word.)"
                . anything-c-query-replace-regexp)
               ("Kill Regexp" . anything-c-kill-regexp)))))

(defun anything-c-regexp-get-line (s e)
  (propertize
   (apply 'concat
          ;; Line contents
          (format "%5d: %s" (line-number-at-pos (1- s)) (buffer-substring s e))
          ;; subexps
          (loop for i from 0 to (1- (/ (length (match-data)) 2))
                collect (format "\n         %s'%s'"
                                (if (zerop i) "Group 0: " (format "Group %d: " i))
                                (match-string i))))
   ;; match beginning
   ;; KLUDGE: point of anything-candidate-buffer is +1 than that of anything-current-buffer.
   ;; It is implementation problem of candidates-in-buffer.
   'anything-realvalue
   (1- s)))

(defun anything-c-regexp-persistent-action (pt)
  (anything-goto-char pt)
  (anything-persistent-highlight-point))

(defun anything-c-regexp-kill-new (input)
  (kill-new input)
  (message "Killed: %s" input))

(defun anything-quote-whitespace (candidate)
  "Quote whitespace, if some, in string CANDIDATE."
  (replace-regexp-in-string " " "\\\\ " candidate))


;;; Toggle all marks.
;;
;;
;;;###autoload
(defun anything-mark-all ()
  "Mark all visible unmarked candidates in current source."
  (interactive)
  (with-anything-window
    (save-excursion
      (goto-char (anything-get-previous-header-pos))
      (anything-next-line)
      (let* ((next-head (anything-get-next-header-pos))
             (end       (and next-head
                             (save-excursion
                               (goto-char next-head)
                               (forward-line -1)
                               (point))))
             (maxpoint  (or end (point-max))))
        (while (< (point) maxpoint)
          (anything-mark-current-line)
          (let* ((prefix (get-text-property (point-at-bol) 'display))
                 (cand   (anything-get-selection))
                 (bn     (and (anything-file-completion-source-p)
                              (anything-c-basename cand)))
                 (src    (assoc-default 'name (anything-get-current-source))))
            (when (and (not (anything-this-visible-mark))
                       (not (or (string= prefix "[?]")
                                (string= prefix "[@]"))))
              ;; Don't mark possibles directories ending with . or ..
              ;; autosave files/links and non--existent file.
              (unless
                  (and (or (anything-file-completion-source-p)
                           (equal src "Files from Current Directory"))
                       (or (string-match "^\\.#.*\\|^#.*#$\\|\\.$" bn)
                           ;; We need to test here when not using a transformer
                           ;; that tag prefix (i.e on tramp)
                           (not (file-exists-p cand))))
                (anything-make-visible-mark))))
          (forward-line 1) (end-of-line))))
    (anything-mark-current-line)
    (message "%s candidates marked" (length anything-marked-candidates))))

;;;###autoload
(defun anything-unmark-all ()
  "Unmark all candidates in all sources of current anything session."
  (interactive)
  (with-anything-window
    (let ((len (length anything-marked-candidates)))
      (save-excursion
        (anything-clear-visible-mark))
      (setq anything-marked-candidates nil)
      (anything-mark-current-line)
      (message "%s candidates unmarked" len))))

;;;###autoload
(defun anything-toggle-all-marks ()
  "Toggle all marks.
Mark all visible candidates of current source or unmark all candidates
visible or invisible in all sources of current anything session"
  (interactive)
  (let ((marked (anything-marked-candidates)))
    (if (and (>= (length marked) 1)
             (with-anything-window anything-visible-mark-overlays))
        (anything-unmark-all)
        (anything-mark-all))))



;;; Buffers
;;
;;
(defun anything-c-buffer-list ()
  "Return a list of buffer names.
The first buffer in the list will be the last recently used
buffer that is not the current buffer unless
`anything-allow-skipping-current-buffer' is nil."
  (let ((buffers (mapcar 'buffer-name (buffer-list))))
    (if anything-allow-skipping-current-buffer
        (progn
          (setq buffers (remove (buffer-name anything-current-buffer) buffers))
          (append (cdr buffers) (list (car buffers))))
        buffers)))

(defvar anything-c-source-buffers
  '((name . "Buffers")
    (candidates . anything-c-buffer-list)
    (type . buffer)))

(defvar anything-c-source-buffer-not-found
  `((name . "Create buffer")
    (dummy)
    (filtered-candidate-transformer (lambda (cands source)
                                      (list anything-pattern)))
    (keymap . ,anything-map)
    (action . (lambda (candidate)
                (anything-c-switch-to-buffer (get-buffer-create candidate))))))

;;; Buffers-list (was buffers+)
;;
;;
(defun anything-c-highlight-buffers (buffers)
  "Transformer function to highlight BUFFERS list.
Should be called after others transformers i.e (boring buffers)."
  (loop with buflist = (if anything-allow-skipping-current-buffer
                           buffers
                           (cons (pop (cdr buffers)) buffers))
        for i in buflist
        for buf = (get-buffer i)
        for bfname = (buffer-file-name buf)
        collect
        (cond (;; A dired buffer.
               (rassoc buf dired-buffers)
               (propertize i 'face 'anything-ff-directory
                           'help-echo (car (rassoc buf dired-buffers))))
              ;; A buffer file modified somewhere outside of emacs.
              ((and bfname (not (file-remote-p bfname))
                    (file-exists-p bfname)
                    (not (verify-visited-file-modtime buf)))
               (propertize i 'face 'anything-buffer-saved-out
                           'help-echo bfname))
              ;; A new buffer file not already saved on disk.
              ((and bfname (not (file-remote-p bfname))
                    (not (verify-visited-file-modtime buf)))
               (propertize i 'face 'anything-buffer-not-saved
                           'help-echo bfname))
              ;; A Remote buffer file modified and not saved on disk.
              ((and bfname (file-remote-p bfname) (buffer-modified-p buf))
               (let ((prefix (propertize
                              " " 'display
                              (propertize "@ " 'face 'anything-ff-prefix))))
                 (cons (concat prefix (propertize i 'face 'anything-ff-symlink
                                                  'help-echo bfname)) i)))
              ;; A buffer file modified and not saved on disk.
              ((and bfname (buffer-modified-p buf))
               (propertize i 'face 'anything-ff-symlink
                           'help-echo bfname))
              ;; A remote buffer file not modified and saved on disk.
              ((and bfname (file-remote-p bfname))
               (let ((prefix (propertize
                              " " 'display
                              (propertize "@ " 'face 'anything-ff-prefix))))
                 (cons (concat prefix (propertize i 'face 'font-lock-type-face
                                                  'help-echo bfname)) i)))
              ;; A buffer file not modified and saved on disk.
              (bfname
               (propertize i 'face 'font-lock-type-face
                           'help-echo bfname))
              ;; Any non--file buffer.
              (t (propertize i 'face 'italic)))))


(defvar anything-c-source-buffers-list
  `((name . "Buffers")
    (candidates . anything-c-buffer-list)
    (type . buffer)
    (match anything-c-buffer-match-major-mode)
    (candidate-transformer anything-c-skip-boring-buffers
                           anything-c-highlight-buffers)
    (persistent-action . anything-c-buffers-list-persistent-action)
    (keymap . ,anything-c-buffer-map)
    (volatile)
    (mode-line . anything-buffer-mode-line-string)
    (persistent-help . "Show this buffer / C-u \\[anything-execute-persistent-action]: Kill this buffer")))

(defvaralias 'anything-c-source-buffers+ 'anything-c-source-buffers-list)

(defun anything-c-buffer-match-major-mode (candidate)
  "Match maybe buffer by major-mode.
If you give a major-mode or partial major-mode,
it will list all buffers of this major-mode and/or buffers with name
matching this major-mode.
If you add a space after major-mode and then a space,
it will match all buffers of the major-mode
before space matching pattern after space.
If you give a pattern which doesn't match a major-mode, it will search buffer
with name matching pattern."
  (let* ((cand (replace-regexp-in-string "^\\s-\\{1\\}" "" candidate))
         (buf  (get-buffer cand)))
    (when buf
      (with-current-buffer buf
        (let ((mjm   (symbol-name major-mode))
              (split (split-string anything-pattern)))
          (cond ((string-match "\\s-$" anything-pattern)
                 (string-match (car split) mjm))
                ((string-match "\\s-" anything-pattern)
                 (and (string-match (car split) mjm)
                      (string-match (cadr split) cand)))
                (t (or (string-match anything-pattern mjm)
                       (string-match anything-pattern cand)))))))))

(defun anything-c-buffer-query-replace-1 (&optional regexp-flag)
  "Query replace in marked buffers.
If REGEXP-FLAG is given use `query-replace-regexp'."
  (let ((fn     (if regexp-flag 'query-replace-regexp 'query-replace))
        (prompt (if regexp-flag "Query replace regexp" "Query replace"))
        (bufs   (anything-marked-candidates)))
    (loop 
          with replace = (query-replace-read-from prompt regexp-flag)
          with tostring = (unless (consp replace)
                            (query-replace-read-to
                             replace prompt regexp-flag))
          for buf in bufs
          do
          (save-window-excursion
            (anything-c-switch-to-buffer buf)
            (save-excursion
              (let ((case-fold-search t))
                (goto-char (point-min))
                (if (consp replace)
                    (apply fn (list (car replace) (cdr replace)))
                    (apply fn (list replace tostring)))))))))

(defun anything-c-buffer-query-replace-regexp (candidate)
  (anything-c-buffer-query-replace-1 'regexp))

(defun anything-c-buffer-query-replace (candidate)
  (anything-c-buffer-query-replace-1))

(defun anything-buffer-toggle-diff (candidate)
  "Toggle diff buffer CANDIDATE with it's file."
  (if (get-buffer-window "*Diff*")
      (kill-buffer "*Diff*")
      (diff-buffer-with-file (get-buffer candidate))))

;;;###autoload
(defun anything-buffer-diff-persistent ()
  "Toggle diff buffer without quitting anything."
  (interactive)
  (anything-attrset 'diff-action 'anything-buffer-toggle-diff)
  (anything-execute-persistent-action 'diff-action))

(defun anything-buffer-revert-and-update (candidate)
  (let ((marked (anything-marked-candidates)))
    (loop for buf in marked do (anything-revert-buffer buf))
    (anything-force-update candidate)))

;;;###autoload
(defun anything-buffer-revert-persistent ()
  "Revert buffer without quitting anything."
  (interactive)
  (anything-attrset 'revert-action 'anything-buffer-revert-and-update)
  (anything-execute-persistent-action 'revert-action 'onewindow))

(defun anything-buffer-save-and-update (candidate)
  (let ((marked (anything-marked-candidates))
        (enable-recursive-minibuffers t))
    (loop for buf in marked do
          (with-current-buffer (get-buffer buf)
            (save-buffer)))
    (anything-force-update candidate)))

;;;###autoload
(defun anything-buffer-save-persistent ()
  "Save buffer without quitting anything."
  (interactive)
  (anything-attrset 'save-action 'anything-buffer-save-and-update)
  (anything-execute-persistent-action 'save-action 'onewindow))

;;;###autoload
(defun anything-buffer-run-kill-buffers ()
  "Run kill buffer action from `anything-c-source-buffers-list'."
  (interactive)
  (anything-c-quit-and-execute-action 'anything-kill-marked-buffers))

;;;###autoload
(defun anything-buffer-run-grep ()
  "Run Grep action from `anything-c-source-buffers-list'."
  (interactive)
  (anything-c-quit-and-execute-action 'anything-c-grep-buffers))

;;;###autoload
(defun anything-buffer-run-zgrep ()
  "Run Grep action from `anything-c-source-buffers-list'."
  (interactive)
  (anything-c-quit-and-execute-action 'anything-c-zgrep-buffers))

;;;###autoload
(defun anything-buffer-run-query-replace-regexp ()
  "Run Query replace regexp action from `anything-c-source-buffers-list'."
  (interactive)
  (anything-c-quit-and-execute-action 'anything-c-buffer-query-replace-regexp))

;;;###autoload
(defun anything-buffer-run-query-replace ()
  "Run Query replace action from `anything-c-source-buffers-list'."
  (interactive)
  (anything-c-quit-and-execute-action 'anything-c-buffer-query-replace))

;;;###autoload
(defun anything-buffer-switch-other-window ()
  "Run switch to other window action from `anything-c-source-buffers-list'."
  (interactive)
  (anything-c-quit-and-execute-action 'switch-to-buffer-other-window))

;;;###autoload
(defun anything-buffer-switch-other-frame ()
  "Run switch to other frame action from `anything-c-source-buffers-list'."
  (interactive)
  (anything-c-quit-and-execute-action 'switch-to-buffer-other-frame))

;;;###autoload
(defun anything-buffer-switch-to-elscreen ()
  "Run switch to elscreen  action from `anything-c-source-buffers-list'."
  (interactive)
  (anything-c-quit-and-execute-action 'anything-find-buffer-on-elscreen))

;;;###autoload
(defun anything-buffer-run-ediff ()
  "Run ediff action from `anything-c-source-buffers-list'."
  (interactive)
  (anything-c-quit-and-execute-action 'anything-ediff-marked-buffers))

(defun anything-buffer-run-ediff-merge ()
  "Run ediff action from `anything-c-source-buffers-list'."
  (interactive)
  (anything-c-quit-and-execute-action 'anything-ediff-marked-buffers-merge))

(defun anything-c-buffers-persistent-kill (buffer)
  "Persistent action to kill buffer."
  (with-current-buffer (get-buffer buffer)
    (if (and (buffer-modified-p)
             (buffer-file-name (current-buffer)))
        (progn
          (save-buffer)
          (kill-buffer buffer))
        (kill-buffer buffer)))
  (anything-delete-current-selection))

(defun anything-c-buffers-list-persistent-action (candidate)
  (if current-prefix-arg
      (anything-c-buffers-persistent-kill candidate)
      (anything-c-switch-to-buffer candidate)))


;;;; <File>
;;
;;
;;; File name history
(defvar anything-c-source-file-name-history
  '((name . "File Name History")
    (candidates . file-name-history)
    (match anything-c-match-on-basename)
    (type . file)))

;;; Files in current dir
;;
;;
(defvar anything-c-source-files-in-current-dir
  '((name . "Files from Current Directory")
    (candidates . (lambda ()
                    (with-anything-current-buffer
                      (directory-files (anything-c-current-directory)))))
    ;; volatile is not needed, I think.
    (type . file)))

(defun anything-c-highlight-files (files)
  (loop for i in files
        if (file-directory-p i)
        collect (propertize (file-name-nondirectory i)
                            'face 'anything-ff-directory
                            'help-echo (expand-file-name i))
        else
        collect (propertize (file-name-nondirectory i)
                            'face 'anything-ff-file
                            'help-echo (expand-file-name i))))

(defvar anything-c-source-files-in-current-dir+
  `((name . "Files from Current Directory")
    (candidates . (lambda ()
                    (with-anything-current-buffer
                      (directory-files (anything-c-current-directory) t))))
    (keymap . ,anything-generic-files-map)
    (help-message . anything-generic-file-help-message)
    (mode-line . anything-generic-file-mode-line-string)
    (candidate-transformer anything-c-highlight-files)
    ;; volatile is not needed, I think.
    (type . file)))



;;; Anything-find-files - The anything files browser.
;;
;;
;; Internal.
(defvar anything-c-find-files-doc-header " (`C-l': Go to precedent level)"
  "*The doc that is inserted in the Name header of a find-files or dired source.")
(defvar anything-ff-auto-update-flag nil
  "Internal, flag to turn on/off auto-update in `anything-find-files'.
Don't set it directly, use instead `anything-ff-auto-update-initial-value'.")
(defvar anything-ff-last-expanded nil
  "Store last expanded directory or file.")
(defvar anything-ff-default-directory nil)
(defvar anything-ff-history nil)
(defvar anything-ff-cand-to-mark nil)
(defvar anything-ff-url-regexp
  "\\`\\(news\\(post\\)?:\\|nntp:\\|mailto:\\|file:\\|\\(ftp\\|https?\\|telnet\\|gopher\\|www\\|wais\\):/?/?\\).*"
  "Same as `ffap-url-regexp' but match earlier possible url.")

(defvar anything-c-source-find-files
  `((name . "Find Files")
    (header-name . (lambda (name)
                     (concat name anything-c-find-files-doc-header)))
    ;; It is needed for filenames with capital letters
    (disable-shortcuts)
    (init . (lambda ()
              (setq anything-ff-auto-update-flag
                    anything-ff-auto-update-initial-value)))
    (candidates . anything-find-files-get-candidates)
    (filtered-candidate-transformer anything-c-find-files-transformer)
    (persistent-action . anything-find-files-persistent-action)
    (persistent-help . "Hit1 Expand Candidate, Hit2 or (C-u) Find file")
    (mode-line . anything-ff-mode-line-string)
    (volatile)
    (candidate-number-limit . 9999)
    (action-transformer . anything-find-files-action-transformer)
    (action
     . ,(delq
         nil
         `(("Find File" . anything-c-find-file-or-marked)
           ("Find file in Dired" . anything-c-point-file-in-dired)
           ,(and (locate-library "elscreen")
                 '("Find file in Elscreen"  . anything-elscreen-find-file))
           ,(and (locate-library "popwin")
                 '("Find file in popup window" . popwin:find-file))
           ("Checksum File" . anything-ff-checksum)
           ("Complete at point `M-tab'"
            . anything-c-insert-file-name-completion-at-point)
           ("Open file externally `C-c C-x, C-u to choose'"
            . anything-c-open-file-externally)
           ("Grep File(s) `M-g s, C-u Recurse'" . anything-find-files-grep)
           ("Zgrep File(s) `M-g z, C-u Recurse'" . anything-ff-zgrep)
           ("Switch to Eshell `M-e'" . anything-ff-switch-to-eshell)
           ("Etags `M-., C-u tap, C-u C-u reload tag file'" . anything-ff-etags-select)
           ("Eshell command on file(s) `M-!, C-u run on all marked at once.'"
            . anything-find-files-eshell-command-on-file)
           ("Find file as root" . anything-find-file-as-root)
           ("Find file in hex dump" . hexl-find-file)
           ("Ediff File `C-='" . anything-find-files-ediff-files)
           ("Ediff Merge File `C-c ='" . anything-find-files-ediff-merge-files)
           ("Delete File(s) `M-D'" . anything-delete-marked-files)
           ("Copy file(s) `M-C, C-u to follow'" . anything-find-files-copy)
           ("Copy file(s) Async" . anything-ff-copy-async)
           ("Rename file(s) `M-R, C-u to follow'" . anything-find-files-rename)
           ("Serial rename files" . anything-ff-serial-rename)
           ("Serial rename by symlinking files" . anything-ff-serial-rename-by-symlink)
           ("Serial rename by copying files" . anything-ff-serial-rename-by-copying)
           ("Symlink files(s) `M-S, C-u to follow'" . anything-find-files-symlink)
           ("Relsymlink file(s) `C-u to follow'" . anything-find-files-relsymlink)
           ("Hardlink file(s) `M-H, C-u to follow'" . anything-find-files-hardlink)
           ("Find file other window `C-o'" . find-file-other-window)
           ("Switch to history `M-p'" . anything-find-files-switch-to-hist)
           ("Find file other frame `C-c C-o'" . find-file-other-frame)
           ("Print File `C-c p, C-u to refresh'" . anything-ff-print)
           ("Locate `C-x C-f, C-u to specify locate db'" . anything-ff-locate))))))

(defun anything-find-files-set-prompt-for-action (action files)
  "Set prompt for action ACTION for FILES."
  (let ((len (length files)))
    (format "%s *%s File(s)\n%s to: "
            action len
            (mapconcat (lambda (f)
                         (format "- %s\n" f)) files ""))))

(defun anything-dwim-target-directory ()
  "Return value of `default-directory' of buffer in other window.
If there is only one window return the value ot `default-directory'
for current buffer."
  (with-anything-current-buffer
    (let ((num-windows (length (window-list))))
      (if (> num-windows 1)
          (save-selected-window
            (other-window 1)
            default-directory)
          (car anything-ff-history)))))

(defun anything-find-files-do-action (action)
  "Generic function for creating action from `anything-c-source-find-files'.
ACTION must be an action supported by `anything-dired-action'."
  (let* ((ifiles (mapcar 'expand-file-name ; Allow modify '/foo/.' -> '/foo'
                         (anything-marked-candidates)))
         (cand   (anything-get-selection)) ; Target
         (prompt (anything-find-files-set-prompt-for-action
                  (capitalize (symbol-name action)) ifiles))
         (parg   anything-current-prefix-arg)
         (dest   (anything-c-read-file-name
                  prompt
                  :preselect (if anything-ff-transformer-show-only-basename
                                 (anything-c-basename cand) cand)
                  :initial-input (anything-dwim-target-directory)
                  :history (anything-find-files-history :comp-read nil))))
    (anything-dired-action
     dest :files ifiles :action action :follow parg)))

(defun anything-find-files-copy (candidate)
  "Copy files from `anything-find-files'."
  (anything-find-files-do-action 'copy))

(defun anything-find-files-rename (candidate)
  "Rename files from `anything-find-files'."
  (anything-find-files-do-action 'rename))

(defun anything-find-files-symlink (candidate)
  "Symlink files from `anything-find-files'."
  (anything-find-files-do-action 'symlink))

(defun anything-find-files-relsymlink (candidate)
  "Relsymlink files from `anything-find-files'."
  (anything-find-files-do-action 'relsymlink))

(defun anything-find-files-hardlink (candidate)
  "Hardlink files from `anything-find-files'."
  (anything-find-files-do-action 'hardlink))

(defun anything-find-files-byte-compile (candidate)
  "Byte compile elisp files from `anything-find-files'."
  (let ((files    (anything-marked-candidates))
        (parg     anything-current-prefix-arg))
    (loop for fname in files
          do (byte-compile-file fname parg))))

(defun anything-find-files-load-files (candidate)
  "Load elisp files from `anything-find-files'."
  (let ((files    (anything-marked-candidates)))
    (loop for fname in files
          do (load fname))))

(defun anything-find-files-ediff-files-1 (candidate &optional merge)
  "Generic function to ediff/merge files in `anything-find-files'."
  (let ((bname  (anything-c-basename candidate))
        (prompt (if merge "Ediff Merge `%s' With File: "
                    "Ediff `%s' With File: "))
        (fun    (if merge 'ediff-merge-files 'ediff-files))) 
    (funcall fun
             candidate
             (condition-case quit
                 (anything-c-read-file-name
                  (format prompt bname))
               (quit ;; Hit C-g ask user to fallback to locate.
                (if (y-or-n-p "Search file for ediff with locate? ")
                    (anything-c-locate-read-file-name
                     (format prompt bname)
                     ;; Check if -b option is available.
                     (if (and (eq system-type 'windows-nt)
                              (string-match "^es" anything-c-locate-command))
                         bname
                         (concat bname " -b")))
                    (error "Error: Ediff Operation aborted")))))))

(defun anything-find-files-ediff-files (candidate)
  (anything-find-files-ediff-files-1 candidate))

(defun anything-find-files-ediff-merge-files (candidate)
  (anything-find-files-ediff-files-1 candidate 'merge))

(defun anything-find-files-grep (candidate)
  "Default action to grep files from `anything-find-files'."
  (anything-do-grep-1 (anything-marked-candidates)
                      anything-current-prefix-arg))

(defun anything-ff-zgrep (candidate)
  "Default action to zgrep files from `anything-find-files'."
  (let ((prefarg anything-current-prefix-arg)
        (ls      (anything-marked-candidates)))
    (anything-ff-zgrep-1 ls prefarg)))

(defun anything-ff-pdfgrep (candidate)
  "Default action to pdfgrep files from `anything-find-files'."
  (let ((cands (loop for file in (anything-marked-candidates)
                     if (or (string= (file-name-extension file) "pdf")
                            (string= (file-name-extension file) "PDF"))
                     collect file))
        (anything-c-pdfgrep-default-function 'anything-c-pdfgrep-init))
    (when cands
      (anything-do-pdfgrep-1 cands))))

(defun anything-ff-etags-select (candidate)
  "Default action to jump to etags from `anything-find-files'."
  (when (get-buffer anything-action-buffer)
    (kill-buffer anything-action-buffer))
  (let ((default-directory anything-ff-default-directory))
    (anything-c-etags-select anything-current-prefix-arg)))

(defun anything-find-files-switch-to-hist (candidate)
  "Switch to anything-find-files history."
  (anything-find-files t))

;;; Asynchronous copy of files.
;;
;;
(defun anything-c-copy-files-async-1 (flist dest)
  "Copy a list of Files FLIST to DEST asynchronously.
It use another emacs process to do the job.
Communication with background emacs is done with temp file
`anything-c-copy-files-async-log-file'."
  (start-file-process "emacs-batch" nil anything-c-copy-async-prefered-emacs
                      "-Q" "--batch" "--eval"
                      (format "(progn
  (require 'dired) (require 'cl)
  (let ((dired-recursive-copies 'always)
        failures success
        (ovw-count 0)
        (cpf-count 0))
    (dolist (f '%S)
       (condition-case err
             (let ((file-exists (file-exists-p
                                 (expand-file-name
                                  (file-name-nondirectory (directory-file-name f))
                                   (file-name-directory
                                     (file-name-as-directory \"%s\"))))))
                (dired-copy-file f \"%s\" t)
                (if file-exists
                    (progn (push (cons \"Overwriting\" f) success)
                           (incf ovw-count))
                    (push (cons \"Copying\" f) success)
                    (incf cpf-count)))
          (file-error
           (push (dired-make-relative
                   (expand-file-name
                     (file-name-nondirectory (directory-file-name f))
                     (file-name-directory \"%s\")))
                 failures))))
    (with-current-buffer (find-file-noselect \"%s\")
       (erase-buffer)
       (when failures
         (dolist (fail (reverse failures))
           (insert (concat \"Failed to copy \" fail \"\n\"))))
       (when success
         (loop for (a . s) in (reverse success) do
           (insert (concat a \" \" s  \" to %s done\n\"))))
       (and (/= cpf-count 0) (insert (concat (int-to-string cpf-count) \" File(s) Copied\n\")))
       (and (/= ovw-count 0) (insert (concat (int-to-string ovw-count) \" File(s) Overwrited\n\")))
       (and failures (insert (concat (int-to-string (length failures)) \" File(s) Failed to copy\n\")))
       (save-buffer))))"
                              flist dest dest dest anything-c-copy-files-async-log-file dest)))

(defun anything-c-copy-async-with-log (flist dest)
  "Copy file list FLIST to DEST showing log.
Log is send to `anything-c-copy-files-async-log-file'.
Copying is done asynchronously with `anything-c-copy-files-async-1'."
  (declare (special auto-revert-interval))
  (pop-to-buffer (find-file-noselect anything-c-copy-files-async-log-file))
  (set (make-local-variable 'auto-revert-interval) 1)
  (erase-buffer)
  (insert "Wait copying files...\n")
  (sit-for 0.5) (save-buffer)
  (goto-char (point-max))
  (auto-revert-mode 1)
  (anything-c-copy-files-async-1 flist dest))

(defun anything-ff-copy-async (candidate)
  "Anything find files action to copy files async.
Copying is done asynchronously with `anything-c-copy-files-async-1'."
  (let* ((flist (anything-marked-candidates))
         (dest  (anything-c-read-file-name
                 (anything-find-files-set-prompt-for-action
                  "Copy Async" flist)
                 :preselect candidate
                 :initial-input (car anything-ff-history)
                 :history (anything-find-files-history :comp-read nil))))
    (anything-c-copy-async-with-log flist dest)))

(defvar eshell-command-aliases-list nil)
(defvar anything-eshell-command-on-file-input-history nil)
(defun anything-find-files-eshell-command-on-file-1 (candidate &optional map)
  "Run `eshell-command' on CANDIDATE or marked candidates.
This is done possibly with an eshell alias, if no alias found, you can type in
an eshell command.

Basename of CANDIDATE can be a wild-card.
e.g you can do \"eshell-command command *.el\"
Where \"*.el\" is the CANDIDATE.

It is possible to do eshell-command command <CANDIDATE> <some more args>
like this: \"command %s some more args\".

If MAP is given run `eshell-command' on all marked files at once,
Otherwise, run `eshell-command' on each marked files.
In other terms, with a prefix arg do on the three marked files
\"foo\" \"bar\" \"baz\":

\"eshell-command command foo bar baz\"

otherwise do

\"eshell-command command foo\"
\"eshell-command command bar\"
\"eshell-command command baz\"

Note:
If `eshell' or `eshell-command' have not been run once,
or if you have no eshell aliases `eshell-command-aliases-list'
will not be loaded first time you use this."
  (when (or eshell-command-aliases-list
            (y-or-n-p "Eshell is not loaded, run eshell-command without alias anyway? "))
    (and eshell-command-aliases-list (eshell-read-aliases-list))
    (let* ((cand-list (anything-marked-candidates))
           (default-directory (or anything-ff-default-directory
                                  ;; If candidate is an url *-ff-default-directory is nil
                                  ;; so keep value of default-directory.
                                  default-directory))
           (command (anything-comp-read
                     "Command: "
                     (loop for (a . c) in eshell-command-aliases-list
                           when (string-match "\\(\\$1\\|\\$\\*\\)$" (car c))
                           collect (propertize a 'help-echo (car c)) into ls
                           finally return (sort ls 'string<))
                     :buffer "*esh command on file*"
                     :name "Eshell command"
                     :keymap anything-esh-on-file-map
                     :mode-line
                     '("Eshell alias"
                       "C-c ?: Help, \\[universal-argument]: Insert output at point")
                     :input-history
                     'anything-eshell-command-on-file-input-history))
           (alias-value (car (assoc-default command eshell-command-aliases-list))))
      (when (and (= (length cand-list) 1)
                 (string-match "[*]" (anything-c-basename (car cand-list))))
        (setq cand-list (file-expand-wildcards (car cand-list) t)))
      ;; Be sure output don't go in current buffer
      ;; but allow sending output to current buffer
      ;; if a prefix arg have been passed during the
      ;; `anything-comp-read' call.
      (setq current-prefix-arg anything-current-prefix-arg)
      ;; MAP have been set before calling `anything-comp-read' 
      ;; by `anything-current-prefix-arg'.
      (if (and (or map ; prefix-arg
                   (and alias-value
                        ;; If command is an alias be sure it accept
                        ;; more than one arg i.e $*.
                        (string-match "\\$\\*$" alias-value)))
               (> (length cand-list) 1))
          
          ;; Run eshell-command with ALL marked files as arguments.
          (let ((mapfiles (mapconcat 'shell-quote-argument cand-list " ")))
            (if (string-match "'%s'\\|\"%s\"\\|%s" command)
                (eshell-command (format command mapfiles)) ; See [1]
                (eshell-command (format "%s %s" command mapfiles))))
          
          ;; Run eshell-command on EACH marked files.
          (loop for i in cand-list
                for bn = (anything-c-basename i)
                for files = (format "'%s'" i)
                for com = (if (string-match "'%s'\\|\"%s\"\\|%s" command)
                              ;; [1] This allow to enter other args AFTER filename
                              ;; i.e <command %s some_more_args>
                              (format command files)
                              (format "%s %s" command files))
                do (eshell-command com))))))

(defun anything-find-files-eshell-command-on-file (candidate)
  "Run `eshell-command' on CANDIDATE or marked candidates.
See `anything-find-files-eshell-command-on-file-1' for more info."
  (anything-find-files-eshell-command-on-file-1
   candidate anything-current-prefix-arg))

(defun anything-ff-switch-to-eshell (candidate)
  "Switch to eshell and cd to `anything-ff-default-directory'."
  (flet ((cd-eshell ()
           (goto-char (point-max))
           (insert
            (format "cd '%s'" anything-ff-default-directory))
           (eshell-send-input)))
    (if (get-buffer "*eshell*")
        (progn
          (anything-c-switch-to-buffer "*eshell*")
          (cd-eshell))
        (call-interactively 'eshell)
        (cd-eshell))))

(defun anything-ff-serial-rename-action (method)
  "Rename all marked files to `anything-ff-default-directory' with METHOD.
See `anything-ff-serial-rename-1'."
  (let* ((cands     (anything-marked-candidates))
         (def-name  (car cands))
         (name      (read-string "NewName: "
                                 (replace-regexp-in-string
                                  "[0-9]+$" ""
                                  (anything-c-basename
                                   def-name
                                   (file-name-extension def-name)))))
         (start     (read-number "StartAtNumber: "))
         (extension (read-string "Extension: "
                                 (file-name-extension (car cands))))
         (dir       (expand-file-name
                     (anything-c-read-file-name
                      "Serial Rename to directory: "
                      :initial-input
                      (expand-file-name anything-ff-default-directory)
                      :test 'file-directory-p
                      :must-match t)))
         (res       (loop for f in cands
                          for bn = (anything-c-basename f)
                          for count from start
                          concat (format "%s <-> %s%s.%s\n"
                                         bn name count extension))))
    (if (y-or-n-p
         (format "Result:\n %sRename like this to <%s> ? " res dir))
        (progn
          (anything-ff-serial-rename-1
           dir cands name start extension :method method)
          (message nil)
          (anything-find-files-1 dir))
        (message "Operation aborted"))))

(defun anything-ff-member-directory-p (file directory)
  (let ((dir-file (expand-file-name
                   (file-name-as-directory (file-name-directory file))))
        (cur-dir  (expand-file-name (file-name-as-directory directory))))
    (string= dir-file cur-dir)))

(defun* anything-ff-serial-rename-1
    (directory collection new-name start-at-num extension &key (method 'rename))
  "rename files in COLLECTION to DIRECTORY with the prefix name NEW-NAME.
Rename start at number START-AT-NUM - ex: prefixname-01.jpg.
EXTENSION is the file extension to use, in empty prompt,
reuse the original extension of file.
METHOD can be one of rename, copy or symlink.
Files will be renamed if they are files of current directory, otherwise they
will be treated with METHOD.
Default METHOD is rename."
  ;; Maybe remove directories selected by error in collection.
  (setq collection (remove-if 'file-directory-p collection))
  (flet ((symlink-file (file dest)
           (let ((flist (list file)))
             (anything-dired-action
              dest :action 'symlink :files flist))))

    (let* ((tmp-dir  (file-name-as-directory
                      (concat (file-name-as-directory directory)
                              (symbol-name (gensym "tmp")))))
           (fn       (case method
                       (copy    'copy-file)
                       (symlink 'symlink-file)
                       (rename  'rename-file)
                       (t (error "Error: Unknow method %s" method)))))
      (make-directory tmp-dir)
      (unwind-protect
           (progn
             ;; Rename all files to tmp-dir with new-name.
             ;; If files are not from start directory, use method
             ;; to move files to tmp-dir.
             (loop for i in collection
                   for count from start-at-num
                   for fnum = (if (< count 10) "0%s" "%s")
                   for nname = (concat tmp-dir new-name (format fnum count)
                                       (if (not (string= extension ""))
                                           (format ".%s" (replace-regexp-in-string
                                                          "[.]" "" extension))
                                           (file-name-extension i 'dot)))
                   do (if (anything-ff-member-directory-p i directory)
                          (rename-file i nname)
                          (funcall fn i nname)))
             ;; Now move all from tmp-dir to destination.
             (loop with dirlist = (directory-files
                                   tmp-dir t directory-files-no-dot-files-regexp)
                   for f in dirlist do
                   (if (file-symlink-p f)
                       (symlink-file (file-truename f)
                                     (concat (file-name-as-directory directory)
                                             (anything-c-basename f)))
                       (rename-file f directory))))
        (delete-directory tmp-dir t)))))

(defun anything-ff-serial-rename (candidate)
  "Serial rename all marked files to `anything-ff-default-directory'.
Rename only file of current directory, and symlink files coming from
other directories.
See `anything-ff-serial-rename-1'."
  (anything-ff-serial-rename-action 'rename))

(defun anything-ff-serial-rename-by-symlink (candidate)
  "Serial rename all marked files to `anything-ff-default-directory'.
Rename only file of current directory, and symlink files coming from
other directories.
See `anything-ff-serial-rename-1'."
  (anything-ff-serial-rename-action 'symlink))

(defun anything-ff-serial-rename-by-copying (candidate)
  "Serial rename all marked files to `anything-ff-default-directory'.
Rename only file of current directory, and copy files coming from
other directories.
See `anything-ff-serial-rename-1'."
  (anything-ff-serial-rename-action 'copy))

(defun anything-c-quit-and-execute-action (action)
  "Quit current anything session and execute ACTION." 
  (setq anything-saved-action action)
  (anything-exit-minibuffer))

(defun anything-ff-toggle-auto-update (candidate)
  (setq anything-ff-auto-update-flag (not anything-ff-auto-update-flag))
  (message "[Auto expansion %s]"
           (if anything-ff-auto-update-flag "enabled" "disabled")))

;;;###autoload
(defun anything-ff-run-toggle-auto-update ()
  (interactive)
  (when (anything-file-completion-source-p)
    (anything-attrset 'toggle-auto-update 'anything-ff-toggle-auto-update)
    (anything-execute-persistent-action 'toggle-auto-update)))

;;;###autoload
(defun anything-ff-run-switch-to-history ()
  "Run Switch to history action from `anything-c-source-find-files'."
  (interactive)
  (when (anything-file-completion-source-p)
    (anything-c-quit-and-execute-action 'anything-find-files-switch-to-hist)))

;;;###autoload
(defun anything-ff-run-grep ()
  "Run Grep action from `anything-c-source-find-files'."
  (interactive)
  (when (anything-file-completion-source-p)
    (anything-c-quit-and-execute-action 'anything-find-files-grep)))

;;;###autoload
(defun anything-ff-run-pdfgrep ()
  "Run Pdfgrep action from `anything-c-source-find-files'."
  (interactive)
  (when (anything-file-completion-source-p)
    (anything-c-quit-and-execute-action 'anything-ff-pdfgrep)))

;;;###autoload
(defun anything-ff-run-zgrep ()
  "Run Grep action from `anything-c-source-find-files'."
  (interactive)
  (when (anything-file-completion-source-p)
    (anything-c-quit-and-execute-action 'anything-ff-zgrep)))

;;;###autoload
(defun anything-ff-run-copy-file ()
  "Run Copy file action from `anything-c-source-find-files'."
  (interactive)
  (when (anything-file-completion-source-p)
    (anything-c-quit-and-execute-action 'anything-find-files-copy)))

;;;###autoload
(defun anything-ff-run-rename-file ()
  "Run Rename file action from `anything-c-source-find-files'."
  (interactive)
  (when (anything-file-completion-source-p)
    (anything-c-quit-and-execute-action 'anything-find-files-rename)))

;;;###autoload
(defun anything-ff-run-byte-compile-file ()
  "Run Byte compile file action from `anything-c-source-find-files'."
  (interactive)
  (when (anything-file-completion-source-p)
    (anything-c-quit-and-execute-action 'anything-find-files-byte-compile)))

;;;###autoload
(defun anything-ff-run-load-file ()
  "Run Load file action from `anything-c-source-find-files'."
  (interactive)
  (when (anything-file-completion-source-p)
    (anything-c-quit-and-execute-action 'anything-find-files-load-files)))

;;;###autoload
(defun anything-ff-run-eshell-command-on-file ()
  "Run eshell command on file action from `anything-c-source-find-files'."
  (interactive)
  (when (anything-file-completion-source-p)
    (anything-c-quit-and-execute-action
     'anything-find-files-eshell-command-on-file)))

;;;###autoload
(defun anything-ff-run-ediff-file ()
  "Run Ediff file action from `anything-c-source-find-files'."
  (interactive)
  (when (anything-file-completion-source-p)
    (anything-c-quit-and-execute-action 'anything-find-files-ediff-files)))

;;;###autoload
(defun anything-ff-run-ediff-merge-file ()
  "Run Ediff merge file action from `anything-c-source-find-files'."
  (interactive)
  (when (anything-file-completion-source-p)
    (anything-c-quit-and-execute-action
     'anything-find-files-ediff-merge-files)))

;;;###autoload
(defun anything-ff-run-symlink-file ()
  "Run Symlink file action from `anything-c-source-find-files'."
  (interactive)
  (when (anything-file-completion-source-p)
    (anything-c-quit-and-execute-action 'anything-find-files-symlink)))

;;;###autoload
(defun anything-ff-run-hardlink-file ()
  "Run Hardlink file action from `anything-c-source-find-files'."
  (interactive)
  (when (anything-file-completion-source-p)
    (anything-c-quit-and-execute-action 'anything-find-files-hardlink)))

;;;###autoload
(defun anything-ff-run-delete-file ()
  "Run Delete file action from `anything-c-source-find-files'."
  (interactive)
  (when (anything-file-completion-source-p)
    (anything-c-quit-and-execute-action 'anything-delete-marked-files)))

;;;###autoload
(defun anything-ff-run-complete-fn-at-point ()
  "Run complete file name action from `anything-c-source-find-files'."
  (interactive)
  (when (anything-file-completion-source-p)
    (anything-c-quit-and-execute-action
     'anything-c-insert-file-name-completion-at-point)))

;;;###autoload
(defun anything-ff-run-switch-to-eshell ()
  "Run switch to eshell action from `anything-c-source-find-files'."
  (interactive)
  (when (anything-file-completion-source-p)
    (anything-c-quit-and-execute-action 'anything-ff-switch-to-eshell)))

;;;###autoload
(defun anything-ff-run-switch-other-window ()
  "Run switch to other window action from `anything-c-source-find-files'."
  (interactive)
  (when (anything-file-completion-source-p)
    (anything-c-quit-and-execute-action 'find-file-other-window)))

;;;###autoload
(defun anything-ff-run-switch-other-frame ()
  "Run switch to other frame action from `anything-c-source-find-files'."
  (interactive)
  (when (anything-file-completion-source-p)
    (anything-c-quit-and-execute-action 'find-file-other-frame)))

;;;###autoload
(defun anything-ff-run-open-file-externally ()
  "Run open file externally command action from `anything-c-source-find-files'."
  (interactive)
  (when (anything-file-completion-source-p)
    (anything-c-quit-and-execute-action 'anything-c-open-file-externally)))

(defun anything-ff-locate (candidate)
  "Locate action function for `anything-find-files'."
  (let ((input (concat (anything-c-basename
                        (expand-file-name
                         candidate
                         anything-ff-default-directory))
                       ;; The locate '-b' option doesn't exists
                       ;; in everything.
                       (unless (and (eq system-type 'windows-nt)
                                    (string-match "^es" anything-c-locate-command))
                         " -b")))
        (anything-mp-highlight-delay 0.7))
    (anything-locate-1 anything-current-prefix-arg input 'from-ff)))

;;;###autoload
(defun anything-ff-run-locate ()
  "Run locate action from `anything-c-source-find-files'."
  (interactive)
  (when (anything-file-completion-source-p)
    (anything-c-quit-and-execute-action 'anything-ff-locate)))

;;;###autoload
(defun anything-ff-run-gnus-attach-files ()
  "Run gnus attach files command action from `anything-c-source-find-files'."
  (interactive)
  (when (anything-file-completion-source-p)
    (anything-c-quit-and-execute-action 'anything-ff-gnus-attach-files)))

;;;###autoload
(defun anything-ff-run-etags ()
  "Run Etags command action from `anything-c-source-find-files'."
  (interactive)
  (when (anything-file-completion-source-p)
    (anything-c-quit-and-execute-action 'anything-ff-etags-select)))

(defun anything-ff-print (candidate)
  "Print marked files.
You have to set in order
variables `lpr-command',`lpr-switches' and/or `printer-name'.

e.g:
\(setq lpr-command \"gtklp\"\)
\(setq lpr-switches '(\"-P\")\)
\(setq printer-name \"Epson-Stylus-Photo-R265\"\)

Same as `dired-do-print' but for anything."
  (when (or anything-current-prefix-arg
            (not anything-ff-printer-list))
    (setq anything-ff-printer-list
          (anything-ff-find-printers)))
  (let* ((file-list (anything-marked-candidates))
         (len (length file-list))
         (printer-name (if anything-ff-printer-list
                           (anything-comp-read
                            "Printer: " anything-ff-printer-list)
                           printer-name))
	 (command (read-string
                   (format "Print *%s File(s):\n%s with: "
                           len
                           (mapconcat
                            (lambda (f) (format "- %s\n" f))
                            file-list ""))
                   (when (and lpr-command
                              (or lpr-switches
                                  printer-name))
                     (mapconcat 'identity
                                (cons lpr-command
                                      (append (if (stringp lpr-switches)
                                                  (list lpr-switches)
                                                  lpr-switches)
                                              (list printer-name)))
                                " "))))
         (file-args (mapconcat #'(lambda (x)
                                   (format "'%s'" x))
                               file-list " "))
         (cmd-line (concat command " " file-args)))
    (if command
        (start-process-shell-command "anything-print" nil cmd-line)
        (error "Error: Please verify your printer settings in Emacs."))))

;;;###autoload
(defun anything-ff-run-print-file ()
  "Run Print file action from `anything-c-source-find-files'."
  (interactive)
  (when (anything-file-completion-source-p)
    (anything-c-quit-and-execute-action 'anything-ff-print)))

(defun anything-ff-checksum (file)
  "Calculate the checksum of FILE.
Provide completion on different algorithms to use on Emacs24.
On Emacs23 only 'sha1' is available.
The checksum is copied to kill-ring."
  (let ((algo-list (and (fboundp 'secure-hash)
                        '(md5 sha1 sha224 sha256 sha384 sha512))))
    (kill-new
     (if algo-list
         (secure-hash (intern
                       (anything-comp-read
                        "Algorithm: " algo-list))
                      file)
         (sha1 (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string)))))
    (message "Checksum copied to kill-ring.")))

(defun anything-ff-toggle-basename (candidate)
  (setq anything-ff-transformer-show-only-basename
        (not anything-ff-transformer-show-only-basename))
  (let ((target (if anything-ff-transformer-show-only-basename
                    (anything-c-basename candidate) candidate)))
    (anything-force-update target)))

(defun anything-ff-run-toggle-basename ()
  (interactive)
  (when (anything-file-completion-source-p)
    (anything-attrset 'toggle-basename 'anything-ff-toggle-basename)
    (anything-execute-persistent-action 'toggle-basename)))

(defun* anything-reduce-file-name (fname level &key unix-close expand)
  "Reduce FNAME by LEVEL from end or beginning depending LEVEL value.
If LEVEL is positive reduce from end else from beginning.
If UNIX-CLOSE is non--nil close filename with /.
If EXPAND is non--nil expand-file-name."
  (let* ((exp-fname  (expand-file-name fname))
         (fname-list (split-string (if (or (string= fname "~/") expand)
                                       exp-fname fname) "/" t))
         (len        (length fname-list))
         (pop-list   (if (< level 0)
                         (subseq fname-list (* level -1))
                         (subseq fname-list 0 (- len level))))
         (result     (mapconcat 'identity pop-list "/"))
         (empty      (string= result "")))
    (when unix-close (setq result (concat result "/")))
    (if (string-match "^~" result)
        (if (string= result "~/") "~/" result)
        (if (< level 0)
            (if empty "../" (concat "../" result))
            (cond ((eq system-type 'windows-nt)
                   (if empty (expand-file-name "/") ; Expand to "/" or "c:/".
                       result))
                  (empty "/")
                  (t
                   (concat "/" result)))))))

;; Internal
(defvar anything-file-completion-sources
  '("Find Files" "Read File Name"
    "Read File Name History" "Copy Files"
    "Rename Files" "Symlink Files"
    "Hardlink Files" "Write File" "Insert File")
  "Sources that use the *find-files mechanism can be added here.
Sources generated by `ac-mode' don't need to be added here, it will
be done automatically.
You should not modify this yourself unless you know what you do.")

(defun anything-file-completion-source-p ()
  "Return non--nil if current source is a file completion source.
A source is a file completion source if it is
one of `anything-file-completion-sources'.
Return nil if anything is not running."
  (let ((cur-source (cdr (assoc 'name (anything-get-current-source)))))
    (loop for i in anything-file-completion-sources
          thereis (string= cur-source i))))

(defun anything-find-files-down-one-level (arg)
  "Go down one level like unix command `cd ..'.
If prefix numeric arg is given go ARG level down."
  (interactive "p")
  (when (and (anything-file-completion-source-p)
             (not (anything-ff-invalid-tramp-name-p)))
    (with-anything-window
      (setq anything-follow-mode nil))
    ;; When going to precedent level we want to be at the line
    ;; corresponding to actual directory, so store this info
    ;; in `anything-ff-last-expanded'.
    (if (and (not (file-directory-p anything-pattern))
             (file-exists-p anything-pattern))
        (setq anything-ff-last-expanded anything-pattern)
        (setq anything-ff-last-expanded anything-ff-default-directory))
    (let ((new-pattern (anything-reduce-file-name anything-pattern arg
                                                  :unix-close t :expand t)))
      (anything-set-pattern new-pattern))))

(defun anything-ff-retrieve-last-expanded ()
  "Move overlay to last visited directory `anything-ff-last-expanded'.
This happen after using `anything-find-files-down-one-level',
or hitting C-z on \"..\"."
  (when (and anything-ff-last-expanded
             (anything-file-completion-source-p))
    (let ((presel (if anything-ff-transformer-show-only-basename
                       (anything-c-basename
                        (directory-file-name anything-ff-last-expanded))
                       (directory-file-name anything-ff-last-expanded))))
      (with-anything-window
        (when (re-search-forward
               (concat "^" (regexp-quote presel) "$") nil t)
          (forward-line 0)
          (anything-mark-current-line)))
      (setq anything-ff-last-expanded nil))))
(add-hook 'anything-after-update-hook 'anything-ff-retrieve-last-expanded)

;; Auto-update - anything-find-files auto expansion of directories.
;;
(defun anything-ff-update-when-only-one-matched ()
  "Expand to directory when sole completion.
When only one candidate is remaining and it is a directory,
expand to this directory."
  (when (and anything-ff-auto-update-flag
             (anything-file-completion-source-p)
             (not (anything-ff-invalid-tramp-name-p)))
    (let* ((history-p   (string= (assoc-default
                                  'name (anything-get-current-source))
                                 "Read File Name History"))
           (pat         (if (string-match tramp-file-name-regexp
                                          anything-pattern)
                            (anything-create-tramp-name anything-pattern)
                            anything-pattern))
           (completed-p (string= (file-name-as-directory pat)
                                 anything-ff-default-directory)))
      (when (and (or
                  ;; Only one candidate remaining
                  ;; and at least 2 char in basename.
                  (and (<= (anything-approximate-candidate-number) 2)
                       (>= (length (anything-c-basename anything-pattern)) 2))
                  ;; Already completed.
                  completed-p)
                 (not history-p)) ; Don't try to auto complete in history.
        (with-anything-window
          (let ((cur-cand (prog2
                              (unless completed-p
                                ;; Only one non--existing candidate
                                ;; and one directory candidate, move to it.
                                (anything-next-line))
                              (anything-get-selection))))
            (when (and (stringp cur-cand) (file-directory-p cur-cand))
              (if (and (not (string-match "^.*[.]\\{1,2\\}$" cur-cand)) ; [1]
                       ;; Maybe we are here because completed-p is true
                       ;; but check this again to be sure. (Windows fix)
                       (<= (anything-approximate-candidate-number) 2)) ; [2]
                  ;; If after going to next line the candidate
                  ;; is not one of "." or ".." [1]
                  ;; and only one candidate is remaining [2],
                  ;; assume candidate is a new directory to expand, and do it.
                  (anything-set-pattern (file-name-as-directory cur-cand))
                  ;; The candidate is one of "." or ".."
                  ;; that mean we have entered the last letter of the directory name
                  ;; in prompt, so expansion is already done, just add the "/" at end
                  ;; of name unless anything-pattern ends with "."
                  ;; (i.e we are writing something starting with ".")
                  (unless (string-match "^.*[.]\\{1\\}$" anything-pattern)
                    (anything-set-pattern
                     ;; Need to expand-file-name to avoid e.g /ssh:host:./ in prompt.
                     (expand-file-name (file-name-as-directory anything-pattern)))))
              (anything-check-minibuffer-input-1))))))))
(add-hook 'anything-after-update-hook 'anything-ff-update-when-only-one-matched)

;; Allow expanding to home directory or root
;; when entering respectively "~/" or "//" at end of pattern.
;; e.g /home/thierry/labo/anything-config-qp/~/
;; will expand to "~/"
;; and /home/thierry/labo/anything-config-qp//
;; will expand to "/"
(defun anything-ff-auto-expand-to-home-or-root ()
  "Goto home, root or default directory when pattern ends with ~/, /, or ./.
This happen only in function using sources that are
`anything-file-completion-source-p' compliant."
  (when (and (anything-file-completion-source-p)
             (string-match ".*\\(/~/\\|/\\{2\\}\\|/[.]\\{1\\}/\\)$"
                           anything-pattern)
             (not (string-match anything-ff-url-regexp anything-pattern)))
    (let ((match (match-string 1 anything-pattern)))
      (cond ((string= match "//")
             ;; Expand to "/" or "c:/"
             (setq anything-pattern (expand-file-name "/")))
            ((string= match "/~/")
             (if (eq system-type 'windows-nt)
                 (setq anything-pattern (file-name-as-directory (getenv "HOME")))
                 (setq anything-pattern "~/")))
            ((string= match "/./")
             (setq anything-pattern
                   (with-anything-current-buffer
                     (expand-file-name default-directory))))))
    (setq anything-ff-default-directory anything-pattern)
    ;; For some reasons, i must use here with-current-buffer => mini buffer
    ;; and not `anything-set-pattern' that use with-selected-window => mini win.
    (with-current-buffer (window-buffer (minibuffer-window))
      (delete-minibuffer-contents)
      (insert anything-pattern))))

(add-hook 'anything-after-update-hook 'anything-ff-auto-expand-to-home-or-root)

(defun anything-c-point-file-in-dired (file)
  "Put point on filename FILE in dired buffer."
  (dired (file-name-directory file))
  (dired-goto-file file))

(defun anything-create-tramp-name (fname)
  "Build filename for `anything-pattern' like /su:: or /sudo::."
  (apply #'tramp-make-tramp-file-name
         (loop with v = (tramp-dissect-file-name fname)
               for i across v collect i)))

(defun* anything-ff-tramp-hostnames (&optional (pattern anything-pattern))
  "Get a list of hosts for tramp method found in `anything-pattern'.
Argument PATTERN default to `anything-pattern', it is here only for debugging
purpose."
  (when (string-match tramp-file-name-regexp pattern)
    (let ((method      (match-string 1 pattern))
          (tn          (match-string 0 pattern))
          (all-methods (mapcar 'car tramp-methods)))
      (anything-fast-remove-dups
       (loop for (f . h) in (tramp-get-completion-function method)
             append (loop for e in (funcall f (car h))
                          for host = (and (consp e) (cadr e))
                          when (and host (not (member host all-methods)))
                          collect (concat tn host)))
       :test 'equal))))

(defun anything-ff-before-action-hook-fn ()
  "Exit anything when user try to execute action on an invalid tramp fname."
  (let ((cand (anything-get-selection)))
    (when (and (anything-file-completion-source-p)
               (anything-ff-invalid-tramp-name-p cand) ; Check candidate.
               (anything-ff-invalid-tramp-name-p)) ; check anything-pattern.
      (error "Error: Unknow file or directory `%s'" cand))))
(add-hook 'anything-before-action-hook 'anything-ff-before-action-hook-fn)

(defun* anything-ff-invalid-tramp-name-p (&optional (pattern anything-pattern))
  "Return non--nil when PATTERN is an invalid tramp filename."
  (string= (anything-ff-set-pattern pattern)
           "Invalid tramp file name"))

(defun anything-ff-set-pattern (pattern)
  "Handle tramp filenames in `anything-pattern'."
  (let ((methods (mapcar 'car tramp-methods))
        (reg "\\`/\\([^[/:]+\\|[^/]+]\\):.*:")
        cur-method tramp-name)
    (cond ((string= pattern "") "")
          ((string-match ".*\\(~?/?[.]\\{1\\}/\\)$" pattern)
           (with-anything-current-buffer
             (expand-file-name default-directory)))
          ((and (string-match ".*\\(~//\\|//\\)$" pattern)
                (not (string-match anything-ff-url-regexp anything-pattern)))
           (expand-file-name "/") ; Expand to "/" or "c:/"
           )
          ((string-match "^~\\|.*/~/$" pattern)
           (let* ((home (expand-file-name (getenv "HOME"))))
             (replace-match home nil t pattern)))
          ;; Match "/method:maybe_hostname:"
          ((and (string-match reg pattern)
                (setq cur-method (match-string 1 pattern))
                (member cur-method methods))
           (setq tramp-name (anything-create-tramp-name
                             (match-string 0 pattern)))
           (replace-match tramp-name nil t pattern))
          ;; Match "/hostname:"
          ((and (string-match  tramp-file-name-regexp pattern)
                (setq cur-method (match-string 1 pattern))
                (and cur-method (not (member cur-method methods))))
           (setq tramp-name (anything-create-tramp-name
                             (match-string 0 pattern)))
           (replace-match tramp-name nil t pattern))
          ;; Match "/method:" in this case don't try to connect.
          ((and (not (string-match reg pattern))
                (string-match tramp-file-name-regexp pattern)
                (member (match-string 1 pattern) methods))
           "Invalid tramp file name")   ; Write in anything-buffer.
          ;; PATTERN is a directory, end it with "/".
          ;; This will make PATTERN not ending yet with "/"
          ;; candidate for `anything-ff-default-directory',
          ;; allowing `anything-ff-retrieve-last-expanded' to retrieve it
          ;; when descending level.
          ((file-directory-p pattern)
           (file-name-as-directory pattern))
          ;; Return PATTERN unchanged.
          (t pattern))))

(defun anything-find-files-get-candidates (&optional require-match)
  "Create candidate list for `anything-c-source-find-files'."
  (let* ((path          (anything-ff-set-pattern anything-pattern))
         (path-name-dir (if (file-directory-p path)
                            (file-name-as-directory path)
                            (file-name-directory path)))
         (tramp-verbose anything-tramp-verbose)) ; No tramp message when 0.
    (set-text-properties 0 (length path) nil path)
    ;; Don't set now `anything-pattern' if `path' == "Invalid tramp file name"
    ;; like that the actual value (e.g /ssh:) is passed to
    ;; `anything-ff-tramp-hostnames'.
    (unless (string= path "Invalid tramp file name")
      (setq anything-pattern (anything-ff-transform-fname-for-completion path)))
    (setq anything-ff-default-directory
          (if (string= anything-pattern "")
              (expand-file-name "/") ; Expand to "/" or "c:/"
              ;; If path is an url *default-directory have to be nil.
              (unless (or (string-match anything-ff-url-regexp path)
                          (string-match ffap-url-regexp path))
                path-name-dir)))
    (cond ((string= path "Invalid tramp file name")
           (or (anything-ff-tramp-hostnames) ; Hostnames completion.
               (prog2
                   ;; `anything-pattern' have not been modified yet.
                   ;; Set it here to the value of `path' that should be now
                   ;; "Invalid tramp file name" and set the candidates list
                   ;; to ("Invalid tramp file name") to make `anything-pattern'
                   ;; match single candidate "Invalid tramp file name".
                   (setq anything-pattern path)
                   ;; "Invalid tramp file name" is now printed
                   ;; in `anything-buffer'.
                   (list path))))
          ((or (file-regular-p path)
               ;; `ffap-url-regexp' don't match until url is complete.
               (string-match anything-ff-url-regexp path)
               (and (not (file-exists-p path)) (string-match "/$" path))
               (and ffap-url-regexp (string-match ffap-url-regexp path)))
           (list path))
          ((string= path "") (anything-ff-directory-files "/" t))
          ((and (file-directory-p path) (not (file-readable-p path)))
           (list (format "Opening directory: access denied, `%s'" path)))
          ((file-directory-p path) (anything-ff-directory-files path t))
          (t
           (append (unless require-match (list path))
                   (anything-ff-directory-files path-name-dir t))))))

(defun anything-ff-directory-files (directory &optional full)
  "List contents of DIRECTORY.
Argument FULL mean absolute path.
It is same as `directory-files' but always returns the
dotted filename '.' and '..' on root directories on Windows
systems."
  (setq directory (expand-file-name directory))
  (let ((ls (directory-files directory full))
        dot dot2 lsdir)
    (if (or
         ;; A windows volume.
         (string-match "^[a-zA-Z]\\{1\\}:/$" directory)
         ;; Empty directories on ftp hosts may have no dot dirs.
         (and (file-remote-p directory)
              (string-match "^/ftp:" directory)))
        (progn (setq dot (concat directory "."))
               (setq dot2 (concat directory ".."))
               (setq lsdir (remove dot2 (remove dot ls)))
               (append (list dot dot2) lsdir))
        ls)))

(defun anything-ff-transform-fname-for-completion (fname)
  "Return FNAME with it's basename modified as a regexp.
e.g foo => f.*o.*o .
If basename contain one or more space or FNAME is a valid directory name
return FNAME unchanged."
  (let ((bn (anything-c-basename fname)))
    (if (or (not anything-ff-smart-completion)
            (string-match "\\s-" bn)
            (string-match "/$" fname) ; Allow mkdir.
            (file-directory-p fname)
            (string-match anything-ff-url-regexp fname))
        fname ; Fall back to match-plugin.
        (setq bn (if (> (length bn) 2) ; Normal completion on first 2 char.
                     (mapconcat 'identity (split-string bn "" t) ".*") bn))
        (concat (file-name-directory fname) bn))))

(defun anything-ff-save-history ()
  "Store the last value of `anything-ff-default-directory' \
in `anything-ff-history'."
  (when (and anything-ff-default-directory
             (anything-file-completion-source-p))
    (push anything-ff-default-directory anything-ff-history)))
(add-hook 'anything-cleanup-hook 'anything-ff-save-history)

(defun anything-ff-valid-symlink-p (file)
  (file-exists-p (file-truename file)))

(defun anything-ff-properties (candidate)
  "Show file properties of CANDIDATE in a tooltip or message."
  (let ((type       (anything-ff-attributes candidate :type t))
        (dired-line (anything-ff-attributes candidate :dired t :human-size t)))
    (if (window-system)
        (tooltip-show
         (concat
          (anything-c-basename candidate) "\n"
          "Type: " type "\n"
          (when (string= type "symlink")
            (format "True name: '%s'\n"
                    (cond ((string-match "^\.#" (anything-c-basename candidate))
                           "Autosave symlink")
                          ((anything-ff-valid-symlink-p candidate)
                           (file-truename candidate))
                          (t "Invalid Symlink"))))
          dired-line))
        (message dired-line) (sit-for 5))))

;;;###autoload
(defun anything-ff-properties-persistent ()
  "Show properties without quitting anything."
  (interactive)
  (anything-attrset 'properties-action 'anything-ff-properties)
  (anything-execute-persistent-action 'properties-action))

;;;###autoload
(defun anything-ff-persistent-delete ()
  "Delete current candidate without quitting."
  (interactive)
  (anything-attrset 'quick-delete 'anything-ff-quick-delete)
  (anything-execute-persistent-action 'quick-delete))

(defun anything-ff-dot-file-p (file)
  "Check if FILE is `.' or `..'."
  (member (anything-c-basename file) '("." "..")))

(defun anything-ff-quick-delete (candidate)
  "Delete file CANDIDATE without quitting."
  (let ((presel (prog1 (save-excursion
                         (let (sel)
                           (anything-next-line)
                           (setq sel (anything-get-selection))
                           (if (string= sel candidate)
                               (progn (anything-previous-line)
                                      (anything-get-selection))
                               sel)))
                  (anything-mark-current-line))))
    (setq presel (if (and anything-ff-transformer-show-only-basename
                          (not (anything-ff-dot-file-p presel)))
                     (anything-c-basename presel) presel))
    (if anything-ff-quick-delete-dont-prompt-for-deletion
        (anything-c-delete-file candidate
                                anything-ff-signal-error-on-dot-files)
        (save-selected-window
          (when (y-or-n-p (format "Really Delete file `%s'? " candidate))
            (anything-c-delete-file candidate
                                    anything-ff-signal-error-on-dot-files)
            (message nil))))
    (anything-force-update presel)))

(defun anything-ff-kill-buffer-fname (candidate)
  (let* ((buf (get-file-buffer candidate))
         (buf-name (buffer-name buf)))
    (if buf
        (progn
          (kill-buffer buf) (message "Buffer `%s' killed" buf))
        (message "No buffer to kill"))))

(defun anything-ff-kill-or-find-buffer-fname (candidate)
  "Find file CANDIDATE or kill it's buffer if it is visible.
Never kill `anything-current-buffer'.
Never kill buffer modified.
This is called normally on third hit of \
\\<anything-map>\\[anything-execute-persistent-action]
in `anything-find-files-persistent-action'."
  (let* ((buf      (get-file-buffer candidate))
         (buf-name (buffer-name buf)))
    (if (and buf (get-buffer-window buf)
             (not (eq buf (get-buffer anything-current-buffer)))
             (not (buffer-modified-p buf)))
        (progn
          (kill-buffer buf) (message "Buffer `%s' killed" buf-name))
        (find-file candidate))))

;;;###autoload
(defun anything-ff-run-kill-buffer-persistent ()
  "Execute `anything-ff-kill-buffer-fname' whitout quitting."
  (interactive)
  (when (anything-file-completion-source-p)
    (anything-attrset 'kill-buffer-fname 'anything-ff-kill-buffer-fname)  
    (anything-execute-persistent-action 'kill-buffer-fname)))

(defun anything-ff-human-size (size)
  "Return a string showing SIZE of a file in human readable form.
SIZE can be an integer or a float depending it's value.
`file-attributes' will take care of that to avoid overflow error.
KBSIZE if a floating point number, default value is 1024.0."
  (let ((M (cons "M" (/ size (expt anything-ff-default-kbsize 2))))
        (G (cons "G" (/ size (expt anything-ff-default-kbsize 3))))
        (K (cons "K" (/ size anything-ff-default-kbsize)))
        (B (cons "B" size)))
    (loop with result = B
          for (a . b) in
          (loop for (x . y) in (list M G K B)
                unless (< y 1) collect (cons x y))
          when (< b (cdr result)) do (setq result (cons a b))
          finally return (if (string= (car result) "B")
                             (format "%s" size)
                             (format "%.1f%s" (cdr result) (car result))))))

(defun* anything-ff-attributes
    (file &key type links uid gid access-time modif-time
          status size mode gid-change inode device-num dired human-size)
  "Easy interface for `file-attributes'."
  (let ((all (destructuring-bind
                   (type links uid gid access-time modif-time
                         status size mode gid-change inode device-num)
                 (file-attributes file 'string)
               (list :type        type
                     :links       links
                     :uid         uid
                     :gid         gid
                     :access-time access-time
                     :modif-time  modif-time
                     :status      status
                     :size        size
                     :mode        mode
                     :gid-change  gid-change
                     :inode       inode
                     :device-num  device-num))))
    (cond (type
           (let ((result (getf all :type)))
             (cond ((stringp result)
                    "symlink")
                   (result "directory")
                   (t "file"))))
          (links (getf all :links))
          (uid   (getf all :uid))
          (gid   (getf all :gid))
          (access-time
           (format-time-string "%Y-%m-%d %R" (getf all :access-time)))
          (modif-time
           (format-time-string "%Y-%m-%d %R" (getf all :modif-time)))
          (status
           (format-time-string "%Y-%m-%d %R" (getf all :status)))
          (size (if human-size (anything-ff-human-size (getf all :size))
                    (getf all :size)))
          (mode (getf all :mode))
          (gid-change (getf all :gid-change))
          (inode (getf all :inode))
          (device-num (getf all :device-num))
          (dired
           (concat
            (getf all :mode) " "
            (number-to-string (getf all :links)) " "
            (getf all :uid) ":"
            (getf all :gid) " "
            (if human-size (anything-ff-human-size (getf all :size))
                (int-to-string (getf all :size))) " "
                (format-time-string "%Y-%m-%d %R" (getf all :modif-time))))
          (t all))))

(defun anything-ff-prefix-filename (fname &optional file-or-symlinkp new-file)
  "Return filename FNAME maybe prefixed with [?] or [@].
If FILE-OR-SYMLINKP is non--nil this mean we assume FNAME is an
existing filename or valid symlink and there is no need to test it.
NEW-FILE when non--nil mean FNAME is a non existing file and
return FNAME prefixed with [?]."
  (let* ((prefix-new (propertize
                      " " 'display
                      (propertize "[?]" 'face 'anything-ff-prefix)))
         (prefix-url (propertize
                      " " 'display
                      (propertize "[@]" 'face 'anything-ff-prefix))))
    (cond ((or file-or-symlinkp (file-exists-p fname)) fname)
          ((or (string-match anything-ff-url-regexp fname)
               (string-match ffap-url-regexp fname))
           (concat prefix-url " " fname))
          ((or new-file (not (file-exists-p fname)))
           (concat prefix-new " " fname)))))

(defun anything-c-find-files-transformer (files sources)
  "Transformer for `anything-c-source-find-files'.
Tramp files are not highlighted unless `anything-ff-tramp-not-fancy'
is non--nil."
  (if (and (string-match tramp-file-name-regexp anything-pattern)
           anything-ff-tramp-not-fancy)
      (if anything-ff-transformer-show-only-basename
          (loop for i in files collect 
                (if (string-match "[.]\\{1,2\\}$" i)
                    i (cons (anything-c-basename i) i)))
          files)
      (anything-ff-highlight-files files sources)))

(defun anything-ff-highlight-files (files sources)
  "Candidate transformer for `anything-c-source-find-files' without icons."
  (loop for i in files
        for disp = (if (and anything-ff-transformer-show-only-basename
                            (not (string-match "[.]\\{1,2\\}$" i))
                            (not (string-match ffap-url-regexp i))
                            (not (string-match anything-ff-url-regexp i)))
                       (anything-c-basename i) i)
        collect
        (cond ((and (stringp (car (file-attributes i)))
                    (not (anything-ff-valid-symlink-p i))
                    (not (string-match "^\.#" (anything-c-basename i))))
               (cons (anything-ff-prefix-filename
                      (propertize disp 'face 'anything-ff-invalid-symlink) t)
                     i))
              ((stringp (car (file-attributes i)))
               (cons (anything-ff-prefix-filename
                      (propertize disp 'face 'anything-ff-symlink) t)
                     i))
              ((eq t (car (file-attributes i)))
               (cons (anything-ff-prefix-filename
                      (propertize disp 'face 'anything-ff-directory) t)
                     i))
              ((file-executable-p i)
               (cons (anything-ff-prefix-filename
                      (propertize disp 'face 'anything-ff-executable) t)
                     i))
              ((file-exists-p i)
               (cons (anything-ff-prefix-filename
                      (propertize disp 'face 'anything-ff-file) t)
                     i))
              (t
               (cons (anything-ff-prefix-filename
                      (propertize disp 'face 'anything-ff-file) nil 'new-file)
                     i)))))

(defun anything-find-files-action-transformer (actions candidate)
  "Action transformer for `anything-c-source-find-files'."
  (cond ((with-anything-current-buffer
           (eq major-mode 'message-mode))
         (append (subseq actions 0 4)
                 '(("Gnus attach file(s)" . anything-ff-gnus-attach-files))
                 (subseq actions 4)))
        ((string-match (image-file-name-regexp) candidate)
         (append (subseq actions 0 4)
                 '(("Rotate image right `M-r'" . anything-ff-rotate-image-right)
                   ("Rotate image left `M-l'" . anything-ff-rotate-image-left))
                 (subseq actions 4)))
        ((string-match "\.el$" (anything-aif (anything-marked-candidates)
                                   (car it) candidate))
         (append (subseq actions 0 4)
                 '(("Byte compile lisp file(s) `M-B, C-u to load'"
                    . anything-find-files-byte-compile)
                   ("Load File(s) `M-L'" . anything-find-files-load-files))
                 (subseq actions 4)))
        ((and (string-match "\.html?$" candidate)
              (file-exists-p candidate))
         (append (subseq actions 0 4)
                 '(("Browse url file" . browse-url-of-file))
                 (subseq actions 5)))
        ((or (string= (file-name-extension candidate) "pdf")
             (string= (file-name-extension candidate) "PDF"))
         (append (subseq actions 0 4)
                 '(("Pdfgrep File(s)" . anything-ff-pdfgrep))
                 (subseq actions 5)))
        (t actions)))

(defun anything-ff-gnus-attach-files (candidate)
  "Run `gnus-dired-attach' on `anything-marked-candidates' or CANDIDATE."
  (let ((flist (anything-marked-candidates)))
    (gnus-dired-attach flist)))

(defun anything-ff-rotate-current-image-1 (file &optional num-arg)
  "Rotate current image at NUM-ARG degrees.
This is a destructive operation on FILE made by external tool mogrify."
  (declare (special image-dired-display-image-buffer))
  (setq file (file-truename file)) ; For symlinked images.
  ;; When FILE is not an image-file, do nothing.
  (when (string-match (image-file-name-regexp) file)
    (if (executable-find "mogrify")
        (progn
          (shell-command (format "mogrify -rotate %s %s"
                                 (or num-arg 90)
                                 (shell-quote-argument file)))
          (when (buffer-live-p image-dired-display-image-buffer)
            (kill-buffer image-dired-display-image-buffer))
          (image-dired-display-image file)
          (message nil)
          (display-buffer (get-buffer image-dired-display-image-buffer)))
        (error "mogrify not found"))))

(defun anything-ff-rotate-image-left (candidate)
  "Rotate image file CANDIDATE left.
This affect directly file CANDIDATE."
  (anything-ff-rotate-current-image-1 candidate -90))

(defun anything-ff-rotate-image-right (candidate)
  "Rotate image file CANDIDATE right.
This affect directly file CANDIDATE."
  (anything-ff-rotate-current-image-1 candidate))

(defun anything-ff-rotate-left-persistent ()
  "Rotate image left without quitting anything."
  (interactive)
  (anything-attrset 'image-action1 'anything-ff-rotate-image-left)
  (anything-execute-persistent-action 'image-action1))

(defun anything-ff-rotate-right-persistent ()
  "Rotate image right without quitting anything."
  (interactive)
  (anything-attrset 'image-action2 'anything-ff-rotate-image-right)
  (anything-execute-persistent-action 'image-action2))

(defun anything-ff-exif-data (candidate)
  "Extract exif data from file CANDIDATE using `anything-ff-exif-data-program'."
  (if (and anything-ff-exif-data-program
           (executable-find anything-ff-exif-data-program))
      (shell-command-to-string (format "%s %s %s"
                                       anything-ff-exif-data-program
                                       anything-ff-exif-data-program-args
                                       candidate))
      (format "No program %s found to extract exif"
              anything-ff-exif-data-program)))

(defun anything-find-files-persistent-action (candidate)
  "Open subtree CANDIDATE without quitting anything.
If CANDIDATE is not a directory expand CANDIDATE filename.
If CANDIDATE is alone, open file CANDIDATE filename.
That's mean:
First hit on C-z expand CANDIDATE second hit open file.
If a prefix arg is given or `anything-follow-mode' is on open file."
  (let ((follow        (buffer-local-value
                        'anything-follow-mode
                        (get-buffer-create anything-buffer)))
        (new-pattern   (anything-get-selection))
        (num-lines-buf (with-current-buffer anything-buffer
                         (count-lines (point-min) (point-max)))))
    (flet ((insert-in-minibuffer (fname)
             (with-selected-window (minibuffer-window)
               (unless follow
                 (delete-minibuffer-contents)
                 (set-text-properties 0 (length fname) nil fname)
                 (insert fname)))))
      (cond ((and (string= (anything-ff-set-pattern anything-pattern)
                           "Invalid tramp file name")
                  (string-match tramp-file-name-regexp candidate))
             ;; First hit insert hostname and
             ;; second hit insert ":" and expand.
             (if (string= candidate anything-pattern)
                 (insert-in-minibuffer (concat candidate ":"))
                 (insert-in-minibuffer candidate)))
            (;; A symlink directory, expand it's truename.
             (and (file-directory-p candidate) (file-symlink-p candidate))
             (insert-in-minibuffer (file-name-as-directory
                                    (file-truename
                                     (expand-file-name candidate)))))
            ;; A directory, open it.
            ((file-directory-p candidate)
             (when (string= (anything-c-basename candidate) "..")
               (setq anything-ff-last-expanded anything-ff-default-directory))
             (insert-in-minibuffer (file-name-as-directory
                                    (expand-file-name candidate))))
            ;; A symlink file, expand to it's true name. (first hit)
            ((and (file-symlink-p candidate) (not current-prefix-arg) (not follow))
             (insert-in-minibuffer (file-truename candidate)))
            ;; A regular file, expand it, (first hit)
            ((and (>= num-lines-buf 3) (not current-prefix-arg) (not follow))
             (insert-in-minibuffer new-pattern))
            ;; An image file and it is the second hit on C-z,
            ;; show the file in `image-dired'.
            ((string-match (image-file-name-regexp) candidate)
             (when (buffer-live-p image-dired-display-image-buffer)
               (kill-buffer image-dired-display-image-buffer))
             (image-dired-display-image candidate)
             (message nil)
             (anything-c-switch-to-buffer image-dired-display-image-buffer)
             (with-current-buffer image-dired-display-image-buffer
               (let ((exif-data (anything-ff-exif-data candidate)))
                 (image-dired-update-property 'help-echo exif-data))))
            ;; Allow browsing archive on avfs fs.
            ;; Assume volume is already mounted with mountavfs.
            ((and anything-ff-avfs-directory
                  (string-match
                   (regexp-quote (expand-file-name anything-ff-avfs-directory))
                   (file-name-directory candidate))
                  (anything-ff-file-compressed-p candidate))
             (insert-in-minibuffer (concat candidate "#")))
            ;; On second hit we open file.
            ;; On Third hit we kill it's buffer maybe.
            (t
             (anything-ff-kill-or-find-buffer-fname candidate))))))

(defun anything-ff-file-compressed-p (candidate)
  "Whether CANDIDATE is a compressed file or not."
  (member (file-name-extension candidate)
          anything-ff-file-compressed-list))

(defun anything-c-insert-file-name-completion-at-point (candidate)
  "Insert file name completion at point."
  (with-anything-current-buffer
    (if buffer-read-only
        (error "Error: Buffer `%s' is read-only" (buffer-name))
        (let* ((end         (point))
               (guess       (substring-no-properties (thing-at-point 'filename)))
               (beg         (- (point) (length guess)))
               (full-path-p (or (string-match-p (concat "^" (getenv "HOME")) guess)
                                (string-match-p "^[^\~]" guess))))
          (set-text-properties 0 (length candidate) nil candidate)
          (if (and guess (not (string= guess ""))
                   (string-match-p "^~\\|/.*" guess))
              (progn
                (delete-region beg end)
                (insert (if full-path-p
                            (expand-file-name candidate)
                            (abbreviate-file-name candidate))))
              (error "Aborting completion: No valid file name at point"))))))

(defun* anything-find-files-history (&key (comp-read t))
  "The `anything-find-files' history.
Show the first `anything-ff-history-max-length' elements of
`anything-ff-history' in an `anything-comp-read'."
  (let ((history (when anything-ff-history
                   (anything-fast-remove-dups anything-ff-history
                                              :test 'equal))))
    (when history
      (setq anything-ff-history
            (if (>= (length history) anything-ff-history-max-length)
                (subseq history 0 anything-ff-history-max-length)
                history))
      (if comp-read
          (anything-comp-read
           "Switch to Directory: "
           anything-ff-history
           :name "Anything Find Files History"
           :must-match t)
          anything-ff-history))))

(defun anything-find-files-1 (fname &optional preselect)
  "Find FNAME with `anything' completion.
Like `find-file' but with `anything' support.
Use it for non--interactive calls of `anything-find-files'."
  (when (get-buffer anything-action-buffer)
    (kill-buffer anything-action-buffer))
  (let ((anything-mp-highlight-delay nil)
        ;; Be sure we don't erase the precedent minibuffer if some.
        (anything-ff-auto-update-initial-value
         (and anything-ff-auto-update-initial-value
              (not (minibuffer-window-active-p (minibuffer-window)))))
        anything-samewindow)
    (anything :sources 'anything-c-source-find-files
              :input fname
              :preselect preselect
              :keymap anything-find-files-map
              :prompt "Find Files or Url: "
              :buffer "*Anything Find Files*")))


(defun anything-find-files-initial-input (&optional input)
  "Return INPUT if present, otherwise try to guess it."
  (or (and input (or (and (file-remote-p input) input)
                     (expand-file-name input)))
      (anything-find-files-input
       (ffap-guesser)
       (thing-at-point 'filename))))

(defun anything-find-files-input (fap tap)
  "Default input of `anything-find-files'."
  (let* ((def-dir (anything-c-current-directory))
         (lib     (anything-find-library-at-point))
         (url     (anything-ff-find-url-at-point))
         (remp    (and fap (file-remote-p fap)))
         (file-p  (and (not remp)
                       fap
                       (not (string= fap ""))
                       (file-exists-p fap)
                       tap (not (string= tap ""))
                       (file-exists-p
                        (file-name-directory (expand-file-name tap def-dir))))))
    (cond (lib) ; e.g we are inside a require sexp.
          (url) ; String at point is an hyperlink.
          (remp fap)
          (file-p (expand-file-name tap def-dir))
          (t (and (not (string= fap "")) fap)))))

(defun anything-c-current-directory ()
  "Return current-directory name at point.
Useful in dired buffers when there is inserted subdirs."
  (if (eq major-mode 'dired-mode)
      (dired-current-directory)
      default-directory))

(defun anything-ff-find-url-at-point ()
  "Try to find link to an url in text-property at point."
  (let* ((he      (get-text-property (point) 'help-echo))
         (ov      (overlays-at (point)))
         (ov-he   (and ov (overlay-get
                           (car (overlays-at (point))) 'help-echo)))
         (w3m-l   (get-text-property (point) 'w3m-href-anchor))
         (nt-prop (get-text-property (point) 'nt-link)))
    ;; Org link.
    (when (and (stringp he) (string-match "^LINK: " he))
      (setq he (replace-match "" t t he)))
    (loop for i in (list he ov-he w3m-l nt-prop)
          thereis (and (stringp i) (string-match ffap-url-regexp i) i))))

(defun anything-find-library-at-point ()
  "Try to find library path at point.
Find inside `require' and `declare-function' sexp."
  (require 'find-func)
  (let* ((beg-sexp (save-excursion (search-backward "(" (point-at-bol) t)))
         (end-sexp (save-excursion (search-forward ")" (point-at-eol) t)))
         (sexp     (and beg-sexp end-sexp
                        (buffer-substring-no-properties
                         (1+ beg-sexp) (1- end-sexp)))))
    (ignore-errors
      (cond ((and sexp (string-match "require \'.+[^)]" sexp))
             (find-library-name
              (replace-regexp-in-string
               "'\\|\)\\|\(" ""
               ;; If require use third arg, ignore it,
               ;; always use library path found in `load-path'.
               (second (split-string (match-string 0 sexp))))))
            ((and sexp (string-match-p "^declare-function" sexp))
             (find-library-name
              (replace-regexp-in-string
               "\"\\|ext:" ""
               (third (split-string sexp)))))
            (t nil)))))

;;; Anything completion for `write-file'.==> C-x C-w
(defvar anything-c-source-write-file
  `((name . "Write File")
    (header-name . (lambda (name)
                     (concat name anything-c-find-files-doc-header)))
    ;; It is needed for filenames with capital letters
    (disable-shortcuts)
    (candidates . anything-find-files-get-candidates)
    (filtered-candidate-transformer anything-c-find-files-transformer)
    (persistent-action . anything-find-files-persistent-action)
    (persistent-help . "Expand Candidate")
    (volatile)
    (action .
            (("Write File" . (lambda (candidate)
                               (write-file candidate 'confirm)))))))

;;; Anything completion for `insert-file'.==> C-x i
(defvar anything-c-source-insert-file
  `((name . "Insert File")
    (header-name . (lambda (name)
                     (concat name anything-c-find-files-doc-header)))
    ;; It is needed for filenames with capital letters
    (disable-shortcuts)
    (candidates . anything-find-files-get-candidates)
    (filtered-candidate-transformer anything-c-find-files-transformer)
    (persistent-action . anything-find-files-persistent-action)
    (persistent-help . "Expand Candidate")
    (volatile)
    (action .
            (("Insert File" . (lambda (candidate)
                                (when (y-or-n-p (format "Really insert %s in %s "
                                                        candidate anything-current-buffer))
                                  (insert-file-contents candidate))))))))

;;; Anything completion for copy, rename and (rel)sym/hard/link files from dired.
(defvar anything-c-source-copy-files
  `((name . "Copy Files")
    (header-name . (lambda (name)
                     (concat name anything-c-find-files-doc-header)))
    ;; It is needed for filenames with capital letters
    (disable-shortcuts)
    (candidates . anything-find-files-get-candidates)
    (filtered-candidate-transformer anything-c-find-files-transformer)
    (persistent-action . anything-find-files-persistent-action)
    (persistent-help . "Expand Candidate")
    (volatile)
    (action .
            (("Copy File"
              . (lambda (candidate)
                  (anything-dired-action candidate :action 'copy)))
             ("Copy and Follow"
              . (lambda (candidate)
                  (anything-dired-action candidate :action 'copy :follow t)))))))


(defvar  anything-c-source-rename-files
  `((name . "Rename Files")
    (header-name . (lambda (name)
                     (concat name anything-c-find-files-doc-header)))
    ;; It is needed for filenames with capital letters
    (disable-shortcuts)
    (candidates . anything-find-files-get-candidates)
    (filtered-candidate-transformer anything-c-find-files-transformer)
    (persistent-action . anything-find-files-persistent-action)
    (persistent-help . "Expand Candidate")
    (volatile)
    (action .
            (("Rename File"
              . (lambda (candidate)
                  (anything-dired-action candidate :action 'rename)))
             ("Rename and Follow"
              . (lambda (candidate)
                  (anything-dired-action candidate :action 'rename :follow t)))))))

(defvar anything-c-source-symlink-files
  `((name . "Symlink Files")
    (header-name . (lambda (name)
                     (concat name anything-c-find-files-doc-header)))
    ;; It is needed for filenames with capital letters
    (disable-shortcuts)
    (candidates . anything-find-files-get-candidates)
    (filtered-candidate-transformer anything-c-find-files-transformer)
    (persistent-action . anything-find-files-persistent-action)
    (persistent-help . "Expand Candidate")
    (volatile)
    (action
     . (("Symlink File"
         . (lambda (candidate)
             (anything-dired-action candidate :action 'symlink)))
        ("RelSymlink File"
         . (lambda (candidate)
             (anything-dired-action candidate :action 'relsymlink)))))))


(defvar anything-c-source-hardlink-files
  `((name . "Hardlink Files")
    (header-name . (lambda (name)
                     (concat name anything-c-find-files-doc-header)))
    ;; It is needed for filenames with capital letters
    (disable-shortcuts)
    (candidates . anything-find-files-get-candidates)
    (filtered-candidate-transformer anything-c-find-files-transformer)
    (persistent-action . anything-find-files-persistent-action)
    (persistent-help . "Expand Candidate")
    (volatile)
    (action
     . (("Hardlink File"
         . (lambda (candidate)
             (anything-dired-action candidate :action 'hardlink)))))))

(defun* anything-dired-action (candidate &key action follow (files (dired-get-marked-files)))
  "Copy, rename or symlink file at point or marked files in dired to CANDIDATE.
ACTION is a key that can be one of 'copy, 'rename, 'symlink, 'relsymlink."
  (when (get-buffer dired-log-buffer) (kill-buffer dired-log-buffer))
  (let ((fn     (case action
                  ('copy       'dired-copy-file)
                  ('rename     'dired-rename-file)
                  ('symlink    'make-symbolic-link)
                  ('relsymlink 'dired-make-relative-symlink)
                  ('hardlink   'dired-hardlink)))
        (marker (case action
                  ((copy rename)   dired-keep-marker-copy)
                  ('symlink        dired-keep-marker-symlink)
                  ('relsymlink     dired-keep-marker-relsymlink)
                  ('hardlink       dired-keep-marker-hardlink)))
        (dirflag (and (= (length files) 1)
                      (file-directory-p (car files))
                      (not (file-directory-p candidate)))))
    (dired-create-files
     fn (symbol-name action) files
     ;; CANDIDATE is the destination.
     (if (file-directory-p candidate)
         ;; When CANDIDATE is a directory, build file-name in this directory.
         ;; Else we use CANDIDATE.
         #'(lambda (from)
             (expand-file-name (file-name-nondirectory from) candidate))
         #'(lambda (from) candidate))
     marker)
    (push (file-name-as-directory
           (if (file-directory-p candidate)
               (expand-file-name candidate)
               (file-name-directory candidate)))
           anything-ff-history)
    (when (and follow (not (get-buffer dired-log-buffer)))
      (let ((target (directory-file-name candidate)))
        (unwind-protect
             (progn
               (setq anything-ff-cand-to-mark
                     (anything-get-dest-fnames-from-list files candidate dirflag))
               (if (and dirflag (eq action 'rename))
                   (anything-find-files-1 (file-name-directory target)
                                          (if anything-ff-transformer-show-only-basename
                                              (anything-c-basename target) target))
                   (anything-find-files-1 (expand-file-name candidate))))
          (setq anything-ff-cand-to-mark nil))))))

(defun anything-c-basename (fname &optional ext)
  "Print FNAME  with any  leading directory  components removed.
If specified, also remove filename extension EXT."
  (if (and ext (or (string= (file-name-extension fname) ext)
                   (string= (file-name-extension fname t) ext))
           (not (file-directory-p fname)))
      (file-name-sans-extension (file-name-nondirectory fname))
      (file-name-nondirectory (directory-file-name fname))))

(defun anything-get-dest-fnames-from-list (flist dest-cand rename-dir-flag)
  "Transform filenames of FLIST to abs of DEST-CAND.
If RENAME-DIR-FLAG is non--nil collect the `directory-file-name' of transformed
members of FLIST."
  ;; At this point files have been renamed/copied at destination.
  ;; That's mean DEST-CAND exists.
  (loop
        with dest = (expand-file-name dest-cand)
        for src in flist
        for basename-src = (anything-c-basename src)
        for fname = (cond (rename-dir-flag (directory-file-name dest))
                          ((file-directory-p dest)
                           (concat (file-name-as-directory dest) basename-src))
                          (t dest))
        when (file-exists-p fname)
        collect fname into tmp-list
        finally return (sort tmp-list 'string<)))

(defun anything-ff-maybe-mark-candidates ()
  "Mark all candidates of list `anything-ff-cand-to-mark'."
  (when (and (string= (assoc-default 'name (anything-get-current-source))
                      (assoc-default 'name anything-c-source-find-files))
             anything-ff-cand-to-mark)
    (with-anything-window
      (while anything-ff-cand-to-mark
        (if (string= (car anything-ff-cand-to-mark) (anything-get-selection))
            (progn
              (anything-make-visible-mark)
              (anything-next-line)
              (setq anything-ff-cand-to-mark (cdr anything-ff-cand-to-mark)))
            (anything-next-line)))
      (unless (anything-this-visible-mark)
        (anything-prev-visible-mark)))))

(add-hook 'anything-after-update-hook #'anything-ff-maybe-mark-candidates)

(defun* anything-dired-do-action-on-file (&key action)
  (let* ((files     (dired-get-marked-files))
         (len       (length files))
         (fname     (if (> len 1)
                        (format "* %d Files" len)
                        (car files)))
         (source    (case action
                      ('copy     'anything-c-source-copy-files)
                      ('rename   'anything-c-source-rename-files)
                      ('symlink  'anything-c-source-symlink-files)
                      ('hardlink 'anything-c-source-hardlink-files)))
         (prompt-fm (case action
                      ('copy     "Copy %s to: ")
                      ('rename   "Rename %s to: ")
                      ('symlink  "Symlink %s to: ")
                      ('hardlink "Hardlink %s to: ")))
         (buffer    (case action
                      ('copy     "*Anything Copy Files*")
                      ('rename   "*Anything Rename Files*")
                      ('symlink  "*Anything Symlink Files*")
                      ('hardlink "*Anything Hardlink Files*")))
         (anything-mp-highlight-delay     nil))
    (anything :sources source
              :input (or (dired-dwim-target-directory)
                         (expand-file-name (anything-c-current-directory)))
              :preselect (dired-get-filename)
              :prompt (format prompt-fm fname)
              :keymap anything-c-read-file-map
              :buffer buffer)))

;;;###autoload
(define-minor-mode anything-dired-mode ()
  "Enable anything completion in Dired functions.
Bindings affected are C, R, S, H.
This is deprecated for Emacs24+ users, use `ac-mode' instead."
  :group 'anything-config
  :global t
  (if anything-dired-mode
      (progn
        (substitute-key-definition
         'dired-do-copy 'anything-dired-copy-file dired-mode-map)
        (substitute-key-definition
         'dired-do-rename 'anything-dired-rename-file dired-mode-map)
        (substitute-key-definition
         'dired-do-symlink 'anything-dired-symlink-file dired-mode-map)
        (substitute-key-definition
         'dired-do-hardlink 'anything-dired-hardlink-file dired-mode-map))
      (substitute-key-definition
       'anything-dired-copy-file 'dired-do-copy dired-mode-map)
      (substitute-key-definition
       'anything-dired-rename-file 'dired-do-rename dired-mode-map)
      (substitute-key-definition
       'anything-dired-symlink-file 'dired-do-symlink dired-mode-map)
      (substitute-key-definition
       'anything-dired-hardlink-file 'dired-do-hardlink dired-mode-map)))

(defalias 'anything-dired-bindings 'anything-dired-mode)

(defun* anything-c-read-file-name
    (prompt
     &key
     (name "Read File Name")
     (initial-input (expand-file-name default-directory))
     (buffer "*Anything Completions*")
     test
     (preselect nil)
     (history nil)
     must-match
     (marked-candidates nil)
     (alistp t)
     (persistent-action 'anything-find-files-persistent-action)
     (persistent-help "Hit1 Expand Candidate, Hit2 or (C-u) Find file"))
  "Read a file name with anything completion.
It is anything `read-file-name' emulation.

Argument PROMPT is the default prompt to use.

Keys description:

- NAME: Source name, default to \"Read File Name\".

- INITIAL-INPUT: Where to start read file name, default to `default-directory'.

- BUFFER: `anything-buffer' name default to \"*Anything Completions*\".

- TEST: A predicate called with one arg 'candidate'.

- PRESELECT: anything preselection.

- HISTORY: Display HISTORY in a special source.

- MUST-MATCH: Can be 'confirm, nil, or t.

- MARKED-CANDIDATES: When non--nil return a list of marked candidates.

- ALISTP: Don't use `all-completions' in history (take effect only on history).

- PERSISTENT-ACTION: a persistent action function.

- PERSISTENT-HELP: persistent help message."
  (when (get-buffer anything-action-buffer)
    (kill-buffer anything-action-buffer))
  
  ;; Assume completion have been already required,
  ;; so always use 'confirm.
  (when (eq must-match 'confirm-after-completion)
    (setq must-match 'confirm))

  (flet ((action-fn (candidate)
           (if marked-candidates
               (anything-marked-candidates)
               (identity candidate))))
  
    (let* ((anything-mp-highlight-delay nil)
           ;; Be sure we don't erase the underlying minibuffer if some.
           (anything-ff-auto-update-initial-value
            (and anything-ff-auto-update-initial-value
                 (not (minibuffer-window-active-p (minibuffer-window)))))
           anything-same-window
           (hist (and history (anything-comp-read-get-candidates
                               history nil nil alistp)))
           (minibuffer-completion-confirm must-match)
           (must-match-map (when must-match
                             (let ((map (make-sparse-keymap)))
                               (define-key map (kbd "RET")
                                 'anything-confirm-and-exit-minibuffer)
                               map)))
           (anything-map (if must-match-map
                             (make-composed-keymap
                              must-match-map anything-c-read-file-map)
                             anything-c-read-file-map)))
      
      (or (anything
           :sources
           `(((name . ,(format "%s History" name))
              (header-name . (lambda (name)
                               (concat name anything-c-find-files-doc-header)))
              (disable-shortcuts)
              (mode-line . anything-read-file-name-mode-line-string)
              (candidates . hist)
              (persistent-action . ,persistent-action)
              (persistent-help . ,persistent-help)
              (action . ,'action-fn))
             ((name . ,name)
              (header-name . (lambda (name)
                               (concat name anything-c-find-files-doc-header)))
              (init . (lambda ()
                        (setq anything-ff-auto-update-flag
                              anything-ff-auto-update-initial-value)))
              ;; It is needed for filenames with capital letters
              (disable-shortcuts)
              (mode-line . anything-read-file-name-mode-line-string)
              (candidates
               . (lambda ()
                   (if test
                       (loop with hn = (anything-ff-tramp-hostnames)
                             for i in (anything-find-files-get-candidates
                                       must-match)
                             when (or (member i hn) (funcall test i))
                             collect i)
                       (anything-find-files-get-candidates must-match))))
              (filtered-candidate-transformer anything-c-find-files-transformer)
              (persistent-action . ,persistent-action)
              (candidate-number-limit . 9999)
              (toggle-auto-update . anything-ff-toggle-auto-update)
              (persistent-help . ,persistent-help)
              (volatile)
              (action . ,'action-fn)))
           :input initial-input
           :prompt prompt
           :resume 'noresume
           :buffer buffer
           :preselect preselect)
          (when (and (not (string= anything-pattern ""))
                     (eq anything-exit-status 0)
                     (eq must-match 'confirm))
            (identity anything-pattern))
          (keyboard-quit)))))


;;; File Cache
(defvar anything-c-file-cache-initialized-p nil)

(defvar anything-c-file-cache-files nil)

(defvar anything-c-source-file-cache
  `((name . "File Cache")
    (init
     . (lambda ()
         (require 'filecache nil t)
         (unless anything-c-file-cache-initialized-p
           (setq anything-c-file-cache-files
                 (loop for item in file-cache-alist append
                       (destructuring-bind (base &rest dirs) item
                         (loop for dir in dirs collect
                               (concat dir base)))))
           (defadvice file-cache-add-file (after file-cache-list activate)
             (add-to-list 'anything-c-file-cache-files (expand-file-name file)))
           (setq anything-c-file-cache-initialized-p t))))
    (keymap . ,anything-generic-files-map)
    (help-message . anything-generic-file-help-message)
    (mode-line . anything-generic-file-mode-line-string)
    (candidates . anything-c-file-cache-files)
    (match anything-c-match-on-basename)
    (type . file)))


;;; Locate
;;
;;
;; NOTE for WINDOZE users:
;; You have to install Everything with his command line interface here:
;; http://www.voidtools.com/download.php

(defun anything-ff-find-locatedb (&optional from-ff)
  "Try to find if a local locatedb file is available.
The search is done in `anything-ff-default-directory' or
fall back to `default-directory' if FROM-FF is nil."
  (when anything-ff-locate-db-filename
    (cond ((and anything-ff-default-directory
                from-ff
                (file-exists-p (expand-file-name
                                anything-ff-locate-db-filename
                                anything-ff-default-directory))
                (expand-file-name
                 anything-ff-locate-db-filename
                 anything-ff-default-directory)))
          ((and (not from-ff)
                (file-exists-p (expand-file-name
                                anything-ff-locate-db-filename
                                default-directory))
                (expand-file-name
                 anything-ff-locate-db-filename
                 default-directory))))))

(defun anything-locate-1 (&optional localdb init from-ff)
  "Generic function to run Locate.
if LOCALDB is non--nil search and use a local locate db file.
INIT is a string to use as initial input in prompt.
See `anything-locate-with-db' and `anything-locate'."
  (anything-locate-with-db
   (and localdb
        (or (anything-ff-find-locatedb from-ff)
            (anything-c-read-file-name
             "LocateDBFiles: "
             :initial-input (or anything-ff-default-directory
                                default-directory)
             :marked-candidates t
             :preselect anything-locate-db-file-regexp
             :test #'(lambda (x)
                       (if anything-locate-db-file-regexp
                           ;; Select only locate db files and directories
                           ;; to allow navigation.
                           (or (string-match
                                anything-locate-db-file-regexp x)
                               (file-directory-p x))
                           x)))))
   init))
;; (anything-locate-1 t)

(defun anything-locate-with-db (&optional db initial-input)
  "Run locate -d DB.
If DB is not given or nil use locate without -d option.
Argument DB can be given as a string or list of db files.
Argument INITIAL-INPUT is a string to use as initial-input.
See also `anything-locate'."
  (when (and db (stringp db)) (setq db (list db)))
  (unless anything-c-locate-command
    (setq anything-c-locate-command
          (case system-type
            ('gnu/linux "locate -i -r %s")
            ('berkeley-unix "locate -i %s")
            ('windows-nt "es -i -r %s")
            (t "locate %s"))))  
  (let ((anything-c-locate-command
         (if db
             (replace-regexp-in-string
              "locate"
              (format "locate -d %s"
                      (mapconcat 'identity
                                 ;; Remove eventually
                                 ;; marked directories by error.
                                 (loop for i in db
                                       unless (file-directory-p i)
                                       collect i) ":"))
              anything-c-locate-command)
             anything-c-locate-command)))
    (anything :sources 'anything-c-source-locate
              :buffer "*anything locate*"
              :input initial-input
              :keymap anything-generic-files-map)))
;; (anything-locate-with-db "~/locate.db")

(defun anything-c-locate-init ()
  "Initialize async locate process for `anything-c-source-locate'."
  (setq mode-line-format
        '(" " mode-line-buffer-identification " "
          (line-number-mode "%l") " "
          (:eval (propertize "(Locate Process Running) "
                  'face '((:foreground "red"))))))
  (prog1
      (start-process-shell-command "locate-process" nil
                                   (format anything-c-locate-command
                                           anything-pattern))
    (set-process-sentinel (get-process "locate-process")
                          #'(lambda (process event)
                              (when (string= event "finished\n")
                                (with-anything-window
                                  (force-mode-line-update nil)
                                  (anything-update-move-first-line)))))))

(defvar anything-c-source-locate
  `((name . "Locate")
    (candidates . anything-c-locate-init)
    (type . file)
    (requires-pattern . 3)
    (keymap . ,anything-generic-files-map)
    (help-message . anything-generic-file-help-message)
    (candidate-number-limit . 9999)
    (mode-line . anything-generic-file-mode-line-string)
    (delayed))
  "Find files matching the current input pattern with locate.")

(defun anything-c-locate-read-file-name (prompt &optional init)
  "Search a file with locate and return it's filename.
Use argument PROMPT and INIT for `anything' arguments
prompt and input."
  (anything :sources
            '((name . "Locate")
              (candidates . anything-c-locate-init)
              (action . identity)
              (requires-pattern . 3)
              (candidate-number-limit . 9999)
              (mode-line . anything-generic-file-mode-line-string)
              (delayed))
            :prompt prompt
            :input init
            :buffer "*anything locate rfn*"))



;;; Anything Incremental Grep.
;;
;;
;; Allow to grep incrementally with anything interface.
;; It allow also to Grep files recursively without using 'find' shell command.
;; On Windows you will need at least Grep version 2.5.4 of Gnuwin32.
(defvar anything-c-grep-default-command
  "grep -d skip %e -niH -e %p %f"
  "Default grep format command for `anything-do-grep-1'.
Where:
'%e' format spec is for --exclude or --include grep options.
'%p' format spec is for pattern.
'%f' format spec is for filenames.")

(defvar anything-c-grep-default-recurse-command
  "grep -d recurse %e -niH -e %p %f"
  "Default recursive grep format command for `anything-do-grep-1'.
See `anything-c-grep-default-command' for format specs.")

(defvar anything-c-default-zgrep-command "zgrep -niH -e %p %f")

(defvar anything-c-rzgrep-cache (make-hash-table :test 'equal))

(defvar anything-c-grep-default-function 'anything-c-grep-init)

(defvar anything-c-grep-debug-command-line nil
  "Turn on anything grep command-line debugging when non--nil.")

(defvar anything-c-zgrep-recurse-flag nil)

(defvar anything-c-grep-history nil)

(defvar anything-c-grep-max-length-history 100
  "*Max number of elements to save in `anything-c-grep-history'.")

(defun anything-c-grep-prepare-candidates (candidates)
  "Prepare filenames and directories CANDIDATES for grep command line."
  ;; If one or more candidate is a directory, search in all files
  ;; of this candidate (e.g /home/user/directory/*).
  ;; If r option is enabled search also in subdidrectories.
  ;; We need here to expand wildcards to support crap windows filenames
  ;; as grep doesn't accept quoted wildcards (e.g "dir/*.el").
  (if anything-c-zgrep-recurse-flag
      (mapconcat 'shell-quote-argument candidates " ")
      (loop for i in candidates append
            (cond ( ;; Candidate is a directory and we use recursion.
                   (and (file-directory-p i)
                        (anything-c-grep-recurse-p))
                   (list (expand-file-name i)))
                  ;; Candidate is a directory, search in all files.
                  ((file-directory-p i)
                   (file-expand-wildcards
                    (concat (file-name-as-directory (expand-file-name i)) "*") t))
                  ;; Candidate is a file or wildcard and we use recursion, use the
                  ;; current directory instead of candidate.
                  ((and (or (file-exists-p i) (string-match "\*" i))
                        (anything-c-grep-recurse-p))
                   (list (expand-file-name
                          (directory-file-name ; Needed for windoze.
                           (file-name-directory (directory-file-name i))))))
                  ;; Candidate use wildcard.
                  ((string-match "^\*" (anything-c-basename i))
                   (file-expand-wildcards i t))
                  ;; Else should be one or more file.
                  (t (list i))) into all-files
            finally return
            (mapconcat 'shell-quote-argument all-files " "))))

(defun anything-c-grep-recurse-p ()
  "Check if `anything-do-grep-1' have switched to recursive."
  (let ((args (replace-regexp-in-string
               "grep" "" anything-c-grep-default-command)))
    (string-match-p "r\\|recurse" args)))

(defun anything-c-grep-init (only-files &optional include zgrep)
  "Start an asynchronous grep process in ONLY-FILES list."
  (let* ((fnargs        (anything-c-grep-prepare-candidates
                         (if (file-remote-p anything-ff-default-directory)
                             (mapcar #'(lambda (x)
                                         (file-remote-p x 'localname))
                                     only-files)
                             only-files)))
         (ignored-files (mapconcat
                         #'(lambda (x)
                             (concat "--exclude=" (shell-quote-argument x)))
                         grep-find-ignored-files " "))
         (ignored-dirs  (mapconcat
                         ;; Need grep version >=2.5.4 of Gnuwin32 on windoze.
                         #'(lambda (x)
                             (concat "--exclude-dir=" (shell-quote-argument x)))
                         grep-find-ignored-directories " "))
         (exclude       (if (anything-c-grep-recurse-p)
                            (concat (or include ignored-files) " " ignored-dirs)
                            ignored-files))
         (cmd-line      (format-spec
                         anything-c-grep-default-command
                         (delq nil
                               (list (unless zgrep (cons ?e exclude))
                                     (cons ?p (shell-quote-argument anything-pattern))
                                     (cons ?f fnargs))))))
    (when anything-c-grep-debug-command-line
      (with-current-buffer (get-buffer-create "*any grep debug*")
        (goto-char (point-max))
        (insert (concat ">>> " cmd-line "\n\n"))))
    (setq mode-line-format
          '(" " mode-line-buffer-identification " "
            (line-number-mode "%l") " "
            (:eval (when (get-process "grep-process")
                     (propertize "[Grep Process Running] "
                                 'face 'anything-grep-running)))))
    (force-mode-line-update nil)
    (prog1
        (let ((default-directory anything-ff-default-directory))
          (start-file-process-shell-command "grep-process" nil cmd-line))
      (message nil)
      (set-process-sentinel
       (get-process "grep-process")
       #'(lambda (process event)
           (when (string= event "finished\n")
             (with-anything-window
               (anything-update-move-first-line)
               (setq mode-line-format
                     '(" " mode-line-buffer-identification " "
                       (line-number-mode "%l") " "
                       (:eval (propertize
                               (format "[Grep Process Finished - (%s results)] "
                                       (let ((nlines (1- (count-lines
                                                          (point-min)
                                                          (point-max)))))
                                         (if (> nlines 0) nlines 0)))
                               'face 'anything-grep-finish))))
               (force-mode-line-update nil))))))))

(defun anything-c-grep-action (candidate &optional where mark)
  "Define a default action for `anything-do-grep' on CANDIDATE.
WHERE can be one of other-window, elscreen, other-frame."
  (let* ((split        (anything-c-grep-split-line candidate))
         (lineno       (string-to-number (nth 1 split)))
         (loc-fname    (car split))
         (tramp-method (file-remote-p anything-ff-default-directory 'method))
         (tramp-host   (file-remote-p anything-ff-default-directory 'host))
         (tramp-prefix (concat "/" tramp-method ":" tramp-host ":"))
         (fname        (if tramp-host
                           (concat tramp-prefix loc-fname) loc-fname)))
    (case where
      (other-window (find-file-other-window fname))
      (elscreen     (anything-elscreen-find-file fname))
      (other-frame  (find-file-other-frame fname))
      (grep         (anything-c-grep-save-results-1))
      (t (find-file fname)))
    (unless (eq where 'grep)
      (anything-goto-line lineno))
    (when mark
      (set-marker (mark-marker) (point))
      (push-mark (point) 'nomsg))
    ;; Save history
    (unless (or anything-in-persistent-action
                (string= anything-pattern ""))
      (setq anything-c-grep-history
            (cons anything-pattern
                  (delete anything-pattern anything-c-grep-history)))
      (when (> (length anything-c-grep-history)
               anything-c-grep-max-length-history)
        (setq anything-c-grep-history
              (delete (car (last anything-c-grep-history))
                      anything-c-grep-history))))))

(defun anything-c-grep-other-window (candidate)
  "Jump to result in other window from anything grep."
  (anything-c-grep-action candidate 'other-window))

(defun anything-c-grep-other-frame (candidate)
  "Jump to result in other frame from anything grep."
  (anything-c-grep-action candidate 'other-frame))

(defun anything-c-grep-jump-elscreen (candidate)
  "Jump to result in elscreen from anything grep."
  (anything-c-grep-action candidate 'elscreen))

(defun anything-c-grep-save-results (_candidate)
  (anything-c-grep-action _candidate 'grep))

(defun anything-c-grep-save-results-1 ()
  "Save anything grep result in a `grep-mode' buffer."
  (let ((buf "*grep*")
        new-buf)
    (when (get-buffer buf)
      (setq new-buf (read-string "GrepBufferName: " buf))
      (loop for b in (anything-c-buffer-list)
            when (and (string= new-buf b)
                      (not (y-or-n-p
                            (format "Buffer `%s' already exists overwrite? "
                                    new-buf))))
            do (setq new-buf (read-string "GrepBufferName: " "*grep ")))
      (setq buf new-buf))
    (with-current-buffer (get-buffer-create buf)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "-*- mode: grep -*-\n\n"
                (format "Grep Results for `%s':\n\n" anything-pattern))
        (save-excursion
          (insert (with-current-buffer anything-buffer
                    (goto-char (point-min)) (forward-line 1)
                    (buffer-substring (point) (point-max))))
          (grep-mode))))
    (message "Anything Grep Results saved in `%s' buffer" buf)))

(defun anything-c-grep-persistent-action (candidate)
  "Persistent action for `anything-do-grep'.
With a prefix arg record CANDIDATE in `mark-ring'."
  (if current-prefix-arg
      (anything-c-grep-action candidate nil 'mark)
      (anything-c-grep-action candidate))
  (anything-match-line-color-current-line))

(defun anything-c-grep-guess-extensions (files)
  "Try to guess file extensions in FILES list when using grep recurse.
These extensions will be added to command line with --include arg of grep."
  (loop
        with glob-list = nil
        with lst = (if (file-directory-p (car files))
                       (directory-files
                        (car files) nil
                        directory-files-no-dot-files-regexp)
                       files)
        for i in lst
        for ext = (file-name-extension i t)
        for glob = (and ext (not (string= ext ""))
                        (concat "*" ext))
        unless (or (not glob)
                   (member glob glob-list)
                   (member glob grep-find-ignored-files))
        collect glob into glob-list
        finally return glob-list))

(defun anything-do-grep-1 (only &optional recurse zgrep)
  "Launch grep with a list of ONLY files.
When RECURSE is given use -r option of grep and prompt user
to set the --include args of grep.
You can give more than one arg separated by space.
e.g *.el *.py *.tex.
If it's empty --exclude `grep-find-ignored-files' is used instead."
  (let* ((anything-compile-source-functions
          ;; rule out anything-match-plugin because the input is one regexp.
          (delq 'anything-compile-source--match-plugin
                (copy-sequence anything-compile-source-functions)))
         (exts (anything-c-grep-guess-extensions only))
         (globs (and (not zgrep) (mapconcat 'identity exts " ")))
         (include-files (and recurse (not zgrep)
                             (read-string "OnlyExt(*.[ext]): "
                                          globs)))
         ;; Set `minibuffer-history' AFTER includes-files
         ;; to avoid storing wild-cards here.
         (minibuffer-history anything-c-grep-history)
         (anything-c-grep-default-command (cond ((and recurse zgrep) anything-c-default-zgrep-command)
                                                (recurse anything-c-grep-default-recurse-command)
                                                (zgrep anything-c-default-zgrep-command)
                                                (t anything-c-grep-default-command)))
         ;; Disable match-plugin and use here own highlighting.
         (anything-mp-highlight-delay     nil))
    (when include-files
      (setq include-files
            (and (not (string= include-files ""))
                 (mapconcat #'(lambda (x)
                                (concat "--include=" (shell-quote-argument x)))
                            (split-string include-files) " "))))
    ;; When called as action from an other source e.g *-find-files
    ;; we have to kill action buffer.
    (when (get-buffer anything-action-buffer)
      (kill-buffer anything-action-buffer))
    ;; `anything-find-files' haven't already started,
    ;; give a default value to `anything-ff-default-directory'.
    (setq anything-ff-default-directory (or anything-ff-default-directory
                                            default-directory))
    (anything
     :sources
     `(((name . "Grep")
        (header-name . (lambda (name)
                         (concat name "(C-c ? Help)")))
        (candidates
         . (lambda ()
             (funcall anything-c-grep-default-function only include-files zgrep)))
        (filtered-candidate-transformer anything-c-grep-cand-transformer)
        (candidate-number-limit . 9999)
        (mode-line . anything-grep-mode-line-string)
        (keymap . ,anything-c-grep-map)
        (action . ,(delq
                    nil
                    `(("Find File" . anything-c-grep-action)
                      ("Find file other frame" . anything-c-grep-other-frame)
                      ,(and (locate-library "elscreen")
                            '("Find file in Elscreen"
                              . anything-c-grep-jump-elscreen))
                      ("Save results in grep buffer" . anything-c-grep-save-results)
                      ("Find file other window" . anything-c-grep-other-window))))
        (persistent-action . anything-c-grep-persistent-action)
        (persistent-help . "Jump to line (`C-u' Record in mark ring)")
        (requires-pattern . 3)
        (delayed)))
     :buffer "*anything grep*")))

(defun anything-ff-zgrep-1 (flist recursive)
  (unwind-protect
       (let* ((def-dir (or anything-ff-default-directory
                           default-directory))
              (only    (if recursive
                           (or (gethash def-dir anything-c-rzgrep-cache)
                               (puthash
                                def-dir
                                (anything-c-walk-directory
                                 def-dir
                                 :directories nil
                                 :path 'full
                                 :match ".*\\(\.gz\\|\.bz\\|\.xz\\|\.lzma\\)$")
                                anything-c-rzgrep-cache))
                           flist)))
         (when recursive (setq anything-c-zgrep-recurse-flag t))
         (anything-do-grep-1 only recursive 'zgrep))
    (setq anything-c-zgrep-recurse-flag nil)))

(defun anything-c-grep-split-line (line)
  "Split a grep output line."
  (let (beg fname lineno str)
    ;; Don't print until grep line is valid.
    (when (string-match "\\(.*\\)\\(:[0-9]+:\\)\\(.*\\)" line)
      (with-temp-buffer
        (insert line)
        (goto-char (point-min))
        (setq beg (point))
        (forward-char 2)
        (re-search-forward ":" nil t)
        (setq fname (buffer-substring-no-properties beg (1- (point))))
        (setq beg (point))
        (re-search-forward ":" nil t)
        (setq lineno (buffer-substring-no-properties beg (1- (point))))
        (setq str (buffer-substring-no-properties (point) (point-at-eol))))
      (list fname lineno str))))

(defun anything-c-grep-cand-transformer (candidates sources)
  "Filtered candidate transformer function for `anything-do-grep'."
  (loop for i in candidates
        for split  = (and i (anything-c-grep-split-line i))
        for fname  = (car split)
        for lineno = (nth 1 split)
        for str    = (nth 2 split)
        when (and fname lineno str)
        collect
        (cons (concat (propertize (file-name-nondirectory fname)
                                  'face 'anything-grep-file
                                  'help-echo fname) ":"
                                  (propertize lineno 'face 'anything-grep-lineno) ":"
                                  (anything-c-grep-highlight-match str))
              i)))

(defun anything-c-grep-highlight-match (str)
  "Highlight in string STR all occurences matching `anything-pattern'."
  (condition-case nil
      (with-temp-buffer
        (insert str)
        (goto-char (point-min))
        (while (and (re-search-forward anything-pattern nil t)
                    (> (- (match-end 0) (match-beginning 0)) 0))
          (add-text-properties
           (match-beginning 0) (match-end 0)
           '(face anything-grep-match)))
        (buffer-string))
    (error nil)))

;; Go to next or precedent file (common to etags and grep).
(defun anything-c-goto-next-or-prec-file (n)
  "Go to next or precedent candidate file in anything grep/etags buffers.
If N is positive go forward otherwise go backward."
  (with-anything-window
    (let* ((current-line-list  (split-string
                                (buffer-substring
                                 (point-at-bol)
                                 (point-at-eol)) ":"))
           (current-fname      (nth 0 current-line-list))
           (fn-b-o-f           (if (eq n 1) 'eobp 'bobp)))
      (catch 'break
        (while (not (funcall fn-b-o-f))
          (forward-line n) ; Go forward or backward depending of n value.
          (unless (search-forward current-fname (point-at-eol) t)
            (anything-mark-current-line)
            (throw 'break nil))))
      (cond ((and (eq n 1) (eobp))
             (re-search-backward ".")
             (forward-line 0)
             (anything-mark-current-line))
            ((and (< n 1) (bobp))
             (forward-line 1)
             (anything-mark-current-line))))))

;;;###autoload
(defun anything-c-goto-precedent-file ()
  "Go to precedent file in anything grep/etags buffers."
  (interactive)
  (anything-c-goto-next-or-prec-file -1))

;;;###autoload
(defun anything-c-goto-next-file ()
  "Go to precedent file in anything grep/etags buffers."
  (interactive)
  (anything-c-goto-next-or-prec-file 1))

;;;###autoload
(defun anything-c-grep-run-persistent-action ()
  "Run grep persistent action from `anything-do-grep-1'."
  (interactive)
  (anything-attrset 'jump-persistent 'anything-c-grep-persistent-action)
  (anything-execute-persistent-action 'jump-persistent))

;;;###autoload
(defun anything-c-grep-run-default-action ()
  "Run grep default action from `anything-do-grep-1'."
  (interactive)
  (anything-c-quit-and-execute-action 'anything-c-grep-action))

;;;###autoload
(defun anything-c-grep-run-other-window-action ()
  "Run grep goto other window action from `anything-do-grep-1'."
  (interactive)
  (anything-c-quit-and-execute-action 'anything-c-grep-other-window))

;;;###autoload
(defun anything-c-grep-run-save-buffer ()
  "Run grep save results action from `anything-do-grep-1'."
  (interactive)
  (anything-c-quit-and-execute-action 'anything-c-grep-save-results))

;; Grep buffers
(defun anything-c-grep-buffers-1 (candidate &optional zgrep)
  "Run grep on all file--buffers or CANDIDATE if it is a file--buffer.
If one of selected buffers is not a file--buffer,
it is ignored and grep will run on all others file--buffers.
If only one candidate is selected and it is not a file--buffer,
switch to this buffer and run `anything-occur'.
If a prefix arg is given run grep on all buffers ignoring non--file-buffers."
  (let* ((prefarg (or current-prefix-arg anything-current-prefix-arg))
         (cands (if prefarg
                    (buffer-list)
                    (anything-marked-candidates)))
         (win-conf (current-window-configuration))
         ;; Non--fname buffers are ignored.
         (bufs (loop for buf in cands
                     for fname = (buffer-file-name (get-buffer buf))
                     when fname
                     collect (expand-file-name fname))))
    (if bufs
        (if zgrep
            (anything-do-grep-1 bufs nil 'zgrep)
            (anything-do-grep-1 bufs))
        ;; bufs is empty, thats mean we have only CANDIDATE
        ;; and it is not a buffer-filename, fallback to occur.
        (anything-c-switch-to-buffer candidate)
        (when (get-buffer anything-action-buffer)
          (kill-buffer anything-action-buffer))
        (anything-occur)
        (when (eq anything-exit-status 1)
          (set-window-configuration win-conf)))))

(defun anything-c-grep-buffers (candidate)
  "Action to grep buffers."
  (anything-c-grep-buffers-1 candidate))

(defun anything-c-zgrep-buffers (candidate)
  "Action to zgrep buffers."
  (anything-c-grep-buffers-1 candidate 'zgrep))


;;; Anything interface for pdfgrep
;;  pdfgrep program <http://pdfgrep.sourceforge.net/>
;;  and a pdf-reader (e.g xpdf) are needed.
;;
(defvar anything-c-pdfgrep-default-command "pdfgrep --color never -niH %s %s")
(defvar anything-c-pdfgrep-default-function 'anything-c-pdfgrep-init)
(defvar anything-c-pdfgrep-debug-command-line nil)

(defun anything-c-pdfgrep-init (only-files)
  "Start an asynchronous pdfgrep process in ONLY-FILES list."
  (let* ((fnargs   (anything-c-grep-prepare-candidates
                    (if (file-remote-p anything-ff-default-directory)
                        (mapcar #'(lambda (x)
                                    (file-remote-p x 'localname))
                                only-files)
                        only-files)))
         (cmd-line (format anything-c-pdfgrep-default-command
                           anything-pattern
                           fnargs)))
    (when anything-c-pdfgrep-debug-command-line
      (with-current-buffer (get-buffer-create "*any pdfgrep debug*")
        (goto-char (point-max))
        (insert (concat ">>> " cmd-line "\n\n"))))
    (setq mode-line-format
          '(" " mode-line-buffer-identification " "
            (line-number-mode "%l") " "
            (:eval (propertize "(Pdfgrep Process Running) "
                    'face '((:foreground "red"))))))
    (prog1
        (let ((default-directory anything-ff-default-directory))
          (start-file-process-shell-command "pdfgrep-process" nil cmd-line))
      (message nil)
      (set-process-sentinel
       (get-process "pdfgrep-process")
       #'(lambda (process event)
           (when (string= event "finished\n")
             (with-anything-window
               (anything-update-move-first-line))
             (force-mode-line-update nil)))))))


(defun anything-do-pdfgrep-1 (only)
  "Launch pdfgrep with a list of ONLY files."
  (unless (executable-find "pdfgrep")
    (error "Error: No such program `pdfgrep'."))
  (let* ((anything-compile-source-functions
          ;; rule out anything-match-plugin because the input is one regexp.
          (delq 'anything-compile-source--match-plugin
                (copy-sequence anything-compile-source-functions)))
         ;; Disable match-plugin and use here own highlighting.
         (anything-mp-highlight-delay nil))
    ;; When called as action from an other source e.g *-find-files
    ;; we have to kill action buffer.
    (when (get-buffer anything-action-buffer)
      (kill-buffer anything-action-buffer))
    ;; If `anything-find-files' haven't already started,
    ;; give a default value to `anything-ff-default-directory'.
    (setq anything-ff-default-directory (or anything-ff-default-directory
                                            default-directory))
    (anything
     :sources
     `(((name . "PdfGrep")
        (candidates
         . (lambda ()
             (funcall anything-c-pdfgrep-default-function only)))
        (filtered-candidate-transformer anything-c-grep-cand-transformer)
        (candidate-number-limit . 9999)
        (mode-line . anything-pdfgrep-mode-line-string)
        (action . anything-c-pdfgrep-action)
        (persistent-help . "Jump to PDF Page")
        (requires-pattern . 3)
        (delayed)))
     :keymap anything-c-pdfgrep-map
     :buffer "*anything grep*")))


(defun anything-c-pdfgrep-action (candidate)
  (let* ((split  (anything-c-grep-split-line candidate))
         (pageno (nth 1 split))
         (fname  (car split)))
    (start-file-process-shell-command
     "pdf-reader" nil
     (format-spec anything-c-pdfgrep-default-read-command
                  (list (cons ?f fname) (cons ?p pageno))))))


;; Yank text at point.
;;
;;
;; Internal
(defvar anything-yank-point nil)

;;;###autoload
(defun anything-yank-text-at-point ()
  "Yank text at point in minibuffer."
  (interactive)
  (let (input)
    (flet ((insert-in-minibuffer (word)
             (with-selected-window (minibuffer-window)
               (let ((str anything-pattern))
                 (delete-minibuffer-contents)
                 (set-text-properties 0 (length word) nil word)
                 (insert (concat str word))))))
      (with-anything-current-buffer
        ;; Start to initial point if C-w have never been hit.
        (unless anything-yank-point (setq anything-yank-point (point)))
        (and anything-yank-point (goto-char anything-yank-point))
        (forward-word 1)
        (setq input (buffer-substring-no-properties anything-yank-point (point)))
        (setq anything-yank-point (point))) ; End of last forward-word
      (insert-in-minibuffer input))))

(defun anything-reset-yank-point ()
  (setq anything-yank-point nil))

(add-hook 'anything-after-persistent-action-hook 'anything-reset-yank-point)
(add-hook 'anything-cleanup-hook 'anything-reset-yank-point)


;;; Recentf files
;;
;;
(defvar anything-c-source-recentf
  `((name . "Recentf")
    (init . (lambda ()
              (require 'recentf)
              (or recentf-mode (recentf-mode 1))))
    ;; Needed for filenames with capitals letters.
    (disable-shortcuts)
    (candidates . recentf-list)
    (keymap . ,anything-generic-files-map)
    (help-message . anything-generic-file-help-message)
    (mode-line . anything-generic-file-mode-line-string)
    (match anything-c-match-on-basename)
    (type . file))
  "See (info \"(emacs)File Conveniences\").
Set `recentf-max-saved-items' to a bigger value if default is too small.")

;;; ffap
(eval-when-compile (require 'ffap))
(defvar anything-c-source-ffap-guesser
  `((name . "File at point")
    (init . (lambda () (require 'ffap)))
    (candidates . (lambda ()
                    (anything-aif
                        (with-anything-current-buffer
                          (ffap-guesser))
                        (list it))))
    (keymap . ,anything-generic-files-map)
    (help-message . anything-generic-file-help-message)
    (mode-line . anything-generic-file-mode-line-string)
    (type . file)))

;;; ffap with line number
(defun anything-c-ffap-file-line-at-point ()
  "Get (FILENAME . LINENO) at point."
  (anything-aif (let (ffap-alist) (ffap-file-at-point))
      (save-excursion
        (beginning-of-line)
        (when (and (search-forward it nil t)
                   (looking-at ":\\([0-9]+\\)"))
          (cons it (string-to-number (match-string 1)))))))

(defun anything-c-ffap-line-candidates ()
  (with-anything-current-buffer
    (anything-attrset 'ffap-line-location (anything-c-ffap-file-line-at-point)))
  (anything-aif (anything-attr 'ffap-line-location)
    (destructuring-bind (file . line) it
      (list (cons (format "%s (line %d)" file line) file)))))

;;; Goto line after opening file by `anything-c-source-ffap-line'.
(defun anything-c-ffap-line-goto-line ()
  (when (car (anything-attr 'ffap-line-location))
    (unwind-protect
        (ignore-errors
          (with-selected-window
              (get-buffer-window
               (get-file-buffer (car (anything-attr 'ffap-line-location))))
            (anything-goto-line (cdr (anything-attr 'ffap-line-location)))))
      (anything-attrset 'ffap-line-location nil))))
(add-hook 'anything-after-action-hook 'anything-c-ffap-line-goto-line)
(add-hook 'anything-after-persistent-action-hook 'anything-c-ffap-line-goto-line)

(defvar anything-c-source-ffap-line
  `((name . "File/Lineno at point")
    (init . (lambda () (require 'ffap)))
    (candidates . anything-c-ffap-line-candidates)
    (keymap . ,anything-map)
    (type . file)))

;;; list of files gleaned from every dired buffer
(defun anything-c-files-in-all-dired-candidates ()
  (save-excursion
    (mapcan
     (lambda (dir)
       (cond ((listp dir)               ;filelist
              dir)
             ((equal "" (file-name-nondirectory dir)) ;dir
              (directory-files dir t))
             (t                         ;wildcard
              (file-expand-wildcards dir t))))
     (delq nil
           (mapcar (lambda (buf)
                     (set-buffer buf)
                     (when (eq major-mode 'dired-mode)
                       (if (consp dired-directory)
                           (cdr dired-directory) ;filelist
                           dired-directory))) ;dir or wildcard
                   (buffer-list))))))
;; (dired '("~/" "~/.emacs-custom.el" "~/.emacs.bmk"))

(defvar anything-c-source-files-in-all-dired
  '((name . "Files in all dired buffer.")
    (candidates . anything-c-files-in-all-dired-candidates)
    (type . file)))

(defvar anything-c-source-filelist
  '((name . "FileList")
    (grep-candidates . anything-c-filelist-file-name)
    (candidate-number-limit . 200)
    (requires-pattern . 4)
    (type . file))
  "Source to find files instantly.
See `anything-c-filelist-file-name' docstring for usage.")


;;;; <info>
;;; Info pages
(defvar anything-c-info-pages nil
  "All info pages on system.
Will be calculated the first time you invoke anything with this
source.")

(defun anything-c-info-pages-init ()
  "Collect candidates for initial Info node Top."
  (if anything-c-info-pages
      anything-c-info-pages
      (let ((info-topic-regexp "\\* +\\([^:]+: ([^)]+)[^.]*\\)\\.")
            topics)
        (require 'info)
        (with-temp-buffer
          (Info-find-node "dir" "top")
          (goto-char (point-min))
          (while (re-search-forward info-topic-regexp nil t)
            (push (match-string-no-properties 1) topics))
          (kill-buffer))
        (setq anything-c-info-pages topics))))

(defvar anything-c-source-info-pages
  `((name . "Info Pages")
    (init . anything-c-info-pages-init)
    (candidates . anything-c-info-pages)
    (action . (("Show with Info" .(lambda (node-str)
                                    (info (replace-regexp-in-string
                                           "^[^:]+: " "" node-str))))))
    (requires-pattern . 2)))


;;; Man and woman UI
;;
;;
(defvar anything-c-man-pages nil
  "All man pages on system.
Will be calculated the first time you invoke anything with this
source.")

(defun anything-c-man-default-action (candidate)
  "Default action for jumping to a woman or man page from anything."
  (let ((wfiles (woman-file-name-all-completions candidate)))
    (condition-case err
        (if (> (length wfiles) 1)
            (woman-find-file
             (anything-comp-read
              "ManFile: " wfiles :must-match t))
            (woman candidate))
      ;; If woman is unable to format correctly
      ;; use man instead.
      (error (kill-buffer) ; Kill woman buffer.
             (let ((Man-notify-method 'meek))
               (Man-getpage-in-background candidate))))))

(defvar anything-c-source-man-pages
  `((name . "Manual Pages")
    (candidates . (lambda ()
                    (if anything-c-man-pages
                        anything-c-man-pages
                        ;; XEmacs doesn't have a woman :)
                        (setq anything-c-man-pages
                              (ignore-errors
                                (require 'woman)
                                (woman-file-name "")
                                (sort (mapcar 'car woman-topic-all-completions)
                                      'string-lessp))))))
    (action  ("Show with Woman" . anything-c-man-default-action))
    ;; Woman does not work OS X
    ;; http://xahlee.org/emacs/modernization_man_page.html
    (action-transformer . (lambda (actions candidate)
                            (if (eq system-type 'darwin)
                                '(("Show with Man" . man))
                                actions)))
    (requires-pattern . 2)))


;;;; <Command>
;;; Anything M-x - Enhanced M-x UI
;;
;;
;; Another replacement of `M-x' that act exactly like the
;; vanilla Emacs one, no problem of windows configuration, prefix args
;; can be passed before calling `M-x' (e.g C-u M-x..) but also during
;; anything invocation.
;; Documentation of commands available without quitting,
;; Show keybindings of commands.
;; Show history.
(defvar anything-M-x-input-history nil)

(defun* anything-M-x-get-major-mode-command-alist (mode-map)
  "Return alist of MODE-MAP."
  (loop for key being the key-seqs of mode-map using (key-bindings com)
        for str-key  = (key-description key)
        for ismenu   = (string-match "<menu-bar>" str-key)
        unless ismenu collect (cons str-key com)))

(defun anything-get-mode-map-from-mode (mode)
  "Guess the mode-map name according to MODE.
Some modes don't use conventional mode-map name
so we need to guess mode-map name. e.g python-mode ==> py-mode-map.
Return nil if no mode-map found."
  (loop
        ;; Start with a conventional mode-map name.
        with mode-map    = (intern-soft (format "%s-map" mode))
        with mode-string = (symbol-name mode)
        with mode-name   = (replace-regexp-in-string "-mode" "" mode-string)
        while (not mode-map)
        for count downfrom (length mode-name)
        ;; Return when no result after parsing entire string.
        when (eq count 0) return nil
        for sub-name = (substring mode-name 0 count)
        do (setq mode-map (intern-soft (format "%s-map" (concat sub-name "-mode"))))
        finally return mode-map))

(defun anything-M-x-current-mode-map-alist ()
  "Return mode-map alist of current `major-mode'."
  (let ((map (anything-get-mode-map-from-mode major-mode)))
    (when (and map (boundp map))
      (anything-M-x-get-major-mode-command-alist (symbol-value map)))))


(defun anything-M-x-transformer (candidates sources)
  "filtered-candidate-transformer to show bindings in emacs commands.
Show global bindings and local bindings according to current `major-mode'."
  (with-anything-current-buffer
    (loop with local-map = (anything-M-x-current-mode-map-alist)
          for cand in candidates
          for local-key  = (car (rassq cand local-map))
          for key        = (substitute-command-keys (format "\\[%s]" cand))
          collect
          (cons (cond ((and (string-match "^M-x" key) local-key)
                       (format "%s (%s)"
                               cand (propertize
                                     local-key
                                     'face 'anything-M-x-key-face)))
                      ((string-match "^M-x" key) cand)
                      (t (format "%s (%s)"
                                 cand (propertize
                                       key
                                       'face 'anything-M-x-key-face))))
                cand) into ls
          finally return
          (sort ls #'(lambda (x y) (string-lessp (car x) (car y)))))))


;;; Complex command history
;;
;;
(defvar anything-c-source-complex-command-history
  '((name . "Complex Command History")
    (candidates . (lambda () (mapcar 'prin1-to-string command-history)))
    (type . sexp)))

;;; M-x history (not related to `anything-M-x')
;;
;;
(defvar anything-c-source-extended-command-history
  '((name . "Emacs Commands History")
    (candidates
     . (lambda ()
         (anything-fast-remove-dups extended-command-history :test 'equal)))
    (type . command)))

;;; Emacs commands (Basic source for emacs commands)
;;
;;
(defvar anything-c-source-emacs-commands
  '((name . "Emacs Commands")
    (candidates . (lambda ()
                    (let (commands)
                      (mapatoms (lambda (a)
                                  (if (commandp a)
                                      (push (symbol-name a)
                                            commands))))
                      (sort commands 'string-lessp))))
    (type . command)
    (requires-pattern . 2))
  "Source for completing and invoking Emacs commands.
A command is a function with interactive spec that can
be invoked with `M-x'.

To get non-interactive functions listed, use
`anything-c-source-emacs-functions'.")


;;;; <Function>
;;; Emacs functions
;;
;;
(defvar anything-c-source-emacs-functions
  '((name . "Emacs Functions")
    (candidates . (lambda ()
                    (let (commands)
                      (mapatoms (lambda (a)
                                  (if (functionp a)
                                      (push (symbol-name a) commands))))
                      (sort commands 'string-lessp))))
    (type . function)
    (requires-pattern . 2))
  "Source for completing Emacs functions.")

;;; With abbrev expansion
;;; Similar to my exec-abbrev-cmd.el
;;; See http://www.tsdh.de/cgi-bin/wiki.pl/exec-abbrev-cmd.el
(defvar anything-c-function-abbrev-regexp nil
  "The regexp for `anything-c-source-emacs-functions-with-abbrevs'.
Regexp built from the current `anything-pattern' interpreting it
as abbreviation.
Only for internal use.")

(defun anything-c-match-function-by-abbrev (candidate)
  "Return non-nil if `anything-pattern' is an abbreviation of the function CANDIDATE.

Abbreviations are made by taking the first character from each
word in the function's name, e.g. \"bb\" is an abbrev for
`bury-buffer', \"stb\" is an abbrev for `anything-c-switch-to-buffer'."
  (string-match anything-c-function-abbrev-regexp candidate))

(defvar anything-c-source-emacs-functions-with-abbrevs
  (append anything-c-source-emacs-functions
          '((match anything-c-match-function-by-abbrev
             anything-c-string-match))
          '((init
             . (lambda ()
                 (defadvice anything-update
                     (before anything-c-update-function-abbrev-regexp activate)
                   (let ((char-list (append anything-pattern nil))
                         (str "^"))
                     (dolist (c char-list)
                       (setq str (concat str (list c) "[^-]*-")))
                     (setq str (concat (substring str 0 (1- (length str))) "$"))
                     (setq anything-c-function-abbrev-regexp str))))))))

(defvar anything-c-source-advice
  '((name . "Function Advice")
    (candidates . anything-c-advice-candidates)
    (action ("Toggle Enable/Disable" . anything-c-advice-toggle))
    (persistent-action . anything-c-advice-persistent-action)
    (multiline)
    (persistent-help . "Describe function / C-u C-z: Toggle advice")))
;; (let ((debug-on-signal t))(anything 'anything-c-source-advice))
;; (testadvice)

(defun anything-c-advice-candidates ()
  (require 'advice)
  (loop for (fname) in ad-advised-functions
        for function = (intern fname)
        append
        (loop for class in ad-advice-classes append
              (loop for advice in (ad-get-advice-info-field function class)
                    for enabled = (ad-advice-enabled advice)
                    collect
                    (cons (format
                           "%s %s %s"
                           (if enabled "Enabled " "Disabled")
                           (propertize fname 'face 'font-lock-function-name-face)
                           (ad-make-single-advice-docstring advice class nil))
                          (list function class advice))))))

(defun anything-c-advice-persistent-action (func-class-advice)
  (if current-prefix-arg
      (anything-c-advice-toggle func-class-advice)
      (describe-function (car func-class-advice))))

(defun anything-c-advice-toggle (func-class-advice)
  (destructuring-bind (function class advice) func-class-advice
    (cond ((ad-advice-enabled advice)
           (ad-advice-set-enabled advice nil)
           (message "Disabled"))
          (t                            ;disabled
           (ad-advice-set-enabled advice t)
           (message "Enabled")))
    (ad-activate function)
    (and anything-in-persistent-action
         (anything-c-advice-update-current-display-string))))

(defun anything-c-advice-update-current-display-string ()
  (anything-edit-current-selection
    (let ((newword (cond ((looking-at "Disabled") "Enabled")
                         ((looking-at "Enabled")  "Disabled")))
          realvalue)
      (when newword
        (delete-region (point) (progn (forward-word 1) (point)))
        (insert newword)))))


;;;; <Variable>
;;; Emacs variables
;;
;;
(defvar anything-c-source-emacs-variables
  '((name . "Emacs Variables")
    (candidates . (lambda ()
                    (sort (all-completions "" obarray 'boundp) 'string-lessp)))
    (type . variable)
    (requires-pattern . 2))
  "Source for completing Emacs variables.")


;;; LaCarte
(defvar anything-c-source-lacarte
  '((name . "Lacarte")
    (init . (lambda () (require 'lacarte )))
    (candidates . (lambda () (delete '(nil) (lacarte-get-overall-menu-item-alist))))
    (candidate-number-limit . 9999)
    (action . anything-c-call-interactively))
  "Needs lacarte.el.

http://www.emacswiki.org/cgi-bin/wiki/download/lacarte.el")


;;; Bookmarks
;;
;;
;; Bind some faces for bookmarks.
(defvar anything-c-bookmarks-face1 'anything-ff-directory)
(defvar anything-c-bookmarks-face2 'anything-ff-file)
(defvar anything-c-bookmarks-face3 'anything-bookmarks-su-face)

(eval-when-compile (require 'bookmark))
(defvar anything-c-source-bookmarks
  `((name . "Bookmarks")
    (init . (lambda ()
              (require 'bookmark)))
    (candidates . bookmark-all-names)
    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")

;;; bookmark-set
(defvar anything-c-source-bookmark-set
  '((name . "Set Bookmark")
    (dummy)
    (action . bookmark-set))
  "See (info \"(emacs)Bookmarks\").")

;;; Visible Bookmarks
;; (install-elisp "http://cvs.savannah.gnu.org/viewvc/*checkout*/bm/bm/bm.el")


;; http://d.hatena.ne.jp/grandVin/20080911/1221114327
(defvar anything-c-source-bm
  '((name . "Visible Bookmarks")
    (init . anything-c-bm-init)
    (candidates-in-buffer)
    (type . line))
  "Needs bm.el.

http://www.nongnu.org/bm/")

(defun anything-c-bm-init ()
  "Init function for `anything-c-source-bm'."
  (when (require 'bm nil t)
    (with-no-warnings
      (let ((bookmarks (bm-lists))
            (buf (anything-candidate-buffer 'global)))
        (dolist (bm (sort* (append (car bookmarks) (cdr bookmarks))
                           '< :key 'overlay-start))
          (let ((start (overlay-start bm))
                (end (overlay-end bm))
                (annotation (or (overlay-get bm 'annotation) "")))
            (unless (< (- end start) 1) ; org => (if (< (- end start) 2)
              (let ((str (format "%5d: [%s]: %s\n"
                                 (line-number-at-pos start)
                                 annotation
                                 (buffer-substring start (1- end)))))
                (with-current-buffer buf (insert str))))))))))

;;; Special bookmarks
(defvar anything-c-source-bookmarks-ssh
  '((name . "Bookmarks-ssh")
    (init . (lambda ()
              (require 'bookmark)))
    (candidates . (lambda () (anything-c-collect-bookmarks :ssh t)))
    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")

(defvar anything-c-source-bookmarks-su
  '((name . "Bookmarks-root")
    (init . (lambda ()
              (require 'bookmark)))
    (candidates . (lambda () (anything-c-collect-bookmarks :su t)))
    (filtered-candidate-transformer anything-c-highlight-bookmark-su)

    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")

(defvar anything-c-source-bookmarks-local
  '((name . "Bookmarks-Local")
    (init . (lambda ()
              (require 'bookmark)))
    (candidates . (lambda () (anything-c-collect-bookmarks :local t)))
    (filtered-candidate-transformer
     anything-c-adaptive-sort
     anything-c-highlight-bookmark)
    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")

(defun* anything-c-collect-bookmarks (&key local su sudo ssh)
  (let* ((lis-all (bookmark-all-names))
         (lis-loc (cond (local (loop for i in lis-all
                                     unless (string-match "^(ssh)\\|^(su)" i)
                                     collect i))
                        (su (loop for i in lis-all
                                  when (string-match "^(su)" i)
                                  collect i))
                        (sudo (loop for i in lis-all
                                    when (string-match "^(sudo)" i)
                                    collect i))
                        (ssh (loop for i in lis-all
                                   when (string-match "^(ssh)" i)
                                   collect i)))))
    (sort lis-loc 'string-lessp)))

(defun anything-c-bookmark-root-logged-p ()
  (catch 'break
    (dolist (i (mapcar #'buffer-name (buffer-list)))
      (when (string-match (format "*tramp/%s ." anything-su-or-sudo) i)
        (throw 'break t)))))

(defun anything-c-highlight-bookmark-su (files source)
  (if (anything-c-bookmark-root-logged-p)
      (anything-c-highlight-bookmark files source)
      (anything-c-highlight-not-logged files source)))

(defun anything-c-highlight-not-logged (files source)
  (loop for i in files
        collect (propertize i 'face anything-c-bookmarks-face3)))

(defun anything-c-highlight-bookmark (bookmarks source)
  "Used as `candidate-transformer' to colorize bookmarks.
Work both with standard Emacs bookmarks and bookmark-extensions.el."
  (loop for i in bookmarks
        for isfile        = (bookmark-get-filename i)
        for bufp          = (and (fboundp 'bmkext-get-buffer-name)
                                 (bmkext-get-buffer-name i))
        for handlerp      = (and (fboundp 'bookmark-get-handler)
                                 (bookmark-get-handler i))
        for isw3m         = (and (fboundp 'bmkext-w3m-bookmark-p)
                                 (bmkext-w3m-bookmark-p i))
        for isgnus        = (and (fboundp 'bmkext-gnus-bookmark-p)
                                 (bmkext-gnus-bookmark-p i))
        for isman         = (and (fboundp 'bmkext-man-bookmark-p) ; Man
                                 (bmkext-man-bookmark-p i))
        for iswoman       = (and (fboundp 'bmkext-woman-bookmark-p) ; Woman
                                 (bmkext-woman-bookmark-p i))
        for handlerp      = (bookmark-get-handler i)
        for isannotation  = (bookmark-get-annotation i)
        for isabook       = (string= (bookmark-prop-get i 'type) "addressbook")
        for isinfo        = (eq handlerp 'Info-bookmark-jump)
        ;; Add a * if bookmark have annotation
        if (and isannotation (not (string-equal isannotation "")))
        do (setq i (concat "*" i))
        collect (cond (;; info buffers
                       isinfo
                       (propertize i 'face 'anything-bmkext-info 'help-echo isfile))
                      (;; w3m buffers
                       isw3m
                       (propertize i 'face 'anything-bmkext-w3m 'help-echo isfile))
                      (;; gnus buffers
                       isgnus
                       (propertize i 'face 'anything-bmkext-gnus 'help-echo isfile))
                      (;; Man Woman
                       (or iswoman isman)
                       (propertize i 'face 'anything-bmkext-man 'help-echo isfile))
                      (;; Addressbook
                       isabook
                       (propertize i 'face '((:foreground "Tomato"))))
                      (;; directories
                       (and isfile (file-directory-p isfile))
                       (propertize i 'face anything-c-bookmarks-face1 'help-echo isfile))
                      (;; regular files
                       t
                       (propertize i 'face 'anything-bmkext-file 'help-echo isfile)))))

(defun anything-c-bookmark-jump (candidate)
  "Jump to bookmark from keyboard."
  (let ((current-prefix-arg anything-current-prefix-arg))
    (bookmark-jump candidate)))

;;;###autoload
(defun anything-c-bookmark-run-jump-other-window ()
  "Jump to bookmark from keyboard."
  (interactive)
  (anything-c-quit-and-execute-action 'bookmark-jump-other-window))

;;;###autoload
(defun anything-c-bookmark-run-delete ()
  "Delete bookmark from keyboard."
  (interactive)
  (when (y-or-n-p "Delete bookmark?")
    (anything-c-quit-and-execute-action 'anything-delete-marked-bookmarks)))


;;; Sources to filter bookmark-extensions bookmarks.
;;
;;
;; Dependency: http://mercurial.intuxication.org/hg/emacs-bookmark-extension
;; If you want to enable google-maps in addressbook you will need
;; Julien Danjou google-maps-el package available here:
;; http://julien.danjou.info/google-maps-el.html

(defun anything-c-bmkext-filter-setup-alist (fn &rest args)
  "Return a filtered `bookmark-alist' sorted alphabetically."
  (loop
        with alist = (if args
                         (apply #'(lambda (x) (funcall fn x)) args)
                         (funcall fn))
        for i in alist
        for b = (car i)
        collect b into sa
        finally return (sort sa 'string-lessp)))

;;;###autoload
(defun anything-c-bmkext-run-edit ()
  "Run `bmkext-edit-bookmark' from keyboard."
  (interactive)
  (anything-c-quit-and-execute-action 'bmkext-edit-bookmark))

;;; Addressbook.
;;
;;
(defvar anything-c-source-bmkext-addressbook
  '((name . "Bookmark Addressbook")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . anything-c-bmkext-addressbook-setup-alist)
    (persistent-action
     . (lambda (candidate)
         (let ((bmk (anything-bookmark-get-bookmark-from-name
                     candidate)))
           (bookmark--jump-via bmk 'pop-to-buffer))))
    (persistent-help . "Show contact - Prefix with C-u to append")
    (filtered-candidate-transformer
     anything-c-adaptive-sort
     anything-c-highlight-bookmark)
    (action . (("Show Contact(s)"
                . (lambda (candidate)
                    (let* ((contacts (anything-marked-candidates))
                           (current-prefix-arg (or anything-current-prefix-arg
                                                   (> (length contacts) 1))))
                      (bookmark-jump
                       (anything-bookmark-get-bookmark-from-name (car contacts)))
                      (anything-aif (cdr contacts)
                          (loop for bmk in it do
                                (bookmark-jump
                                 (anything-bookmark-get-bookmark-from-name bmk)))))))
               ("Send Mail"
                . (lambda (candidate)
                    (let* ((contacts (anything-marked-candidates))
                           (bmk      (anything-bookmark-get-bookmark-from-name
                                      (car contacts)))
                           (append   (message-buffers)))
                      (if append
                          (addressbook-set-mail-buffer1 bmk 'append)
                          (addressbook-set-mail-buffer1 bmk))
                      (setq contacts (cdr contacts))
                      (when contacts
                        (loop for bmk in contacts do
                              (addressbook-set-mail-buffer1 bmk 'append))))))
               ("Edit Bookmark"
                . (lambda (candidate)
                    (let ((bmk (anything-bookmark-get-bookmark-from-name
                                candidate)))
                      (addressbook-bookmark-edit
                       (assoc bmk bookmark-alist)))))
               ("Insert Email at point"
                . (lambda (candidate)
                    (let* ((bmk   (anything-bookmark-get-bookmark-from-name
                                   candidate))
                           (mlist (split-string
                                   (assoc-default
                                    'email (assoc bmk bookmark-alist))
                                   ", ")))
                      (insert
                       (if (> (length mlist) 1)
                           (anything-comp-read
                            "Insert Mail Address: " mlist :must-match t)
                           (car mlist))))))
               ("Show annotation"
                . (lambda (candidate)
                    (let ((bmk (anything-bookmark-get-bookmark-from-name
                                candidate)))
                      (bookmark-show-annotation bmk))))
               ("Edit annotation"
                . (lambda (candidate)
                    (let ((bmk (anything-bookmark-get-bookmark-from-name
                                candidate)))
                      (bookmark-edit-annotation bmk))))
               ("Show Google map"
                . (lambda (candidate)
                    (let* ((bmk (anything-bookmark-get-bookmark-from-name
                                 candidate))
                           (full-bmk (assoc bmk bookmark-alist)))
                      (addressbook-google-map full-bmk))))))))


(defun anything-c-bmkext-addressbook-setup-alist ()
  "Specialized filter function for bookmarks w3m."
  (anything-c-bmkext-filter-setup-alist 'bmkext-addressbook-alist-only))

;; W3m bookmarks from bookmark-extensions.
(defvar anything-c-source-bookmark-w3m
  '((name . "Bookmark W3m")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . anything-c-bookmark-w3m-setup-alist)
    (filtered-candidate-transformer
     anything-c-adaptive-sort
     anything-c-highlight-bookmark)
    (type . bookmark)))

(defun anything-c-bookmark-w3m-setup-alist ()
  "Specialized filter function for bookmarks w3m."
  (anything-c-bmkext-filter-setup-alist 'bmkext-w3m-alist-only))

;; Images
(defvar anything-c-source-bookmark-images
  '((name . "Bookmark Images")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . anything-c-bookmark-images-setup-alist)
    (filtered-candidate-transformer
     anything-c-adaptive-sort
     anything-c-highlight-bookmark)
    (type . bookmark)))

(defun anything-c-bookmark-images-setup-alist ()
  "Specialized filter function for images bookmarks."
  (anything-c-bmkext-filter-setup-alist 'bmkext-image-file-alist-only))

;; Woman Man
(defvar anything-c-source-bookmark-man
  '((name . "Bookmark Woman&Man")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . anything-c-bookmark-man-setup-alist)
    (filtered-candidate-transformer
     anything-c-adaptive-sort
     anything-c-highlight-bookmark)
    (type . bookmark)))

(defun anything-c-bookmark-man-setup-alist ()
  "Specialized filter function for bookmarks w3m."
  (append (anything-c-bmkext-filter-setup-alist 'bmkext-man-alist-only)
          (anything-c-bmkext-filter-setup-alist 'bmkext-woman-alist-only)))

;; Gnus
(defvar anything-c-source-bookmark-gnus
  '((name . "Bookmark Gnus")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . anything-c-bookmark-gnus-setup-alist)
    (filtered-candidate-transformer
     anything-c-adaptive-sort
     anything-c-highlight-bookmark)
    (type . bookmark)))

(defun anything-c-bookmark-gnus-setup-alist ()
  "Specialized filter function for bookmarks gnus."
  (anything-c-bmkext-filter-setup-alist 'bmkext-gnus-alist-only))

;; Info
(defvar anything-c-source-bookmark-info
  '((name . "Bookmark Info")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . anything-c-bookmark-info-setup-alist)
    (filtered-candidate-transformer
     anything-c-adaptive-sort
     anything-c-highlight-bookmark)
    (type . bookmark)))

(defun anything-c-bookmark-info-setup-alist ()
  "Specialized filter function for bookmarks info."
  (anything-c-bmkext-filter-setup-alist 'bmkext-info-alist-only))

;; Local Files&directories
(defvar anything-c-source-bookmark-files&dirs
  '((name . "Bookmark Files&Directories")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . anything-c-bookmark-local-files-setup-alist)
    (filtered-candidate-transformer
     anything-c-adaptive-sort
     anything-c-highlight-bookmark)
    (type . bookmark)))

(defun anything-c-bookmark-local-files-setup-alist ()
  "Specialized filter function for bookmarks locals files."
  (anything-c-bmkext-filter-setup-alist 'bmkext-local-file-alist-only))

;; Su Files&directories
(defvar anything-c-source-bookmark-su-files&dirs
  '((name . "Bookmark Root-Files&Directories")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . anything-c-bookmark-su-files-setup-alist)
    (filtered-candidate-transformer
     anything-c-adaptive-sort
     anything-c-highlight-bookmark-su)
    (type . bookmark)))

(defun anything-c-bookmark-su-files-setup-alist ()
  "Specialized filter function for bookmarks su/sudo files."
  (declare (special bmkext-su-or-sudo-regexp))
  (loop
        with l = (anything-c-bmkext-filter-setup-alist 'bmkext-remote-file-alist-only)
        for i in l
        for isfile = (bookmark-get-filename i)
        for istramp = (and isfile (boundp 'tramp-file-name-regexp)
                           (save-match-data
                             (string-match tramp-file-name-regexp isfile)))
        for issu = (and istramp
                        (string-match bmkext-su-or-sudo-regexp isfile))
        if issu
        collect i))

;; Ssh Files&directories
(defvar anything-c-source-bookmark-ssh-files&dirs
  '((name . "Bookmark Ssh-Files&Directories")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . anything-c-bookmark-ssh-files-setup-alist)
    (filtered-candidate-transformer . anything-c-adaptive-sort)
    (type . bookmark)))

(defun anything-c-bookmark-ssh-files-setup-alist ()
  "Specialized filter function for bookmarks ssh files."
  (loop
        with l = (anything-c-bmkext-filter-setup-alist 'bmkext-remote-file-alist-only)
        for i in l
        for isfile = (bookmark-get-filename i)
        for istramp = (and isfile (boundp 'tramp-file-name-regexp)
                           (save-match-data
                             (string-match tramp-file-name-regexp isfile)))
        for isssh = (and istramp
                         (string-match "/ssh:" isfile))
        if isssh
        collect i))



;;; Firefox bookmarks
;;
;;
;; You will have to set firefox to import bookmarks in his html file bookmarks.html.
;; (only for firefox versions >=3)
;; To achieve that, open about:config in firefox and double click on this line to enable value
;; to true:
;; user_pref("browser.bookmarks.autoExportHTML", false);
;; You should have now:
;; user_pref("browser.bookmarks.autoExportHTML", true);
;; NOTE: This is also working in the same way for mozilla aka seamonkey.

(defvar anything-firefox-bookmark-url-regexp "\\(https\\|http\\|ftp\\|about\\|file\\)://[^ \"]*")
(defvar anything-firefox-bookmarks-regexp ">\\([^><]+.[^</a>]\\)")

(defun anything-get-firefox-user-init-dir ()
  "Guess the default Firefox user directory name."
  (let* ((moz-dir (concat (getenv "HOME") "/.mozilla/firefox/"))
         (moz-user-dir
          (with-current-buffer (find-file-noselect (concat moz-dir "profiles.ini"))
            (goto-char (point-min))
            (prog1
                (when (search-forward "Path=" nil t)
                  (buffer-substring-no-properties (point) (point-at-eol)))
              (kill-buffer)))))
    (file-name-as-directory (concat moz-dir moz-user-dir))))

(defun anything-guess-firefox-bookmark-file ()
  "Return the path of the Firefox bookmarks file."
  (concat (anything-get-firefox-user-init-dir) "bookmarks.html"))

(defun anything-html-bookmarks-to-alist (file url-regexp bmk-regexp)
  "Parse html bookmark FILE and return an alist with (title . url) as elements."
  (let (bookmarks-alist url title)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "href=\\|^ *<DT><A HREF=" nil t)
        (forward-line 0)
        (when (re-search-forward url-regexp nil t)
          (setq url (match-string 0)))
        (when (re-search-forward bmk-regexp nil t)
          (setq title (match-string 1)))
        (push (cons title url) bookmarks-alist)
        (forward-line)))
    (nreverse bookmarks-alist)))

(defvar anything-c-firefox-bookmarks-alist nil)
(defvar anything-c-source-firefox-bookmarks
  '((name . "Firefox Bookmarks")
    (init . (lambda ()
              (setq anything-c-firefox-bookmarks-alist
                    (anything-html-bookmarks-to-alist
                     (anything-guess-firefox-bookmark-file)
                     anything-firefox-bookmark-url-regexp
                     anything-firefox-bookmarks-regexp))))
    (candidates . (lambda ()
                    (mapcar #'car anything-c-firefox-bookmarks-alist)))
    (filtered-candidate-transformer
     anything-c-adaptive-sort
     anything-c-highlight-firefox-bookmarks)
    (action . (("Browse Url"
                . (lambda (candidate)
                    (anything-c-browse-url
                     (anything-c-firefox-bookmarks-get-value candidate))))
               ("Copy Url"
                . (lambda (elm)
                    (kill-new (anything-c-w3m-bookmarks-get-value elm))))))))


(defun anything-c-firefox-bookmarks-get-value (elm)
  (assoc-default elm anything-c-firefox-bookmarks-alist))

(defun anything-c-highlight-firefox-bookmarks (bookmarks source)
  (loop for i in bookmarks
        collect (propertize
                 i 'face '((:foreground "YellowGreen"))
                 'help-echo (anything-c-firefox-bookmarks-get-value i))))



;;; W3m bookmark - anything interface.
;;
;;
;; Some users have the emacs-w3m library in load-path
;; without having the w3m executable :-;
;; So check if w3m program is present before trying to load
;; emacs-w3m.
(eval-when-compile
  (when (executable-find "w3m")
    (require 'w3m-bookmark nil t)))

(defvar w3m-bookmark-file "~/.w3m/bookmark.html")
(defvar anything-w3m-bookmarks-regexp ">\\([^><]+.[^</a>]\\)")
(defvar anything-w3m-bookmark-url-regexp "\\(https\\|http\\|ftp\\|file\\)://[^>]*")
(defvar anything-c-w3m-bookmarks-alist nil)
(defvar anything-c-source-w3m-bookmarks
  '((name . "W3m Bookmarks")
    (init . (lambda ()
              (setq anything-c-w3m-bookmarks-alist
                    (anything-html-bookmarks-to-alist
                     w3m-bookmark-file
                     anything-w3m-bookmark-url-regexp
                     anything-w3m-bookmarks-regexp))))
    (candidates . (lambda ()
                    (mapcar #'car anything-c-w3m-bookmarks-alist)))
    (filtered-candidate-transformer
     anything-c-adaptive-sort
     anything-c-highlight-w3m-bookmarks)
    (action . (("Browse Url"
                . (lambda (candidate)
                    (anything-c-w3m-browse-bookmark candidate)))
               ("Copy Url"
                . (lambda (elm)
                    (kill-new (anything-c-w3m-bookmarks-get-value elm))))
               ("Browse Url Externally"
                . (lambda (candidate)
                    (anything-c-w3m-browse-bookmark candidate t)))
               ("Delete Bookmark"
                . (lambda (candidate)
                    (anything-c-w3m-delete-bookmark candidate)))
               ("Rename Bookmark"
                . (lambda (candidate)
                    (anything-c-w3m-rename-bookmark candidate)))))
    (persistent-action . (lambda (candidate)
                           (if current-prefix-arg
                               (anything-c-w3m-browse-bookmark candidate t)
                               (anything-c-w3m-browse-bookmark candidate nil t))))
    (persistent-help . "Open URL with emacs-w3m in new tab / \
C-u \\[anything-execute-persistent-action]: Open URL with Firefox"))
  "Needs w3m and emacs-w3m.

http://w3m.sourceforge.net/
http://emacs-w3m.namazu.org/")


(defun anything-c-w3m-bookmarks-get-value (elm)
  (replace-regexp-in-string
   "\"" "" (cdr (assoc elm anything-c-w3m-bookmarks-alist))))

(defun anything-c-w3m-browse-bookmark (elm &optional use-external new-tab)
  (let* ((fn  (if use-external 'anything-c-browse-url 'w3m-browse-url))
         (arg (and (eq fn 'w3m-browse-url) new-tab)))
    (funcall fn (anything-c-w3m-bookmarks-get-value elm) arg)))

(defun anything-c-highlight-w3m-bookmarks (bookmarks source)
  (loop for i in bookmarks
        collect (propertize
                 i 'face 'anything-w3m-bookmarks-face
                 'help-echo (anything-c-w3m-bookmarks-get-value i))))


(defun anything-c-w3m-delete-bookmark (elm)
  "Delete w3m bookmark from `w3m-bookmark-file'."
  (with-current-buffer
      (find-file-literally w3m-bookmark-file)
    (goto-char (point-min))
    (when (re-search-forward elm nil t)
      (beginning-of-line)
      (delete-region (point)
                     (line-end-position))
      (delete-blank-lines))
    (save-buffer)
    (kill-buffer)))

(defun anything-c-w3m-rename-bookmark (elm)
  "Rename w3m bookmark in `w3m-bookmark-file'."
  (let* ((old-title (replace-regexp-in-string ">" "" elm))
         (new-title (read-string "NewTitle: " old-title)))
    (with-current-buffer
        (find-file-literally w3m-bookmark-file)
      (goto-char (point-min))
      (when (re-search-forward (concat elm "<") nil t)
        (goto-char (1- (point)))
        (delete-char (- (length old-title)))
        (insert new-title))
      (save-buffer)
      (kill-buffer))))


;;;; <Library>
;;; Elisp library scan
;;
;;
(defvar anything-c-source-elisp-library-scan
  '((name . "Elisp libraries (Scan)")
    (init . (anything-c-elisp-library-scan-init))
    (candidates-in-buffer)
    (action ("Find library"
             . (lambda (candidate) (find-file (find-library-name candidate))))
     ("Find library other window"
      . (lambda (candidate)
          (find-file-other-window (find-library-name candidate))))
     ("Load library"
      . (lambda (candidate) (load-library candidate))))))

(defun anything-c-elisp-library-scan-init ()
  "Init anything buffer status."
  (let ((anything-buffer (anything-candidate-buffer 'global))
        (library-list (anything-c-elisp-library-scan-list)))
    (with-current-buffer anything-buffer
      (dolist (library library-list)
        (insert (format "%s\n" library))))))

(defun anything-c-elisp-library-scan-list (&optional dirs string)
  "Do completion for file names passed to `locate-file'.
DIRS is directory to search path.
STRING is string to match."
  ;; Use `load-path' as path when ignore `dirs'.
  (or dirs (setq dirs load-path))
  ;; Init with blank when ignore `string'.
  (or string (setq string ""))
  ;; Get library list.
  (let ((string-dir (file-name-directory string))
        ;; File regexp that suffix match `load-file-rep-suffixes'.
        (match-regexp (format "^.*\\.el%s$" (regexp-opt load-file-rep-suffixes)))
        name
        names)
    (dolist (dir dirs)
      (unless dir
        (setq dir default-directory))
      (if string-dir
          (setq dir (expand-file-name string-dir dir)))
      (when (file-directory-p dir)
        (dolist (file (file-name-all-completions
                       (file-name-nondirectory string) dir))
          ;; Suffixes match `load-file-rep-suffixes'.
          (setq name (if string-dir (concat string-dir file) file))
          (if (string-match match-regexp name)
              (add-to-list 'names name)))))
    names))


;;;; <Programming>
;;
;;

;;; Imenu
;;
;;
(defvar anything-c-imenu-delimiter " / ")

(defvar anything-c-imenu-index-filter nil)
(make-variable-buffer-local 'anything-c-imenu-index-filter)

(defvar anything-c-cached-imenu-alist nil)
(make-variable-buffer-local 'anything-c-cached-imenu-alist)

(defvar anything-c-cached-imenu-candidates nil)
(make-variable-buffer-local 'anything-c-cached-imenu-candidates)

(defvar anything-c-cached-imenu-tick nil)
(make-variable-buffer-local 'anything-c-cached-imenu-tick)

(eval-when-compile (require 'imenu))
(setq imenu-auto-rescan t)

(defun anything-imenu-create-candidates (entry)
  "Create candidates with ENTRY."
  (if (listp (cdr entry))
      (mapcan
       (lambda (sub)
         (if (consp (cdr sub))
             (mapcar
              (lambda (subentry)
                (concat (car entry) anything-c-imenu-delimiter subentry))
              (anything-imenu-create-candidates sub))
             (list (concat (car entry) anything-c-imenu-delimiter (car sub)))))
       (cdr entry))
      (list entry)))

(defvar anything-c-source-imenu
  '((name . "Imenu")
    (init . (lambda () (require 'imenu)))
    (candidates . anything-c-imenu-candidates)
    (persistent-action . (lambda (elm)
                           (anything-c-imenu-default-action elm)
                           (unless (fboundp 'semantic-imenu-tag-overlay)
                             (anything-match-line-color-current-line))))
    (persistent-help . "Show this entry")
    (action . anything-c-imenu-default-action))
  "See (info \"(emacs)Imenu\")")


(defun anything-c-imenu-candidates ()
  (with-anything-current-buffer
    (let ((tick (buffer-modified-tick)))
      (if (eq anything-c-cached-imenu-tick tick)
          anything-c-cached-imenu-candidates
          (setq imenu--index-alist nil)
          (setq anything-c-cached-imenu-tick tick
                anything-c-cached-imenu-candidates
                (ignore-errors
                  (mapcan
                   'anything-imenu-create-candidates
                   (setq anything-c-cached-imenu-alist
                         (let ((index (imenu--make-index-alist)))
                           (if anything-c-imenu-index-filter
                               (funcall anything-c-imenu-index-filter index)
                               index))))))
          (setq anything-c-cached-imenu-candidates
                (mapcar #'(lambda (x)
                            (if (stringp x)
                                x
                                (car x)))
                        anything-c-cached-imenu-candidates))))))

(setq imenu-default-goto-function 'imenu-default-goto-function)
(defun anything-c-imenu-default-action (elm)
  "The default action for `anything-c-source-imenu'."
  (let ((path (split-string elm anything-c-imenu-delimiter))
        (alist anything-c-cached-imenu-alist))
    (dolist (elm path)
      (setq alist (assoc elm alist)))
    (imenu alist)))



;;; Ctags
;;
;;
(defvar anything-c-ctags-modes
  '( c-mode c++-mode awk-mode csharp-mode java-mode javascript-mode lua-mode
    makefile-mode pascal-mode perl-mode cperl-mode php-mode python-mode
    scheme-mode sh-mode slang-mode sql-mode tcl-mode ))

(defun anything-c-source-ctags-init ()
  (when (and buffer-file-name
             (memq major-mode anything-c-ctags-modes)
             (anything-current-buffer-is-modified))
    (with-current-buffer (anything-candidate-buffer 'local)
      (call-process-shell-command
       (if (string-match "\\.el\\.gz$" anything-buffer-file-name)
           (format "ctags -e -u -f- --language-force=lisp --fields=n =(zcat %s) "
                   anything-buffer-file-name)
           (format "ctags -e -u -f- --fields=n %s " anything-buffer-file-name))
       nil (current-buffer))
      (goto-char (point-min))
      (forward-line 2)
      (delete-region (point-min) (point))
      (loop while (and (not (eobp)) (search-forward "\001" (point-at-eol) t))
            for lineno-start = (point)
            for lineno = (buffer-substring
                          lineno-start
                          (1- (search-forward "," (point-at-eol) t)))
            do
            (beginning-of-line)
            (insert (format "%5s:" lineno))
            (search-forward "\177" (point-at-eol) t)
            (delete-region (1- (point)) (point-at-eol))
            (forward-line 1)))))

(defvar anything-c-source-ctags
  '((name . "Exuberant ctags")
    (init . anything-c-source-ctags-init)
    (candidates-in-buffer)
    (adjust)
    (type . line))
  "Needs Exuberant Ctags.

http://ctags.sourceforge.net/")


;;; Etags
;;
;;
;; anything-etags.el is deprecated, if this file is found,
;; warn user at compile time.
(eval-when-compile
  (when (locate-library "anything-etags.el")
    (display-warning
     '(anything-config)
     "You are using obsolete library `anything-etags.el' and should remove it."
     :warning)))

(defvar anything-c-etags-tag-file-dir nil
  "Etags file directory.")
(defvar anything-c-etags-mtime-alist nil
  "Store the last modification time of etags files here.")
(defvar anything-c-etags-cache (make-hash-table :test 'equal)
  "Cache content of etags files used here for faster access.")

(defun anything-c-etags-get-tag-file (&optional directory)
  "Return the path of etags file if found."
  ;; Get tag file from `default-directory' or upper directory.
  (let ((current-dir (anything-c-etags-find-tag-file-directory
                      (or directory default-directory))))
    ;; Return nil if not find tag file.
    (when current-dir
      ;; Set tag file directory.
      (setq anything-c-etags-tag-file-dir current-dir)
      (expand-file-name anything-c-etags-tag-file-name current-dir))))

(defun anything-c-etags-find-tag-file-directory (current-dir)
  "Try to find the directory containing tag file.
If not found in CURRENT-DIR search in upper directory."
  (flet ((file-exists? (dir)
           (let ((tag-path (expand-file-name
                            anything-c-etags-tag-file-name dir)))
             (and (stringp tag-path)
                  (file-regular-p tag-path)
                  (file-readable-p tag-path)))))
    (loop with count = 0
          until (file-exists? current-dir)
          ;; Return nil if outside the value of
          ;; `anything-c-etags-tag-file-search-limit'.
          if (= count anything-c-etags-tag-file-search-limit)
          do (return nil)
          ;; Or search upper directories.
          else
          do (incf count)
          (setq current-dir (expand-file-name (concat current-dir "../")))
          finally return current-dir)))

(defun anything-c-source-etags-header-name (x)
  "Create header name for this anything etags session."
  (concat "Etags in "
          (with-anything-current-buffer
            (anything-c-etags-get-tag-file))))

(defmacro anything-c-etags-create-buffer (file)
  "Create the `anything-buffer' based on contents of etags tag FILE."
  `(let* ((tag-fname ,file)
          max
          (split (with-current-buffer (find-file-noselect tag-fname)
                   (prog1
                       (split-string (buffer-string) "\n" 'omit-nulls)
                     (setq max (line-number-at-pos (point-max)))
                     (kill-buffer))))
          (progress-reporter (make-progress-reporter "Loading tag file..." 0 max)))
     (loop
           with fname
           with cand
           for i in split for count from 0
           for elm = (unless (string-match "^\x0c" i)
                       (anything-aif (string-match "\177" i)
                           (substring i 0 it)
                         i))
           do (cond ((and elm (string-match "^\\(.+\\),[0-9]+" elm))
                     (setq fname (match-string 1 elm)))
                    (elm (setq cand (concat fname ": " elm)))
                    (t (setq cand nil)))
           when cand do (progn
                          (insert (concat cand "\n"))
                          (progress-reporter-update progress-reporter count)))))

(defun anything-c-etags-init ()
  "Feed `anything-buffer' using `anything-c-etags-cache' or tag file.
If no entry in cache, create one."
  (let ((tagfile (anything-c-etags-get-tag-file)))
    (when tagfile
      (with-current-buffer (anything-candidate-buffer 'global)
        (anything-aif (gethash tagfile anything-c-etags-cache)
            ;; An entry is present in cache, insert it.
            (insert it)
          ;; No entry, create a new buffer using content of tag file (slower).
          (anything-c-etags-create-buffer tagfile)
          ;; Store content of buffer in cache.
          (puthash tagfile (buffer-string) anything-c-etags-cache)
          ;; Store or set the last modification of tag file.
          (anything-aif (assoc tagfile anything-c-etags-mtime-alist)
              ;; If an entry exists modify it.
              (setcdr it (anything-c-etags-mtime tagfile))
            ;; No entry create a new one.
            (add-to-list 'anything-c-etags-mtime-alist
                         (cons tagfile (anything-c-etags-mtime tagfile)))))))))

(defvar anything-c-source-etags-select
  '((name . "Etags")
    (header-name . anything-c-source-etags-header-name)
    (init . anything-c-etags-init)
    (candidates-in-buffer)
    (search . (anything-c-etags-search-fn))
    (mode-line . anything-etags-mode-line-string)
    (action . anything-c-etags-default-action)
    (persistent-action . (lambda (candidate)
                           (anything-c-etags-default-action candidate)
                           (anything-match-line-color-current-line))))
  "Anything source for Etags.")

(defun anything-c-etags-search-fn (pattern)
  "Search function for `anything-c-source-etags-select'."
  (re-search-forward
   (if anything-c-etags-use-regexp-search
       (format anything-c-etags-search-regexp pattern)
       pattern)
   nil t))

(defun anything-c-etags-default-action (candidate)
  "Anything default action to jump to an etags entry."
  (let* ((split (split-string candidate ": "))
         (fname (expand-file-name
                 (car split) anything-c-etags-tag-file-dir))
         (elm   (cadr split)))
    (find-file fname)
    (goto-char (point-min))
    (search-forward elm nil t)
    (goto-char (match-beginning 0))))

(defun anything-c-etags-mtime (file)
  "Last modification time of etags tag FILE."
  (cadr (nth 5 (file-attributes file))))

(defun anything-c-etags-file-modified-p (file)
  "Check if tag FILE have been modified in this session.
If FILE is nil return nil."
  (let ((last-modif (and file
                         (assoc-default file anything-c-etags-mtime-alist))))
    (and last-modif
         (/= last-modif (anything-c-etags-mtime file)))))



;;; Semantic
;;
;; 
(defvar anything-semantic-candidates nil)

(defun anything-semantic-construct-candidates (tags depth)
  (when (require 'semantic nil t)
    (apply
     'append
     (mapcar
      (lambda (tag)
        (if (listp tag)
            (let ((type (semantic-tag-type tag))
                  (class (semantic-tag-class tag)))
              (if (or (and (stringp type)
                           (or (string= type "class")
                               (string= type "namespace")))
                      (eq class 'function)
                      (eq class 'variable))
                  (cons (cons (concat (make-string (* depth 2) ?\s)
                                      (semantic-format-tag-summarize tag nil t))
                              tag)
                        (anything-semantic-construct-candidates
                         (semantic-tag-components tag) (1+ depth)))))))
      tags))))

(defun anything-semantic-default-action (candidate)
  (let ((tag (cdr (assoc candidate anything-semantic-candidates))))
    (semantic-go-to-tag tag)))

(defvar anything-c-source-semantic
  '((name . "Semantic Tags")
    (init . (lambda ()
              (setq anything-semantic-candidates
                    (ignore-errors (anything-semantic-construct-candidates
                                    (semantic-fetch-tags) 0)))))
    (candidates . (lambda ()
                    (if anything-semantic-candidates
                        (mapcar 'car anything-semantic-candidates))))
    (persistent-action . (lambda (elm)
                           (anything-semantic-default-action elm)
                           (anything-match-line-color-current-line)))
    (persistent-help . "Show this entry")
    (action . anything-semantic-default-action)
    "Needs semantic in CEDET.

http://cedet.sourceforge.net/semantic.shtml
http://cedet.sourceforge.net/"))



;;; Anything interface of `simple-call-tree.el'.
;;
;; <http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el>
;;
;; Function is called by
(defvar anything-c-source-simple-call-tree-functions-callers
  '((name . "Function is called by")
    (init . anything-c-simple-call-tree-functions-callers-init)
    (multiline)
    (candidates . anything-c-simple-call-tree-candidates)
    (persistent-action . anything-c-simple-call-tree-persistent-action)
    (persistent-help . "Show function definitions by rotation")
    (action ("Find definition selected by persistent-action" .
             anything-c-simple-call-tree-find-definition)))
  "Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el")

(defvar anything-c-simple-call-tree-tick nil)
(make-variable-buffer-local 'anything-c-simple-call-tree-tick)
(defun anything-c-simple-call-tree-analyze-maybe ()
  (unless (eq (buffer-chars-modified-tick) anything-c-simple-call-tree-tick)
    (simple-call-tree-analyze)
    (setq anything-c-simple-call-tree-tick (buffer-chars-modified-tick))))

(defun anything-c-simple-call-tree-init-base (function message)
  (require 'simple-call-tree)
  (with-no-warnings
    (when (anything-current-buffer-is-modified)
      (anything-c-simple-call-tree-analyze-maybe)
      (let ((list (funcall function simple-call-tree-alist)))
        (with-current-buffer (anything-candidate-buffer 'local)
          (dolist (entry list)
            (let ((funcs (concat "  " (mapconcat #'identity (cdr entry) "\n  "))))
              (insert (car entry) message
                      (if (string= funcs "  ")
                          "  no functions."
                          funcs)
                      "\n\n"))))))))

(defun anything-c-simple-call-tree-functions-callers-init ()
  (anything-c-simple-call-tree-init-base 'simple-call-tree-invert
                                         " is called by\n"))

(defun anything-c-simple-call-tree-candidates ()
  (with-current-buffer (anything-candidate-buffer)
    (split-string (buffer-string) "\n\n")))

(defvar anything-c-simple-call-tree-related-functions nil)
(defvar anything-c-simple-call-tree-function-index 0)
(defun anything-c-simple-call-tree-persistent-action (candidate)
  (unless (eq last-command 'anything-execute-persistent-action)
    (setq anything-c-simple-call-tree-related-functions
          (delete "no functions."
                  (split-string
                   (replace-regexp-in-string "  \\| is called by\\| calls "
                                             "" candidate)
                   "\n")))
    (setq anything-c-simple-call-tree-function-index -1))
  (incf anything-c-simple-call-tree-function-index)
  (anything-c-simple-call-tree-find-definition candidate))

(defun anything-c-simple-call-tree-find-definition (candidate)
  (find-function
   (intern
    (nth (mod anything-c-simple-call-tree-function-index
              (length anything-c-simple-call-tree-related-functions))
         anything-c-simple-call-tree-related-functions))))


;;; Function calls
(defvar anything-c-source-simple-call-tree-callers-functions
  '((name . "Function calls")
    (init . anything-c-simple-call-tree-callers-functions-init)
    (multiline)
    (candidates . anything-c-simple-call-tree-candidates)
    (persistent-action . anything-c-simple-call-tree-persistent-action)
    (persistent-help . "Show function definitions by rotation")
    (action ("Find definition selected by persistent-action" .
             anything-c-simple-call-tree-find-definition)))
  "Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el")

(defun anything-c-simple-call-tree-callers-functions-init ()
  (anything-c-simple-call-tree-init-base 'identity " calls \n"))




;;; Anything UI of auto-document.el
;;
;; <http://www.emacswiki.org/cgi-bin/wiki/download/auto-document.el>
;;
;; Commands/Options with doc
(defvar anything-c-auto-document-data nil)
(make-variable-buffer-local 'anything-c-auto-document-data)
(defvar anything-c-source-commands-and-options-in-file
  '((name . "Commands/Options in file")
    (header-name
     . (lambda (x) (format "Commands/Options in %s"
                           (buffer-local-value 'buffer-file-name
                                               anything-current-buffer))))
    (candidates . anything-command-and-options-candidates)
    (multiline)
    (action . imenu))
  "List Commands and Options with doc. It needs auto-document.el .

http://www.emacswiki.org/cgi-bin/wiki/download/auto-document.el")

(eval-when-compile (require 'auto-document nil t))
(defun anything-command-and-options-candidates ()
  (with-anything-current-buffer
    (when (and (require 'auto-document nil t)
               (eq major-mode 'emacs-lisp-mode)
               (or (anything-current-buffer-is-modified)
                   (not anything-c-auto-document-data)))
      (or imenu--index-alist (imenu--make-index-alist t))
      (setq anything-c-auto-document-data
            (destructuring-bind (commands options)
                (adoc-construct anything-current-buffer)
              (append
               (loop for (command . doc) in commands
                     for cmdname = (symbol-name command)
                     collect
                     (cons
                      (format "Command: %s\n %s"
                              (propertize cmdname 'face font-lock-function-name-face)
                              (adoc-first-line doc))
                      (assoc cmdname imenu--index-alist)))
               (loop with var-alist = (cdr (assoc "Variables" imenu--index-alist))
                     for (option doc default) in options
                     for optname = (symbol-name option)
                     collect
                     (cons
                      (format "Option: %s\n %s\n default = %s"
                              (propertize optname 'face font-lock-variable-name-face)
                              (adoc-first-line doc)
                              (adoc-prin1-to-string default))
                      (assoc optname
                             var-alist)))))))
    anything-c-auto-document-data))



;;;; <Color and Face>
;;

;;; Customize Face
;;
;;
(defvar anything-c-source-customize-face
  '((name . "Customize Face")
    (init . (lambda ()
              (unless (anything-candidate-buffer)
                (save-selected-window
                  (list-faces-display))
                (anything-candidate-buffer (get-buffer "*Faces*")))))
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (action . (lambda (line)
                (customize-face (intern (car (split-string line))))))
    (requires-pattern . 3))
  "See (info \"(emacs)Faces\")")

;;; Colors browser
;;
;;
(defvar anything-c-source-colors
  '((name . "Colors")
    (init . (lambda () (unless (anything-candidate-buffer)
                         (save-selected-window
                           (list-colors-display))
                         (anything-candidate-buffer (get-buffer "*Colors*")))))
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (action
     ("Copy Name" . (lambda (candidate)
                      (kill-new (anything-c-colors-get-name candidate))))
     ("Copy RGB" . (lambda (candidate)
                     (kill-new (anything-c-colors-get-rgb candidate))))
     ("Insert Name" . (lambda (candidate)
                        (with-anything-current-buffer
                          (insert (anything-c-colors-get-name candidate)))))
     ("Insert RGB" . (lambda (candidate)
                       (with-anything-current-buffer
                         (insert (anything-c-colors-get-rgb candidate))))))))

(defun anything-c-colors-get-name (candidate)
  "Get color name."
  (replace-regexp-in-string
   " " ""
   (with-temp-buffer
     (insert (capitalize candidate))
     (goto-char (point-min))
     (search-forward-regexp "\\s-\\{2,\\}")
     (delete-region (point) (point-max))
     (buffer-string))))

(defun anything-c-colors-get-rgb (candidate)
  "Get color RGB."
  (replace-regexp-in-string
   " " ""
   (with-temp-buffer
     (insert (capitalize candidate))
     (goto-char (point-max))
     (search-backward-regexp "\\s-\\{2,\\}")
     (delete-region (point) (point-min))
     (buffer-string))))


;;;; <Search Engine>
;;; Tracker desktop search
(defvar anything-c-source-tracker-search
  '((name . "Tracker Search")
    (candidates . (lambda ()
                    (start-process "tracker-search-process" nil
                                   "tracker-search"
                                   anything-pattern)))
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files matching the current input pattern
with the tracker desktop search.")

;;; Spotlight (MacOS X desktop search)
(defvar anything-c-source-mac-spotlight
  '((name . "mdfind")
    (candidates
     . (lambda () (start-process "mdfind-process" nil "mdfind" anything-pattern)))
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files via Spotlight's command line
utility mdfind.")

;;; Picklist
(defvar anything-c-source-picklist
  '((name . "Picklist")
    (candidates . (lambda () (mapcar 'car picklist-list)))
    (type . file)))



;;; Kill ring
;;
;;
(defvar anything-c-source-kill-ring
  `((name . "Kill Ring")
    (init . (lambda () (anything-attrset 'last-command last-command)))
    (candidates . anything-c-kill-ring-candidates)
    (filtered-candidate-transformer anything-c-kill-ring-transformer)
    (action . anything-c-kill-ring-action)
    (keymap . ,anything-kill-ring-map)
    (last-command)
    (migemo)
    (multiline))
  "Source for browse and insert contents of kill-ring.")

(defun anything-c-kill-ring-candidates ()
  (loop for kill in (anything-fast-remove-dups kill-ring :test 'equal)
        unless (or (< (length kill) anything-kill-ring-threshold)
                   (string-match "^[\\s\\t]+$" kill))
        collect kill))

(defun anything-c-kill-ring-transformer (candidates source)
  "Display only the `anything-c-kill-ring-max-lines-number' lines of candidate."
  (loop for i in candidates
        for nlines = (with-temp-buffer (insert i) (count-lines (point-min) (point-max)))
        if (and anything-c-kill-ring-max-lines-number
                (> nlines anything-c-kill-ring-max-lines-number))
        collect (cons
                 (with-temp-buffer
                   (insert i)
                   (goto-char (point-min))
                   (concat
                    (buffer-substring
                     (point-min)
                     (save-excursion
                       (forward-line anything-c-kill-ring-max-lines-number)
                       (point)))
                    "[...]")) i)
        else collect i))

(defun anything-c-kill-ring-action (str)
  "Insert STR in `kill-ring' and set STR to the head.
If this action is executed just after `yank',
replace with STR as yanked string."
  (setq kill-ring (delete str kill-ring))
  (if (not (eq (anything-attr 'last-command) 'yank))
      (insert-for-yank str)
      ;; from `yank-pop'
      (let ((inhibit-read-only t)
            (before (< (point) (mark t))))
        (if before
            (funcall (or yank-undo-function 'delete-region) (point) (mark t))
            (funcall (or yank-undo-function 'delete-region) (mark t) (point)))
        (setq yank-undo-function nil)
        (set-marker (mark-marker) (point) (current-buffer))
        (insert-for-yank str)
        ;; Set the window start back where it was in the yank command,
        ;; if possible.
        (set-window-start (selected-window) yank-window-start t)
        (if before
            ;; This is like exchange-point-and-mark, but doesn't activate the mark.
            ;; It is cleaner to avoid activation, even though the command
            ;; loop would deactivate the mark because we inserted text.
            (goto-char (prog1 (mark t)
                         (set-marker (mark-marker) (point) (current-buffer)))))))
  (kill-new str))



;;;; <Mark ring>
;; DO NOT include these sources in `anything-sources' use
;; the commands `anything-mark-ring', `anything-global-mark-ring' or
;; `anything-all-mark-rings' instead.

(defun anything-c-source-mark-ring-candidates ()
  (flet ((get-marks (pos)
           (save-excursion
             (goto-char pos)
             (beginning-of-line)
             (let ((line  (car (split-string (thing-at-point 'line) "[\n\r]"))))
               (when (string= "" line)
                 (setq line  "<EMPTY LINE>"))
               (format "%7d: %s" (line-number-at-pos) line)))))
    (with-anything-current-buffer
      (loop
            with marks = (if (mark) (cons (mark-marker) mark-ring) mark-ring)
            with recip = nil
            for i in marks
            for m = (get-marks i)
            unless (member m recip)
            collect m into recip
            finally return recip))))

(defvar anything-mark-ring-cache nil)
(defvar anything-c-source-mark-ring
  '((name . "mark-ring")
    (init . (lambda ()
              (setq anything-mark-ring-cache
                    (ignore-errors (anything-c-source-mark-ring-candidates)))))
    (candidates . (lambda ()
                    (anything-aif anything-mark-ring-cache
                        it)))
    (action . (("Goto line"
                . (lambda (candidate)
                    (anything-goto-line (string-to-number candidate))))))
    (persistent-action . (lambda (candidate)
                           (anything-goto-line (string-to-number candidate))
                           (anything-match-line-color-current-line)))
    (persistent-help . "Show this line")))


;;; Global-mark-ring
(defvar anything-c-source-global-mark-ring
  '((name . "global-mark-ring")
    (candidates . anything-c-source-global-mark-ring-candidates)
    (action . (("Goto line"
                . (lambda (candidate)
                    (let ((items (split-string candidate ":")))
                      (anything-c-switch-to-buffer (second items))
                      (anything-goto-line (string-to-number (car items))))))))
    (persistent-action . (lambda (candidate)
                           (let ((items (split-string candidate ":")))
                             (anything-c-switch-to-buffer (second items))
                             (anything-goto-line (string-to-number (car items)))
                             (anything-match-line-color-current-line))))
    (persistent-help . "Show this line")))

(defun anything-c-source-global-mark-ring-candidates ()
  (flet ((buf-fn (m)
           (with-current-buffer (marker-buffer m)
             (goto-char m)
             (beginning-of-line)
             (let (line)
               (if (string= "" line)
                   (setq line  "<EMPTY LINE>")
                   (setq line (car (split-string (thing-at-point 'line)
                                                 "[\n\r]"))))
               (format "%7d:%s:    %s"
                       (line-number-at-pos) (marker-buffer m) line)))))
    (loop
          with marks = global-mark-ring
          with recip = nil
          for i in marks
          for gm = (unless (or (string-match
                                "^ " (format "%s" (marker-buffer i)))
                               (null (marker-buffer i)))
                     (buf-fn i))
          when (and gm (not (member gm recip)))
          collect gm into recip
          finally return recip)))



;;;; <Register>
;;; Insert from register
(defvar anything-c-source-register
  '((name . "Registers")
    (candidates . anything-c-register-candidates)
    (action-transformer . anything-c-register-action-transformer)
    (multiline)
    (action))
  "See (info \"(emacs)Registers\")")

(defun anything-c-register-candidates ()
  "Collecting register contents and appropriate commands."
  (loop for (char . val) in register-alist
        for key    = (single-key-description char)
        for string-actions =
        (cond
          ((numberp val)
           (list (int-to-string val)
                 'insert-register
                 'increment-register))
          ((markerp val)
           (let ((buf (marker-buffer val)))
             (if (null buf)
                 (list "a marker in no buffer")
                 (list (concat
                        "a buffer position:"
                        (buffer-name buf)
                        ", position "
                        (int-to-string (marker-position val)))
                       'jump-to-register
                       'insert-register))))
          ((and (consp val) (window-configuration-p (car val)))
           (list "window configuration."
                 'jump-to-register))
          ((and (consp val) (frame-configuration-p (car val)))
           (list "frame configuration."
                 'jump-to-register))
          ((and (consp val) (eq (car val) 'file))
           (list (concat "file:"
                         (prin1-to-string (cdr val))
                         ".")
                 'jump-to-register))
          ((and (consp val) (eq (car val) 'file-query))
           (list (concat "file:a file-query reference: file "
                         (car (cdr val))
                         ", position "
                         (int-to-string (car (cdr (cdr val))))
                         ".")
                 'jump-to-register))
          ((consp val)
           (let ((lines (format "%4d" (length val))))
             (list (format "%s: %s\n" lines
                           (truncate-string-to-width
                            (mapconcat 'identity (list (car val))
                                       "^J") (- (window-width) 15)))
                   'insert-register)))
          ((stringp val)
           (list
            ;; without properties
            (concat (substring-no-properties
                     val 0 (min (length val) anything-c-register-max-offset))
                    (if (> (length val) anything-c-register-max-offset)
                        "[...]" ""))
            'insert-register
            'append-to-register
            'prepend-to-register))
          ((vectorp val)
           (list
            "Undo-tree entry."
            'undo-tree-restore-state-from-register))
          (t
           "GARBAGE!"))
        collect (cons (format "register %3s: %s" key (car string-actions))
                      (cons char (cdr string-actions)))))

(defun anything-c-register-action-transformer (actions register-and-functions)
  "Decide actions by the contents of register."
  (loop with func-actions =
        '((insert-register
           "Insert Register" .
           (lambda (c) (insert-register (car c))))
          (jump-to-register
           "Jump to Register" .
           (lambda (c) (jump-to-register (car c))))
          (append-to-register
           "Append Region to Register" .
           (lambda (c) (append-to-register
                        (car c) (region-beginning) (region-end))))
          (prepend-to-register
           "Prepend Region to Register" .
           (lambda (c) (prepend-to-register
                        (car c) (region-beginning) (region-end))))
          (increment-register
           "Increment Prefix Arg to Register" .
           (lambda (c) (increment-register
                        anything-current-prefix-arg (car c))))
          (undo-tree-restore-state-from-register
           "Restore Undo-tree register"
           (lambda (c) (and (fboundp 'undo-tree-restore-state-from-register)
                            (undo-tree-restore-state-from-register (car c))))))
        for func in (cdr register-and-functions)
        for cell = (assq func func-actions)
        when cell
        collect (cdr cell)))



;;; Latex completion
(defun anything-c-latex-math-candidates ()
  "Collect candidates for latex math completion."
  (declare (special LaTeX-math-menu))
  (loop for i in (cddr LaTeX-math-menu)
        for elm = (loop for s in i when (vectorp s)
                        collect (cons (aref s 0) (aref s 1)))
        append elm))

(defvar anything-c-source-latex-math
  '((name . "Latex Math Menu")
    (init . (lambda ()
              (with-anything-current-buffer
                (LaTeX-math-mode 1))))
    (candidate-number-limit . 9999)
    (candidates . anything-c-latex-math-candidates)
    (action . (lambda (candidate)
                (call-interactively candidate)))))


;;;; <Headline Extraction>
(defvar anything-c-source-fixme
  '((name . "TODO/FIXME/DRY comments")
    (headline . "^.*\\<\\(TODO\\|FIXME\\|DRY\\)\\>.*$")
    (adjust)
    (recenter))
  "Show TODO/FIXME/DRY comments in current file.")

(defvar anything-c-source-rd-headline
  '((name . "RD HeadLine")
    (headline  "^= \\(.+\\)$" "^== \\(.+\\)$" "^=== \\(.+\\)$" "^==== \\(.+\\)$")
    (condition . (memq major-mode '(rdgrep-mode rd-mode)))
    (migemo)
    (subexp . 1))
  "Show RD headlines.

RD is Ruby's POD.
http://en.wikipedia.org/wiki/Ruby_Document_format")

(defvar anything-c-source-oddmuse-headline
  '((name . "Oddmuse HeadLine")
    (headline  "^= \\(.+\\) =$" "^== \\(.+\\) ==$"
     "^=== \\(.+\\) ===$" "^==== \\(.+\\) ====$")
    (condition . (memq major-mode '(oddmuse-mode yaoddmuse-mode)))
    (migemo)
    (subexp . 1))
  "Show Oddmuse headlines, such as EmacsWiki.")

(defvar anything-c-source-emacs-source-defun
  '((name . "Emacs Source DEFUN")
    (headline . "DEFUN\\|DEFVAR")
    (condition . (string-match "/emacs2[0-9].+/src/.+c$"
                  (or buffer-file-name ""))))
  "Show DEFUN/DEFVAR in Emacs C source file.")

(defvar anything-c-source-emacs-lisp-expectations
  '((name . "Emacs Lisp Expectations")
    (headline . "(desc[ ]\\|(expectations")
    (condition . (eq major-mode 'emacs-lisp-mode)))
  "Show descriptions (desc) in Emacs Lisp Expectations.

http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")

(defvar anything-c-source-emacs-lisp-toplevels
  '((name . "Emacs Lisp Toplevel / Level 4 Comment / Linkd Star")
    (headline . "^(\\|(@\\*\\|^;;;;")
    (get-line . buffer-substring)
    (condition . (eq major-mode 'emacs-lisp-mode))
    (adjust))
  "Show top-level forms, level 4 comments and linkd stars (optional) in Emacs Lisp.
linkd.el is optional because linkd stars are extracted by regexp.
http://www.emacswiki.org/cgi-bin/wiki/download/linkd.el")


;;; Anything yaoddmuse
;;
;; Be sure to have yaoddmuse.el installed
;; install-elisp may be required if you want to install elisp file from here.
(defvar anything-yaoddmuse-use-cache-file nil)
(defvar anything-c-yaoddmuse-cache-file "~/.emacs.d/yaoddmuse-cache.el")
(defvar anything-c-yaoddmuse-ew-cache nil)

(defun anything-yaoddmuse-get-candidates ()
  (declare (special yaoddmuse-pages-hash))
  (if anything-yaoddmuse-use-cache-file
      (ignore-errors
        (unless anything-c-yaoddmuse-ew-cache
          (load anything-c-yaoddmuse-cache-file)
          (setq anything-c-yaoddmuse-ew-cache
                (gethash "EmacsWiki" yaoddmuse-pages-hash)))
        anything-c-yaoddmuse-ew-cache)
      (yaoddmuse-update-pagename t)
      (gethash "EmacsWiki" yaoddmuse-pages-hash)))

(defvar anything-c-source-yaoddmuse-emacswiki-edit-or-view
  '((name . "Yaoddmuse Edit or View (EmacsWiki)")
    (candidates . anything-yaoddmuse-get-candidates)
    (action . (("Edit page" . (lambda (candidate)
                                (yaoddmuse-edit "EmacsWiki" candidate)))
               ("Browse page"
                . (lambda (candidate)
                    (yaoddmuse-browse-page "EmacsWiki" candidate)))
               ("Browse page other window"
                . (lambda (candidate)
                    (if (one-window-p)
                        (split-window-vertically))
                    (yaoddmuse-browse-page "EmacsWiki" candidate)))
               ("Browse diff"
                . (lambda (candidate)
                    (yaoddmuse-browse-page-diff "EmacsWiki" candidate)))
               ("Copy URL"
                . (lambda (candidate)
                    (kill-new (yaoddmuse-url "EmacsWiki" candidate))
                    (message "Have copy page %s's URL to yank." candidate)))
               ("Create page"
                . (lambda (candidate)
                    (yaoddmuse-edit "EmacsWiki" anything-input)))
               ("Update cache"
                . (lambda (candidate)
                    (if anything-yaoddmuse-use-cache-file
                        (progn
                          (anything-yaoddmuse-cache-pages t)
                          (setq anything-c-yaoddmuse-ew-cache
                                (gethash "EmacsWiki" yaoddmuse-pages-hash)))
                        (yaoddmuse-update-pagename))))))
    (action-transformer anything-c-yaoddmuse-action-transformer))
  "Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el")


(defvar anything-c-source-yaoddmuse-emacswiki-post-library
  '((name . "Yaoddmuse Post library (EmacsWiki)")
    (init . (anything-yaoddmuse-init))
    (candidates-in-buffer)
    (action . (("Post library and Browse"
                . (lambda (candidate)
                    (yaoddmuse-post-file
                     (find-library-name candidate)
                     "EmacsWiki"
                     (file-name-nondirectory (find-library-name candidate))
                     nil t)))
               ("Post library"
                . (lambda (candidate)
                    (yaoddmuse-post-file
                     (find-library-name candidate)
                     "EmacsWiki"
                     (file-name-nondirectory
                      (find-library-name candidate))))))))
  "Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el")


(defun anything-c-yaoddmuse-action-transformer (actions candidate)
  "Allow the use of `install-elisp' only on elisp files."
  (if (string-match "\.el$" candidate)
      (append actions '(("Install Elisp"
                         . (lambda (elm)
                             (install-elisp-from-emacswiki elm)))))
      actions))

;;;###autoload
(defun anything-yaoddmuse-cache-pages (&optional load)
  "Fetch the list of files on emacswiki and create cache file.
If load is non--nil load the file and feed `yaoddmuse-pages-hash'."
  (interactive)
  (declare (special yaoddmuse-pages-hash))
  (yaoddmuse-update-pagename)
  (save-excursion
    (find-file anything-c-yaoddmuse-cache-file)
    (erase-buffer)
    (insert "(puthash \"EmacsWiki\" '(")
    (loop for i in (gethash "EmacsWiki" yaoddmuse-pages-hash)
          do
          (insert (concat "(\"" (car i) "\") ")))
    (insert ") yaoddmuse-pages-hash)\n")
    (save-buffer)
    (kill-buffer (current-buffer))
    (when (or current-prefix-arg
              load)
      (load anything-c-yaoddmuse-cache-file))))

(defun anything-yaoddmuse-init ()
  "Init anything buffer status."
  (let ((anything-buffer (anything-candidate-buffer 'global))
        (library-list (yaoddmuse-get-library-list)))
    (with-current-buffer anything-buffer
      ;; Insert library name.
      (dolist (library library-list)
        (insert (format "%s\n" library)))
      ;; Sort lines.
      (sort-lines nil (point-min) (point-max)))))


;;; Eev anchors
(defvar anything-c-source-eev-anchor
  '((name . "Anchors")
    (candidates
     . (lambda ()
         (ignore-errors
           (with-anything-current-buffer
             (loop initially (goto-char (point-min))
                   while (re-search-forward
                          (format ee-anchor-format "\\([^\.].+\\)") nil t)
                   for anchor = (match-string-no-properties 1)
                   collect (cons (format "%5d:%s"
                                         (line-number-at-pos (match-beginning 0))
                                         (format ee-anchor-format anchor))
                                 anchor))))))
    (persistent-action . (lambda (item)
                           (ee-to item)
                           (anything-match-line-color-current-line)))
    (persistent-help . "Show this entry")
    (action . (("Goto link" . ee-to)))))


;;; Org headlines
;;
;;
(defvar anything-c-source-org-headline
  `((name . "Org HeadLine")
    (headline
     ,@(mapcar
        (lambda (num)
          (format "^\\*\\{%d\\} \\(.+?\\)\\([ \t]*:[a-zA-Z0-9_@:]+:\\)?[ \t]*$"
                  num))
        (number-sequence 1 8)))
    (condition . (eq major-mode 'org-mode))
    (migemo)
    (subexp . 1)
    (persistent-action . (lambda (elm)
                           (anything-c-action-line-goto elm)
                           (org-cycle)))
    (action-transformer
     . (lambda (actions candidate)
         '(("Go to Line" . anything-c-action-line-goto)
           ("Refile to this Headline" . anything-c-org-headline-refile)
           ("Insert Link to This Headline"
            . anything-c-org-headline-insert-link-to-headline)))))
  "Show Org headlines.
org-mode is very very much extended text-mode/outline-mode.

See (find-library \"org.el\")
See http://orgmode.org for the latest version.")

(defun anything-c-org-headline-insert-link-to-headline (lineno-and-content)
  (insert
   (save-excursion
     (anything-goto-line (car lineno-and-content))
     (and (looking-at org-complex-heading-regexp)
          (org-make-link-string (concat "*" (match-string 4)))))))

(defun anything-c-org-headline-refile (lineno-and-content)
  "Refile current org entry to LINENO-AND-CONTENT."
  (with-anything-current-buffer
    (org-cut-subtree)
    (anything-goto-line (car lineno-and-content))
    (org-end-of-subtree t t)
    (let ((org-yank-adjusted-subtrees t))
      (org-yank))))


;;; Org keywords
;;
;;
(defvar anything-c-source-org-keywords
  '((name . "Org Keywords")
    (init . anything-c-org-keywords-init)
    (candidates . anything-c-org-keywords-candidates)
    (action . anything-c-org-keywords-insert)
    (persistent-action . anything-c-org-keywords-show-help)
    (persistent-help . "Show an example and info page to describe this keyword.")
    (keywords-examples)
    (keywords)))

(defvar anything-c-org-keywords-info-location
  '(("#+TITLE:" . "(org)Export options")
    ("#+AUTHOR:" . "(org)Export options")
    ("#+DATE:" . "(org)Export options")
    ("#+EMAIL:" . "(org)Export options")
    ("#+DESCRIPTION:" . "(org)Export options")
    ("#+KEYWORDS:" . "(org)Export options")
    ("#+LANGUAGE:" . "(org)Export options")
    ("#+TEXT:" . "(org)Export options")
    ("#+TEXT:" . "(org)Export options")
    ("#+OPTIONS:" . "(org)Export options")
    ("#+BIND:" . "(org)Export options")
    ("#+LINK_UP:" . "(org)Export options")
    ("#+LINK_HOME:" . "(org)Export options")
    ("#+LATEX_HEADER:" . "(org)Export options")
    ("#+EXPORT_SELECT_TAGS:" . "(org)Export options")
    ("#+EXPORT_EXCLUDE_TAGS:" . "(org)Export options")
    ("#+INFOJS_OPT" . "(org)Javascript support")
    ("#+BEGIN_HTML" . "(org)Quoting HTML tags")
    ("#+BEGIN_LaTeX" . "(org)Quoting LaTeX code")
    ("#+ORGTBL" . "(org)Radio tables")
    ("#+HTML:" . "(org)Quoting HTML tags")
    ("#+LaTeX:" . "(org)Quoting LaTeX code")
    ("#+BEGIN:" . "(org)Dynamic blocks") ;clocktable columnview
    ("#+BEGIN_EXAMPLE" . "(org)Literal examples")
    ("#+BEGIN_QUOTE" . "(org)Paragraphs")
    ("#+BEGIN_VERSE" . "(org)Paragraphs")
    ("#+BEGIN_SRC" . "(org)Literal examples")
    ("#+CAPTION" . "(org)Tables in HTML export")
    ("#+LABEL" . "(org)Tables in LaTeX export")
    ("#+ATTR_HTML" . "(org)Links")
    ("#+ATTR_LaTeX" . "(org)Images in LaTeX export")))

(defun anything-c-org-keywords-init ()
  (unless (anything-attr 'keywords-examples)
    (require 'org)
    (anything-attrset 'keywords-examples
                      (append
                       (mapcar
                        (lambda (x)
                          (string-match "^#\\+\\(\\([A-Z_]+:?\\).*\\)" x)
                          (cons (match-string 2 x) (match-string 1 x)))
                        (org-split-string (org-get-current-options) "\n"))
                       (mapcar 'list org-additional-option-like-keywords)))
    (anything-attrset 'keywords (mapcar 'car (anything-attr 'keywords-examples)))))

(defun anything-c-org-keywords-candidates ()
  (and (or (eq (buffer-local-value 'major-mode anything-current-buffer) 'org-mode)
           (eq (buffer-local-value 'major-mode anything-current-buffer) 'message-mode))
       (anything-attr 'keywords)))

(defun anything-c-org-keywords-insert (keyword)
  (cond ((and (string-match "BEGIN" keyword)
              (anything-region-active-p))
         (let ((beg (region-beginning))
               (end (region-end)))
           (goto-char end)
           (insert "\n#+" (replace-regexp-in-string
                           "BEGIN" "END" keyword) "\n")
           (goto-char beg)
           (insert "#+" keyword " ")
           (save-excursion (insert "\n"))))
        ((string-match "BEGIN" keyword)
         (insert "#+" keyword " ")
         (save-excursion
           (insert "\n#+" (replace-regexp-in-string
                           "BEGIN" "END" keyword) "\n")))
        (t (insert "#+" keyword " "))))

(defun anything-c-org-keywords-show-help (keyword)
  (info (or (assoc-default (concat "#+" keyword) anything-c-org-keywords-info-location)
            "(org)In-buffer settings"))
  (search-forward (concat "#+" keyword) nil t)
  (anything-persistent-highlight-point)
  (message "%s" (or (cdr (assoc keyword (anything-attr 'keywords-examples))) "")))



;;; bbdb
;;
;;
(defvar bbdb-records)
(defvar bbdb-buffer-name)

(defun anything-c-bbdb-candidates ()
  "Return a list of all names in the bbdb database.  The format
is \"Firstname Lastname\"."
  (mapcar (lambda (bbdb-record)
            (replace-regexp-in-string
             "\\s-+$" ""
             (concat (aref bbdb-record 0) " " (aref bbdb-record 1))))
          (bbdb-records)))

(defun anything-c-bbdb-create-contact (actions candidate)
  "Action transformer that returns only an entry to add the
current `anything-pattern' as new contact.  All other actions are
removed."
  (if (string= candidate "*Add to contacts*")
      '(("Add to contacts" . (lambda (actions)
                               (bbdb-create-internal
                                (read-from-minibuffer "Name: " anything-c-bbdb-name)
                                (read-from-minibuffer "Company: ")
                                (read-from-minibuffer "Email: ")
                                nil
                                nil
                                (read-from-minibuffer "Note: ")))))
      actions))

(defun anything-c-bbdb-get-record (candidate)
  "Return record that match CANDIDATE."
  (bbdb candidate nil)
  (set-buffer "*BBDB*")
  (bbdb-current-record))

(defvar anything-c-bbdb-name nil
  "Only for internal use.")

(defvar anything-c-source-bbdb
  '((name . "BBDB")
    (candidates . anything-c-bbdb-candidates)
    (action ("Send a mail" . anything-c-bbdb-compose-mail)
     ("View person's data" . anything-c-bbdb-view-person-action))
    (filtered-candidate-transformer . (lambda (candidates source)
                                        (setq anything-c-bbdb-name anything-pattern)
                                        (if (not candidates)
                                            (list "*Add to contacts*")
                                            candidates)))
    (action-transformer . (lambda (actions candidate)
                            (anything-c-bbdb-create-contact actions candidate))))
  "Needs BBDB.

http://bbdb.sourceforge.net/")

(defun anything-c-bbdb-view-person-action (candidate)
  "View BBDB data of single CANDIDATE or marked candidates."
  (anything-aif (anything-marked-candidates)
      (let ((bbdb-append-records (length it)))
        (dolist (i it)
          (bbdb-redisplay-one-record (anything-c-bbdb-get-record i))))
    (bbdb-redisplay-one-record (anything-c-bbdb-get-record candidate))))

(defun anything-c-bbdb-collect-mail-addresses ()
  "Return a list of all mail addresses of records in bbdb buffer."
  (with-current-buffer bbdb-buffer-name
    (loop for i in bbdb-records
          if (bbdb-record-net (car i))
          collect (bbdb-dwim-net-address (car i)))))

(defun anything-c-bbdb-compose-mail (candidate)
  "Compose a mail with all records of bbdb buffer."
  (anything-c-bbdb-view-person-action candidate)
  (let* ((address-list (anything-c-bbdb-collect-mail-addresses))
         (address-str  (mapconcat 'identity address-list ",\n    ")))
    (compose-mail address-str)))


;;; Evaluation Result
;;
;;
;; Internal
(defvar anything-eldoc-active-minibuffers-list nil)
(defvar anything-eval-expression-input-history nil)

(defvar anything-c-source-evaluation-result
  '((name . "Evaluation Result")
    (disable-shortcuts)
    (dummy)
    (multiline)
    (mode-line . "C-RET: nl-and-indent, tab: reindent, C-tab:complete, C-p/n: next/prec-line.")
    (filtered-candidate-transformer . (lambda (candidates source)
                                        (list
                                         (condition-case nil
                                             (with-anything-current-buffer
                                               (pp-to-string
                                                (eval (read anything-pattern))))
                                           (error "Error")))))
    (action . (("Copy result to kill-ring" . (lambda (candidate)
                                               (with-current-buffer anything-buffer
                                                 (let ((end (save-excursion
                                                              (goto-char (point-max))
                                                              (search-backward "\n")
                                                              (point))))
                                                   (kill-region (point) end)))))
               ("copy sexp to kill-ring" . (lambda (candidate)
                                             (kill-new anything-input)))))))

(defun anything-eval-new-line-and-indent ()
  (interactive)
  (newline) (lisp-indent-line))

(defun anything-eldoc-store-minibuffer ()
  "Store minibuffer buffer name in `anything-eldoc-active-minibuffers-list'."
  (with-selected-window (minibuffer-window)
    (push (buffer-name) anything-eldoc-active-minibuffers-list)))

(defun anything-eldoc-show-in-eval ()
  "Return eldoc in mode-line for current minibuffer input."
  (let ((buf (with-selected-window (minibuffer-window)
               (buffer-name))))
    (when (member buf anything-eldoc-active-minibuffers-list)  
      (let* ((str-all (with-current-buffer buf
                        (minibuffer-completion-contents)))
             (sym     (when str-all
                        (with-temp-buffer
                          (insert str-all)
                          (goto-char (point-max))
                          (unless (looking-back ")\\|\"") (forward-char -1))
                          (eldoc-current-symbol))))
             (info-fn (eldoc-fnsym-in-current-sexp))
             (doc     (or (eldoc-get-var-docstring sym)
                          (eldoc-get-fnsym-args-string
                           (car info-fn) (cadr info-fn)))))
        (when doc (funcall anything-c-eldoc-in-minibuffer-show-fn doc))))))

(defun anything-c-show-info-in-mode-line (str)
  "Display string STR in mode-line."
  (save-selected-window
    (with-current-buffer anything-buffer
      (let ((mode-line-format (concat " " str)))
        (force-mode-line-update)
        (sit-for anything-c-show-info-in-mode-line-delay))
      (force-mode-line-update))))

;;; Calculation Result
;;
;;
(defvar anything-c-source-calculation-result
  '((name . "Calculation Result")
    (dummy)
    (filtered-candidate-transformer . (lambda (candidates source)
                                        (list
                                         (condition-case nil
                                             (calc-eval anything-pattern)
                                           (error "error")))))
    (action ("Copy result to kill-ring" . kill-new))))


;;; Google Suggestions
;;
;;
;; Internal
(defvar anything-ggs-max-length-real-flag 0)
(defvar anything-ggs-max-length-num-flag 0)

(defun anything-c-google-suggest-fetch (input)
  "Fetch suggestions for INPUT from XML buffer.
Return an alist with elements like (data . number_results)."
  (setq anything-ggs-max-length-real-flag 0
        anything-ggs-max-length-num-flag 0)
  (let ((request (concat anything-c-google-suggest-url
                         (url-hexify-string input))))
    (flet ((fetch ()
             (loop
                   with result-alist = (xml-get-children
                                        (car (xml-parse-region
                                              (point-min) (point-max)))
                                        'CompleteSuggestion)
                   for i in result-alist
                   for data = (cdr (caadr (assoc 'suggestion i)))
                   for nqueries = (cdr (caadr (assoc 'num_queries i)))
                   for lqueries = (length (anything-c-ggs-set-number-result
                                           nqueries))
                   for ldata = (length data)
                   do
                   (progn
                     (when (> ldata anything-ggs-max-length-real-flag)
                       (setq anything-ggs-max-length-real-flag ldata))
                     (when (> lqueries anything-ggs-max-length-num-flag)
                       (setq anything-ggs-max-length-num-flag lqueries)))
                   collect (cons data nqueries) into cont
                   finally return cont)))
      (if anything-google-suggest-use-curl-p
          (with-temp-buffer
            (call-process "curl" nil t nil request)
            (fetch))
          (with-current-buffer
              (url-retrieve-synchronously request)
            (fetch))))))

(defun anything-c-google-suggest-set-candidates (&optional request-prefix)
  "Set candidates with result and number of google results found."
  (let ((suggestions
         (loop with suggested-results = (anything-c-google-suggest-fetch
                                         (or (and request-prefix
                                                  (concat request-prefix
                                                          " " anything-pattern))
                                             anything-pattern))
               for (real . numresult) in suggested-results
               ;; Prepare number of results with ","
               for fnumresult = (anything-c-ggs-set-number-result numresult)
               ;; Calculate number of spaces to add before fnumresult
               ;; if it is smaller than longest result
               ;; `anything-ggs-max-length-num-flag'.
               ;; e.g 1,234,567
               ;;       345,678
               ;; To be sure it is aligned properly.
               for nspaces = (if (< (length fnumresult)
                                    anything-ggs-max-length-num-flag)
                                 (- anything-ggs-max-length-num-flag
                                    (length fnumresult))
                                 0)
               ;; Add now the spaces before fnumresult.
               for align-fnumresult = (concat (make-string nspaces ? )
                                              fnumresult)
               for interval = (- anything-ggs-max-length-real-flag
                                 (length real))
               for spaces   = (make-string (+ 2 interval) ? )
               for display = (format "%s%s(%s results)"
                                     real spaces align-fnumresult)
               collect (cons display real))))
    (if (loop for (disp . dat) in suggestions
              thereis (equal dat anything-pattern))
        suggestions
        ;; if there is no suggestion exactly matching the input then
        ;; prepend a Search on Google item to the list
        (append
         suggestions
         (list (cons (concat "Search for " "'" anything-input "'" " on Google")
                     anything-input))))))

(defun anything-c-ggs-set-number-result (num)
  (if num
      (progn
        (and (numberp num) (setq num (number-to-string num)))
        (loop for i in (reverse (split-string num "" t))
              for count from 1
              append (list i) into C
              when (= count 3)
              append (list ",") into C
              and do (setq count 0)
              finally return
              (replace-regexp-in-string
               "^," "" (mapconcat 'identity (reverse C) ""))))
      "?"))

(defvar anything-c-google-suggest-default-browser-function nil
  "*The browse url function you prefer to use with google suggest.
When nil, use the first browser function available
See `anything-browse-url-default-browser-alist'.")

(defun anything-c-google-suggest-action (candidate)
  "Default action to jump to a google suggested candidate."
  (let ((arg (concat anything-c-google-suggest-search-url
                     (url-hexify-string candidate))))
    (anything-aif anything-c-google-suggest-default-browser-function
        (funcall it arg)
      (anything-c-browse-url arg))))

(defvar anything-c-google-suggest-default-function
  'anything-c-google-suggest-set-candidates
  "Default function to use in anything google suggest.")

(defvar anything-c-source-google-suggest
  '((name . "Google Suggest")
    (candidates . (lambda ()
                    (funcall anything-c-google-suggest-default-function)))
    (action . (("Google Search" . anything-c-google-suggest-action)))
    (volatile)
    (requires-pattern . 3)
    (delayed)))

(defun anything-c-google-suggest-emacs-lisp ()
  "Try to emacs lisp complete with google suggestions."
  (anything-c-google-suggest-set-candidates "emacs lisp"))


;;; Yahoo suggestions
;;
;;
(defun anything-c-yahoo-suggest-fetch (input)
  "Fetch Yahoo suggestions for INPUT from XML buffer.
Return an alist with elements like (data . number_results)."
  (let ((request (concat anything-c-yahoo-suggest-url
                         (url-hexify-string input))))
    (flet ((fetch ()
             (loop
                   with result-alist = (xml-get-children
                                        (car (xml-parse-region
                                              (point-min) (point-max)))
                                        'Result)
                   for i in result-alist
                   collect (caddr i))))
      (with-current-buffer
          (url-retrieve-synchronously request)
        (fetch)))))

(defun anything-c-yahoo-suggest-set-candidates ()
  "Set candidates with Yahoo results found."
  (let ((suggestions (anything-c-yahoo-suggest-fetch anything-input)))
    (or suggestions
        (append
         suggestions
         (list (cons (concat "Search for " "'" anything-input "'" " on Yahoo")
                     anything-input))))))

(defun anything-c-yahoo-suggest-action (candidate)
  "Default action to jump to a Yahoo suggested candidate."
  (anything-c-browse-url (concat anything-c-yahoo-suggest-search-url
                                 (url-hexify-string candidate))))

(defvar anything-c-source-yahoo-suggest
  '((name . "Yahoo Suggest")
    (candidates . anything-c-yahoo-suggest-set-candidates)
    (action . (("Yahoo Search" . anything-c-yahoo-suggest-action)))
    (volatile)
    (requires-pattern . 3)
    (delayed)))



;;; Web browser functions.
;;
;;
(require 'browse-url)
;; If default setting of `w3m-command' is not
;; what you want you and you modify it, you will have to reeval
;; also `anything-browse-url-default-browser-alist'.
(defvar w3m-command "/usr/bin/w3m")
(defvar anything-c-home-url "http://www.google.fr"
  "*Default url to use as home url.")

(defvar ac-browse-url-chromium-program "chromium-browser")
(defvar ac-browse-url-uzbl-program "uzbl-browser")
(defvar anything-browse-url-default-browser-alist
  `((,w3m-command . w3m-browse-url)
    (,browse-url-firefox-program . browse-url-firefox)
    (,ac-browse-url-chromium-program . ac-browse-url-chromium)
    (,ac-browse-url-uzbl-program . ac-browse-url-uzbl)
    (,browse-url-kde-program . browse-url-kde)
    (,browse-url-gnome-moz-program . browse-url-gnome-moz)
    (,browse-url-mozilla-program . browse-url-mozilla)
    (,browse-url-galeon-program . browse-url-galeon)
    (,browse-url-netscape-program . browse-url-netscape)
    (,browse-url-mosaic-program . browse-url-mosaic)
    (,browse-url-xterm-program . browse-url-text-xterm))
  "*Alist of \(executable . function\) to try to find a suitable url browser.")

(defun* anything-c-generic-browser (url name &rest args)
  "Browse URL with NAME browser."
  (let ((proc (concat name " " url)))
    (message "Starting %s..." name)
    (apply 'start-process proc nil name
           (append args (list url)))
    (set-process-sentinel
     (get-process proc)
     #'(lambda (process event)
         (when (string= event "finished\n")
           (message "%s process %s" process event))))))

(defun ac-browse-url-chromium (url)
  "Browse URL with google chrome browser."
  (interactive "sURL: ")
  (anything-c-generic-browser
   url ac-browse-url-chromium-program))

(defun ac-browse-url-uzbl (url &optional ignore)
  "Browse URL with uzbl browser."
  (interactive "sURL: ")
  (anything-c-generic-browser url ac-browse-url-uzbl-program "-u"))

(defun anything-browse-url-default-browser (url &rest args)
  "Find the first available browser and ask it to load URL."
  (let ((default-browser-fn
         (loop for (exe . fn) in anything-browse-url-default-browser-alist
               thereis (and exe (executable-find exe) fn))))
    (if default-browser-fn
        (apply default-browser-fn url args)
        (error "No usable browser found"))))

(defun anything-c-browse-url (url &rest args)
  "Default command to browse URL."
  (if browse-url-browser-function
      (browse-url url args)
      (anything-browse-url-default-browser url args)))


;;; Surfraw
;;
;; Need external program surfraw.
;; <http://surfraw.alioth.debian.org/>

(defvar anything-surfraw-default-browser-function nil
  "*The browse url function you prefer to use with surfraw.
When nil, fallback to `browse-url-browser-function'.")

;; Internal
(defvar anything-surfraw-engines-history nil)
(defvar anything-surfraw-input-history nil)

(defun anything-c-build-elvi-list ()
  "Return list of all engines and descriptions handled by surfraw."
  (cdr
   (with-temp-buffer
     (call-process "surfraw" nil t nil
                   "-elvi")
     (split-string (buffer-string) "\n"))))


;;; Emms
;;
;;
(defun anything-emms-stream-edit-bookmark (elm)
  "Change the information of current emms-stream bookmark from anything."
  (declare (special emms-stream-list))
  (let* ((cur-buf anything-current-buffer)
         (bookmark (assoc elm emms-stream-list))
         (name     (read-from-minibuffer "Description: "
                                         (nth 0 bookmark)))
         (url      (read-from-minibuffer "URL: "
                                         (nth 1 bookmark)))
         (fd       (read-from-minibuffer "Feed Descriptor: "
                                         (int-to-string (nth 2 bookmark))))
         (type     (read-from-minibuffer "Type (url, streamlist, or lastfm): "
                                         (format "%s" (car (last bookmark))))))
    (save-window-excursion
      (emms-streams)
      (when (re-search-forward (concat "^" name) nil t)
        (beginning-of-line)
        (emms-stream-delete-bookmark)
        (emms-stream-add-bookmark name url (string-to-number fd) type)
        (emms-stream-save-bookmarks-file)
        (emms-stream-quit)
        (anything-c-switch-to-buffer cur-buf)))))

(defun anything-emms-stream-delete-bookmark (candidate)
  "Delete emms-streams bookmarks from anything."
  (let* ((cands   (anything-marked-candidates))
         (bmks    (loop for bm in cands collect
                        (car (assoc bm emms-stream-list))))
         (bmk-reg (mapconcat 'regexp-quote bmks "\\|^")))
    (when (y-or-n-p (format "Really delete radios\n -%s: ? "
                            (mapconcat 'identity bmks "\n -")))
      (save-window-excursion
        (emms-streams)
        (goto-char (point-min))
        (loop while (re-search-forward bmk-reg nil t)
              do (progn (beginning-of-line)
                        (emms-stream-delete-bookmark))
              finally do (progn
                           (emms-stream-save-bookmarks-file)
                           (emms-stream-quit)))))))

(defvar anything-c-source-emms-streams
  '((name . "Emms Streams")
    (init . (lambda ()
              (emms-stream-init)))
    (candidates . (lambda ()
                    (declare (special emms-stream-list))
                    (mapcar 'car emms-stream-list)))
    (action . (("Play" . (lambda (elm)
                           (declare (special emms-stream-list))
                           (let* ((stream (assoc elm emms-stream-list))
                                  (fn (intern (concat "emms-play-" (symbol-name (car (last stream))))))
                                  (url (second stream)))
                             (funcall fn url))))
               ("Delete" . anything-emms-stream-delete-bookmark)
               ("Edit" . anything-emms-stream-edit-bookmark)))
    (filtered-candidate-transformer . anything-c-adaptive-sort)))

;; Don't forget to set `emms-source-file-default-directory'
(defvar anything-c-source-emms-dired
  '((name . "Music Directory")
    (candidates . (lambda ()
                    (cddr (directory-files emms-source-file-default-directory))))
    (action .
     (("Play Directory" . (lambda (item)
                            (emms-play-directory
                             (expand-file-name
                              item
                              emms-source-file-default-directory))))
      ("Open dired in file's directory" . (lambda (item)
                                            (anything-c-open-dired
                                             (expand-file-name
                                              item
                                              emms-source-file-default-directory))))))
    (filtered-candidate-transformer . anything-c-adaptive-sort)))


(defun anything-c-emms-files-modifier (candidates source)
  (let ((current-playlist (with-current-emms-playlist
                              (loop with cur-list = (emms-playlist-tracks-in-region
                                                     (point-min) (point-max))
                                    for i in cur-list
                                    for name = (assoc-default 'name i)
                                    when name
                                    collect name))))
    (loop for i in candidates
          if (member (cdr i) current-playlist)
          collect (cons (propertize (car i)
                                    'face 'anything-emms-playlist)
                        (cdr i)) into lis
          else collect i into lis
          finally return (reverse lis))))

(defun anything-c-emms-play-current-playlist ()
  "Play current playlist."
  (with-current-emms-playlist
      (emms-playlist-first)
    (emms-playlist-mode-play-smart)))

(defvar anything-c-source-emms-files
  '((name . "Emms files")
    (candidates . (lambda ()
                    (loop for v being the hash-values in emms-cache-db
                          for name      = (assoc-default 'name v)
                          for artist    = (or (assoc-default 'info-artist v) "unknown")
                          for genre     = (or (assoc-default 'info-genre v) "unknown")
                          for tracknum  = (or (assoc-default 'info-tracknumber v) "unknown")
                          for song      = (or (assoc-default 'info-title v) "unknown")
                          for info      = (concat artist " - " genre " - " tracknum ": " song)
                          unless (string-match "^\\(http\\|mms\\):" name)
                          collect (cons info name))))
    (filtered-candidate-transformer . anything-c-emms-files-modifier)
    (candidate-number-limit . 9999)
    (action . (("Play file" . emms-play-file)
               ("Add to Playlist and play (C-u clear current)"
                . (lambda (candidate)
                    (when anything-current-prefix-arg
                      (emms-playlist-current-clear))
                    (emms-playlist-new)
                    (mapc 'emms-add-playlist-file (anything-marked-candidates))
                    (unless emms-player-playing-p
                      (anything-c-emms-play-current-playlist))))))))



;;; Jabber Contacts (jabber.el)
(defun anything-c-jabber-online-contacts ()
  "List online Jabber contacts."
  (with-no-warnings
    (let (jids)
      (dolist (item (jabber-concat-rosters) jids)
        (when (get item 'connected)
          (push (if (get item 'name)
                    (cons (get item 'name) item)
                    (cons (symbol-name item) item)) jids))))))

(defvar anything-c-source-jabber-contacts
  '((name . "Jabber Contacts")
    (init . (lambda () (require 'jabber)))
    (candidates . (lambda () (mapcar 'car (anything-c-jabber-online-contacts))))
    (action . (lambda (x)
                (jabber-chat-with
                 (jabber-read-account)
                 (symbol-name
                  (cdr (assoc x (anything-c-jabber-online-contacts)))))))))



;;; Call source.
(defvar anything-source-select-buffer "*anything source select*")
(defvar anything-c-source-call-source
  `((name . "Call anything source")
    (candidate-number-limit)
    (candidates
     . (lambda ()
         (loop for vname in (all-completions "anything-c-source-" obarray)
               for var = (intern vname)
               for name = (ignore-errors (assoc-default 'name (symbol-value var)))
               if name collect
               (cons (format "%s `%s'"
                             name (propertize vname 'face 'font-lock-variable-name-face))
                     var))))
    (action
     . (("Invoke anything with selected source"
         .
         (lambda (candidate)
           (setq anything-candidate-number-limit 9999)
           (anything candidate nil nil nil nil
                     anything-source-select-buffer)))
        ("Describe variable" . describe-variable)
        ("Find variable" . find-variable)))
    (persistent-action . describe-variable)
    (persistent-help . "Show description of this source")))

(defun anything-call-source-from-anything ()
  "Call anything source within `anything' session."
  (interactive)
  (setq anything-input-idle-delay 0)
  (anything-set-sources '(anything-c-source-call-source)))

;;; Execute Preconfigured anything.
(defvar anything-c-source-anything-commands
  '((name . "Preconfigured Anything")
    (candidates . anything-c-anything-commands-candidates)
    (type . command)
    (candidate-number-limit)))

(defun anything-c-anything-commands-candidates ()
  (loop for (cmd . desc) in (anything-c-list-preconfigured-anything)
        collect (cons (if (where-is-internal cmd nil t)
                          (substitute-command-keys (format "M-x %s (\\[%s]) : %s" cmd cmd desc))
                          (substitute-command-keys (format "\\[%s] : %s" cmd desc)))
                      cmd)))


;;; Occur
;;
;;
(defun anything-c-occur-init ()
  "Create the initial anything occur buffer.
If region is active use region as buffer contents
instead of whole buffer."
  (with-current-buffer (anything-candidate-buffer 'global)
    (erase-buffer)
    (let ((buf-contents
           (with-anything-current-buffer
             (if (anything-region-active-p)
                 (buffer-substring (region-beginning) (region-end))
                 (buffer-substring (point-min) (point-max))))))
      (insert buf-contents))))

(defun anything-c-occur-get-line (s e)
  (format "%7d:%s" (line-number-at-pos (1- s)) (buffer-substring s e)))

(defun anything-c-occur-query-replace-regexp (candidate)
  "Query replace regexp starting from CANDIDATE.
If region is active ignore CANDIDATE and replace only in region.
With a prefix arg replace only matches surrounded by word boundaries,
i.e Don't replace inside a word, regexp is surrounded with \\bregexp\\b."
  (let ((regexp anything-input))
    (unless (anything-region-active-p)
      (anything-c-action-line-goto candidate))
    (apply 'query-replace-regexp
           (anything-c-query-replace-args regexp))))

(defun anything-occur-run-query-replace-regexp ()
  "Run `query-replace-regexp' in anything occur from keymap."
  (interactive)
  (anything-c-quit-and-execute-action
   'anything-c-occur-query-replace-regexp))

(defvar anything-c-source-occur
  `((name . "Occur")
    (init . anything-c-occur-init)
    (candidates-in-buffer)
    (migemo)
    (get-line . anything-c-occur-get-line)
    (display-to-real . anything-c-display-to-real-line)
    (action . (("Go to Line" . anything-c-action-line-goto)
               ("Query replace regexp (C-u Not inside word.)"
                . anything-c-occur-query-replace-regexp)))
    (recenter)
    (mode-line . anything-occur-mode-line)
    (keymap . ,anything-occur-map)
    (requires-pattern . 1)
    (delayed)))


;;; Anything browse code.
(defun anything-c-browse-code-get-line (beg end)
  "Select line if it match the regexp corresponding to current `major-mode'.
Line is parsed for BEG position to END position."
  (let ((str-line (buffer-substring beg end))
        (regexp   (assoc-default major-mode
                                 anything-c-browse-code-regexp-alist))
        (num-line (if (string= anything-pattern "") beg (1- beg))))
    (when (and regexp (string-match regexp str-line))
      (format "%4d:%s" (line-number-at-pos num-line) str-line))))


(defvar anything-c-source-browse-code
  '((name . "Browse code")
    (init . (lambda ()
              (anything-candidate-buffer anything-current-buffer)
              (with-anything-current-buffer
                (jit-lock-fontify-now))))
    (candidate-number-limit . 9999)
    (candidates-in-buffer)
    (get-line . anything-c-browse-code-get-line)
    (type . line)
    (recenter)))


;; Do many actions for input
(defvar anything-c-source-create
  '((name . "Create")
    (dummy)
    (action)
    (action-transformer . anything-create--actions))
  "Do many create actions from `anything-pattern'.
See also `anything-create--actions'.")

(defun anything-create-from-anything ()
  "Run `anything-create' from `anything' as a fallback."
  (interactive)
  (anything-run-after-quit 'anything-create nil anything-pattern))

(defun anything-create--actions (&rest ignored)
  "Default actions for `anything-create' / `anything-c-source-create'."
  (remove-if-not
   (lambda (pair) (and (consp pair) (functionp (cdr pair))))
   (append anything-create--actions-private
           '(("find-file" . find-file)
             ("find-file other window" . find-file-other-window)
             ("New buffer" . anything-c-switch-to-buffer)
             ("New buffer other window" . switch-to-buffer-other-window)
             ("Bookmark Set" . bookmark-set)
             ("Set Register" .
              (lambda (x) (set-register (read-char "Register: ") x)))
             ("Insert Linkd star" . linkd-insert-star)
             ("Insert Linkd Tag" . linkd-insert-tag)
             ("Insert Linkd Link" . linkd-insert-link)
             ("Insert Linkd Lisp" . linkd-insert-lisp)
             ("Insert Linkd Wiki" . linkd-insert-wiki)
             ("Google Search" . google)))))


;; Minibuffer History
;;
;;
(defvar anything-c-source-minibuffer-history
  '((name . "Minibuffer History")
    (header-name . (lambda (name)
                     (format "%s (%s)" name minibuffer-history-variable)))
    (candidates
     . (lambda ()
         (let ((history (loop for i in
                              (symbol-value minibuffer-history-variable)
                              unless (string= "" i) collect i)))
           (if (consp (car history))
               (mapcar 'prin1-to-string history)
               history))))
    (migemo)
    (action . (lambda (candidate)
                (delete-minibuffer-contents)
                (insert candidate)))))


;;; Elscreen
;;
;;
(defvar anything-c-source-elscreen
  '((name . "Elscreen")
    (candidates
     . (lambda ()
         (if (cdr (elscreen-get-screen-to-name-alist))
             (sort
              (loop for sname in (elscreen-get-screen-to-name-alist)
                    append (list (format "[%d] %s" (car sname) (cdr sname))))
              #'(lambda (a b) (compare-strings a nil nil b nil nil))))))
    (action
     . (("Change Screen" .
                         (lambda (candidate)
                           (elscreen-goto (- (aref candidate 1) (aref "0" 0)))))
        ("Kill Screen(s)" .
                          (lambda (candidate)
                            (dolist (i (anything-marked-candidates))
                              (elscreen-goto (- (aref i 1) (aref "0" 0)))
                              (elscreen-kill))))
        ("Only Screen" .
                       (lambda (candidate)
                         (elscreen-goto (- (aref candidate 1) (aref "0" 0)))
                         (elscreen-kill-others)))))))


;;;; <System>

;;; Top (process)
(defvar anything-c-top-command "COLUMNS=%s top -b -n 1"
  "Top command (batch mode). %s is replaced with `frame-width'.")
(defvar anything-c-source-top
  '((name . "Top (Press C-c C-u to refresh)")
    (init . anything-c-top-init)
    (candidates-in-buffer)
    (display-to-real . anything-c-top-display-to-real)
    (persistent-action . anything-c-top-sh-persistent-action)
    (persistent-help . "SIGTERM")
    (action
     ("kill (TERM)" . (lambda (pid)
                        (anything-c-top-sh (format "kill -TERM %s" pid))))
     ("kill (KILL)" . (lambda (pid)
                        (anything-c-top-sh (format "kill -KILL %s" pid))))
     ("Copy PID" . (lambda (pid) (kill-new pid))))))

(defun anything-c-top-sh (cmd)
  (message "Executed %s\n%s" cmd (shell-command-to-string cmd)))

(defun anything-c-top-sh-persistent-action (pid)
  (delete-other-windows)
  (anything-c-top-sh (format "kill -TERM %s" pid))
  (anything-force-update))

(defun anything-c-top-init ()
  (with-current-buffer (anything-candidate-buffer 'global)
    (call-process-shell-command
     (format anything-c-top-command
             (- (frame-width) (if anything-enable-digit-shortcuts 4 0)))
     nil (current-buffer))))

(defun anything-c-top-display-to-real (line)
  (car (split-string line)))

;;; Timers
(defvar anything-c-source-absolute-time-timers
  '((name . "Absolute Time Timers")
    (candidates . timer-list)
    (type . timer)))

(defvar anything-c-source-idle-time-timers
  '((name . "Idle Time Timers")
    (candidates . timer-idle-list)
    (type . timer)))

(defun anything-c-timer-real-to-display (timer)
  (destructuring-bind (triggered t1 t2 t3 repeat-delay func args idle-delay)
      (append timer nil)                ;use `append' to convert vector->list
    (format "%s repeat=%5S %s(%s)"
            (let ((time (list t1 t2 t3)))
              (if idle-delay
                  (format-time-string "idle-for=%5s" time)
                  (format-time-string "%m/%d %T" time)))
            repeat-delay
            func
            (mapconcat 'prin1-to-string args " "))))

;;; X RandR resolution change
;;
;;
;;; FIXME I do not care multi-display.

(defun anything-c-xrandr-info ()
  "Return a pair with current X screen number and current X display name."
  (with-temp-buffer
    (call-process "xrandr" nil (current-buffer) nil
                  "--current")
    (let (screen output)
      (goto-char (point-min))
      (save-excursion
        (when (re-search-forward "\\(^Screen \\)\\([0-9]\\):" nil t)
          (setq screen (match-string 2))))
      (when (re-search-forward "^\\(.*\\) connected" nil t)
        (setq output (match-string 1)))
      (list screen output))))

(defun anything-c-xrandr-screen ()
  "Return current X screen number."
  (car (anything-c-xrandr-info)))

(defun anything-c-xrandr-output ()
  "Return current X display name."
  (cadr (anything-c-xrandr-info)))

(defvar anything-c-source-xrandr-change-resolution
  '((name . "Change Resolution")
    (candidates
     . (lambda ()
         (with-temp-buffer
           (call-process "xrandr" nil (current-buffer) nil
                         "--screen" (anything-c-xrandr-screen) "-q")
           (goto-char 1)
           (loop with modes = nil
                 while (re-search-forward "   \\([0-9]+x[0-9]+\\)" nil t)
                 for mode = (match-string 1)
                 unless (member mode modes)
                 collect mode into modes
                 finally return modes))))
    (action
     ("Change Resolution"
      . (lambda (mode)
          (call-process "xrandr" nil nil nil
                        "--screen" (anything-c-xrandr-screen)
                        "--output" (anything-c-xrandr-output)
                        "--mode" mode))))))

;;; Xfont selection
;;
;;
(defun anything-c-persistent-xfont-action (elm)
  "Show current font temporarily"
  (let ((current-font (cdr (assoc 'font (frame-parameters))))
        (default-font elm))
    (unwind-protect
         (progn (set-frame-font default-font 'keep-size) (sit-for 2))
      (set-frame-font current-font))))

(defvar anything-c-xfonts-cache nil)
(defvar anything-c-source-xfonts
  '((name . "X Fonts")
    (init . (lambda ()
              (unless anything-c-xfonts-cache
                (setq anything-c-xfonts-cache
                      (x-list-fonts "*")))))
    (candidates . anything-c-xfonts-cache)
    (action . (("Copy to kill ring" . (lambda (elm)
                                        (kill-new elm)))
               ("Set Font" . (lambda (elm)
                               (kill-new elm)
                               (set-frame-font elm 'keep-size)
                               (message "New font have been copied to kill ring")))))
    (persistent-action . anything-c-persistent-xfont-action)
    (persistent-help . "Switch to this font temporarily")))

;;; 𝕌𝕔𝕤 𝕊𝕪𝕞𝕓𝕠𝕝 𝕔𝕠𝕞𝕡𝕝𝕖𝕥𝕚𝕠𝕟
;;
;;
(defvar anything-c-ucs-max-len 0)
(defun anything-c-calculate-ucs-max-len ()
  "Calculate the length of longest `ucs-names' candidate."
  (loop with count = 0
        for (n . v) in (ucs-names)
        for len = (length n)
        if (> len count)
        do (setq count len)
        finally return count))

(defun anything-c-ucs-init ()
  "Initialize an anything buffer with ucs symbols.
Only math* symbols are collected."
  (unless (> anything-c-ucs-max-len 0)
    (setq anything-c-ucs-max-len
          (anything-c-calculate-ucs-max-len)))
  (with-current-buffer (anything-candidate-buffer
                        (get-buffer-create "*anything ucs*"))
    ;; `ucs-names' fn will not run again, data is cached in
    ;; var `ucs-names'.
    (loop for (n . v) in (ucs-names)
          for len = (length n)
          for diff = (+ (- anything-c-ucs-max-len len) 2)
          unless (string= "" n)
          do (progn (insert (concat
                             n ":"
                             (make-string
                              diff ? )))
                    (ucs-insert v)
                    (insert "\n")))))

(defun anything-c-ucs-forward-char (candidate)
  (with-anything-current-buffer
    (forward-char 1)))

(defun anything-c-ucs-backward-char (candidate)
  (with-anything-current-buffer
    (forward-char -1)))

(defun anything-c-ucs-delete-backward (candidate)
  (with-anything-current-buffer
    (delete-char -1)))

(defun anything-c-ucs-insert-char (candidate)
  (with-anything-current-buffer
    (insert
     (replace-regexp-in-string
      " " ""
      (cadr (split-string candidate ":"))))))

(defun anything-c-ucs-persistent-insert ()
  (interactive)
  (anything-attrset 'action-insert 'anything-c-ucs-insert-char)
  (anything-execute-persistent-action 'action-insert))

(defun anything-c-ucs-persistent-forward ()
  (interactive)
  (anything-attrset 'action-forward 'anything-c-ucs-forward-char)
  (anything-execute-persistent-action 'action-forward))

(defun anything-c-ucs-persistent-backward ()
  (interactive)
  (anything-attrset 'action-back 'anything-c-ucs-backward-char)
  (anything-execute-persistent-action 'action-back))

(defun anything-c-ucs-persistent-delete ()
  (interactive)
  (anything-attrset 'action-delete 'anything-c-ucs-delete-backward)
  (anything-execute-persistent-action 'action-delete))

(defvar anything-c-source-ucs
  '((name . "Ucs names")
    (init . anything-c-ucs-init)
    (candidate-number-limit . 9999)
    (candidates-in-buffer)
    (mode-line . anything-c-ucs-mode-line-string)
    (action . (("Insert" . anything-c-ucs-insert-char)
               ("Forward char" . anything-c-ucs-forward-char)
               ("Backward char" . anything-c-ucs-backward-char)
               ("Delete char backward" . anything-c-ucs-delete-backward))))
  "Source for collecting `ucs-names' math symbols.")


;;; Emacs process
;;
;;
(defvar anything-c-source-emacs-process
  '((name . "Emacs Process")
    (candidates . (lambda () (mapcar #'process-name (process-list))))
    (persistent-action . (lambda (elm)
                           (delete-process (get-process elm))
                           (anything-delete-current-selection)))
    (persistent-help . "Kill Process")
    (action ("Kill Process" . (lambda (elm)
                                (delete-process (get-process elm)))))))

;;; World time
;;
;;
(defvar anything-c-source-time-world
  '((name . "Time World List")
    (init . (lambda ()
              (let ((anything-buffer (anything-candidate-buffer 'global)))
                (with-current-buffer anything-buffer
                  (display-time-world-display display-time-world-list)))))
    (candidates-in-buffer)))



;;; Anything interface for Debian/Ubuntu packages (apt-*)
;;
;;
(defvar anything-c-source-apt
  '((name . "APT")
    (init . anything-c-apt-init)
    (candidates-in-buffer)
    (candidate-transformer anything-c-apt-candidate-transformer)
    (display-to-real . anything-c-apt-display-to-real)
    (requires-pattern . 2)
    (update . anything-c-apt-refresh)
    (action
     ("Show package description" . anything-c-apt-cache-show)
     ("Install package" . anything-c-apt-install)
     ("Reinstall package" . anything-c-apt-reinstall)
     ("Remove package" . anything-c-apt-uninstall)
     ("Purge package" . anything-c-apt-purge))
    (persistent-action . anything-c-apt-persistent-action)
    (persistent-help . "Show package description")))

(defvar anything-c-apt-query "emacs")
(defvar anything-c-apt-search-command "apt-cache search '%s'")
(defvar anything-c-apt-show-command "apt-cache show '%s'")
(defvar anything-c-apt-installed-packages nil)
(defvar anything-c-apt-all-packages nil)
(defvar anything-c-apt-input-history nil)

(defun anything-c-apt-refresh ()
  "Refresh installed candidates list."
  (setq anything-c-apt-installed-packages nil)
  (setq anything-c-apt-all-packages nil))

(defun anything-c-apt-persistent-action (candidate)
  "Persistent action for APT source."
  (anything-c-apt-cache-show candidate))

(defun anything-c-apt-candidate-transformer (candidates)
  "Show installed CANDIDATES and the ones to deinstall in a different color."
  (loop for cand in candidates
        for name = (anything-c-apt-display-to-real cand)
        collect (cond ((string= (assoc-default
                                 name anything-c-apt-installed-packages)
                                "deinstall")
                       (propertize cand 'face 'anything-apt-deinstalled))
                      ((string= (assoc-default
                                 name anything-c-apt-installed-packages)
                                "install")
                       (propertize cand 'face 'anything-apt-installed))
                      (t cand))))

(defun anything-c-apt-init ()
  "Initialize list of debian packages."
  (let ((query ""))
    (unless (and anything-c-apt-installed-packages
                 anything-c-apt-all-packages)
      (message "Loading package list...")
      (setq anything-c-apt-installed-packages
            (with-temp-buffer
              (call-process-shell-command "dpkg --get-selections"
                                          nil (current-buffer))
              (loop for i in (split-string (buffer-string) "\n" t)
                    for p = (split-string i)
                    collect (cons (car p) (cadr p)))))
      (setq anything-c-apt-all-packages
            (with-current-buffer
                (anything-candidate-buffer
                 (get-buffer-create (format "*anything-apt*")))
              (erase-buffer)
              (call-process-shell-command
               (format anything-c-apt-search-command query)
               nil (current-buffer))))
      (message "Loading package list done")
      (sit-for 0.5))))

(defun anything-c-apt-display-to-real (line)
  "Return only name of a debian package.
LINE is displayed like:
package name - description."
  (car (split-string line " - ")))

(defun anything-c-shell-command-if-needed (command)
  "Run shell command COMMAND to describe package.
If a buffer named COMMAND already exists, just switch to it."
  (let ((buf (get-buffer command)))
    (anything-c-switch-to-buffer (get-buffer-create command))
    (unless buf (insert (shell-command-to-string command)))))

(defun anything-c-apt-cache-show (package)
  "Show information on apt package PACKAGE."
  (anything-c-shell-command-if-needed
   (format anything-c-apt-show-command package)))

(defun anything-c-apt-install (package)
  "Run 'apt-get install' shell command on PACKAGE."
  (anything-c-apt-generic-action :action 'install))

(defun anything-c-apt-reinstall (package)
  "Run 'apt-get install --reinstall' shell command on PACKAGE."
  (anything-c-apt-generic-action :action 'reinstall))

(defun anything-c-apt-uninstall (package)
  "Run 'apt-get remove' shell command on PACKAGE."
  (anything-c-apt-generic-action :action 'uninstall))

(defun anything-c-apt-purge (package)
  "Run 'apt-get purge' shell command on PACKAGE."
  (anything-c-apt-generic-action :action 'purge))

(defun* anything-c-apt-generic-action (&key action)
  "Run 'apt-get ACTION'.
Support install, remove and purge actions."
  (ansi-term (getenv "SHELL") "anything apt")
  (term-line-mode)
  (let ((command   (case action
                     ('install   "sudo apt-get install ")
                     ('reinstall "sudo apt-get install --reinstall ")
                     ('uninstall "sudo apt-get remove ")
                     ('purge     "sudo apt-get purge ")
                     (t          (error "Unknow action"))))
        (beg       (point))
        end
        (cand-list (mapconcat #'(lambda (x) (format "'%s'" x))
                              (anything-marked-candidates) " ")))
    (goto-char (point-max))
    (insert (concat command cand-list))
    (setq end (point))
    (if (y-or-n-p (format "%s package" (symbol-name action)))
        (progn
          (setq anything-c-external-commands-list nil)
          (setq anything-c-apt-installed-packages nil)
          (term-char-mode) (term-send-input))
        (delete-region beg end) (term-send-eof) (kill-buffer))))

;; (anything-c-apt-install "jed")


;;; Anything UI for gentoo portage.
;;
;;
(defvar anything-c-gentoo-use-flags nil)
(defvar anything-c-gentoo-buffer "*anything-gentoo-output*")
(defvar anything-c-cache-gentoo nil)
(defvar anything-c-cache-world nil)
(defvar anything-c-source-gentoo
  '((name . "Portage sources")
    (init . (lambda ()
              (get-buffer-create anything-c-gentoo-buffer)
              (unless anything-c-cache-gentoo
                (anything-c-gentoo-setup-cache))
              (unless anything-c-cache-world
                (setq anything-c-cache-world (anything-c-gentoo-get-world)))
              (anything-c-gentoo-init-list)))
    (candidates-in-buffer)
    (match . identity)
    (candidate-transformer anything-c-highlight-world)
    (action . (("Show package" . (lambda (elm)
                                   (anything-c-gentoo-eshell-action elm "eix")))
               ("Show history" . (lambda (elm)
                                   (if (member elm anything-c-cache-world)
                                       (anything-c-gentoo-eshell-action elm "genlop -qe")
                                       (message "No infos on packages not yet installed"))))
               ("Copy in kill-ring" . kill-new)
               ("insert at point" . insert)
               ("Browse HomePage" . (lambda (elm)
                                      (let ((urls (anything-c-gentoo-get-url elm)))
                                        (browse-url (anything-comp-read "Url: " urls :must-match t)))))
               ("Show extra infos" . (lambda (elm)
                                       (if (member elm anything-c-cache-world)
                                           (anything-c-gentoo-eshell-action elm "genlop -qi")
                                           (message "No infos on packages not yet installed"))))
               ("Show use flags" . (lambda (elm)
                                     (anything-c-gentoo-default-action elm "equery" "-C" "u")
                                     (font-lock-add-keywords nil '(("^\+.*" . font-lock-variable-name-face)))
                                     (font-lock-mode 1)))
               ("Run emerge pretend" . (lambda (elm)
                                         (anything-c-gentoo-eshell-action elm "emerge -p")))
               ("Emerge" . (lambda (elm)
                             (anything-gentoo-install elm :action 'install)))
               ("Unmerge" . (lambda (elm)
                              (anything-gentoo-install elm :action 'uninstall)))
               ("Show dependencies" . (lambda (elm)
                                        (anything-c-gentoo-default-action elm "equery" "-C" "d")))
               ("Show related files" . (lambda (elm)
                                         (anything-c-gentoo-default-action elm "equery" "files")))
               ("Refresh" . (lambda (elm)
                              (anything-c-gentoo-setup-cache)
                              (setq anything-c-cache-world (anything-c-gentoo-get-world))))))))


(defun* anything-gentoo-install (candidate &key action)
  (setq anything-c-external-commands-list nil)
  (ansi-term (getenv "SHELL") "Gentoo emerge")
  (term-line-mode)
  (let ((command (case action
                   ('install "sudo emerge -av ")
                   ('uninstall "sudo emerge -avC ")
                   (t (error "Unknow action"))))
        (elms (mapconcat 'identity (anything-marked-candidates) " "))
        (beg (point)) end)
    (goto-char (point-max))
    (insert (concat command elms))
    (setq end (point))
    (term-char-mode) (term-send-input)))

(defun anything-c-gentoo-default-action (elm command &rest args)
  "Gentoo default action that use `anything-c-gentoo-buffer'."
  (if (member elm anything-c-cache-world)
      (progn
        (anything-c-switch-to-buffer anything-c-gentoo-buffer)
        (erase-buffer)
        (let ((com-list (append args (list elm))))
          (apply #'call-process command nil t nil
                 com-list)))
      (message "No infos on packages not yet installed")))

(defvar anything-c-source-use-flags
  '((name . "Use Flags")
    (init . (lambda ()
              (unless anything-c-gentoo-use-flags
                (anything-c-gentoo-setup-use-flags-cache))
              (anything-c-gentoo-get-use)))
    (candidates-in-buffer)
    (match . identity)
    (candidate-transformer anything-c-highlight-local-use)
    (action . (("Description"
                . (lambda (elm)
                    (anything-c-switch-to-buffer anything-c-gentoo-buffer)
                    (erase-buffer)
                    (apply #'call-process "euse" nil t nil
                           `("-i"
                             ,elm))
                    (font-lock-add-keywords nil `((,elm . font-lock-variable-name-face)))
                    (font-lock-mode 1)))
               ("Enable"
                . (lambda (elm)
                    (anything-c-gentoo-eshell-action elm "*sudo -p Password: euse -E")))
               ("Disable"
                . (lambda (elm)
                    (anything-c-gentoo-eshell-action elm "*sudo -p Password: euse -D")))
               ("Remove"
                . (lambda (elm)
                    (anything-c-gentoo-eshell-action elm "*sudo -p Password: euse -P")))
               ("Show which dep use this flag"
                . (lambda (elm)
                    (anything-c-switch-to-buffer anything-c-gentoo-buffer)
                    (erase-buffer)
                    (apply #'call-process "equery" nil t nil
                           `("-C"
                             "h"
                             ,elm))))))))



(defun anything-c-gentoo-init-list ()
  "Initialize buffer with all packages in Portage."
  (let* ((portage-buf (get-buffer-create "*anything-gentoo*"))
         (buf (anything-candidate-buffer 'portage-buf)))
    (with-current-buffer buf
      (dolist (i anything-c-cache-gentoo)
        (insert (concat i "\n"))))))

(defun anything-c-gentoo-setup-cache ()
  "Set up `anything-c-cache-gentoo'"
  (setq anything-c-cache-gentoo
        (split-string (with-temp-buffer
                        (call-process "eix" nil t nil
                                      "--only-names")
                        (buffer-string)))))

(defun anything-c-gentoo-eshell-action (elm command)
  (when (get-buffer "*EShell Command Output*")
    (kill-buffer "*EShell Command Output*"))
  (message "Wait searching...")
  (let ((buf-fname (buffer-file-name anything-current-buffer)))
    (if (and buf-fname (string-match tramp-file-name-regexp buf-fname))
        (progn
          (save-window-excursion
            (pop-to-buffer "*scratch*")
            (eshell-command (format "%s %s" command elm)))
          (pop-to-buffer "*EShell Command Output*"))
        (eshell-command (format "%s %s" command elm)))))

(defun anything-c-gentoo-get-use ()
  "Initialize buffer with all use flags."
  (let* ((use-buf (get-buffer-create "*anything-gentoo-use*"))
         (buf (anything-candidate-buffer 'use-buf)))
    (with-current-buffer buf
      (dolist (i anything-c-gentoo-use-flags)
        (insert (concat i "\n"))))))


(defun anything-c-gentoo-setup-use-flags-cache ()
  "Setup `anything-c-gentoo-use-flags'"
  (setq anything-c-gentoo-use-flags
        (split-string (with-temp-buffer
                        (call-process "eix" nil t nil
                                      "--print-all-useflags")
                        (buffer-string)))))

(defun anything-c-gentoo-get-url (elm)
  "Return a list of urls from eix output."
  (loop
        with url-list = (split-string
                         (with-temp-buffer
                           (call-process "eix" nil t nil
                                         elm "--format" "<homepage>\n")
                           (buffer-string)))
        with all
        for i in url-list
        when (and (string-match "^http://.*" i)
                  (not (member i all)))
        collect i into all
        finally return all))

(defun anything-c-gentoo-get-world ()
  "Return list of all installed package on your system."
  (split-string (with-temp-buffer
                  (call-process "qlist" nil t nil
                                "-I")
                  (buffer-string))))

(defun anything-c-gentoo-get-local-use ()
  (split-string (with-temp-buffer
                  (call-process "portageq" nil t nil
                                "envvar"
                                "USE")
                  (buffer-string))))


(defun anything-c-highlight-world (eix)
  "Highlight all installed package."
  (loop for i in eix
        if (member i anything-c-cache-world)
        collect (propertize i 'face 'anything-gentoo-match-face)
        else
        collect i))

(defun anything-c-highlight-local-use (use-flags)
  (let ((local-uses (anything-c-gentoo-get-local-use)))
    (loop for i in use-flags
          if (member i local-uses)
          collect (propertize i 'face 'anything-gentoo-match-face)
          else
          collect i)))



;;; Anything ratpoison UI
;;
;;
(defvar anything-c-source-ratpoison-commands
  '((name . "Ratpoison Commands")
    (init . anything-c-ratpoison-commands-init)
    (candidates-in-buffer)
    (action ("Execute the command" . anything-c-ratpoison-commands-execute))
    (display-to-real . anything-c-ratpoison-commands-display-to-real)
    (candidate-number-limit)))

(defun anything-c-ratpoison-commands-init ()
  (unless (anything-candidate-buffer)
    (with-current-buffer (anything-candidate-buffer 'global)
      ;; with ratpoison prefix key
      (save-excursion
        (call-process "ratpoison" nil (current-buffer) nil "-c" "help"))
      (while (re-search-forward "^\\([^ ]+\\) \\(.+\\)$" nil t)
        (replace-match "<ratpoison> \\1: \\2"))
      (goto-char (point-max))
      ;; direct binding
      (save-excursion
        (call-process "ratpoison" nil (current-buffer) nil "-c" "help top"))
      (while (re-search-forward "^\\([^ ]+\\) \\(.+\\)$" nil t)
        (replace-match "\\1: \\2")))))

(defun anything-c-ratpoison-commands-display-to-real (display)
  (and (string-match ": " display)
       (substring display (match-end 0))))

(defun anything-c-ratpoison-commands-execute (candidate)
  (call-process "ratpoison" nil nil nil "-ic" candidate))



;;; Anything `completing-read' replacement
;;
;;
(defun anything-comp-read-get-candidates (collection &optional test sort-fn alistp)
  "Convert COLLECTION to list removing elements that don't match TEST.
See `anything-comp-read' about supported COLLECTION arguments.

SORT-FN is a predicate to sort COLLECTION.

ALISTP when non--nil will not use `all-completions' to collect
candidates because it doesn't handle alists correctly for anything.
i.e In `all-completions' the keys \(cars of elements\)
are the possible completions. In anything we want to use the cdr instead
like \(display . real\).

e.g

\(setq A '((a . 1) (b . 2) (c . 3)))
==>((a . 1) (b . 2) (c . 3))
\(anything-comp-read \"test: \" A :alistp nil
                                  :exec-when-only-one t
                                  :initial-input \"a\")
==>\"a\"
\(anything-comp-read \"test: \" A :alistp t
                                  :exec-when-only-one t
                                  :initial-input \"1\")
==>\"1\"

See docstring of `all-completions' for more info.

If COLLECTION is an `obarray', a TEST should be needed. See `obarray'."
  (let ((cands
         (cond ((and (eq collection obarray) test)
                (all-completions "" collection test))
               ((and (vectorp collection) test)
                (loop for i across collection when (funcall test i) collect i))
               ((vectorp collection)
                (loop for i across collection collect i))
               ((and alistp test)
                (loop for i in collection when (funcall test i) collect i))
               ((and (symbolp collection) (boundp collection))
                (symbol-value collection))
               (alistp collection)
               ((and collection test)
                (all-completions "" collection test))
               (t (all-completions "" collection)))))
    (if sort-fn (sort cands sort-fn) cands)))

(defun anything-cr-default-transformer (candidates source)
  "Default filter candidate function for `anything-comp-read'.
Do nothing, just return candidate list unmodified."
  candidates)

(defun* anything-comp-read (prompt collection
                                   &key
                                   test
                                   initial-input
                                   default
                                   preselect
                                   (buffer "*Anything Completions*")
                                   must-match
                                   (requires-pattern 0)
                                   (history nil)
                                   input-history
                                   (persistent-action nil)
                                   (persistent-help "DoNothing")
                                   (mode-line anything-mode-line-string)
                                   (keymap anything-map)
                                   (name "Anything Completions")
                                   candidates-in-buffer
                                   exec-when-only-one
                                   (volatile t)
                                   sort
                                   (fc-transformer 'anything-cr-default-transformer)
                                   (marked-candidates nil)
                                   (alistp t))
  "Read a string in the minibuffer, with anything completion.

It is anything `completing-read' equivalent.

- PROMPT is the prompt name to use.

- COLLECTION can be a list, vector, obarray or hash-table.
  It can be also a function that receives three arguments:
  the values string, predicate and t. See `all-completions' for more details.

Keys description:

- TEST: A predicate called with one arg i.e candidate.

- INITIAL-INPUT: Same as input arg in `anything'.

- PRESELECT: See preselect arg of `anything'.

- DEFAULT: This option is used only for compatibility with regular
  Emacs `completing-read'.
 
- BUFFER: Name of anything-buffer.

- MUST-MATCH: Candidate selected must be one of COLLECTION.

- REQUIRES-PATTERN: Same as anything attribute, default is 0.

- HISTORY: A list containing specific history, default is nil.
  When it is non--nil, all elements of HISTORY are displayed in
  a special source before COLLECTION.

- INPUT-HISTORY: A symbol. the minibuffer input history will be
  stored there, if nil or not provided, `minibuffer-history'
  will be used instead.

- PERSISTENT-ACTION: A function called with one arg i.e candidate.

- PERSISTENT-HELP: A string to document PERSISTENT-ACTION.

- MODE-LINE: A string or list to display in mode line.
  (See `anything-mode-line-string')

- KEYMAP: A keymap to use in this `anything-comp-read'.
  (The keymap will be shared with history source)

- NAME: The name related to this local source.

- EXEC-WHEN-ONLY-ONE: Bound `anything-execute-action-at-once-if-one'
  to non--nil. (possibles values are t or nil).

- VOLATILE: Use volatile attribute \(enabled by default\).

- SORT: A predicate to give to `sort' e.g `string-lessp'.

- FC-TRANSFORMER: A `filtered-candidate-transformer' function.

- MARKED-CANDIDATES: If non--nil return candidate or marked candidates as a list.

- ALISTP: \(default is non--nil\) See `anything-comp-read-get-candidates'.

- CANDIDATES-IN-BUFFER: when non--nil use a source build with
  `anything-candidates-in-buffer' which is much faster.
  Argument VOLATILE have no effect when CANDIDATES-IN-BUFFER is non--nil.
 
Any prefix args passed during `anything-comp-read' invocation will be recorded
in `anything-current-prefix-arg', otherwise if prefix args were given before
`anything-comp-read' invocation, the value of `current-prefix-arg' will be used.
That's mean you can pass prefix args before or after calling a command
that use `anything-comp-read' See `anything-M-x' for example."
  (when (get-buffer anything-action-buffer)
    (kill-buffer anything-action-buffer))
  (flet ((action-fn (candidate)
           (if marked-candidates
               (anything-marked-candidates)
               (identity candidate))))
    ;; Assume completion have been already required,
    ;; so always use 'confirm.
    (when (eq must-match 'confirm-after-completion)
      (setq must-match 'confirm))
    (let* ((minibuffer-completion-confirm must-match)
           (must-match-map (when must-match
                             (let ((map (make-sparse-keymap)))
                               (define-key map (kbd "RET")
                                 'anything-confirm-and-exit-minibuffer)
                               map)))
           (anything-map (if must-match-map
                             (make-composed-keymap
                              must-match-map (or keymap anything-map))
                             (or keymap anything-map)))
           (src-hist `((name . ,(format "%s History" name))
                       (candidates
                        . (lambda ()
                            (let ((all (anything-comp-read-get-candidates
                                        history nil nil ,alistp)))
                              (delete
                               ""
                               (anything-fast-remove-dups
                                (if (and default (not (string= default "")))
                                    (delq nil (cons default
                                                    (delete default all)))
                                    all)
                                :test 'equal)))))
                       (filtered-candidate-transformer
                        . (lambda (candidates sources)
                            (loop for i in candidates
                                  do (set-text-properties 0 (length i) nil i)
                                  collect i)))
                       (persistent-action . ,persistent-action)
                       (persistent-help . ,persistent-help)
                       (mode-line . ,mode-line)
                       (action . ,'action-fn)))
           (src `((name . ,name)
                  (candidates
                   . (lambda ()
                       (let ((cands (anything-comp-read-get-candidates
                                     collection test sort alistp)))
                         (unless (or must-match (string= anything-pattern ""))
                           (setq cands (append (list anything-pattern) cands)))
                         (if (and default (not (string= default "")))
                             (delq nil (cons default (delete default cands)))
                             cands))))
                  (filtered-candidate-transformer ,fc-transformer)
                  (requires-pattern . ,requires-pattern)
                  (persistent-action . ,persistent-action)
                  (persistent-help . ,persistent-help)
                  (mode-line . ,mode-line)
                  (action . ,'action-fn)))
           (src-1 `((name . ,name)
                    (init
                     . (lambda ()
                         (let ((cands (anything-comp-read-get-candidates
                                       collection test sort alistp)))
                           (unless (or must-match (string= anything-pattern ""))
                             (setq cands (append (list anything-pattern) cands)))
                           (with-current-buffer (anything-candidate-buffer 'global)
                             (loop for i in
                                   (if (and default (not (string= default "")))
                                       (delq nil (cons default (delete default cands)))
                                       cands)
                                   do (insert (concat i "\n")))))))
                    (candidates-in-buffer)
                    (filtered-candidate-transformer ,fc-transformer)
                    (requires-pattern . ,requires-pattern)
                    (persistent-action . ,persistent-action)
                    (persistent-help . ,persistent-help)
                    (mode-line . ,mode-line)
                    (action . ,'action-fn)))
           (src-list (list src-hist
                           (if candidates-in-buffer
                               src-1
                               (if volatile
                                   (append src '((volatile)))
                                   src))))
           (anything-execute-action-at-once-if-one exec-when-only-one))
      (or
       (anything
        :sources src-list
        :input initial-input
        :default default
        :preselect preselect
        :prompt prompt
        :resume 'noresume
        :keymap anything-map
        :history (and (symbolp input-history) input-history)
        :buffer buffer)
       (when (and (eq anything-exit-status 0)
                  (eq must-match 'confirm))
         ;; Return empty string only if it is the DEFAULT
         ;; value and anything-pattern is empty.
         ;; otherwise return anything-pattern
         (if (and (string= anything-pattern "") default)
             default (identity anything-pattern)))
       (unless (or (eq anything-exit-status 1)
                   must-match) ; FIXME this should not be needed now.
         default)
       (keyboard-quit)))))

;; Generic completing-read
;;
;; Support also function as collection.
;; e.g M-x man is supported.
;; Support hash-table and vectors as collection.
;; NOTE:
;; Some crap emacs functions may not be supported
;; like ffap-alternate-file (bad use of completing-read)
;; and maybe others.
;; Provide a mode `anything-completion-mode' which turn on
;; anything in all `completing-read' and `read-file-name' in Emacs.
;;
(defvar anything-completion-mode-string " AC")

(defvar anything-completion-mode-quit-message
  "Anything completion disabled")

(defvar anything-completion-mode-start-message
  "Anything completion enabled")

;;; Specialized handlers
;;
;;
(defun anything-completing-read-symbols
    (prompt collection test require-match init
     hist default inherit-input-method name buffer)
  "Specialized function for fast symbols completion in `ac-mode'."
  (or
   (anything
    :sources `((name . ,name)
               (init . (lambda ()
                         (with-current-buffer (anything-candidate-buffer 'global)
                           (goto-char (point-min))
                           (when (and default (stringp default)
                                      ;; Some defaults args result as
                                      ;; (symbol-name nil) == "nil".
                                      ;; e.g debug-on-entry.
                                      (not (string= default "nil"))
                                      (not (string= default "")))
                             (insert (concat default "\n")))
                           (loop with all = (all-completions "" collection test)
                                 for sym in all
                                 unless (and default (eq sym default))
                                 do (insert (concat sym "\n"))))))
               (persistent-action . anything-lisp-completion-persistent-action)
               (persistent-help . "Show brief doc in mode-line")
               (candidates-in-buffer)
               (action . identity))
    :prompt prompt
    :buffer buffer
    :input init
    :history hist
    :resume 'noresume
    :default (or default ""))
   (keyboard-quit)))


;;; Generic completing read
;;
;;
(defun anything-completing-read-default-1
    (prompt collection test require-match
     init hist default inherit-input-method
     name buffer &optional cands-in-buffer exec-when-only-one)
  "Call `anything-comp-read' with same args as `completing-read'.
Extra optional arg CANDS-IN-BUFFER mean use `candidates-in-buffer'
method which is faster.
It should be used when candidate list don't need to rebuild dynamically."
  (let ((history (or (car-safe hist) hist)))
    (anything-comp-read
     prompt collection
     :test test
     :history history
     :input-history history
     :must-match require-match
     :alistp nil ; Be sure `all-completions' is used.
     :name name
     :requires-pattern (if (and (string= default "")
                                (or (eq require-match 'confirm)
                                    (eq require-match
                                        'confirm-after-completion)))
                           1 0)
     :candidates-in-buffer cands-in-buffer
     :exec-when-only-one exec-when-only-one
     :buffer buffer
     ;; If DEF is not provided, fallback to empty string
     ;; to avoid `thing-at-point' to be appended on top of list
     :default (or default "")
     ;; Use `regexp-quote' to fix initial input
     ;; with special characters (e.g nnimap+gmail:)
     :initial-input (and (stringp init) (regexp-quote init)))))

(defun anything-completing-read-with-cands-in-buffer
    (prompt collection test require-match
     init hist default inherit-input-method
     name buffer)
  "Same as `anything-completing-read-default-1' but use candidates-in-buffer."
  ;; Some commands like find-tag may use `read-file-name' from inside
  ;; the calculation of collection. in this case it clash with
  ;; candidates-in-buffer that reuse precedent data (files) which is wrong.
  ;; So (re)calculate collection outside of main anything-session.
  (let ((cands (all-completions "" collection)))
    (anything-completing-read-default-1 prompt cands test require-match
                                        init hist default inherit-input-method
                                        name buffer t)))

(defun* anything-completing-read-default
    (prompt collection &optional
            predicate require-match
            initial-input hist def
            inherit-input-method)
  "An anything replacement of `completing-read'.
This function should be used only as a `completing-read-function'.
 
Don't use it directly, use instead `anything-comp-read' in your programs.

See documentation of `completing-read' and `all-completions' for details."
  (declare (special anything-completion-mode))
  (let* ((current-command this-command)
         (str-command     (symbol-name current-command))
         (buf-name        (format "*ac-mode-%s*" str-command))
         (entry           (assq current-command
                                anything-completing-read-handlers-alist))
         (def-com         (cdr-safe entry))
         (str-defcom      (and def-com (symbol-name def-com)))
         (def-args        (list prompt collection predicate require-match
                                initial-input hist def inherit-input-method))
         ;; Append the two extra args needed to set the buffer and source name
         ;; in anything specialized functions.
         (any-args        (append def-args (list str-command buf-name)))
         anything-completion-mode-start-message ; Be quiet
         anything-completion-mode-quit-message
         (minibuffer-completion-table collection)
         (minibuffer-completion-predicate predicate)
         ;; Be sure this pesty *completion* buffer doesn't popup.
         (minibuffer-setup-hook (remove 'minibuffer-completion-help
                                        minibuffer-setup-hook)))
    (when (eq def-com 'ido) (setq def-com 'ido-completing-read))
    (unless (or (not entry) def-com)
      ;; An entry in *read-handlers-alist exists but have
      ;; a nil value, so we exit from here, disable `ac-mode'
      ;; and run the command again with it original behavior.
      ;; `ac-mode' will be restored on exit.
      (return-from anything-completing-read-default
        (unwind-protect
             (progn
               (ac-mode -1)
               (apply completing-read-function def-args))
          (ac-mode 1))))
    ;; If we use now `completing-read' we MUST turn off `ac-mode'
    ;; to avoid infinite recursion and CRASH. It will be reenabled on exit.
    (when (or (eq def-com 'completing-read)
              ;; All specialized functions are prefixed by "anything"
              (and (stringp str-defcom)
                   (not (string-match "^anything" str-defcom))))
      (ac-mode -1))
    (unwind-protect
         (cond (;; An anything specialized function exists, run it.
                (and def-com anything-completion-mode)
                (apply def-com any-args))
               (;; Try to handle `ido-completing-read' everywhere.
                (and def-com (eq def-com 'ido-completing-read))
                (setcar (memq collection def-args)
                        (all-completions "" collection predicate))
                (apply def-com def-args))
               (;; User set explicitely `completing-read' or something similar
                ;; in *read-handlers-alist, use this with exactly the same
                ;; args as in `completing-read'.
                ;; If we are here `anything-completion-mode' is now disabled.
                def-com
                (apply def-com def-args))
               (t ; Fall back to classic `anything-comp-read'.
                (anything-completing-read-default-1
                 prompt collection predicate require-match
                 initial-input hist def inherit-input-method
                 str-command buf-name)))
      (ac-mode 1)
      ;; When exiting minibuffer, `this-command' is set to
      ;; `anything-exit-minibuffer', which is unwanted when starting
      ;; on another `completing-read', so restore `this-command' to
      ;; initial value when exiting.
      (setq this-command current-command))))

(defun* anything-generic-read-file-name
    (prompt &optional dir default-filename mustmatch initial predicate)
  "An anything replacement of `read-file-name'."
  (declare (special anything-completion-mode))
  (let* ((default (and default-filename
                       (if (listp default-filename)
                           (car default-filename)
                           default-filename)))
         (init (or default initial dir default-directory))
         (ini-input (and init (expand-file-name init)))
         (current-command this-command)
         (str-command (symbol-name current-command))
         (anything-file-completion-sources
          (cons str-command
                (remove str-command anything-file-completion-sources)))
         (buf-name (format "*ac-mode-%s*" str-command))
         (entry (assq current-command
                      anything-completing-read-handlers-alist))
         (def-com  (cdr-safe entry))
         (str-defcom (symbol-name def-com))
         (def-args (list prompt dir default-filename mustmatch initial predicate))
         ;; Append the two extra args needed to set the buffer and source name
         ;; in anything specialized functions.
         (any-args (append def-args (list str-command buf-name)))
         (ido-state ido-mode)
         anything-completion-mode-start-message ; Be quiet
         anything-completion-mode-quit-message  ; Same here
         fname)
    ;; Some functions that normally call `completing-read' can switch
    ;; brutally to `read-file-name' (e.g find-tag), in this case
    ;; the anything specialized function will fail because it is build
    ;; for `completing-read', so set it to 'incompatible to be sure
    ;; we switch to `anything-c-read-file-name' and don't try to call it
    ;; with wrong number of args.
    (when (and def-com (> (length (help-function-arglist def-com)) 8))
      (setq def-com 'incompatible))
    (when (eq def-com 'ido) (setq def-com 'ido-read-file-name))
    (unless (or (not entry) def-com)
      (return-from anything-generic-read-file-name
        (unwind-protect
             (progn
               (ac-mode -1)
               (apply read-file-name-function def-args))
          (ac-mode 1))))
    ;; If we use now `read-file-name' we MUST turn off `ac-mode'
    ;; to avoid infinite recursion and CRASH. It will be reenabled on exit.
    (when (or (eq def-com 'read-file-name)
              (eq def-com 'ido-read-file-name)
              (and (stringp str-defcom)
                   (not (string-match "^anything" str-defcom))))
      (ac-mode -1))
    (unwind-protect
         (setq fname
               (cond (;; A specialized function exists, run it
                      ;; with the two extra args specific to anything..
                      (and def-com anything-completion-mode
                           (not (eq def-com 'ido-read-file-name))
                           (not (eq def-com 'incompatible)))
                      (apply def-com any-args))
                     (;; Def-com value is `ido-read-file-name'
                      ;; run it with default args.
                      (and def-com (eq def-com 'ido-read-file-name))
                      (ido-mode 1)
                      (apply def-com def-args))
                     (;; Def-com value is `read-file-name'
                      ;; run it with default args.
                      (eq def-com 'read-file-name)
                      (apply def-com def-args))
                     (t ; Fall back to classic `anything-c-read-file-name'.
                      (anything-c-read-file-name
                       prompt
                       :name str-command
                       :buffer buf-name
                       :initial-input (expand-file-name init dir)
                       :alistp nil
                       :must-match mustmatch
                       :test predicate))))
      (ac-mode 1)
      (ido-mode (if ido-state 1 -1))
      ;; Same comment as in `anything-completing-read-default'.
      (setq this-command current-command))
    (if (and mustmatch (not (file-exists-p fname)))
        (if (y-or-n-p "File does not exists, create buffer?")
            fname (error "Abort file does not exists"))
        fname)))

;;;###autoload
(define-minor-mode anything-completion-mode
    "Toggle generic anything completion.

All functions in Emacs that use `completing-read'
or `read-file-name' and friends will use anything interface
when this mode is turned on.
However you can modify this behavior for functions of your choice
with `anything-completing-read-handlers-alist'.

Called with a positive arg, turn on unconditionally, with a
negative arg turn off.
You can turn it on with `ac-mode'.

Some crap emacs functions may not be supported,
e.g `ffap-alternate-file' and maybe others
You can add such functions to `anything-completing-read-handlers-alist'
with a nil value.

Note: This mode will work only partially on Emacs23."
  :group 'anything
  :global t
  :lighter anything-completion-mode-string
  (declare (special completing-read-function))
  (if anything-completion-mode
      (progn
        (setq completing-read-function 'anything-completing-read-default
              read-file-name-function  'anything-generic-read-file-name)
        (message anything-completion-mode-start-message))
      (setq completing-read-function (and (fboundp 'completing-read-default)
                                          'completing-read-default)
            read-file-name-function  (and (fboundp 'read-file-name-default)
                                          'read-file-name-default))
      (message anything-completion-mode-quit-message)))

(defalias 'ac-mode 'anything-completion-mode)



;;; Eshell completion.
;;
;; Enable like this in .emacs:
;;
;; (add-hook 'eshell-mode-hook
;;           #'(lambda ()
;;               (define-key eshell-mode-map [remap pcomplete] 'anything-esh-pcomplete)))
;;
(defvar anything-c-source-esh
  '((name . "Eshell completions")
    (init . (lambda ()
              (setq pcomplete-current-completions nil
                    pcomplete-last-completion-raw nil)
              ;; Eshell-command add this hook in all minibuffers
              ;; Remove it for the anything one. (Fixed in Emacs24)
              (remove-hook 'minibuffer-setup-hook 'eshell-mode)))
    (candidates . anything-esh-get-candidates)
    (action . anything-ec-insert))
  "Anything source for Eshell completion.")

;; Internal.
(defvar anything-ec-target "")
(defun anything-ec-insert (candidate)
  "Replace text at point with CANDIDATE.
The function that call this should set `anything-ec-target' to thing at point.
This is the same as `ac-insert', just inlined here for compatibility."
  (let ((pt (point)))
    (when (and anything-ec-target
               (search-backward anything-ec-target nil t)
               (string= (buffer-substring (point) pt) anything-ec-target))
      (delete-region (point) pt)))
  (insert (anything-quote-whitespace candidate)))

(defun anything-esh-get-candidates ()
  "Get candidates for eshell completion using `pcomplete'."
  (catch 'pcompleted
    (let* ((pcomplete-stub)
           pcomplete-seen pcomplete-norm-func
           pcomplete-args pcomplete-last pcomplete-index
           (pcomplete-autolist pcomplete-autolist)
           (pcomplete-suffix-list pcomplete-suffix-list))
      (with-anything-current-buffer
        (loop with table  = (pcomplete-completions)
              with entry  = (condition-case nil
                                ;; On Emacs24 `try-completion' return
                                ;; pattern when more than one result.
                                ;; Otherwise Emacs23 return nil, which
                                ;; is wrong, in this case use pattern
                                ;; to behave like Emacs24.
                                (or (try-completion anything-pattern
                                                    (pcomplete-entries))
                                    anything-pattern)
                              ;; In Emacs23 `pcomplete-entries' may fail
                              ;; with error, so try this instead.
                              (error
                               nil
                               (let ((fc (car (last
                                               (pcomplete-parse-arguments)))))
                                 ;; Check if last arg require fname completion.
                                 (and (file-name-directory fc) fc))))
              for i in (all-completions pcomplete-stub table)
              for file-cand = (and entry
                                   (if (file-remote-p i) i
                                       (expand-file-name
                                        i (file-name-directory entry))))
              if (and file-cand (or (file-remote-p file-cand)
                                    (file-exists-p file-cand)))
              collect file-cand into ls
              else collect i into ls
              finally return
              (if (and entry (not (string= entry "")) (file-exists-p entry))
                  (append (list (expand-file-name entry default-directory)) ls)
                  ls))))))

;;; Eshell history.
;;
;;
(defvar anything-c-source-eshell-history
  `((name . "Eshell history")
    (init . (lambda ()
              (let (eshell-hist-ignoredups)
                ;; Write the content's of ring to file.
                (eshell-write-history eshell-history-file-name t)
                (with-current-buffer (anything-candidate-buffer 'global)
                  (insert-file-contents eshell-history-file-name)))
              ;; Same comment as in `anything-c-source-esh'
              (remove-hook 'minibuffer-setup-hook 'eshell-mode)))
    (candidates-in-buffer)
    (keymap . ,anything-eshell-history-map)
    (filtered-candidate-transformer . (lambda (candidates sources)
                                        (reverse candidates)))
    (candidate-number-limit . 9999)
    (action . (lambda (candidate)
                (eshell-kill-input)
                (insert candidate))))
  "Anything source for Eshell history.")


;;; Show completion - an alternative of anything-show-completion.el.
;;
;; Provide show completion with macro `with-anything-show-completion'.


;; Called each time cursor move in anything-buffer.
(defun anything-c-show-completion ()
  (with-anything-current-buffer
    (overlay-put anything-c-show-completion-overlay
                 'display (anything-get-selection))))

(defun anything-c-show-completion-init-overlay (beg end)
  (and anything-c-turn-on-show-completion
       (setq anything-c-show-completion-overlay (make-overlay beg end))
       (overlay-put anything-c-show-completion-overlay
                    'face 'anything-lisp-show-completion)))

(defun anything-c-show-completion-display-function (buffer)
  "A special resized anything window is used depending on position in BUFFER."
  (with-selected-window (selected-window)
    (let* ((screen-size  (+ (count-screen-lines (window-start) (point) t)
                            1                             ; mode-line
                            (if header-line-format 1 0))) ; header-line
           (def-size     (- (window-height)
                            anything-c-show-completion-min-window-height))
           (upper-height (max window-min-height (min screen-size def-size)))
           split-window-keep-point)
      (recenter -1)
      (set-window-buffer (if (active-minibuffer-window)
                             (minibuffer-selected-window)
                             (split-window nil upper-height))
                         buffer))))

(defmacro with-anything-show-completion (beg end &rest body)
  "Show anything candidate in an overlay at point.
BEG and END are the beginning and end position of the current completion
in `anything-current-buffer'.
BODY is an anything call where we want to enable show completion.
If `anything-c-turn-on-show-completion' is nil just do nothing."
  (declare (indent 2) (debug t))
  `(let ((anything-move-selection-after-hook
          (and anything-c-turn-on-show-completion
               (append (list 'anything-c-show-completion)
                       anything-move-selection-after-hook))))
     (unwind-protect
          (progn
            (anything-c-show-completion-init-overlay ,beg ,end)
            (let ((anything-display-function
                   (if anything-c-show-completion-use-special-display
                       'anything-c-show-completion-display-function
                       'anything-default-display-buffer)))
              ,@body))
       (and anything-c-turn-on-show-completion
            (delete-overlay anything-c-show-completion-overlay)))))


;;; Lisp symbol completion.
;;
;;
;;;###autoload
(defun anything-lisp-completion-at-point ()
  "Anything lisp symbol completion at point."
  (interactive)
  (let* ((data       (lisp-completion-at-point))
         (beg        (car data))
         (end        (point)) ; 'cadr data' is wrong when no space after point.
         (plist      (nthcdr 3 data))
         (pred       (plist-get plist :predicate))
         (lgst-len   0)
         (target     (and beg end (buffer-substring-no-properties beg end)))
         (candidates (all-completions target (nth 2 data) pred))
         (anything-quit-if-no-candidate t)
         
         (anything-execute-action-at-once-if-one t)
         (anything-match-plugin-enabled
          (member 'anything-compile-source--match-plugin
                  anything-compile-source-functions)))
    (if candidates
        (with-anything-show-completion beg end
          ;; Overlay is initialized now in anything-current-buffer.
          (anything
           :sources
           '((name . "Lisp completion")
             (init . (lambda ()
                       (with-current-buffer (anything-candidate-buffer 'global)
                         (loop for sym in candidates
                               for len = (length sym)
                               when (> len lgst-len) do (setq lgst-len len)
                               do (insert (concat sym "\n"))))))
             (candidates-in-buffer)
             (persistent-action . anything-lisp-completion-persistent-action)
             (persistent-help . "Show brief doc in mode-line")
             (filtered-candidate-transformer anything-lisp-completion-transformer)
             (action . (lambda (candidate)
                         (delete-region beg end)
                         (insert candidate))))
           :input (if anything-match-plugin-enabled (concat target " ") target)))
        (message "[No Match]"))))

(defun anything-lisp-completion-persistent-action (candidate)
  (let ((cursor-in-echo-area t)
        mode-line-in-non-selected-windows)
    (anything-c-show-info-in-mode-line
     (propertize
      (anything-c-get-first-line-documentation
       (intern candidate))
      'face 'anything-lisp-completion-info))))

(defun anything-lisp-completion-transformer (candidates source)
  "Anything candidates transformer for lisp completion."
  (declare (special lgst-len))
  (loop for c in candidates
        for sym = (intern c)
        for annot = (cond ((commandp sym) " (Com)")
                          ((fboundp sym)  " (Fun)")
                          ((boundp sym)   " (Var)")
                          ((facep sym)    " (Face)"))
        for spaces = (make-string (- lgst-len (length c)) ? )
        collect (cons (concat c spaces annot) c)))

(defun anything-c-get-first-line-documentation (sym)
  "Return first line documentation of symbol SYM.
If SYM is not documented, return \"Not documented\"."
  (let ((doc (cond ((fboundp sym)
                    (documentation sym t))
                   ((boundp sym)
                    (documentation-property sym 'variable-documentation t))
                   ((facep sym)
                    (face-documentation sym))
                   (t nil))))
    (if (and doc (not (string= doc ""))
             ;; `documentation' return "\n\n(args...)"
             ;; for CL-style functions.
             (not (string-match-p "^\n\n" doc)))
        (car (split-string doc "\n"))
        "Not documented")))

;;; File completion.
;;
;; Complete file name at point.
(defun anything-c-thing-before-point ()
  "Get symbol name before point.
Borrowed from anything-complete.el, inlined here for compatibility."
  (save-excursion
    (let ((beg (point)))
      ;; older regexp "\(\\|\\s-\\|^\\|\\_<\\|\r\\|'\\|#'"
      (when (re-search-backward
             "\\_<" (field-beginning nil nil (point-at-bol)) t)
        (buffer-substring-no-properties beg (match-end 0))))))

;;;###autoload
(defun anything-c-complete-file-name-at-point ()
  "Complete file name at point."
  (interactive)
  (let* ((init (substring-no-properties (thing-at-point 'filename)))
         (end  (point))
         (beg  (- (point) (length init)))
         (anything-quit-if-no-candidate t)
         (anything-execute-action-at-once-if-one t)
         completion)
    (with-anything-show-completion beg end
      (setq completion (anything-c-read-file-name "FileName: "
                                                  :initial-input init)))
    (anything-c-insert-file-name-completion-at-point completion)))

;; Internal
(defvar anything-lisp-completion-counter 0)
;;;###autoload
(defun anything-lisp-completion-at-point-or-indent (arg)
  "First call indent and second call complete lisp symbol.
The second call should happen before `anything-lisp-completion-or-indent-delay',
after this delay, next call will indent again.
After completion, next call is always indent.
See that like click and double mouse click.
One hit indent, two quick hits maybe indent and complete."
  (interactive "P")
  ;; Be sure `indent-for-tab-command' will not try
  ;; to use `completion-at-point'.
  (let ((tab-always-indent (if (eq tab-always-indent 'complete)
                               t tab-always-indent)))
    (incf anything-lisp-completion-counter)
    (unwind-protect
         (if (> anything-lisp-completion-counter 1)
             (anything-lisp-completion-or-file-name-at-point)
             (indent-for-tab-command arg))
      ;; After `anything-lisp-completion-or-indent-delay' seconds
      ;; reset to 0.
      (run-with-timer anything-lisp-completion-or-indent-delay nil
                      #'(lambda ()
                          (setq anything-lisp-completion-counter 0)))
      ;; Always reset to 0 at second hit.
      (when (eq anything-lisp-completion-counter 2)
        (setq anything-lisp-completion-counter 0)))))

;;;###autoload
(defun anything-lisp-completion-or-file-name-at-point ()
  "Complete lisp symbol or filename at point.
Filename completion happen if filename is started in
or between double quotes."
  (interactive)
  (let ((tap (substring-no-properties (thing-at-point 'filename))))
    (if (and tap (string-match "^\\(~/\\|/\\|[a-zA-Z]\:/\\).*" tap)
             (save-excursion (search-backward "\"" (point-at-bol) t)))
        (anything-c-complete-file-name-at-point)
        (anything-lisp-completion-at-point))))

(defun anything-c-apropos-init (test default)
  "Init candidates buffer for `anything-c-apropos' sources."
  (with-current-buffer (anything-candidate-buffer 'global)
    (goto-char (point-min))
    (when (and default (stringp default)
               ;; Some defaults args result as
               ;; (symbol-name nil) == "nil".
               ;; e.g debug-on-entry.
               (not (string= default "nil"))
               (funcall test (intern default)))
      (insert (concat default "\n")))
    (loop with all = (all-completions "" obarray test)
          for sym in all
          unless (and default (eq sym default))
          do (insert (concat sym "\n")))))


;;; Run Externals commands within Emacs with anything completion
;;
;;
(defvar anything-external-command-history nil)

(defun anything-c-external-commands-list-1 (&optional sort)
  "Returns a list of all external commands the user can execute.
If `anything-c-external-commands-list' is non-nil it will
return its contents.  Else it calculates all external commands
and sets `anything-c-external-commands-list'."
  (if anything-c-external-commands-list
      anything-c-external-commands-list
      (setq anything-c-external-commands-list
            (loop
                  with paths = (split-string (getenv "PATH") path-separator)
                  with completions = ()
                  for dir in paths
                  when (and (file-exists-p dir) (file-accessible-directory-p dir))
                  for lsdir = (loop for i in (directory-files dir t)
                                    for bn = (file-name-nondirectory i)
                                    when (and (not (member bn completions))
                                              (not (file-directory-p i))
                                              (file-executable-p i))
                                    collect bn)
                  append lsdir into completions
                  finally return (if sort (sort completions 'string-lessp) completions)))))

(defun anything-run-or-raise (exe &optional file)
  "Generic command that run asynchronously EXE.
If EXE is already running just jump to his window if `anything-raise-command'
is non--nil.
When FILE argument is provided run EXE with FILE.
In this case EXE must be provided as \"EXE %s\"."
  (lexical-let* ((real-com (car (split-string (replace-regexp-in-string
                                               "%s" "" exe))))
                 (proc     (if file (concat real-com " " file) real-com)))
    (if (get-process proc)
        (if anything-raise-command
            (shell-command  (format anything-raise-command real-com))
            (error "Error: %s is already running" real-com))
        (when (loop for i in anything-c-external-commands-list thereis real-com)
          (message "Starting %s..." real-com)
          (if file
              (start-process-shell-command
               proc nil (format exe (shell-quote-argument
                                     (if (eq system-type 'windows-nt)
                                         (anything-w32-prepare-filename file)
                                         file))))
              (start-process-shell-command proc nil real-com))
          (set-process-sentinel
           (get-process proc)
           #'(lambda (process event)
               (when (and (string= event "finished\n")
                          anything-raise-command
                          (not (anything-c-get-pid-from-process-name real-com)))
                 (shell-command  (format anything-raise-command "emacs")))
               (message "%s process...Finished." process))))
        (setq anything-c-external-commands-list
              (cons real-com
                    (delete real-com anything-c-external-commands-list))))))



;;; Generic action functions
;;
;;
(defun anything-c-file-buffers (filename)
  "Returns a list of buffer names corresponding to FILENAME."
  (let ((name     (expand-file-name filename))
        (buf-list ()))
    (dolist (buf (buffer-list) buf-list)
      (let ((bfn (buffer-file-name buf)))
        (when (and bfn (string= name bfn))
          (push (buffer-name buf) buf-list))))))

(defun anything-revert-buffer (candidate)
  (with-current-buffer candidate
    (when (or (buffer-modified-p)
              (not (verify-visited-file-modtime
                    (get-buffer candidate))))
      (revert-buffer t t))))

(defun anything-revert-marked-buffers (ignore)
  (mapc 'anything-revert-buffer (anything-marked-candidates)))

(defun anything-kill-marked-buffers (ignore)
  (mapc 'kill-buffer (anything-marked-candidates)))

(defun anything-c-delete-file (file &optional error-if-dot-file-p)
  "Delete the given file after querying the user.
Ask to kill buffers associated with that file, too."
  (when (and error-if-dot-file-p
             (anything-ff-dot-file-p file))
    (error "Error: Cannot operate on `.' or `..'"))
  (let ((buffers (anything-c-file-buffers file)))
    (if (< emacs-major-version 24)
        ;; `dired-delete-file' in Emacs versions < 24
        ;; doesn't support delete-by-moving-to-trash
        ;; so use `delete-directory' and `delete-file'
        ;; that handle it.
        (cond ((and (not (file-symlink-p file))
                    (file-directory-p file)
                    (directory-files file t dired-re-no-dot))
               (when (y-or-n-p (format "Recursive delete of `%s'? " file))
                 (delete-directory file 'recursive)))
              ((and (not (file-symlink-p file))
                    (file-directory-p file))
               (delete-directory file))
              (t (delete-file file)))
        (dired-delete-file
         file 'dired-recursive-deletes delete-by-moving-to-trash))
    (when buffers
      (dolist (buf buffers)
        (when (y-or-n-p (format "Kill buffer %s, too? " buf))
          (kill-buffer buf))))))

(defun anything-get-mailcap-for-file (filename)
  "Get the command to use for FILENAME from mailcap files.
The command is like <command %s> and is meant to use with `format'."
  (mailcap-parse-mailcaps)
  (let* ((ext  (file-name-extension filename))
         (mime (when ext (mailcap-extension-to-mime ext)))
         (result (when mime (mailcap-mime-info mime))))
    ;; If elisp file have no associations in .mailcap
    ;; `mailcap-maybe-eval' is returned, in this case just return nil.
    (when (stringp result) result)))

(defun anything-get-default-program-for-file (filename)
  "Try to find a default program to open FILENAME.
Try first in `anything-c-external-programs-associations' and then in mailcap file
if nothing found return nil."
  (let* ((ext      (file-name-extension filename))
         (def-prog (assoc-default ext anything-c-external-programs-associations)))
    (cond ((and def-prog (not (string= def-prog "")))
           (concat def-prog " %s"))
          ((and anything-c-default-external-file-browser
                (file-directory-p filename))
           (concat anything-c-default-external-file-browser " %s"))
          (t (anything-get-mailcap-for-file filename)))))

(defun anything-c-open-file-externally (file)
  "Open FILE with an external program.
Try to guess which program to use with `anything-get-default-program-for-file'.
If not found or a prefix arg is given query the user which tool to use."
  (let* ((fname          (expand-file-name file))
         (collection     (anything-c-external-commands-list-1 'sort))
         (def-prog       (anything-get-default-program-for-file fname))
         (real-prog-name (if (or anything-current-prefix-arg (not def-prog))
                             ;; Prefix arg or no default program.
                             (prog1
                                 (anything-comp-read
                                  "Program: " collection
                                  :must-match t
                                  :name "Open file Externally"
                                  :history anything-external-command-history)
                               ;; Always prompt to set this program as default.
                               (setq def-prog nil))
                             ;; No prefix arg or default program exists.
                             (replace-regexp-in-string " %s\\| '%s'" "" def-prog)))
         (program        (concat real-prog-name " %s")))
    (unless (or def-prog ; Association exists, no need to record it.
                ;; Don't try to record non--filenames associations (e.g urls).
                (not (file-exists-p fname)))
      (when
          (y-or-n-p
           (format
            "Do you want to make `%s' the default program for this kind of files? "
            real-prog-name))
        (anything-aif (assoc (file-name-extension fname)
                             anything-c-external-programs-associations)
            (setq anything-c-external-programs-associations
                  (delete it anything-c-external-programs-associations)))
        (push (cons (file-name-extension fname)
                    (read-string
                     "Program (Add args maybe and confirm): " real-prog-name))
              anything-c-external-programs-associations)
        (customize-save-variable 'anything-c-external-programs-associations
                                 anything-c-external-programs-associations)))
    (anything-run-or-raise program file)
    (setq anything-external-command-history
          (cons real-prog-name
                (delete real-prog-name
                        (loop for i in anything-external-command-history
                              when (executable-find i) collect i))))))

(defun anything-c-find-file-or-marked (candidate)
  "Open file CANDIDATE or open anything marked files in background."
  (let ((marked (anything-marked-candidates))
        (ffap-newfile-prompt anything-ff-newfile-prompt-p)
        (find-file-wildcards nil))
    (if (> (length marked) 1)
        ;; Open all marked files in background and display
        ;; the first one.
        (progn (mapc 'find-file-noselect (cdr marked))
               (find-file (car marked)))
        (if (and (not (file-exists-p candidate))
                 (and ffap-url-regexp
                      (not (string-match ffap-url-regexp candidate)))
                 (string-match "/$" candidate))
            ;; A a non--existing filename ending with /
            ;; Create a directory and jump to it.
            (when (y-or-n-p (format "Create directory `%s'? " candidate))
              (let ((dirfname (directory-file-name candidate)))
                (if (file-exists-p dirfname)
                    (error "Mkdir: Unable to create directory `%s': file exists."
                           (anything-c-basename dirfname))
                    (make-directory candidate 'parent)))
              (anything-find-files-1 candidate))
            ;; A non--existing filename NOT ending with / or
            ;; an existing filename, create or jump to it.
            (find-file-at-point (car marked))))))

(defun anything-delete-marked-files (ignore)
  (let* ((files (anything-marked-candidates))
         (len (length files)))
    (if (not (y-or-n-p
              (format "Delete *%s File(s):\n%s"
                      len
                      (mapconcat (lambda (f) (format "- %s\n" f)) files ""))))
        (message "(No deletions performed)")
        (dolist (i files)
          (set-text-properties 0 (length i) nil i)
          (anything-c-delete-file i anything-ff-signal-error-on-dot-files))
        (message "%s File(s) deleted" len))))

(defun anything-ediff-marked-buffers (candidate &optional merge)
  "Ediff 2 marked buffers or CANDIDATE and `anything-current-buffer'.
With optional arg MERGE call `ediff-merge-buffers'."
  (let ((lg-lst (length (anything-marked-candidates)))
        buf1 buf2)
    (case lg-lst
      (0
       (error "Error:You have to mark at least 1 buffer"))
      (1
       (setq buf1 anything-current-buffer
             buf2 (first (anything-marked-candidates))))
      (2
       (setq buf1 (first (anything-marked-candidates))
             buf2 (second (anything-marked-candidates))))
      (t
       (error "Error:To much buffers marked!")))
    (if merge
        (ediff-merge-buffers buf1 buf2)
        (ediff-buffers buf1 buf2))))

(defun anything-ediff-marked-buffers-merge (candidate)
  "Ediff merge `anything-current-buffer' with CANDIDATE.
See `anything-ediff-marked-buffers'."
  (anything-ediff-marked-buffers candidate t))

(defun anything-bookmark-get-bookmark-from-name (bmk)
  "Return bookmark name even if it is a bookmark with annotation.
e.g prepended with *.
Return nil if bmk is not a valid bookmark."
  (let ((bookmark (replace-regexp-in-string "\*" "" bmk)))
    (if (assoc bookmark bookmark-alist)
        bookmark
        (when (assoc bmk bookmark-alist)
          bmk))))

(defun anything-delete-marked-bookmarks (ignore)
  "Delete this bookmark or all marked bookmarks."
  (dolist (i (anything-marked-candidates))
    (bookmark-delete (anything-bookmark-get-bookmark-from-name i)
                     'batch)))

(defun anything-require-or-error (feature function)
  (or (require feature nil t)
      (error "Need %s to use `%s'." feature function)))

(defun anything-find-buffer-on-elscreen (candidate)
  "Open buffer in new screen, if marked buffers open all in elscreens."
  (anything-require-or-error 'elscreen 'anything-find-buffer-on-elscreen)
  (anything-aif (anything-marked-candidates)
      (dolist (i it)
        (let ((target-screen (elscreen-find-screen-by-buffer
                              (get-buffer i) 'create)))
          (elscreen-goto target-screen)))
    (let ((target-screen (elscreen-find-screen-by-buffer
                          (get-buffer candidate) 'create)))
      (elscreen-goto target-screen))))

(defun anything-elscreen-find-file (file)
  (anything-require-or-error 'elscreen 'anything-elscreen-find-file)
  (elscreen-find-file file))

(defun anything-w32-prepare-filename (file)
  "Convert filename FILE to something usable by external w32 executables."
  (replace-regexp-in-string ; For UNC paths
   "/" "\\"
   (replace-regexp-in-string ; Strip cygdrive paths
    "/cygdrive/\\(.\\)" "\\1:"
    file nil nil) nil t))

;;;###autoload
(defun anything-w32-shell-execute-open-file (file)
  (interactive "fOpen file:")
  (with-no-warnings
    (w32-shell-execute "open" (anything-w32-prepare-filename file))))

(defun anything-c-open-file-with-default-tool (file)
  "Open FILE with the default tool on this platform."
  (if (eq system-type 'windows-nt)
      (anything-w32-shell-execute-open-file file)
      (start-process "anything-c-open-file-with-default-tool"
                     nil
                     (cond ((eq system-type 'gnu/linux)
                            "xdg-open")
                           ((or (eq system-type 'darwin) ;; Mac OS X
                                (eq system-type 'macos)) ;; Mac OS 9
                            "open"))
                     file)))

(defun anything-c-open-dired (file)
  "Opens a dired buffer in FILE's directory.  If FILE is a
directory, open this directory."
  (if (file-directory-p file)
      (dired file)
      (dired (file-name-directory file))
      (dired-goto-file file)))

(defun anything-c-display-to-real-line (candidate)
  (if (string-match "^ *\\([0-9]+\\):\\(.*\\)$" candidate)
      (list (string-to-number (match-string 1 candidate)) (match-string 2 candidate))
      (error "Line number not found")))

(defun anything-c-action-line-goto (lineno-and-content)
  (apply #'anything-goto-file-line (anything-interpret-value (anything-attr 'target-file))
         (append lineno-and-content
                 (list (if (and (anything-attr-defined 'target-file)
                                (not anything-in-persistent-action))
                           'find-file-other-window
                           'find-file)))))

(defun* anything-c-action-file-line-goto (file-line-content &optional (find-file-function #'find-file))
  (apply #'anything-goto-file-line
         (if (stringp file-line-content)
             ;; Case: filtered-candidate-transformer is skipped
             (cdr (anything-c-filtered-candidate-transformer-file-line-1 file-line-content))
             file-line-content)))

(require 'compile)
(defun anything-c-filtered-candidate-transformer-file-line (candidates source)
  (delq nil (mapcar 'anything-c-filtered-candidate-transformer-file-line-1 candidates)))

(defun anything-c-filtered-candidate-transformer-file-line-1 (candidate)
  (when (string-match "^\\(.+?\\):\\([0-9]+\\):\\(.*\\)$" candidate)
    (let ((filename (match-string 1 candidate))
          (lineno (match-string 2 candidate))
          (content (match-string 3 candidate)))
      (cons (format "%s:%s\n %s"
                    (propertize filename 'face compilation-info-face)
                    (propertize lineno 'face compilation-line-face)
                    content)
            (list (expand-file-name
                   filename
                   (or (anything-interpret-value (anything-attr 'default-directory))
                       (and (anything-candidate-buffer)
                            (buffer-local-value
                             'default-directory (anything-candidate-buffer)))))
                  (string-to-number lineno) content)))))

(defun* anything-goto-file-line (file lineno content &optional (find-file-function #'find-file))
  (anything-aif (anything-attr 'before-jump-hook)
      (funcall it))
  (when file (funcall find-file-function file))
  (if (anything-attr-defined 'adjust)
      (anything-c-goto-line-with-adjustment lineno content)
      (anything-goto-line lineno))
  (unless (anything-attr-defined 'recenter)
    (set-window-start (get-buffer-window anything-current-buffer) (point)))
  (anything-aif (anything-attr 'after-jump-hook)
      (funcall it))
  (when anything-in-persistent-action
    (anything-match-line-color-current-line)))

(defun anything-find-file-as-root (candidate)
  (find-file (concat "/" anything-su-or-sudo "::" (expand-file-name candidate))))

(defun anything-find-many-files (ignore)
  (mapc 'find-file (anything-marked-candidates)))

;; borrowed from etags.el
;; (anything-c-goto-line-with-adjustment (line-number-at-pos) ";; borrowed from etags.el")
(defun anything-c-goto-line-with-adjustment (line line-content)
  (let ((startpos)
        offset found pat)
    ;; This constant is 1/2 the initial search window.
    ;; There is no sense in making it too small,
    ;; since just going around the loop once probably
    ;; costs about as much as searching 2000 chars.
    (setq offset 1000
          found nil
          pat (concat (if (eq selective-display t)
                          "\\(^\\|\^m\\) *" "^ *") ;allow indent
                      (regexp-quote line-content)))
    ;; If no char pos was given, try the given line number.
    (setq startpos (progn (anything-goto-line line) (point)))
    (or startpos (setq startpos (point-min)))
    ;; First see if the tag is right at the specified location.
    (goto-char startpos)
    (setq found (looking-at pat))
    (while (and (not found)
                (progn
                  (goto-char (- startpos offset))
                  (not (bobp))))
      (setq found
            (re-search-forward pat (+ startpos offset) t)
            offset (* 3 offset)))       ; expand search window
    (or found
        (re-search-forward pat nil t)
        (error "not found")))
  ;; Position point at the right place
  ;; if the search string matched an extra Ctrl-m at the beginning.
  (and (eq selective-display t)
       (looking-at "\^m")
       (forward-char 1))
  (beginning-of-line))

(anything-document-attribute 'default-directory "type . file-line"
                             "`default-directory' to interpret file.")
(anything-document-attribute 'before-jump-hook "type . file-line / line"
                             "Function to call before jumping to the target location.")
(anything-document-attribute 'after-jump-hook "type . file-line / line"
                             "Function to call after jumping to the target location.")
(anything-document-attribute 'adjust "type . file-line"
                             "Search around line matching line contents.")
(anything-document-attribute 'recenter "type . file-line / line"
                             "`recenter' after jumping.")
(anything-document-attribute 'target-file "type . line"
                             "Goto line of target-file.")

;;;###autoload
(defun anything-c-call-interactively (cmd-or-name)
  "Execute CMD-OR-NAME as Emacs command.
It is added to `extended-command-history'.
`anything-current-prefix-arg' is used as the command's prefix argument."
  (setq extended-command-history
        (cons (anything-c-stringify cmd-or-name)
              (delete (anything-c-stringify cmd-or-name) extended-command-history)))
  (let ((current-prefix-arg anything-current-prefix-arg)
        (cmd (anything-c-symbolify cmd-or-name)))
    (if (stringp (symbol-function cmd))
        (execute-kbd-macro (symbol-function cmd))
        (setq this-command cmd)
        (call-interactively cmd))))

;;;###autoload
(defun anything-c-set-variable (var)
  "Set value to VAR interactively."
  (interactive)
  (let ((sym (anything-c-symbolify var)))
    (set sym (eval-minibuffer (format "Set %s: " var)
                              (prin1-to-string (symbol-value sym))))))
;; (setq hh 12)
;; (anything-c-set-variable 'hh)



;;; Persistent Action Helpers
;;
;;
(defvar anything-match-line-overlay-face nil)
(defvar anything-match-line-overlay nil)

(defun anything-match-line-color-current-line (&optional start end buf face rec)
  "Highlight and underline current position"
  (let ((args (list (or start (line-beginning-position))
                    (or end (1+ (line-end-position)))
                    buf)))
    (if (not anything-match-line-overlay)
        (setq anything-match-line-overlay (apply 'make-overlay args))
        (apply 'move-overlay anything-match-line-overlay args)))
  (overlay-put anything-match-line-overlay
               'face (or face anything-match-line-overlay-face))
  (when rec
    (goto-char start)
    (recenter)))

(defalias 'anything-persistent-highlight-point 'anything-match-line-color-current-line)


(setq anything-match-line-overlay-face 'anything-overlay-line-face)

(defun anything-match-line-cleanup ()
  (when anything-match-line-overlay
    (delete-overlay anything-match-line-overlay)
    (setq anything-match-line-overlay nil)))

(defun anything-match-line-update ()
  (when anything-match-line-overlay
    (delete-overlay anything-match-line-overlay)
    (anything-match-line-color-current-line)))

(add-hook 'anything-cleanup-hook 'anything-match-line-cleanup)
(add-hook 'anything-after-persistent-action-hook 'anything-match-line-update)


;;; Actions Transformers
;;
;;
;;; Files
(defun anything-c-transform-file-load-el (actions candidate)
  "Add action to load the file CANDIDATE if it is an emacs lisp
file.  Else return ACTIONS unmodified."
  (if (member (file-name-extension candidate) '("el" "elc"))
      (append actions '(("Load Emacs Lisp File" . load-file)))
      actions))

(defun anything-c-transform-file-browse-url (actions candidate)
  "Add an action to browse the file CANDIDATE if it in a html
file or URL.  Else return ACTIONS unmodified."
  (let ((browse-action '("Browse with Browser" . browse-url)))
    (cond ((string-match "^http\\|^ftp" candidate)
           (cons browse-action actions))
          ((string-match "\\.html?$" candidate)
           (append actions (list browse-action)))
          (t actions))))

;;; Function
(defun anything-c-transform-function-call-interactively (actions candidate)
  "Add an action to call the function CANDIDATE interactively if
it is a command.  Else return ACTIONS unmodified."
  (if (commandp (intern-soft candidate))
      (append actions '(("Call Interactively"
                         .
                         anything-c-call-interactively)))
      actions))

;;;; S-Expressions
(defun anything-c-transform-sexp-eval-command-sexp (actions candidate)
  "If CANDIDATE's `car' is a command, then add an action to
evaluate it and put it onto the `command-history'."
  (if (commandp (car (read candidate)))
      ;; Make it first entry
      (cons '("Eval and put onto command-history" .
              (lambda (sexp)
                (let ((sym (read sexp)))
                  (eval sym)
                  (setq command-history
                        (cons sym command-history)))))
            actions)
      actions))


;;; Candidate Transformers
;;
;;
;;; Buffers
(defun anything-c-skip-boring-buffers (buffers)
  (anything-c-skip-entries buffers anything-c-boring-buffer-regexp))

(defun anything-c-skip-current-buffer (buffers)
  "[DEPRECATED] Skip current buffer in buffer lists.
This transformer should not be used as this is now handled directly
in `anything-c-buffer-list' and `anything-c-highlight-buffers'."
  (if anything-allow-skipping-current-buffer
      (remove (buffer-name anything-current-buffer) buffers)
      buffers))

(defun anything-c-shadow-boring-buffers (buffers)
  "Buffers matching `anything-c-boring-buffer-regexp' will be
displayed with the `file-name-shadow' face if available."
  (anything-c-shadow-entries buffers anything-c-boring-buffer-regexp))

(defvar anything-c-buffer-display-string-functions
  '(anything-c-buffer-display-string--compilation
    anything-c-buffer-display-string--shell
    anything-c-buffer-display-string--eshell)
  "Functions to setup display string for buffer.

Function has one argument, buffer name.
If it returns string, use it.
If it returns nil, display buffer name.
See `anything-c-buffer-display-string--compilation' for example.")

(defun anything-c-transform-buffer-display-string (buffers)
  "Setup display string for buffer candidates
using `anything-c-buffer-display-string-functions'."
  (loop for buf in buffers
        if (consp buf)
        collect buf
        else
        for disp = (progn (set-buffer buf)
                          (run-hook-with-args-until-success
                           'anything-c-buffer-display-string-functions buf))
        collect (if disp (cons disp buf) buf)))

(defun anything-c-buffer-display-string--compilation (buf)
  (anything-aif (car compilation-arguments)
      (format "%s: %s [%s]" buf it default-directory)))

(defun anything-c-buffer-display-string--eshell (buf)
  (declare (special eshell-history-ring))
  (when (eq major-mode 'eshell-mode)
    (format "%s: %s [%s]" buf
            (ignore-errors (ring-ref eshell-history-ring 0))
            default-directory)))

(defun anything-c-buffer-display-string--shell (buf)
  (when (eq major-mode 'shell-mode)
    (format "%s: %s [%s]" buf
            (ignore-errors (ring-ref comint-input-ring 0))
            default-directory)))

;;; Files
(defun anything-c-shadow-boring-files (files)
  "Files matching `anything-c-boring-file-regexp' will be
displayed with the `file-name-shadow' face if available."
  (anything-c-shadow-entries files anything-c-boring-file-regexp))

(defun anything-c-skip-boring-files (files)
  "Files matching `anything-c-boring-file-regexp' will be skipped."
  (anything-c-skip-entries files anything-c-boring-file-regexp))
;; (anything-c-skip-boring-files '("README" "/src/.svn/hoge"))

(defun anything-c-skip-current-file (files)
  "Current file will be skipped."
  (remove (buffer-file-name anything-current-buffer) files))

(defun anything-c-w32-pathname-transformer (args)
  "Change undesirable features of windows pathnames to ones more acceptable to
other candidate transformers."
  (if (eq system-type 'windows-nt)
      (anything-transform-mapcar
       (lambda (x)
         (replace-regexp-in-string
          "/cygdrive/\\(.\\)" "\\1:"
          (replace-regexp-in-string "\\\\" "/" x)))
       args)
    args))

(defun anything-c-shorten-home-path (files)
  "Replaces /home/user with ~."
  (let ((home (replace-regexp-in-string "\\\\" "/" ; stupid Windows...
                                        (getenv "HOME"))))
    (anything-transform-mapcar
     (lambda (file)
       (if (and (stringp file) (string-match home file))
           (cons (replace-match "~" nil nil file) file)
         file))
     files)))

;;; Functions
(defun anything-c-mark-interactive-functions (functions)
  "Mark interactive functions (commands) with (i) after the function name."
  (let (list)
    (loop for function in functions
          do (push (cons (concat function
                                 (when (commandp (intern-soft function)) " (i)"))
                         function)
                   list)
          finally (return (nreverse list)))))


;;; Adaptive Sorting of Candidates
;;
;;
;; Internal
(defvar anything-c-adaptive-done nil
  "nil if history information is not yet stored for the current
selection.")

(defvar anything-c-adaptive-history nil
  "Contains the stored history information.
Format: ((SOURCE-NAME (SELECTED-CANDIDATE (PATTERN . NUMBER-OF-USE) ...) ...) ...)")

;; Should run at beginning of `anything-initial-setup'.
(add-hook 'anything-before-initialize-hook #'(lambda ()
                                               (when anything-c-use-adaptative-sorting
                                                 (setq anything-c-adaptive-done nil))))

;; Should run at beginning of `anything-exit-minibuffer'.
(add-hook 'anything-before-action-hook #'(lambda ()
                                          (when anything-c-use-adaptative-sorting
                                            (anything-c-adaptive-store-selection))))

;; Should run at beginning of `anything-select-action'.
(add-hook 'anything-select-action-hook #'(lambda ()
                                           (when anything-c-use-adaptative-sorting
                                             (anything-c-adaptive-store-selection))))

(defun anything-c-source-use-adaptative-p (&optional source-name)
  "Return current source only if it use adaptative history, nil otherwise."
  (when anything-c-use-adaptative-sorting
    (let* ((source (or source-name (anything-get-current-source)))
           (adapt-source (or (assoc-default 'filtered-candidate-transformer
                                            (assoc (assoc-default 'type source)
                                                   anything-type-attributes))
                             (assoc-default 'candidate-transformer
                                            (assoc (assoc-default 'type source)
                                                   anything-type-attributes))
                             (assoc-default 'filtered-candidate-transformer source)
                             (assoc-default 'candidate-transformer source))))
      (if (listp adapt-source)
          (when (member 'anything-c-adaptive-sort adapt-source) source)
          (when (eq adapt-source 'anything-c-adaptive-sort) source)))))

(defun anything-c-adaptive-store-selection ()
  "Store history information for the selected candidate."
  (unless anything-c-adaptive-done
    (setq anything-c-adaptive-done t)
    (let ((source (anything-c-source-use-adaptative-p)))
      (when source
        (let* ((source-name (or (assoc-default 'type source)
                                (assoc-default 'name source)))
               (source-info (or (assoc source-name anything-c-adaptive-history)
                                (progn
                                  (push (list source-name) anything-c-adaptive-history)
                                  (car anything-c-adaptive-history))))
               (selection (anything-get-selection))
               (selection-info (progn
                                 (setcdr source-info
                                         (cons
                                          (let ((found (assoc selection (cdr source-info))))
                                            (if (not found)
                                                ;; new entry
                                                (list selection)

                                                ;; move entry to the beginning of the
                                                ;; list, so that it doesn't get
                                                ;; trimmed when the history is
                                                ;; truncated
                                                (setcdr source-info
                                                        (delete found (cdr source-info)))
                                                found))
                                          (cdr source-info)))
                                 (cadr source-info)))
               (pattern-info (progn
                               (setcdr selection-info
                                       (cons
                                        (let ((found (assoc anything-pattern (cdr selection-info))))
                                          (if (not found)
                                              ;; new entry
                                              (cons anything-pattern 0)

                                              ;; move entry to the beginning of the
                                              ;; list, so if two patterns used the
                                              ;; same number of times then the one
                                              ;; used last appears first in the list
                                              (setcdr selection-info
                                                      (delete found (cdr selection-info)))
                                              found))
                                        (cdr selection-info)))
                               (cadr selection-info))))

          ;; increase usage count
          (setcdr pattern-info (1+ (cdr pattern-info)))

          ;; truncate history if needed
          (if (> (length (cdr selection-info)) anything-c-adaptive-history-length)
              (setcdr selection-info
                      (subseq (cdr selection-info) 0 anything-c-adaptive-history-length))))))))

(defun anything-c-adaptative-maybe-load-history ()
  (when (and anything-c-use-adaptative-sorting
             (file-readable-p anything-c-adaptive-history-file))
    (load-file anything-c-adaptive-history-file)))

(add-hook 'emacs-startup-hook 'anything-c-adaptative-maybe-load-history)
(add-hook 'kill-emacs-hook 'anything-c-adaptive-save-history)

(defun anything-c-adaptive-save-history (&optional arg)
  "Save history information to file given by `anything-c-adaptive-history-file'."
  (interactive "p")
  (when anything-c-use-adaptative-sorting
    (with-temp-buffer
      (insert
       ";; -*- mode: emacs-lisp -*-\n"
       ";; History entries used for anything adaptive display.\n")
      (prin1 `(setq anything-c-adaptive-history ',anything-c-adaptive-history)
             (current-buffer))
      (insert ?\n)
      (write-region (point-min) (point-max) anything-c-adaptive-history-file nil
                    (unless arg 'quiet)))))

(defun anything-c-adaptive-sort (candidates source)
  "Sort the CANDIDATES for SOURCE by usage frequency.
This is a filtered candidate transformer you can use for the
attribute `filtered-candidate-transformer' of a source in
`anything-sources' or a type in `anything-type-attributes'."
  (let* ((source-name (or (assoc-default 'type source)
                          (assoc-default 'name source)))
         (source-info (assoc source-name anything-c-adaptive-history)))
    (if source-info
        (let ((usage
               ;; ... assemble a list containing the (CANIDATE . USAGE-COUNT)
               ;; pairs
               (mapcar (lambda (candidate-info)
                         (let ((count 0))
                           (dolist (pattern-info (cdr candidate-info))
                             (if (not (equal (car pattern-info)
                                             anything-pattern))
                                 (incf count (cdr pattern-info))

                                 ;; if current pattern is equal to the previously
                                 ;; used one then this candidate has priority
                                 ;; (that's why its count is boosted by 10000) and
                                 ;; it only has to compete with other candidates
                                 ;; which were also selected with the same pattern
                                 (setq count (+ 10000 (cdr pattern-info)))
                                 (return)))
                           (cons (car candidate-info) count)))
                       (cdr source-info)))
              sorted)
          (if (and usage (consp usage))
              ;; sort the list in descending order, so candidates with highest
              ;; priorty come first
              (progn
                (setq usage (sort usage (lambda (first second)
                                          (> (cdr first) (cdr second)))))

                ;; put those candidates first which have the highest usage count
                (dolist (info usage)
                  (when (member* (car info) candidates
                                 :test 'anything-c-adaptive-compare)
                    (push (car info) sorted)
                    (setq candidates (remove* (car info) candidates
                                              :test 'anything-c-adaptive-compare))))

                ;; and append the rest
                (append (reverse sorted) candidates nil))
              (message "Your `%s' is maybe corrupted or too old, \
you should reinitialize it with `anything-c-reset-adaptative-history'"
                       anything-c-adaptive-history-file)
              (sit-for 1)
              candidates))
        ;; if there is no information stored for this source then do nothing
        candidates)))

;;;###autoload
(defun anything-c-reset-adaptative-history ()
  "Delete all `anything-c-adaptive-history' and his file.
Useful when you have a old or corrupted `anything-c-adaptive-history-file'."
  (interactive)
  (when (y-or-n-p "Really delete all your `anything-c-adaptive-history'? ")
    (setq anything-c-adaptive-history nil)
    (delete-file anything-c-adaptive-history-file)))

(defun anything-c-adaptive-compare (x y)
  "Compare candidates X and Y taking into account that the
candidate can be in (DISPLAY . REAL) format."
  (equal (if (listp x)
             (cdr x)
             x)
         (if (listp y)
             (cdr y)
             y)))



;;; Outliner
;;
;;
(defvar anything-outline-goto-near-line-flag t)
(defvar anything-outline-using nil)
(defun anything-after-update-hook--outline ()
  (if (and (eq anything-outline-using t)
           (eq anything-outline-goto-near-line-flag t))
      (anything-outline-goto-near-line)))
(add-hook 'anything-after-update-hook 'anything-after-update-hook--outline)

(defun anything-outline-goto-near-line ()
  (with-anything-window
    ;; TODO need consideration whether to update position by every input.
    (when t ; (equal anything-pattern "")
      (anything-goto-line 2)
      (let ((lineno (with-anything-current-buffer
                      (line-number-at-pos (car anything-current-position)))))
        (block exit
          (while (<= (progn (skip-chars-forward " ")
                            (or (number-at-point) lineno))
                     lineno)
            (forward-line 1)
            (when (eobp)
              (forward-line -1)
              (return-from exit))))
        (forward-line -1)
        (and (bobp) (forward-line 1))
        (and (anything-pos-header-line-p) (forward-line -2))
        (anything-mark-current-line)))))



;;; Plug-in
;;
;;
;; Plug-in: info-index
(defun* anything-c-info-init (&optional (file (anything-attr 'info-file)))
  (let (result)
    (unless (anything-candidate-buffer)
      (save-window-excursion
        (info file)
        (let (Info-history
              (tobuf (anything-candidate-buffer 'global))
              (infobuf (current-buffer))
              s e)
          (dolist (node (or (anything-attr 'index-nodes) (Info-index-nodes)))
            (Info-goto-node node)
            (goto-char (point-min))
            (while (search-forward "\n* " nil t)
              (unless (search-forward "Menu:\n" (1+ (point-at-eol)) t)
                '(save-current-buffer (buffer-substring-no-properties (point-at-bol) (point-at-eol)) result)
                (setq s (point-at-bol)
                      e (point-at-eol))
                (with-current-buffer tobuf
                  (insert-buffer-substring infobuf s e)
                  (insert "\n"))))))))))

(defun anything-c-info-goto (node-line)
  (Info-goto-node (car node-line))
  (anything-goto-line (cdr node-line)))

(defun anything-c-info-display-to-real (line)
  (and (string-match
        ;; This regexp is stolen from Info-apropos-matches
        "\\* +\\([^\n]*.+[^\n]*\\):[ \t]+\\([^\n]*\\)\\.\\(?:[ \t\n]*(line +\\([0-9]+\\))\\)?" line)
       (cons (format "(%s)%s" (anything-attr 'info-file) (match-string 2 line))
             (string-to-number (or (match-string 3 line) "1")))))

(defun anything-c-make-info-source (source file)
  `(,@source
    (name . ,(concat "Info Index: " file))
    (info-file . ,file)
    (init . anything-c-info-init)
    (display-to-real . anything-c-info-display-to-real)
    (get-line . buffer-substring)
    (candidates-in-buffer)
    (action ("Goto node" . anything-c-info-goto))))

(defun anything-compile-source--info-index (source)
  (anything-aif (anything-interpret-value (assoc-default 'info-index source))
      (anything-c-make-info-source source it)
    source))
(add-to-list 'anything-compile-source-functions 'anything-compile-source--info-index)

(anything-document-attribute 'info-index "info-index plugin"
                             "Create a source of info index very easily.

ex. (defvar anything-c-source-info-wget '((info-index . \"wget\"))")

(anything-document-attribute 'index-nodes "info-index plugin (optional)"
                             "Index nodes of info file.

If it is omitted, `Info-index-nodes' is used to collect index nodes.
Some info files are missing index specification.

ex. See `anything-c-source-info-screen'.")

;; Plug-in: candidates-file
(defun anything-compile-source--candidates-file (source)
  (if (assoc-default 'candidates-file source)
      `((init anything-p-candidats-file-init
              ,@(let ((orig-init (assoc-default 'init source)))
                     (cond ((null orig-init) nil)
                           ((functionp orig-init) (list orig-init))
                           (t orig-init))))
        (candidates-in-buffer)
        ,@source)
      source))
(add-to-list 'anything-compile-source-functions 'anything-compile-source--candidates-file)

(defun anything-p-candidats-file-init ()
  (destructuring-bind (file &optional updating)
      (anything-mklist (anything-attr 'candidates-file))
    (setq file (anything-interpret-value file))
    (with-current-buffer (anything-candidate-buffer (find-file-noselect file))
      (when updating
        (buffer-disable-undo)
        (font-lock-mode -1)
        (auto-revert-mode 1)))))

(anything-document-attribute 'candidates-file "candidates-file plugin"
                             "Use a file as the candidates buffer.

1st argument is a filename, string or function name or variable name.
If optional 2nd argument is non-nil, the file opened with `auto-revert-mode'.")

;; Plug-in: headline
(defun anything-compile-source--anything-headline (source)
  (if (assoc-default 'headline source)
      (append '((init . anything-headline-init)
                (get-line . buffer-substring)
                (type . line))
              source
              '((candidates-in-buffer)
                (persistent-help . "Show this line")))
      source))
(add-to-list 'anything-compile-source-functions 'anything-compile-source--anything-headline)

(defun anything-headline-init ()
  (when (and (anything-current-buffer-is-modified)
             (with-anything-current-buffer
               (eval (or (anything-attr 'condition) t))))
    (anything-headline-make-candidate-buffer
     (anything-interpret-value (anything-attr 'headline))
     (anything-interpret-value (anything-attr 'subexp)))))

(anything-document-attribute 'headline "Headline plug-in"
                             "Regexp string for anything-headline to scan.")
(anything-document-attribute 'condition "Headline plug-in"
                             "A sexp representing the condition to use anything-headline.")
(anything-document-attribute 'subexp "Headline plug-in"
                             "Display (match-string-no-properties subexp).")

;; Le Wang: Note on how `anything-head-line-get-candidates' works with a list
;; of regexps.
;;
;;   1. Create list of ((title . start-of-match) . hiearchy)
;;   2. Sort this list by start-of-match.
;;   3. Go through sorted list and return titles that reflect full hiearchy.
;;
;; It's quite brilliantly written.
;;


(defun anything-headline-get-candidates (regexp subexp)
  (with-anything-current-buffer
    (save-excursion
      (goto-char (point-min))
      (if (functionp regexp) (setq regexp (funcall regexp)))
      (let (hierarchy curhead)
        (flet ((matched ()
                 (if (numberp subexp)
                     (cons (match-string-no-properties subexp) (match-beginning subexp))
                     (cons (buffer-substring (point-at-bol) (point-at-eol))
                           (point-at-bol))))
               (hierarchies (headlines)
                 (1+ (loop for (_ . hierarchy) in headlines
                           maximize hierarchy)))
               (vector-0-n (v n)
                 (loop for i from 0 to hierarchy
                       collecting (aref curhead i)))
               (arrange (headlines)
                 (unless (null headlines) ; FIX headlines empty bug!
                   (loop with curhead = (make-vector (hierarchies headlines) "")
                         for ((str . pt) . hierarchy) in headlines
                         do (aset curhead hierarchy str)
                         collecting
                         (cons
                          (format "H%d:%s" (1+ hierarchy)
                                  (mapconcat 'identity (vector-0-n curhead hierarchy) " / "))
                          pt)))))
          (if (listp regexp)
              (arrange
               (sort
                (loop for re in regexp
                      for hierarchy from 0
                      do (goto-char (point-min))
                      appending
                      (loop
                            while (re-search-forward re nil t)
                            collect (cons (matched) hierarchy)))
                (lambda (a b) (> (cdar b) (cdar a)))))
              (loop while (re-search-forward regexp nil t)
                    collect (matched))))))))


(defun anything-headline-make-candidate-buffer (regexp subexp)
  (with-current-buffer (anything-candidate-buffer 'local)
    (loop for (content . pos) in (anything-headline-get-candidates regexp subexp)
          do (insert
              (format "%5d:%s\n"
                      (with-anything-current-buffer
                        (line-number-at-pos pos))
                      content)))))

(defun anything-headline-goto-position (pos recenter)
  (goto-char pos)
  (unless recenter
    (set-window-start (get-buffer-window anything-current-buffer) (point))))


;; Plug-in: persistent-help
(defun anything-compile-source--persistent-help (source)
  (append source '((header-line . anything-persistent-help-string))))
(add-to-list 'anything-compile-source-functions 'anything-compile-source--persistent-help)

(defun anything-persistent-help-string ()
  (substitute-command-keys
   (concat "\\<anything-map>\\[anything-execute-persistent-action]: "
           (or (anything-interpret-value (anything-attr 'persistent-help))
               (anything-aif (or (assoc-default 'persistent-action
                                                (anything-get-current-source))
                                 (assoc-default 'action
                                                (anything-get-current-source)))
                   (cond ((symbolp it) (symbol-name it))
                         ((listp it) (or (ignore-errors (caar it))  ""))))
               "")
           " (keeping session)")))

(anything-document-attribute 'persistent-help "persistent-help plug-in"
                             "A string to explain persistent-action of this source.
It also accepts a function or a variable name.")

;;; (anything '(((name . "persistent-help test")(candidates "a")(persistent-help . "TEST"))))

;; Plug-in: Type customize
(defun anything-c-uniq-list (lst)
  "Like `remove-duplicates' in CL.
But cut deeper duplicates and test by `equal'. "
  (reverse (remove-duplicates (reverse lst) :test 'equal)))
(defvar anything-additional-type-attributes nil)
(defun anything-c-arrange-type-attribute (type spec)
  "Override type attributes by `define-anything-type-attribute'.

The SPEC is like source. The symbol `REST' is replaced
with original attribute value.

 Example: Set `play-sound-file' as default action
   (anything-c-arrange-type-attribute 'file
      '((action (\"Play sound\" . play-sound-file)
         REST ;; Rest of actions (find-file, find-file-other-window, etc...)."
  (add-to-list 'anything-additional-type-attributes
               (cons type
                     (loop with typeattr = (assoc-default
                                            type anything-type-attributes)
                           for (attr . value) in spec
                           if (listp value)
                           collect (cons attr
                                         (anything-c-uniq-list
                                          (loop for v in value
                                                if (eq v 'REST)
                                                append
                                                (assoc-default attr typeattr)
                                                else
                                                collect v)))
                           else
                           collect (cons attr value)))))
(put 'anything-c-arrange-type-attribute 'lisp-indent-function 1)

(defun anything-compile-source--type-customize (source)
  (anything-aif (assoc-default (assoc-default 'type source)
                               anything-additional-type-attributes)
      (append it source)
    source))
(add-to-list 'anything-compile-source-functions
             'anything-compile-source--type-customize t)

;; Plug-in: default-action
(defun anything-compile-source--default-action (source)
  (anything-aif (assoc-default 'default-action source)
      (append `((action ,it ,@(remove it (assoc-default 'action source))))
              source)
    source))
(add-to-list 'anything-compile-source-functions
             'anything-compile-source--default-action t)
(anything-document-attribute 'default-action "default-action plug-in"
                             "Default action.")


;;; Type Attributes
;;
;;
(define-anything-type-attribute 'buffer
    `((action
       ("Switch to buffer" . anything-c-switch-to-buffer)
       ,(and (locate-library "popwin") '("Switch to buffer in popup window" . popwin:popup-buffer))
       ("Switch to buffer other window" . switch-to-buffer-other-window)
       ("Switch to buffer other frame" . switch-to-buffer-other-frame)
       ,(and (locate-library "elscreen") '("Display buffer in Elscreen" . anything-find-buffer-on-elscreen))
       ("Query replace regexp" . anything-c-buffer-query-replace-regexp)
       ("Query replace" . anything-c-buffer-query-replace)
       ("View buffer" . view-buffer)
       ("Display buffer"   . display-buffer)
       ("Grep buffers (C-u grep all buffers)" . anything-c-zgrep-buffers)
       ("Revert buffer(s)" . anything-revert-marked-buffers)
       ("Insert buffer" . insert-buffer)
       ("Kill buffer(s)" . anything-kill-marked-buffers)
       ("Diff with file" . diff-buffer-with-file)
       ("Ediff Marked buffers" . anything-ediff-marked-buffers)
       ("Ediff Merge marked buffers" . (lambda (candidate)
                                         (anything-ediff-marked-buffers candidate t))))
      (persistent-help . "Show this buffer")
      (candidate-transformer anything-c-skip-boring-buffers
                             anything-c-transform-buffer-display-string))
  "Buffer or buffer name.")

(define-anything-type-attribute 'file
    `((action
       ("Find file" . anything-find-many-files)
       ,(and (locate-library "popwin") '("Find file in popup window" . popwin:find-file))
       ("Find file as root" . anything-find-file-as-root)
       ("Find file other window" . find-file-other-window)
       ("Find file other frame" . find-file-other-frame)
       ("Open dired in file's directory" . anything-c-open-dired)
       ("Grep File(s) `C-u recurse'" . anything-find-files-grep)
       ("Zgrep File(s) `C-u Recurse'" . anything-ff-zgrep)
       ("Pdfgrep File(s)" . anything-ff-pdfgrep)
       ("Checksum File" . anything-ff-checksum)
       ("Ediff File" . anything-find-files-ediff-files)
       ("Ediff Merge File" . anything-find-files-ediff-merge-files)
       ("View file" . view-file)
       ("Insert file" . insert-file)
       ("Delete file(s)" . anything-delete-marked-files)
       ("Open file externally (C-u to choose)" . anything-c-open-file-externally)
       ("Open file with default tool" . anything-c-open-file-with-default-tool)
       ("Find file in hex dump" . hexl-find-file))
      (persistent-help . "Show this file")
      (action-transformer anything-c-transform-file-load-el
                          anything-c-transform-file-browse-url)
      (candidate-transformer anything-c-w32-pathname-transformer
                             anything-c-skip-current-file
                             anything-c-skip-boring-files
                             anything-c-shorten-home-path))
  "File name.")

(let ((actions '(("Describe command" . describe-function)
                 ("Add command to kill ring" . anything-c-kill-new)
                 ("Go to command's definition" . find-function)
                 ("Debug on entry" . debug-on-entry)
                 ("Cancel debug on entry" . cancel-debug-on-entry)
                 ("Trace function" . trace-function)
                 ("Trace function (background)" . trace-function-background)
                 ("Untrace function" . untrace-function))))
  (define-anything-type-attribute 'command
      `((action ("Call interactively" . anything-c-call-interactively)
                ,@actions)
        (coerce . anything-c-symbolify)
        (persistent-action . describe-function))
    "Command. (string or symbol)")

  (define-anything-type-attribute 'function
      `((action . ,actions)
        (action-transformer anything-c-transform-function-call-interactively)
        (candidate-transformer anything-c-mark-interactive-functions)
        (coerce . anything-c-symbolify))
    "Function. (string or symbol)"))

(define-anything-type-attribute 'variable
    '((action ("Describe variable" . describe-variable)
       ("Add variable to kill ring" . anything-c-kill-new)
       ("Go to variable's definition" . find-variable)
       ("Set variable" . anything-c-set-variable))
      (coerce . anything-c-symbolify))
  "Variable.")

(define-anything-type-attribute 'sexp
    '((action ("Eval s-expression" . (lambda (c) (eval (read c))))
       ("Add s-expression to kill ring" . kill-new))
      (action-transformer anything-c-transform-sexp-eval-command-sexp))
  "String representing S-Expressions.")

(define-anything-type-attribute 'bookmark
    `((coerce . anything-bookmark-get-bookmark-from-name)
      (action
       ("Jump to bookmark" . anything-c-bookmark-jump)
       ("Jump to BM other window" . bookmark-jump-other-window)
       ("Bookmark edit annotation" . bookmark-edit-annotation)
       ("Bookmark show annotation" . bookmark-show-annotation)
       ("Delete bookmark(s)" . anything-delete-marked-bookmarks)
       ,@(and (locate-library "bookmark-extensions")
              `(("Edit Bookmark" . bmkext-edit-bookmark)))
       ("Rename bookmark" . bookmark-rename)
       ("Relocate bookmark" . bookmark-relocate))
      (keymap . ,anything-c-bookmark-map)
      (mode-line . anything-bookmark-mode-line-string))
  "Bookmark name.")

(define-anything-type-attribute 'line
    '((display-to-real . anything-c-display-to-real-line)
      (action ("Go to Line" . anything-c-action-line-goto)))
  "LINENO:CONTENT string, eg. \"  16:foo\".

Optional `target-file' attribute is a name of target file.

Optional `before-jump-hook' attribute is a function with no
arguments which is called before jumping to position.

Optional `after-jump-hook' attribute is a function with no
arguments which is called after jumping to position.

If `adjust' attribute is specified, searches the line whose
content is CONTENT near the LINENO.

If `recenter' attribute is specified, the line is displayed at
the center of window, otherwise at the top of window.
")

(define-anything-type-attribute 'file-line
    `((filtered-candidate-transformer anything-c-filtered-candidate-transformer-file-line)
      (multiline)
      (action ("Go to" . anything-c-action-file-line-goto)))
  "FILENAME:LINENO:CONTENT string, eg. \"~/.emacs:16:;; comment\".

Optional `default-directory' attribute is a default-directory
FILENAME is interpreted.

Optional `before-jump-hook' attribute is a function with no
arguments which is called before jumping to position.

Optional `after-jump-hook' attribute is a function with no
arguments which is called after jumping to position.

If `adjust' attribute is specified, searches the line whose
content is CONTENT near the LINENO.

If `recenter' attribute is specified, the line is displayed at
the center of window, otherwise at the top of window.
")

(define-anything-type-attribute 'timer
    '((real-to-display . anything-c-timer-real-to-display)
      (action ("Cancel Timer" . cancel-timer)
       ("Describe Function" . (lambda (tm) (describe-function (timer--function tm))))
       ("Find Function" . (lambda (tm) (find-function (timer--function tm)))))
      (persistent-action . (lambda (tm) (describe-function (timer--function tm))))
      (persistent-help . "Describe Function"))
  "Timer.")


;;; Default `anything-sources'
;; Setting `anything-sources' is DEPRECATED, but it seems that newbies
;; tend to invoke M-x anything directly. So I offer default setting.
(setq anything-sources
      '(anything-c-source-buffers-list
        anything-c-source-recentf
        anything-c-source-files-in-current-dir+))


;;; Preconfigured Anything
;;
;;
;;;###autoload
(defun anything-mini ()
  "Preconfigured `anything' lightweight version \(buffer -> recentf\)."
  (interactive)
  (anything-other-buffer '(anything-c-source-buffers-list
                           anything-c-source-recentf
                           anything-c-source-buffer-not-found)
                         "*anything mini*"))
;;;###autoload
(defun anything-for-files ()
  "Preconfigured `anything' for opening files.
ffap -> recentf -> buffer -> bookmark -> file-cache -> files-in-current-dir -> locate."
  (interactive)
  (anything-other-buffer anything-for-files-prefered-list "*anything for files*"))

;;;###autoload
(defun anything-recentf ()
  "Preconfigured `anything' for `recentf'."
  (interactive)
  (anything-other-buffer 'anything-c-source-recentf "*anything recentf*"))

;;;###autoload
(defun anything-info-at-point (arg)
  "Preconfigured `anything' for searching info at point.
With a prefix-arg insert symbol at point."
  (interactive "P")
  (let ((anything-c-google-suggest-default-function
         'anything-c-google-suggest-emacs-lisp))
    (anything :sources '(anything-c-source-info-elisp
                         anything-c-source-info-cl
                         anything-c-source-info-pages
                         anything-c-source-google-suggest)
              :input (and arg (thing-at-point 'symbol))
              :buffer "*anything info*")))

;;;###autoload
(defun anything-show-kill-ring ()
  "Preconfigured `anything' for `kill-ring'.
It is drop-in replacement of `yank-pop'.
You may bind this command to M-y.
First call open the kill-ring browser, next calls move to next line."
  (interactive)
  (anything :sources 'anything-c-source-kill-ring
            :buffer "*anything kill-ring*"))

;;;###autoload
(defun anything-minibuffer-history ()
  "Preconfigured `anything' for `minibuffer-history'."
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (anything-other-buffer 'anything-c-source-minibuffer-history
                           "*anything minibuffer-history*")))

;;;###autoload
(defun anything-gentoo ()
  "Preconfigured `anything' for gentoo linux."
  (interactive)
  (anything-other-buffer '(anything-c-source-gentoo
                           anything-c-source-use-flags)
                         "*anything gentoo*"))

;;;###autoload
(defun anything-imenu ()
  "Preconfigured `anything' for `imenu'."
  (interactive)
  (anything :sources 'anything-c-source-imenu
            :buffer "*anything imenu*"))

;;;###autoload
(defun anything-google-suggest ()
  "Preconfigured `anything' for google search with google suggest."
  (interactive)
  (anything-other-buffer 'anything-c-source-google-suggest "*anything google*"))

;;;###autoload
(defun anything-yahoo-suggest ()
  "Preconfigured `anything' for Yahoo searching with Yahoo suggest."
  (interactive)
  (anything-other-buffer 'anything-c-source-yahoo-suggest "*anything yahoo*"))

;;; Converted from anything-show-*-only
;;;###autoload
(defun anything-for-buffers ()
  "Preconfigured `anything' for buffers."
  (interactive)
  (anything-other-buffer 'anything-c-source-buffers "*anything for buffers*"))

;;;###autoload
(defun anything-buffers-list ()
  "Preconfigured `anything' to list buffers.
It is an enhanced version of `anything-for-buffers'."
  (interactive)
  (anything :sources '(anything-c-source-buffers-list
                       anything-c-source-buffer-not-found)
            :buffer "*anything buffers*" :keymap anything-c-buffer-map))

(defalias 'anything-buffers+ 'anything-buffers-list
  "Preconfigured `anything' to list buffers.
It is an alias of `anything-buffers-list'.")

;;;###autoload
(defun anything-bbdb ()
  "Preconfigured `anything' for BBDB.

Needs BBDB.

http://bbdb.sourceforge.net/"
  (interactive)
  (anything-other-buffer 'anything-c-source-bbdb "*anything bbdb*"))

;;;###autoload
(defun anything-locate (arg)
  "Preconfigured `anything' for Locate.
Note: you can add locate options after entering pattern.
See 'man locate' for valid options.

You can specify a specific database with prefix argument ARG \(C-u\).
Many databases can be used: navigate and mark them.
See also `anything-locate-with-db'.

To create a user specific db, use
\"updatedb -l 0 -o db_path -U directory\".
Where db_path is a filename matched by
`anything-locate-db-file-regexp'."
  (interactive "P")
  (setq anything-ff-default-directory default-directory)
  (anything-locate-1 arg))

;;;###autoload
(defun anything-w3m-bookmarks ()
  "Preconfigured `anything' for w3m bookmark.

Needs w3m and emacs-w3m.

http://w3m.sourceforge.net/
http://emacs-w3m.namazu.org/"
  (interactive)
  (anything-other-buffer 'anything-c-source-w3m-bookmarks
                         "*anything w3m bookmarks*"))

;;;###autoload
(defun anything-firefox-bookmarks ()
  "Preconfigured `anything' for firefox bookmark.
You will have to enable html bookmarks in firefox:
open about:config in firefox and double click on this line to enable value \
to true:

user_pref(\"browser.bookmarks.autoExportHTML\", false);

You should have now:

user_pref(\"browser.bookmarks.autoExportHTML\", true);

After closing firefox, you will be able to browse you bookmarks.
"
  (interactive)
  (anything-other-buffer 'anything-c-source-firefox-bookmarks
                         "*Anything Firefox*"))

;;;###autoload
(defun anything-colors ()
  "Preconfigured `anything' for color."
  (interactive)
  (anything-other-buffer
   '(anything-c-source-colors anything-c-source-customize-face)
   "*anything colors*"))

;;;###autoload
(defun anything-bookmarks ()
  "Preconfigured `anything' for bookmarks."
  (interactive)
  (anything-other-buffer 'anything-c-source-bookmarks "*anything bookmarks*"))

;;;###autoload
(defun anything-c-pp-bookmarks ()
  "Preconfigured `anything' for bookmarks (pretty-printed)."
  (interactive)
  (anything-other-buffer '(anything-c-source-bookmarks-local
                           anything-c-source-bookmarks-su
                           anything-c-source-bookmarks-ssh)
                         "*anything pp bookmarks*"))

;;;###autoload
(defun anything-c-insert-latex-math ()
  "Preconfigured anything for latex math symbols completion."
  (interactive)
  (anything-other-buffer 'anything-c-source-latex-math "*anything latex*"))

;;;###autoload
(defun anything-register ()
  "Preconfigured `anything' for Emacs registers."
  (interactive)
  (anything-other-buffer 'anything-c-source-register "*anything register*"))

;;;###autoload
(defun anything-man-woman ()
  "Preconfigured `anything' for Man and Woman pages."
  (interactive)
  (anything-other-buffer 'anything-c-source-man-pages "*Anything man woman*"))

;;;###autoload
(defun anything-org-keywords ()
  "Preconfigured `anything' for org keywords."
  (interactive)
  (anything-other-buffer 'anything-c-source-org-keywords "*org keywords*"))

;;;###autoload
(defun anything-emms ()
  "Preconfigured `anything' for emms sources."
  (interactive)
  (anything :sources '(anything-c-source-emms-streams
                       anything-c-source-emms-files
                       anything-c-source-emms-dired)
            :buffer "*Anything Emms*"))

;;;###autoload
(defun anything-eev-anchors ()
  "Preconfigured `anything' for eev anchors."
  (interactive)
  (anything-other-buffer 'anything-c-source-eev-anchor "*Anything eev anchors*"))

;;;###autoload
(defun anything-bm-list ()
  "Preconfigured `anything' for visible bookmarks.

Needs bm.el

http://cvs.savannah.gnu.org/viewvc/*checkout*/bm/bm/bm.el"
  (interactive)
  (let ((anything-outline-using t))
    (anything-other-buffer 'anything-c-source-bm "*anything bm list*")))

;;;###autoload
(defun anything-timers ()
  "Preconfigured `anything' for timers."
  (interactive)
  (anything-other-buffer '(anything-c-source-absolute-time-timers
                           anything-c-source-idle-time-timers)
                         "*anything timers*"))

;;;###autoload
(defun anything-list-emacs-process ()
  "Preconfigured `anything' for emacs process."
  (interactive)
  (anything-other-buffer 'anything-c-source-emacs-process "*anything process*"))

;;;###autoload
(defun anything-occur ()
  "Preconfigured Anything for Occur source.
If region is active, search only in region,
otherwise search in whole buffer."
  (interactive)
  (let ((anything-compile-source-functions
         ;; rule out anything-match-plugin because the input is one regexp.
         (delq 'anything-compile-source--match-plugin
               (copy-sequence anything-compile-source-functions))))
    (anything :sources 'anything-c-source-occur
              :buffer "*Anything Occur*"
              :history 'anything-c-grep-history)))

;;;###autoload
(defun anything-browse-code ()
  "Preconfigured anything to browse code."
  (interactive)
  (anything :sources 'anything-c-source-browse-code
            :buffer "*anything browse code*"
            :default (thing-at-point 'symbol)))

;;;###autoload
(defun anything-org-headlines ()
  "Preconfigured anything to show org headlines."
  (interactive)
  (anything-other-buffer 'anything-c-source-org-headline "*org headlines*"))

;;;###autoload
(defun anything-regexp ()
  "Preconfigured anything to build regexps.
`query-replace-regexp' can be run from there against found regexp."
  (interactive)
  (save-restriction
    (let ((anything-compile-source-functions
           ;; rule out anything-match-plugin because the input is one regexp.
           (delq 'anything-compile-source--match-plugin
                 (copy-sequence anything-compile-source-functions))))
      (when (and (anything-region-active-p)
                 ;; Don't narrow to region if buffer is already narrowed.
                 (not (anything-current-buffer-narrowed-p)))
        (narrow-to-region (region-beginning) (region-end)))
      (anything :sources anything-c-source-regexp
                :buffer "*anything regexp*"
                :prompt "Regexp: "
                :history 'anything-build-regexp-history))))

;;;###autoload
(defun anything-c-copy-files-async ()
  "Preconfigured anything to copy file list FLIST to DEST asynchronously."
  (interactive)
  (let* ((flist (anything-c-read-file-name
                 "Copy File async: "
                 :marked-candidates t))
         (dest  (anything-c-read-file-name
                 "Copy File async To: "
                 :preselect (car flist)
                 :initial-input (car anything-ff-history)
                 :history (anything-find-files-history :comp-read nil))))
    (anything-c-copy-async-with-log flist dest)))

;;;###autoload
(defun anything-find-files (arg)
  "Preconfigured `anything' for anything implementation of `find-file'.
Called with a prefix arg show history if some.
Don't call it from programs, use `anything-find-files-1' instead.
This is the starting point for nearly all actions you can do on files."
  (interactive "P")
  (let ((any-input (if (and arg anything-ff-history)
                       (anything-find-files-history)
                       (anything-find-files-initial-input)))
        (presel    (buffer-file-name (current-buffer))))
    (when (and (eq major-mode 'org-agenda-mode)
               org-directory
               (not any-input))
      (setq any-input (expand-file-name org-directory)))
    (set-text-properties 0 (length any-input) nil any-input)
    (if any-input
        (anything-find-files-1 any-input)
        (setq any-input (expand-file-name (anything-c-current-directory)))
        (anything-find-files-1
         any-input (if anything-ff-transformer-show-only-basename
                       (and presel (anything-c-basename presel))
                       presel)))))

;;;###autoload
(defun anything-write-file ()
  "Preconfigured `anything' providing completion for `write-file'."
  (interactive)
  (let ((anything-mp-highlight-delay nil))
    (anything :sources 'anything-c-source-write-file
              :input (expand-file-name default-directory)
              :prompt "Write buffer to file: "
              :buffer "*Anything write file*")))

;;;###autoload
(defun anything-insert-file ()
  "Preconfigured `anything' providing completion for `insert-file'."
  (interactive)
  (let ((anything-mp-highlight-delay nil))
    (anything :sources 'anything-c-source-insert-file
              :input (expand-file-name default-directory)
              :prompt "Insert file: "
              :buffer "*Anything insert file*")))

;;;###autoload
(defun anything-dired-rename-file ()
  "Preconfigured `anything' to rename files from dired."
  (interactive)
  (anything-dired-do-action-on-file :action 'rename))

;;;###autoload
(defun anything-dired-copy-file ()
  "Preconfigured `anything' to copy files from dired."
  (interactive)
  (anything-dired-do-action-on-file :action 'copy))

;;;###autoload
(defun anything-dired-symlink-file ()
  "Preconfigured `anything' to symlink files from dired."
  (interactive)
  (anything-dired-do-action-on-file :action 'symlink))

;;;###autoload
(defun anything-dired-hardlink-file ()
  "Preconfigured `anything' to hardlink files from dired."
  (interactive)
  (anything-dired-do-action-on-file :action 'hardlink))

;;;###autoload
(defun anything-do-grep ()
  "Preconfigured anything for grep.
Contrarily to Emacs `grep' no default directory is given, but
the full path of candidates in ONLY.
That allow to grep different files not only in `default-directory' but anywhere
by marking them (C-<SPACE>). If one or more directory is selected
grep will search in all files of these directories.
You can use also wildcard in the base name of candidate.
If a prefix arg is given use the -r option of grep.
The prefix arg can be passed before or after start.
See also `anything-do-grep-1'."
  (interactive)
  (let ((only    (anything-c-read-file-name
                  "Search in file(s): "
                  :marked-candidates t
                  :preselect (or (dired-get-filename nil t)
                                 (buffer-file-name (current-buffer)))))
        (prefarg (or current-prefix-arg anything-current-prefix-arg)))
    (anything-do-grep-1 only prefarg)))

;;;###autoload
(defun anything-do-zgrep ()
  "Preconfigured anything for zgrep."
  (interactive)
  (let ((prefarg (or current-prefix-arg anything-current-prefix-arg))
        (ls (anything-c-read-file-name
             "Search in file(s): "
             :marked-candidates t
             :preselect (or (dired-get-filename nil t)
                            (buffer-file-name (current-buffer))))))
    (anything-ff-zgrep-1 ls prefarg)))

;;;###autoload
(defun anything-do-pdfgrep ()
  "Preconfigured anything for pdfgrep."
  (interactive)
  (let ((only (anything-c-read-file-name
               "Search in file(s): "
               :marked-candidates t
               :test #'(lambda (file)
                         (or (string= (file-name-extension file) "pdf")
                             (string= (file-name-extension file) "PDF")
                             (file-directory-p file)))
               :preselect (or (dired-get-filename nil t)
                              (buffer-file-name (current-buffer)))))
        (anything-c-grep-default-function 'anything-c-pdfgrep-init))
    (anything-do-pdfgrep-1 only)))

;;;###autoload
(defun anything-c-etags-select (arg)
  "Preconfigured anything for etags.
Called with one prefix arg use symbol at point as initial input.
Called with two prefix arg reinitialize cache.
If tag file have been modified reinitialize cache."
  (interactive "P")
  (let ((tag  (anything-c-etags-get-tag-file))
        (init (and (equal arg '(4)) (thing-at-point 'symbol)))
        (anything-quit-if-no-candidate t)
        (anything-execute-action-at-once-if-one t)
        (anything-compile-source-functions
         (if anything-c-etags-use-regexp-search
             ;; rule out anything-match-plugin because the input is one regexp.
             (delq 'anything-compile-source--match-plugin
                   (copy-sequence anything-compile-source-functions))
             anything-compile-source-functions)))
    (when (or (equal arg '(16))
              (and anything-c-etags-mtime-alist
                   (anything-c-etags-file-modified-p tag)))
      (remhash tag anything-c-etags-cache))
    (if (and tag (file-exists-p tag))
        (anything :sources 'anything-c-source-etags-select
                  :keymap anything-c-etags-map
                  :input init
                  :buffer "*anything etags*")
        (message "Error: No tag file found, please create one with etags shell command."))))

;;;###autoload
(defun anything-filelist ()
  "Preconfigured `anything' to open files instantly.

See `anything-c-filelist-file-name' docstring for usage."
  (interactive)
  (anything-other-buffer 'anything-c-source-filelist "*anything file list*"))

;;;###autoload
(defun anything-filelist+ ()
  "Preconfigured `anything' to open files/buffers/bookmarks instantly.

This is a replacement for `anything-for-files'.
See `anything-c-filelist-file-name' docstring for usage."
  (interactive)
  (anything-other-buffer
   '(anything-c-source-ffap-line
     anything-c-source-ffap-guesser
     anything-c-source-buffers-list
     anything-c-source-recentf
     anything-c-source-bookmarks
     anything-c-source-file-cache
     anything-c-source-filelist)
   "*anything file list*"))

;;;###autoload
(defun anything-M-x ()
  "Preconfigured `anything' for Emacs commands.
It is `anything' replacement of regular `M-x' `execute-extended-command'."
  (interactive)
  (let* (in-help
         help-cand
         special-display-buffer-names
         special-display-regexps
         anything-persistent-action-use-special-display
         (history (loop with hist
                        for i in extended-command-history
                        for com = (intern i)
                        when (fboundp com)
                        collect i into hist finally return hist)))
    (flet ((pers-help (candidate)
             (let ((hbuf (get-buffer (help-buffer))))
               (if (and in-help (string= candidate help-cand))
                   (progn
                     ;; When M-x is started from a help buffer,
                     ;; Don't kill it as it is anything-current-buffer.
                     (unless (equal hbuf anything-current-buffer)
                       (kill-buffer hbuf))
                     (setq in-help nil))
                   ;; Be sure anything-current-buffer
                   ;; have not a dedicated window.
                   (set-window-dedicated-p
                    (get-buffer-window anything-current-buffer) nil)
                   (describe-function (intern candidate))
                   (message nil) ; Erase the new stupid message Type "q"[...]
                   (setq in-help t))
               (setq help-cand candidate))))
      (let* ((command (anything-comp-read
                       "M-x " obarray
                       :test 'commandp
                       :requires-pattern anything-M-x-requires-pattern
                       :name "Emacs Commands"
                       :buffer "*anything M-x*"
                       :persistent-action 'pers-help
                       :persistent-help "Describe this command"
                       :history history
                       :must-match t
                       :candidates-in-buffer t
                       :fc-transformer 'anything-M-x-transformer))
             (sym-com (intern command)))
        (unless current-prefix-arg
          (setq current-prefix-arg anything-current-prefix-arg))
        ;; Avoid having `this-command' set to *exit-minibuffer.
        (setq this-command sym-com)
        (call-interactively sym-com)
        (setq extended-command-history
              (cons command (delete command history)))))))

;;;###autoload
(defun anything-manage-advice ()
  "Preconfigured `anything' to disable/enable function advices."
  (interactive)
  (anything-other-buffer 'anything-c-source-advice "*anything advice*"))

;;;###autoload
(defun anything-bookmark-ext ()
  "Preconfigured `anything' for bookmark-extensions sources.
Needs bookmark-ext.el:
<http://mercurial.intuxication.org/hg/emacs-bookmark-extension>.
Contain also `anything-c-source-google-suggest'."
  (interactive)
  (anything
   :sources
   '(anything-c-source-bookmark-files&dirs
     anything-c-source-bookmark-w3m
     anything-c-source-google-suggest
     anything-c-source-bmkext-addressbook
     anything-c-source-bookmark-gnus
     anything-c-source-bookmark-info
     anything-c-source-bookmark-man
     anything-c-source-bookmark-images
     anything-c-source-bookmark-su-files&dirs
     anything-c-source-bookmark-ssh-files&dirs)
   :prompt "SearchBookmark: "
   :buffer "*anything bmkext*"))

;;;###autoload
(defun anything-simple-call-tree ()
  "Preconfigured `anything' for simple-call-tree. List function relationships.

Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el"
  (interactive)
  (anything-other-buffer
   '(anything-c-source-simple-call-tree-functions-callers
     anything-c-source-simple-call-tree-callers-functions)
   "*anything simple-call-tree*"))

;;;###autoload
(defun anything-mark-ring ()
  "Preconfigured `anything' for `anything-c-source-mark-ring'."
  (interactive)
  (anything :sources 'anything-c-source-mark-ring))

;;;###autoload
(defun anything-global-mark-ring ()
  "Preconfigured `anything' for `anything-c-source-global-mark-ring'."
  (interactive)
  (anything :sources 'anything-c-source-global-mark-ring))

;;;###autoload
(defun anything-all-mark-rings ()
  "Preconfigured `anything' for `anything-c-source-global-mark-ring' and \
`anything-c-source-mark-ring'."
  (interactive)
  (anything :sources '(anything-c-source-mark-ring
                       anything-c-source-global-mark-ring)))

;;;###autoload
(defun anything-yaoddmuse-emacswiki-edit-or-view ()
  "Preconfigured `anything' to edit or view EmacsWiki page.

Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el"
  (interactive)
  (anything :sources 'anything-c-source-yaoddmuse-emacswiki-edit-or-view))

;;;###autoload
(defun anything-yaoddmuse-emacswiki-post-library ()
  "Preconfigured `anything' to post library to EmacsWiki.

Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el"
  (interactive)
  (anything :sources 'anything-c-source-yaoddmuse-emacswiki-post-library))

;;;###autoload
(defun anything-eval-expression (arg)
  "Preconfigured anything for `anything-c-source-evaluation-result'."
  (interactive "P")
  (anything :sources 'anything-c-source-evaluation-result
            :input (when arg (thing-at-point 'sexp))
            :buffer "*anything eval*"
            :history 'anything-eval-expression-input-history
            :keymap anything-eval-expression-map))

;;;###autoload
(defun anything-eval-expression-with-eldoc ()
  "Preconfigured anything for `anything-c-source-evaluation-result' with `eldoc' support. "
  (interactive)
  (declare (special eldoc-idle-delay))
  (let ((timer (run-with-idle-timer eldoc-idle-delay
                                    'repeat 'anything-eldoc-show-in-eval))
        (minibuffer-completing-symbol t) ; Enable lisp completion.
        (completion-cycle-threshold t))  ; Always cycle, no pesty completion buffer (emacs24 only).
    (unwind-protect
         (minibuffer-with-setup-hook
             'anything-eldoc-store-minibuffer
           (call-interactively 'anything-eval-expression))
      (and timer (cancel-timer timer))
      (setq anything-eldoc-active-minibuffers-list
            (cdr anything-eldoc-active-minibuffers-list)))))

;;;###autoload
(defun anything-calcul-expression ()
  "Preconfigured anything for `anything-c-source-calculation-result'."
  (interactive)
  (anything-other-buffer 'anything-c-source-calculation-result "*anything calcul*"))

;;;###autoload
(defun anything-surfraw (pattern engine)
  "Preconfigured `anything' to search PATTERN with search ENGINE."
  (interactive (list (read-string "SearchFor: "
                                  nil 'anything-surfraw-input-history)
                     (anything-comp-read
                      "Engine: "
                      (anything-c-build-elvi-list)
                      :must-match t
                      :name "Surfraw Search Engines"
                      :history anything-surfraw-engines-history)))
  (let* ((engine-nodesc (car (split-string engine)))
         (url (with-temp-buffer
                (apply 'call-process "surfraw" nil t nil
		       ;;JAVE
                       (append  (list engine-nodesc "-p") (split-string pattern)))
                (replace-regexp-in-string
                 "\n" "" (buffer-string))))
         (browse-url-browser-function (or anything-surfraw-default-browser-function
                                          browse-url-browser-function)))
    (if (string= engine-nodesc "W")
        (anything-c-browse-url anything-c-home-url)
        (anything-c-browse-url url)
        (setq anything-surfraw-engines-history
              (cons engine (delete engine anything-surfraw-engines-history))))))

;;;###autoload
(defun anything-call-source ()
  "Preconfigured `anything' to call anything source."
  (interactive)
  (anything :sources 'anything-c-source-call-source
            :buffer anything-source-select-buffer))

;;;###autoload
(defun anything-execute-anything-command ()
  "Preconfigured `anything' to execute preconfigured `anything'."
  (interactive)
  (anything-other-buffer 'anything-c-source-anything-commands
                         "*anything commands*"))

;;;###autoload
(defun anything-create (&optional string initial-input)
  "Preconfigured `anything' to do many create actions from STRING.
See also `anything-create--actions'."
  (interactive)
  (setq string (or string (read-string "Create Anything: " initial-input)))
  (anything :sources '(((name . "Anything Create")
                        (header-name . (lambda (_) (format "Action for \"%s\"" string)))
                        (candidates . anything-create--actions)
                        (candidate-number-limit)
                        (action . (lambda (func) (funcall func string)))))))

;;;###autoload
(defun anything-top ()
  "Preconfigured `anything' for top command."
  (interactive)
  (let ((anything-samewindow t)
        (anything-enable-shortcuts)
        (anything-display-function 'anything-default-display-buffer)
        (anything-candidate-number-limit 9999))
    (save-window-excursion
      (delete-other-windows)
      (anything-other-buffer 'anything-c-source-top "*anything top*"))))

;;;###autoload
(defun anything-select-xfont ()
  "Preconfigured `anything' to select Xfont."
  (interactive)
  (anything-other-buffer 'anything-c-source-xfonts "*anything select* xfont"))

;;;###autoload
(defun anything-world-time ()
  "Preconfigured `anything' to show world time."
  (interactive)
  (anything-other-buffer 'anything-c-source-time-world "*anything world time*"))

;;;###autoload
(defun anything-apt (arg)
  "Preconfigured `anything' : frontend of APT package manager.
With a prefix arg reload cache."
  (interactive "P")
  (let ((query (read-string "Search Package: " nil 'anything-c-apt-input-history)))
    (when arg (anything-c-apt-refresh))
    (anything :sources 'anything-c-source-apt
              :prompt "Search Package: "
              :input query
              :history 'anything-c-apt-input-history)))

;;;###autoload
(defun anything-esh-pcomplete ()
  "Preconfigured anything to provide anything completion in eshell."
  (interactive)
  (let* ((anything-quit-if-no-candidate t)
         (anything-execute-action-at-once-if-one t)
         (target (thing-at-point 'symbol))
         (end (point))
         (beg (or (and target (- end (length target)))
                  ;; Nothing at point.
                  (progn (insert " ") (point)))))
    (setq anything-ec-target (or target " "))
    (with-anything-show-completion beg end
      (anything :sources 'anything-c-source-esh
                :buffer "*anything pcomplete*"
                :input (anything-ff-set-pattern ; Handle tramp filenames.
                        (car (last (ignore-errors ; Needed in lisp symbols completion.
                                     (pcomplete-parse-arguments)))))))))

;;;###autoload
(defun anything-eshell-history ()
  "Preconfigured anything for eshell history."
  (interactive)
  (let* ((end (point))
         (beg (save-excursion (eshell-bol) (point)))
         (input (buffer-substring beg end))
         flag-empty)
    (when (eq beg end)
      (insert " ")
      (setq flag-empty t)
      (setq end (point)))
    (unwind-protect
         (with-anything-show-completion beg end
           (anything :sources 'anything-c-source-eshell-history
                     :buffer "*Eshell history*"
                     :input input))
      (when (and flag-empty
                 (looking-back " "))
        (delete-char -1)))))

;;;###autoload
(defun anything-c-run-external-command (program)
  "Preconfigured `anything' to run External PROGRAM asyncronously from Emacs.
If program is already running exit with error.
You can set your own list of commands with
`anything-c-external-commands-list'."
  (interactive (list
                (anything-comp-read
                 "RunProgram: "
                 (anything-c-external-commands-list-1 'sort)
                 :must-match t
                 :name "External Commands"
                 :history anything-external-command-history)))
  (anything-run-or-raise program)
  (setq anything-external-command-history
        (cons program (delete program
                              (loop for i in anything-external-command-history
                                    when (executable-find i) collect i)))))

;;;###autoload
(defun anything-ratpoison-commands ()
  "Preconfigured `anything' to execute ratpoison commands."
  (interactive)
  (anything-other-buffer 'anything-c-source-ratpoison-commands
                         "*anything ratpoison commands*"))

;;;###autoload
(defun anything-ucs ()
  "Preconfigured anything for `ucs-names' math symbols."
  (interactive)
  (anything :sources 'anything-c-source-ucs
            :keymap  anything-c-ucs-map))

;;;###autoload
(defun anything-c-apropos ()
  "Preconfigured anything to describe commands, functions, variables and faces."
  (interactive)
  (let ((default (thing-at-point 'symbol)))
    (anything :sources
              `(((name . "Commands")
                 (init . (lambda ()
                           (anything-c-apropos-init 'commandp ,default)))
                 (persistent-action . anything-lisp-completion-persistent-action)
                 (persistent-help . "Show brief doc in mode-line")
                 (candidates-in-buffer)
                 (action . (lambda (candidate)
                             (describe-function (intern candidate)))))
                ((name . "Functions")
                 (init . (lambda ()
                           (anything-c-apropos-init #'(lambda (x) (and (fboundp x)
                                                                       (not (commandp x))))
                                                    ,default)))
                 (persistent-action . anything-lisp-completion-persistent-action)
                 (persistent-help . "Show brief doc in mode-line")
                 (candidates-in-buffer)
                 (action . (lambda (candidate)
                             (describe-function (intern candidate)))))
                ((name . "Variables")
                 (init . (lambda ()
                           (anything-c-apropos-init 'boundp ,default)))
                 (persistent-action . anything-lisp-completion-persistent-action)
                 (persistent-help . "Show brief doc in mode-line")
                 (candidates-in-buffer)
                 (action . (lambda (candidate)
                             (describe-variable (intern candidate)))))
                ((name . "Faces")
                 (init . (lambda ()
                           (anything-c-apropos-init 'facep ,default)))
                 (persistent-action . anything-lisp-completion-persistent-action)
                 (persistent-help . "Show brief doc in mode-line")
                 (candidates-in-buffer)
                 (filtered-candidate-transformer . (lambda (candidates source)
                                                     (loop for c in candidates
                                                           collect (propertize c 'face (intern c)))))
                 (action . (lambda (candidate)
                             (describe-face (intern candidate)))))
                ((name . "Anything attributes")
                 (candidates . (lambda ()
                                 (mapcar 'symbol-name anything-additional-attributes)))
                 (action . (lambda (candidate)
                             (with-output-to-temp-buffer "*Help*"
                               (princ (get (intern candidate) 'anything-attrdoc))))))))))

;;;###autoload
(defun anything-xrandr-set ()
  (interactive)
  (anything :sources 'anything-c-source-xrandr-change-resolution
            :buffer "*anything xrandr*"))


;;; Unit tests are now in ../developer-tools/unit-test-anything-config.el.

(provide 'anything-config)

;; Local Variables:
;; coding: utf-8
;; End:

;;; anything-config.el ends here
