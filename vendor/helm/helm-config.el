;;; helm-config.el --- Applications libary for `helm.el'

;; Filename: helm-config.el

;; Description: Applications libary for `helm.el'

;; Original Author: Tassilo Horn <tassilo@member.fsf.org>

;; This is a fork of original `anything-config.el' created by
;; Tassilo Horn <tassilo@member.fsf.org>.

;; Maintainers: Thierry Volpiatto <thierry.volpiatto@gmail.com>
;;              Le Wang

;; Copyright (C) 2007 ~ 2011, Tassilo Horn, all rights reserved.
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Copyright (C) 2009 ~ 2012, rubikitch, all rights reserved.
;; Copyright (C) 2009 ~ 2012, Thierry Volpiatto, all rights reserved.

;; Created: 2012-03-15 12:29:23

;; X-URL: <https://github.com/emacs-helm/helm>

;; MailingList: <https://groups.google.com/forum/?hl=en&fromgroups#!forum/emacs-anything>

;; Keywords: helm, helm-config

;; Compatibility: GNU Emacs 22 ~ 24

;; Dependencies: `helm.el', `helm-match-plugin.el'.

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
;; Predefined configurations for `helm.el'
;;
;; For quick start, try `helm-for-files' to open files.
;;
;; To configure helm you should define helm command
;; with your favorite sources, like below:
;;
;; (defun my-helm ()
;;   (interactive)
;;   (helm-other-buffer
;;    '(helm-c-source-buffers
;;      helm-c-source-file-name-history
;;      helm-c-source-info-pages
;;      helm-c-source-info-elisp
;;      helm-c-source-man-pages
;;      helm-c-source-locate
;;      helm-c-source-emacs-commands)
;;    " *my-helm*"))
;;
;; Then type M-x my-helm to use sources.
;;
;; Defining own command is better than setup `helm-sources'
;; directly, because you can define multiple helm commands with
;; different sources. Each helm command should have own helm
;; buffer, because M-x helm-resume revives helm command.

;; NOTE: What you find on Emacswiki is mostly deprecated and not maintained,
;;       don't complain if you use such code or configuration and something
;;       doesn't work.


;;; Autodoc documentation:
;;  ---------------------

;;  * Commands defined here are:
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "helm-" :docstring t)
;; `helm-configuration'
;; Customize `helm'.
;; `helm-c-buffer-help'
;; Help command for helm buffers.
;; `helm-ff-help'
;; Help command for `helm-find-files'.
;; `helm-read-file-name-help'
;; Not documented.
;; `helm-generic-file-help'
;; Not documented.
;; `helm-grep-help'
;; Not documented.
;; `helm-pdfgrep-help'
;; Not documented.
;; `helm-etags-help'
;; The help function for etags.
;; `helm-c-ucs-help'
;; Help command for `helm-ucs'.
;; `helm-c-bookmark-help'
;; Help command for bookmarks.
;; `helm-show-this-source-only'
;; Show all candidates of this source.
;; `helm-test-sources'
;; List all helm sources for test.
;; `helm-select-source'
;; [OBSOLETE] Select source.
;; `helm-insert-buffer-name'
;; Insert buffer name.
;; `helm-quit-and-find-file'
;; Drop into `helm-find-files' from `helm'.
;; `helm-mark-all'
;; Mark all visible unmarked candidates in current source.
;; `helm-unmark-all'
;; Unmark all candidates in all sources of current helm session.
;; `helm-toggle-all-marks'
;; Toggle all marks.
;; `helm-buffer-diff-persistent'
;; Toggle diff buffer without quitting helm.
;; `helm-buffer-revert-persistent'
;; Revert buffer without quitting helm.
;; `helm-buffer-save-persistent'
;; Save buffer without quitting helm.
;; `helm-buffer-run-kill-buffers'
;; Run kill buffer action from `helm-c-source-buffers-list'.
;; `helm-buffer-run-grep'
;; Run Grep action from `helm-c-source-buffers-list'.
;; `helm-buffer-run-zgrep'
;; Run Grep action from `helm-c-source-buffers-list'.
;; `helm-buffer-run-query-replace-regexp'
;; Run Query replace regexp action from `helm-c-source-buffers-list'.
;; `helm-buffer-run-query-replace'
;; Run Query replace action from `helm-c-source-buffers-list'.
;; `helm-buffer-switch-other-window'
;; Run switch to other window action from `helm-c-source-buffers-list'.
;; `helm-buffer-switch-other-frame'
;; Run switch to other frame action from `helm-c-source-buffers-list'.
;; `helm-buffer-switch-to-elscreen'
;; Run switch to elscreen  action from `helm-c-source-buffers-list'.
;; `helm-buffer-run-ediff'
;; Run ediff action from `helm-c-source-buffers-list'.
;; `helm-buffer-run-ediff-merge'
;; Run ediff action from `helm-c-source-buffers-list'.
;; `helm-ff-run-toggle-auto-update'
;; Not documented.
;; `helm-ff-run-switch-to-history'
;; Run Switch to history action from `helm-c-source-find-files'.
;; `helm-ff-run-grep'
;; Run Grep action from `helm-c-source-find-files'.
;; `helm-ff-run-pdfgrep'
;; Run Pdfgrep action from `helm-c-source-find-files'.
;; `helm-ff-run-zgrep'
;; Run Grep action from `helm-c-source-find-files'.
;; `helm-ff-run-copy-file'
;; Run Copy file action from `helm-c-source-find-files'.
;; `helm-ff-run-rename-file'
;; Run Rename file action from `helm-c-source-find-files'.
;; `helm-ff-run-byte-compile-file'
;; Run Byte compile file action from `helm-c-source-find-files'.
;; `helm-ff-run-load-file'
;; Run Load file action from `helm-c-source-find-files'.
;; `helm-ff-run-eshell-command-on-file'
;; Run eshell command on file action from `helm-c-source-find-files'.
;; `helm-ff-run-ediff-file'
;; Run Ediff file action from `helm-c-source-find-files'.
;; `helm-ff-run-ediff-merge-file'
;; Run Ediff merge file action from `helm-c-source-find-files'.
;; `helm-ff-run-symlink-file'
;; Run Symlink file action from `helm-c-source-find-files'.
;; `helm-ff-run-hardlink-file'
;; Run Hardlink file action from `helm-c-source-find-files'.
;; `helm-ff-run-delete-file'
;; Run Delete file action from `helm-c-source-find-files'.
;; `helm-ff-run-complete-fn-at-point'
;; Run complete file name action from `helm-c-source-find-files'.
;; `helm-ff-run-switch-to-eshell'
;; Run switch to eshell action from `helm-c-source-find-files'.
;; `helm-ff-run-switch-other-window'
;; Run switch to other window action from `helm-c-source-find-files'.
;; `helm-ff-run-switch-other-frame'
;; Run switch to other frame action from `helm-c-source-find-files'.
;; `helm-ff-run-open-file-externally'
;; Run open file externally command action from `helm-c-source-find-files'.
;; `helm-ff-run-locate'
;; Run locate action from `helm-c-source-find-files'.
;; `helm-ff-run-gnus-attach-files'
;; Run gnus attach files command action from `helm-c-source-find-files'.
;; `helm-ff-run-etags'
;; Run Etags command action from `helm-c-source-find-files'.
;; `helm-ff-run-print-file'
;; Run Print file action from `helm-c-source-find-files'.
;; `helm-ff-run-toggle-basename'
;; Not documented.
;; `helm-find-files-down-one-level'
;; Go down one level like unix command `cd ..'.
;; `helm-ff-properties-persistent'
;; Show properties without quitting helm.
;; `helm-ff-persistent-delete'
;; Delete current candidate without quitting.
;; `helm-ff-run-kill-buffer-persistent'
;; Execute `helm-ff-kill-buffer-fname' whitout quitting.
;; `helm-ff-rotate-left-persistent'
;; Rotate image left without quitting helm.
;; `helm-ff-rotate-right-persistent'
;; Rotate image right without quitting helm.
;; `helm-c-goto-precedent-file'
;; Go to precedent file in helm grep/etags buffers.
;; `helm-c-goto-next-file'
;; Go to precedent file in helm grep/etags buffers.
;; `helm-c-grep-run-persistent-action'
;; Run grep persistent action from `helm-do-grep-1'.
;; `helm-c-grep-run-default-action'
;; Run grep default action from `helm-do-grep-1'.
;; `helm-c-grep-run-other-window-action'
;; Run grep goto other window action from `helm-do-grep-1'.
;; `helm-c-grep-run-save-buffer'
;; Run grep save results action from `helm-do-grep-1'.
;; `helm-yank-text-at-point'
;; Yank text at point in minibuffer.
;; `helm-c-bookmark-run-jump-other-window'
;; Jump to bookmark from keyboard.
;; `helm-c-bookmark-run-delete'
;; Delete bookmark from keyboard.
;; `helm-c-bmkext-run-edit'
;; Run `bmkext-edit-bookmark' from keyboard.
;; `helm-yaoddmuse-cache-pages'
;; Fetch the list of files on emacswiki and create cache file.
;; `helm-eval-new-line-and-indent'
;; Not documented.
;; `helm-call-source-from-helm'
;; Call helm source within `helm' session.
;; `helm-create-from-helm'
;; Run `helm-create' from `helm' as a fallback.
;; `helm-c-ucs-persistent-insert'
;; Not documented.
;; `helm-c-ucs-persistent-forward'
;; Not documented.
;; `helm-c-ucs-persistent-backward'
;; Not documented.
;; `helm-c-ucs-persistent-delete'
;; Not documented.
;; `helm-lisp-completion-at-point'
;; Helm lisp symbol completion at point.
;; `helm-c-complete-file-name-at-point'
;; Complete file name at point.
;; `helm-lisp-completion-at-point-or-indent'
;; First call indent and second call complete lisp symbol.
;; `helm-lisp-completion-or-file-name-at-point'
;; Complete lisp symbol or filename at point.
;; `helm-w32-shell-execute-open-file'
;; Not documented.
;; `helm-c-set-variable'
;; Set value to VAR interactively.
;; `helm-c-adaptive-save-history'
;; Save history information to file given by `helm-c-adaptive-history-file'.
;; `helm-c-reset-adaptative-history'
;; Delete all `helm-c-adaptive-history' and his file.
;; `helm-mini'
;; Preconfigured `helm' lightweight version (buffer -> recentf).
;; `helm-for-files'
;; Preconfigured `helm' for opening files.
;; `helm-recentf'
;; Preconfigured `helm' for `recentf'.
;; `helm-info-at-point'
;; Preconfigured `helm' for searching info at point.
;; `helm-show-kill-ring'
;; Preconfigured `helm' for `kill-ring'.
;; `helm-minibuffer-history'
;; Preconfigured `helm' for `minibuffer-history'.
;; `helm-gentoo'
;; Preconfigured `helm' for gentoo linux.
;; `helm-imenu'
;; Preconfigured `helm' for `imenu'.
;; `helm-google-suggest'
;; Preconfigured `helm' for google search with google suggest.
;; `helm-yahoo-suggest'
;; Preconfigured `helm' for Yahoo searching with Yahoo suggest.
;; `helm-for-buffers'
;; Preconfigured `helm' for buffers.
;; `helm-buffers-list'
;; Preconfigured `helm' to list buffers.
;; `helm-bbdb'
;; Preconfigured `helm' for BBDB.
;; `helm-locate'
;; Preconfigured `helm' for Locate.
;; `helm-w3m-bookmarks'
;; Preconfigured `helm' for w3m bookmark.
;; `helm-firefox-bookmarks'
;; Preconfigured `helm' for firefox bookmark.
;; `helm-colors'
;; Preconfigured `helm' for color.
;; `helm-bookmarks'
;; Preconfigured `helm' for bookmarks.
;; `helm-c-pp-bookmarks'
;; Preconfigured `helm' for bookmarks	(pretty-printed).
;; `helm-c-insert-latex-math'
;; Preconfigured helm for latex math symbols completion.
;; `helm-register'
;; Preconfigured `helm' for Emacs registers.
;; `helm-man-woman'
;; Preconfigured `helm' for Man and Woman pages.
;; `helm-org-keywords'
;; Preconfigured `helm' for org keywords.
;; `helm-emms'
;; Preconfigured `helm' for emms sources.
;; `helm-eev-anchors'
;; Preconfigured `helm' for eev anchors.
;; `helm-bm-list'
;; Preconfigured `helm' for visible bookmarks.
;; `helm-timers'
;; Preconfigured `helm' for timers.
;; `helm-list-emacs-process'
;; Preconfigured `helm' for emacs process.
;; `helm-occur'
;; Preconfigured Helm for Occur source.
;; `helm-browse-code'
;; Preconfigured helm to browse code.
;; `helm-org-headlines'
;; Preconfigured helm to show org headlines.
;; `helm-regexp'
;; Preconfigured helm to build regexps.
;; `helm-c-copy-files-async'
;; Preconfigured helm to copy file list FLIST to DEST asynchronously.
;; `helm-find-files'
;; Preconfigured `helm' for helm implementation of `find-file'.
;; `helm-write-file'
;; Preconfigured `helm' providing completion for `write-file'.
;; `helm-insert-file'
;; Preconfigured `helm' providing completion for `insert-file'.
;; `helm-dired-rename-file'
;; Preconfigured `helm' to rename files from dired.
;; `helm-dired-copy-file'
;; Preconfigured `helm' to copy files from dired.
;; `helm-dired-symlink-file'
;; Preconfigured `helm' to symlink files from dired.
;; `helm-dired-hardlink-file'
;; Preconfigured `helm' to hardlink files from dired.
;; `helm-do-grep'
;; Preconfigured helm for grep.
;; `helm-do-pdfgrep'
;; Preconfigured helm for pdfgrep.
;; `helm-c-etags-select'
;; Preconfigured helm for etags.
;; `helm-filelist'
;; Preconfigured `helm' to open files instantly.
;; `helm-filelist+'
;; Preconfigured `helm' to open files/buffers/bookmarks instantly.
;; `helm-M-x'
;; Preconfigured `helm' for Emacs commands.
;; `helm-manage-advice'
;; Preconfigured `helm' to disable/enable function advices.
;; `helm-bookmark-ext'
;; Preconfigured `helm' for bookmark-extensions sources.
;; `helm-simple-call-tree'
;; Preconfigured `helm' for simple-call-tree. List function relationships.
;; `helm-mark-ring'
;; Preconfigured `helm' for `helm-c-source-mark-ring'.
;; `helm-global-mark-ring'
;; Preconfigured `helm' for `helm-c-source-global-mark-ring'.
;; `helm-all-mark-rings'
;; Preconfigured `helm' for `helm-c-source-global-mark-ring' and `helm-c-source-mark-ring'.
;; `helm-yaoddmuse-emacswiki-edit-or-view'
;; Preconfigured `helm' to edit or view EmacsWiki page.
;; `helm-yaoddmuse-emacswiki-post-library'
;; Preconfigured `helm' to post library to EmacsWiki.
;; `helm-eval-expression'
;; Preconfigured helm for `helm-c-source-evaluation-result'.
;; `helm-eval-expression-with-eldoc'
;; Preconfigured helm for `helm-c-source-evaluation-result' with `eldoc' support. 
;; `helm-calcul-expression'
;; Preconfigured helm for `helm-c-source-calculation-result'.
;; `helm-surfraw'
;; Preconfigured `helm' to search PATTERN with search ENGINE.
;; `helm-call-source'
;; Preconfigured `helm' to call helm source.
;; `helm-execute-helm-command'
;; Preconfigured `helm' to execute preconfigured `helm'.
;; `helm-create'
;; Preconfigured `helm' to do many create actions from STRING.
;; `helm-top'
;; Preconfigured `helm' for top command.
;; `helm-select-xfont'
;; Preconfigured `helm' to select Xfont.
;; `helm-world-time'
;; Preconfigured `helm' to show world time.
;; `helm-apt'
;; Preconfigured `helm' : frontend of APT package manager.
;; `helm-esh-pcomplete'
;; Preconfigured helm to provide helm completion in eshell.
;; `helm-eshell-history'
;; Preconfigured helm for eshell history.
;; `helm-c-run-external-command'
;; Preconfigured `helm' to run External PROGRAM asyncronously from Emacs.
;; `helm-ratpoison-commands'
;; Preconfigured `helm' to execute ratpoison commands.
;; `helm-ucs'
;; Preconfigured helm for `ucs-names' math symbols.
;; `helm-c-apropos'
;; Preconfigured helm to describe commands, functions, variables and faces.
;; `helm-xrandr-set'
;; Not documented.

;;  * User variables defined here:
;; [EVAL] (autodoc-document-lisp-buffer :type 'user-variable :prefix "helm-" :var-value t)
;; `helm-c-adaptive-history-file'
;; Default Value: "~/.emacs.d/helm-c-adaptive-history"
;; `helm-c-adaptive-history-length'
;; Default Value: 50
;; `helm-c-google-suggest-url'
;; Default Value: "http://google.com/complete/search?output=toolbar&q="
;; `helm-c-google-suggest-search-url'
;; Default Value: "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
;; `helm-google-suggest-use-curl-p'
;; Default Value: nil
;; `helm-c-yahoo-suggest-url'
;; Default Value: "http://search.yahooapis.com/WebSearchService/V1/relatedSuggestion?appid=G [...]
;; `helm-c-yahoo-suggest-search-url'
;; Default Value: "http://search.yahoo.com/search?&ei=UTF-8&fr&h=c&p="
;; `helm-c-boring-buffer-regexp'
;; Default Value: "\\	(\\` \\)\\|\\*helm\\|\\*helm-mode\\| \\*Echo Area\\| \\*Minibuf"
;; `helm-c-boring-file-regexp'
;; Default Value: "/\\	(?:\\(?:\\.\\(?:git\\|hg\\|svn\\)\\|CVS\\|_darcs\\)\\)\\(?:/\\|$\\)\\| [...]
;; `helm-kill-ring-threshold'
;; Default Value: 10
;; `helm-c-kill-ring-max-lines-number'
;; Default Value: nil
;; `helm-su-or-sudo'
;; Default Value: "su"
;; `helm-for-files-prefered-list'
;; Default Value:	(helm-c-source-ffap-line helm-c-source-ffap-guesser helm-c-sou [...]
;; `helm-create--actions-private'
;; Default Value: nil
;; `helm-allow-skipping-current-buffer'
;; Default Value: nil
;; `helm-c-enable-eval-defun-hack'
;; Default Value: t
;; `helm-tramp-verbose'
;; Default Value: 0
;; `helm-raise-command'
;; Default Value: nil
;; `helm-command-map-prefix-key'
;; Default Value: "<f5> a"
;; `helm-c-browse-code-regexp-lisp'
;; Default Value: "^ *	(def\\(un\\|subst\\|macro\\|face\\|alias\\|advice\\|struct\\|type\\|th [...]
;; `helm-c-browse-code-regexp-python'
;; Default Value: "\\<def\\>\\|\\<class\\>"
;; `helm-c-browse-code-regexp-alist'
;; Default Value:	((lisp-interaction-mode . "^ *(def\\(un\\|subst\\|macro\\|face\\|alias\\|a [...]
;; `helm-c-external-programs-associations'
;; Default Value: nil
;; `helm-ff-auto-update-initial-value'
;; Default Value: t
;; `helm-c-copy-async-prefered-emacs'
;; Default Value: "emacs"
;; `helm-ff-lynx-style-map'
;; Default Value: t
;; `helm-ff-history-max-length'
;; Default Value: 100
;; `helm-ff-smart-completion'
;; Default Value: t
;; `helm-ff-default-kbsize'
;; Default Value: 1024.0
;; `helm-ff-tramp-not-fancy'
;; Default Value: t
;; `helm-ff-exif-data-program'
;; Default Value: "exiftran"
;; `helm-ff-exif-data-program-args'
;; Default Value: "-d"
;; `helm-c-grep-use-ioccur-style-keys'
;; Default Value: t
;; `helm-c-pdfgrep-default-read-command'
;; Default Value: "xpdf '%f' %p"
;; `helm-c-etags-tag-file-name'
;; Default Value: "TAGS"
;; `helm-c-etags-tag-file-search-limit'
;; Default Value: 10
;; `helm-c-etags-use-regexp-search'
;; Default Value: nil
;; `helm-c-etags-search-regexp'
;; Default Value: "^.+: .+ \\<%s"
;; `helm-c-filelist-file-name'
;; Default Value: nil
;; `helm-c-eldoc-in-minibuffer-show-fn'
;; Default Value: helm-c-show-info-in-mode-line
;; `helm-c-turn-on-show-completion'
;; Default Value: t
;; `helm-c-show-completion-use-special-display'
;; Default Value: t
;; `helm-c-show-completion-min-window-height'
;; Default Value: 7
;; `helm-lisp-completion-or-indent-delay'
;; Default Value: 0.6
;; `helm-c-default-external-file-browser'
;; Default Value: "nautilus"
;; `helm-c-use-adaptative-sorting'
;; Default Value: nil
;; `helm-ff-newfile-prompt-p'
;; Default Value: t
;; `helm-ff-avfs-directory'
;; Default Value: nil
;; `helm-ff-file-compressed-list'
;; Default Value:	("gz" "bz2" "zip" "7z") 
;; `helm-locate-db-file-regexp'
;; Default Value: "m?locate.db$"
;; `helm-c-locate-command'
;; Default Value: nil
;; `helm-c-show-info-in-mode-line-delay'
;; Default Value: 12
;; `helm-c-copy-files-async-log-file'
;; Default Value: "/tmp/dired.log"
;; `helm-ff-printer-list'
;; Default Value: nil
;; `helm-ff-transformer-show-only-basename'
;; Default Value: nil
;; `helm-ff-quick-delete-dont-prompt-for-deletion'
;; Default Value: nil
;; `helm-ff-signal-error-on-dot-files'
;; Default Value: t
;; `helm-completing-read-handlers-alist'
;; Default Value:	((describe-function . helm-completing-read-symbols)  (describe-variabl [...]

;;  * Helm sources defined here:
;; [EVAL] (autodoc-document-lisp-buffer :type 'helm-source :prefix "helm-" :any-sname t)
;; `helm-c-source-regexp'					(Regexp Builder)
;; `helm-c-source-buffers'					(Buffers)
;; `helm-c-source-buffer-not-found'				(Create buffer)
;; `helm-c-source-buffers-list'				(Buffers)
;; `helm-c-source-file-name-history'			(File Name History)
;; `helm-c-source-files-in-current-dir'			(Files from Current Directory)
;; `helm-c-source-files-in-current-dir+'			(Files from Current Directory)
;; `helm-c-source-find-files'				(Find Files)
;; `helm-c-source-write-file'				(Write File)
;; `helm-c-source-insert-file'				(Insert File)
;; `helm-c-source-copy-files'				(Copy Files)
;; `helm-c-source-symlink-files'				(Symlink Files)
;; `helm-c-source-hardlink-files'				(Hardlink Files)
;; `helm-c-source-file-cache'				(File Cache)
;; `helm-c-source-locate'					(Locate)
;; `helm-c-source-recentf'					(Recentf)
;; `helm-c-source-ffap-guesser'				(File at point)
;; `helm-c-source-ffap-line'				(File/Lineno at point)
;; `helm-c-source-files-in-all-dired'			(Files in all dired buffer.)
;; `helm-c-source-filelist'					(FileList)
;; `helm-c-source-info-pages'				(Info Pages)
;; `helm-c-source-man-pages'				(Manual Pages)
;; `helm-c-source-complex-command-history'			(Complex Command History)
;; `helm-c-source-extended-command-history'			(Emacs Commands History)
;; `helm-c-source-emacs-commands'				(Emacs Commands)
;; `helm-c-source-emacs-functions'				(Emacs Functions)
;; `helm-c-source-emacs-functions-with-abbrevs'		(Emacs Functions)
;; `helm-c-source-advice'					(Function Advice)
;; `helm-c-source-emacs-variables'				(Emacs Variables)
;; `helm-c-source-lacarte'					(Lacarte)
;; `helm-c-source-bookmarks'				(Bookmarks)
;; `helm-c-source-bookmark-set'				(Set Bookmark)
;; `helm-c-source-bm'					(Visible Bookmarks)
;; `helm-c-source-bookmarks-ssh'				(Bookmarks-ssh)
;; `helm-c-source-bookmarks-su'				(Bookmarks-root)
;; `helm-c-source-bookmarks-local'				(Bookmarks-Local)
;; `helm-c-source-bmkext-addressbook'			(Bookmark Addressbook)
;; `helm-c-source-bookmark-w3m'				(Bookmark W3m)
;; `helm-c-source-bookmark-images'				(Bookmark Images)
;; `helm-c-source-bookmark-man'				(Bookmark Woman&Man)
;; `helm-c-source-bookmark-gnus'				(Bookmark Gnus)
;; `helm-c-source-bookmark-info'				(Bookmark Info)
;; `helm-c-source-bookmark-files&dirs'			(Bookmark Files&Directories)
;; `helm-c-source-bookmark-su-files&dirs'			(Bookmark Root-Files&Directories)
;; `helm-c-source-bookmark-ssh-files&dirs'			(Bookmark Ssh-Files&Directories)
;; `helm-c-source-firefox-bookmarks'			(Firefox Bookmarks)
;; `helm-c-source-w3m-bookmarks'				(W3m Bookmarks)
;; `helm-c-source-elisp-library-scan'			(Elisp libraries (Scan))
;; `helm-c-source-imenu'					(Imenu)
;; `helm-c-source-ctags'					(Exuberant ctags)
;; `helm-c-source-etags-select'				(Etags)
;; `helm-c-source-semantic'					(Semantic Tags)
;; `helm-c-source-simple-call-tree-functions-callers'	(Function is called by)
;; `helm-c-source-simple-call-tree-callers-functions'	(Function calls)
;; `helm-c-source-commands-and-options-in-file'		(Commands/Options in file)
;; `helm-c-source-customize-face'				(Customize Face)
;; `helm-c-source-colors'					(Colors)
;; `helm-c-source-tracker-search'				(Tracker Search)
;; `helm-c-source-mac-spotlight'				(mdfind)
;; `helm-c-source-picklist'					(Picklist)
;; `helm-c-source-kill-ring'				(Kill Ring)
;; `helm-c-source-mark-ring'				(mark-ring)
;; `helm-c-source-global-mark-ring'				(global-mark-ring)
;; `helm-c-source-register'					(Registers)
;; `helm-c-source-latex-math'				(Latex Math Menu)
;; `helm-c-source-fixme'					(TODO/FIXME/DRY comments)
;; `helm-c-source-rd-headline'				(RD HeadLine)
;; `helm-c-source-oddmuse-headline'				(Oddmuse HeadLine)
;; `helm-c-source-emacs-source-defun'			(Emacs Source DEFUN)
;; `helm-c-source-emacs-lisp-expectations'			(Emacs Lisp Expectations)
;; `helm-c-source-emacs-lisp-toplevels'			(Emacs Lisp Toplevel / Level 4 Comment / Linkd Star)
;; `helm-c-source-yaoddmuse-emacswiki-edit-or-view'		(Yaoddmuse Edit or View (EmacsWiki))
;; `helm-c-source-yaoddmuse-emacswiki-post-library'		(Yaoddmuse Post library (EmacsWiki))
;; `helm-c-source-eev-anchor'				(Anchors)
;; `helm-c-source-org-headline'				(Org HeadLine)
;; `helm-c-source-org-keywords'				(Org Keywords)
;; `helm-c-source-bbdb'					(BBDB)
;; `helm-c-source-evaluation-result'			(Evaluation Result)
;; `helm-c-source-calculation-result'			(Calculation Result)
;; `helm-c-source-google-suggest'				(Google Suggest)
;; `helm-c-source-yahoo-suggest'				(Yahoo Suggest)
;; `helm-c-source-emms-streams'				(Emms Streams)
;; `helm-c-source-emms-dired'				(Music Directory)
;; `helm-c-source-emms-files'				(Emms files)
;; `helm-c-source-jabber-contacts'				(Jabber Contacts)
;; `helm-c-source-call-source'				(Call helm source)
;; `helm-c-source-helm-commands'			(Preconfigured Helm)
;; `helm-c-source-occur'					(Occur)
;; `helm-c-source-browse-code'				(Browse code)
;; `helm-c-source-create'					(Create)
;; `helm-c-source-minibuffer-history'			(Minibuffer History)
;; `helm-c-source-elscreen'					(Elscreen)
;; `helm-c-source-top'					(Top (Press C-c C-u to refresh))
;; `helm-c-source-absolute-time-timers'			(Absolute Time Timers)
;; `helm-c-source-idle-time-timers'				(Idle Time Timers)
;; `helm-c-source-xrandr-change-resolution'			(Change Resolution)
;; `helm-c-source-xfonts'					(X Fonts)
;; `helm-c-source-ucs'					(Ucs names)
;; `helm-c-source-emacs-process'				(Emacs Process)
;; `helm-c-source-time-world'				(Time World List)
;; `helm-c-source-apt'					(APT)
;; `helm-c-source-gentoo'					(Portage sources)
;; `helm-c-source-use-flags'				(Use Flags)
;; `helm-c-source-ratpoison-commands'			(Ratpoison Commands)
;; `helm-c-source-esh'					(Eshell completions)
;; `helm-c-source-eshell-history'				(Eshell history)

;;  *** END auto-documentation

;;; For Maintainers:
;;
;; Install developer-tools/autodoc.el and
;; Evaluate (autodoc-update-all) before commit or run it interactively.
;; This function generates helm-c-source-* / functions / options list.
;;
;; [EVAL IT] (autodoc-update-all)
;;
;; Please write details documentation about function, then others will
;; read code more easier.   -- Andy Stewart
;;


;;; Change log:
;;
;;  Change log of this file is found at
;;  http://repo.or.cz/w/helm-config.git/history/master:/helm-config.el
;;
;;  Change log of this project is found at
;;  http://repo.or.cz/w/helm-config.git?a=shortlog

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
(require 'helm)
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
(require 'helm-match-plugin)



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
(defgroup helm-config nil
  "Predefined configurations for `helm.el'."
  :group 'helm)

(defcustom helm-c-adaptive-history-file
  "~/.emacs.d/helm-c-adaptive-history"
  "Path of file where history information is stored."
  :type 'string
  :group 'helm-config)

(defcustom helm-c-adaptive-history-length 50
  "Maximum number of candidates stored for a source."
  :type 'number
  :group 'helm-config)

(defcustom helm-c-google-suggest-url
  "http://google.com/complete/search?output=toolbar&q="
  "URL used for looking up Google suggestions."
  :type 'string
  :group 'helm-config)

(defcustom helm-c-google-suggest-search-url
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
  "URL used for Google searching."
  :type 'string
  :group 'helm-config)

(defcustom helm-google-suggest-use-curl-p nil
  "When non--nil use CURL to get info from `helm-c-google-suggest-url'.
Otherwise `url-retrieve-synchronously' is used."
  :type 'boolean
  :group 'helm-config)

(defcustom helm-c-yahoo-suggest-url
  "http://search.yahooapis.com/WebSearchService/V1/relatedSuggestion?appid=Generic&query="
  "Url used for looking up Yahoo suggestions."
  :type 'string
  :group 'helm-config)

(defcustom helm-c-yahoo-suggest-search-url
  "http://search.yahoo.com/search?&ei=UTF-8&fr&h=c&p="
  "Url used for Yahoo searching."
  :type 'string
  :group 'helm-config)

(defcustom helm-c-boring-buffer-regexp
  (rx (or
       (group bos  " ")
       ;; helm-buffers
       "*helm" "*helm-mode"
       ;; echo area
       " *Echo Area" " *Minibuf"))
  "The regexp that match boring buffers.
Buffer candidates matching this regular expression will be
filtered from the list of candidates if the
`helm-c-skip-boring-buffers' candidate transformer is used, or
they will be displayed with face `file-name-shadow' if
`helm-c-shadow-boring-buffers' is used."
  :type 'string
  :group 'helm-config)
;; (string-match helm-c-boring-buffer-regexp "buf")
;; (string-match helm-c-boring-buffer-regexp " hidden")
;; (string-match helm-c-boring-buffer-regexp " *Minibuf-1*")

(defcustom helm-c-boring-file-regexp
  (rx (or
       ;; Boring directories
       (and "/" (or ".svn" "CVS" "_darcs" ".git" ".hg") (or "/" eol))
       ;; Boring files
       (and line-start  ".#")
       (and (or ".class" ".la" ".o" "~") eol)))
  "The regexp that match boring files.
File candidates matching this regular expression will be
filtered from the list of candidates if the
`helm-c-skip-boring-files' candidate transformer is used, or
they will be displayed with face `file-name-shadow' if
`helm-c-shadow-boring-files' is used."
  :type 'string
  :group 'helm-config)

(defcustom helm-kill-ring-threshold 10
  "Minimum length to be listed by `helm-c-source-kill-ring'."
  :type 'integer
  :group 'helm-config)

(defcustom helm-c-kill-ring-max-lines-number nil
  "Max number of lines displayed per candidate in kill-ring browser.
If nil or zero, don't truncate candidate, show all."
  :type 'integer
  :group 'helm-config)

(defcustom helm-su-or-sudo "su"
  "What command to use for root access."
  :type 'string
  :group 'helm-config)

(defcustom helm-for-files-prefered-list
  '(helm-c-source-ffap-line
    helm-c-source-ffap-guesser
    helm-c-source-buffers-list
    helm-c-source-recentf
    helm-c-source-bookmarks
    helm-c-source-file-cache
    helm-c-source-files-in-current-dir+
    helm-c-source-locate)
  "Your prefered sources to find files."
  :type 'list
  :group 'helm-config)

(defcustom helm-create--actions-private nil
  "User defined actions for `helm-create' / `helm-c-source-create'.
It is a list of (DISPLAY . FUNCTION) pairs like `action'
attribute of `helm-sources'.

It is prepended to predefined pairs."
  :type 'list
  :group 'helm-config)

(defcustom helm-allow-skipping-current-buffer nil
  "Show current buffer or not in helm buffer"
  :type 'boolean
  :group 'helm-config)

(defcustom helm-c-enable-eval-defun-hack t
  "If non-nil, execute `helm' using the source at point when C-M-x is pressed.
This hack is invoked when pressing C-M-x in the form \
 (defvar helm-c-source-XXX ...) or (setq helm-c-source-XXX ...)."
  :type 'boolean
  :group 'helm-config)

(defcustom helm-tramp-verbose 0
  "Just like `tramp-verbose' but specific to helm.
When set to 0 don't show tramp messages in helm.
If you want to have the default tramp messages set it to 3."
  :type 'integer
  :group 'helm-config)

(defcustom helm-raise-command nil
  "A shell command to jump to a window running specific program.
Need external program wmctrl.
This will be use with `format', so use something like \"wmctrl -xa %s\"."
  :type 'string
  :group 'helm-config)

(defun helm-set-helm-command-map-prefix-key (var key)
  "The customize set function for `helm-command-map-prefix-key'."
  (when (boundp var)
    (define-key global-map (read-kbd-macro (symbol-value var)) nil))
  (set var key)
  (define-key global-map
      (read-kbd-macro (symbol-value var)) 'helm-command-map))

(defcustom helm-command-map-prefix-key "C-x c"
  "The prefix key for all `helm-command-map' commands."
  :type  'string
  :set   'helm-set-helm-command-map-prefix-key
  :group 'helm-config)

(defcustom helm-c-browse-code-regexp-lisp
  "^ *\(def\\(un\\|subst\\|macro\\|face\\|alias\\|advice\\|struct\\|\
type\\|theme\\|var\\|group\\|custom\\|const\\|method\\|class\\)"
  "Regexp used to parse lisp buffer when browsing code."
  :type 'string
  :group 'helm-config)

(defcustom helm-c-browse-code-regexp-python
  "\\<def\\>\\|\\<class\\>"
  "Regexp used to parse python buffer when browsing code."
  :type 'string
  :group 'helm-config)

(defcustom helm-c-browse-code-regexp-alist
  `((lisp-interaction-mode . ,helm-c-browse-code-regexp-lisp)
    (emacs-lisp-mode . ,helm-c-browse-code-regexp-lisp)
    (lisp-mode . ,helm-c-browse-code-regexp-lisp)
    (python-mode . ,helm-c-browse-code-regexp-python))
  "Alist to store regexps for browsing code corresponding \
to a specific `major-mode'."
  :type 'list
  :group 'helm-config)

(defcustom helm-c-external-programs-associations nil
  "Alist to store externals programs associated with file extension.
This variable overhide setting in .mailcap file.
e.g : '\(\(\"jpg\" . \"gqview\"\) (\"pdf\" . \"xpdf\"\)\) "
  :type 'list
  :group 'helm-config)

(defcustom helm-ff-auto-update-initial-value t
  "Auto update when only one candidate directory is matched.
This is the default value when starting `helm-find-files'."
  :group 'helm-config
  :type  'boolean)

(defcustom helm-c-copy-async-prefered-emacs "emacs"
  "Path to the emacs you want to use for copying async.
Emacs versions < 24 fail to copy directory due to a bug not fixed
in `copy-directory'."
  :group 'helm-config
  :type 'string)

(defcustom helm-ff-lynx-style-map t
  "Use arrow keys to navigate with `helm-find-files'.
You will have to restart Emacs or reeval `helm-find-files-map'
and `helm-c-read-file-map' for this take effect."
  :group 'helm-config
  :type 'boolean)

(defcustom helm-ff-history-max-length 100
  "Number of elements shown in `helm-find-files' history."
  :group 'helm-config
  :type 'integer)

(defcustom helm-ff-smart-completion t
  "Try to complete filenames smarter when non--nil.
See `helm-ff-transform-fname-for-completion' for more info."
  :group 'helm-config
  :type 'boolean)

(defcustom helm-ff-default-kbsize 1024.0
  "Default Kbsize to use for showing files size.
It is a float, usually 1024.0 but could be 1000.0 on some systems."
  :group 'helm-config
  :type 'float)

(defcustom helm-ff-tramp-not-fancy t
  "No colors when listing remote files when set to non--nil.
This make listing much faster, specially on slow machines."
  :group 'helm-config
  :type  'boolean)

(defcustom helm-ff-exif-data-program "exiftran"
  "Program used to extract exif data of an image file."
  :group 'helm-config
  :type 'string)

(defcustom helm-ff-exif-data-program-args "-d"
  "*Arguments used for `helm-ff-exif-data-program'."
  :group 'helm-config
  :type 'string)

(defcustom helm-c-grep-use-ioccur-style-keys t
  "Use Arrow keys to jump to occurences."
  :group 'helm-config
  :type  'boolean)

(defcustom helm-c-pdfgrep-default-read-command "xpdf '%f' %p"
  "Default command to read pdf files from pdfgrep.
Where '%f' format spec is filename and '%p' is page number"
  :group 'helm-config
  :type  'string)

(defcustom helm-c-etags-tag-file-name "TAGS"
  "Etags tag file name."
  :type  'string
  :group 'helm-config)

(defcustom helm-c-etags-tag-file-search-limit 10
  "The limit level of directory to search tag file.
Don't search tag file deeply if outside this value."
  :type  'number
  :group 'helm-config)

(defcustom helm-c-etags-use-regexp-search nil
  "When non--nil search etags candidates by regexp.
This disable helm-match-plugin when enabled.
When nil search is performed directly on patter and *match-plugin is used
if available.  You can customize `helm-c-etags-search-regexp'."
  :group 'helm-config
  :type  'boolean)

(defcustom helm-c-etags-search-regexp "^.+: .+ \\<%s"
  "Regexp that match tags in an helm etags buffer.
The format spec is replaced by pattern.
This regexp have no effect when `helm-c-etags-use-regexp-search'
is nil."
  :group 'helm-config
  :type  'regexp)

(defcustom helm-c-eldoc-in-minibuffer-show-fn
  'helm-c-show-info-in-mode-line
  "A function to display eldoc info.
Should take one arg: the string to display."
  :group 'helm-config
  :type  'symbol)

(defcustom helm-c-turn-on-show-completion t
  "Display candidate in buffer while moving selection when non--nil."
  :group 'helm-config
  :type  'boolean)

(defcustom helm-c-show-completion-use-special-display t
  "A special display will be used in lisp completion if non--nil.
All functions that are wrapped in macro `with-helm-show-completion'
will be affected."
  :group 'helm-config
  :type  'boolean)

(defcustom helm-c-show-completion-min-window-height 7
  "Minimum completion window height used in show completion.
This is used in macro `with-helm-show-completion'."
  :group 'helm-config
  :type  'integer)

(defcustom helm-lisp-completion-or-indent-delay 0.6
  "After this delay `helm-lisp-completion-counter' is reset to 0.
This allow to indent again without completing lisp symbol after this delay.
Default is 0.6 seconds."
  :group 'helm-config
  :type  'number)

(defcustom helm-c-default-external-file-browser "nautilus"
  "Default external file browser for your system.
Directories will be opened externally with it when
opening file externally in `helm-find-files'.
Set to nil if you do not have external file browser
or do not want to use it.
Windows users should set that to \"explorer.exe\"."
  :group 'helm-config
  :type  'string)

(defcustom helm-c-use-adaptative-sorting nil
  "Wheter to use or not adaptative sorting.
Even if a source use it, it will have no effect when set to nil."
  :type 'boolean
  :group 'helm-config)

(defcustom helm-ff-newfile-prompt-p t
  "Whether Prompt or not when creating new file.
This set `ffap-newfile-prompt'."
  :type  'boolean
  :group 'helm-config)


(defcustom helm-ff-avfs-directory nil
  "The default avfs directory, usually '.avfs'.
When this is set you will be able to expand archive filenames with `C-z'
inside an avfs directory mounted with mountavfs.
See <http://sourceforge.net/projects/avf/>."
  :type  'boolean
  :group 'helm-config)

(defcustom helm-ff-file-compressed-list '("gz" "bz2" "zip" "7z")
  "Minimal list of compressed files extension."
  :type  'list
  :group 'helm-config)

(defcustom helm-locate-db-file-regexp "m?locate\.db$"
  "Default regexp to match locate database.
If nil Search in all files."
  :type  'string
  :group 'helm-config)

(defcustom helm-ff-locate-db-filename "locate.db"
  "The basename of the locatedb file you use locally in your directories.
When this is set and `helm' find such a file in the directory from
where you launch locate, it will use this file and will not prompt you
for a db file.
Note that this happen only when locate is launched with a prefix arg."
  :group 'helm-config
  :type 'string)

(defcustom helm-c-locate-command nil
  "A list of arguments for locate program.
If nil it will be calculated when `helm-locate' startup
with these default values for different systems:

Gnu/linux: \"locate -i -r %s\"
berkeley-unix: \"locate -i %s\"
windows-nt: \"es -i -r %s\"
Others: \"locate %s\"

This string will be passed to format so it should end with `%s'.
The \"-r\" option must be the last option."
  :type 'string
  :group 'helm-config)

(defcustom helm-c-show-info-in-mode-line-delay 12
  "Eldoc will show info in mode-line during this delay if user is idle."
  :type  'integer
  :group 'helm-config)

(defcustom helm-c-copy-files-async-log-file "/tmp/dired.log"
  "The file used to communicate with two emacs when copying files async."
  :type  'string
  :group 'helm-config)

(defcustom helm-ff-printer-list nil
  "A list of available printers on your system.
When non--nil let you choose a printer to print file.
Otherwise when nil the variable `printer-name' will be used.
On Unix based systems (lpstat command needed) you don't need to set this,
`helm-ff-find-printers' will find a list of available printers for you."
  :type 'list
  :group 'helm-config)

(defcustom helm-ff-transformer-show-only-basename nil
  "Show only basename of candidates in `helm-find-files'.
This can be toggled at anytime from `helm-find-files' with \
\\<helm-find-files-map>0\\[helm-ff-run-toggle-basename]."
  :type 'boolean
  :group 'helm-config)

(defcustom helm-ff-quick-delete-dont-prompt-for-deletion nil
  "Don't ask in persistent deletion of files when non--nil."
  :group 'helm-config
  :type 'boolean)

(defcustom helm-ff-signal-error-on-dot-files t
  "Signal error when file is `.' or `..' on file deletion when non--nil.
Default is non--nil.
WARNING: Setting this to nil is unsafe and can cause deletion of a whole tree."
  :group 'helm-config
  :type 'boolean)

(defcustom helm-completing-read-handlers-alist
  '((describe-function . helm-completing-read-symbols)
    (describe-variable . helm-completing-read-symbols)
    (debug-on-entry . helm-completing-read-symbols)
    (find-function . helm-completing-read-symbols)
    (trace-function . helm-completing-read-symbols)
    (trace-function-background . helm-completing-read-symbols)
    (find-tag . helm-completing-read-with-cands-in-buffer)
    (ffap-alternate-file . nil))
  "Alist of handlers to replace `completing-read', `read-file-name' in `helm-mode'.
Each entry is a cons cell like \(emacs_command . completing-read_handler\)
where key and value are symbols.

Each key is an Emacs command that use originaly `completing-read'.

Each value maybe an helm function that take same arguments as
`completing-read' plus NAME and BUFFER, where NAME is the name of the new
helm source and BUFFER the name of the buffer we will use.
This function prefix name must start by \"helm\".

See `helm-completing-read-symbols' for example.

If the value of an entry is nil completion will fall back to
emacs vanilla behavior.
e.g If you want to disable helm completion for `describe-function':
\(describe-function . nil\).

Ido is also supported, you can use `ido-completing-read' and
`ido-read-file-name' as value of an entry or just 'ido.
e.g ido completion for `find-file':
\(find-file . ido\)
same as
\(find-file . ido-read-file-name\)
Note that you don't need to enable `ido-mode' for this to work."
  :group 'helm-config
  :type '(alist :key-type symbol :value-type symbol))

(defcustom helm-M-x-requires-pattern 2
  "Value of requires-pattern for `helm-M-x'.
Set it to 0 to disable requires-pattern in `helm-M-x'."
  :group 'helm-config
  :type 'boolean)

;;; Build info-index sources with info-index plug-in.
;;
;;
(defun helm-c-build-info-index-command (name doc source buffer)
  "Define an helm command NAME with documentation DOC.
Arg SOURCE will be an existing helm source named
`helm-c-source-info-<NAME>' and BUFFER a string buffer name."
  (eval (list 'defun name nil doc
              (list 'interactive)
              (list 'helm
                    :sources source
                    :buffer buffer
                    :candidate-number-limit 1000))))

(defun helm-c-define-info-index-sources (var-value &optional commands)
  "Define helm sources named helm-c-source-info-<NAME>.
Sources are generated for all entries of `helm-c-default-info-index-list'.
If COMMANDS arg is non--nil build also commands named `helm-info<NAME>'.
Where NAME is one of `helm-c-default-info-index-list'."
  (loop with symbols = (loop for str in var-value
                             collect
                             (intern (concat "helm-c-source-info-" str)))
        for sym in symbols
        for str in var-value
        do (set sym (list (cons 'name (format "Info index: %s" str))
                          (cons 'info-index str)))
        when commands
        do (let ((com (intern (concat "helm-info-" str))))
	     (helm-c-build-info-index-command com
               (format "Predefined helm for %s info." str) sym
               (format "*helm info %s*" str)))))

(defun helm-info-index-set (var value)
  (set var value)
  (helm-c-define-info-index-sources value t))

(defcustom helm-c-default-info-index-list
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
  "Info Manual entries to use for building helm info index commands."
  :group 'helm-config
  :type 'list
  :set 'helm-info-index-set)

(defcustom helm-c-register-max-offset 160
  "Max size of string register entries before truncating."
  :group 'helm-config
  :type 'integer)


;;; General internal variables
;;
;; Some internals variable that need to be loaded
;; here to avoid compiler warnings.
(defvar helm-c-external-commands-list nil
  "A list of all external commands the user can execute.  If this
variable is not set by the user, it will be calculated
automatically.")

(defvar helm-c-show-completion-overlay nil)



;;; Faces
;;
;;
(defface helm-buffer-saved-out
    '((t (:foreground "red")))
  "*Face used for buffer files modified outside of emacs."
  :group 'helm-config)

(defface helm-buffer-not-saved
    '((t (:foreground "Indianred2")))
  "*Face used for buffer files not already saved on disk."
  :group 'helm-config)

(defface helm-ff-prefix
    '((t (:background "yellow" :foreground "black")))
  "*Face used to prefix new file or url paths in `helm-find-files'."
  :group 'helm-config)

(defface helm-ff-executable
    '((t (:foreground "green")))
  "*Face used for executable files in `helm-find-files'."
  :group 'helm-config)

(defface helm-ff-directory
    '((t (:foreground "DarkRed" :background "LightGray")))
  "*Face used for directories in `helm-find-files'."
  :group 'helm-config)

(defface helm-ff-symlink
    '((t (:foreground "DarkOrange")))
  "*Face used for symlinks in `helm-find-files'."
  :group 'helm-config)

(defface helm-ff-invalid-symlink
    '((t (:foreground "black" :background "red")))
  "*Face used for invalid symlinks in `helm-find-files'."
  :group 'helm-config)

(defface helm-ff-file
    '((t (:foreground "CadetBlue" :underline t)))
  "*Face used for file names in `helm-find-files'."
  :group 'helm-config)

(defface helm-grep-match
    '((t (:inherit match)))
  "Face used to highlight grep matches."
  :group 'helm-config)

(defface helm-grep-file
    '((t (:foreground "BlueViolet" :underline t)))
  "Face used to highlight grep results filenames."
  :group 'helm-config)

(defface helm-grep-lineno
    '((t (:foreground "Darkorange1")))
  "Face used to highlight grep number lines."
  :group 'helm-config)

(defface helm-grep-running
    '((t (:foreground "Red")))
  "Face used in mode line when grep is running."
  :group 'helm-config)

(defface helm-grep-finish
    '((t (:foreground "Green")))
  "Face used in mode line when grep is finish."
  :group 'helm-config)

(defface helm-M-x-key-face '((t (:foreground "orange" :underline t)))
  "*Face used in helm-M-x to show keybinding."
  :group 'helm)

(defface helm-bmkext-info
    '((t (:foreground "green")))
  "*Face used for W3m Emacs bookmarks (not w3m bookmarks)."
  :group 'helm)

(defface helm-bmkext-w3m
    '((t (:foreground "yellow")))
  "*Face used for W3m Emacs bookmarks (not w3m bookmarks)."
  :group 'helm)

(defface helm-bmkext-gnus
    '((t (:foreground "magenta")))
  "*Face used for Gnus bookmarks."
  :group 'helm)

(defface helm-bmkext-man
    '((t (:foreground "Orange4")))
  "*Face used for Woman/man bookmarks."
  :group 'helm)

(defface helm-bmkext-no--file
    '((t (:foreground "grey")))
  "*Face used for non--file bookmarks."
  :group 'helm)

(defface helm-bmkext-file
    '((t (:foreground "Deepskyblue2")))
  "*Face used for non--file bookmarks."
  :group 'helm)

(defface helm-bookmarks-su-face '((t (:foreground "red")))
  "Face for su/sudo bookmarks."
  :group 'helm)

(defface helm-w3m-bookmarks-face '((t (:foreground "cyan1" :underline t)))
  "Face for w3m bookmarks" :group 'helm)

(defface helm-emms-playlist
    '((t (:foreground "Springgreen4" :underline t)))
  "*Face used for tracks in current emms playlist."
  :group 'helm)

(defface helm-apt-installed
    '((t (:foreground "green")))
  "*Face used for apt installed candidates."
  :group 'helm)

(defface helm-apt-deinstalled
    '((t (:foreground "DimGray")))
  "*Face used for apt deinstalled candidates."
  :group 'helm)

(defface helm-gentoo-match-face '((t (:foreground "red")))
  "Face for helm-gentoo installed packages."
  :group 'traverse-faces)

(defface helm-lisp-show-completion
    '((t (:background "DarkSlateGray")))
  "*Face used for showing candidates in `helm-lisp-completion'."
  :group 'helm-config)

(defface helm-lisp-completion-info
    '((t (:foreground "red")))
  "*Face used for showing info in `helm-lisp-completion'."
  :group 'helm-config)

(defface helm-overlay-line-face '((t (:background "IndianRed4" :underline t)))
  "Face for source header in the helm buffer." :group 'helm)

;;;###autoload
(defun helm-configuration ()
  "Customize `helm'."
  (interactive)
  (customize-group "helm-config"))



;;; Helm-command-map
;;
;;
;;;###autoload
(defvar helm-command-map)
(define-prefix-command 'helm-command-map)


(define-key helm-command-map (kbd "<SPC>")     'helm-execute-helm-command)
(define-key helm-command-map (kbd "a")         'helm-c-apropos)
(define-key helm-command-map (kbd "e")         'helm-c-etags-select)
(define-key helm-command-map (kbd "l")         'helm-locate)
(define-key helm-command-map (kbd "s")         'helm-surfraw)
(define-key helm-command-map (kbd "r")         'helm-regexp)
(define-key helm-command-map (kbd "w")         'helm-w3m-bookmarks)
(define-key helm-command-map (kbd "x")         'helm-firefox-bookmarks)
(define-key helm-command-map (kbd "#")         'helm-emms)
(define-key helm-command-map (kbd "m")         'helm-man-woman)
(define-key helm-command-map (kbd "t")         'helm-top)
(define-key helm-command-map (kbd "i")         'helm-imenu)
(define-key helm-command-map (kbd "<tab>")     'helm-lisp-completion-at-point)
(define-key helm-command-map (kbd "p")         'helm-list-emacs-process)
(define-key helm-command-map (kbd "C-x r b")   'helm-c-pp-bookmarks)
(define-key helm-command-map (kbd "M-y")       'helm-show-kill-ring)
(define-key helm-command-map (kbd "C-c <SPC>") 'helm-all-mark-rings)
(define-key helm-command-map (kbd "C-x C-f")   'helm-find-files)
(define-key helm-command-map (kbd "f")         'helm-for-files)
(define-key helm-command-map (kbd "C-:")       'helm-eval-expression-with-eldoc)
(define-key helm-command-map (kbd "C-,")       'helm-calcul-expression)
(define-key helm-command-map (kbd "M-x")       'helm-M-x)
(define-key helm-command-map (kbd "C-x C-w")   'helm-write-file)
(define-key helm-command-map (kbd "C-x i")     'helm-insert-file)
(define-key helm-command-map (kbd "M-s o")     'helm-occur)
(define-key helm-command-map (kbd "M-g s")     'helm-do-grep)
(define-key helm-command-map (kbd "c")         'helm-colors)
(define-key helm-command-map (kbd "F")         'helm-select-xfont)
(define-key helm-command-map (kbd "C-c f")     'helm-recentf)
(define-key helm-command-map (kbd "C-c g")     'helm-google-suggest)
(define-key helm-command-map (kbd "h i")       'helm-info-at-point)
(define-key helm-command-map (kbd "h r")       'helm-info-emacs)
(define-key helm-command-map (kbd "h g")       'helm-info-gnus)
(define-key helm-command-map (kbd "C-x C-b")   'helm-buffers-list)
(define-key helm-command-map (kbd "C-c C-b")   'helm-browse-code)
(define-key helm-command-map (kbd "C-x r i")   'helm-register)
(define-key helm-command-map (kbd "C-c C-x")   'helm-c-run-external-command)

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
  (define-key map "\C-r" 'helm-minibuffer-history))



;;; Menu
;;
;;
(easy-menu-define nil global-map
  "`helm' menu"
  '("Helm"
    ["All helm commands" helm-execute-helm-command t]
    ["Find any Files/Buffers" helm-for-files t]
    ["Helm Everywhere (Toggle)" helm-mode t]
    "----"
    ("Files:"
     ["Find files" helm-find-files t]
     ["Recent Files" helm-recentf t]
     ["Locate" helm-locate t]
     ["Bookmarks" helm-c-pp-bookmarks t])
    ("Buffers:"
     ["Find buffers" helm-buffers-list t])
    ("Commands:"
     ["Emacs Commands" helm-M-x t]
     ["Externals Commands" helm-c-run-external-command t])
    ("Help:"
     ["Helm Apropos" helm-c-apropos t])
    ("Info:"
     ["Info at point" helm-info-at-point t]
     ["Emacs Manual index" helm-info-emacs t]
     ["Gnus Manual index" helm-info-gnus t])
    ("Org:"
     ["Org keywords" helm-org-keywords t]
     ["Org headlines" helm-org-headlines t])
    ("Tools:"
     ["Occur" helm-occur t]
     ["Grep" helm-do-grep t]
     ["Etags" helm-c-etags-select t]
     ["Lisp complete at point" helm-lisp-completion-at-point t]
     ["Browse Kill ring" helm-show-kill-ring t]
     ["Browse register" helm-register t]
     ["Browse code" helm-browse-code t]
     ["Mark Ring" helm-all-mark-rings t]
     ["Regexp handler" helm-regexp t]
     ["Colors & Faces" helm-colors t]
     ["Show xfonts" helm-select-xfont t]
     ["Ucs Symbols" helm-ucs t]
     ["Imenu" helm-imenu t]
     ["Google Suggest" helm-google-suggest t]
     ["Eval expression" helm-eval-expression-with-eldoc t]
     ["Calcul expression" helm-calcul-expression t]
     ["Man pages" helm-man-woman t]
     ["Top externals process" helm-top t]
     ["Emacs internals process" helm-list-emacs-process t])
    "----"
    ["Prefered Options" helm-configuration t]))

;;; Helm map add ons
;;
;;
(define-key helm-map (kbd "C-x C-f") 'helm-quit-and-find-file)
(define-key helm-map (kbd "M-m")     'helm-toggle-all-marks)
(define-key helm-map (kbd "C-w")     'helm-yank-text-at-point)


;;; Specialized keymaps
;;
;;
(defvar helm-c-buffer-map
  (let ((map (copy-keymap helm-map)))
    (define-key map (kbd "C-c ?")     'helm-c-buffer-help)
    ;; No need to have separate command for grep and zgrep
    ;; as we don't use recursivity for buffers.
    ;; So use zgrep for both as it is capable to handle non--compressed files.
    (define-key map (kbd "M-g s")     'helm-buffer-run-zgrep)
    (define-key map (kbd "C-c o")     'helm-buffer-switch-other-window)
    (define-key map (kbd "C-c C-o")   'helm-buffer-switch-other-frame)
    (define-key map (kbd "C-c =")     'helm-buffer-run-ediff)
    (define-key map (kbd "M-=")       'helm-buffer-run-ediff-merge)
    (define-key map (kbd "C-=")       'helm-buffer-diff-persistent)
    (define-key map (kbd "M-U")       'helm-buffer-revert-persistent)
    (define-key map (kbd "M-D")       'helm-buffer-run-kill-buffers)
    (define-key map (kbd "C-x C-s")   'helm-buffer-save-persistent)
    (define-key map (kbd "C-M-%")     'helm-buffer-run-query-replace-regexp)
    (define-key map (kbd "M-%")       'helm-buffer-run-query-replace)
    (define-key map (kbd "M-m")       'helm-toggle-all-marks)
    (define-key map (kbd "M-a")       'helm-mark-all)
    (when (locate-library "elscreen")
      (define-key map (kbd "<C-tab>") 'helm-buffer-switch-to-elscreen))
    (delq nil map))
  "Keymap for buffer sources in helm.")

(defvar helm-find-files-map
  (let ((map (copy-keymap helm-map)))
    (define-key map (kbd "C-]")           'helm-ff-run-toggle-basename)
    (define-key map (kbd "C-x C-f")       'helm-ff-run-locate)
    (define-key map (kbd "M-g s")         'helm-ff-run-grep)
    (define-key map (kbd "M-g p")         'helm-ff-run-pdfgrep)
    (define-key map (kbd "M-g z")         'helm-ff-run-zgrep)
    (define-key map (kbd "M-.")           'helm-ff-run-etags)
    (define-key map (kbd "M-R")           'helm-ff-run-rename-file)
    (define-key map (kbd "M-C")           'helm-ff-run-copy-file)
    (define-key map (kbd "M-B")           'helm-ff-run-byte-compile-file)
    (define-key map (kbd "M-L")           'helm-ff-run-load-file)
    (define-key map (kbd "M-S")           'helm-ff-run-symlink-file)
    (define-key map (kbd "M-H")           'helm-ff-run-hardlink-file)
    (define-key map (kbd "M-D")           'helm-ff-run-delete-file)
    (define-key map (kbd "M-K")           'helm-ff-run-kill-buffer-persistent)
    (define-key map (kbd "C-d")           'helm-ff-persistent-delete)
    (define-key map (kbd "M-e")           'helm-ff-run-switch-to-eshell)
    (define-key map (kbd "<M-tab>")       'helm-ff-run-complete-fn-at-point)
    (define-key map (kbd "C-c o")         'helm-ff-run-switch-other-window)
    (define-key map (kbd "C-c C-o")       'helm-ff-run-switch-other-frame)
    (define-key map (kbd "C-c C-x")       'helm-ff-run-open-file-externally)
    (define-key map (kbd "M-!")           'helm-ff-run-eshell-command-on-file)
    (define-key map (kbd "C-=")           'helm-ff-run-ediff-file)
    (define-key map (kbd "C-c =")         'helm-ff-run-ediff-merge-file)
    (define-key map (kbd "M-p")           'helm-ff-run-switch-to-history)
    (define-key map (kbd "M-i")           'helm-ff-properties-persistent)
    (define-key map (kbd "C-c ?")         'helm-ff-help)
    (define-key map (kbd "C-}")           'helm-narrow-window)
    (define-key map (kbd "C-{")           'helm-enlarge-window)
    (define-key map (kbd "C-<backspace>") 'helm-ff-run-toggle-auto-update)
    (define-key map (kbd "M-a")           'helm-mark-all)
    (define-key map (kbd "M-m")           'helm-toggle-all-marks)
    (define-key map (kbd "M-u")           'helm-unmark-all)
    (define-key map (kbd "C-c C-a")       'helm-ff-run-gnus-attach-files)
    (define-key map (kbd "C-c p")         'helm-ff-run-print-file)
    ;; Next 2 have no effect if candidate is not an image file.
    (define-key map (kbd "M-l")           'helm-ff-rotate-left-persistent)
    (define-key map (kbd "M-r")           'helm-ff-rotate-right-persistent)
    (define-key map (kbd "C-.")           'helm-find-files-down-one-level)
    (define-key map (kbd "C-l")           'helm-find-files-down-one-level)
    (define-key map (kbd "C-h C-b")       'helm-send-bug-report-from-helm)
    (define-key map (kbd "C-h C-d")       'helm-debug-output)
    (when helm-ff-lynx-style-map
      (define-key map (kbd "<left>")      'helm-find-files-down-one-level)
      (define-key map (kbd "<right>")     'helm-execute-persistent-action))
    (delq nil map))
  "Keymap for `helm-find-files'.")

(defvar helm-c-read-file-map
  (let ((map (copy-keymap helm-map)))
    (define-key map (kbd "C-]")           'helm-ff-run-toggle-basename)
    (define-key map (kbd "C-.")           'helm-find-files-down-one-level)
    (define-key map (kbd "C-l")           'helm-find-files-down-one-level)
    (define-key map (kbd "C-<backspace>") 'helm-ff-run-toggle-auto-update)
    (define-key map (kbd "C-c ?")         'helm-read-file-name-help)
    (when helm-ff-lynx-style-map
      (define-key map (kbd "<left>")      'helm-find-files-down-one-level)
      (define-key map (kbd "<right>")     'helm-execute-persistent-action)
      (define-key map (kbd "C-o")         nil)
      (define-key map (kbd "<M-left>")    'helm-previous-source)
      (define-key map (kbd "<M-right>")   'helm-next-source))
    (delq nil map))
  "Keymap for `helm-c-read-file-name'.")

(defvar helm-generic-files-map
  (let ((map (copy-keymap helm-map)))
    (define-key map (kbd "M-g s")   'helm-ff-run-grep)
    (define-key map (kbd "M-g z")   'helm-ff-run-zgrep)
    (define-key map (kbd "M-g p")   'helm-ff-run-pdfgrep)
    (define-key map (kbd "M-D")     'helm-ff-run-delete-file)
    (define-key map (kbd "C-=")     'helm-ff-run-ediff-file)
    (define-key map (kbd "C-c =")   'helm-ff-run-ediff-merge-file)
    (define-key map (kbd "C-c o")   'helm-ff-run-switch-other-window)
    (define-key map (kbd "M-i")     'helm-ff-properties-persistent)
    (define-key map (kbd "C-c C-x") 'helm-ff-run-open-file-externally)
    (define-key map (kbd "C-w")     'helm-yank-text-at-point)
    (define-key map (kbd "C-c ?")   'helm-generic-file-help)
    map)
  "Generic Keymap for files.")

(defvar helm-c-grep-map
  (let ((map (copy-keymap helm-map)))
    (define-key map (kbd "M-<down>") 'helm-c-goto-next-file)
    (define-key map (kbd "M-<up>")   'helm-c-goto-precedent-file)
    (define-key map (kbd "C-c o")    'helm-c-grep-run-other-window-action)
    (define-key map (kbd "C-w")      'helm-yank-text-at-point)
    (define-key map (kbd "C-x C-s")  'helm-c-grep-run-save-buffer)
    (when helm-c-grep-use-ioccur-style-keys
      (define-key map (kbd "<right>")  'helm-c-grep-run-persistent-action)
      (define-key map (kbd "<left>")  'helm-c-grep-run-default-action))
    (define-key map (kbd "C-c ?")    'helm-grep-help)
    (delq nil map))
  "Keymap used in Grep sources.")

(defvar helm-c-pdfgrep-map
  (let ((map (copy-keymap helm-map)))
    (define-key map (kbd "M-<down>") 'helm-c-goto-next-file)
    (define-key map (kbd "M-<up>")   'helm-c-goto-precedent-file)
    (define-key map (kbd "C-w")      'helm-yank-text-at-point)
    (define-key map (kbd "C-c ?")    'helm-pdfgrep-help)
    map)
  "Keymap used in pdfgrep.")

(defvar helm-c-etags-map
  (let ((map (copy-keymap helm-map)))
    (define-key map (kbd "M-<down>") 'helm-c-goto-next-file)
    (define-key map (kbd "M-<up>")   'helm-c-goto-precedent-file)
    (define-key map (kbd "C-w")      'helm-yank-text-at-point)
    (define-key map (kbd "C-c ?")    'helm-etags-help)
    map)
  "Keymap used in Etags.")

(defvar helm-eval-expression-map
  (let ((map (copy-keymap helm-map)))
    (define-key map (kbd "<C-return>") 'helm-eval-new-line-and-indent)
    (define-key map (kbd "<tab>")      'lisp-indent-line)
    (define-key map (kbd "<C-tab>")    'lisp-complete-symbol)
    (define-key map (kbd "C-p")        'previous-line)
    (define-key map (kbd "C-n")        'next-line)
    (define-key map (kbd "<up>")       'previous-line)
    (define-key map (kbd "<down>")     'next-line)
    (define-key map (kbd "<right>")    'forward-char)
    (define-key map (kbd "<left>")     'backward-char)
    map))

(defvar helm-c-ucs-map
  (let ((map (copy-keymap helm-map)))
    (define-key map (kbd "<C-backspace>") 'helm-c-ucs-persistent-delete)
    (define-key map (kbd "<C-left>")      'helm-c-ucs-persistent-backward)
    (define-key map (kbd "<C-right>")     'helm-c-ucs-persistent-forward)
    (define-key map (kbd "<C-return>")    'helm-c-ucs-persistent-insert)
    (define-key map (kbd "C-c ?")         'helm-c-ucs-help)
    map)
  "Keymap for `helm-ucs'.")

(defvar helm-c-bookmark-map
  (let ((map (copy-keymap helm-map)))
    (define-key map (kbd "C-c o") 'helm-c-bookmark-run-jump-other-window)
    (define-key map (kbd "C-d")   'helm-c-bookmark-run-delete)
    (when (locate-library "bookmark-extensions")
      (define-key map (kbd "M-e") 'helm-c-bmkext-run-edit))
    (define-key map (kbd "C-c ?") 'helm-c-bookmark-help)
    (delq nil map))
  "Generic Keymap for emacs bookmark sources.")

(defvar helm-esh-on-file-map
  (let ((map (copy-keymap helm-map)))
    (define-key map (kbd "C-c ?")    'helm-esh-help)
    map)
  "Keymap for `helm-find-files-eshell-command-on-file'.")

(defvar helm-eshell-history-map
  (let ((map (copy-keymap helm-map)))
    (define-key map (kbd "M-p") 'helm-next-line)
    map)
  "Keymap for `helm-eshell-history'.")

(defvar helm-kill-ring-map
  (let ((map (copy-keymap helm-map)))
    (define-key map (kbd "M-y") 'helm-next-line)
    (define-key map (kbd "M-u") 'helm-previous-line)
    map)
  "Keymap for `helm-show-kill-ring'.")

(defvar helm-occur-map
  (let ((map (copy-keymap helm-map)))
    (define-key map (kbd "C-M-%") 'helm-occur-run-query-replace-regexp)
    map)
  "Keymap for `helm-occur'.")


;;; Embeded documentation.
;;
;;
(defun helm-c-list-preconfigured-helm ()
  "Collect preconfigured helm functions in this file."
  (loop with doc
        with sym
        for entry in (cdr (assoc
                           (file-truename (locate-library "helm-config"))
                           load-history))
        if (and (consp entry)
                (eq (car entry) 'defun)
                (string-match "^Preconfigured.+$"
                              (setq doc (or (documentation (setq sym (cdr entry)))
                                            ""))))
        collect (cons sym (match-string 0 doc))))

(defun helm-c-format-preconfigured-helm ()
  (mapcar (lambda (x) (format "\\[%s] : %s\n" (car x) (cdr x)))
          (helm-c-list-preconfigured-helm)))

;;; Global help message - Used by `helm-help'
;;
;;
(setq helm-help-message
      (lambda ()
        (concat
         "\\<helm-map>"
         "`helm' is QuickSilver-like candidate-selection framework.

Narrow the list by typing some pattern,
Multiple patterns are allowed by splitting by space.
Select with natural Emacs operations, choose with RET.

If you have any problems, press C-c C-x C-b!!
Feel free to send bug reports. I'll fix them.
The steps are described in the beginning of helm.el file.

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
C-z : Persistent Action (Execute action with helm session kept)
C-c C-x C-b: Send a bug report

== Shortcuts For 2nd/3rd Action ==
\\[helm-select-2nd-action-or-end-of-line] : Execute 2nd Action (if the minibuffer cursor is at end of line)
\\[helm-select-3rd-action] : Execute 3rd Action

== Visible Marks ==
Visible marks store candidate. Some actions uses marked candidates.

\\[helm-toggle-visible-mark] : Toggle Visible Mark
\\[helm-prev-visible-mark] : Previous Mark
\\[helm-next-visible-mark] : Next Mark

== Miscellaneous Commands ==
\\[helm-toggle-resplit-window] : Toggle vertical/horizontal split helm window
\\[helm-quit-and-find-file] : Drop into `find-file'
\\[helm-delete-current-selection] : Delete Selected Item (visually)
\\[helm-kill-selection-and-quit] : Set Item Into the kill-ring And Quit
\\[helm-yank-selection] : Yank Selected Item Into Pattern
\\[helm-follow-mode] : Toggle Automatical Execution Of Persistent Action
\\[helm-force-update] : Recalculate And Redisplay Candidates

== Global Commands ==
\\<global-map>\\[helm-resume] revives last `helm' session.
It is very useful, so you should bind any key.

Single source is executed by \\[helm-call-source].

== Preconfigured `helm' ==
Preconfigured `helm' is commands that uses `helm' interface.
You can use them without configuration.

"
         (apply 'concat (helm-c-format-preconfigured-helm))
         "
Enjoy!")))

;;; `helm-buffer-list' help
;;
;;
(defvar helm-c-buffer-help-message
  "== Helm Buffer ==
\nTips:
You can enter a partial name of major-mode (e.g lisp, sh) to narrow down buffers.
Enter then a space and a pattern to narrow down to buffers matching this pattern. 
\nSpecific commands for `helm-buffers-list':
\\<helm-c-buffer-map>
\\[helm-buffer-run-zgrep]\t\t->Grep Buffer(s) works as zgrep too. (C-u grep all buffers but non--file buffers).
\\[helm-buffer-switch-other-window]\t\t->Switch other window.
\\[helm-buffer-switch-other-frame]\t\t->Switch other frame.
\\[helm-buffer-run-query-replace-regexp]\t\t->Query replace regexp in marked buffers.
\\[helm-buffer-run-query-replace]\t\t->Query replace in marked buffers.
\\[helm-buffer-switch-to-elscreen]\t\t->Find buffer in Elscreen.
\\[helm-buffer-run-ediff]\t\t->Ediff current buffer with candidate.  If two marked buffers ediff those buffers.
\\[helm-buffer-run-ediff-merge]\t\t->Ediff merge current buffer with candidate.  If two marked buffers ediff merge those buffers.
\\[helm-buffer-diff-persistent]\t\t->Toggle Diff buffer with saved file without quitting.
\\[helm-buffer-revert-persistent]\t\t->Revert buffer without quitting.
\\[helm-buffer-save-persistent]\t\t->Save buffer without quitting.
\\[helm-buffer-run-kill-buffers]\t\t->Delete marked buffers and quit.
\\[helm-toggle-all-marks]\t\t->Toggle all marks.
\\[helm-mark-all]\t\t->Mark all.
\\[helm-c-buffer-help]\t\t->Display this help.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-c-buffer-help ()
  "Help command for helm buffers."
  (interactive)
  (let ((helm-help-message helm-c-buffer-help-message))
    (helm-help)))

;;; Find files help (`helm-find-files')
;;
;;
(defvar helm-ff-help-message
  "== Helm Find Files ==
\nTips:
\n- Enter `~/' at end of pattern to quickly reach home directory.
- Enter `/' at end of pattern to quickly reach root of your file system.
- Enter `./' at end of pattern to quickly reach `default-directory' (initial start of session).
- You can complete with partial basename \(e.g \"fb\" will complete \"foobar\"\).
- Use `C-u C-z' to watch an image.
- To browse images directories turn on `helm-follow-mode' and navigate with arrow keys.
- When entered ediff, hitting `C-g' will ask you to use locate to find the file to ediff with.
 
\nSpecific commands for `helm-find-files':
\\<helm-find-files-map>
\\[helm-ff-run-locate]\t\t->Run Locate on basename of candidate (C-u to specify locate db).
\\[helm-ff-run-grep]\t\t->Run Grep (C-u Recursive).
\\[helm-ff-run-pdfgrep]\t\t->Run Pdfgrep on marked files.
\\[helm-ff-run-zgrep]\t\t->Run zgrep (C-u Recursive).
\\[helm-ff-run-etags]\t\t->Run Etags (C-u use thing-at-point `C-u C-u' reload cache)
\\[helm-ff-run-rename-file]\t\t->Rename File (C-u Follow).
\\[helm-ff-run-copy-file]\t\t->Copy File (C-u Follow).
\\[helm-ff-run-byte-compile-file]\t\t->Byte Compile File (C-u Load).
\\[helm-ff-run-load-file]\t\t->Load File.
\\[helm-ff-run-symlink-file]\t\t->Symlink File.
\\[helm-ff-run-hardlink-file]\t\t->Hardlink file.
\\[helm-ff-run-delete-file]\t\t->Delete File.
\\[helm-ff-run-kill-buffer-persistent]\t\t->Kill buffer candidate without quitting.
\\[helm-ff-persistent-delete]\t\t->Delete file without quitting.
\\[helm-ff-run-switch-to-eshell]\t\t->Switch to Eshell.
\\[helm-ff-run-eshell-command-on-file]\t\t->Eshell command on file (C-u Run on all marked files at once).
\\[helm-ff-run-ediff-file]\t\t->Ediff file.
\\[helm-ff-run-ediff-merge-file]\t\t->Ediff merge file.
\\[helm-ff-run-complete-fn-at-point]\t\t->Complete file name at point.
\\[helm-ff-run-switch-other-window]\t\t->Switch other window.
\\[helm-ff-run-switch-other-frame]\t\t->Switch other frame.
\\[helm-ff-run-open-file-externally]\t\t->Open file with external program (C-u to choose).
\\[helm-ff-rotate-left-persistent]\t\t->Rotate Image Left.
\\[helm-ff-rotate-right-persistent]\t\t->Rotate Image Right.
\\[helm-find-files-down-one-level]\t\t->Go down precedent directory.
\\[helm-ff-run-switch-to-history]\t\t->Switch to helm find-files history.
\\[helm-ff-properties-persistent]\t\t->Show file properties in a tooltip.
\\[helm-mark-all]\t\t->Mark all visibles candidates.
\\[helm-ff-run-toggle-auto-update]\t->Toggle auto expansion of directories.
\\[helm-unmark-all]\t\t->Unmark all candidates, visibles and invisibles.
\\[helm-ff-run-gnus-attach-files]\t\t->Gnus attach files to message buffer.
\\[helm-ff-run-print-file]\t\t->Print file, (C-u to refresh printers list).
\\[helm-enlarge-window]\t\t->Enlarge helm window.
\\[helm-narrow-window]\t\t->Narrow helm window.
\\[helm-ff-run-toggle-basename]\t\t->Toggle basename/fullpath.
\\[helm-send-bug-report-from-helm]\t\t->Send Bug report.
\\[helm-ff-help]\t\t->Display this help info.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-ff-help ()
  "Help command for `helm-find-files'."
  (interactive)
  (let ((helm-help-message helm-ff-help-message))
    (helm-help)))

;;; Help for `helm-c-read-file-name'
;;
;;
(defvar helm-read-file-name-help-message
  "== Helm read file name Map ==\
\nSpecific commands for helm-c-read-file-name:
\\<helm-c-read-file-map>
\\[helm-find-files-down-one-level]\t\t->Go down precedent directory.
\\[helm-ff-run-toggle-auto-update]\t->Toggle auto expansion of directories.
\\[helm-next-source]\t->Goto next source.
\\[helm-previous-source]\t->Goto previous source.
\\[helm-read-file-name-help]\t\t->Display this help info.
\n== Helm Map ==
\\{helm-map}")
  
;;;###autoload
(defun helm-read-file-name-help ()
  (interactive)
  (let ((helm-help-message helm-read-file-name-help-message))
    (helm-help)))

;;; Generic file help - Used by locate.
;;
;;
(defvar helm-generic-file-help-message
  "== Helm Generic files Map ==\
\nSpecific commands for helm locate and others files sources:
\\<helm-generic-files-map>
\\[helm-ff-run-grep]\t\t->Run grep (C-u recurse).
\\[helm-ff-run-pdfgrep]\t\t->Run Pdfgrep on marked files.
\\[helm-ff-run-delete-file]\t\t->Delete file.
\\[helm-ff-run-ediff-file]\t\t->Ediff file.
\\[helm-ff-run-ediff-merge-file]\t\t->Ediff merge file.
\\[helm-ff-run-switch-other-window]\t\t->Switch other window.
\\[helm-ff-properties-persistent]\t\t->Show file properties.
\\[helm-yank-text-at-point]\t\t->Yank text at point.
\\[helm-ff-run-open-file-externally]\t\t->Open file with external program (C-u to choose).
\nLocate tips:
You can add after writing search pattern any of the locate command line options.
e.g -b, -e, -n <number>...etc.
See Man locate for more infos.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-generic-file-help ()
  (interactive)
  (let ((helm-help-message helm-generic-file-help-message))
    (helm-help)))

;;; Grep help
;;
;;
(defvar helm-grep-help-message
  "== Helm Grep Map ==\
\nHelm Grep tips:
You can start grep with a prefix arg to recurse in subdirectories.
You can use wild card when selecting files (e.g *.el)
You can grep in many differents directories by marking files or wild cards.
You can save your results in a grep-mode buffer, see below.

\nSpecific commands for Helm Grep:
\\<helm-c-grep-map>
\\[helm-c-goto-next-file]\t->Next File.
\\[helm-c-goto-precedent-file]\t\t->Precedent File.
\\[helm-yank-text-at-point]\t\t->Yank Text at point in minibuffer.
\\[helm-c-grep-run-other-window-action]\t\t->Jump other window.
\\[helm-c-grep-run-persistent-action]\t\t->Run persistent action (Same as `C-z').
\\[helm-c-grep-run-default-action]\t\t->Run default action (Same as RET).
\\[helm-c-grep-run-save-buffer]\t\t->Save to a `grep-mode' enabled buffer.
\\[helm-grep-help]\t\t->Show this help.
\n== Helm Map ==
\\{helm-map}")
  
;;;###autoload
(defun helm-grep-help ()
  (interactive)
  (let ((helm-help-message helm-grep-help-message))
    (helm-help)))

;;; Pdf grep help
;;
;;
(defvar helm-pdfgrep-help-message
  "== Helm PdfGrep Map ==\
\nSpecific commands for Pdf Grep:
\\<helm-c-pdfgrep-map>
\\[helm-c-goto-next-file]\t->Next File.
\\[helm-c-goto-precedent-file]\t\t->Precedent File.
\\[helm-yank-text-at-point]\t\t->Yank Text at point in minibuffer.
\\[helm-pdfgrep-help]\t\t->Show this help.
\n== Helm Map ==
\\{helm-map}")
  
;;;###autoload
(defun helm-pdfgrep-help ()
  (interactive)
  (let ((helm-help-message helm-pdfgrep-help-message))
    (helm-help)))

;;; Etags help
;;
;;
(defvar helm-etags-help-message
  "== Helm Etags Map ==\
\nSpecific commands for Etags:
\\<helm-c-etags-map>
\\[helm-c-goto-next-file]\t->Next File.
\\[helm-c-goto-precedent-file]\t\t->Precedent File.
\\[helm-yank-text-at-point]\t\t->Yank Text at point in minibuffer.
\\[helm-etags-help]\t\t->Show this help.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-etags-help ()
  "The help function for etags."
  (interactive)
  (let ((helm-help-message helm-etags-help-message))
    (helm-help)))

;;; Ucs help
;;
;;
(defvar helm-c-ucs-help-message
  "== Helm Ucs ==
\nSpecific commands for `helm-ucs':
\\<helm-c-ucs-map>
\\[helm-c-ucs-persistent-insert]\t->Insert char.
\\[helm-c-ucs-persistent-forward]\t->Forward char.
\\[helm-c-ucs-persistent-backward]\t->Backward char.
\\[helm-c-ucs-persistent-delete]\t->Delete char backward.
\\[helm-c-ucs-help]\t\t->Show this help.

\n== Helm Map ==
\\{helm-map}")
  
(defun helm-c-ucs-help ()
  "Help command for `helm-ucs'."
  (interactive)
  (let ((helm-help-message helm-c-ucs-help-message))
    (helm-help)))

;;; Bookmark help
;;
;;
(defvar helm-bookmark-help-message
  "== Helm bookmark name Map ==\
\nSpecific commands for bookmarks:
\\<helm-c-bookmark-map>
\\[helm-c-bookmark-run-jump-other-window]\t\t->Jump other window.
\\[helm-c-bookmark-run-delete]\t\t->Delete bookmark.
\\[helm-c-bmkext-run-edit]\t\t->Edit bookmark (only for bmkext).
\\[helm-c-bookmark-help]\t\t->Run this help.
\n== Helm Map ==
\\{helm-map}")

(defun helm-c-bookmark-help ()
  "Help command for bookmarks."
  (interactive)
  (let ((helm-help-message helm-bookmark-help-message))
    (helm-help)))

;;; Eshell command on file help
;;
;;
(defvar helm-c-esh-help-message
  "== Helm eshell on file ==
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

Please restart and use a prefix arg to call `helm-find-files-eshell-command-on-file'.
Otherwise your command will be called many times like this:

<command> file1 <command> file2 etc...

\nSpecific commands for `helm-find-files-eshell-command-on-file':
\\<helm-esh-on-file-map>
\\[helm-esh-help]\t\t->Display this help.
\n== Helm Map ==
\\{helm-map}")

(defun helm-esh-help ()
  "Help command for `helm-find-files-eshell-command-on-file'."
  (interactive)
  (let ((helm-help-message helm-c-esh-help-message))
    (helm-help)))


;;; Mode line strings
;;
;;
(defvar helm-buffer-mode-line-string
  '("Buffer(s)"
    "\\<helm-c-buffer-map>\
\\[helm-c-buffer-help]:Help, \
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct,\
\\[helm-send-bug-report-from-helm]:BugReport."
    "String displayed in mode-line in `helm-c-source-buffers-list'"))

(defvar helm-ff-mode-line-string
  "\\<helm-find-files-map>\
\\[helm-ff-help]:Help, \
\\[helm-send-bug-report-from-helm]:BugReport, \
\\<helm-map>\
\\[helm-select-action]:Acts, \
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
  "String displayed in mode-line in `helm-c-source-find-files'")

(defvar helm-read-file-name-mode-line-string
  "\\<helm-c-read-file-map>\
\\[helm-read-file-name-help]:Help, \
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
  "String displayed in mode-line in `helm-c-source-find-files'")

(defvar helm-generic-file-mode-line-string
  "\\<helm-generic-files-map>\
\\[helm-generic-file-help]:Help, \
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct,\
\\[helm-send-bug-report-from-helm]:BugReport."
  "String displayed in mode-line in Locate.")

(defvar helm-grep-mode-line-string
  "\\<helm-c-grep-map>\
\\[helm-grep-help]:Help,\
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct,\
\\[helm-send-bug-report-from-helm]:BugReport."
  "String displayed in mode-line in `helm-do-grep'.")

(defvar helm-pdfgrep-mode-line-string
  "\\<helm-c-pdfgrep-map>\
\\[helm-pdfgrep-help]:Help,\
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct,\
\\[helm-send-bug-report-from-helm]:BugReport."
  "String displayed in mode-line in `helm-do-pdfgrep'.")

(defvar helm-etags-mode-line-string
  "\\<helm-c-etags-map>\
\\[helm-etags-help]:Help,\
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct,\
\\[helm-send-bug-report-from-helm]:BugReport."
  "String displayed in mode-line in `helm-c-etags-select'.")


(defvar helm-c-ucs-mode-line-string
  "\\<helm-c-ucs-map>\
\\[helm-c-ucs-help]:Help, \
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct."
  "String displayed in mode-line in `helm-ucs'.")

(defvar helm-bookmark-mode-line-string
  '("Bookmark(s)"
    "\\<helm-c-bookmark-map>\
\\[helm-c-bookmark-help]:Help, \
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct,\
\\[helm-send-bug-report-from-helm]:BugReport."
    "String displayed in mode-line in `helm-c-source-buffers-list'"))

(defvar helm-occur-mode-line
  "\\<helm-map>\
\\[helm-help]:Help,\
\\<helm-occur-map>\
\\[helm-occur-run-query-replace-regexp]:Query replace regexp,\
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct,\
\\[helm-send-bug-report-from-helm]:BugReport.")


;;; Utilities Functions
;;
;;
(defun helm-ff-find-printers ()
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
(defun helm-c-switch-to-buffer (buffer-or-name)
  "Same as `switch-to-buffer' whithout warnings at compile time."
  (with-no-warnings
    (switch-to-buffer buffer-or-name)))

(defun* helm-c-position (item seq &key (test 'eq) all)
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

(defun helm-c-get-pid-from-process-name (process-name)
  "Get pid from running process PROCESS-NAME."
  (loop with process-list = (list-system-processes)
        for pid in process-list
        for process = (assoc-default 'comm (process-attributes pid))
        when (and process (string-match process-name process))
        return pid))

(defun* helm-current-buffer-narrowed-p (&optional
                                            (buffer helm-current-buffer))
  "Check if BUFFER is narrowed.
Default is `helm-current-buffer'."
  (with-current-buffer buffer
    (let ((beg (point-min))
          (end (point-max))
          (total (buffer-size)))
      (or (/= beg 1) (/= end (1+ total))))))

(defun helm-region-active-p ()
  (and transient-mark-mode mark-active (/= (mark) (point))))

(defun helm-goto-char (loc)
  "Go to char, revealing if necessary."
  (goto-char loc)
  (when (or (eq major-mode 'org-mode)
            (and (boundp 'outline-minor-mode)
                 outline-minor-mode))
    (require 'org) ; On some old Emacs versions org may not be loaded.
    (org-reveal)))

(defun helm-goto-line (lineno &optional noanim)
  "Goto LINENO opening only outline headline if needed.
Animation is used unless NOANIM is non--nil."
  (goto-char (point-min))
  (helm-goto-char (point-at-bol lineno))
  (unless noanim
    (helm-match-line-color-current-line)
    (sit-for 0.3)
    (helm-match-line-cleanup)))

(defun helm-show-this-source-only ()
  "Show all candidates of this source."
  (interactive)
  (let (helm-candidate-number-limit)
    (helm-set-source-filter
     (list (assoc-default 'name (helm-get-current-source))))))

;;;###autoload
(defun helm-test-sources ()
  "List all helm sources for test.
The output is sexps which are evaluated by \\[eval-last-sexp]."
  (interactive)
  (with-output-to-temp-buffer "*Helm Test Sources*"
    (mapc (lambda (s) (princ (format ";; (helm '%s)\n" s)))
          (apropos-internal "^helm-c-source" #'boundp))
    (pop-to-buffer standard-output)))

(defun helm-displaying-source-names ()
  "Display sources name."
  (with-current-buffer helm-buffer
    (goto-char (point-min))
    (loop with pos
          while (setq pos (next-single-property-change (point) 'helm-header))
          do (goto-char pos)
          collect (buffer-substring-no-properties (point-at-bol)(point-at-eol))
          do (forward-line 1))))

(defun helm-c-match-on-file-name (candidate)
  "Return non-nil if `helm-pattern' match basename of filename CANDIDATE."
  (string-match helm-pattern (file-name-nondirectory candidate)))

(defun helm-c-match-on-directory-name (candidate)
  "Return non-nil if `helm-pattern' match directory part of CANDIDATE."
  (helm-aif (file-name-directory candidate)
      (string-match helm-pattern it)))

(defun helm-c-match-on-basename (candidate)
  "Return non-nil if `helm-pattern' match basename of filename CANDIDATE."
  (string-match helm-pattern (helm-c-basename candidate)))

(defun helm-c-string-match (candidate)
  "Return non-nil if `helm-pattern' match CANDIDATE.
The match is done with `string-match'."
  (string-match helm-pattern candidate))

(defun helm-c-skip-entries (list regexp)
  "Remove entries which matches REGEXP from LIST."
  (remove-if (lambda (x) (and (stringp x) (string-match regexp x)))
             list))

(defun helm-c-shadow-entries (list regexp)
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

(defsubst helm-c-stringify (str-or-sym)
  "Get string of STR-OR-SYM."
  (if (stringp str-or-sym)
      str-or-sym
      (symbol-name str-or-sym)))

(defsubst helm-c-symbolify (str-or-sym)
  "Get symbol of STR-OR-SYM."
  (if (symbolp str-or-sym)
      str-or-sym
      (intern str-or-sym)))

(defun helm-c-describe-function (func)
  "FUNC is symbol or string."
  (describe-function (helm-c-symbolify func)))

(defun helm-c-describe-variable (var)
  "VAR is symbol or string."
  (describe-variable (helm-c-symbolify var)))

(defun helm-c-find-function (func)
  "FUNC is symbol or string."
  (find-function (helm-c-symbolify func)))

(defun helm-c-find-variable (var)
  "VAR is symbol or string."
  (find-variable (helm-c-symbolify var)))

(defun helm-c-kill-new (candidate &optional replace)
  "CANDIDATE is symbol or string.
See `kill-new' for argument REPLACE."
  (kill-new (helm-c-stringify candidate) replace))

(defun* helm-fast-remove-dups (seq &key (test 'eq))
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

(defadvice eval-defun (after helm-source-hack activate)
  "Allow immediate execution of helm source when evaling it.
See `helm-c-enable-eval-defun-hack'."
  (when helm-c-enable-eval-defun-hack
    (let ((varsym (save-excursion
                    (beginning-of-defun)
                    (forward-char 1)
                    (when (memq (read (current-buffer)) '(defvar setq))
                      (read (current-buffer))))))
      (when (string-match "^helm-c-source-" (symbol-name varsym))
        (helm varsym)))))
;; (progn (ad-disable-advice 'eval-defun 'after 'helm-source-hack) (ad-update 'eval-defun))


;; Move this function from helm.el and redefine here
;; to avoid an unneeded defadvice.
(defun helm-quit-and-find-file ()
  "Drop into `helm-find-files' from `helm'.
If current selection is a buffer or a file, `helm-find-files'
from its directory."
  (interactive)
  (helm-run-after-quit
   (lambda (f)
     (if (file-exists-p f)
         (helm-find-files-1 (file-name-directory f)
                                (if helm-ff-transformer-show-only-basename
                                    (helm-c-basename f) f))
         (helm-find-files-1 f)))
   (helm-aif (get-buffer (helm-get-selection))
       (or (buffer-file-name it)
           (car (rassoc it dired-buffers))
           (and (with-current-buffer it
                  (eq major-mode 'org-agenda-mode))
                org-directory
                (expand-file-name org-directory))
           default-directory)
     (let ((sel (helm-get-selection)))
       (cond ((or (file-remote-p sel)
                  (file-exists-p sel))
              (expand-file-name sel))
             ((string-match ffap-url-regexp sel)
              sel)
             (t default-directory))))))


(defmacro* helm-c-walk-directory (directory &key path (directories t) match)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Helm Applications ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Helm regexp.
;;
;;
(defvar helm-build-regexp-history nil)
(defun helm-c-query-replace-regexp (candidate)
  "Query replace regexp from `helm-regexp'.
With a prefix arg replace only matches surrounded by word boundaries,
i.e Don't replace inside a word, regexp is surrounded with \\bregexp\\b."
  (let ((regexp (funcall (helm-attr 'regexp))))
    (apply 'query-replace-regexp
           (helm-c-query-replace-args regexp))))

(defun helm-c-kill-regexp-as-sexp (candidate)
  "Kill regexp in a format usable in lisp code."
  (helm-c-regexp-kill-new
   (prin1-to-string (funcall (helm-attr 'regexp)))))

(defun helm-c-kill-regexp (candidate)
  "Kill regexp as it is in `helm-pattern'."
  (helm-c-regexp-kill-new (funcall (helm-attr 'regexp))))

(defun helm-c-query-replace-args (regexp)
  "create arguments of `query-replace-regexp' action in `helm-regexp'."
  (let ((region-only (helm-region-active-p)))
    (list
     regexp
     (query-replace-read-to regexp
                            (format "Query replace %sregexp %s"
                                    (if helm-current-prefix-arg "word " "")
                                    (if region-only "in region " ""))
                            t)
     helm-current-prefix-arg
     (when region-only (region-beginning))
     (when region-only (region-end)))))

(defvar helm-c-source-regexp
  '((name . "Regexp Builder")
    (init . (lambda ()
              (helm-candidate-buffer helm-current-buffer)))
    (candidates-in-buffer)
    (get-line . helm-c-regexp-get-line)
    (persistent-action . helm-c-regexp-persistent-action)
    (persistent-help . "Show this line")
    (multiline)
    (delayed)
    (requires-pattern . 2)
    (mode-line . "Press TAB to select action.")
    (regexp . (lambda () helm-input))
    (action . (("Kill Regexp as sexp" . helm-c-kill-regexp-as-sexp)
               ("Query Replace Regexp (C-u Not inside word.)"
                . helm-c-query-replace-regexp)
               ("Kill Regexp" . helm-c-kill-regexp)))))

(defun helm-c-regexp-get-line (s e)
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
   ;; KLUDGE: point of helm-candidate-buffer is +1 than that of helm-current-buffer.
   ;; It is implementation problem of candidates-in-buffer.
   'helm-realvalue
   (1- s)))

(defun helm-c-regexp-persistent-action (pt)
  (helm-goto-char pt)
  (helm-persistent-highlight-point))

(defun helm-c-regexp-kill-new (input)
  (kill-new input)
  (message "Killed: %s" input))

(defun helm-quote-whitespace (candidate)
  "Quote whitespace, if some, in string CANDIDATE."
  (replace-regexp-in-string " " "\\\\ " candidate))


;;; Toggle all marks.
;;
;;
;;;###autoload
(defun helm-mark-all ()
  "Mark all visible unmarked candidates in current source."
  (interactive)
  (with-helm-window
    (save-excursion
      (goto-char (helm-get-previous-header-pos))
      (helm-next-line)
      (let* ((next-head (helm-get-next-header-pos))
             (end       (and next-head
                             (save-excursion
                               (goto-char next-head)
                               (forward-line -1)
                               (point))))
             (maxpoint  (or end (point-max))))
        (while (< (point) maxpoint)
          (helm-mark-current-line)
          (let* ((prefix (get-text-property (point-at-bol) 'display))
                 (cand   (helm-get-selection))
                 (bn     (and (helm-file-completion-source-p)
                              (helm-c-basename cand)))
                 (src    (assoc-default 'name (helm-get-current-source))))
            (when (and (not (helm-this-visible-mark))
                       (not (or (string= prefix "[?]")
                                (string= prefix "[@]"))))
              ;; Don't mark possibles directories ending with . or ..
              ;; autosave files/links and non--existent file.
              (unless
                  (and (or (helm-file-completion-source-p)
                           (equal src "Files from Current Directory"))
                       (or (string-match "^\\.#.*\\|^#.*#$\\|\\.$" bn)
                           ;; We need to test here when not using a transformer
                           ;; that tag prefix (i.e on tramp)
                           (not (file-exists-p cand))))
                (helm-make-visible-mark))))
          (forward-line 1) (end-of-line))))
    (helm-mark-current-line)
    (message "%s candidates marked" (length helm-marked-candidates))))

;;;###autoload
(defun helm-unmark-all ()
  "Unmark all candidates in all sources of current helm session."
  (interactive)
  (with-helm-window
    (let ((len (length helm-marked-candidates)))
      (save-excursion
        (helm-clear-visible-mark))
      (setq helm-marked-candidates nil)
      (helm-mark-current-line)
      (message "%s candidates unmarked" len))))

;;;###autoload
(defun helm-toggle-all-marks ()
  "Toggle all marks.
Mark all visible candidates of current source or unmark all candidates
visible or invisible in all sources of current helm session"
  (interactive)
  (let ((marked (helm-marked-candidates)))
    (if (and (>= (length marked) 1)
             (with-helm-window helm-visible-mark-overlays))
        (helm-unmark-all)
        (helm-mark-all))))



;;; Buffers
;;
;;
(defun helm-c-buffer-list ()
  "Return a list of buffer names.
The first buffer in the list will be the last recently used
buffer that is not the current buffer unless
`helm-allow-skipping-current-buffer' is nil."
  (let ((buffers (mapcar 'buffer-name (buffer-list))))
    (if helm-allow-skipping-current-buffer
        (progn
          (setq buffers (remove (buffer-name helm-current-buffer) buffers))
          (append (cdr buffers) (list (car buffers))))
        buffers)))

(defvar helm-c-source-buffers
  '((name . "Buffers")
    (candidates . helm-c-buffer-list)
    (type . buffer)))

(defvar helm-c-source-buffer-not-found
  `((name . "Create buffer")
    (dummy)
    (filtered-candidate-transformer (lambda (cands source)
                                      (list helm-pattern)))
    (keymap . ,helm-map)
    (action . (lambda (candidate)
                (helm-c-switch-to-buffer (get-buffer-create candidate))))))

;;; Buffers-list (was buffers+)
;;
;;
(defun helm-c-highlight-buffers (buffers)
  "Transformer function to highlight BUFFERS list.
Should be called after others transformers i.e (boring buffers)."
  (loop with buflist = (if helm-allow-skipping-current-buffer
                           buffers
                           (cons (pop (cdr buffers)) buffers))
        for i in buflist
        for buf = (get-buffer i)
        for bfname = (buffer-file-name buf)
        collect
        (cond (;; A dired buffer.
               (rassoc buf dired-buffers)
               (propertize i 'face 'helm-ff-directory
                           'help-echo (car (rassoc buf dired-buffers))))
              ;; A buffer file modified somewhere outside of emacs.
              ((and bfname (not (file-remote-p bfname))
                    (file-exists-p bfname)
                    (not (verify-visited-file-modtime buf)))
               (propertize i 'face 'helm-buffer-saved-out
                           'help-echo bfname))
              ;; A new buffer file not already saved on disk.
              ((and bfname (not (file-remote-p bfname))
                    (not (verify-visited-file-modtime buf)))
               (propertize i 'face 'helm-buffer-not-saved
                           'help-echo bfname))
              ;; A Remote buffer file modified and not saved on disk.
              ((and bfname (file-remote-p bfname) (buffer-modified-p buf))
               (let ((prefix (propertize
                              " " 'display
                              (propertize "@ " 'face 'helm-ff-prefix))))
                 (cons (concat prefix (propertize i 'face 'helm-ff-symlink
                                                  'help-echo bfname)) i)))
              ;; A buffer file modified and not saved on disk.
              ((and bfname (buffer-modified-p buf))
               (propertize i 'face 'helm-ff-symlink
                           'help-echo bfname))
              ;; A remote buffer file not modified and saved on disk.
              ((and bfname (file-remote-p bfname))
               (let ((prefix (propertize
                              " " 'display
                              (propertize "@ " 'face 'helm-ff-prefix))))
                 (cons (concat prefix (propertize i 'face 'font-lock-type-face
                                                  'help-echo bfname)) i)))
              ;; A buffer file not modified and saved on disk.
              (bfname
               (propertize i 'face 'font-lock-type-face
                           'help-echo bfname))
              ;; Any non--file buffer.
              (t (propertize i 'face 'italic)))))


(defvar helm-c-source-buffers-list
  `((name . "Buffers")
    (candidates . helm-c-buffer-list)
    (type . buffer)
    (match helm-c-buffer-match-major-mode)
    (candidate-transformer helm-c-skip-boring-buffers
                           helm-c-highlight-buffers)
    (persistent-action . helm-c-buffers-list-persistent-action)
    (keymap . ,helm-c-buffer-map)
    (volatile)
    (mode-line . helm-buffer-mode-line-string)
    (persistent-help . "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer")))

(defvaralias 'helm-c-source-buffers+ 'helm-c-source-buffers-list)

(defun helm-c-buffer-match-major-mode (candidate)
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
              (split (split-string helm-pattern)))
          (cond ((string-match "\\s-$" helm-pattern)
                 (string-match (car split) mjm))
                ((string-match "\\s-" helm-pattern)
                 (and (string-match (car split) mjm)
                      (string-match (cadr split) cand)))
                (t (or (string-match helm-pattern mjm)
                       (string-match helm-pattern cand)))))))))

(defun helm-c-buffer-query-replace-1 (&optional regexp-flag)
  "Query replace in marked buffers.
If REGEXP-FLAG is given use `query-replace-regexp'."
  (let ((fn     (if regexp-flag 'query-replace-regexp 'query-replace))
        (prompt (if regexp-flag "Query replace regexp" "Query replace"))
        (bufs   (helm-marked-candidates)))
    (loop 
          with replace = (query-replace-read-from prompt regexp-flag)
          with tostring = (unless (consp replace)
                            (query-replace-read-to
                             replace prompt regexp-flag))
          for buf in bufs
          do
          (save-window-excursion
            (helm-c-switch-to-buffer buf)
            (save-excursion
              (let ((case-fold-search t))
                (goto-char (point-min))
                (if (consp replace)
                    (apply fn (list (car replace) (cdr replace)))
                    (apply fn (list replace tostring)))))))))

(defun helm-c-buffer-query-replace-regexp (candidate)
  (helm-c-buffer-query-replace-1 'regexp))

(defun helm-c-buffer-query-replace (candidate)
  (helm-c-buffer-query-replace-1))

(defun helm-buffer-toggle-diff (candidate)
  "Toggle diff buffer CANDIDATE with it's file."
  (if (get-buffer-window "*Diff*")
      (kill-buffer "*Diff*")
      (diff-buffer-with-file (get-buffer candidate))))

;;;###autoload
(defun helm-buffer-diff-persistent ()
  "Toggle diff buffer without quitting helm."
  (interactive)
  (helm-attrset 'diff-action 'helm-buffer-toggle-diff)
  (helm-execute-persistent-action 'diff-action))

(defun helm-buffer-revert-and-update (candidate)
  (let ((marked (helm-marked-candidates)))
    (loop for buf in marked do (helm-revert-buffer buf))
    (helm-force-update candidate)))

;;;###autoload
(defun helm-buffer-revert-persistent ()
  "Revert buffer without quitting helm."
  (interactive)
  (helm-attrset 'revert-action 'helm-buffer-revert-and-update)
  (helm-execute-persistent-action 'revert-action 'onewindow))

(defun helm-buffer-save-and-update (candidate)
  (let ((marked (helm-marked-candidates))
        (enable-recursive-minibuffers t))
    (loop for buf in marked do
          (with-current-buffer (get-buffer buf)
            (save-buffer)))
    (helm-force-update candidate)))

;;;###autoload
(defun helm-buffer-save-persistent ()
  "Save buffer without quitting helm."
  (interactive)
  (helm-attrset 'save-action 'helm-buffer-save-and-update)
  (helm-execute-persistent-action 'save-action 'onewindow))

;;;###autoload
(defun helm-buffer-run-kill-buffers ()
  "Run kill buffer action from `helm-c-source-buffers-list'."
  (interactive)
  (helm-c-quit-and-execute-action 'helm-kill-marked-buffers))

;;;###autoload
(defun helm-buffer-run-grep ()
  "Run Grep action from `helm-c-source-buffers-list'."
  (interactive)
  (helm-c-quit-and-execute-action 'helm-c-grep-buffers))

;;;###autoload
(defun helm-buffer-run-zgrep ()
  "Run Grep action from `helm-c-source-buffers-list'."
  (interactive)
  (helm-c-quit-and-execute-action 'helm-c-zgrep-buffers))

;;;###autoload
(defun helm-buffer-run-query-replace-regexp ()
  "Run Query replace regexp action from `helm-c-source-buffers-list'."
  (interactive)
  (helm-c-quit-and-execute-action 'helm-c-buffer-query-replace-regexp))

;;;###autoload
(defun helm-buffer-run-query-replace ()
  "Run Query replace action from `helm-c-source-buffers-list'."
  (interactive)
  (helm-c-quit-and-execute-action 'helm-c-buffer-query-replace))

;;;###autoload
(defun helm-buffer-switch-other-window ()
  "Run switch to other window action from `helm-c-source-buffers-list'."
  (interactive)
  (helm-c-quit-and-execute-action 'switch-to-buffer-other-window))

;;;###autoload
(defun helm-buffer-switch-other-frame ()
  "Run switch to other frame action from `helm-c-source-buffers-list'."
  (interactive)
  (helm-c-quit-and-execute-action 'switch-to-buffer-other-frame))

;;;###autoload
(defun helm-buffer-switch-to-elscreen ()
  "Run switch to elscreen  action from `helm-c-source-buffers-list'."
  (interactive)
  (helm-c-quit-and-execute-action 'helm-find-buffer-on-elscreen))

;;;###autoload
(defun helm-buffer-run-ediff ()
  "Run ediff action from `helm-c-source-buffers-list'."
  (interactive)
  (helm-c-quit-and-execute-action 'helm-ediff-marked-buffers))

(defun helm-buffer-run-ediff-merge ()
  "Run ediff action from `helm-c-source-buffers-list'."
  (interactive)
  (helm-c-quit-and-execute-action 'helm-ediff-marked-buffers-merge))

(defun helm-c-buffers-persistent-kill (buffer)
  "Persistent action to kill buffer."
  (with-current-buffer (get-buffer buffer)
    (if (and (buffer-modified-p)
             (buffer-file-name (current-buffer)))
        (progn
          (save-buffer)
          (kill-buffer buffer))
        (kill-buffer buffer)))
  (helm-delete-current-selection))

(defun helm-c-buffers-list-persistent-action (candidate)
  (if current-prefix-arg
      (helm-c-buffers-persistent-kill candidate)
      (helm-c-switch-to-buffer candidate)))


;;;; <File>
;;
;;
;;; File name history
(defvar helm-c-source-file-name-history
  '((name . "File Name History")
    (candidates . file-name-history)
    (match helm-c-match-on-basename)
    (type . file)))

;;; Files in current dir
;;
;;
(defvar helm-c-source-files-in-current-dir
  '((name . "Files from Current Directory")
    (candidates . (lambda ()
                    (with-helm-current-buffer
                      (directory-files (helm-c-current-directory)))))
    ;; volatile is not needed, I think.
    (type . file)))

(defun helm-c-highlight-files (files)
  (loop for i in files
        if (file-directory-p i)
        collect (propertize (file-name-nondirectory i)
                            'face 'helm-ff-directory
                            'help-echo (expand-file-name i))
        else
        collect (propertize (file-name-nondirectory i)
                            'face 'helm-ff-file
                            'help-echo (expand-file-name i))))

(defvar helm-c-source-files-in-current-dir+
  `((name . "Files from Current Directory")
    (candidates . (lambda ()
                    (with-helm-current-buffer
                      (directory-files (helm-c-current-directory) t))))
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (mode-line . helm-generic-file-mode-line-string)
    (candidate-transformer helm-c-highlight-files)
    ;; volatile is not needed, I think.
    (type . file)))



;;; Helm-find-files - The helm files browser.
;;
;;
;; Internal.
(defvar helm-c-find-files-doc-header " (`C-l': Go to precedent level)"
  "*The doc that is inserted in the Name header of a find-files or dired source.")
(defvar helm-ff-auto-update-flag nil
  "Internal, flag to turn on/off auto-update in `helm-find-files'.
Don't set it directly, use instead `helm-ff-auto-update-initial-value'.")
(defvar helm-ff-last-expanded nil
  "Store last expanded directory or file.")
(defvar helm-ff-default-directory nil)
(defvar helm-ff-history nil)
(defvar helm-ff-cand-to-mark nil)
(defvar helm-ff-url-regexp
  "\\`\\(news\\(post\\)?:\\|nntp:\\|mailto:\\|file:\\|\\(ftp\\|https?\\|telnet\\|gopher\\|www\\|wais\\):/?/?\\).*"
  "Same as `ffap-url-regexp' but match earlier possible url.")

(defvar helm-c-source-find-files
  `((name . "Find Files")
    (header-name . (lambda (name)
                     (concat name helm-c-find-files-doc-header)))
    ;; It is needed for filenames with capital letters
    (disable-shortcuts)
    (init . (lambda ()
              (setq helm-ff-auto-update-flag
                    helm-ff-auto-update-initial-value)))
    (candidates . helm-find-files-get-candidates)
    (filtered-candidate-transformer helm-c-find-files-transformer)
    (persistent-action . helm-find-files-persistent-action)
    (persistent-help . "Hit1 Expand Candidate, Hit2 or (C-u) Find file")
    (mode-line . helm-ff-mode-line-string)
    (volatile)
    (candidate-number-limit . 9999)
    (action-transformer . helm-find-files-action-transformer)
    (action
     . ,(delq
         nil
         `(("Find File" . helm-c-find-file-or-marked)
           ("Find file in Dired" . helm-c-point-file-in-dired)
           ,(and (locate-library "elscreen")
                 '("Find file in Elscreen"  . helm-elscreen-find-file))
           ,(and (locate-library "popwin")
                 '("Find file in popup window" . popwin:find-file))
           ("Checksum File" . helm-ff-checksum)
           ("Complete at point `M-tab'"
            . helm-c-insert-file-name-completion-at-point)
           ("Open file externally `C-c C-x, C-u to choose'"
            . helm-c-open-file-externally)
           ("Grep File(s) `M-g s, C-u Recurse'" . helm-find-files-grep)
           ("Zgrep File(s) `M-g z, C-u Recurse'" . helm-ff-zgrep)
           ("Switch to Eshell `M-e'" . helm-ff-switch-to-eshell)
           ("Etags `M-., C-u tap, C-u C-u reload tag file'" . helm-ff-etags-select)
           ("Eshell command on file(s) `M-!, C-u run on all marked at once.'"
            . helm-find-files-eshell-command-on-file)
           ("Find file as root" . helm-find-file-as-root)
           ("Find file in hex dump" . hexl-find-file)
           ("Ediff File `C-='" . helm-find-files-ediff-files)
           ("Ediff Merge File `C-c ='" . helm-find-files-ediff-merge-files)
           ("Delete File(s) `M-D'" . helm-delete-marked-files)
           ("Copy file(s) `M-C, C-u to follow'" . helm-find-files-copy)
           ("Copy file(s) Async" . helm-ff-copy-async)
           ("Rename file(s) `M-R, C-u to follow'" . helm-find-files-rename)
           ("Serial rename files" . helm-ff-serial-rename)
           ("Serial rename by symlinking files" . helm-ff-serial-rename-by-symlink)
           ("Serial rename by copying files" . helm-ff-serial-rename-by-copying)
           ("Symlink files(s) `M-S, C-u to follow'" . helm-find-files-symlink)
           ("Relsymlink file(s) `C-u to follow'" . helm-find-files-relsymlink)
           ("Hardlink file(s) `M-H, C-u to follow'" . helm-find-files-hardlink)
           ("Find file other window `C-o'" . find-file-other-window)
           ("Switch to history `M-p'" . helm-find-files-switch-to-hist)
           ("Find file other frame `C-c C-o'" . find-file-other-frame)
           ("Print File `C-c p, C-u to refresh'" . helm-ff-print)
           ("Locate `C-x C-f, C-u to specify locate db'" . helm-ff-locate))))))

(defun helm-find-files-set-prompt-for-action (action files)
  "Set prompt for action ACTION for FILES."
  (let ((len (length files)))
    (format "%s *%s File(s)\n%s to: "
            action len
            (mapconcat (lambda (f)
                         (format "- %s\n" f)) files ""))))

(defun helm-dwim-target-directory ()
  "Return value of `default-directory' of buffer in other window.
If there is only one window return the value ot `default-directory'
for current buffer."
  (with-helm-current-buffer
    (let ((num-windows (length (window-list))))
      (if (> num-windows 1)
          (save-selected-window
            (other-window 1)
            default-directory)
          (car helm-ff-history)))))

(defun helm-find-files-do-action (action)
  "Generic function for creating action from `helm-c-source-find-files'.
ACTION must be an action supported by `helm-dired-action'."
  (let* ((ifiles (mapcar 'expand-file-name ; Allow modify '/foo/.' -> '/foo'
                         (helm-marked-candidates)))
         (cand   (helm-get-selection)) ; Target
         (prompt (helm-find-files-set-prompt-for-action
                  (capitalize (symbol-name action)) ifiles))
         (parg   helm-current-prefix-arg)
         (dest   (helm-c-read-file-name
                  prompt
                  :preselect (if helm-ff-transformer-show-only-basename
                                 (helm-c-basename cand) cand)
                  :initial-input (helm-dwim-target-directory)
                  :history (helm-find-files-history :comp-read nil))))
    (helm-dired-action
     dest :files ifiles :action action :follow parg)))

(defun helm-find-files-copy (candidate)
  "Copy files from `helm-find-files'."
  (helm-find-files-do-action 'copy))

(defun helm-find-files-rename (candidate)
  "Rename files from `helm-find-files'."
  (helm-find-files-do-action 'rename))

(defun helm-find-files-symlink (candidate)
  "Symlink files from `helm-find-files'."
  (helm-find-files-do-action 'symlink))

(defun helm-find-files-relsymlink (candidate)
  "Relsymlink files from `helm-find-files'."
  (helm-find-files-do-action 'relsymlink))

(defun helm-find-files-hardlink (candidate)
  "Hardlink files from `helm-find-files'."
  (helm-find-files-do-action 'hardlink))

(defun helm-find-files-byte-compile (candidate)
  "Byte compile elisp files from `helm-find-files'."
  (let ((files    (helm-marked-candidates))
        (parg     helm-current-prefix-arg))
    (loop for fname in files
          do (byte-compile-file fname parg))))

(defun helm-find-files-load-files (candidate)
  "Load elisp files from `helm-find-files'."
  (let ((files    (helm-marked-candidates)))
    (loop for fname in files
          do (load fname))))

(defun helm-find-files-ediff-files-1 (candidate &optional merge)
  "Generic function to ediff/merge files in `helm-find-files'."
  (let ((bname  (helm-c-basename candidate))
        (prompt (if merge "Ediff Merge `%s' With File: "
                    "Ediff `%s' With File: "))
        (fun    (if merge 'ediff-merge-files 'ediff-files))) 
    (funcall fun
             candidate
             (condition-case quit
                 (helm-c-read-file-name
                  (format prompt bname))
               (quit ;; Hit C-g ask user to fallback to locate.
                (if (y-or-n-p "Search file for ediff with locate? ")
                    (helm-c-locate-read-file-name
                     (format prompt bname)
                     ;; Check if -b option is available.
                     (if (and (eq system-type 'windows-nt)
                              (string-match "^es" helm-c-locate-command))
                         bname
                         (concat bname " -b")))
                    (error "Error: Ediff Operation aborted")))))))

(defun helm-find-files-ediff-files (candidate)
  (helm-find-files-ediff-files-1 candidate))

(defun helm-find-files-ediff-merge-files (candidate)
  (helm-find-files-ediff-files-1 candidate 'merge))

(defun helm-find-files-grep (candidate)
  "Default action to grep files from `helm-find-files'."
  (helm-do-grep-1 (helm-marked-candidates)
                      helm-current-prefix-arg))

(defun helm-ff-zgrep (candidate)
  "Default action to zgrep files from `helm-find-files'."
  (let ((prefarg helm-current-prefix-arg)
        (ls      (helm-marked-candidates)))
    (helm-ff-zgrep-1 ls prefarg)))

(defun helm-ff-pdfgrep (candidate)
  "Default action to pdfgrep files from `helm-find-files'."
  (let ((cands (loop for file in (helm-marked-candidates)
                     if (or (string= (file-name-extension file) "pdf")
                            (string= (file-name-extension file) "PDF"))
                     collect file))
        (helm-c-pdfgrep-default-function 'helm-c-pdfgrep-init))
    (when cands
      (helm-do-pdfgrep-1 cands))))

(defun helm-ff-etags-select (candidate)
  "Default action to jump to etags from `helm-find-files'."
  (when (get-buffer helm-action-buffer)
    (kill-buffer helm-action-buffer))
  (let ((default-directory helm-ff-default-directory))
    (helm-c-etags-select helm-current-prefix-arg)))

(defun helm-find-files-switch-to-hist (candidate)
  "Switch to helm-find-files history."
  (helm-find-files t))

;;; Asynchronous copy of files.
;;
;;
(defun helm-c-copy-files-async-1 (flist dest)
  "Copy a list of Files FLIST to DEST asynchronously.
It use another emacs process to do the job.
Communication with background emacs is done with temp file
`helm-c-copy-files-async-log-file'."
  (start-file-process "emacs-batch" nil helm-c-copy-async-prefered-emacs
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
                              flist dest dest dest helm-c-copy-files-async-log-file dest)))

(defun helm-c-copy-async-with-log (flist dest)
  "Copy file list FLIST to DEST showing log.
Log is send to `helm-c-copy-files-async-log-file'.
Copying is done asynchronously with `helm-c-copy-files-async-1'."
  (declare (special auto-revert-interval))
  (pop-to-buffer (find-file-noselect helm-c-copy-files-async-log-file))
  (set (make-local-variable 'auto-revert-interval) 1)
  (erase-buffer)
  (insert "Wait copying files...\n")
  (sit-for 0.5) (save-buffer)
  (goto-char (point-max))
  (auto-revert-mode 1)
  (helm-c-copy-files-async-1 flist dest))

(defun helm-ff-copy-async (candidate)
  "Helm find files action to copy files async.
Copying is done asynchronously with `helm-c-copy-files-async-1'."
  (let* ((flist (helm-marked-candidates))
         (dest  (helm-c-read-file-name
                 (helm-find-files-set-prompt-for-action
                  "Copy Async" flist)
                 :preselect candidate
                 :initial-input (car helm-ff-history)
                 :history (helm-find-files-history :comp-read nil))))
    (helm-c-copy-async-with-log flist dest)))

(defvar eshell-command-aliases-list nil)
(defvar helm-eshell-command-on-file-input-history nil)
(defun helm-find-files-eshell-command-on-file-1 (candidate &optional map)
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
    (let* ((cand-list (helm-marked-candidates))
           (default-directory (or helm-ff-default-directory
                                  ;; If candidate is an url *-ff-default-directory is nil
                                  ;; so keep value of default-directory.
                                  default-directory))
           (command (helm-comp-read
                     "Command: "
                     (loop for (a . c) in eshell-command-aliases-list
                           when (string-match "\\(\\$1\\|\\$\\*\\)$" (car c))
                           collect (propertize a 'help-echo (car c)) into ls
                           finally return (sort ls 'string<))
                     :buffer "*esh command on file*"
                     :name "Eshell command"
                     :keymap helm-esh-on-file-map
                     :mode-line
                     '("Eshell alias"
                       "C-c ?: Help, \\[universal-argument]: Insert output at point")
                     :input-history
                     'helm-eshell-command-on-file-input-history))
           (alias-value (car (assoc-default command eshell-command-aliases-list))))
      (when (and (= (length cand-list) 1)
                 (string-match "[*]" (helm-c-basename (car cand-list))))
        (setq cand-list (file-expand-wildcards (car cand-list) t)))
      ;; Be sure output don't go in current buffer
      ;; but allow sending output to current buffer
      ;; if a prefix arg have been passed during the
      ;; `helm-comp-read' call.
      (setq current-prefix-arg helm-current-prefix-arg)
      ;; MAP have been set before calling `helm-comp-read' 
      ;; by `helm-current-prefix-arg'.
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
                for bn = (helm-c-basename i)
                for files = (format "'%s'" i)
                for com = (if (string-match "'%s'\\|\"%s\"\\|%s" command)
                              ;; [1] This allow to enter other args AFTER filename
                              ;; i.e <command %s some_more_args>
                              (format command files)
                              (format "%s %s" command files))
                do (eshell-command com))))))

(defun helm-find-files-eshell-command-on-file (candidate)
  "Run `eshell-command' on CANDIDATE or marked candidates.
See `helm-find-files-eshell-command-on-file-1' for more info."
  (helm-find-files-eshell-command-on-file-1
   candidate helm-current-prefix-arg))

(defun helm-ff-switch-to-eshell (candidate)
  "Switch to eshell and cd to `helm-ff-default-directory'."
  (flet ((cd-eshell ()
           (goto-char (point-max))
           (insert
            (format "cd '%s'" helm-ff-default-directory))
           (eshell-send-input)))
    (if (get-buffer "*eshell*")
        (progn
          (helm-c-switch-to-buffer "*eshell*")
          (cd-eshell))
        (call-interactively 'eshell)
        (cd-eshell))))

(defun helm-ff-serial-rename-action (method)
  "Rename all marked files to `helm-ff-default-directory' with METHOD.
See `helm-ff-serial-rename-1'."
  (let* ((cands     (helm-marked-candidates))
         (def-name  (car cands))
         (name      (read-string "NewName: "
                                 (replace-regexp-in-string
                                  "[0-9]+$" ""
                                  (helm-c-basename
                                   def-name
                                   (file-name-extension def-name)))))
         (start     (read-number "StartAtNumber: "))
         (extension (read-string "Extension: "
                                 (file-name-extension (car cands))))
         (dir       (expand-file-name
                     (helm-c-read-file-name
                      "Serial Rename to directory: "
                      :initial-input
                      (expand-file-name helm-ff-default-directory)
                      :test 'file-directory-p
                      :must-match t)))
         (res       (loop for f in cands
                          for bn = (helm-c-basename f)
                          for count from start
                          concat (format "%s <-> %s%s.%s\n"
                                         bn name count extension))))
    (if (y-or-n-p
         (format "Result:\n %sRename like this to <%s> ? " res dir))
        (progn
          (helm-ff-serial-rename-1
           dir cands name start extension :method method)
          (message nil)
          (helm-find-files-1 dir))
        (message "Operation aborted"))))

(defun helm-ff-member-directory-p (file directory)
  (let ((dir-file (expand-file-name
                   (file-name-as-directory (file-name-directory file))))
        (cur-dir  (expand-file-name (file-name-as-directory directory))))
    (string= dir-file cur-dir)))

(defun* helm-ff-serial-rename-1
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
             (helm-dired-action
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
                   do (if (helm-ff-member-directory-p i directory)
                          (rename-file i nname)
                          (funcall fn i nname)))
             ;; Now move all from tmp-dir to destination.
             (loop with dirlist = (directory-files
                                   tmp-dir t directory-files-no-dot-files-regexp)
                   for f in dirlist do
                   (if (file-symlink-p f)
                       (symlink-file (file-truename f)
                                     (concat (file-name-as-directory directory)
                                             (helm-c-basename f)))
                       (rename-file f directory))))
        (delete-directory tmp-dir t)))))

(defun helm-ff-serial-rename (candidate)
  "Serial rename all marked files to `helm-ff-default-directory'.
Rename only file of current directory, and symlink files coming from
other directories.
See `helm-ff-serial-rename-1'."
  (helm-ff-serial-rename-action 'rename))

(defun helm-ff-serial-rename-by-symlink (candidate)
  "Serial rename all marked files to `helm-ff-default-directory'.
Rename only file of current directory, and symlink files coming from
other directories.
See `helm-ff-serial-rename-1'."
  (helm-ff-serial-rename-action 'symlink))

(defun helm-ff-serial-rename-by-copying (candidate)
  "Serial rename all marked files to `helm-ff-default-directory'.
Rename only file of current directory, and copy files coming from
other directories.
See `helm-ff-serial-rename-1'."
  (helm-ff-serial-rename-action 'copy))

(defun helm-c-quit-and-execute-action (action)
  "Quit current helm session and execute ACTION." 
  (setq helm-saved-action action)
  (helm-exit-minibuffer))

(defun helm-ff-toggle-auto-update (candidate)
  (setq helm-ff-auto-update-flag (not helm-ff-auto-update-flag))
  (message "[Auto expansion %s]"
           (if helm-ff-auto-update-flag "enabled" "disabled")))

;;;###autoload
(defun helm-ff-run-toggle-auto-update ()
  (interactive)
  (when (helm-file-completion-source-p)
    (helm-attrset 'toggle-auto-update 'helm-ff-toggle-auto-update)
    (helm-execute-persistent-action 'toggle-auto-update)))

;;;###autoload
(defun helm-ff-run-switch-to-history ()
  "Run Switch to history action from `helm-c-source-find-files'."
  (interactive)
  (when (helm-file-completion-source-p)
    (helm-c-quit-and-execute-action 'helm-find-files-switch-to-hist)))

;;;###autoload
(defun helm-ff-run-grep ()
  "Run Grep action from `helm-c-source-find-files'."
  (interactive)
  (when (helm-file-completion-source-p)
    (helm-c-quit-and-execute-action 'helm-find-files-grep)))

;;;###autoload
(defun helm-ff-run-pdfgrep ()
  "Run Pdfgrep action from `helm-c-source-find-files'."
  (interactive)
  (when (helm-file-completion-source-p)
    (helm-c-quit-and-execute-action 'helm-ff-pdfgrep)))

;;;###autoload
(defun helm-ff-run-zgrep ()
  "Run Grep action from `helm-c-source-find-files'."
  (interactive)
  (when (helm-file-completion-source-p)
    (helm-c-quit-and-execute-action 'helm-ff-zgrep)))

;;;###autoload
(defun helm-ff-run-copy-file ()
  "Run Copy file action from `helm-c-source-find-files'."
  (interactive)
  (when (helm-file-completion-source-p)
    (helm-c-quit-and-execute-action 'helm-find-files-copy)))

;;;###autoload
(defun helm-ff-run-rename-file ()
  "Run Rename file action from `helm-c-source-find-files'."
  (interactive)
  (when (helm-file-completion-source-p)
    (helm-c-quit-and-execute-action 'helm-find-files-rename)))

;;;###autoload
(defun helm-ff-run-byte-compile-file ()
  "Run Byte compile file action from `helm-c-source-find-files'."
  (interactive)
  (when (helm-file-completion-source-p)
    (helm-c-quit-and-execute-action 'helm-find-files-byte-compile)))

;;;###autoload
(defun helm-ff-run-load-file ()
  "Run Load file action from `helm-c-source-find-files'."
  (interactive)
  (when (helm-file-completion-source-p)
    (helm-c-quit-and-execute-action 'helm-find-files-load-files)))

;;;###autoload
(defun helm-ff-run-eshell-command-on-file ()
  "Run eshell command on file action from `helm-c-source-find-files'."
  (interactive)
  (when (helm-file-completion-source-p)
    (helm-c-quit-and-execute-action
     'helm-find-files-eshell-command-on-file)))

;;;###autoload
(defun helm-ff-run-ediff-file ()
  "Run Ediff file action from `helm-c-source-find-files'."
  (interactive)
  (when (helm-file-completion-source-p)
    (helm-c-quit-and-execute-action 'helm-find-files-ediff-files)))

;;;###autoload
(defun helm-ff-run-ediff-merge-file ()
  "Run Ediff merge file action from `helm-c-source-find-files'."
  (interactive)
  (when (helm-file-completion-source-p)
    (helm-c-quit-and-execute-action
     'helm-find-files-ediff-merge-files)))

;;;###autoload
(defun helm-ff-run-symlink-file ()
  "Run Symlink file action from `helm-c-source-find-files'."
  (interactive)
  (when (helm-file-completion-source-p)
    (helm-c-quit-and-execute-action 'helm-find-files-symlink)))

;;;###autoload
(defun helm-ff-run-hardlink-file ()
  "Run Hardlink file action from `helm-c-source-find-files'."
  (interactive)
  (when (helm-file-completion-source-p)
    (helm-c-quit-and-execute-action 'helm-find-files-hardlink)))

;;;###autoload
(defun helm-ff-run-delete-file ()
  "Run Delete file action from `helm-c-source-find-files'."
  (interactive)
  (when (helm-file-completion-source-p)
    (helm-c-quit-and-execute-action 'helm-delete-marked-files)))

;;;###autoload
(defun helm-ff-run-complete-fn-at-point ()
  "Run complete file name action from `helm-c-source-find-files'."
  (interactive)
  (when (helm-file-completion-source-p)
    (helm-c-quit-and-execute-action
     'helm-c-insert-file-name-completion-at-point)))

;;;###autoload
(defun helm-ff-run-switch-to-eshell ()
  "Run switch to eshell action from `helm-c-source-find-files'."
  (interactive)
  (when (helm-file-completion-source-p)
    (helm-c-quit-and-execute-action 'helm-ff-switch-to-eshell)))

;;;###autoload
(defun helm-ff-run-switch-other-window ()
  "Run switch to other window action from `helm-c-source-find-files'."
  (interactive)
  (when (helm-file-completion-source-p)
    (helm-c-quit-and-execute-action 'find-file-other-window)))

;;;###autoload
(defun helm-ff-run-switch-other-frame ()
  "Run switch to other frame action from `helm-c-source-find-files'."
  (interactive)
  (when (helm-file-completion-source-p)
    (helm-c-quit-and-execute-action 'find-file-other-frame)))

;;;###autoload
(defun helm-ff-run-open-file-externally ()
  "Run open file externally command action from `helm-c-source-find-files'."
  (interactive)
  (when (helm-file-completion-source-p)
    (helm-c-quit-and-execute-action 'helm-c-open-file-externally)))

(defun helm-ff-locate (candidate)
  "Locate action function for `helm-find-files'."
  (let ((input (concat (helm-c-basename
                        (expand-file-name
                         candidate
                         helm-ff-default-directory))
                       ;; The locate '-b' option doesn't exists
                       ;; in everything.
                       (unless (and (eq system-type 'windows-nt)
                                    (string-match "^es" helm-c-locate-command))
                         " -b")))
        (helm-mp-highlight-delay 0.7))
    (helm-locate-1 helm-current-prefix-arg input 'from-ff)))

;;;###autoload
(defun helm-ff-run-locate ()
  "Run locate action from `helm-c-source-find-files'."
  (interactive)
  (when (helm-file-completion-source-p)
    (helm-c-quit-and-execute-action 'helm-ff-locate)))

;;;###autoload
(defun helm-ff-run-gnus-attach-files ()
  "Run gnus attach files command action from `helm-c-source-find-files'."
  (interactive)
  (when (helm-file-completion-source-p)
    (helm-c-quit-and-execute-action 'helm-ff-gnus-attach-files)))

;;;###autoload
(defun helm-ff-run-etags ()
  "Run Etags command action from `helm-c-source-find-files'."
  (interactive)
  (when (helm-file-completion-source-p)
    (helm-c-quit-and-execute-action 'helm-ff-etags-select)))

(defun helm-ff-print (candidate)
  "Print marked files.
You have to set in order
variables `lpr-command',`lpr-switches' and/or `printer-name'.

e.g:
\(setq lpr-command \"gtklp\"\)
\(setq lpr-switches '(\"-P\")\)
\(setq printer-name \"Epson-Stylus-Photo-R265\"\)

Same as `dired-do-print' but for helm."
  (when (or helm-current-prefix-arg
            (not helm-ff-printer-list))
    (setq helm-ff-printer-list
          (helm-ff-find-printers)))
  (let* ((file-list (helm-marked-candidates))
         (len (length file-list))
         (printer-name (if helm-ff-printer-list
                           (helm-comp-read
                            "Printer: " helm-ff-printer-list)
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
        (start-process-shell-command "helm-print" nil cmd-line)
        (error "Error: Please verify your printer settings in Emacs."))))

;;;###autoload
(defun helm-ff-run-print-file ()
  "Run Print file action from `helm-c-source-find-files'."
  (interactive)
  (when (helm-file-completion-source-p)
    (helm-c-quit-and-execute-action 'helm-ff-print)))

(defun helm-ff-checksum (file)
  "Calculate the checksum of FILE.
Provide completion on different algorithms to use on Emacs24.
On Emacs23 only 'sha1' is available.
The checksum is copied to kill-ring."
  (let ((algo-list (and (fboundp 'secure-hash)
                        '(md5 sha1 sha224 sha256 sha384 sha512))))
    (kill-new
     (if algo-list
         (secure-hash (intern
                       (helm-comp-read
                        "Algorithm: " algo-list))
                      file)
         (sha1 (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string)))))
    (message "Checksum copied to kill-ring.")))

(defun helm-ff-toggle-basename (candidate)
  (setq helm-ff-transformer-show-only-basename
        (not helm-ff-transformer-show-only-basename))
  (let ((target (if helm-ff-transformer-show-only-basename
                    (helm-c-basename candidate) candidate)))
    (helm-force-update target)))

(defun helm-ff-run-toggle-basename ()
  (interactive)
  (when (helm-file-completion-source-p)
    (helm-attrset 'toggle-basename 'helm-ff-toggle-basename)
    (helm-execute-persistent-action 'toggle-basename)))

(defun* helm-reduce-file-name (fname level &key unix-close expand)
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
(defvar helm-file-completion-sources
  '("Find Files" "Read File Name"
    "Read File Name History" "Copy Files"
    "Rename Files" "Symlink Files"
    "Hardlink Files" "Write File" "Insert File")
  "Sources that use the *find-files mechanism can be added here.
Sources generated by `helm-mode' don't need to be added here, it will
be done automatically.
You should not modify this yourself unless you know what you do.")

(defun helm-file-completion-source-p ()
  "Return non--nil if current source is a file completion source.
A source is a file completion source if it is
one of `helm-file-completion-sources'.
Return nil if helm is not running."
  (let ((cur-source (cdr (assoc 'name (helm-get-current-source)))))
    (loop for i in helm-file-completion-sources
          thereis (string= cur-source i))))

(defun helm-find-files-down-one-level (arg)
  "Go down one level like unix command `cd ..'.
If prefix numeric arg is given go ARG level down."
  (interactive "p")
  (when (and (helm-file-completion-source-p)
             (not (helm-ff-invalid-tramp-name-p)))
    (with-helm-window
      (setq helm-follow-mode nil))
    ;; When going to precedent level we want to be at the line
    ;; corresponding to actual directory, so store this info
    ;; in `helm-ff-last-expanded'.
    (if (and (not (file-directory-p helm-pattern))
             (file-exists-p helm-pattern))
        (setq helm-ff-last-expanded helm-pattern)
        (setq helm-ff-last-expanded helm-ff-default-directory))
    (let ((new-pattern (helm-reduce-file-name helm-pattern arg
                                                  :unix-close t :expand t)))
      (helm-set-pattern new-pattern))))

(defun helm-ff-retrieve-last-expanded ()
  "Move overlay to last visited directory `helm-ff-last-expanded'.
This happen after using `helm-find-files-down-one-level',
or hitting C-z on \"..\"."
  (when (and helm-ff-last-expanded
             (helm-file-completion-source-p))
    (let ((presel (if helm-ff-transformer-show-only-basename
                       (helm-c-basename
                        (directory-file-name helm-ff-last-expanded))
                       (directory-file-name helm-ff-last-expanded))))
      (with-helm-window
        (when (re-search-forward
               (concat "^" (regexp-quote presel) "$") nil t)
          (forward-line 0)
          (helm-mark-current-line)))
      (setq helm-ff-last-expanded nil))))
(add-hook 'helm-after-update-hook 'helm-ff-retrieve-last-expanded)

;; Auto-update - helm-find-files auto expansion of directories.
;;
(defun helm-ff-update-when-only-one-matched ()
  "Expand to directory when sole completion.
When only one candidate is remaining and it is a directory,
expand to this directory."
  (when (and helm-ff-auto-update-flag
             (helm-file-completion-source-p)
             (not (helm-ff-invalid-tramp-name-p)))
    (let* ((history-p   (string= (assoc-default
                                  'name (helm-get-current-source))
                                 "Read File Name History"))
           (pat         (if (string-match tramp-file-name-regexp
                                          helm-pattern)
                            (helm-create-tramp-name helm-pattern)
                            helm-pattern))
           (completed-p (string= (file-name-as-directory pat)
                                 helm-ff-default-directory)))
      (when (and (or
                  ;; Only one candidate remaining
                  ;; and at least 2 char in basename.
                  (and (<= (helm-approximate-candidate-number) 2)
                       (>= (length (helm-c-basename helm-pattern)) 2))
                  ;; Already completed.
                  completed-p)
                 (not history-p)) ; Don't try to auto complete in history.
        (with-helm-window
          (let ((cur-cand (prog2
                              (unless completed-p
                                ;; Only one non--existing candidate
                                ;; and one directory candidate, move to it.
                                (helm-next-line))
                              (helm-get-selection))))
            (when (and (stringp cur-cand) (file-directory-p cur-cand))
              (if (and (not (string-match "^.*[.]\\{1,2\\}$" cur-cand)) ; [1]
                       ;; Maybe we are here because completed-p is true
                       ;; but check this again to be sure. (Windows fix)
                       (<= (helm-approximate-candidate-number) 2)) ; [2]
                  ;; If after going to next line the candidate
                  ;; is not one of "." or ".." [1]
                  ;; and only one candidate is remaining [2],
                  ;; assume candidate is a new directory to expand, and do it.
                  (helm-set-pattern (file-name-as-directory cur-cand))
                  ;; The candidate is one of "." or ".."
                  ;; that mean we have entered the last letter of the directory name
                  ;; in prompt, so expansion is already done, just add the "/" at end
                  ;; of name unless helm-pattern ends with "."
                  ;; (i.e we are writing something starting with ".")
                  (unless (string-match "^.*[.]\\{1\\}$" helm-pattern)
                    (helm-set-pattern
                     ;; Need to expand-file-name to avoid e.g /ssh:host:./ in prompt.
                     (expand-file-name (file-name-as-directory helm-pattern)))))
              (helm-check-minibuffer-input-1))))))))
(add-hook 'helm-after-update-hook 'helm-ff-update-when-only-one-matched)

;; Allow expanding to home directory or root
;; when entering respectively "~/" or "//" at end of pattern.
;; e.g /home/thierry/labo/helm-config-qp/~/
;; will expand to "~/"
;; and /home/thierry/labo/helm-config-qp//
;; will expand to "/"
(defun helm-ff-auto-expand-to-home-or-root ()
  "Goto home, root or default directory when pattern ends with ~/, /, or ./.
This happen only in function using sources that are
`helm-file-completion-source-p' compliant."
  (when (and (helm-file-completion-source-p)
             (string-match ".*\\(/~/\\|/\\{2\\}\\|/[.]\\{1\\}/\\)$"
                           helm-pattern)
             (not (string-match helm-ff-url-regexp helm-pattern)))
    (let ((match (match-string 1 helm-pattern)))
      (cond ((string= match "//")
             ;; Expand to "/" or "c:/"
             (setq helm-pattern (expand-file-name "/")))
            ((string= match "/~/")
             (if (eq system-type 'windows-nt)
                 (setq helm-pattern (file-name-as-directory (getenv "HOME")))
                 (setq helm-pattern "~/")))
            ((string= match "/./")
             (setq helm-pattern
                   (with-helm-current-buffer
                     (expand-file-name default-directory))))))
    (setq helm-ff-default-directory helm-pattern)
    ;; For some reasons, i must use here with-current-buffer => mini buffer
    ;; and not `helm-set-pattern' that use with-selected-window => mini win.
    (with-current-buffer (window-buffer (minibuffer-window))
      (delete-minibuffer-contents)
      (insert helm-pattern))))

(add-hook 'helm-after-update-hook 'helm-ff-auto-expand-to-home-or-root)

(defun helm-c-point-file-in-dired (file)
  "Put point on filename FILE in dired buffer."
  (dired (file-name-directory file))
  (dired-goto-file file))

(defun helm-create-tramp-name (fname)
  "Build filename for `helm-pattern' like /su:: or /sudo::."
  (apply #'tramp-make-tramp-file-name
         (loop with v = (tramp-dissect-file-name fname)
               for i across v collect i)))

(defun* helm-ff-tramp-hostnames (&optional (pattern helm-pattern))
  "Get a list of hosts for tramp method found in `helm-pattern'.
Argument PATTERN default to `helm-pattern', it is here only for debugging
purpose."
  (when (string-match tramp-file-name-regexp pattern)
    (let ((method      (match-string 1 pattern))
          (tn          (match-string 0 pattern))
          (all-methods (mapcar 'car tramp-methods)))
      (helm-fast-remove-dups
       (loop for (f . h) in (tramp-get-completion-function method)
             append (loop for e in (funcall f (car h))
                          for host = (and (consp e) (cadr e))
                          when (and host (not (member host all-methods)))
                          collect (concat tn host)))
       :test 'equal))))

(defun helm-ff-before-action-hook-fn ()
  "Exit helm when user try to execute action on an invalid tramp fname."
  (let ((cand (helm-get-selection)))
    (when (and (helm-file-completion-source-p)
               (helm-ff-invalid-tramp-name-p cand) ; Check candidate.
               (helm-ff-invalid-tramp-name-p)) ; check helm-pattern.
      (error "Error: Unknow file or directory `%s'" cand))))
(add-hook 'helm-before-action-hook 'helm-ff-before-action-hook-fn)

(defun* helm-ff-invalid-tramp-name-p (&optional (pattern helm-pattern))
  "Return non--nil when PATTERN is an invalid tramp filename."
  (string= (helm-ff-set-pattern pattern)
           "Invalid tramp file name"))

(defun helm-ff-set-pattern (pattern)
  "Handle tramp filenames in `helm-pattern'."
  (let ((methods (mapcar 'car tramp-methods))
        (reg "\\`/\\([^[/:]+\\|[^/]+]\\):.*:")
        cur-method tramp-name)
    (cond ((string= pattern "") "")
          ((string-match ".*\\(~?/?[.]\\{1\\}/\\)$" pattern)
           (with-helm-current-buffer
             (expand-file-name default-directory)))
          ((and (string-match ".*\\(~//\\|//\\)$" pattern)
                (not (string-match helm-ff-url-regexp helm-pattern)))
           (expand-file-name "/") ; Expand to "/" or "c:/"
           )
          ((string-match "^~\\|.*/~/$" pattern)
           (let* ((home (expand-file-name (getenv "HOME"))))
             (replace-match home nil t pattern)))
          ;; Match "/method:maybe_hostname:"
          ((and (string-match reg pattern)
                (setq cur-method (match-string 1 pattern))
                (member cur-method methods))
           (setq tramp-name (helm-create-tramp-name
                             (match-string 0 pattern)))
           (replace-match tramp-name nil t pattern))
          ;; Match "/hostname:"
          ((and (string-match  tramp-file-name-regexp pattern)
                (setq cur-method (match-string 1 pattern))
                (and cur-method (not (member cur-method methods))))
           (setq tramp-name (helm-create-tramp-name
                             (match-string 0 pattern)))
           (replace-match tramp-name nil t pattern))
          ;; Match "/method:" in this case don't try to connect.
          ((and (not (string-match reg pattern))
                (string-match tramp-file-name-regexp pattern)
                (member (match-string 1 pattern) methods))
           "Invalid tramp file name")   ; Write in helm-buffer.
          ;; PATTERN is a directory, end it with "/".
          ;; This will make PATTERN not ending yet with "/"
          ;; candidate for `helm-ff-default-directory',
          ;; allowing `helm-ff-retrieve-last-expanded' to retrieve it
          ;; when descending level.
          ((file-directory-p pattern)
           (file-name-as-directory pattern))
          ;; Return PATTERN unchanged.
          (t pattern))))

(defun helm-find-files-get-candidates (&optional require-match)
  "Create candidate list for `helm-c-source-find-files'."
  (let* ((path          (helm-ff-set-pattern helm-pattern))
         (path-name-dir (if (file-directory-p path)
                            (file-name-as-directory path)
                            (file-name-directory path)))
         (tramp-verbose helm-tramp-verbose)) ; No tramp message when 0.
    (set-text-properties 0 (length path) nil path)
    ;; Don't set now `helm-pattern' if `path' == "Invalid tramp file name"
    ;; like that the actual value (e.g /ssh:) is passed to
    ;; `helm-ff-tramp-hostnames'.
    (unless (string= path "Invalid tramp file name")
      (setq helm-pattern (helm-ff-transform-fname-for-completion path)))
    (setq helm-ff-default-directory
          (if (string= helm-pattern "")
              (expand-file-name "/") ; Expand to "/" or "c:/"
              ;; If path is an url *default-directory have to be nil.
              (unless (or (string-match helm-ff-url-regexp path)
                          (string-match ffap-url-regexp path))
                path-name-dir)))
    (cond ((string= path "Invalid tramp file name")
           (or (helm-ff-tramp-hostnames) ; Hostnames completion.
               (prog2
                   ;; `helm-pattern' have not been modified yet.
                   ;; Set it here to the value of `path' that should be now
                   ;; "Invalid tramp file name" and set the candidates list
                   ;; to ("Invalid tramp file name") to make `helm-pattern'
                   ;; match single candidate "Invalid tramp file name".
                   (setq helm-pattern path)
                   ;; "Invalid tramp file name" is now printed
                   ;; in `helm-buffer'.
                   (list path))))
          ((or (file-regular-p path)
               ;; `ffap-url-regexp' don't match until url is complete.
               (string-match helm-ff-url-regexp path)
               (and (not (file-exists-p path)) (string-match "/$" path))
               (and ffap-url-regexp (string-match ffap-url-regexp path)))
           (list path))
          ((string= path "") (helm-ff-directory-files "/" t))
          ((and (file-directory-p path) (not (file-readable-p path)))
           (list (format "Opening directory: access denied, `%s'" path)))
          ((file-directory-p path) (helm-ff-directory-files path t))
          (t
           (append (list path) ; No need to check for must-match.
                   (helm-ff-directory-files path-name-dir t))))))

(defun helm-ff-directory-files (directory &optional full)
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

(defun helm-ff-transform-fname-for-completion (fname)
  "Return FNAME with it's basename modified as a regexp.
e.g foo => f.*o.*o .
If basename contain one or more space or FNAME is a valid directory name
return FNAME unchanged."
  (let ((bn (helm-c-basename fname)))
    (if (or (not helm-ff-smart-completion)
            (string-match "\\s-" bn)
            (string-match "/$" fname) ; Allow mkdir.
            (file-directory-p fname)
            (string-match helm-ff-url-regexp fname))
        fname ; Fall back to match-plugin.
        (setq bn (if (> (length bn) 2) ; Normal completion on first 2 char.
                     (mapconcat 'identity (split-string bn "" t) ".*") bn))
        (concat (file-name-directory fname) bn))))

(defun helm-ff-save-history ()
  "Store the last value of `helm-ff-default-directory' \
in `helm-ff-history'."
  (when (and helm-ff-default-directory
             (helm-file-completion-source-p))
    (push helm-ff-default-directory helm-ff-history)))
(add-hook 'helm-cleanup-hook 'helm-ff-save-history)

(defun helm-ff-valid-symlink-p (file)
  (file-exists-p (file-truename file)))

(defun helm-ff-properties (candidate)
  "Show file properties of CANDIDATE in a tooltip or message."
  (let ((type       (helm-ff-attributes candidate :type t))
        (dired-line (helm-ff-attributes candidate :dired t :human-size t)))
    (if (window-system)
        (tooltip-show
         (concat
          (helm-c-basename candidate) "\n"
          "Type: " type "\n"
          (when (string= type "symlink")
            (format "True name: '%s'\n"
                    (cond ((string-match "^\.#" (helm-c-basename candidate))
                           "Autosave symlink")
                          ((helm-ff-valid-symlink-p candidate)
                           (file-truename candidate))
                          (t "Invalid Symlink"))))
          dired-line))
        (message dired-line) (sit-for 5))))

;;;###autoload
(defun helm-ff-properties-persistent ()
  "Show properties without quitting helm."
  (interactive)
  (helm-attrset 'properties-action 'helm-ff-properties)
  (helm-execute-persistent-action 'properties-action))

;;;###autoload
(defun helm-ff-persistent-delete ()
  "Delete current candidate without quitting."
  (interactive)
  (helm-attrset 'quick-delete 'helm-ff-quick-delete)
  (helm-execute-persistent-action 'quick-delete))

(defun helm-ff-dot-file-p (file)
  "Check if FILE is `.' or `..'."
  (member (helm-c-basename file) '("." "..")))

(defun helm-ff-quick-delete (candidate)
  "Delete file CANDIDATE without quitting."
  (let ((presel (prog1 (save-excursion
                         (let (sel)
                           (helm-next-line)
                           (setq sel (helm-get-selection))
                           (if (string= sel candidate)
                               (progn (helm-previous-line)
                                      (helm-get-selection))
                               sel)))
                  (helm-mark-current-line))))
    (setq presel (if (and helm-ff-transformer-show-only-basename
                          (not (helm-ff-dot-file-p presel)))
                     (helm-c-basename presel) presel))
    (if helm-ff-quick-delete-dont-prompt-for-deletion
        (helm-c-delete-file candidate
                                helm-ff-signal-error-on-dot-files)
        (save-selected-window
          (when (y-or-n-p (format "Really Delete file `%s'? " candidate))
            (helm-c-delete-file candidate
                                    helm-ff-signal-error-on-dot-files)
            (message nil))))
    (helm-force-update presel)))

(defun helm-ff-kill-buffer-fname (candidate)
  (let* ((buf (get-file-buffer candidate))
         (buf-name (buffer-name buf)))
    (if buf
        (progn
          (kill-buffer buf) (message "Buffer `%s' killed" buf))
        (message "No buffer to kill"))))

(defun helm-ff-kill-or-find-buffer-fname (candidate)
  "Find file CANDIDATE or kill it's buffer if it is visible.
Never kill `helm-current-buffer'.
Never kill buffer modified.
This is called normally on third hit of \
\\<helm-map>\\[helm-execute-persistent-action]
in `helm-find-files-persistent-action'."
  (let* ((buf      (get-file-buffer candidate))
         (buf-name (buffer-name buf)))
    (if (and buf (get-buffer-window buf)
             (not (eq buf (get-buffer helm-current-buffer)))
             (not (buffer-modified-p buf)))
        (progn
          (kill-buffer buf) (message "Buffer `%s' killed" buf-name))
        (find-file candidate))))

;;;###autoload
(defun helm-ff-run-kill-buffer-persistent ()
  "Execute `helm-ff-kill-buffer-fname' whitout quitting."
  (interactive)
  (when (helm-file-completion-source-p)
    (helm-attrset 'kill-buffer-fname 'helm-ff-kill-buffer-fname)  
    (helm-execute-persistent-action 'kill-buffer-fname)))

(defun helm-ff-human-size (size)
  "Return a string showing SIZE of a file in human readable form.
SIZE can be an integer or a float depending it's value.
`file-attributes' will take care of that to avoid overflow error.
KBSIZE if a floating point number, default value is 1024.0."
  (let ((M (cons "M" (/ size (expt helm-ff-default-kbsize 2))))
        (G (cons "G" (/ size (expt helm-ff-default-kbsize 3))))
        (K (cons "K" (/ size helm-ff-default-kbsize)))
        (B (cons "B" size)))
    (loop with result = B
          for (a . b) in
          (loop for (x . y) in (list M G K B)
                unless (< y 1) collect (cons x y))
          when (< b (cdr result)) do (setq result (cons a b))
          finally return (if (string= (car result) "B")
                             (format "%s" size)
                             (format "%.1f%s" (cdr result) (car result))))))

(defun* helm-ff-attributes
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
          (size (if human-size (helm-ff-human-size (getf all :size))
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
            (if human-size (helm-ff-human-size (getf all :size))
                (int-to-string (getf all :size))) " "
                (format-time-string "%Y-%m-%d %R" (getf all :modif-time))))
          (t all))))

(defun helm-ff-prefix-filename (fname &optional file-or-symlinkp new-file)
  "Return filename FNAME maybe prefixed with [?] or [@].
If FILE-OR-SYMLINKP is non--nil this mean we assume FNAME is an
existing filename or valid symlink and there is no need to test it.
NEW-FILE when non--nil mean FNAME is a non existing file and
return FNAME prefixed with [?]."
  (let* ((prefix-new (propertize
                      " " 'display
                      (propertize "[?]" 'face 'helm-ff-prefix)))
         (prefix-url (propertize
                      " " 'display
                      (propertize "[@]" 'face 'helm-ff-prefix))))
    (cond ((or file-or-symlinkp (file-exists-p fname)) fname)
          ((or (string-match helm-ff-url-regexp fname)
               (string-match ffap-url-regexp fname))
           (concat prefix-url " " fname))
          ((or new-file (not (file-exists-p fname)))
           (concat prefix-new " " fname)))))

(defun helm-c-find-files-transformer (files sources)
  "Transformer for `helm-c-source-find-files'.
Tramp files are not highlighted unless `helm-ff-tramp-not-fancy'
is non--nil."
  (if (and (string-match tramp-file-name-regexp helm-pattern)
           helm-ff-tramp-not-fancy)
      (if helm-ff-transformer-show-only-basename
          (loop for i in files collect 
                (if (string-match "[.]\\{1,2\\}$" i)
                    i (cons (helm-c-basename i) i)))
          files)
      (helm-ff-highlight-files files sources)))

(defun helm-ff-highlight-files (files sources)
  "Candidate transformer for `helm-c-source-find-files' without icons."
  (loop for i in files
        for disp = (if (and helm-ff-transformer-show-only-basename
                            (not (string-match "[.]\\{1,2\\}$" i))
                            (not (string-match ffap-url-regexp i))
                            (not (string-match helm-ff-url-regexp i)))
                       (helm-c-basename i) i)
        collect
        (cond ((and (stringp (car (file-attributes i)))
                    (not (helm-ff-valid-symlink-p i))
                    (not (string-match "^\.#" (helm-c-basename i))))
               (cons (helm-ff-prefix-filename
                      (propertize disp 'face 'helm-ff-invalid-symlink) t)
                     i))
              ((stringp (car (file-attributes i)))
               (cons (helm-ff-prefix-filename
                      (propertize disp 'face 'helm-ff-symlink) t)
                     i))
              ((eq t (car (file-attributes i)))
               (cons (helm-ff-prefix-filename
                      (propertize disp 'face 'helm-ff-directory) t)
                     i))
              ((file-executable-p i)
               (cons (helm-ff-prefix-filename
                      (propertize disp 'face 'helm-ff-executable) t)
                     i))
              ((file-exists-p i)
               (cons (helm-ff-prefix-filename
                      (propertize disp 'face 'helm-ff-file) t)
                     i))
              (t
               (cons (helm-ff-prefix-filename
                      (propertize disp 'face 'helm-ff-file) nil 'new-file)
                     i)))))

(defun helm-find-files-action-transformer (actions candidate)
  "Action transformer for `helm-c-source-find-files'."
  (cond ((with-helm-current-buffer
           (eq major-mode 'message-mode))
         (append (subseq actions 0 4)
                 '(("Gnus attach file(s)" . helm-ff-gnus-attach-files))
                 (subseq actions 4)))
        ((string-match (image-file-name-regexp) candidate)
         (append (subseq actions 0 4)
                 '(("Rotate image right `M-r'" . helm-ff-rotate-image-right)
                   ("Rotate image left `M-l'" . helm-ff-rotate-image-left))
                 (subseq actions 4)))
        ((string-match "\.el$" (helm-aif (helm-marked-candidates)
                                   (car it) candidate))
         (append (subseq actions 0 4)
                 '(("Byte compile lisp file(s) `M-B, C-u to load'"
                    . helm-find-files-byte-compile)
                   ("Load File(s) `M-L'" . helm-find-files-load-files))
                 (subseq actions 4)))
        ((and (string-match "\.html?$" candidate)
              (file-exists-p candidate))
         (append (subseq actions 0 4)
                 '(("Browse url file" . browse-url-of-file))
                 (subseq actions 5)))
        ((or (string= (file-name-extension candidate) "pdf")
             (string= (file-name-extension candidate) "PDF"))
         (append (subseq actions 0 4)
                 '(("Pdfgrep File(s)" . helm-ff-pdfgrep))
                 (subseq actions 5)))
        (t actions)))

(defun helm-ff-gnus-attach-files (candidate)
  "Run `gnus-dired-attach' on `helm-marked-candidates' or CANDIDATE."
  (let ((flist (helm-marked-candidates)))
    (gnus-dired-attach flist)))

(defun helm-ff-rotate-current-image-1 (file &optional num-arg)
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

(defun helm-ff-rotate-image-left (candidate)
  "Rotate image file CANDIDATE left.
This affect directly file CANDIDATE."
  (helm-ff-rotate-current-image-1 candidate -90))

(defun helm-ff-rotate-image-right (candidate)
  "Rotate image file CANDIDATE right.
This affect directly file CANDIDATE."
  (helm-ff-rotate-current-image-1 candidate))

(defun helm-ff-rotate-left-persistent ()
  "Rotate image left without quitting helm."
  (interactive)
  (helm-attrset 'image-action1 'helm-ff-rotate-image-left)
  (helm-execute-persistent-action 'image-action1))

(defun helm-ff-rotate-right-persistent ()
  "Rotate image right without quitting helm."
  (interactive)
  (helm-attrset 'image-action2 'helm-ff-rotate-image-right)
  (helm-execute-persistent-action 'image-action2))

(defun helm-ff-exif-data (candidate)
  "Extract exif data from file CANDIDATE using `helm-ff-exif-data-program'."
  (if (and helm-ff-exif-data-program
           (executable-find helm-ff-exif-data-program))
      (shell-command-to-string (format "%s %s %s"
                                       helm-ff-exif-data-program
                                       helm-ff-exif-data-program-args
                                       candidate))
      (format "No program %s found to extract exif"
              helm-ff-exif-data-program)))

(defun helm-find-files-persistent-action (candidate)
  "Open subtree CANDIDATE without quitting helm.
If CANDIDATE is not a directory expand CANDIDATE filename.
If CANDIDATE is alone, open file CANDIDATE filename.
That's mean:
First hit on C-z expand CANDIDATE second hit open file.
If a prefix arg is given or `helm-follow-mode' is on open file."
  (let ((follow        (buffer-local-value
                        'helm-follow-mode
                        (get-buffer-create helm-buffer)))
        (new-pattern   (helm-get-selection))
        (num-lines-buf (with-current-buffer helm-buffer
                         (count-lines (point-min) (point-max)))))
    (flet ((insert-in-minibuffer (fname)
             (with-selected-window (minibuffer-window)
               (unless follow
                 (delete-minibuffer-contents)
                 (set-text-properties 0 (length fname) nil fname)
                 (insert fname)))))
      (cond ((and (string= (helm-ff-set-pattern helm-pattern)
                           "Invalid tramp file name")
                  (string-match tramp-file-name-regexp candidate))
             ;; First hit insert hostname and
             ;; second hit insert ":" and expand.
             (if (string= candidate helm-pattern)
                 (insert-in-minibuffer (concat candidate ":"))
                 (insert-in-minibuffer candidate)))
            (;; A symlink directory, expand it's truename.
             (and (file-directory-p candidate) (file-symlink-p candidate))
             (insert-in-minibuffer (file-name-as-directory
                                    (file-truename
                                     (expand-file-name candidate)))))
            ;; A directory, open it.
            ((file-directory-p candidate)
             (when (string= (helm-c-basename candidate) "..")
               (setq helm-ff-last-expanded helm-ff-default-directory))
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
             (helm-c-switch-to-buffer image-dired-display-image-buffer)
             (with-current-buffer image-dired-display-image-buffer
               (let ((exif-data (helm-ff-exif-data candidate)))
                 (image-dired-update-property 'help-echo exif-data))))
            ;; Allow browsing archive on avfs fs.
            ;; Assume volume is already mounted with mountavfs.
            ((and helm-ff-avfs-directory
                  (string-match
                   (regexp-quote (expand-file-name helm-ff-avfs-directory))
                   (file-name-directory candidate))
                  (helm-ff-file-compressed-p candidate))
             (insert-in-minibuffer (concat candidate "#")))
            ;; On second hit we open file.
            ;; On Third hit we kill it's buffer maybe.
            (t
             (helm-ff-kill-or-find-buffer-fname candidate))))))

(defun helm-ff-file-compressed-p (candidate)
  "Whether CANDIDATE is a compressed file or not."
  (member (file-name-extension candidate)
          helm-ff-file-compressed-list))

(defun helm-c-insert-file-name-completion-at-point (candidate)
  "Insert file name completion at point."
  (with-helm-current-buffer
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

(defun* helm-find-files-history (&key (comp-read t))
  "The `helm-find-files' history.
Show the first `helm-ff-history-max-length' elements of
`helm-ff-history' in an `helm-comp-read'."
  (let ((history (when helm-ff-history
                   (helm-fast-remove-dups helm-ff-history
                                              :test 'equal))))
    (when history
      (setq helm-ff-history
            (if (>= (length history) helm-ff-history-max-length)
                (subseq history 0 helm-ff-history-max-length)
                history))
      (if comp-read
          (helm-comp-read
           "Switch to Directory: "
           helm-ff-history
           :name "Helm Find Files History"
           :must-match t)
          helm-ff-history))))

(defun helm-find-files-1 (fname &optional preselect)
  "Find FNAME with `helm' completion.
Like `find-file' but with `helm' support.
Use it for non--interactive calls of `helm-find-files'."
  (when (get-buffer helm-action-buffer)
    (kill-buffer helm-action-buffer))
  (let ((helm-mp-highlight-delay nil)
        ;; Be sure we don't erase the precedent minibuffer if some.
        (helm-ff-auto-update-initial-value
         (and helm-ff-auto-update-initial-value
              (not (minibuffer-window-active-p (minibuffer-window)))))
        helm-samewindow)
    (helm :sources 'helm-c-source-find-files
              :input fname
              :preselect preselect
              :keymap helm-find-files-map
              :prompt "Find Files or Url: "
              :buffer "*Helm Find Files*")))


(defun helm-find-files-initial-input (&optional input)
  "Return INPUT if present, otherwise try to guess it."
  (or (and input (or (and (file-remote-p input) input)
                     (expand-file-name input)))
      (helm-find-files-input
       (ffap-guesser)
       (thing-at-point 'filename))))

(defun helm-find-files-input (fap tap)
  "Default input of `helm-find-files'."
  (let* ((def-dir (helm-c-current-directory))
         (lib     (helm-find-library-at-point))
         (url     (helm-ff-find-url-at-point))
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

(defun helm-c-current-directory ()
  "Return current-directory name at point.
Useful in dired buffers when there is inserted subdirs."
  (if (eq major-mode 'dired-mode)
      (dired-current-directory)
      default-directory))

(defun helm-ff-find-url-at-point ()
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

(defun helm-find-library-at-point ()
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

;;; Helm completion for `write-file'.==> C-x C-w
(defvar helm-c-source-write-file
  `((name . "Write File")
    (header-name . (lambda (name)
                     (concat name helm-c-find-files-doc-header)))
    ;; It is needed for filenames with capital letters
    (disable-shortcuts)
    (candidates . helm-find-files-get-candidates)
    (filtered-candidate-transformer helm-c-find-files-transformer)
    (persistent-action . helm-find-files-persistent-action)
    (persistent-help . "Expand Candidate")
    (volatile)
    (action .
            (("Write File" . (lambda (candidate)
                               (write-file candidate 'confirm)))))))

;;; Helm completion for `insert-file'.==> C-x i
(defvar helm-c-source-insert-file
  `((name . "Insert File")
    (header-name . (lambda (name)
                     (concat name helm-c-find-files-doc-header)))
    ;; It is needed for filenames with capital letters
    (disable-shortcuts)
    (candidates . helm-find-files-get-candidates)
    (filtered-candidate-transformer helm-c-find-files-transformer)
    (persistent-action . helm-find-files-persistent-action)
    (persistent-help . "Expand Candidate")
    (volatile)
    (action .
            (("Insert File" . (lambda (candidate)
                                (when (y-or-n-p (format "Really insert %s in %s "
                                                        candidate helm-current-buffer))
                                  (insert-file-contents candidate))))))))

;;; Helm completion for copy, rename and (rel)sym/hard/link files from dired.
(defvar helm-c-source-copy-files
  `((name . "Copy Files")
    (header-name . (lambda (name)
                     (concat name helm-c-find-files-doc-header)))
    ;; It is needed for filenames with capital letters
    (disable-shortcuts)
    (candidates . helm-find-files-get-candidates)
    (filtered-candidate-transformer helm-c-find-files-transformer)
    (persistent-action . helm-find-files-persistent-action)
    (persistent-help . "Expand Candidate")
    (volatile)
    (action .
            (("Copy File"
              . (lambda (candidate)
                  (helm-dired-action candidate :action 'copy)))
             ("Copy and Follow"
              . (lambda (candidate)
                  (helm-dired-action candidate :action 'copy :follow t)))))))


(defvar  helm-c-source-rename-files
  `((name . "Rename Files")
    (header-name . (lambda (name)
                     (concat name helm-c-find-files-doc-header)))
    ;; It is needed for filenames with capital letters
    (disable-shortcuts)
    (candidates . helm-find-files-get-candidates)
    (filtered-candidate-transformer helm-c-find-files-transformer)
    (persistent-action . helm-find-files-persistent-action)
    (persistent-help . "Expand Candidate")
    (volatile)
    (action .
            (("Rename File"
              . (lambda (candidate)
                  (helm-dired-action candidate :action 'rename)))
             ("Rename and Follow"
              . (lambda (candidate)
                  (helm-dired-action candidate :action 'rename :follow t)))))))

(defvar helm-c-source-symlink-files
  `((name . "Symlink Files")
    (header-name . (lambda (name)
                     (concat name helm-c-find-files-doc-header)))
    ;; It is needed for filenames with capital letters
    (disable-shortcuts)
    (candidates . helm-find-files-get-candidates)
    (filtered-candidate-transformer helm-c-find-files-transformer)
    (persistent-action . helm-find-files-persistent-action)
    (persistent-help . "Expand Candidate")
    (volatile)
    (action
     . (("Symlink File"
         . (lambda (candidate)
             (helm-dired-action candidate :action 'symlink)))
        ("RelSymlink File"
         . (lambda (candidate)
             (helm-dired-action candidate :action 'relsymlink)))))))


(defvar helm-c-source-hardlink-files
  `((name . "Hardlink Files")
    (header-name . (lambda (name)
                     (concat name helm-c-find-files-doc-header)))
    ;; It is needed for filenames with capital letters
    (disable-shortcuts)
    (candidates . helm-find-files-get-candidates)
    (filtered-candidate-transformer helm-c-find-files-transformer)
    (persistent-action . helm-find-files-persistent-action)
    (persistent-help . "Expand Candidate")
    (volatile)
    (action
     . (("Hardlink File"
         . (lambda (candidate)
             (helm-dired-action candidate :action 'hardlink)))))))

(defun* helm-dired-action (candidate &key action follow (files (dired-get-marked-files)))
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
           helm-ff-history)
    (when (and follow (not (get-buffer dired-log-buffer)))
      (let ((target (directory-file-name candidate)))
        (unwind-protect
             (progn
               (setq helm-ff-cand-to-mark
                     (helm-get-dest-fnames-from-list files candidate dirflag))
               (if (and dirflag (eq action 'rename))
                   (helm-find-files-1 (file-name-directory target)
                                          (if helm-ff-transformer-show-only-basename
                                              (helm-c-basename target) target))
                   (helm-find-files-1 (expand-file-name candidate))))
          (setq helm-ff-cand-to-mark nil))))))

(defun helm-c-basename (fname &optional ext)
  "Print FNAME  with any  leading directory  components removed.
If specified, also remove filename extension EXT."
  (if (and ext (or (string= (file-name-extension fname) ext)
                   (string= (file-name-extension fname t) ext))
           (not (file-directory-p fname)))
      (file-name-sans-extension (file-name-nondirectory fname))
      (file-name-nondirectory (directory-file-name fname))))

(defun helm-get-dest-fnames-from-list (flist dest-cand rename-dir-flag)
  "Transform filenames of FLIST to abs of DEST-CAND.
If RENAME-DIR-FLAG is non--nil collect the `directory-file-name' of transformed
members of FLIST."
  ;; At this point files have been renamed/copied at destination.
  ;; That's mean DEST-CAND exists.
  (loop
        with dest = (expand-file-name dest-cand)
        for src in flist
        for basename-src = (helm-c-basename src)
        for fname = (cond (rename-dir-flag (directory-file-name dest))
                          ((file-directory-p dest)
                           (concat (file-name-as-directory dest) basename-src))
                          (t dest))
        when (file-exists-p fname)
        collect fname into tmp-list
        finally return (sort tmp-list 'string<)))

(defun helm-ff-maybe-mark-candidates ()
  "Mark all candidates of list `helm-ff-cand-to-mark'."
  (when (and (string= (assoc-default 'name (helm-get-current-source))
                      (assoc-default 'name helm-c-source-find-files))
             helm-ff-cand-to-mark)
    (with-helm-window
      (while helm-ff-cand-to-mark
        (if (string= (car helm-ff-cand-to-mark) (helm-get-selection))
            (progn
              (helm-make-visible-mark)
              (helm-next-line)
              (setq helm-ff-cand-to-mark (cdr helm-ff-cand-to-mark)))
            (helm-next-line)))
      (unless (helm-this-visible-mark)
        (helm-prev-visible-mark)))))

(add-hook 'helm-after-update-hook #'helm-ff-maybe-mark-candidates)

(defun* helm-dired-do-action-on-file (&key action)
  (let* ((files     (dired-get-marked-files))
         (len       (length files))
         (fname     (if (> len 1)
                        (format "* %d Files" len)
                        (car files)))
         (source    (case action
                      ('copy     'helm-c-source-copy-files)
                      ('rename   'helm-c-source-rename-files)
                      ('symlink  'helm-c-source-symlink-files)
                      ('hardlink 'helm-c-source-hardlink-files)))
         (prompt-fm (case action
                      ('copy     "Copy %s to: ")
                      ('rename   "Rename %s to: ")
                      ('symlink  "Symlink %s to: ")
                      ('hardlink "Hardlink %s to: ")))
         (buffer    (case action
                      ('copy     "*Helm Copy Files*")
                      ('rename   "*Helm Rename Files*")
                      ('symlink  "*Helm Symlink Files*")
                      ('hardlink "*Helm Hardlink Files*")))
         (helm-mp-highlight-delay     nil))
    (helm :sources source
              :input (or (dired-dwim-target-directory)
                         (expand-file-name (helm-c-current-directory)))
              :preselect (dired-get-filename)
              :prompt (format prompt-fm fname)
              :keymap helm-c-read-file-map
              :buffer buffer)))

;;;###autoload
(define-minor-mode helm-dired-mode ()
  "Enable helm completion in Dired functions.
Bindings affected are C, R, S, H.
This is deprecated for Emacs24+ users, use `helm-mode' instead."
  :group 'helm-config
  :global t
  (if helm-dired-mode
      (progn
        (substitute-key-definition
         'dired-do-copy 'helm-dired-copy-file dired-mode-map)
        (substitute-key-definition
         'dired-do-rename 'helm-dired-rename-file dired-mode-map)
        (substitute-key-definition
         'dired-do-symlink 'helm-dired-symlink-file dired-mode-map)
        (substitute-key-definition
         'dired-do-hardlink 'helm-dired-hardlink-file dired-mode-map))
      (substitute-key-definition
       'helm-dired-copy-file 'dired-do-copy dired-mode-map)
      (substitute-key-definition
       'helm-dired-rename-file 'dired-do-rename dired-mode-map)
      (substitute-key-definition
       'helm-dired-symlink-file 'dired-do-symlink dired-mode-map)
      (substitute-key-definition
       'helm-dired-hardlink-file 'dired-do-hardlink dired-mode-map)))

(defalias 'helm-dired-bindings 'helm-dired-mode)

(defun* helm-c-read-file-name
    (prompt
     &key
     (name "Read File Name")
     (initial-input (expand-file-name default-directory))
     (buffer "*Helm Completions*")
     test
     (preselect nil)
     (history nil)
     must-match
     (marked-candidates nil)
     (alistp t)
     (persistent-action 'helm-find-files-persistent-action)
     (persistent-help "Hit1 Expand Candidate, Hit2 or (C-u) Find file"))
  "Read a file name with helm completion.
It is helm `read-file-name' emulation.

Argument PROMPT is the default prompt to use.

Keys description:

- NAME: Source name, default to \"Read File Name\".

- INITIAL-INPUT: Where to start read file name, default to `default-directory'.

- BUFFER: `helm-buffer' name default to \"*Helm Completions*\".

- TEST: A predicate called with one arg 'candidate'.

- PRESELECT: helm preselection.

- HISTORY: Display HISTORY in a special source.

- MUST-MATCH: Can be 'confirm, nil, or t.

- MARKED-CANDIDATES: When non--nil return a list of marked candidates.

- ALISTP: Don't use `all-completions' in history (take effect only on history).

- PERSISTENT-ACTION: a persistent action function.

- PERSISTENT-HELP: persistent help message."
  (when (get-buffer helm-action-buffer)
    (kill-buffer helm-action-buffer))
  
  ;; Assume completion have been already required,
  ;; so always use 'confirm.
  (when (eq must-match 'confirm-after-completion)
    (setq must-match 'confirm))

  (flet ((action-fn (candidate)
           (if marked-candidates
               (helm-marked-candidates)
               (identity candidate))))
  
    (let* ((helm-mp-highlight-delay nil)
           ;; Be sure we don't erase the underlying minibuffer if some.
           (helm-ff-auto-update-initial-value
            (and helm-ff-auto-update-initial-value
                 (not (minibuffer-window-active-p (minibuffer-window)))))
           helm-same-window
           (hist (and history (helm-comp-read-get-candidates
                               history nil nil alistp)))
           (minibuffer-completion-confirm must-match)
           (must-match-map (when must-match
                             (let ((map (make-sparse-keymap)))
                               (define-key map (kbd "RET")
                                 'helm-confirm-and-exit-minibuffer)
                               map)))
           (helm-map (if must-match-map
                             (make-composed-keymap
                              must-match-map helm-c-read-file-map)
                             helm-c-read-file-map)))
      
      (or (helm
           :sources
           `(((name . ,(format "%s History" name))
              (header-name . (lambda (name)
                               (concat name helm-c-find-files-doc-header)))
              (disable-shortcuts)
              (mode-line . helm-read-file-name-mode-line-string)
              (candidates . hist)
              (persistent-action . ,persistent-action)
              (persistent-help . ,persistent-help)
              (action . ,'action-fn))
             ((name . ,name)
              (header-name . (lambda (name)
                               (concat name helm-c-find-files-doc-header)))
              (init . (lambda ()
                        (setq helm-ff-auto-update-flag
                              helm-ff-auto-update-initial-value)))
              ;; It is needed for filenames with capital letters
              (disable-shortcuts)
              (mode-line . helm-read-file-name-mode-line-string)
              (candidates
               . (lambda ()
                   (if test
                       (loop with hn = (helm-ff-tramp-hostnames)
                             for i in (helm-find-files-get-candidates
                                       must-match)
                             when (or (member i hn)            ; A tramp host
                                      (funcall test i)         ; Test ok
                                      (not (file-exists-p i))) ; A new file.
                             collect i)
                       (helm-find-files-get-candidates must-match))))
              (filtered-candidate-transformer helm-c-find-files-transformer)
              (persistent-action . ,persistent-action)
              (candidate-number-limit . 9999)
              (toggle-auto-update . helm-ff-toggle-auto-update)
              (persistent-help . ,persistent-help)
              (volatile)
              (action . ,'action-fn)))
           :input initial-input
           :prompt prompt
           :resume 'noresume
           :buffer buffer
           :preselect preselect)
          (when (and (not (string= helm-pattern ""))
                     (eq helm-exit-status 0)
                     (eq must-match 'confirm))
            (identity helm-pattern))
          (keyboard-quit)))))


;;; File Cache
(defvar helm-c-file-cache-initialized-p nil)

(defvar helm-c-file-cache-files nil)

(defvar helm-c-source-file-cache
  `((name . "File Cache")
    (init
     . (lambda ()
         (require 'filecache nil t)
         (unless helm-c-file-cache-initialized-p
           (setq helm-c-file-cache-files
                 (loop for item in file-cache-alist append
                       (destructuring-bind (base &rest dirs) item
                         (loop for dir in dirs collect
                               (concat dir base)))))
           (defadvice file-cache-add-file (after file-cache-list activate)
             (add-to-list 'helm-c-file-cache-files (expand-file-name file)))
           (setq helm-c-file-cache-initialized-p t))))
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (mode-line . helm-generic-file-mode-line-string)
    (candidates . helm-c-file-cache-files)
    (match helm-c-match-on-basename)
    (type . file)))


;;; Locate
;;
;;
;; NOTE for WINDOZE users:
;; You have to install Everything with his command line interface here:
;; http://www.voidtools.com/download.php

(defun helm-ff-find-locatedb (&optional from-ff)
  "Try to find if a local locatedb file is available.
The search is done in `helm-ff-default-directory' or
fall back to `default-directory' if FROM-FF is nil."
  (when helm-ff-locate-db-filename
    (cond ((and helm-ff-default-directory
                from-ff
                (file-exists-p (expand-file-name
                                helm-ff-locate-db-filename
                                helm-ff-default-directory))
                (expand-file-name
                 helm-ff-locate-db-filename
                 helm-ff-default-directory)))
          ((and (not from-ff)
                (file-exists-p (expand-file-name
                                helm-ff-locate-db-filename
                                default-directory))
                (expand-file-name
                 helm-ff-locate-db-filename
                 default-directory))))))

(defun helm-locate-1 (&optional localdb init from-ff)
  "Generic function to run Locate.
if LOCALDB is non--nil search and use a local locate db file.
INIT is a string to use as initial input in prompt.
See `helm-locate-with-db' and `helm-locate'."
  (helm-locate-with-db
   (and localdb
        (or (helm-ff-find-locatedb from-ff)
            (helm-c-read-file-name
             "LocateDBFiles: "
             :initial-input (or helm-ff-default-directory
                                default-directory)
             :marked-candidates t
             :preselect helm-locate-db-file-regexp
             :test #'(lambda (x)
                       (if helm-locate-db-file-regexp
                           ;; Select only locate db files and directories
                           ;; to allow navigation.
                           (or (string-match
                                helm-locate-db-file-regexp x)
                               (file-directory-p x))
                           x)))))
   init))
;; (helm-locate-1 t)

(defun helm-locate-with-db (&optional db initial-input)
  "Run locate -d DB.
If DB is not given or nil use locate without -d option.
Argument DB can be given as a string or list of db files.
Argument INITIAL-INPUT is a string to use as initial-input.
See also `helm-locate'."
  (when (and db (stringp db)) (setq db (list db)))
  (unless helm-c-locate-command
    (setq helm-c-locate-command
          (case system-type
            ('gnu/linux "locate -i -r %s")
            ('berkeley-unix "locate -i %s")
            ('windows-nt "es -i -r %s")
            (t "locate %s"))))  
  (let ((helm-c-locate-command
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
              helm-c-locate-command)
             helm-c-locate-command)))
    (helm :sources 'helm-c-source-locate
              :buffer "*helm locate*"
              :input initial-input
              :keymap helm-generic-files-map)))
;; (helm-locate-with-db "~/locate.db")

(defun helm-c-locate-init ()
  "Initialize async locate process for `helm-c-source-locate'."
  (setq mode-line-format
        '(" " mode-line-buffer-identification " "
          (line-number-mode "%l") " "
          (:eval (propertize "(Locate Process Running) "
                  'face '((:foreground "red"))))))
  (prog1
      (start-process-shell-command "locate-process" nil
                                   (format helm-c-locate-command
                                           helm-pattern))
    (set-process-sentinel (get-process "locate-process")
                          #'(lambda (process event)
                              (when (string= event "finished\n")
                                (with-helm-window
                                  (force-mode-line-update nil)
                                  (helm-update-move-first-line)))))))

(defvar helm-c-source-locate
  `((name . "Locate")
    (candidates . helm-c-locate-init)
    (type . file)
    (requires-pattern . 3)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (candidate-number-limit . 9999)
    (mode-line . helm-generic-file-mode-line-string)
    (delayed))
  "Find files matching the current input pattern with locate.")

(defun helm-c-locate-read-file-name (prompt &optional init)
  "Search a file with locate and return it's filename.
Use argument PROMPT and INIT for `helm' arguments
prompt and input."
  (helm :sources
            '((name . "Locate")
              (candidates . helm-c-locate-init)
              (action . identity)
              (requires-pattern . 3)
              (candidate-number-limit . 9999)
              (mode-line . helm-generic-file-mode-line-string)
              (delayed))
            :prompt prompt
            :input init
            :buffer "*helm locate rfn*"))



;;; Helm Incremental Grep.
;;
;;
;; Allow to grep incrementally with helm interface.
;; It allow also to Grep files recursively without using 'find' shell command.
;; On Windows you will need at least Grep version 2.5.4 of Gnuwin32.
(defvar helm-c-grep-default-command
  "grep -d skip %e -niH -e %p %f"
  "Default grep format command for `helm-do-grep-1'.
Where:
'%e' format spec is for --exclude or --include grep options.
'%p' format spec is for pattern.
'%f' format spec is for filenames.")

(defvar helm-c-grep-default-recurse-command
  "grep -d recurse %e -niH -e %p %f"
  "Default recursive grep format command for `helm-do-grep-1'.
See `helm-c-grep-default-command' for format specs.")

(defvar helm-c-default-zgrep-command "zgrep -niH -e %p %f")

(defvar helm-c-rzgrep-cache (make-hash-table :test 'equal))

(defvar helm-c-grep-default-function 'helm-c-grep-init)

(defvar helm-c-grep-debug-command-line nil
  "Turn on helm grep command-line debugging when non--nil.")

(defvar helm-c-zgrep-recurse-flag nil)

(defvar helm-c-grep-history nil)

(defvar helm-c-grep-max-length-history 100
  "*Max number of elements to save in `helm-c-grep-history'.")

(defun helm-c-grep-prepare-candidates (candidates)
  "Prepare filenames and directories CANDIDATES for grep command line."
  ;; If one or more candidate is a directory, search in all files
  ;; of this candidate (e.g /home/user/directory/*).
  ;; If r option is enabled search also in subdidrectories.
  ;; We need here to expand wildcards to support crap windows filenames
  ;; as grep doesn't accept quoted wildcards (e.g "dir/*.el").
  (if helm-c-zgrep-recurse-flag
      (mapconcat 'shell-quote-argument candidates " ")
      (loop for i in candidates append
            (cond ( ;; Candidate is a directory and we use recursion.
                   (and (file-directory-p i)
                        (helm-c-grep-recurse-p))
                   (list (expand-file-name i)))
                  ;; Candidate is a directory, search in all files.
                  ((file-directory-p i)
                   (file-expand-wildcards
                    (concat (file-name-as-directory (expand-file-name i)) "*") t))
                  ;; Candidate is a file or wildcard and we use recursion, use the
                  ;; current directory instead of candidate.
                  ((and (or (file-exists-p i) (string-match "\*" i))
                        (helm-c-grep-recurse-p))
                   (list (expand-file-name
                          (directory-file-name ; Needed for windoze.
                           (file-name-directory (directory-file-name i))))))
                  ;; Candidate use wildcard.
                  ((string-match "^\*" (helm-c-basename i))
                   (file-expand-wildcards i t))
                  ;; Else should be one or more file.
                  (t (list i))) into all-files
            finally return
            (mapconcat 'shell-quote-argument all-files " "))))

(defun helm-c-grep-recurse-p ()
  "Check if `helm-do-grep-1' have switched to recursive."
  (let ((args (replace-regexp-in-string
               "grep" "" helm-c-grep-default-command)))
    (string-match-p "r\\|recurse" args)))

(defun helm-c-grep-init (only-files &optional include zgrep)
  "Start an asynchronous grep process in ONLY-FILES list."
  (let* ((fnargs        (helm-c-grep-prepare-candidates
                         (if (file-remote-p helm-ff-default-directory)
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
         (exclude       (if (helm-c-grep-recurse-p)
                            (concat (or include ignored-files) " " ignored-dirs)
                            ignored-files))
         (cmd-line      (format-spec
                         helm-c-grep-default-command
                         (delq nil
                               (list (unless zgrep (cons ?e exclude))
                                     (cons ?p (shell-quote-argument helm-pattern))
                                     (cons ?f fnargs))))))
    (when helm-c-grep-debug-command-line
      (with-current-buffer (get-buffer-create "*any grep debug*")
        (goto-char (point-max))
        (insert (concat ">>> " cmd-line "\n\n"))))
    (setq mode-line-format
          '(" " mode-line-buffer-identification " "
            (line-number-mode "%l") " "
            (:eval (when (get-process "grep-process")
                     (propertize "[Grep Process Running] "
                                 'face 'helm-grep-running)))))
    (force-mode-line-update nil)
    (prog1
        (let ((default-directory helm-ff-default-directory))
          (start-file-process-shell-command "grep-process" nil cmd-line))
      (message nil)
      (set-process-sentinel
       (get-process "grep-process")
       #'(lambda (process event)
           (when (string= event "finished\n")
             (with-helm-window
               (helm-update-move-first-line)
               (setq mode-line-format
                     '(" " mode-line-buffer-identification " "
                       (line-number-mode "%l") " "
                       (:eval (propertize
                               (format "[Grep Process Finished - (%s results)] "
                                       (let ((nlines (1- (count-lines
                                                          (point-min)
                                                          (point-max)))))
                                         (if (> nlines 0) nlines 0)))
                               'face 'helm-grep-finish))))
               (force-mode-line-update nil))))))))

(defun helm-c-grep-action (candidate &optional where mark)
  "Define a default action for `helm-do-grep' on CANDIDATE.
WHERE can be one of other-window, elscreen, other-frame."
  (let* ((split        (helm-c-grep-split-line candidate))
         (lineno       (string-to-number (nth 1 split)))
         (loc-fname    (car split))
         (tramp-method (file-remote-p helm-ff-default-directory 'method))
         (tramp-host   (file-remote-p helm-ff-default-directory 'host))
         (tramp-prefix (concat "/" tramp-method ":" tramp-host ":"))
         (fname        (if tramp-host
                           (concat tramp-prefix loc-fname) loc-fname)))
    (case where
      (other-window (find-file-other-window fname))
      (elscreen     (helm-elscreen-find-file fname))
      (other-frame  (find-file-other-frame fname))
      (grep         (helm-c-grep-save-results-1))
      (t (find-file fname)))
    (unless (eq where 'grep)
      (helm-goto-line lineno))
    (when mark
      (set-marker (mark-marker) (point))
      (push-mark (point) 'nomsg))
    ;; Save history
    (unless (or helm-in-persistent-action
                (string= helm-pattern ""))
      (setq helm-c-grep-history
            (cons helm-pattern
                  (delete helm-pattern helm-c-grep-history)))
      (when (> (length helm-c-grep-history)
               helm-c-grep-max-length-history)
        (setq helm-c-grep-history
              (delete (car (last helm-c-grep-history))
                      helm-c-grep-history))))))

(defun helm-c-grep-other-window (candidate)
  "Jump to result in other window from helm grep."
  (helm-c-grep-action candidate 'other-window))

(defun helm-c-grep-other-frame (candidate)
  "Jump to result in other frame from helm grep."
  (helm-c-grep-action candidate 'other-frame))

(defun helm-c-grep-jump-elscreen (candidate)
  "Jump to result in elscreen from helm grep."
  (helm-c-grep-action candidate 'elscreen))

(defun helm-c-grep-save-results (_candidate)
  (helm-c-grep-action _candidate 'grep))

(defun helm-c-grep-save-results-1 ()
  "Save helm grep result in a `grep-mode' buffer."
  (let ((buf "*grep*")
        new-buf)
    (when (get-buffer buf)
      (setq new-buf (read-string "GrepBufferName: " buf))
      (loop for b in (helm-c-buffer-list)
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
                (format "Grep Results for `%s':\n\n" helm-pattern))
        (save-excursion
          (insert (with-current-buffer helm-buffer
                    (goto-char (point-min)) (forward-line 1)
                    (buffer-substring (point) (point-max))))
          (grep-mode))))
    (message "Helm Grep Results saved in `%s' buffer" buf)))

(defun helm-c-grep-persistent-action (candidate)
  "Persistent action for `helm-do-grep'.
With a prefix arg record CANDIDATE in `mark-ring'."
  (if current-prefix-arg
      (helm-c-grep-action candidate nil 'mark)
      (helm-c-grep-action candidate))
  (helm-match-line-color-current-line))

(defun helm-c-grep-guess-extensions (files)
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

(defun helm-do-grep-1 (only &optional recurse zgrep)
  "Launch grep with a list of ONLY files.
When RECURSE is given use -r option of grep and prompt user
to set the --include args of grep.
You can give more than one arg separated by space.
e.g *.el *.py *.tex.
If it's empty --exclude `grep-find-ignored-files' is used instead."
  (let* ((helm-compile-source-functions
          ;; rule out helm-match-plugin because the input is one regexp.
          (delq 'helm-compile-source--match-plugin
                (copy-sequence helm-compile-source-functions)))
         (exts (helm-c-grep-guess-extensions only))
         (globs (and (not zgrep) (mapconcat 'identity exts " ")))
         (include-files (and recurse (not zgrep)
                             (read-string "OnlyExt(*.[ext]): "
                                          globs)))
         ;; Set `minibuffer-history' AFTER includes-files
         ;; to avoid storing wild-cards here.
         (minibuffer-history helm-c-grep-history)
         (helm-c-grep-default-command (cond ((and recurse zgrep) helm-c-default-zgrep-command)
                                                (recurse helm-c-grep-default-recurse-command)
                                                (zgrep helm-c-default-zgrep-command)
                                                (t helm-c-grep-default-command)))
         ;; Disable match-plugin and use here own highlighting.
         (helm-mp-highlight-delay     nil))
    (when include-files
      (setq include-files
            (and (not (string= include-files ""))
                 (mapconcat #'(lambda (x)
                                (concat "--include=" (shell-quote-argument x)))
                            (split-string include-files) " "))))
    ;; When called as action from an other source e.g *-find-files
    ;; we have to kill action buffer.
    (when (get-buffer helm-action-buffer)
      (kill-buffer helm-action-buffer))
    ;; `helm-find-files' haven't already started,
    ;; give a default value to `helm-ff-default-directory'.
    (setq helm-ff-default-directory (or helm-ff-default-directory
                                            default-directory))
    (helm
     :sources
     `(((name . "Grep")
        (header-name . (lambda (name)
                         (concat name "(C-c ? Help)")))
        (candidates
         . (lambda ()
             (funcall helm-c-grep-default-function only include-files zgrep)))
        (filtered-candidate-transformer helm-c-grep-cand-transformer)
        (candidate-number-limit . 9999)
        (mode-line . helm-grep-mode-line-string)
        (keymap . ,helm-c-grep-map)
        (action . ,(delq
                    nil
                    `(("Find File" . helm-c-grep-action)
                      ("Find file other frame" . helm-c-grep-other-frame)
                      ,(and (locate-library "elscreen")
                            '("Find file in Elscreen"
                              . helm-c-grep-jump-elscreen))
                      ("Save results in grep buffer" . helm-c-grep-save-results)
                      ("Find file other window" . helm-c-grep-other-window))))
        (persistent-action . helm-c-grep-persistent-action)
        (persistent-help . "Jump to line (`C-u' Record in mark ring)")
        (requires-pattern . 3)
        (delayed)))
     :buffer "*helm grep*")))

(defun helm-ff-zgrep-1 (flist recursive)
  (unwind-protect
       (let* ((def-dir (or helm-ff-default-directory
                           default-directory))
              (only    (if recursive
                           (or (gethash def-dir helm-c-rzgrep-cache)
                               (puthash
                                def-dir
                                (helm-c-walk-directory
                                 def-dir
                                 :directories nil
                                 :path 'full
                                 :match ".*\\(\.gz\\|\.bz\\|\.xz\\|\.lzma\\)$")
                                helm-c-rzgrep-cache))
                           flist)))
         (when recursive (setq helm-c-zgrep-recurse-flag t))
         (helm-do-grep-1 only recursive 'zgrep))
    (setq helm-c-zgrep-recurse-flag nil)))

(defun helm-c-grep-split-line (line)
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

(defun helm-c-grep-cand-transformer (candidates sources)
  "Filtered candidate transformer function for `helm-do-grep'."
  (loop for i in candidates
        for split  = (and i (helm-c-grep-split-line i))
        for fname  = (car split)
        for lineno = (nth 1 split)
        for str    = (nth 2 split)
        when (and fname lineno str)
        collect
        (cons (concat (propertize (file-name-nondirectory fname)
                                  'face 'helm-grep-file
                                  'help-echo fname) ":"
                                  (propertize lineno 'face 'helm-grep-lineno) ":"
                                  (helm-c-grep-highlight-match str))
              i)))

(defun helm-c-grep-highlight-match (str)
  "Highlight in string STR all occurences matching `helm-pattern'."
  (condition-case nil
      (with-temp-buffer
        (insert str)
        (goto-char (point-min))
        (while (and (re-search-forward helm-pattern nil t)
                    (> (- (match-end 0) (match-beginning 0)) 0))
          (add-text-properties
           (match-beginning 0) (match-end 0)
           '(face helm-grep-match)))
        (buffer-string))
    (error nil)))

;; Go to next or precedent file (common to etags and grep).
(defun helm-c-goto-next-or-prec-file (n)
  "Go to next or precedent candidate file in helm grep/etags buffers.
If N is positive go forward otherwise go backward."
  (with-helm-window
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
            (helm-mark-current-line)
            (throw 'break nil))))
      (cond ((and (eq n 1) (eobp))
             (re-search-backward ".")
             (forward-line 0)
             (helm-mark-current-line))
            ((and (< n 1) (bobp))
             (forward-line 1)
             (helm-mark-current-line))))))

;;;###autoload
(defun helm-c-goto-precedent-file ()
  "Go to precedent file in helm grep/etags buffers."
  (interactive)
  (helm-c-goto-next-or-prec-file -1))

;;;###autoload
(defun helm-c-goto-next-file ()
  "Go to precedent file in helm grep/etags buffers."
  (interactive)
  (helm-c-goto-next-or-prec-file 1))

;;;###autoload
(defun helm-c-grep-run-persistent-action ()
  "Run grep persistent action from `helm-do-grep-1'."
  (interactive)
  (helm-attrset 'jump-persistent 'helm-c-grep-persistent-action)
  (helm-execute-persistent-action 'jump-persistent))

;;;###autoload
(defun helm-c-grep-run-default-action ()
  "Run grep default action from `helm-do-grep-1'."
  (interactive)
  (helm-c-quit-and-execute-action 'helm-c-grep-action))

;;;###autoload
(defun helm-c-grep-run-other-window-action ()
  "Run grep goto other window action from `helm-do-grep-1'."
  (interactive)
  (helm-c-quit-and-execute-action 'helm-c-grep-other-window))

;;;###autoload
(defun helm-c-grep-run-save-buffer ()
  "Run grep save results action from `helm-do-grep-1'."
  (interactive)
  (helm-c-quit-and-execute-action 'helm-c-grep-save-results))

;; Grep buffers
(defun helm-c-grep-buffers-1 (candidate &optional zgrep)
  "Run grep on all file--buffers or CANDIDATE if it is a file--buffer.
If one of selected buffers is not a file--buffer,
it is ignored and grep will run on all others file--buffers.
If only one candidate is selected and it is not a file--buffer,
switch to this buffer and run `helm-occur'.
If a prefix arg is given run grep on all buffers ignoring non--file-buffers."
  (let* ((prefarg (or current-prefix-arg helm-current-prefix-arg))
         (cands (if prefarg
                    (buffer-list)
                    (helm-marked-candidates)))
         (win-conf (current-window-configuration))
         ;; Non--fname buffers are ignored.
         (bufs (loop for buf in cands
                     for fname = (buffer-file-name (get-buffer buf))
                     when fname
                     collect (expand-file-name fname))))
    (if bufs
        (if zgrep
            (helm-do-grep-1 bufs nil 'zgrep)
            (helm-do-grep-1 bufs))
        ;; bufs is empty, thats mean we have only CANDIDATE
        ;; and it is not a buffer-filename, fallback to occur.
        (helm-c-switch-to-buffer candidate)
        (when (get-buffer helm-action-buffer)
          (kill-buffer helm-action-buffer))
        (helm-occur)
        (when (eq helm-exit-status 1)
          (set-window-configuration win-conf)))))

(defun helm-c-grep-buffers (candidate)
  "Action to grep buffers."
  (helm-c-grep-buffers-1 candidate))

(defun helm-c-zgrep-buffers (candidate)
  "Action to zgrep buffers."
  (helm-c-grep-buffers-1 candidate 'zgrep))


;;; Helm interface for pdfgrep
;;  pdfgrep program <http://pdfgrep.sourceforge.net/>
;;  and a pdf-reader (e.g xpdf) are needed.
;;
(defvar helm-c-pdfgrep-default-command "pdfgrep --color never -niH %s %s")
(defvar helm-c-pdfgrep-default-function 'helm-c-pdfgrep-init)
(defvar helm-c-pdfgrep-debug-command-line nil)

(defun helm-c-pdfgrep-init (only-files)
  "Start an asynchronous pdfgrep process in ONLY-FILES list."
  (let* ((fnargs   (helm-c-grep-prepare-candidates
                    (if (file-remote-p helm-ff-default-directory)
                        (mapcar #'(lambda (x)
                                    (file-remote-p x 'localname))
                                only-files)
                        only-files)))
         (cmd-line (format helm-c-pdfgrep-default-command
                           helm-pattern
                           fnargs)))
    (when helm-c-pdfgrep-debug-command-line
      (with-current-buffer (get-buffer-create "*any pdfgrep debug*")
        (goto-char (point-max))
        (insert (concat ">>> " cmd-line "\n\n"))))
    (setq mode-line-format
          '(" " mode-line-buffer-identification " "
            (line-number-mode "%l") " "
            (:eval (propertize "(Pdfgrep Process Running) "
                    'face '((:foreground "red"))))))
    (prog1
        (let ((default-directory helm-ff-default-directory))
          (start-file-process-shell-command "pdfgrep-process" nil cmd-line))
      (message nil)
      (set-process-sentinel
       (get-process "pdfgrep-process")
       #'(lambda (process event)
           (when (string= event "finished\n")
             (with-helm-window
               (helm-update-move-first-line))
             (force-mode-line-update nil)))))))


(defun helm-do-pdfgrep-1 (only)
  "Launch pdfgrep with a list of ONLY files."
  (unless (executable-find "pdfgrep")
    (error "Error: No such program `pdfgrep'."))
  (let* ((helm-compile-source-functions
          ;; rule out helm-match-plugin because the input is one regexp.
          (delq 'helm-compile-source--match-plugin
                (copy-sequence helm-compile-source-functions)))
         ;; Disable match-plugin and use here own highlighting.
         (helm-mp-highlight-delay nil))
    ;; When called as action from an other source e.g *-find-files
    ;; we have to kill action buffer.
    (when (get-buffer helm-action-buffer)
      (kill-buffer helm-action-buffer))
    ;; If `helm-find-files' haven't already started,
    ;; give a default value to `helm-ff-default-directory'.
    (setq helm-ff-default-directory (or helm-ff-default-directory
                                            default-directory))
    (helm
     :sources
     `(((name . "PdfGrep")
        (candidates
         . (lambda ()
             (funcall helm-c-pdfgrep-default-function only)))
        (filtered-candidate-transformer helm-c-grep-cand-transformer)
        (candidate-number-limit . 9999)
        (mode-line . helm-pdfgrep-mode-line-string)
        (action . helm-c-pdfgrep-action)
        (persistent-help . "Jump to PDF Page")
        (requires-pattern . 3)
        (delayed)))
     :keymap helm-c-pdfgrep-map
     :buffer "*helm grep*")))


(defun helm-c-pdfgrep-action (candidate)
  (let* ((split  (helm-c-grep-split-line candidate))
         (pageno (nth 1 split))
         (fname  (car split)))
    (start-file-process-shell-command
     "pdf-reader" nil
     (format-spec helm-c-pdfgrep-default-read-command
                  (list (cons ?f fname) (cons ?p pageno))))))


;; Yank text at point.
;;
;;
;; Internal
(defvar helm-yank-point nil)

;;;###autoload
(defun helm-yank-text-at-point ()
  "Yank text at point in minibuffer."
  (interactive)
  (let (input)
    (flet ((insert-in-minibuffer (word)
             (with-selected-window (minibuffer-window)
               (let ((str helm-pattern))
                 (delete-minibuffer-contents)
                 (set-text-properties 0 (length word) nil word)
                 (insert (concat str word))))))
      (with-helm-current-buffer
        ;; Start to initial point if C-w have never been hit.
        (unless helm-yank-point (setq helm-yank-point (point)))
        (and helm-yank-point (goto-char helm-yank-point))
        (forward-word 1)
        (setq input (buffer-substring-no-properties helm-yank-point (point)))
        (setq helm-yank-point (point))) ; End of last forward-word
      (insert-in-minibuffer input))))

(defun helm-reset-yank-point ()
  (setq helm-yank-point nil))

(add-hook 'helm-after-persistent-action-hook 'helm-reset-yank-point)
(add-hook 'helm-cleanup-hook 'helm-reset-yank-point)


;;; Recentf files
;;
;;
(defvar helm-c-source-recentf
  `((name . "Recentf")
    (init . (lambda ()
              (require 'recentf)
              (or recentf-mode (recentf-mode 1))))
    ;; Needed for filenames with capitals letters.
    (disable-shortcuts)
    (candidates . recentf-list)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (mode-line . helm-generic-file-mode-line-string)
    (match helm-c-match-on-basename)
    (type . file))
  "See (info \"(emacs)File Conveniences\").
Set `recentf-max-saved-items' to a bigger value if default is too small.")

;;; ffap
(eval-when-compile (require 'ffap))
(defvar helm-c-source-ffap-guesser
  `((name . "File at point")
    (init . (lambda () (require 'ffap)))
    (candidates . (lambda ()
                    (helm-aif
                        (with-helm-current-buffer
                          (ffap-guesser))
                        (list it))))
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (mode-line . helm-generic-file-mode-line-string)
    (type . file)))

;;; ffap with line number
(defun helm-c-ffap-file-line-at-point ()
  "Get (FILENAME . LINENO) at point."
  (helm-aif (let (ffap-alist) (ffap-file-at-point))
      (save-excursion
        (beginning-of-line)
        (when (and (search-forward it nil t)
                   (looking-at ":\\([0-9]+\\)"))
          (cons it (string-to-number (match-string 1)))))))

(defun helm-c-ffap-line-candidates ()
  (with-helm-current-buffer
    (helm-attrset 'ffap-line-location (helm-c-ffap-file-line-at-point)))
  (helm-aif (helm-attr 'ffap-line-location)
    (destructuring-bind (file . line) it
      (list (cons (format "%s (line %d)" file line) file)))))

;;; Goto line after opening file by `helm-c-source-ffap-line'.
(defun helm-c-ffap-line-goto-line ()
  (when (car (helm-attr 'ffap-line-location))
    (unwind-protect
        (ignore-errors
          (with-selected-window
              (get-buffer-window
               (get-file-buffer (car (helm-attr 'ffap-line-location))))
            (helm-goto-line (cdr (helm-attr 'ffap-line-location)))))
      (helm-attrset 'ffap-line-location nil))))
(add-hook 'helm-after-action-hook 'helm-c-ffap-line-goto-line)
(add-hook 'helm-after-persistent-action-hook 'helm-c-ffap-line-goto-line)

(defvar helm-c-source-ffap-line
  `((name . "File/Lineno at point")
    (init . (lambda () (require 'ffap)))
    (candidates . helm-c-ffap-line-candidates)
    (keymap . ,helm-map)
    (type . file)))

;;; list of files gleaned from every dired buffer
(defun helm-c-files-in-all-dired-candidates ()
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

(defvar helm-c-source-files-in-all-dired
  '((name . "Files in all dired buffer.")
    (candidates . helm-c-files-in-all-dired-candidates)
    (type . file)))


;;;; <info>
;;; Info pages
(defvar helm-c-info-pages nil
  "All info pages on system.
Will be calculated the first time you invoke helm with this
source.")

(defun helm-c-info-pages-init ()
  "Collect candidates for initial Info node Top."
  (if helm-c-info-pages
      helm-c-info-pages
      (let ((info-topic-regexp "\\* +\\([^:]+: ([^)]+)[^.]*\\)\\.")
            topics)
        (require 'info)
        (with-temp-buffer
          (Info-find-node "dir" "top")
          (goto-char (point-min))
          (while (re-search-forward info-topic-regexp nil t)
            (push (match-string-no-properties 1) topics))
          (kill-buffer))
        (setq helm-c-info-pages topics))))

(defvar helm-c-source-info-pages
  `((name . "Info Pages")
    (init . helm-c-info-pages-init)
    (candidates . helm-c-info-pages)
    (action . (("Show with Info" .(lambda (node-str)
                                    (info (replace-regexp-in-string
                                           "^[^:]+: " "" node-str))))))
    (requires-pattern . 2)))


;;; Man and woman UI
;;
;;
(defvar helm-c-man-pages nil
  "All man pages on system.
Will be calculated the first time you invoke helm with this
source.")

(defun helm-c-man-default-action (candidate)
  "Default action for jumping to a woman or man page from helm."
  (let ((wfiles (woman-file-name-all-completions candidate)))
    (condition-case err
        (if (> (length wfiles) 1)
            (woman-find-file
             (helm-comp-read
              "ManFile: " wfiles :must-match t))
            (woman candidate))
      ;; If woman is unable to format correctly
      ;; use man instead.
      (error (kill-buffer) ; Kill woman buffer.
             (let ((Man-notify-method 'meek))
               (Man-getpage-in-background candidate))))))

(defvar helm-c-source-man-pages
  `((name . "Manual Pages")
    (candidates . (lambda ()
                    (if helm-c-man-pages
                        helm-c-man-pages
                        ;; XEmacs doesn't have a woman :)
                        (setq helm-c-man-pages
                              (ignore-errors
                                (require 'woman)
                                (woman-file-name "")
                                (sort (mapcar 'car woman-topic-all-completions)
                                      'string-lessp))))))
    (action  ("Show with Woman" . helm-c-man-default-action))
    ;; Woman does not work OS X
    ;; http://xahlee.org/emacs/modernization_man_page.html
    (action-transformer . (lambda (actions candidate)
                            (if (eq system-type 'darwin)
                                '(("Show with Man" . man))
                                actions)))
    (requires-pattern . 2)))


;;;; <Command>
;;; Helm M-x - Enhanced M-x UI
;;
;;
;; Another replacement of `M-x' that act exactly like the
;; vanilla Emacs one, no problem of windows configuration, prefix args
;; can be passed before calling `M-x' (e.g C-u M-x..) but also during
;; helm invocation.
;; Documentation of commands available without quitting,
;; Show keybindings of commands.
;; Show history.
(defvar helm-M-x-input-history nil)

(defun* helm-M-x-get-major-mode-command-alist (mode-map)
  "Return alist of MODE-MAP."
  (loop for key being the key-seqs of mode-map using (key-bindings com)
        for str-key  = (key-description key)
        for ismenu   = (string-match "<menu-bar>" str-key)
        unless ismenu collect (cons str-key com)))

(defun helm-get-mode-map-from-mode (mode)
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

(defun helm-M-x-current-mode-map-alist ()
  "Return mode-map alist of current `major-mode'."
  (let ((map (helm-get-mode-map-from-mode major-mode)))
    (when (and map (boundp map))
      (helm-M-x-get-major-mode-command-alist (symbol-value map)))))


(defun helm-M-x-transformer (candidates sources)
  "filtered-candidate-transformer to show bindings in emacs commands.
Show global bindings and local bindings according to current `major-mode'."
  (with-helm-current-buffer
    (loop with local-map = (helm-M-x-current-mode-map-alist)
          for cand in candidates
          for local-key  = (car (rassq cand local-map))
          for key        = (substitute-command-keys (format "\\[%s]" cand))
          collect
          (cons (cond ((and (string-match "^M-x" key) local-key)
                       (format "%s (%s)"
                               cand (propertize
                                     local-key
                                     'face 'helm-M-x-key-face)))
                      ((string-match "^M-x" key) cand)
                      (t (format "%s (%s)"
                                 cand (propertize
                                       key
                                       'face 'helm-M-x-key-face))))
                cand) into ls
          finally return
          (sort ls #'(lambda (x y) (string-lessp (car x) (car y)))))))


;;; Complex command history
;;
;;
(defvar helm-c-source-complex-command-history
  '((name . "Complex Command History")
    (candidates . (lambda () (mapcar 'prin1-to-string command-history)))
    (type . sexp)))

;;; M-x history (not related to `helm-M-x')
;;
;;
(defvar helm-c-source-extended-command-history
  '((name . "Emacs Commands History")
    (candidates
     . (lambda ()
         (helm-fast-remove-dups extended-command-history :test 'equal)))
    (type . command)))

;;; Emacs commands (Basic source for emacs commands)
;;
;;
(defvar helm-c-source-emacs-commands
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
`helm-c-source-emacs-functions'.")


;;;; <Function>
;;; Emacs functions
;;
;;
(defvar helm-c-source-emacs-functions
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
(defvar helm-c-function-abbrev-regexp nil
  "The regexp for `helm-c-source-emacs-functions-with-abbrevs'.
Regexp built from the current `helm-pattern' interpreting it
as abbreviation.
Only for internal use.")

(defun helm-c-match-function-by-abbrev (candidate)
  "Return non-nil if `helm-pattern' is an abbreviation of the function CANDIDATE.

Abbreviations are made by taking the first character from each
word in the function's name, e.g. \"bb\" is an abbrev for
`bury-buffer', \"stb\" is an abbrev for `helm-c-switch-to-buffer'."
  (string-match helm-c-function-abbrev-regexp candidate))

(defvar helm-c-source-emacs-functions-with-abbrevs
  (append helm-c-source-emacs-functions
          '((match helm-c-match-function-by-abbrev
             helm-c-string-match))
          '((init
             . (lambda ()
                 (defadvice helm-update
                     (before helm-c-update-function-abbrev-regexp activate)
                   (let ((char-list (append helm-pattern nil))
                         (str "^"))
                     (dolist (c char-list)
                       (setq str (concat str (list c) "[^-]*-")))
                     (setq str (concat (substring str 0 (1- (length str))) "$"))
                     (setq helm-c-function-abbrev-regexp str))))))))

(defvar helm-c-source-advice
  '((name . "Function Advice")
    (candidates . helm-c-advice-candidates)
    (action ("Toggle Enable/Disable" . helm-c-advice-toggle))
    (persistent-action . helm-c-advice-persistent-action)
    (multiline)
    (persistent-help . "Describe function / C-u C-z: Toggle advice")))
;; (let ((debug-on-signal t))(helm 'helm-c-source-advice))
;; (testadvice)

(defun helm-c-advice-candidates ()
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

(defun helm-c-advice-persistent-action (func-class-advice)
  (if current-prefix-arg
      (helm-c-advice-toggle func-class-advice)
      (describe-function (car func-class-advice))))

(defun helm-c-advice-toggle (func-class-advice)
  (destructuring-bind (function class advice) func-class-advice
    (cond ((ad-advice-enabled advice)
           (ad-advice-set-enabled advice nil)
           (message "Disabled"))
          (t                            ;disabled
           (ad-advice-set-enabled advice t)
           (message "Enabled")))
    (ad-activate function)
    (and helm-in-persistent-action
         (helm-c-advice-update-current-display-string))))

(defun helm-c-advice-update-current-display-string ()
  (helm-edit-current-selection
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
(defvar helm-c-source-emacs-variables
  '((name . "Emacs Variables")
    (candidates . (lambda ()
                    (sort (all-completions "" obarray 'boundp) 'string-lessp)))
    (type . variable)
    (requires-pattern . 2))
  "Source for completing Emacs variables.")


;;; LaCarte
(defvar helm-c-source-lacarte
  '((name . "Lacarte")
    (init . (lambda () (require 'lacarte )))
    (candidates . (lambda () (delete '(nil) (lacarte-get-overall-menu-item-alist))))
    (candidate-number-limit . 9999)
    (action . helm-c-call-interactively))
  "Needs lacarte.el.

http://www.emacswiki.org/cgi-bin/wiki/download/lacarte.el")


;;; Bookmarks
;;
;;
;; Bind some faces for bookmarks.
(defvar helm-c-bookmarks-face1 'helm-ff-directory)
(defvar helm-c-bookmarks-face2 'helm-ff-file)
(defvar helm-c-bookmarks-face3 'helm-bookmarks-su-face)

(eval-when-compile (require 'bookmark))
(defvar helm-c-source-bookmarks
  `((name . "Bookmarks")
    (init . (lambda ()
              (require 'bookmark)))
    (candidates . bookmark-all-names)
    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")

;;; bookmark-set
(defvar helm-c-source-bookmark-set
  '((name . "Set Bookmark")
    (dummy)
    (action . bookmark-set))
  "See (info \"(emacs)Bookmarks\").")

;;; Visible Bookmarks
;; (install-elisp "http://cvs.savannah.gnu.org/viewvc/*checkout*/bm/bm/bm.el")


;; http://d.hatena.ne.jp/grandVin/20080911/1221114327
(defvar helm-c-source-bm
  '((name . "Visible Bookmarks")
    (init . helm-c-bm-init)
    (candidates-in-buffer)
    (type . line))
  "Needs bm.el.

http://www.nongnu.org/bm/")

(defun helm-c-bm-init ()
  "Init function for `helm-c-source-bm'."
  (when (require 'bm nil t)
    (with-no-warnings
      (let ((bookmarks (bm-lists))
            (buf (helm-candidate-buffer 'global)))
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
(defvar helm-c-source-bookmarks-ssh
  '((name . "Bookmarks-ssh")
    (init . (lambda ()
              (require 'bookmark)))
    (candidates . (lambda () (helm-c-collect-bookmarks :ssh t)))
    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")

(defvar helm-c-source-bookmarks-su
  '((name . "Bookmarks-root")
    (init . (lambda ()
              (require 'bookmark)))
    (candidates . (lambda () (helm-c-collect-bookmarks :su t)))
    (filtered-candidate-transformer helm-c-highlight-bookmark-su)

    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")

(defvar helm-c-source-bookmarks-local
  '((name . "Bookmarks-Local")
    (init . (lambda ()
              (require 'bookmark)))
    (candidates . (lambda () (helm-c-collect-bookmarks :local t)))
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark)
    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")

(defun* helm-c-collect-bookmarks (&key local su sudo ssh)
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

(defun helm-c-bookmark-root-logged-p ()
  (catch 'break
    (dolist (i (mapcar #'buffer-name (buffer-list)))
      (when (string-match (format "*tramp/%s ." helm-su-or-sudo) i)
        (throw 'break t)))))

(defun helm-c-highlight-bookmark-su (files source)
  (if (helm-c-bookmark-root-logged-p)
      (helm-c-highlight-bookmark files source)
      (helm-c-highlight-not-logged files source)))

(defun helm-c-highlight-not-logged (files source)
  (loop for i in files
        collect (propertize i 'face helm-c-bookmarks-face3)))

(defun helm-c-highlight-bookmark (bookmarks source)
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
                       (propertize i 'face 'helm-bmkext-info 'help-echo isfile))
                      (;; w3m buffers
                       isw3m
                       (propertize i 'face 'helm-bmkext-w3m 'help-echo isfile))
                      (;; gnus buffers
                       isgnus
                       (propertize i 'face 'helm-bmkext-gnus 'help-echo isfile))
                      (;; Man Woman
                       (or iswoman isman)
                       (propertize i 'face 'helm-bmkext-man 'help-echo isfile))
                      (;; Addressbook
                       isabook
                       (propertize i 'face '((:foreground "Tomato"))))
                      (;; directories
                       (and isfile (file-directory-p isfile))
                       (propertize i 'face helm-c-bookmarks-face1 'help-echo isfile))
                      (;; regular files
                       t
                       (propertize i 'face 'helm-bmkext-file 'help-echo isfile)))))

(defun helm-c-bookmark-jump (candidate)
  "Jump to bookmark from keyboard."
  (let ((current-prefix-arg helm-current-prefix-arg))
    (bookmark-jump candidate)))

;;;###autoload
(defun helm-c-bookmark-run-jump-other-window ()
  "Jump to bookmark from keyboard."
  (interactive)
  (helm-c-quit-and-execute-action 'bookmark-jump-other-window))

;;;###autoload
(defun helm-c-bookmark-run-delete ()
  "Delete bookmark from keyboard."
  (interactive)
  (when (y-or-n-p "Delete bookmark?")
    (helm-c-quit-and-execute-action 'helm-delete-marked-bookmarks)))


;;; Sources to filter bookmark-extensions bookmarks.
;;
;;
;; Dependency: http://mercurial.intuxication.org/hg/emacs-bookmark-extension
;; If you want to enable google-maps in addressbook you will need
;; Julien Danjou google-maps-el package available here:
;; http://julien.danjou.info/google-maps-el.html

(defun helm-c-bmkext-filter-setup-alist (fn &rest args)
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
(defun helm-c-bmkext-run-edit ()
  "Run `bmkext-edit-bookmark' from keyboard."
  (interactive)
  (helm-c-quit-and-execute-action 'bmkext-edit-bookmark))

;;; Addressbook.
;;
;;
(defvar helm-c-source-bmkext-addressbook
  '((name . "Bookmark Addressbook")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . helm-c-bmkext-addressbook-setup-alist)
    (persistent-action
     . (lambda (candidate)
         (let ((bmk (helm-bookmark-get-bookmark-from-name
                     candidate)))
           (bookmark--jump-via bmk 'pop-to-buffer))))
    (persistent-help . "Show contact - Prefix with C-u to append")
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark)
    (action . (("Show Contact(s)"
                . (lambda (candidate)
                    (let* ((contacts (helm-marked-candidates))
                           (current-prefix-arg (or helm-current-prefix-arg
                                                   (> (length contacts) 1))))
                      (bookmark-jump
                       (helm-bookmark-get-bookmark-from-name (car contacts)))
                      (helm-aif (cdr contacts)
                          (loop for bmk in it do
                                (bookmark-jump
                                 (helm-bookmark-get-bookmark-from-name bmk)))))))
               ("Send Mail"
                . (lambda (candidate)
                    (let* ((contacts (helm-marked-candidates))
                           (bmk      (helm-bookmark-get-bookmark-from-name
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
                    (let ((bmk (helm-bookmark-get-bookmark-from-name
                                candidate)))
                      (addressbook-bookmark-edit
                       (assoc bmk bookmark-alist)))))
               ("Insert Email at point"
                . (lambda (candidate)
                    (let* ((bmk   (helm-bookmark-get-bookmark-from-name
                                   candidate))
                           (mlist (split-string
                                   (assoc-default
                                    'email (assoc bmk bookmark-alist))
                                   ", ")))
                      (insert
                       (if (> (length mlist) 1)
                           (helm-comp-read
                            "Insert Mail Address: " mlist :must-match t)
                           (car mlist))))))
               ("Show annotation"
                . (lambda (candidate)
                    (let ((bmk (helm-bookmark-get-bookmark-from-name
                                candidate)))
                      (bookmark-show-annotation bmk))))
               ("Edit annotation"
                . (lambda (candidate)
                    (let ((bmk (helm-bookmark-get-bookmark-from-name
                                candidate)))
                      (bookmark-edit-annotation bmk))))
               ("Show Google map"
                . (lambda (candidate)
                    (let* ((bmk (helm-bookmark-get-bookmark-from-name
                                 candidate))
                           (full-bmk (assoc bmk bookmark-alist)))
                      (addressbook-google-map full-bmk))))))))


(defun helm-c-bmkext-addressbook-setup-alist ()
  "Specialized filter function for bookmarks w3m."
  (helm-c-bmkext-filter-setup-alist 'bmkext-addressbook-alist-only))

;; W3m bookmarks from bookmark-extensions.
(defvar helm-c-source-bookmark-w3m
  '((name . "Bookmark W3m")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . helm-c-bookmark-w3m-setup-alist)
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark)
    (type . bookmark)))

(defun helm-c-bookmark-w3m-setup-alist ()
  "Specialized filter function for bookmarks w3m."
  (helm-c-bmkext-filter-setup-alist 'bmkext-w3m-alist-only))

;; Images
(defvar helm-c-source-bookmark-images
  '((name . "Bookmark Images")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . helm-c-bookmark-images-setup-alist)
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark)
    (type . bookmark)))

(defun helm-c-bookmark-images-setup-alist ()
  "Specialized filter function for images bookmarks."
  (helm-c-bmkext-filter-setup-alist 'bmkext-image-file-alist-only))

;; Woman Man
(defvar helm-c-source-bookmark-man
  '((name . "Bookmark Woman&Man")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . helm-c-bookmark-man-setup-alist)
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark)
    (type . bookmark)))

(defun helm-c-bookmark-man-setup-alist ()
  "Specialized filter function for bookmarks w3m."
  (append (helm-c-bmkext-filter-setup-alist 'bmkext-man-alist-only)
          (helm-c-bmkext-filter-setup-alist 'bmkext-woman-alist-only)))

;; Gnus
(defvar helm-c-source-bookmark-gnus
  '((name . "Bookmark Gnus")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . helm-c-bookmark-gnus-setup-alist)
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark)
    (type . bookmark)))

(defun helm-c-bookmark-gnus-setup-alist ()
  "Specialized filter function for bookmarks gnus."
  (helm-c-bmkext-filter-setup-alist 'bmkext-gnus-alist-only))

;; Info
(defvar helm-c-source-bookmark-info
  '((name . "Bookmark Info")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . helm-c-bookmark-info-setup-alist)
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark)
    (type . bookmark)))

(defun helm-c-bookmark-info-setup-alist ()
  "Specialized filter function for bookmarks info."
  (helm-c-bmkext-filter-setup-alist 'bmkext-info-alist-only))

;; Local Files&directories
(defvar helm-c-source-bookmark-files&dirs
  '((name . "Bookmark Files&Directories")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . helm-c-bookmark-local-files-setup-alist)
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark)
    (type . bookmark)))

(defun helm-c-bookmark-local-files-setup-alist ()
  "Specialized filter function for bookmarks locals files."
  (helm-c-bmkext-filter-setup-alist 'bmkext-local-file-alist-only))

;; Su Files&directories
(defvar helm-c-source-bookmark-su-files&dirs
  '((name . "Bookmark Root-Files&Directories")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . helm-c-bookmark-su-files-setup-alist)
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark-su)
    (type . bookmark)))

(defun helm-c-bookmark-su-files-setup-alist ()
  "Specialized filter function for bookmarks su/sudo files."
  (declare (special bmkext-su-or-sudo-regexp))
  (loop
        with l = (helm-c-bmkext-filter-setup-alist 'bmkext-remote-file-alist-only)
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
(defvar helm-c-source-bookmark-ssh-files&dirs
  '((name . "Bookmark Ssh-Files&Directories")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . helm-c-bookmark-ssh-files-setup-alist)
    (filtered-candidate-transformer . helm-c-adaptive-sort)
    (type . bookmark)))

(defun helm-c-bookmark-ssh-files-setup-alist ()
  "Specialized filter function for bookmarks ssh files."
  (loop
        with l = (helm-c-bmkext-filter-setup-alist 'bmkext-remote-file-alist-only)
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

(defvar helm-firefox-bookmark-url-regexp "\\(https\\|http\\|ftp\\|about\\|file\\)://[^ \"]*")
(defvar helm-firefox-bookmarks-regexp ">\\([^><]+.[^</a>]\\)")

(defun helm-get-firefox-user-init-dir ()
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

(defun helm-guess-firefox-bookmark-file ()
  "Return the path of the Firefox bookmarks file."
  (concat (helm-get-firefox-user-init-dir) "bookmarks.html"))

(defun helm-html-bookmarks-to-alist (file url-regexp bmk-regexp)
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

(defvar helm-c-firefox-bookmarks-alist nil)
(defvar helm-c-source-firefox-bookmarks
  '((name . "Firefox Bookmarks")
    (init . (lambda ()
              (setq helm-c-firefox-bookmarks-alist
                    (helm-html-bookmarks-to-alist
                     (helm-guess-firefox-bookmark-file)
                     helm-firefox-bookmark-url-regexp
                     helm-firefox-bookmarks-regexp))))
    (candidates . (lambda ()
                    (mapcar #'car helm-c-firefox-bookmarks-alist)))
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-firefox-bookmarks)
    (action . (("Browse Url"
                . (lambda (candidate)
                    (helm-c-browse-url
                     (helm-c-firefox-bookmarks-get-value candidate))))
               ("Copy Url"
                . (lambda (elm)
                    (kill-new (helm-c-w3m-bookmarks-get-value elm))))))))


(defun helm-c-firefox-bookmarks-get-value (elm)
  (assoc-default elm helm-c-firefox-bookmarks-alist))

(defun helm-c-highlight-firefox-bookmarks (bookmarks source)
  (loop for i in bookmarks
        collect (propertize
                 i 'face '((:foreground "YellowGreen"))
                 'help-echo (helm-c-firefox-bookmarks-get-value i))))



;;; W3m bookmark - helm interface.
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
(defvar helm-w3m-bookmarks-regexp ">\\([^><]+.[^</a>]\\)")
(defvar helm-w3m-bookmark-url-regexp "\\(https\\|http\\|ftp\\|file\\)://[^>]*")
(defvar helm-c-w3m-bookmarks-alist nil)
(defvar helm-c-source-w3m-bookmarks
  '((name . "W3m Bookmarks")
    (init . (lambda ()
              (setq helm-c-w3m-bookmarks-alist
                    (helm-html-bookmarks-to-alist
                     w3m-bookmark-file
                     helm-w3m-bookmark-url-regexp
                     helm-w3m-bookmarks-regexp))))
    (candidates . (lambda ()
                    (mapcar #'car helm-c-w3m-bookmarks-alist)))
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-w3m-bookmarks)
    (action . (("Browse Url"
                . (lambda (candidate)
                    (helm-c-w3m-browse-bookmark candidate)))
               ("Copy Url"
                . (lambda (elm)
                    (kill-new (helm-c-w3m-bookmarks-get-value elm))))
               ("Browse Url Externally"
                . (lambda (candidate)
                    (helm-c-w3m-browse-bookmark candidate t)))
               ("Delete Bookmark"
                . (lambda (candidate)
                    (helm-c-w3m-delete-bookmark candidate)))
               ("Rename Bookmark"
                . (lambda (candidate)
                    (helm-c-w3m-rename-bookmark candidate)))))
    (persistent-action . (lambda (candidate)
                           (if current-prefix-arg
                               (helm-c-w3m-browse-bookmark candidate t)
                               (helm-c-w3m-browse-bookmark candidate nil t))))
    (persistent-help . "Open URL with emacs-w3m in new tab / \
C-u \\[helm-execute-persistent-action]: Open URL with Firefox"))
  "Needs w3m and emacs-w3m.

http://w3m.sourceforge.net/
http://emacs-w3m.namazu.org/")


(defun helm-c-w3m-bookmarks-get-value (elm)
  (replace-regexp-in-string
   "\"" "" (cdr (assoc elm helm-c-w3m-bookmarks-alist))))

(defun helm-c-w3m-browse-bookmark (elm &optional use-external new-tab)
  (let* ((fn  (if use-external 'helm-c-browse-url 'w3m-browse-url))
         (arg (and (eq fn 'w3m-browse-url) new-tab)))
    (funcall fn (helm-c-w3m-bookmarks-get-value elm) arg)))

(defun helm-c-highlight-w3m-bookmarks (bookmarks source)
  (loop for i in bookmarks
        collect (propertize
                 i 'face 'helm-w3m-bookmarks-face
                 'help-echo (helm-c-w3m-bookmarks-get-value i))))


(defun helm-c-w3m-delete-bookmark (elm)
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

(defun helm-c-w3m-rename-bookmark (elm)
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
(defvar helm-c-source-elisp-library-scan
  '((name . "Elisp libraries (Scan)")
    (init . (helm-c-elisp-library-scan-init))
    (candidates-in-buffer)
    (action ("Find library"
             . (lambda (candidate) (find-file (find-library-name candidate))))
     ("Find library other window"
      . (lambda (candidate)
          (find-file-other-window (find-library-name candidate))))
     ("Load library"
      . (lambda (candidate) (load-library candidate))))))

(defun helm-c-elisp-library-scan-init ()
  "Init helm buffer status."
  (let ((helm-buffer (helm-candidate-buffer 'global))
        (library-list (helm-c-elisp-library-scan-list)))
    (with-current-buffer helm-buffer
      (dolist (library library-list)
        (insert (format "%s\n" library))))))

(defun helm-c-elisp-library-scan-list (&optional dirs string)
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
(defvar helm-c-imenu-delimiter " / ")

(defvar helm-c-imenu-index-filter nil)
(make-variable-buffer-local 'helm-c-imenu-index-filter)

(defvar helm-c-cached-imenu-alist nil)
(make-variable-buffer-local 'helm-c-cached-imenu-alist)

(defvar helm-c-cached-imenu-candidates nil)
(make-variable-buffer-local 'helm-c-cached-imenu-candidates)

(defvar helm-c-cached-imenu-tick nil)
(make-variable-buffer-local 'helm-c-cached-imenu-tick)

(eval-when-compile (require 'imenu))
(setq imenu-auto-rescan t)

(defun helm-imenu-create-candidates (entry)
  "Create candidates with ENTRY."
  (if (listp (cdr entry))
      (mapcan
       (lambda (sub)
         (if (consp (cdr sub))
             (mapcar
              (lambda (subentry)
                (concat (car entry) helm-c-imenu-delimiter subentry))
              (helm-imenu-create-candidates sub))
             (list (concat (car entry) helm-c-imenu-delimiter (car sub)))))
       (cdr entry))
      (list entry)))

(defvar helm-c-source-imenu
  '((name . "Imenu")
    (init . (lambda () (require 'imenu)))
    (candidates . helm-c-imenu-candidates)
    (persistent-action . (lambda (elm)
                           (helm-c-imenu-default-action elm)
                           (unless (fboundp 'semantic-imenu-tag-overlay)
                             (helm-match-line-color-current-line))))
    (persistent-help . "Show this entry")
    (action . helm-c-imenu-default-action))
  "See (info \"(emacs)Imenu\")")


(defun helm-c-imenu-candidates ()
  (with-helm-current-buffer
    (let ((tick (buffer-modified-tick)))
      (if (eq helm-c-cached-imenu-tick tick)
          helm-c-cached-imenu-candidates
          (setq imenu--index-alist nil)
          (setq helm-c-cached-imenu-tick tick
                helm-c-cached-imenu-candidates
                (ignore-errors
                  (mapcan
                   'helm-imenu-create-candidates
                   (setq helm-c-cached-imenu-alist
                         (let ((index (imenu--make-index-alist)))
                           (if helm-c-imenu-index-filter
                               (funcall helm-c-imenu-index-filter index)
                               index))))))
          (setq helm-c-cached-imenu-candidates
                (mapcar #'(lambda (x)
                            (if (stringp x)
                                x
                                (car x)))
                        helm-c-cached-imenu-candidates))))))

(setq imenu-default-goto-function 'imenu-default-goto-function)
(defun helm-c-imenu-default-action (elm)
  "The default action for `helm-c-source-imenu'."
  (let ((path (split-string elm helm-c-imenu-delimiter))
        (alist helm-c-cached-imenu-alist))
    (dolist (elm path)
      (setq alist (assoc elm alist)))
    (imenu alist)))



;;; Ctags
;;
;;
(defvar helm-c-ctags-modes
  '( c-mode c++-mode awk-mode csharp-mode java-mode javascript-mode lua-mode
    makefile-mode pascal-mode perl-mode cperl-mode php-mode python-mode
    scheme-mode sh-mode slang-mode sql-mode tcl-mode ))

(defun helm-c-source-ctags-init ()
  (when (and buffer-file-name
             (memq major-mode helm-c-ctags-modes)
             (helm-current-buffer-is-modified))
    (with-current-buffer (helm-candidate-buffer 'local)
      (call-process-shell-command
       (if (string-match "\\.el\\.gz$" helm-buffer-file-name)
           (format "ctags -e -u -f- --language-force=lisp --fields=n =(zcat %s) "
                   helm-buffer-file-name)
           (format "ctags -e -u -f- --fields=n %s " helm-buffer-file-name))
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

(defvar helm-c-source-ctags
  '((name . "Exuberant ctags")
    (init . helm-c-source-ctags-init)
    (candidates-in-buffer)
    (adjust)
    (type . line))
  "Needs Exuberant Ctags.

http://ctags.sourceforge.net/")


;;; Etags
;;
;;
;; helm-etags.el is deprecated, if this file is found,
;; warn user at compile time.
(eval-when-compile
  (when (locate-library "helm-etags.el")
    (display-warning
     '(helm-config)
     "You are using obsolete library `helm-etags.el' and should remove it."
     :warning)))

(defvar helm-c-etags-tag-file-dir nil
  "Etags file directory.")
(defvar helm-c-etags-mtime-alist nil
  "Store the last modification time of etags files here.")
(defvar helm-c-etags-cache (make-hash-table :test 'equal)
  "Cache content of etags files used here for faster access.")

(defun helm-c-etags-get-tag-file (&optional directory)
  "Return the path of etags file if found."
  ;; Get tag file from `default-directory' or upper directory.
  (let ((current-dir (helm-c-etags-find-tag-file-directory
                      (or directory default-directory))))
    ;; Return nil if not find tag file.
    (when current-dir
      ;; Set tag file directory.
      (setq helm-c-etags-tag-file-dir current-dir)
      (expand-file-name helm-c-etags-tag-file-name current-dir))))

(defun helm-c-etags-find-tag-file-directory (current-dir)
  "Try to find the directory containing tag file.
If not found in CURRENT-DIR search in upper directory."
  (flet ((file-exists? (dir)
           (let ((tag-path (expand-file-name
                            helm-c-etags-tag-file-name dir)))
             (and (stringp tag-path)
                  (file-regular-p tag-path)
                  (file-readable-p tag-path)))))
    (loop with count = 0
          until (file-exists? current-dir)
          ;; Return nil if outside the value of
          ;; `helm-c-etags-tag-file-search-limit'.
          if (= count helm-c-etags-tag-file-search-limit)
          do (return nil)
          ;; Or search upper directories.
          else
          do (incf count)
          (setq current-dir (expand-file-name (concat current-dir "../")))
          finally return current-dir)))

(defun helm-c-source-etags-header-name (x)
  "Create header name for this helm etags session."
  (concat "Etags in "
          (with-helm-current-buffer
            (helm-c-etags-get-tag-file))))

(defmacro helm-c-etags-create-buffer (file)
  "Create the `helm-buffer' based on contents of etags tag FILE."
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
                       (helm-aif (string-match "\177" i)
                           (substring i 0 it)
                         i))
           do (cond ((and elm (string-match "^\\(.+\\),[0-9]+" elm))
                     (setq fname (match-string 1 elm)))
                    (elm (setq cand (concat fname ": " elm)))
                    (t (setq cand nil)))
           when cand do (progn
                          (insert (concat cand "\n"))
                          (progress-reporter-update progress-reporter count)))))

(defun helm-c-etags-init ()
  "Feed `helm-buffer' using `helm-c-etags-cache' or tag file.
If no entry in cache, create one."
  (let ((tagfile (helm-c-etags-get-tag-file)))
    (when tagfile
      (with-current-buffer (helm-candidate-buffer 'global)
        (helm-aif (gethash tagfile helm-c-etags-cache)
            ;; An entry is present in cache, insert it.
            (insert it)
          ;; No entry, create a new buffer using content of tag file (slower).
          (helm-c-etags-create-buffer tagfile)
          ;; Store content of buffer in cache.
          (puthash tagfile (buffer-string) helm-c-etags-cache)
          ;; Store or set the last modification of tag file.
          (helm-aif (assoc tagfile helm-c-etags-mtime-alist)
              ;; If an entry exists modify it.
              (setcdr it (helm-c-etags-mtime tagfile))
            ;; No entry create a new one.
            (add-to-list 'helm-c-etags-mtime-alist
                         (cons tagfile (helm-c-etags-mtime tagfile)))))))))

(defvar helm-c-source-etags-select
  '((name . "Etags")
    (header-name . helm-c-source-etags-header-name)
    (init . helm-c-etags-init)
    (candidates-in-buffer)
    (search . (helm-c-etags-search-fn))
    (mode-line . helm-etags-mode-line-string)
    (action . helm-c-etags-default-action)
    (persistent-action . (lambda (candidate)
                           (helm-c-etags-default-action candidate)
                           (helm-match-line-color-current-line))))
  "Helm source for Etags.")

(defun helm-c-etags-search-fn (pattern)
  "Search function for `helm-c-source-etags-select'."
  (re-search-forward
   (if helm-c-etags-use-regexp-search
       (format helm-c-etags-search-regexp pattern)
       pattern)
   nil t))

(defun helm-c-etags-default-action (candidate)
  "Helm default action to jump to an etags entry."
  (let* ((split (split-string candidate ": "))
         (fname (expand-file-name
                 (car split) helm-c-etags-tag-file-dir))
         (elm   (cadr split)))
    (find-file fname)
    (goto-char (point-min))
    (search-forward elm nil t)
    (goto-char (match-beginning 0))))

(defun helm-c-etags-mtime (file)
  "Last modification time of etags tag FILE."
  (cadr (nth 5 (file-attributes file))))

(defun helm-c-etags-file-modified-p (file)
  "Check if tag FILE have been modified in this session.
If FILE is nil return nil."
  (let ((last-modif (and file
                         (assoc-default file helm-c-etags-mtime-alist))))
    (and last-modif
         (/= last-modif (helm-c-etags-mtime file)))))



;;; Semantic
;;
;; 
(defvar helm-semantic-candidates nil)

(defun helm-semantic-construct-candidates (tags depth)
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
                        (helm-semantic-construct-candidates
                         (semantic-tag-components tag) (1+ depth)))))))
      tags))))

(defun helm-semantic-default-action (candidate)
  (let ((tag (cdr (assoc candidate helm-semantic-candidates))))
    (semantic-go-to-tag tag)))

(defvar helm-c-source-semantic
  '((name . "Semantic Tags")
    (init . (lambda ()
              (setq helm-semantic-candidates
                    (ignore-errors (helm-semantic-construct-candidates
                                    (semantic-fetch-tags) 0)))))
    (candidates . (lambda ()
                    (if helm-semantic-candidates
                        (mapcar 'car helm-semantic-candidates))))
    (persistent-action . (lambda (elm)
                           (helm-semantic-default-action elm)
                           (helm-match-line-color-current-line)))
    (persistent-help . "Show this entry")
    (action . helm-semantic-default-action)
    "Needs semantic in CEDET.

http://cedet.sourceforge.net/semantic.shtml
http://cedet.sourceforge.net/"))



;;; Helm interface of `simple-call-tree.el'.
;;
;; <http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el>
;;
;; Function is called by
(defvar helm-c-source-simple-call-tree-functions-callers
  '((name . "Function is called by")
    (init . helm-c-simple-call-tree-functions-callers-init)
    (multiline)
    (candidates . helm-c-simple-call-tree-candidates)
    (persistent-action . helm-c-simple-call-tree-persistent-action)
    (persistent-help . "Show function definitions by rotation")
    (action ("Find definition selected by persistent-action" .
             helm-c-simple-call-tree-find-definition)))
  "Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el")

(defvar helm-c-simple-call-tree-tick nil)
(make-variable-buffer-local 'helm-c-simple-call-tree-tick)
(defun helm-c-simple-call-tree-analyze-maybe ()
  (unless (eq (buffer-chars-modified-tick) helm-c-simple-call-tree-tick)
    (simple-call-tree-analyze)
    (setq helm-c-simple-call-tree-tick (buffer-chars-modified-tick))))

(defun helm-c-simple-call-tree-init-base (function message)
  (require 'simple-call-tree)
  (with-no-warnings
    (when (helm-current-buffer-is-modified)
      (helm-c-simple-call-tree-analyze-maybe)
      (let ((list (funcall function simple-call-tree-alist)))
        (with-current-buffer (helm-candidate-buffer 'local)
          (dolist (entry list)
            (let ((funcs (concat "  " (mapconcat #'identity (cdr entry) "\n  "))))
              (insert (car entry) message
                      (if (string= funcs "  ")
                          "  no functions."
                          funcs)
                      "\n\n"))))))))

(defun helm-c-simple-call-tree-functions-callers-init ()
  (helm-c-simple-call-tree-init-base 'simple-call-tree-invert
                                         " is called by\n"))

(defun helm-c-simple-call-tree-candidates ()
  (with-current-buffer (helm-candidate-buffer)
    (split-string (buffer-string) "\n\n")))

(defvar helm-c-simple-call-tree-related-functions nil)
(defvar helm-c-simple-call-tree-function-index 0)
(defun helm-c-simple-call-tree-persistent-action (candidate)
  (unless (eq last-command 'helm-execute-persistent-action)
    (setq helm-c-simple-call-tree-related-functions
          (delete "no functions."
                  (split-string
                   (replace-regexp-in-string "  \\| is called by\\| calls "
                                             "" candidate)
                   "\n")))
    (setq helm-c-simple-call-tree-function-index -1))
  (incf helm-c-simple-call-tree-function-index)
  (helm-c-simple-call-tree-find-definition candidate))

(defun helm-c-simple-call-tree-find-definition (candidate)
  (find-function
   (intern
    (nth (mod helm-c-simple-call-tree-function-index
              (length helm-c-simple-call-tree-related-functions))
         helm-c-simple-call-tree-related-functions))))


;;; Function calls
(defvar helm-c-source-simple-call-tree-callers-functions
  '((name . "Function calls")
    (init . helm-c-simple-call-tree-callers-functions-init)
    (multiline)
    (candidates . helm-c-simple-call-tree-candidates)
    (persistent-action . helm-c-simple-call-tree-persistent-action)
    (persistent-help . "Show function definitions by rotation")
    (action ("Find definition selected by persistent-action" .
             helm-c-simple-call-tree-find-definition)))
  "Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el")

(defun helm-c-simple-call-tree-callers-functions-init ()
  (helm-c-simple-call-tree-init-base 'identity " calls \n"))




;;; Helm UI of auto-document.el
;;
;; <http://www.emacswiki.org/cgi-bin/wiki/download/auto-document.el>
;;
;; Commands/Options with doc
(defvar helm-c-auto-document-data nil)
(make-variable-buffer-local 'helm-c-auto-document-data)
(defvar helm-c-source-commands-and-options-in-file
  '((name . "Commands/Options in file")
    (header-name
     . (lambda (x) (format "Commands/Options in %s"
                           (buffer-local-value 'buffer-file-name
                                               helm-current-buffer))))
    (candidates . helm-command-and-options-candidates)
    (multiline)
    (action . imenu))
  "List Commands and Options with doc. It needs auto-document.el .

http://www.emacswiki.org/cgi-bin/wiki/download/auto-document.el")

(eval-when-compile (require 'auto-document nil t))
(defun helm-command-and-options-candidates ()
  (with-helm-current-buffer
    (when (and (require 'auto-document nil t)
               (eq major-mode 'emacs-lisp-mode)
               (or (helm-current-buffer-is-modified)
                   (not helm-c-auto-document-data)))
      (or imenu--index-alist (imenu--make-index-alist t))
      (setq helm-c-auto-document-data
            (destructuring-bind (commands options)
                (adoc-construct helm-current-buffer)
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
    helm-c-auto-document-data))



;;;; <Color and Face>
;;

;;; Customize Face
;;
;;
(defvar helm-c-source-customize-face
  '((name . "Customize Face")
    (init . (lambda ()
              (unless (helm-candidate-buffer)
                (save-selected-window
                  (list-faces-display))
                (helm-candidate-buffer (get-buffer "*Faces*")))))
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (action . (lambda (line)
                (customize-face (intern (car (split-string line))))))
    (requires-pattern . 3))
  "See (info \"(emacs)Faces\")")

;;; Colors browser
;;
;;
(defvar helm-c-source-colors
  '((name . "Colors")
    (init . (lambda () (unless (helm-candidate-buffer)
                         (save-selected-window
                           (list-colors-display))
                         (helm-candidate-buffer (get-buffer "*Colors*")))))
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (action
     ("Copy Name" . (lambda (candidate)
                      (kill-new (helm-c-colors-get-name candidate))))
     ("Copy RGB" . (lambda (candidate)
                     (kill-new (helm-c-colors-get-rgb candidate))))
     ("Insert Name" . (lambda (candidate)
                        (with-helm-current-buffer
                          (insert (helm-c-colors-get-name candidate)))))
     ("Insert RGB" . (lambda (candidate)
                       (with-helm-current-buffer
                         (insert (helm-c-colors-get-rgb candidate))))))))

(defun helm-c-colors-get-name (candidate)
  "Get color name."
  (replace-regexp-in-string
   " " ""
   (with-temp-buffer
     (insert (capitalize candidate))
     (goto-char (point-min))
     (search-forward-regexp "\\s-\\{2,\\}")
     (delete-region (point) (point-max))
     (buffer-string))))

(defun helm-c-colors-get-rgb (candidate)
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
(defvar helm-c-source-tracker-search
  '((name . "Tracker Search")
    (candidates . (lambda ()
                    (start-process "tracker-search-process" nil
                                   "tracker-search"
                                   helm-pattern)))
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files matching the current input pattern
with the tracker desktop search.")

;;; Spotlight (MacOS X desktop search)
(defvar helm-c-source-mac-spotlight
  '((name . "mdfind")
    (candidates
     . (lambda () (start-process "mdfind-process" nil "mdfind" helm-pattern)))
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files via Spotlight's command line
utility mdfind.")

;;; Picklist
(defvar helm-c-source-picklist
  '((name . "Picklist")
    (candidates . (lambda () (mapcar 'car picklist-list)))
    (type . file)))



;;; Kill ring
;;
;;
(defvar helm-c-source-kill-ring
  `((name . "Kill Ring")
    (init . (lambda () (helm-attrset 'last-command last-command)))
    (candidates . helm-c-kill-ring-candidates)
    (filtered-candidate-transformer helm-c-kill-ring-transformer)
    (action . helm-c-kill-ring-action)
    (keymap . ,helm-kill-ring-map)
    (last-command)
    (migemo)
    (multiline))
  "Source for browse and insert contents of kill-ring.")

(defun helm-c-kill-ring-candidates ()
  (loop for kill in (helm-fast-remove-dups kill-ring :test 'equal)
        unless (or (< (length kill) helm-kill-ring-threshold)
                   (string-match "^[\\s\\t]+$" kill))
        collect kill))

(defun helm-c-kill-ring-transformer (candidates source)
  "Display only the `helm-c-kill-ring-max-lines-number' lines of candidate."
  (loop for i in candidates
        for nlines = (with-temp-buffer (insert i) (count-lines (point-min) (point-max)))
        if (and helm-c-kill-ring-max-lines-number
                (> nlines helm-c-kill-ring-max-lines-number))
        collect (cons
                 (with-temp-buffer
                   (insert i)
                   (goto-char (point-min))
                   (concat
                    (buffer-substring
                     (point-min)
                     (save-excursion
                       (forward-line helm-c-kill-ring-max-lines-number)
                       (point)))
                    "[...]")) i)
        else collect i))

(defun helm-c-kill-ring-action (str)
  "Insert STR in `kill-ring' and set STR to the head.
If this action is executed just after `yank',
replace with STR as yanked string."
  (setq kill-ring (delete str kill-ring))
  (if (not (eq (helm-attr 'last-command) 'yank))
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
;; DO NOT include these sources in `helm-sources' use
;; the commands `helm-mark-ring', `helm-global-mark-ring' or
;; `helm-all-mark-rings' instead.

(defun helm-c-source-mark-ring-candidates ()
  (flet ((get-marks (pos)
           (save-excursion
             (goto-char pos)
             (beginning-of-line)
             (let ((line  (car (split-string (thing-at-point 'line) "[\n\r]"))))
               (when (string= "" line)
                 (setq line  "<EMPTY LINE>"))
               (format "%7d: %s" (line-number-at-pos) line)))))
    (with-helm-current-buffer
      (loop
            with marks = (if (mark) (cons (mark-marker) mark-ring) mark-ring)
            with recip = nil
            for i in marks
            for m = (get-marks i)
            unless (member m recip)
            collect m into recip
            finally return recip))))

(defvar helm-mark-ring-cache nil)
(defvar helm-c-source-mark-ring
  '((name . "mark-ring")
    (init . (lambda ()
              (setq helm-mark-ring-cache
                    (ignore-errors (helm-c-source-mark-ring-candidates)))))
    (candidates . (lambda ()
                    (helm-aif helm-mark-ring-cache
                        it)))
    (action . (("Goto line"
                . (lambda (candidate)
                    (helm-goto-line (string-to-number candidate))))))
    (persistent-action . (lambda (candidate)
                           (helm-goto-line (string-to-number candidate))
                           (helm-match-line-color-current-line)))
    (persistent-help . "Show this line")))


;;; Global-mark-ring
(defvar helm-c-source-global-mark-ring
  '((name . "global-mark-ring")
    (candidates . helm-c-source-global-mark-ring-candidates)
    (action . (("Goto line"
                . (lambda (candidate)
                    (let ((items (split-string candidate ":")))
                      (helm-c-switch-to-buffer (second items))
                      (helm-goto-line (string-to-number (car items))))))))
    (persistent-action . (lambda (candidate)
                           (let ((items (split-string candidate ":")))
                             (helm-c-switch-to-buffer (second items))
                             (helm-goto-line (string-to-number (car items)))
                             (helm-match-line-color-current-line))))
    (persistent-help . "Show this line")))

(defun helm-c-source-global-mark-ring-candidates ()
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
(defvar helm-c-source-register
  '((name . "Registers")
    (candidates . helm-c-register-candidates)
    (action-transformer . helm-c-register-action-transformer)
    (multiline)
    (action))
  "See (info \"(emacs)Registers\")")

(defun helm-c-register-candidates ()
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
                     val 0 (min (length val) helm-c-register-max-offset))
                    (if (> (length val) helm-c-register-max-offset)
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

(defun helm-c-register-action-transformer (actions register-and-functions)
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
                        helm-current-prefix-arg (car c))))
          (undo-tree-restore-state-from-register
           "Restore Undo-tree register"
           (lambda (c) (and (fboundp 'undo-tree-restore-state-from-register)
                            (undo-tree-restore-state-from-register (car c))))))
        for func in (cdr register-and-functions)
        for cell = (assq func func-actions)
        when cell
        collect (cdr cell)))



;;; Latex completion
(defun helm-c-latex-math-candidates ()
  "Collect candidates for latex math completion."
  (declare (special LaTeX-math-menu))
  (loop for i in (cddr LaTeX-math-menu)
        for elm = (loop for s in i when (vectorp s)
                        collect (cons (aref s 0) (aref s 1)))
        append elm))

(defvar helm-c-source-latex-math
  '((name . "Latex Math Menu")
    (init . (lambda ()
              (with-helm-current-buffer
                (LaTeX-math-mode 1))))
    (candidate-number-limit . 9999)
    (candidates . helm-c-latex-math-candidates)
    (action . (lambda (candidate)
                (call-interactively candidate)))))


;;;; <Headline Extraction>
(defvar helm-c-source-fixme
  '((name . "TODO/FIXME/DRY comments")
    (headline . "^.*\\<\\(TODO\\|FIXME\\|DRY\\)\\>.*$")
    (adjust)
    (recenter))
  "Show TODO/FIXME/DRY comments in current file.")

(defvar helm-c-source-rd-headline
  '((name . "RD HeadLine")
    (headline  "^= \\(.+\\)$" "^== \\(.+\\)$" "^=== \\(.+\\)$" "^==== \\(.+\\)$")
    (condition . (memq major-mode '(rdgrep-mode rd-mode)))
    (migemo)
    (subexp . 1))
  "Show RD headlines.

RD is Ruby's POD.
http://en.wikipedia.org/wiki/Ruby_Document_format")

(defvar helm-c-source-oddmuse-headline
  '((name . "Oddmuse HeadLine")
    (headline  "^= \\(.+\\) =$" "^== \\(.+\\) ==$"
     "^=== \\(.+\\) ===$" "^==== \\(.+\\) ====$")
    (condition . (memq major-mode '(oddmuse-mode yaoddmuse-mode)))
    (migemo)
    (subexp . 1))
  "Show Oddmuse headlines, such as EmacsWiki.")

(defvar helm-c-source-emacs-source-defun
  '((name . "Emacs Source DEFUN")
    (headline . "DEFUN\\|DEFVAR")
    (condition . (string-match "/emacs2[0-9].+/src/.+c$"
                  (or buffer-file-name ""))))
  "Show DEFUN/DEFVAR in Emacs C source file.")

(defvar helm-c-source-emacs-lisp-expectations
  '((name . "Emacs Lisp Expectations")
    (headline . "(desc[ ]\\|(expectations")
    (condition . (eq major-mode 'emacs-lisp-mode)))
  "Show descriptions (desc) in Emacs Lisp Expectations.

http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")

(defvar helm-c-source-emacs-lisp-toplevels
  '((name . "Emacs Lisp Toplevel / Level 4 Comment / Linkd Star")
    (headline . "^(\\|(@\\*\\|^;;;;")
    (get-line . buffer-substring)
    (condition . (eq major-mode 'emacs-lisp-mode))
    (adjust))
  "Show top-level forms, level 4 comments and linkd stars (optional) in Emacs Lisp.
linkd.el is optional because linkd stars are extracted by regexp.
http://www.emacswiki.org/cgi-bin/wiki/download/linkd.el")


;;; Helm yaoddmuse
;;
;; Be sure to have yaoddmuse.el installed
;; install-elisp may be required if you want to install elisp file from here.
(defvar helm-yaoddmuse-use-cache-file nil)
(defvar helm-c-yaoddmuse-cache-file "~/.emacs.d/yaoddmuse-cache.el")
(defvar helm-c-yaoddmuse-ew-cache nil)

(defun helm-yaoddmuse-get-candidates ()
  (declare (special yaoddmuse-pages-hash))
  (if helm-yaoddmuse-use-cache-file
      (ignore-errors
        (unless helm-c-yaoddmuse-ew-cache
          (load helm-c-yaoddmuse-cache-file)
          (setq helm-c-yaoddmuse-ew-cache
                (gethash "EmacsWiki" yaoddmuse-pages-hash)))
        helm-c-yaoddmuse-ew-cache)
      (yaoddmuse-update-pagename t)
      (gethash "EmacsWiki" yaoddmuse-pages-hash)))

(defvar helm-c-source-yaoddmuse-emacswiki-edit-or-view
  '((name . "Yaoddmuse Edit or View (EmacsWiki)")
    (candidates . helm-yaoddmuse-get-candidates)
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
                    (yaoddmuse-edit "EmacsWiki" helm-input)))
               ("Update cache"
                . (lambda (candidate)
                    (if helm-yaoddmuse-use-cache-file
                        (progn
                          (helm-yaoddmuse-cache-pages t)
                          (setq helm-c-yaoddmuse-ew-cache
                                (gethash "EmacsWiki" yaoddmuse-pages-hash)))
                        (yaoddmuse-update-pagename))))))
    (action-transformer helm-c-yaoddmuse-action-transformer))
  "Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el")


(defvar helm-c-source-yaoddmuse-emacswiki-post-library
  '((name . "Yaoddmuse Post library (EmacsWiki)")
    (init . (helm-yaoddmuse-init))
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


(defun helm-c-yaoddmuse-action-transformer (actions candidate)
  "Allow the use of `install-elisp' only on elisp files."
  (if (string-match "\.el$" candidate)
      (append actions '(("Install Elisp"
                         . (lambda (elm)
                             (install-elisp-from-emacswiki elm)))))
      actions))

;;;###autoload
(defun helm-yaoddmuse-cache-pages (&optional load)
  "Fetch the list of files on emacswiki and create cache file.
If load is non--nil load the file and feed `yaoddmuse-pages-hash'."
  (interactive)
  (declare (special yaoddmuse-pages-hash))
  (yaoddmuse-update-pagename)
  (save-excursion
    (find-file helm-c-yaoddmuse-cache-file)
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
      (load helm-c-yaoddmuse-cache-file))))

(defun helm-yaoddmuse-init ()
  "Init helm buffer status."
  (let ((helm-buffer (helm-candidate-buffer 'global))
        (library-list (yaoddmuse-get-library-list)))
    (with-current-buffer helm-buffer
      ;; Insert library name.
      (dolist (library library-list)
        (insert (format "%s\n" library)))
      ;; Sort lines.
      (sort-lines nil (point-min) (point-max)))))


;;; Eev anchors
(defvar helm-c-source-eev-anchor
  '((name . "Anchors")
    (candidates
     . (lambda ()
         (ignore-errors
           (with-helm-current-buffer
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
                           (helm-match-line-color-current-line)))
    (persistent-help . "Show this entry")
    (action . (("Goto link" . ee-to)))))


;;; Org headlines
;;
;;
(defvar helm-c-source-org-headline
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
                           (helm-c-action-line-goto elm)
                           (org-cycle)))
    (action-transformer
     . (lambda (actions candidate)
         '(("Go to Line" . helm-c-action-line-goto)
           ("Refile to this Headline" . helm-c-org-headline-refile)
           ("Insert Link to This Headline"
            . helm-c-org-headline-insert-link-to-headline)))))
  "Show Org headlines.
org-mode is very very much extended text-mode/outline-mode.

See (find-library \"org.el\")
See http://orgmode.org for the latest version.")

(defun helm-c-org-headline-insert-link-to-headline (lineno-and-content)
  (insert
   (save-excursion
     (helm-goto-line (car lineno-and-content))
     (and (looking-at org-complex-heading-regexp)
          (org-make-link-string (concat "*" (match-string 4)))))))

(defun helm-c-org-headline-refile (lineno-and-content)
  "Refile current org entry to LINENO-AND-CONTENT."
  (with-helm-current-buffer
    (org-cut-subtree)
    (helm-goto-line (car lineno-and-content))
    (org-end-of-subtree t t)
    (let ((org-yank-adjusted-subtrees t))
      (org-yank))))


;;; Org keywords
;;
;;
(defvar helm-c-source-org-keywords
  '((name . "Org Keywords")
    (init . helm-c-org-keywords-init)
    (candidates . helm-c-org-keywords-candidates)
    (action . helm-c-org-keywords-insert)
    (persistent-action . helm-c-org-keywords-show-help)
    (persistent-help . "Show an example and info page to describe this keyword.")
    (keywords-examples)
    (keywords)))

(defvar helm-c-org-keywords-info-location
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

(defun helm-c-org-keywords-init ()
  (unless (helm-attr 'keywords-examples)
    (require 'org)
    (helm-attrset 'keywords-examples
                      (append
                       (mapcar
                        (lambda (x)
                          (string-match "^#\\+\\(\\([A-Z_]+:?\\).*\\)" x)
                          (cons (match-string 2 x) (match-string 1 x)))
                        (org-split-string (org-get-current-options) "\n"))
                       (mapcar 'list org-additional-option-like-keywords)))
    (helm-attrset 'keywords (mapcar 'car (helm-attr 'keywords-examples)))))

(defun helm-c-org-keywords-candidates ()
  (and (or (eq (buffer-local-value 'major-mode helm-current-buffer) 'org-mode)
           (eq (buffer-local-value 'major-mode helm-current-buffer) 'message-mode))
       (helm-attr 'keywords)))

(defun helm-c-org-keywords-insert (keyword)
  (cond ((and (string-match "BEGIN" keyword)
              (helm-region-active-p))
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

(defun helm-c-org-keywords-show-help (keyword)
  (info (or (assoc-default (concat "#+" keyword) helm-c-org-keywords-info-location)
            "(org)In-buffer settings"))
  (search-forward (concat "#+" keyword) nil t)
  (helm-persistent-highlight-point)
  (message "%s" (or (cdr (assoc keyword (helm-attr 'keywords-examples))) "")))



;;; bbdb
;;
;;
(defvar bbdb-records)
(defvar bbdb-buffer-name)

(defun helm-c-bbdb-candidates ()
  "Return a list of all names in the bbdb database.  The format
is \"Firstname Lastname\"."
  (mapcar (lambda (bbdb-record)
            (replace-regexp-in-string
             "\\s-+$" ""
             (concat (aref bbdb-record 0) " " (aref bbdb-record 1))))
          (bbdb-records)))

(defun helm-c-bbdb-create-contact (actions candidate)
  "Action transformer that returns only an entry to add the
current `helm-pattern' as new contact.  All other actions are
removed."
  (if (string= candidate "*Add to contacts*")
      '(("Add to contacts" . (lambda (actions)
                               (bbdb-create-internal
                                (read-from-minibuffer "Name: " helm-c-bbdb-name)
                                (read-from-minibuffer "Company: ")
                                (read-from-minibuffer "Email: ")
                                nil
                                nil
                                (read-from-minibuffer "Note: ")))))
      actions))

(defun helm-c-bbdb-get-record (candidate)
  "Return record that match CANDIDATE."
  (bbdb candidate nil)
  (set-buffer "*BBDB*")
  (bbdb-current-record))

(defvar helm-c-bbdb-name nil
  "Only for internal use.")

(defvar helm-c-source-bbdb
  '((name . "BBDB")
    (candidates . helm-c-bbdb-candidates)
    (action ("Send a mail" . helm-c-bbdb-compose-mail)
     ("View person's data" . helm-c-bbdb-view-person-action))
    (filtered-candidate-transformer . (lambda (candidates source)
                                        (setq helm-c-bbdb-name helm-pattern)
                                        (if (not candidates)
                                            (list "*Add to contacts*")
                                            candidates)))
    (action-transformer . (lambda (actions candidate)
                            (helm-c-bbdb-create-contact actions candidate))))
  "Needs BBDB.

http://bbdb.sourceforge.net/")

(defun helm-c-bbdb-view-person-action (candidate)
  "View BBDB data of single CANDIDATE or marked candidates."
  (helm-aif (helm-marked-candidates)
      (let ((bbdb-append-records (length it)))
        (dolist (i it)
          (bbdb-redisplay-one-record (helm-c-bbdb-get-record i))))
    (bbdb-redisplay-one-record (helm-c-bbdb-get-record candidate))))

(defun helm-c-bbdb-collect-mail-addresses ()
  "Return a list of all mail addresses of records in bbdb buffer."
  (with-current-buffer bbdb-buffer-name
    (loop for i in bbdb-records
          if (bbdb-record-net (car i))
          collect (bbdb-dwim-net-address (car i)))))

(defun helm-c-bbdb-compose-mail (candidate)
  "Compose a mail with all records of bbdb buffer."
  (helm-c-bbdb-view-person-action candidate)
  (let* ((address-list (helm-c-bbdb-collect-mail-addresses))
         (address-str  (mapconcat 'identity address-list ",\n    ")))
    (compose-mail address-str)))


;;; Evaluation Result
;;
;;
;; Internal
(defvar helm-eldoc-active-minibuffers-list nil)
(defvar helm-eval-expression-input-history nil)

(defvar helm-c-source-evaluation-result
  '((name . "Evaluation Result")
    (disable-shortcuts)
    (dummy)
    (multiline)
    (mode-line . "C-RET: nl-and-indent, tab: reindent, C-tab:complete, C-p/n: next/prec-line.")
    (filtered-candidate-transformer . (lambda (candidates source)
                                        (list
                                         (condition-case nil
                                             (with-helm-current-buffer
                                               (pp-to-string
                                                (eval (read helm-pattern))))
                                           (error "Error")))))
    (action . (("Copy result to kill-ring" . (lambda (candidate)
                                               (with-current-buffer helm-buffer
                                                 (let ((end (save-excursion
                                                              (goto-char (point-max))
                                                              (search-backward "\n")
                                                              (point))))
                                                   (kill-region (point) end)))))
               ("copy sexp to kill-ring" . (lambda (candidate)
                                             (kill-new helm-input)))))))

(defun helm-eval-new-line-and-indent ()
  (interactive)
  (newline) (lisp-indent-line))

(defun helm-eldoc-store-minibuffer ()
  "Store minibuffer buffer name in `helm-eldoc-active-minibuffers-list'."
  (with-selected-window (minibuffer-window)
    (push (buffer-name) helm-eldoc-active-minibuffers-list)))

(defun helm-eldoc-show-in-eval ()
  "Return eldoc in mode-line for current minibuffer input."
  (let ((buf (with-selected-window (minibuffer-window)
               (buffer-name))))
    (when (member buf helm-eldoc-active-minibuffers-list)  
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
        (when doc (funcall helm-c-eldoc-in-minibuffer-show-fn doc))))))

(defun helm-c-show-info-in-mode-line (str)
  "Display string STR in mode-line."
  (save-selected-window
    (with-current-buffer helm-buffer
      (let ((mode-line-format (concat " " str)))
        (force-mode-line-update)
        (sit-for helm-c-show-info-in-mode-line-delay))
      (force-mode-line-update))))

;;; Calculation Result
;;
;;
(defvar helm-c-source-calculation-result
  '((name . "Calculation Result")
    (dummy)
    (filtered-candidate-transformer . (lambda (candidates source)
                                        (list
                                         (condition-case nil
                                             (calc-eval helm-pattern)
                                           (error "error")))))
    (action ("Copy result to kill-ring" . kill-new))))


;;; Google Suggestions
;;
;;
;; Internal
(defvar helm-ggs-max-length-real-flag 0)
(defvar helm-ggs-max-length-num-flag 0)

(defun helm-c-google-suggest-fetch (input)
  "Fetch suggestions for INPUT from XML buffer.
Return an alist with elements like (data . number_results)."
  (setq helm-ggs-max-length-real-flag 0
        helm-ggs-max-length-num-flag 0)
  (let ((request (concat helm-c-google-suggest-url
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
                   for lqueries = (length (helm-c-ggs-set-number-result
                                           nqueries))
                   for ldata = (length data)
                   do
                   (progn
                     (when (> ldata helm-ggs-max-length-real-flag)
                       (setq helm-ggs-max-length-real-flag ldata))
                     (when (> lqueries helm-ggs-max-length-num-flag)
                       (setq helm-ggs-max-length-num-flag lqueries)))
                   collect (cons data nqueries) into cont
                   finally return cont)))
      (if helm-google-suggest-use-curl-p
          (with-temp-buffer
            (call-process "curl" nil t nil request)
            (fetch))
          (with-current-buffer
              (url-retrieve-synchronously request)
            (fetch))))))

(defun helm-c-google-suggest-set-candidates (&optional request-prefix)
  "Set candidates with result and number of google results found."
  (let ((suggestions
         (loop with suggested-results = (helm-c-google-suggest-fetch
                                         (or (and request-prefix
                                                  (concat request-prefix
                                                          " " helm-pattern))
                                             helm-pattern))
               for (real . numresult) in suggested-results
               ;; Prepare number of results with ","
               for fnumresult = (helm-c-ggs-set-number-result numresult)
               ;; Calculate number of spaces to add before fnumresult
               ;; if it is smaller than longest result
               ;; `helm-ggs-max-length-num-flag'.
               ;; e.g 1,234,567
               ;;       345,678
               ;; To be sure it is aligned properly.
               for nspaces = (if (< (length fnumresult)
                                    helm-ggs-max-length-num-flag)
                                 (- helm-ggs-max-length-num-flag
                                    (length fnumresult))
                                 0)
               ;; Add now the spaces before fnumresult.
               for align-fnumresult = (concat (make-string nspaces ? )
                                              fnumresult)
               for interval = (- helm-ggs-max-length-real-flag
                                 (length real))
               for spaces   = (make-string (+ 2 interval) ? )
               for display = (format "%s%s(%s results)"
                                     real spaces align-fnumresult)
               collect (cons display real))))
    (if (loop for (disp . dat) in suggestions
              thereis (equal dat helm-pattern))
        suggestions
        ;; if there is no suggestion exactly matching the input then
        ;; prepend a Search on Google item to the list
        (append
         suggestions
         (list (cons (concat "Search for " "'" helm-input "'" " on Google")
                     helm-input))))))

(defun helm-c-ggs-set-number-result (num)
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

(defvar helm-c-google-suggest-default-browser-function nil
  "*The browse url function you prefer to use with google suggest.
When nil, use the first browser function available
See `helm-browse-url-default-browser-alist'.")

(defun helm-c-google-suggest-action (candidate)
  "Default action to jump to a google suggested candidate."
  (let ((arg (concat helm-c-google-suggest-search-url
                     (url-hexify-string candidate))))
    (helm-aif helm-c-google-suggest-default-browser-function
        (funcall it arg)
      (helm-c-browse-url arg))))

(defvar helm-c-google-suggest-default-function
  'helm-c-google-suggest-set-candidates
  "Default function to use in helm google suggest.")

(defvar helm-c-source-google-suggest
  '((name . "Google Suggest")
    (candidates . (lambda ()
                    (funcall helm-c-google-suggest-default-function)))
    (action . (("Google Search" . helm-c-google-suggest-action)))
    (volatile)
    (requires-pattern . 3)
    (delayed)))

(defun helm-c-google-suggest-emacs-lisp ()
  "Try to emacs lisp complete with google suggestions."
  (helm-c-google-suggest-set-candidates "emacs lisp"))


;;; Yahoo suggestions
;;
;;
(defun helm-c-yahoo-suggest-fetch (input)
  "Fetch Yahoo suggestions for INPUT from XML buffer.
Return an alist with elements like (data . number_results)."
  (let ((request (concat helm-c-yahoo-suggest-url
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

(defun helm-c-yahoo-suggest-set-candidates ()
  "Set candidates with Yahoo results found."
  (let ((suggestions (helm-c-yahoo-suggest-fetch helm-input)))
    (or suggestions
        (append
         suggestions
         (list (cons (concat "Search for " "'" helm-input "'" " on Yahoo")
                     helm-input))))))

(defun helm-c-yahoo-suggest-action (candidate)
  "Default action to jump to a Yahoo suggested candidate."
  (helm-c-browse-url (concat helm-c-yahoo-suggest-search-url
                                 (url-hexify-string candidate))))

(defvar helm-c-source-yahoo-suggest
  '((name . "Yahoo Suggest")
    (candidates . helm-c-yahoo-suggest-set-candidates)
    (action . (("Yahoo Search" . helm-c-yahoo-suggest-action)))
    (volatile)
    (requires-pattern . 3)
    (delayed)))



;;; Web browser functions.
;;
;;
(require 'browse-url)
;; If default setting of `w3m-command' is not
;; what you want you and you modify it, you will have to reeval
;; also `helm-browse-url-default-browser-alist'.
(defvar w3m-command "/usr/bin/w3m")
(defvar helm-c-home-url "http://www.google.fr"
  "*Default url to use as home url.")

(defvar ac-browse-url-chromium-program "chromium-browser")
(defvar ac-browse-url-uzbl-program "uzbl-browser")
(defvar helm-browse-url-default-browser-alist
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

(defun* helm-c-generic-browser (url name &rest args)
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
  (helm-c-generic-browser
   url ac-browse-url-chromium-program))

(defun ac-browse-url-uzbl (url &optional ignore)
  "Browse URL with uzbl browser."
  (interactive "sURL: ")
  (helm-c-generic-browser url ac-browse-url-uzbl-program "-u"))

(defun helm-browse-url-default-browser (url &rest args)
  "Find the first available browser and ask it to load URL."
  (let ((default-browser-fn
         (loop for (exe . fn) in helm-browse-url-default-browser-alist
               thereis (and exe (executable-find exe) fn))))
    (if default-browser-fn
        (apply default-browser-fn url args)
        (error "No usable browser found"))))

(defun helm-c-browse-url (url &rest args)
  "Default command to browse URL."
  (if browse-url-browser-function
      (browse-url url args)
      (helm-browse-url-default-browser url args)))


;;; Surfraw
;;
;; Need external program surfraw.
;; <http://surfraw.alioth.debian.org/>

(defvar helm-surfraw-default-browser-function nil
  "*The browse url function you prefer to use with surfraw.
When nil, fallback to `browse-url-browser-function'.")

;; Internal
(defvar helm-surfraw-engines-history nil)
(defvar helm-surfraw-input-history nil)

(defun helm-c-build-elvi-list ()
  "Return list of all engines and descriptions handled by surfraw."
  (cdr
   (with-temp-buffer
     (call-process "surfraw" nil t nil
                   "-elvi")
     (split-string (buffer-string) "\n"))))


;;; Emms
;;
;;
(defun helm-emms-stream-edit-bookmark (elm)
  "Change the information of current emms-stream bookmark from helm."
  (declare (special emms-stream-list))
  (let* ((cur-buf helm-current-buffer)
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
        (helm-c-switch-to-buffer cur-buf)))))

(defun helm-emms-stream-delete-bookmark (candidate)
  "Delete emms-streams bookmarks from helm."
  (let* ((cands   (helm-marked-candidates))
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

(defvar helm-c-source-emms-streams
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
               ("Delete" . helm-emms-stream-delete-bookmark)
               ("Edit" . helm-emms-stream-edit-bookmark)))
    (filtered-candidate-transformer . helm-c-adaptive-sort)))

;; Don't forget to set `emms-source-file-default-directory'
(defvar helm-c-source-emms-dired
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
                                            (helm-c-open-dired
                                             (expand-file-name
                                              item
                                              emms-source-file-default-directory))))))
    (filtered-candidate-transformer . helm-c-adaptive-sort)))


(defun helm-c-emms-files-modifier (candidates source)
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
                                    'face 'helm-emms-playlist)
                        (cdr i)) into lis
          else collect i into lis
          finally return (reverse lis))))

(defun helm-c-emms-play-current-playlist ()
  "Play current playlist."
  (with-current-emms-playlist
      (emms-playlist-first)
    (emms-playlist-mode-play-smart)))

(defvar helm-c-source-emms-files
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
    (filtered-candidate-transformer . helm-c-emms-files-modifier)
    (candidate-number-limit . 9999)
    (action . (("Play file" . emms-play-file)
               ("Add to Playlist and play (C-u clear current)"
                . (lambda (candidate)
                    (when helm-current-prefix-arg
                      (emms-playlist-current-clear))
                    (emms-playlist-new)
                    (mapc 'emms-add-playlist-file (helm-marked-candidates))
                    (unless emms-player-playing-p
                      (helm-c-emms-play-current-playlist))))))))



;;; Jabber Contacts (jabber.el)
(defun helm-c-jabber-online-contacts ()
  "List online Jabber contacts."
  (with-no-warnings
    (let (jids)
      (dolist (item (jabber-concat-rosters) jids)
        (when (get item 'connected)
          (push (if (get item 'name)
                    (cons (get item 'name) item)
                    (cons (symbol-name item) item)) jids))))))

(defvar helm-c-source-jabber-contacts
  '((name . "Jabber Contacts")
    (init . (lambda () (require 'jabber)))
    (candidates . (lambda () (mapcar 'car (helm-c-jabber-online-contacts))))
    (action . (lambda (x)
                (jabber-chat-with
                 (jabber-read-account)
                 (symbol-name
                  (cdr (assoc x (helm-c-jabber-online-contacts)))))))))



;;; Call source.
(defvar helm-source-select-buffer "*helm source select*")
(defvar helm-c-source-call-source
  `((name . "Call helm source")
    (candidate-number-limit)
    (candidates
     . (lambda ()
         (loop for vname in (all-completions "helm-c-source-" obarray)
               for var = (intern vname)
               for name = (ignore-errors (assoc-default 'name (symbol-value var)))
               if name collect
               (cons (format "%s `%s'"
                             name (propertize vname 'face 'font-lock-variable-name-face))
                     var))))
    (action
     . (("Invoke helm with selected source"
         .
         (lambda (candidate)
           (setq helm-candidate-number-limit 9999)
           (helm candidate nil nil nil nil
                     helm-source-select-buffer)))
        ("Describe variable" . describe-variable)
        ("Find variable" . find-variable)))
    (persistent-action . describe-variable)
    (persistent-help . "Show description of this source")))

(defun helm-call-source-from-helm ()
  "Call helm source within `helm' session."
  (interactive)
  (setq helm-input-idle-delay 0)
  (helm-set-sources '(helm-c-source-call-source)))

;;; Execute Preconfigured helm.
(defvar helm-c-source-helm-commands
  '((name . "Preconfigured Helm")
    (candidates . helm-c-helm-commands-candidates)
    (type . command)
    (candidate-number-limit)))

(defun helm-c-helm-commands-candidates ()
  (loop for (cmd . desc) in (helm-c-list-preconfigured-helm)
        collect (cons (if (where-is-internal cmd nil t)
                          (substitute-command-keys (format "M-x %s (\\[%s]) : %s" cmd cmd desc))
                          (substitute-command-keys (format "\\[%s] : %s" cmd desc)))
                      cmd)))


;;; Occur
;;
;;
(defun helm-c-occur-init ()
  "Create the initial helm occur buffer.
If region is active use region as buffer contents
instead of whole buffer."
  (with-current-buffer (helm-candidate-buffer 'global)
    (erase-buffer)
    (let ((buf-contents
           (with-helm-current-buffer
             (if (helm-region-active-p)
                 (buffer-substring (region-beginning) (region-end))
                 (buffer-substring (point-min) (point-max))))))
      (insert buf-contents))))

(defun helm-c-occur-get-line (s e)
  (format "%7d:%s" (line-number-at-pos (1- s)) (buffer-substring s e)))

(defun helm-c-occur-query-replace-regexp (candidate)
  "Query replace regexp starting from CANDIDATE.
If region is active ignore CANDIDATE and replace only in region.
With a prefix arg replace only matches surrounded by word boundaries,
i.e Don't replace inside a word, regexp is surrounded with \\bregexp\\b."
  (let ((regexp helm-input))
    (unless (helm-region-active-p)
      (helm-c-action-line-goto candidate))
    (apply 'query-replace-regexp
           (helm-c-query-replace-args regexp))))

(defun helm-occur-run-query-replace-regexp ()
  "Run `query-replace-regexp' in helm occur from keymap."
  (interactive)
  (helm-c-quit-and-execute-action
   'helm-c-occur-query-replace-regexp))

(defvar helm-c-source-occur
  `((name . "Occur")
    (init . helm-c-occur-init)
    (candidates-in-buffer)
    (migemo)
    (get-line . helm-c-occur-get-line)
    (display-to-real . helm-c-display-to-real-line)
    (action . (("Go to Line" . helm-c-action-line-goto)
               ("Query replace regexp (C-u Not inside word.)"
                . helm-c-occur-query-replace-regexp)))
    (recenter)
    (mode-line . helm-occur-mode-line)
    (keymap . ,helm-occur-map)
    (requires-pattern . 1)
    (delayed)))


;;; Helm browse code.
(defun helm-c-browse-code-get-line (beg end)
  "Select line if it match the regexp corresponding to current `major-mode'.
Line is parsed for BEG position to END position."
  (let ((str-line (buffer-substring beg end))
        (regexp   (assoc-default major-mode
                                 helm-c-browse-code-regexp-alist))
        (num-line (if (string= helm-pattern "") beg (1- beg))))
    (when (and regexp (string-match regexp str-line))
      (format "%4d:%s" (line-number-at-pos num-line) str-line))))


(defvar helm-c-source-browse-code
  '((name . "Browse code")
    (init . (lambda ()
              (helm-candidate-buffer helm-current-buffer)
              (with-helm-current-buffer
                (jit-lock-fontify-now))))
    (candidate-number-limit . 9999)
    (candidates-in-buffer)
    (get-line . helm-c-browse-code-get-line)
    (type . line)
    (recenter)))


;; Do many actions for input
(defvar helm-c-source-create
  '((name . "Create")
    (dummy)
    (action)
    (action-transformer . helm-create--actions))
  "Do many create actions from `helm-pattern'.
See also `helm-create--actions'.")

(defun helm-create-from-helm ()
  "Run `helm-create' from `helm' as a fallback."
  (interactive)
  (helm-run-after-quit 'helm-create nil helm-pattern))

(defun helm-create--actions (&rest ignored)
  "Default actions for `helm-create' / `helm-c-source-create'."
  (remove-if-not
   (lambda (pair) (and (consp pair) (functionp (cdr pair))))
   (append helm-create--actions-private
           '(("find-file" . find-file)
             ("find-file other window" . find-file-other-window)
             ("New buffer" . helm-c-switch-to-buffer)
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
(defvar helm-c-source-minibuffer-history
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
(defvar helm-c-source-elscreen
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
                            (dolist (i (helm-marked-candidates))
                              (elscreen-goto (- (aref i 1) (aref "0" 0)))
                              (elscreen-kill))))
        ("Only Screen" .
                       (lambda (candidate)
                         (elscreen-goto (- (aref candidate 1) (aref "0" 0)))
                         (elscreen-kill-others)))))))


;;;; <System>

;;; Top (process)
(defvar helm-c-top-command "COLUMNS=%s top -b -n 1"
  "Top command (batch mode). %s is replaced with `frame-width'.")
(defvar helm-c-source-top
  '((name . "Top (Press C-c C-u to refresh)")
    (init . helm-c-top-init)
    (candidates-in-buffer)
    (display-to-real . helm-c-top-display-to-real)
    (persistent-action . helm-c-top-sh-persistent-action)
    (persistent-help . "SIGTERM")
    (action
     ("kill (TERM)" . (lambda (pid)
                        (helm-c-top-sh (format "kill -TERM %s" pid))))
     ("kill (KILL)" . (lambda (pid)
                        (helm-c-top-sh (format "kill -KILL %s" pid))))
     ("Copy PID" . (lambda (pid) (kill-new pid))))))

(defun helm-c-top-sh (cmd)
  (message "Executed %s\n%s" cmd (shell-command-to-string cmd)))

(defun helm-c-top-sh-persistent-action (pid)
  (delete-other-windows)
  (helm-c-top-sh (format "kill -TERM %s" pid))
  (helm-force-update))

(defun helm-c-top-init ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (call-process-shell-command
     (format helm-c-top-command
             (- (frame-width) (if helm-enable-digit-shortcuts 4 0)))
     nil (current-buffer))))

(defun helm-c-top-display-to-real (line)
  (car (split-string line)))

;;; Timers
(defvar helm-c-source-absolute-time-timers
  '((name . "Absolute Time Timers")
    (candidates . timer-list)
    (type . timer)))

(defvar helm-c-source-idle-time-timers
  '((name . "Idle Time Timers")
    (candidates . timer-idle-list)
    (type . timer)))

(defun helm-c-timer-real-to-display (timer)
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

(defun helm-c-xrandr-info ()
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

(defun helm-c-xrandr-screen ()
  "Return current X screen number."
  (car (helm-c-xrandr-info)))

(defun helm-c-xrandr-output ()
  "Return current X display name."
  (cadr (helm-c-xrandr-info)))

(defvar helm-c-source-xrandr-change-resolution
  '((name . "Change Resolution")
    (candidates
     . (lambda ()
         (with-temp-buffer
           (call-process "xrandr" nil (current-buffer) nil
                         "--screen" (helm-c-xrandr-screen) "-q")
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
                        "--screen" (helm-c-xrandr-screen)
                        "--output" (helm-c-xrandr-output)
                        "--mode" mode))))))

;;; Xfont selection
;;
;;
(defun helm-c-persistent-xfont-action (elm)
  "Show current font temporarily"
  (let ((current-font (cdr (assoc 'font (frame-parameters))))
        (default-font elm))
    (unwind-protect
         (progn (set-frame-font default-font 'keep-size) (sit-for 2))
      (set-frame-font current-font))))

(defvar helm-c-xfonts-cache nil)
(defvar helm-c-source-xfonts
  '((name . "X Fonts")
    (init . (lambda ()
              (unless helm-c-xfonts-cache
                (setq helm-c-xfonts-cache
                      (x-list-fonts "*")))))
    (candidates . helm-c-xfonts-cache)
    (action . (("Copy to kill ring" . (lambda (elm)
                                        (kill-new elm)))
               ("Set Font" . (lambda (elm)
                               (kill-new elm)
                               (set-frame-font elm 'keep-size)
                               (message "New font have been copied to kill ring")))))
    (persistent-action . helm-c-persistent-xfont-action)
    (persistent-help . "Switch to this font temporarily")))

;;; 𝕌𝕔𝕤 𝕊𝕪𝕞𝕓𝕠𝕝 𝕔𝕠𝕞𝕡𝕝𝕖𝕥𝕚𝕠𝕟
;;
;;
(defvar helm-c-ucs-max-len 0)
(defun helm-c-calculate-ucs-max-len ()
  "Calculate the length of longest `ucs-names' candidate."
  (loop with count = 0
        for (n . v) in (ucs-names)
        for len = (length n)
        if (> len count)
        do (setq count len)
        finally return count))

(defun helm-c-ucs-init ()
  "Initialize an helm buffer with ucs symbols.
Only math* symbols are collected."
  (unless (> helm-c-ucs-max-len 0)
    (setq helm-c-ucs-max-len
          (helm-c-calculate-ucs-max-len)))
  (with-current-buffer (helm-candidate-buffer
                        (get-buffer-create "*helm ucs*"))
    ;; `ucs-names' fn will not run again, data is cached in
    ;; var `ucs-names'.
    (loop for (n . v) in (ucs-names)
          for len = (length n)
          for diff = (+ (- helm-c-ucs-max-len len) 2)
          unless (string= "" n)
          do (progn (insert (concat
                             n ":"
                             (make-string
                              diff ? )))
                    (ucs-insert v)
                    (insert "\n")))))

(defun helm-c-ucs-forward-char (candidate)
  (with-helm-current-buffer
    (forward-char 1)))

(defun helm-c-ucs-backward-char (candidate)
  (with-helm-current-buffer
    (forward-char -1)))

(defun helm-c-ucs-delete-backward (candidate)
  (with-helm-current-buffer
    (delete-char -1)))

(defun helm-c-ucs-insert-char (candidate)
  (with-helm-current-buffer
    (insert
     (replace-regexp-in-string
      " " ""
      (cadr (split-string candidate ":"))))))

(defun helm-c-ucs-persistent-insert ()
  (interactive)
  (helm-attrset 'action-insert 'helm-c-ucs-insert-char)
  (helm-execute-persistent-action 'action-insert))

(defun helm-c-ucs-persistent-forward ()
  (interactive)
  (helm-attrset 'action-forward 'helm-c-ucs-forward-char)
  (helm-execute-persistent-action 'action-forward))

(defun helm-c-ucs-persistent-backward ()
  (interactive)
  (helm-attrset 'action-back 'helm-c-ucs-backward-char)
  (helm-execute-persistent-action 'action-back))

(defun helm-c-ucs-persistent-delete ()
  (interactive)
  (helm-attrset 'action-delete 'helm-c-ucs-delete-backward)
  (helm-execute-persistent-action 'action-delete))

(defvar helm-c-source-ucs
  '((name . "Ucs names")
    (init . helm-c-ucs-init)
    (candidate-number-limit . 9999)
    (candidates-in-buffer)
    (mode-line . helm-c-ucs-mode-line-string)
    (action . (("Insert" . helm-c-ucs-insert-char)
               ("Forward char" . helm-c-ucs-forward-char)
               ("Backward char" . helm-c-ucs-backward-char)
               ("Delete char backward" . helm-c-ucs-delete-backward))))
  "Source for collecting `ucs-names' math symbols.")


;;; Emacs process
;;
;;
(defvar helm-c-source-emacs-process
  '((name . "Emacs Process")
    (candidates . (lambda () (mapcar #'process-name (process-list))))
    (persistent-action . (lambda (elm)
                           (delete-process (get-process elm))
                           (helm-delete-current-selection)))
    (persistent-help . "Kill Process")
    (action ("Kill Process" . (lambda (elm)
                                (delete-process (get-process elm)))))))

;;; World time
;;
;;
(defvar helm-c-source-time-world
  '((name . "Time World List")
    (init . (lambda ()
              (let ((helm-buffer (helm-candidate-buffer 'global)))
                (with-current-buffer helm-buffer
                  (display-time-world-display display-time-world-list)))))
    (candidates-in-buffer)))



;;; Helm interface for Debian/Ubuntu packages (apt-*)
;;
;;
(defvar helm-c-source-apt
  '((name . "APT")
    (init . helm-c-apt-init)
    (candidates-in-buffer)
    (candidate-transformer helm-c-apt-candidate-transformer)
    (display-to-real . helm-c-apt-display-to-real)
    (requires-pattern . 2)
    (update . helm-c-apt-refresh)
    (action
     ("Show package description" . helm-c-apt-cache-show)
     ("Install package" . helm-c-apt-install)
     ("Reinstall package" . helm-c-apt-reinstall)
     ("Remove package" . helm-c-apt-uninstall)
     ("Purge package" . helm-c-apt-purge))
    (persistent-action . helm-c-apt-persistent-action)
    (persistent-help . "Show package description")))

(defvar helm-c-apt-query "emacs")
(defvar helm-c-apt-search-command "apt-cache search '%s'")
(defvar helm-c-apt-show-command "apt-cache show '%s'")
(defvar helm-c-apt-installed-packages nil)
(defvar helm-c-apt-all-packages nil)
(defvar helm-c-apt-input-history nil)

(defun helm-c-apt-refresh ()
  "Refresh installed candidates list."
  (setq helm-c-apt-installed-packages nil)
  (setq helm-c-apt-all-packages nil))

(defun helm-c-apt-persistent-action (candidate)
  "Persistent action for APT source."
  (helm-c-apt-cache-show candidate))

(defun helm-c-apt-candidate-transformer (candidates)
  "Show installed CANDIDATES and the ones to deinstall in a different color."
  (loop for cand in candidates
        for name = (helm-c-apt-display-to-real cand)
        collect (cond ((string= (assoc-default
                                 name helm-c-apt-installed-packages)
                                "deinstall")
                       (propertize cand 'face 'helm-apt-deinstalled))
                      ((string= (assoc-default
                                 name helm-c-apt-installed-packages)
                                "install")
                       (propertize cand 'face 'helm-apt-installed))
                      (t cand))))

(defun helm-c-apt-init ()
  "Initialize list of debian packages."
  (let ((query ""))
    (unless (and helm-c-apt-installed-packages
                 helm-c-apt-all-packages)
      (message "Loading package list...")
      (setq helm-c-apt-installed-packages
            (with-temp-buffer
              (call-process-shell-command "dpkg --get-selections"
                                          nil (current-buffer))
              (loop for i in (split-string (buffer-string) "\n" t)
                    for p = (split-string i)
                    collect (cons (car p) (cadr p)))))
      (setq helm-c-apt-all-packages
            (with-current-buffer
                (helm-candidate-buffer
                 (get-buffer-create (format "*helm-apt*")))
              (erase-buffer)
              (call-process-shell-command
               (format helm-c-apt-search-command query)
               nil (current-buffer))))
      (message "Loading package list done")
      (sit-for 0.5))))

(defun helm-c-apt-display-to-real (line)
  "Return only name of a debian package.
LINE is displayed like:
package name - description."
  (car (split-string line " - ")))

(defun helm-c-shell-command-if-needed (command)
  "Run shell command COMMAND to describe package.
If a buffer named COMMAND already exists, just switch to it."
  (let ((buf (get-buffer command)))
    (helm-c-switch-to-buffer (get-buffer-create command))
    (unless buf (insert (shell-command-to-string command)))))

(defun helm-c-apt-cache-show (package)
  "Show information on apt package PACKAGE."
  (helm-c-shell-command-if-needed
   (format helm-c-apt-show-command package)))

(defun helm-c-apt-install (package)
  "Run 'apt-get install' shell command on PACKAGE."
  (helm-c-apt-generic-action :action 'install))

(defun helm-c-apt-reinstall (package)
  "Run 'apt-get install --reinstall' shell command on PACKAGE."
  (helm-c-apt-generic-action :action 'reinstall))

(defun helm-c-apt-uninstall (package)
  "Run 'apt-get remove' shell command on PACKAGE."
  (helm-c-apt-generic-action :action 'uninstall))

(defun helm-c-apt-purge (package)
  "Run 'apt-get purge' shell command on PACKAGE."
  (helm-c-apt-generic-action :action 'purge))

(defun* helm-c-apt-generic-action (&key action)
  "Run 'apt-get ACTION'.
Support install, remove and purge actions."
  (ansi-term (getenv "SHELL") "helm apt")
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
                              (helm-marked-candidates) " ")))
    (goto-char (point-max))
    (insert (concat command cand-list))
    (setq end (point))
    (if (y-or-n-p (format "%s package" (symbol-name action)))
        (progn
          (setq helm-c-external-commands-list nil)
          (setq helm-c-apt-installed-packages nil)
          (term-char-mode) (term-send-input))
        (delete-region beg end) (term-send-eof) (kill-buffer))))

;; (helm-c-apt-install "jed")


;;; Helm UI for gentoo portage.
;;
;;
(defvar helm-c-gentoo-use-flags nil)
(defvar helm-c-gentoo-buffer "*helm-gentoo-output*")
(defvar helm-c-cache-gentoo nil)
(defvar helm-c-cache-world nil)
(defvar helm-c-source-gentoo
  '((name . "Portage sources")
    (init . (lambda ()
              (get-buffer-create helm-c-gentoo-buffer)
              (unless helm-c-cache-gentoo
                (helm-c-gentoo-setup-cache))
              (unless helm-c-cache-world
                (setq helm-c-cache-world (helm-c-gentoo-get-world)))
              (helm-c-gentoo-init-list)))
    (candidates-in-buffer)
    (match . identity)
    (candidate-transformer helm-c-highlight-world)
    (action . (("Show package" . (lambda (elm)
                                   (helm-c-gentoo-eshell-action elm "eix")))
               ("Show history" . (lambda (elm)
                                   (if (member elm helm-c-cache-world)
                                       (helm-c-gentoo-eshell-action elm "genlop -qe")
                                       (message "No infos on packages not yet installed"))))
               ("Copy in kill-ring" . kill-new)
               ("insert at point" . insert)
               ("Browse HomePage" . (lambda (elm)
                                      (let ((urls (helm-c-gentoo-get-url elm)))
                                        (browse-url (helm-comp-read "Url: " urls :must-match t)))))
               ("Show extra infos" . (lambda (elm)
                                       (if (member elm helm-c-cache-world)
                                           (helm-c-gentoo-eshell-action elm "genlop -qi")
                                           (message "No infos on packages not yet installed"))))
               ("Show use flags" . (lambda (elm)
                                     (helm-c-gentoo-default-action elm "equery" "-C" "u")
                                     (font-lock-add-keywords nil '(("^\+.*" . font-lock-variable-name-face)))
                                     (font-lock-mode 1)))
               ("Run emerge pretend" . (lambda (elm)
                                         (helm-c-gentoo-eshell-action elm "emerge -p")))
               ("Emerge" . (lambda (elm)
                             (helm-gentoo-install elm :action 'install)))
               ("Unmerge" . (lambda (elm)
                              (helm-gentoo-install elm :action 'uninstall)))
               ("Show dependencies" . (lambda (elm)
                                        (helm-c-gentoo-default-action elm "equery" "-C" "d")))
               ("Show related files" . (lambda (elm)
                                         (helm-c-gentoo-default-action elm "equery" "files")))
               ("Refresh" . (lambda (elm)
                              (helm-c-gentoo-setup-cache)
                              (setq helm-c-cache-world (helm-c-gentoo-get-world))))))))


(defun* helm-gentoo-install (candidate &key action)
  (setq helm-c-external-commands-list nil)
  (ansi-term (getenv "SHELL") "Gentoo emerge")
  (term-line-mode)
  (let ((command (case action
                   ('install "sudo emerge -av ")
                   ('uninstall "sudo emerge -avC ")
                   (t (error "Unknow action"))))
        (elms (mapconcat 'identity (helm-marked-candidates) " "))
        (beg (point)) end)
    (goto-char (point-max))
    (insert (concat command elms))
    (setq end (point))
    (term-char-mode) (term-send-input)))

(defun helm-c-gentoo-default-action (elm command &rest args)
  "Gentoo default action that use `helm-c-gentoo-buffer'."
  (if (member elm helm-c-cache-world)
      (progn
        (helm-c-switch-to-buffer helm-c-gentoo-buffer)
        (erase-buffer)
        (let ((com-list (append args (list elm))))
          (apply #'call-process command nil t nil
                 com-list)))
      (message "No infos on packages not yet installed")))

(defvar helm-c-source-use-flags
  '((name . "Use Flags")
    (init . (lambda ()
              (unless helm-c-gentoo-use-flags
                (helm-c-gentoo-setup-use-flags-cache))
              (helm-c-gentoo-get-use)))
    (candidates-in-buffer)
    (match . identity)
    (candidate-transformer helm-c-highlight-local-use)
    (action . (("Description"
                . (lambda (elm)
                    (helm-c-switch-to-buffer helm-c-gentoo-buffer)
                    (erase-buffer)
                    (apply #'call-process "euse" nil t nil
                           `("-i"
                             ,elm))
                    (font-lock-add-keywords nil `((,elm . font-lock-variable-name-face)))
                    (font-lock-mode 1)))
               ("Enable"
                . (lambda (elm)
                    (helm-c-gentoo-eshell-action elm "*sudo -p Password: euse -E")))
               ("Disable"
                . (lambda (elm)
                    (helm-c-gentoo-eshell-action elm "*sudo -p Password: euse -D")))
               ("Remove"
                . (lambda (elm)
                    (helm-c-gentoo-eshell-action elm "*sudo -p Password: euse -P")))
               ("Show which dep use this flag"
                . (lambda (elm)
                    (helm-c-switch-to-buffer helm-c-gentoo-buffer)
                    (erase-buffer)
                    (apply #'call-process "equery" nil t nil
                           `("-C"
                             "h"
                             ,elm))))))))



(defun helm-c-gentoo-init-list ()
  "Initialize buffer with all packages in Portage."
  (let* ((portage-buf (get-buffer-create "*helm-gentoo*"))
         (buf (helm-candidate-buffer 'portage-buf)))
    (with-current-buffer buf
      (dolist (i helm-c-cache-gentoo)
        (insert (concat i "\n"))))))

(defun helm-c-gentoo-setup-cache ()
  "Set up `helm-c-cache-gentoo'"
  (setq helm-c-cache-gentoo
        (split-string (with-temp-buffer
                        (call-process "eix" nil t nil
                                      "--only-names")
                        (buffer-string)))))

(defun helm-c-gentoo-eshell-action (elm command)
  (when (get-buffer "*EShell Command Output*")
    (kill-buffer "*EShell Command Output*"))
  (message "Wait searching...")
  (let ((buf-fname (buffer-file-name helm-current-buffer)))
    (if (and buf-fname (string-match tramp-file-name-regexp buf-fname))
        (progn
          (save-window-excursion
            (pop-to-buffer "*scratch*")
            (eshell-command (format "%s %s" command elm)))
          (pop-to-buffer "*EShell Command Output*"))
        (eshell-command (format "%s %s" command elm)))))

(defun helm-c-gentoo-get-use ()
  "Initialize buffer with all use flags."
  (let* ((use-buf (get-buffer-create "*helm-gentoo-use*"))
         (buf (helm-candidate-buffer 'use-buf)))
    (with-current-buffer buf
      (dolist (i helm-c-gentoo-use-flags)
        (insert (concat i "\n"))))))


(defun helm-c-gentoo-setup-use-flags-cache ()
  "Setup `helm-c-gentoo-use-flags'"
  (setq helm-c-gentoo-use-flags
        (split-string (with-temp-buffer
                        (call-process "eix" nil t nil
                                      "--print-all-useflags")
                        (buffer-string)))))

(defun helm-c-gentoo-get-url (elm)
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

(defun helm-c-gentoo-get-world ()
  "Return list of all installed package on your system."
  (split-string (with-temp-buffer
                  (call-process "qlist" nil t nil
                                "-I")
                  (buffer-string))))

(defun helm-c-gentoo-get-local-use ()
  (split-string (with-temp-buffer
                  (call-process "portageq" nil t nil
                                "envvar"
                                "USE")
                  (buffer-string))))


(defun helm-c-highlight-world (eix)
  "Highlight all installed package."
  (loop for i in eix
        if (member i helm-c-cache-world)
        collect (propertize i 'face 'helm-gentoo-match-face)
        else
        collect i))

(defun helm-c-highlight-local-use (use-flags)
  (let ((local-uses (helm-c-gentoo-get-local-use)))
    (loop for i in use-flags
          if (member i local-uses)
          collect (propertize i 'face 'helm-gentoo-match-face)
          else
          collect i)))



;;; Helm ratpoison UI
;;
;;
(defvar helm-c-source-ratpoison-commands
  '((name . "Ratpoison Commands")
    (init . helm-c-ratpoison-commands-init)
    (candidates-in-buffer)
    (action ("Execute the command" . helm-c-ratpoison-commands-execute))
    (display-to-real . helm-c-ratpoison-commands-display-to-real)
    (candidate-number-limit)))

(defun helm-c-ratpoison-commands-init ()
  (unless (helm-candidate-buffer)
    (with-current-buffer (helm-candidate-buffer 'global)
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

(defun helm-c-ratpoison-commands-display-to-real (display)
  (and (string-match ": " display)
       (substring display (match-end 0))))

(defun helm-c-ratpoison-commands-execute (candidate)
  (call-process "ratpoison" nil nil nil "-ic" candidate))



;;; Helm `completing-read' replacement
;;
;;
(defun helm-comp-read-get-candidates (collection &optional test sort-fn alistp)
  "Convert COLLECTION to list removing elements that don't match TEST.
See `helm-comp-read' about supported COLLECTION arguments.

SORT-FN is a predicate to sort COLLECTION.

ALISTP when non--nil will not use `all-completions' to collect
candidates because it doesn't handle alists correctly for helm.
i.e In `all-completions' the keys \(cars of elements\)
are the possible completions. In helm we want to use the cdr instead
like \(display . real\).

e.g

\(setq A '((a . 1) (b . 2) (c . 3)))
==>((a . 1) (b . 2) (c . 3))
\(helm-comp-read \"test: \" A :alistp nil
                                  :exec-when-only-one t
                                  :initial-input \"a\")
==>\"a\"
\(helm-comp-read \"test: \" A :alistp t
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

(defun helm-cr-default-transformer (candidates source)
  "Default filter candidate function for `helm-comp-read'.
Do nothing, just return candidate list unmodified."
  candidates)

(defun* helm-comp-read (prompt collection
                                   &key
                                   test
                                   initial-input
                                   default
                                   preselect
                                   (buffer "*Helm Completions*")
                                   must-match
                                   (requires-pattern 0)
                                   (history nil)
                                   input-history
                                   (persistent-action nil)
                                   (persistent-help "DoNothing")
                                   (mode-line helm-mode-line-string)
                                   (keymap helm-map)
                                   (name "Helm Completions")
                                   candidates-in-buffer
                                   exec-when-only-one
                                   (volatile t)
                                   sort
                                   (fc-transformer 'helm-cr-default-transformer)
                                   (marked-candidates nil)
                                   (alistp t))
  "Read a string in the minibuffer, with helm completion.

It is helm `completing-read' equivalent.

- PROMPT is the prompt name to use.

- COLLECTION can be a list, vector, obarray or hash-table.
  It can be also a function that receives three arguments:
  the values string, predicate and t. See `all-completions' for more details.

Keys description:

- TEST: A predicate called with one arg i.e candidate.

- INITIAL-INPUT: Same as input arg in `helm'.

- PRESELECT: See preselect arg of `helm'.

- DEFAULT: This option is used only for compatibility with regular
  Emacs `completing-read'.
 
- BUFFER: Name of helm-buffer.

- MUST-MATCH: Candidate selected must be one of COLLECTION.

- REQUIRES-PATTERN: Same as helm attribute, default is 0.

- HISTORY: A list containing specific history, default is nil.
  When it is non--nil, all elements of HISTORY are displayed in
  a special source before COLLECTION.

- INPUT-HISTORY: A symbol. the minibuffer input history will be
  stored there, if nil or not provided, `minibuffer-history'
  will be used instead.

- PERSISTENT-ACTION: A function called with one arg i.e candidate.

- PERSISTENT-HELP: A string to document PERSISTENT-ACTION.

- MODE-LINE: A string or list to display in mode line.
  (See `helm-mode-line-string')

- KEYMAP: A keymap to use in this `helm-comp-read'.
  (The keymap will be shared with history source)

- NAME: The name related to this local source.

- EXEC-WHEN-ONLY-ONE: Bound `helm-execute-action-at-once-if-one'
  to non--nil. (possibles values are t or nil).

- VOLATILE: Use volatile attribute \(enabled by default\).

- SORT: A predicate to give to `sort' e.g `string-lessp'.

- FC-TRANSFORMER: A `filtered-candidate-transformer' function.

- MARKED-CANDIDATES: If non--nil return candidate or marked candidates as a list.

- ALISTP: \(default is non--nil\) See `helm-comp-read-get-candidates'.

- CANDIDATES-IN-BUFFER: when non--nil use a source build with
  `helm-candidates-in-buffer' which is much faster.
  Argument VOLATILE have no effect when CANDIDATES-IN-BUFFER is non--nil.
 
Any prefix args passed during `helm-comp-read' invocation will be recorded
in `helm-current-prefix-arg', otherwise if prefix args were given before
`helm-comp-read' invocation, the value of `current-prefix-arg' will be used.
That's mean you can pass prefix args before or after calling a command
that use `helm-comp-read' See `helm-M-x' for example."
  (when (get-buffer helm-action-buffer)
    (kill-buffer helm-action-buffer))
  (flet ((action-fn (candidate)
           (if marked-candidates
               (helm-marked-candidates)
               (identity candidate))))
    ;; Assume completion have been already required,
    ;; so always use 'confirm.
    (when (eq must-match 'confirm-after-completion)
      (setq must-match 'confirm))
    (let* ((minibuffer-completion-confirm must-match)
           (must-match-map (when must-match
                             (let ((map (make-sparse-keymap)))
                               (define-key map (kbd "RET")
                                 'helm-confirm-and-exit-minibuffer)
                               map)))
           (helm-map (if must-match-map
                             (make-composed-keymap
                              must-match-map (or keymap helm-map))
                             (or keymap helm-map)))
           (src-hist `((name . ,(format "%s History" name))
                       (candidates
                        . (lambda ()
                            (let ((all (helm-comp-read-get-candidates
                                        history nil nil ,alistp)))
                              (delete
                               ""
                               (helm-fast-remove-dups
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
                       (let ((cands (helm-comp-read-get-candidates
                                     collection test sort alistp)))
                         (unless (or must-match (string= helm-pattern ""))
                           (setq cands (append (list helm-pattern) cands)))
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
                         (let ((cands (helm-comp-read-get-candidates
                                       collection test sort alistp)))
                           (unless (or must-match (string= helm-pattern ""))
                             (setq cands (append (list helm-pattern) cands)))
                           (with-current-buffer (helm-candidate-buffer 'global)
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
           (helm-execute-action-at-once-if-one exec-when-only-one))
      (or
       (helm
        :sources src-list
        :input initial-input
        :default default
        :preselect preselect
        :prompt prompt
        :resume 'noresume
        :keymap helm-map
        :history (and (symbolp input-history) input-history)
        :buffer buffer)
       (when (and (eq helm-exit-status 0)
                  (eq must-match 'confirm))
         ;; Return empty string only if it is the DEFAULT
         ;; value and helm-pattern is empty.
         ;; otherwise return helm-pattern
         (if (and (string= helm-pattern "") default)
             default (identity helm-pattern)))
       (unless (or (eq helm-exit-status 1)
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
;; Provide a mode `helm-mode' which turn on
;; helm in all `completing-read' and `read-file-name' in Emacs.
;;
(defvar helm-completion-mode-string " Helm")

(defvar helm-completion-mode-quit-message
  "Helm completion disabled")

(defvar helm-completion-mode-start-message
  "Helm completion enabled")

;;; Specialized handlers
;;
;;
(defun helm-completing-read-symbols
    (prompt collection test require-match init
     hist default inherit-input-method name buffer)
  "Specialized function for fast symbols completion in `helm-mode'."
  (or
   (helm
    :sources `((name . ,name)
               (init . (lambda ()
                         (with-current-buffer (helm-candidate-buffer 'global)
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
               (persistent-action . helm-lisp-completion-persistent-action)
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
(defun helm-completing-read-default-1
    (prompt collection test require-match
     init hist default inherit-input-method
     name buffer &optional cands-in-buffer exec-when-only-one)
  "Call `helm-comp-read' with same args as `completing-read'.
Extra optional arg CANDS-IN-BUFFER mean use `candidates-in-buffer'
method which is faster.
It should be used when candidate list don't need to rebuild dynamically."
  (let ((history (or (car-safe hist) hist)))
    (helm-comp-read
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

(defun helm-completing-read-with-cands-in-buffer
    (prompt collection test require-match
     init hist default inherit-input-method
     name buffer)
  "Same as `helm-completing-read-default-1' but use candidates-in-buffer."
  ;; Some commands like find-tag may use `read-file-name' from inside
  ;; the calculation of collection. in this case it clash with
  ;; candidates-in-buffer that reuse precedent data (files) which is wrong.
  ;; So (re)calculate collection outside of main helm-session.
  (let ((cands (all-completions "" collection)))
    (helm-completing-read-default-1 prompt cands test require-match
                                        init hist default inherit-input-method
                                        name buffer t)))

(defun* helm-completing-read-default
    (prompt collection &optional
            predicate require-match
            initial-input hist def
            inherit-input-method)
  "An helm replacement of `completing-read'.
This function should be used only as a `completing-read-function'.
 
Don't use it directly, use instead `helm-comp-read' in your programs.

See documentation of `completing-read' and `all-completions' for details."
  (declare (special helm-mode))
  (let* ((current-command this-command)
         (str-command     (symbol-name current-command))
         (buf-name        (format "*helm-mode-%s*" str-command))
         (entry           (assq current-command
                                helm-completing-read-handlers-alist))
         (def-com         (cdr-safe entry))
         (str-defcom      (and def-com (symbol-name def-com)))
         (def-args        (list prompt collection predicate require-match
                                initial-input hist def inherit-input-method))
         ;; Append the two extra args needed to set the buffer and source name
         ;; in helm specialized functions.
         (any-args        (append def-args (list str-command buf-name)))
         helm-completion-mode-start-message ; Be quiet
         helm-completion-mode-quit-message
         (minibuffer-completion-table collection)
         (minibuffer-completion-predicate predicate)
         ;; Be sure this pesty *completion* buffer doesn't popup.
         (minibuffer-setup-hook (remove 'minibuffer-completion-help
                                        minibuffer-setup-hook)))
    (when (eq def-com 'ido) (setq def-com 'ido-completing-read))
    (unless (or (not entry) def-com)
      ;; An entry in *read-handlers-alist exists but have
      ;; a nil value, so we exit from here, disable `helm-mode'
      ;; and run the command again with it original behavior.
      ;; `helm-mode' will be restored on exit.
      (return-from helm-completing-read-default
        (unwind-protect
             (progn
               (helm-mode -1)
               (apply completing-read-function def-args))
          (helm-mode 1))))
    ;; If we use now `completing-read' we MUST turn off `helm-mode'
    ;; to avoid infinite recursion and CRASH. It will be reenabled on exit.
    (when (or (eq def-com 'completing-read)
              ;; All specialized functions are prefixed by "helm"
              (and (stringp str-defcom)
                   (not (string-match "^helm" str-defcom))))
      (helm-mode -1))
    (unwind-protect
         (cond (;; An helm specialized function exists, run it.
                (and def-com helm-mode)
                (apply def-com any-args))
               (;; Try to handle `ido-completing-read' everywhere.
                (and def-com (eq def-com 'ido-completing-read))
                (setcar (memq collection def-args)
                        (all-completions "" collection predicate))
                (apply def-com def-args))
               (;; User set explicitely `completing-read' or something similar
                ;; in *read-handlers-alist, use this with exactly the same
                ;; args as in `completing-read'.
                ;; If we are here `helm-mode' is now disabled.
                def-com
                (apply def-com def-args))
               (t ; Fall back to classic `helm-comp-read'.
                (helm-completing-read-default-1
                 prompt collection predicate require-match
                 initial-input hist def inherit-input-method
                 str-command buf-name)))
      (helm-mode 1)
      ;; When exiting minibuffer, `this-command' is set to
      ;; `helm-exit-minibuffer', which is unwanted when starting
      ;; on another `completing-read', so restore `this-command' to
      ;; initial value when exiting.
      (setq this-command current-command))))

(defun* helm-generic-read-file-name
    (prompt &optional dir default-filename mustmatch initial predicate)
  "An helm replacement of `read-file-name'."
  (declare (special helm-mode))
  (let* ((default (and default-filename
                       (if (listp default-filename)
                           (car default-filename)
                           default-filename)))
         (init (or default initial dir default-directory))
         (ini-input (and init (expand-file-name init)))
         (current-command this-command)
         (str-command (symbol-name current-command))
         (helm-file-completion-sources
          (cons str-command
                (remove str-command helm-file-completion-sources)))
         (buf-name (format "*helm-mode-%s*" str-command))
         (entry (assq current-command
                      helm-completing-read-handlers-alist))
         (def-com  (cdr-safe entry))
         (str-defcom (symbol-name def-com))
         (def-args (list prompt dir default-filename mustmatch initial predicate))
         ;; Append the two extra args needed to set the buffer and source name
         ;; in helm specialized functions.
         (any-args (append def-args (list str-command buf-name)))
         (ido-state ido-mode)
         helm-completion-mode-start-message ; Be quiet
         helm-completion-mode-quit-message  ; Same here
         fname)
    ;; Some functions that normally call `completing-read' can switch
    ;; brutally to `read-file-name' (e.g find-tag), in this case
    ;; the helm specialized function will fail because it is build
    ;; for `completing-read', so set it to 'incompatible to be sure
    ;; we switch to `helm-c-read-file-name' and don't try to call it
    ;; with wrong number of args.
    (when (and def-com (> (length (help-function-arglist def-com)) 8))
      (setq def-com 'incompatible))
    (when (eq def-com 'ido) (setq def-com 'ido-read-file-name))
    (unless (or (not entry) def-com)
      (return-from helm-generic-read-file-name
        (unwind-protect
             (progn
               (helm-mode -1)
               (apply read-file-name-function def-args))
          (helm-mode 1))))
    ;; If we use now `read-file-name' we MUST turn off `helm-mode'
    ;; to avoid infinite recursion and CRASH. It will be reenabled on exit.
    (when (or (eq def-com 'read-file-name)
              (eq def-com 'ido-read-file-name)
              (and (stringp str-defcom)
                   (not (string-match "^helm" str-defcom))))
      (helm-mode -1))
    (unwind-protect
         (setq fname
               (cond (;; A specialized function exists, run it
                      ;; with the two extra args specific to helm..
                      (and def-com helm-mode
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
                     (t ; Fall back to classic `helm-c-read-file-name'.
                      (helm-c-read-file-name
                       prompt
                       :name str-command
                       :buffer buf-name
                       :initial-input (expand-file-name init dir)
                       :alistp nil
                       :must-match mustmatch
                       :test predicate))))
      (helm-mode 1)
      (ido-mode (if ido-state 1 -1))
      ;; Same comment as in `helm-completing-read-default'.
      (setq this-command current-command))
    fname))

;;;###autoload
(define-minor-mode helm-mode
    "Toggle generic helm completion.

All functions in Emacs that use `completing-read'
or `read-file-name' and friends will use helm interface
when this mode is turned on.
However you can modify this behavior for functions of your choice
with `helm-completing-read-handlers-alist'.

Called with a positive arg, turn on unconditionally, with a
negative arg turn off.
You can turn it on with `helm-mode'.

Some crap emacs functions may not be supported,
e.g `ffap-alternate-file' and maybe others
You can add such functions to `helm-completing-read-handlers-alist'
with a nil value.

Note: This mode will work only partially on Emacs23."
  :group 'helm
  :global t
  :lighter helm-completion-mode-string
  (declare (special completing-read-function))
  (if helm-mode
      (progn
        (setq completing-read-function 'helm-completing-read-default
              read-file-name-function  'helm-generic-read-file-name)
        (message helm-completion-mode-start-message))
      (setq completing-read-function (and (fboundp 'completing-read-default)
                                          'completing-read-default)
            read-file-name-function  (and (fboundp 'read-file-name-default)
                                          'read-file-name-default))
      (message helm-completion-mode-quit-message)))



;;; Eshell completion.
;;
;; Enable like this in .emacs:
;;
;; (add-hook 'eshell-mode-hook
;;           #'(lambda ()
;;               (define-key eshell-mode-map [remap pcomplete] 'helm-esh-pcomplete)))
;;
(defvar helm-c-source-esh
  '((name . "Eshell completions")
    (init . (lambda ()
              (setq pcomplete-current-completions nil
                    pcomplete-last-completion-raw nil)
              ;; Eshell-command add this hook in all minibuffers
              ;; Remove it for the helm one. (Fixed in Emacs24)
              (remove-hook 'minibuffer-setup-hook 'eshell-mode)))
    (candidates . helm-esh-get-candidates)
    (action . helm-ec-insert))
  "Helm source for Eshell completion.")

;; Internal.
(defvar helm-ec-target "")
(defun helm-ec-insert (candidate)
  "Replace text at point with CANDIDATE.
The function that call this should set `helm-ec-target' to thing at point.
This is the same as `ac-insert', just inlined here for compatibility."
  (let ((pt (point)))
    (when (and helm-ec-target
               (search-backward helm-ec-target nil t)
               (string= (buffer-substring (point) pt) helm-ec-target))
      (delete-region (point) pt)))
  (insert (helm-quote-whitespace candidate)))

(defun helm-esh-get-candidates ()
  "Get candidates for eshell completion using `pcomplete'."
  (catch 'pcompleted
    (let* ((pcomplete-stub)
           pcomplete-seen pcomplete-norm-func
           pcomplete-args pcomplete-last pcomplete-index
           (pcomplete-autolist pcomplete-autolist)
           (pcomplete-suffix-list pcomplete-suffix-list))
      (with-helm-current-buffer
        (loop with table  = (pcomplete-completions)
              with entry  = (condition-case nil
                                ;; On Emacs24 `try-completion' return
                                ;; pattern when more than one result.
                                ;; Otherwise Emacs23 return nil, which
                                ;; is wrong, in this case use pattern
                                ;; to behave like Emacs24.
                                (or (try-completion helm-pattern
                                                    (pcomplete-entries))
                                    helm-pattern)
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
(defvar helm-c-source-eshell-history
  `((name . "Eshell history")
    (init . (lambda ()
              (let (eshell-hist-ignoredups)
                ;; Write the content's of ring to file.
                (eshell-write-history eshell-history-file-name t)
                (with-current-buffer (helm-candidate-buffer 'global)
                  (insert-file-contents eshell-history-file-name)))
              ;; Same comment as in `helm-c-source-esh'
              (remove-hook 'minibuffer-setup-hook 'eshell-mode)))
    (candidates-in-buffer)
    (keymap . ,helm-eshell-history-map)
    (filtered-candidate-transformer . (lambda (candidates sources)
                                        (reverse candidates)))
    (candidate-number-limit . 9999)
    (action . (lambda (candidate)
                (eshell-kill-input)
                (insert candidate))))
  "Helm source for Eshell history.")


;;; Show completion - an alternative of helm-show-completion.el.
;;
;; Provide show completion with macro `with-helm-show-completion'.


;; Called each time cursor move in helm-buffer.
(defun helm-c-show-completion ()
  (with-helm-current-buffer
    (overlay-put helm-c-show-completion-overlay
                 'display (helm-get-selection))))

(defun helm-c-show-completion-init-overlay (beg end)
  (and helm-c-turn-on-show-completion
       (setq helm-c-show-completion-overlay (make-overlay beg end))
       (overlay-put helm-c-show-completion-overlay
                    'face 'helm-lisp-show-completion)))

(defun helm-c-show-completion-display-function (buffer)
  "A special resized helm window is used depending on position in BUFFER."
  (with-selected-window (selected-window)
    (let* ((screen-size  (+ (count-screen-lines (window-start) (point) t)
                            1                             ; mode-line
                            (if header-line-format 1 0))) ; header-line
           (def-size     (- (window-height)
                            helm-c-show-completion-min-window-height))
           (upper-height (max window-min-height (min screen-size def-size)))
           split-window-keep-point)
      (recenter -1)
      (set-window-buffer (if (active-minibuffer-window)
                             (minibuffer-selected-window)
                             (split-window nil upper-height))
                         buffer))))

(defmacro with-helm-show-completion (beg end &rest body)
  "Show helm candidate in an overlay at point.
BEG and END are the beginning and end position of the current completion
in `helm-current-buffer'.
BODY is an helm call where we want to enable show completion.
If `helm-c-turn-on-show-completion' is nil just do nothing."
  (declare (indent 2) (debug t))
  `(let ((helm-move-selection-after-hook
          (and helm-c-turn-on-show-completion
               (append (list 'helm-c-show-completion)
                       helm-move-selection-after-hook))))
     (unwind-protect
          (progn
            (helm-c-show-completion-init-overlay ,beg ,end)
            (let ((helm-display-function
                   (if helm-c-show-completion-use-special-display
                       'helm-c-show-completion-display-function
                       'helm-default-display-buffer)))
              ,@body))
       (and helm-c-turn-on-show-completion
            (delete-overlay helm-c-show-completion-overlay)))))


;;; Lisp symbol completion.
;;
;;
;;;###autoload
(defun helm-lisp-completion-at-point ()
  "Helm lisp symbol completion at point."
  (interactive)
  (let* ((data       (lisp-completion-at-point))
         (beg        (car data))
         (end        (point)) ; 'cadr data' is wrong when no space after point.
         (plist      (nthcdr 3 data))
         (pred       (plist-get plist :predicate))
         (lgst-len   0)
         (target     (and beg end (buffer-substring-no-properties beg end)))
         (candidates (all-completions target (nth 2 data) pred))
         (helm-quit-if-no-candidate t)
         
         (helm-execute-action-at-once-if-one t)
         (helm-match-plugin-enabled
          (member 'helm-compile-source--match-plugin
                  helm-compile-source-functions)))
    (if candidates
        (with-helm-show-completion beg end
          ;; Overlay is initialized now in helm-current-buffer.
          (helm
           :sources
           '((name . "Lisp completion")
             (init . (lambda ()
                       (with-current-buffer (helm-candidate-buffer 'global)
                         (loop for sym in candidates
                               for len = (length sym)
                               when (> len lgst-len) do (setq lgst-len len)
                               do (insert (concat sym "\n"))))))
             (candidates-in-buffer)
             (persistent-action . helm-lisp-completion-persistent-action)
             (persistent-help . "Show brief doc in mode-line")
             (filtered-candidate-transformer helm-lisp-completion-transformer)
             (action . (lambda (candidate)
                         (delete-region beg end)
                         (insert candidate))))
           :input (if helm-match-plugin-enabled (concat target " ") target)))
        (message "[No Match]"))))

(defun helm-lisp-completion-persistent-action (candidate)
  (let ((cursor-in-echo-area t)
        mode-line-in-non-selected-windows)
    (helm-c-show-info-in-mode-line
     (propertize
      (helm-c-get-first-line-documentation
       (intern candidate))
      'face 'helm-lisp-completion-info))))

(defun helm-lisp-completion-transformer (candidates source)
  "Helm candidates transformer for lisp completion."
  (declare (special lgst-len))
  (loop for c in candidates
        for sym = (intern c)
        for annot = (cond ((commandp sym) " (Com)")
                          ((fboundp sym)  " (Fun)")
                          ((boundp sym)   " (Var)")
                          ((facep sym)    " (Face)"))
        for spaces = (make-string (- lgst-len (length c)) ? )
        collect (cons (concat c spaces annot) c)))

(defun helm-c-get-first-line-documentation (sym)
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
(defun helm-c-thing-before-point ()
  "Get symbol name before point.
Borrowed from helm-complete.el, inlined here for compatibility."
  (save-excursion
    (let ((beg (point)))
      ;; older regexp "\(\\|\\s-\\|^\\|\\_<\\|\r\\|'\\|#'"
      (when (re-search-backward
             "\\_<" (field-beginning nil nil (point-at-bol)) t)
        (buffer-substring-no-properties beg (match-end 0))))))

;;;###autoload
(defun helm-c-complete-file-name-at-point ()
  "Complete file name at point."
  (interactive)
  (let* ((init (substring-no-properties (thing-at-point 'filename)))
         (end  (point))
         (beg  (- (point) (length init)))
         (helm-quit-if-no-candidate t)
         (helm-execute-action-at-once-if-one t)
         completion)
    (with-helm-show-completion beg end
      (setq completion (helm-c-read-file-name "FileName: "
                                                  :initial-input init)))
    (helm-c-insert-file-name-completion-at-point completion)))

;; Internal
(defvar helm-lisp-completion-counter 0)
;;;###autoload
(defun helm-lisp-completion-at-point-or-indent (arg)
  "First call indent and second call complete lisp symbol.
The second call should happen before `helm-lisp-completion-or-indent-delay',
after this delay, next call will indent again.
After completion, next call is always indent.
See that like click and double mouse click.
One hit indent, two quick hits maybe indent and complete."
  (interactive "P")
  ;; Be sure `indent-for-tab-command' will not try
  ;; to use `completion-at-point'.
  (let ((tab-always-indent (if (eq tab-always-indent 'complete)
                               t tab-always-indent)))
    (incf helm-lisp-completion-counter)
    (unwind-protect
         (if (> helm-lisp-completion-counter 1)
             (helm-lisp-completion-or-file-name-at-point)
             (indent-for-tab-command arg))
      ;; After `helm-lisp-completion-or-indent-delay' seconds
      ;; reset to 0.
      (run-with-timer helm-lisp-completion-or-indent-delay nil
                      #'(lambda ()
                          (setq helm-lisp-completion-counter 0)))
      ;; Always reset to 0 at second hit.
      (when (eq helm-lisp-completion-counter 2)
        (setq helm-lisp-completion-counter 0)))))

;;;###autoload
(defun helm-lisp-completion-or-file-name-at-point ()
  "Complete lisp symbol or filename at point.
Filename completion happen if filename is started in
or between double quotes."
  (interactive)
  (let ((tap (substring-no-properties (thing-at-point 'filename))))
    (if (and tap (string-match "^\\(~/\\|/\\|[a-zA-Z]\:/\\).*" tap)
             (save-excursion (search-backward "\"" (point-at-bol) t)))
        (helm-c-complete-file-name-at-point)
        (helm-lisp-completion-at-point))))

(defun helm-c-apropos-init (test default)
  "Init candidates buffer for `helm-c-apropos' sources."
  (with-current-buffer (helm-candidate-buffer 'global)
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


;;; Run Externals commands within Emacs with helm completion
;;
;;
(defvar helm-external-command-history nil)

(defun helm-c-external-commands-list-1 (&optional sort)
  "Returns a list of all external commands the user can execute.
If `helm-c-external-commands-list' is non-nil it will
return its contents.  Else it calculates all external commands
and sets `helm-c-external-commands-list'."
  (if helm-c-external-commands-list
      helm-c-external-commands-list
      (setq helm-c-external-commands-list
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

(defun helm-run-or-raise (exe &optional file)
  "Generic command that run asynchronously EXE.
If EXE is already running just jump to his window if `helm-raise-command'
is non--nil.
When FILE argument is provided run EXE with FILE.
In this case EXE must be provided as \"EXE %s\"."
  (lexical-let* ((real-com (car (split-string (replace-regexp-in-string
                                               "%s" "" exe))))
                 (proc     (if file (concat real-com " " file) real-com)))
    (if (get-process proc)
        (if helm-raise-command
            (shell-command  (format helm-raise-command real-com))
            (error "Error: %s is already running" real-com))
        (when (loop for i in helm-c-external-commands-list thereis real-com)
          (message "Starting %s..." real-com)
          (if file
              (start-process-shell-command
               proc nil (format exe (shell-quote-argument
                                     (if (eq system-type 'windows-nt)
                                         (helm-w32-prepare-filename file)
                                         file))))
              (start-process-shell-command proc nil real-com))
          (set-process-sentinel
           (get-process proc)
           #'(lambda (process event)
               (when (and (string= event "finished\n")
                          helm-raise-command
                          (not (helm-c-get-pid-from-process-name real-com)))
                 (shell-command  (format helm-raise-command "emacs")))
               (message "%s process...Finished." process))))
        (setq helm-c-external-commands-list
              (cons real-com
                    (delete real-com helm-c-external-commands-list))))))



;;; Generic action functions
;;
;;
(defun helm-c-file-buffers (filename)
  "Returns a list of buffer names corresponding to FILENAME."
  (let ((name     (expand-file-name filename))
        (buf-list ()))
    (dolist (buf (buffer-list) buf-list)
      (let ((bfn (buffer-file-name buf)))
        (when (and bfn (string= name bfn))
          (push (buffer-name buf) buf-list))))))

(defun helm-revert-buffer (candidate)
  (with-current-buffer candidate
    (when (or (buffer-modified-p)
              (not (verify-visited-file-modtime
                    (get-buffer candidate))))
      (revert-buffer t t))))

(defun helm-revert-marked-buffers (ignore)
  (mapc 'helm-revert-buffer (helm-marked-candidates)))

(defun helm-kill-marked-buffers (ignore)
  (mapc 'kill-buffer (helm-marked-candidates)))

(defun helm-c-delete-file (file &optional error-if-dot-file-p)
  "Delete the given file after querying the user.
Ask to kill buffers associated with that file, too."
  (when (and error-if-dot-file-p
             (helm-ff-dot-file-p file))
    (error "Error: Cannot operate on `.' or `..'"))
  (let ((buffers (helm-c-file-buffers file)))
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

(defun helm-get-mailcap-for-file (filename)
  "Get the command to use for FILENAME from mailcap files.
The command is like <command %s> and is meant to use with `format'."
  (mailcap-parse-mailcaps)
  (let* ((ext  (file-name-extension filename))
         (mime (when ext (mailcap-extension-to-mime ext)))
         (result (when mime (mailcap-mime-info mime))))
    ;; If elisp file have no associations in .mailcap
    ;; `mailcap-maybe-eval' is returned, in this case just return nil.
    (when (stringp result) result)))

(defun helm-get-default-program-for-file (filename)
  "Try to find a default program to open FILENAME.
Try first in `helm-c-external-programs-associations' and then in mailcap file
if nothing found return nil."
  (let* ((ext      (file-name-extension filename))
         (def-prog (assoc-default ext helm-c-external-programs-associations)))
    (cond ((and def-prog (not (string= def-prog "")))
           (concat def-prog " %s"))
          ((and helm-c-default-external-file-browser
                (file-directory-p filename))
           (concat helm-c-default-external-file-browser " %s"))
          (t (helm-get-mailcap-for-file filename)))))

(defun helm-c-open-file-externally (file)
  "Open FILE with an external program.
Try to guess which program to use with `helm-get-default-program-for-file'.
If not found or a prefix arg is given query the user which tool to use."
  (let* ((fname          (expand-file-name file))
         (collection     (helm-c-external-commands-list-1 'sort))
         (def-prog       (helm-get-default-program-for-file fname))
         (real-prog-name (if (or helm-current-prefix-arg (not def-prog))
                             ;; Prefix arg or no default program.
                             (prog1
                                 (helm-comp-read
                                  "Program: " collection
                                  :must-match t
                                  :name "Open file Externally"
                                  :history helm-external-command-history)
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
        (helm-aif (assoc (file-name-extension fname)
                             helm-c-external-programs-associations)
            (setq helm-c-external-programs-associations
                  (delete it helm-c-external-programs-associations)))
        (push (cons (file-name-extension fname)
                    (read-string
                     "Program (Add args maybe and confirm): " real-prog-name))
              helm-c-external-programs-associations)
        (customize-save-variable 'helm-c-external-programs-associations
                                 helm-c-external-programs-associations)))
    (helm-run-or-raise program file)
    (setq helm-external-command-history
          (cons real-prog-name
                (delete real-prog-name
                        (loop for i in helm-external-command-history
                              when (executable-find i) collect i))))))

(defun helm-c-find-file-or-marked (candidate)
  "Open file CANDIDATE or open helm marked files in background."
  (let ((marked (helm-marked-candidates))
        (ffap-newfile-prompt helm-ff-newfile-prompt-p)
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
                           (helm-c-basename dirfname))
                    (make-directory candidate 'parent)))
              (helm-find-files-1 candidate))
            ;; A non--existing filename NOT ending with / or
            ;; an existing filename, create or jump to it.
            (find-file-at-point (car marked))))))

(defun helm-delete-marked-files (ignore)
  (let* ((files (helm-marked-candidates))
         (len (length files)))
    (if (not (y-or-n-p
              (format "Delete *%s File(s):\n%s"
                      len
                      (mapconcat (lambda (f) (format "- %s\n" f)) files ""))))
        (message "(No deletions performed)")
        (dolist (i files)
          (set-text-properties 0 (length i) nil i)
          (helm-c-delete-file i helm-ff-signal-error-on-dot-files))
        (message "%s File(s) deleted" len))))

(defun helm-ediff-marked-buffers (candidate &optional merge)
  "Ediff 2 marked buffers or CANDIDATE and `helm-current-buffer'.
With optional arg MERGE call `ediff-merge-buffers'."
  (let ((lg-lst (length (helm-marked-candidates)))
        buf1 buf2)
    (case lg-lst
      (0
       (error "Error:You have to mark at least 1 buffer"))
      (1
       (setq buf1 helm-current-buffer
             buf2 (first (helm-marked-candidates))))
      (2
       (setq buf1 (first (helm-marked-candidates))
             buf2 (second (helm-marked-candidates))))
      (t
       (error "Error:To much buffers marked!")))
    (if merge
        (ediff-merge-buffers buf1 buf2)
        (ediff-buffers buf1 buf2))))

(defun helm-ediff-marked-buffers-merge (candidate)
  "Ediff merge `helm-current-buffer' with CANDIDATE.
See `helm-ediff-marked-buffers'."
  (helm-ediff-marked-buffers candidate t))

(defun helm-bookmark-get-bookmark-from-name (bmk)
  "Return bookmark name even if it is a bookmark with annotation.
e.g prepended with *.
Return nil if bmk is not a valid bookmark."
  (let ((bookmark (replace-regexp-in-string "\*" "" bmk)))
    (if (assoc bookmark bookmark-alist)
        bookmark
        (when (assoc bmk bookmark-alist)
          bmk))))

(defun helm-delete-marked-bookmarks (ignore)
  "Delete this bookmark or all marked bookmarks."
  (dolist (i (helm-marked-candidates))
    (bookmark-delete (helm-bookmark-get-bookmark-from-name i)
                     'batch)))

(defun helm-require-or-error (feature function)
  (or (require feature nil t)
      (error "Need %s to use `%s'." feature function)))

(defun helm-find-buffer-on-elscreen (candidate)
  "Open buffer in new screen, if marked buffers open all in elscreens."
  (helm-require-or-error 'elscreen 'helm-find-buffer-on-elscreen)
  (helm-aif (helm-marked-candidates)
      (dolist (i it)
        (let ((target-screen (elscreen-find-screen-by-buffer
                              (get-buffer i) 'create)))
          (elscreen-goto target-screen)))
    (let ((target-screen (elscreen-find-screen-by-buffer
                          (get-buffer candidate) 'create)))
      (elscreen-goto target-screen))))

(defun helm-elscreen-find-file (file)
  (helm-require-or-error 'elscreen 'helm-elscreen-find-file)
  (elscreen-find-file file))

(defun helm-w32-prepare-filename (file)
  "Convert filename FILE to something usable by external w32 executables."
  (replace-regexp-in-string ; For UNC paths
   "/" "\\"
   (replace-regexp-in-string ; Strip cygdrive paths
    "/cygdrive/\\(.\\)" "\\1:"
    file nil nil) nil t))

;;;###autoload
(defun helm-w32-shell-execute-open-file (file)
  (interactive "fOpen file:")
  (with-no-warnings
    (w32-shell-execute "open" (helm-w32-prepare-filename file))))

(defun helm-c-open-file-with-default-tool (file)
  "Open FILE with the default tool on this platform."
  (if (eq system-type 'windows-nt)
      (helm-w32-shell-execute-open-file file)
      (start-process "helm-c-open-file-with-default-tool"
                     nil
                     (cond ((eq system-type 'gnu/linux)
                            "xdg-open")
                           ((or (eq system-type 'darwin) ;; Mac OS X
                                (eq system-type 'macos)) ;; Mac OS 9
                            "open"))
                     file)))

(defun helm-c-open-dired (file)
  "Opens a dired buffer in FILE's directory.  If FILE is a
directory, open this directory."
  (if (file-directory-p file)
      (dired file)
      (dired (file-name-directory file))
      (dired-goto-file file)))

(defun helm-c-display-to-real-line (candidate)
  (if (string-match "^ *\\([0-9]+\\):\\(.*\\)$" candidate)
      (list (string-to-number (match-string 1 candidate)) (match-string 2 candidate))
      (error "Line number not found")))

(defun helm-c-action-line-goto (lineno-and-content)
  (apply #'helm-goto-file-line (helm-interpret-value (helm-attr 'target-file))
         (append lineno-and-content
                 (list (if (and (helm-attr-defined 'target-file)
                                (not helm-in-persistent-action))
                           'find-file-other-window
                           'find-file)))))

(defun* helm-c-action-file-line-goto (file-line-content &optional (find-file-function #'find-file))
  (apply #'helm-goto-file-line
         (if (stringp file-line-content)
             ;; Case: filtered-candidate-transformer is skipped
             (cdr (helm-c-filtered-candidate-transformer-file-line-1 file-line-content))
             file-line-content)))

(require 'compile)
(defun helm-c-filtered-candidate-transformer-file-line (candidates source)
  (delq nil (mapcar 'helm-c-filtered-candidate-transformer-file-line-1 candidates)))

(defun helm-c-filtered-candidate-transformer-file-line-1 (candidate)
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
                   (or (helm-interpret-value (helm-attr 'default-directory))
                       (and (helm-candidate-buffer)
                            (buffer-local-value
                             'default-directory (helm-candidate-buffer)))))
                  (string-to-number lineno) content)))))

(defun* helm-goto-file-line (file lineno content &optional (find-file-function #'find-file))
  (helm-aif (helm-attr 'before-jump-hook)
      (funcall it))
  (when file (funcall find-file-function file))
  (if (helm-attr-defined 'adjust)
      (helm-c-goto-line-with-adjustment lineno content)
      (helm-goto-line lineno))
  (unless (helm-attr-defined 'recenter)
    (set-window-start (get-buffer-window helm-current-buffer) (point)))
  (helm-aif (helm-attr 'after-jump-hook)
      (funcall it))
  (when helm-in-persistent-action
    (helm-match-line-color-current-line)))

(defun helm-find-file-as-root (candidate)
  (find-file (concat "/" helm-su-or-sudo "::" (expand-file-name candidate))))

(defun helm-find-many-files (ignore)
  (mapc 'find-file (helm-marked-candidates)))

;; borrowed from etags.el
;; (helm-c-goto-line-with-adjustment (line-number-at-pos) ";; borrowed from etags.el")
(defun helm-c-goto-line-with-adjustment (line line-content)
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
    (setq startpos (progn (helm-goto-line line) (point)))
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

(helm-document-attribute 'default-directory "type . file-line"
                             "`default-directory' to interpret file.")
(helm-document-attribute 'before-jump-hook "type . file-line / line"
                             "Function to call before jumping to the target location.")
(helm-document-attribute 'after-jump-hook "type . file-line / line"
                             "Function to call after jumping to the target location.")
(helm-document-attribute 'adjust "type . file-line"
                             "Search around line matching line contents.")
(helm-document-attribute 'recenter "type . file-line / line"
                             "`recenter' after jumping.")
(helm-document-attribute 'target-file "type . line"
                             "Goto line of target-file.")

;;;###autoload
(defun helm-c-call-interactively (cmd-or-name)
  "Execute CMD-OR-NAME as Emacs command.
It is added to `extended-command-history'.
`helm-current-prefix-arg' is used as the command's prefix argument."
  (setq extended-command-history
        (cons (helm-c-stringify cmd-or-name)
              (delete (helm-c-stringify cmd-or-name) extended-command-history)))
  (let ((current-prefix-arg helm-current-prefix-arg)
        (cmd (helm-c-symbolify cmd-or-name)))
    (if (stringp (symbol-function cmd))
        (execute-kbd-macro (symbol-function cmd))
        (setq this-command cmd)
        (call-interactively cmd))))

;;;###autoload
(defun helm-c-set-variable (var)
  "Set value to VAR interactively."
  (interactive)
  (let ((sym (helm-c-symbolify var)))
    (set sym (eval-minibuffer (format "Set %s: " var)
                              (prin1-to-string (symbol-value sym))))))
;; (setq hh 12)
;; (helm-c-set-variable 'hh)



;;; Persistent Action Helpers
;;
;;
(defvar helm-match-line-overlay-face nil)
(defvar helm-match-line-overlay nil)

(defun helm-match-line-color-current-line (&optional start end buf face rec)
  "Highlight and underline current position"
  (let ((args (list (or start (line-beginning-position))
                    (or end (1+ (line-end-position)))
                    buf)))
    (if (not helm-match-line-overlay)
        (setq helm-match-line-overlay (apply 'make-overlay args))
        (apply 'move-overlay helm-match-line-overlay args)))
  (overlay-put helm-match-line-overlay
               'face (or face helm-match-line-overlay-face))
  (when rec
    (goto-char start)
    (recenter)))

(defalias 'helm-persistent-highlight-point 'helm-match-line-color-current-line)


(setq helm-match-line-overlay-face 'helm-overlay-line-face)

(defun helm-match-line-cleanup ()
  (when helm-match-line-overlay
    (delete-overlay helm-match-line-overlay)
    (setq helm-match-line-overlay nil)))

(defun helm-match-line-update ()
  (when helm-match-line-overlay
    (delete-overlay helm-match-line-overlay)
    (helm-match-line-color-current-line)))

(add-hook 'helm-cleanup-hook 'helm-match-line-cleanup)
(add-hook 'helm-after-persistent-action-hook 'helm-match-line-update)


;;; Actions Transformers
;;
;;
;;; Files
(defun helm-c-transform-file-load-el (actions candidate)
  "Add action to load the file CANDIDATE if it is an emacs lisp
file.  Else return ACTIONS unmodified."
  (if (member (file-name-extension candidate) '("el" "elc"))
      (append actions '(("Load Emacs Lisp File" . load-file)))
      actions))

(defun helm-c-transform-file-browse-url (actions candidate)
  "Add an action to browse the file CANDIDATE if it in a html
file or URL.  Else return ACTIONS unmodified."
  (let ((browse-action '("Browse with Browser" . browse-url)))
    (cond ((string-match "^http\\|^ftp" candidate)
           (cons browse-action actions))
          ((string-match "\\.html?$" candidate)
           (append actions (list browse-action)))
          (t actions))))

;;; Function
(defun helm-c-transform-function-call-interactively (actions candidate)
  "Add an action to call the function CANDIDATE interactively if
it is a command.  Else return ACTIONS unmodified."
  (if (commandp (intern-soft candidate))
      (append actions '(("Call Interactively"
                         .
                         helm-c-call-interactively)))
      actions))

;;;; S-Expressions
(defun helm-c-transform-sexp-eval-command-sexp (actions candidate)
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
(defun helm-c-skip-boring-buffers (buffers)
  (helm-c-skip-entries buffers helm-c-boring-buffer-regexp))

(defun helm-c-skip-current-buffer (buffers)
  "[DEPRECATED] Skip current buffer in buffer lists.
This transformer should not be used as this is now handled directly
in `helm-c-buffer-list' and `helm-c-highlight-buffers'."
  (if helm-allow-skipping-current-buffer
      (remove (buffer-name helm-current-buffer) buffers)
      buffers))

(defun helm-c-shadow-boring-buffers (buffers)
  "Buffers matching `helm-c-boring-buffer-regexp' will be
displayed with the `file-name-shadow' face if available."
  (helm-c-shadow-entries buffers helm-c-boring-buffer-regexp))

(defvar helm-c-buffer-display-string-functions
  '(helm-c-buffer-display-string--compilation
    helm-c-buffer-display-string--shell
    helm-c-buffer-display-string--eshell)
  "Functions to setup display string for buffer.

Function has one argument, buffer name.
If it returns string, use it.
If it returns nil, display buffer name.
See `helm-c-buffer-display-string--compilation' for example.")

(defun helm-c-transform-buffer-display-string (buffers)
  "Setup display string for buffer candidates
using `helm-c-buffer-display-string-functions'."
  (loop for buf in buffers
        if (consp buf)
        collect buf
        else
        for disp = (progn (set-buffer buf)
                          (run-hook-with-args-until-success
                           'helm-c-buffer-display-string-functions buf))
        collect (if disp (cons disp buf) buf)))

(defun helm-c-buffer-display-string--compilation (buf)
  (helm-aif (car compilation-arguments)
      (format "%s: %s [%s]" buf it default-directory)))

(defun helm-c-buffer-display-string--eshell (buf)
  (declare (special eshell-history-ring))
  (when (eq major-mode 'eshell-mode)
    (format "%s: %s [%s]" buf
            (ignore-errors (ring-ref eshell-history-ring 0))
            default-directory)))

(defun helm-c-buffer-display-string--shell (buf)
  (when (eq major-mode 'shell-mode)
    (format "%s: %s [%s]" buf
            (ignore-errors (ring-ref comint-input-ring 0))
            default-directory)))

;;; Files
(defun helm-c-shadow-boring-files (files)
  "Files matching `helm-c-boring-file-regexp' will be
displayed with the `file-name-shadow' face if available."
  (helm-c-shadow-entries files helm-c-boring-file-regexp))

(defun helm-c-skip-boring-files (files)
  "Files matching `helm-c-boring-file-regexp' will be skipped."
  (helm-c-skip-entries files helm-c-boring-file-regexp))
;; (helm-c-skip-boring-files '("README" "/src/.svn/hoge"))

(defun helm-c-skip-current-file (files)
  "Current file will be skipped."
  (remove (buffer-file-name helm-current-buffer) files))

(defun helm-c-w32-pathname-transformer (args)
  "Change undesirable features of windows pathnames to ones more acceptable to
other candidate transformers."
  (if (eq system-type 'windows-nt)
      (helm-transform-mapcar
       (lambda (x)
         (replace-regexp-in-string
          "/cygdrive/\\(.\\)" "\\1:"
          (replace-regexp-in-string "\\\\" "/" x)))
       args)
    args))

(defun helm-c-shorten-home-path (files)
  "Replaces /home/user with ~."
  (let ((home (replace-regexp-in-string "\\\\" "/" ; stupid Windows...
                                        (getenv "HOME"))))
    (helm-transform-mapcar
     (lambda (file)
       (if (and (stringp file) (string-match home file))
           (cons (replace-match "~" nil nil file) file)
         file))
     files)))

;;; Functions
(defun helm-c-mark-interactive-functions (functions)
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
(defvar helm-c-adaptive-done nil
  "nil if history information is not yet stored for the current
selection.")

(defvar helm-c-adaptive-history nil
  "Contains the stored history information.
Format: ((SOURCE-NAME (SELECTED-CANDIDATE (PATTERN . NUMBER-OF-USE) ...) ...) ...)")

;; Should run at beginning of `helm-initial-setup'.
(add-hook 'helm-before-initialize-hook #'(lambda ()
                                               (when helm-c-use-adaptative-sorting
                                                 (setq helm-c-adaptive-done nil))))

;; Should run at beginning of `helm-exit-minibuffer'.
(add-hook 'helm-before-action-hook #'(lambda ()
                                          (when helm-c-use-adaptative-sorting
                                            (helm-c-adaptive-store-selection))))

;; Should run at beginning of `helm-select-action'.
(add-hook 'helm-select-action-hook #'(lambda ()
                                           (when helm-c-use-adaptative-sorting
                                             (helm-c-adaptive-store-selection))))

(defun helm-c-source-use-adaptative-p (&optional source-name)
  "Return current source only if it use adaptative history, nil otherwise."
  (when helm-c-use-adaptative-sorting
    (let* ((source (or source-name (helm-get-current-source)))
           (adapt-source (or (assoc-default 'filtered-candidate-transformer
                                            (assoc (assoc-default 'type source)
                                                   helm-type-attributes))
                             (assoc-default 'candidate-transformer
                                            (assoc (assoc-default 'type source)
                                                   helm-type-attributes))
                             (assoc-default 'filtered-candidate-transformer source)
                             (assoc-default 'candidate-transformer source))))
      (if (listp adapt-source)
          (when (member 'helm-c-adaptive-sort adapt-source) source)
          (when (eq adapt-source 'helm-c-adaptive-sort) source)))))

(defun helm-c-adaptive-store-selection ()
  "Store history information for the selected candidate."
  (unless helm-c-adaptive-done
    (setq helm-c-adaptive-done t)
    (let ((source (helm-c-source-use-adaptative-p)))
      (when source
        (let* ((source-name (or (assoc-default 'type source)
                                (assoc-default 'name source)))
               (source-info (or (assoc source-name helm-c-adaptive-history)
                                (progn
                                  (push (list source-name) helm-c-adaptive-history)
                                  (car helm-c-adaptive-history))))
               (selection (helm-get-selection))
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
                                        (let ((found (assoc helm-pattern (cdr selection-info))))
                                          (if (not found)
                                              ;; new entry
                                              (cons helm-pattern 0)

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
          (if (> (length (cdr selection-info)) helm-c-adaptive-history-length)
              (setcdr selection-info
                      (subseq (cdr selection-info) 0 helm-c-adaptive-history-length))))))))

(defun helm-c-adaptative-maybe-load-history ()
  (when (and helm-c-use-adaptative-sorting
             (file-readable-p helm-c-adaptive-history-file))
    (load-file helm-c-adaptive-history-file)))

(add-hook 'emacs-startup-hook 'helm-c-adaptative-maybe-load-history)
(add-hook 'kill-emacs-hook 'helm-c-adaptive-save-history)

(defun helm-c-adaptive-save-history (&optional arg)
  "Save history information to file given by `helm-c-adaptive-history-file'."
  (interactive "p")
  (when helm-c-use-adaptative-sorting
    (with-temp-buffer
      (insert
       ";; -*- mode: emacs-lisp -*-\n"
       ";; History entries used for helm adaptive display.\n")
      (prin1 `(setq helm-c-adaptive-history ',helm-c-adaptive-history)
             (current-buffer))
      (insert ?\n)
      (write-region (point-min) (point-max) helm-c-adaptive-history-file nil
                    (unless arg 'quiet)))))

(defun helm-c-adaptive-sort (candidates source)
  "Sort the CANDIDATES for SOURCE by usage frequency.
This is a filtered candidate transformer you can use for the
attribute `filtered-candidate-transformer' of a source in
`helm-sources' or a type in `helm-type-attributes'."
  (let* ((source-name (or (assoc-default 'type source)
                          (assoc-default 'name source)))
         (source-info (assoc source-name helm-c-adaptive-history)))
    (if source-info
        (let ((usage
               ;; ... assemble a list containing the (CANIDATE . USAGE-COUNT)
               ;; pairs
               (mapcar (lambda (candidate-info)
                         (let ((count 0))
                           (dolist (pattern-info (cdr candidate-info))
                             (if (not (equal (car pattern-info)
                                             helm-pattern))
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
                                 :test 'helm-c-adaptive-compare)
                    (push (car info) sorted)
                    (setq candidates (remove* (car info) candidates
                                              :test 'helm-c-adaptive-compare))))

                ;; and append the rest
                (append (reverse sorted) candidates nil))
              (message "Your `%s' is maybe corrupted or too old, \
you should reinitialize it with `helm-c-reset-adaptative-history'"
                       helm-c-adaptive-history-file)
              (sit-for 1)
              candidates))
        ;; if there is no information stored for this source then do nothing
        candidates)))

;;;###autoload
(defun helm-c-reset-adaptative-history ()
  "Delete all `helm-c-adaptive-history' and his file.
Useful when you have a old or corrupted `helm-c-adaptive-history-file'."
  (interactive)
  (when (y-or-n-p "Really delete all your `helm-c-adaptive-history'? ")
    (setq helm-c-adaptive-history nil)
    (delete-file helm-c-adaptive-history-file)))

(defun helm-c-adaptive-compare (x y)
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
(defvar helm-outline-goto-near-line-flag t)
(defvar helm-outline-using nil)
(defun helm-after-update-hook--outline ()
  (if (and (eq helm-outline-using t)
           (eq helm-outline-goto-near-line-flag t))
      (helm-outline-goto-near-line)))
(add-hook 'helm-after-update-hook 'helm-after-update-hook--outline)

(defun helm-outline-goto-near-line ()
  (with-helm-window
    ;; TODO need consideration whether to update position by every input.
    (when t ; (equal helm-pattern "")
      (helm-goto-line 2)
      (let ((lineno (with-helm-current-buffer
                      (line-number-at-pos (car helm-current-position)))))
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
        (and (helm-pos-header-line-p) (forward-line -2))
        (helm-mark-current-line)))))



;;; Plug-in
;;
;;
;; Plug-in: info-index
(defun* helm-c-info-init (&optional (file (helm-attr 'info-file)))
  (let (result)
    (unless (helm-candidate-buffer)
      (save-window-excursion
        (info file)
        (let (Info-history
              (tobuf (helm-candidate-buffer 'global))
              (infobuf (current-buffer))
              s e)
          (dolist (node (or (helm-attr 'index-nodes) (Info-index-nodes)))
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

(defun helm-c-info-goto (node-line)
  (Info-goto-node (car node-line))
  (helm-goto-line (cdr node-line)))

(defun helm-c-info-display-to-real (line)
  (and (string-match
        ;; This regexp is stolen from Info-apropos-matches
        "\\* +\\([^\n]*.+[^\n]*\\):[ \t]+\\([^\n]*\\)\\.\\(?:[ \t\n]*(line +\\([0-9]+\\))\\)?" line)
       (cons (format "(%s)%s" (helm-attr 'info-file) (match-string 2 line))
             (string-to-number (or (match-string 3 line) "1")))))

(defun helm-c-make-info-source (source file)
  `(,@source
    (name . ,(concat "Info Index: " file))
    (info-file . ,file)
    (init . helm-c-info-init)
    (display-to-real . helm-c-info-display-to-real)
    (get-line . buffer-substring)
    (candidates-in-buffer)
    (action ("Goto node" . helm-c-info-goto))))

(defun helm-compile-source--info-index (source)
  (helm-aif (helm-interpret-value (assoc-default 'info-index source))
      (helm-c-make-info-source source it)
    source))
(add-to-list 'helm-compile-source-functions 'helm-compile-source--info-index)

(helm-document-attribute 'info-index "info-index plugin"
                             "Create a source of info index very easily.

ex. (defvar helm-c-source-info-wget '((info-index . \"wget\"))")

(helm-document-attribute 'index-nodes "info-index plugin (optional)"
                             "Index nodes of info file.

If it is omitted, `Info-index-nodes' is used to collect index nodes.
Some info files are missing index specification.

ex. See `helm-c-source-info-screen'.")

;; Plug-in: candidates-file
(defun helm-compile-source--candidates-file (source)
  (if (assoc-default 'candidates-file source)
      `((init helm-p-candidats-file-init
              ,@(let ((orig-init (assoc-default 'init source)))
                     (cond ((null orig-init) nil)
                           ((functionp orig-init) (list orig-init))
                           (t orig-init))))
        (candidates-in-buffer)
        ,@source)
      source))
(add-to-list 'helm-compile-source-functions 'helm-compile-source--candidates-file)

(defun helm-p-candidats-file-init ()
  (destructuring-bind (file &optional updating)
      (helm-mklist (helm-attr 'candidates-file))
    (setq file (helm-interpret-value file))
    (with-current-buffer (helm-candidate-buffer (find-file-noselect file))
      (when updating
        (buffer-disable-undo)
        (font-lock-mode -1)
        (auto-revert-mode 1)))))

(helm-document-attribute 'candidates-file "candidates-file plugin"
                             "Use a file as the candidates buffer.

1st argument is a filename, string or function name or variable name.
If optional 2nd argument is non-nil, the file opened with `auto-revert-mode'.")

;; Plug-in: headline
(defun helm-compile-source--helm-headline (source)
  (if (assoc-default 'headline source)
      (append '((init . helm-headline-init)
                (get-line . buffer-substring)
                (type . line))
              source
              '((candidates-in-buffer)
                (persistent-help . "Show this line")))
      source))
(add-to-list 'helm-compile-source-functions 'helm-compile-source--helm-headline)

(defun helm-headline-init ()
  (when (and (helm-current-buffer-is-modified)
             (with-helm-current-buffer
               (eval (or (helm-attr 'condition) t))))
    (helm-headline-make-candidate-buffer
     (helm-interpret-value (helm-attr 'headline))
     (helm-interpret-value (helm-attr 'subexp)))))

(helm-document-attribute 'headline "Headline plug-in"
                             "Regexp string for helm-headline to scan.")
(helm-document-attribute 'condition "Headline plug-in"
                             "A sexp representing the condition to use helm-headline.")
(helm-document-attribute 'subexp "Headline plug-in"
                             "Display (match-string-no-properties subexp).")

;; Le Wang: Note on how `helm-head-line-get-candidates' works with a list
;; of regexps.
;;
;;   1. Create list of ((title . start-of-match) . hiearchy)
;;   2. Sort this list by start-of-match.
;;   3. Go through sorted list and return titles that reflect full hiearchy.
;;
;; It's quite brilliantly written.
;;


(defun helm-headline-get-candidates (regexp subexp)
  (with-helm-current-buffer
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


(defun helm-headline-make-candidate-buffer (regexp subexp)
  (with-current-buffer (helm-candidate-buffer 'local)
    (loop for (content . pos) in (helm-headline-get-candidates regexp subexp)
          do (insert
              (format "%5d:%s\n"
                      (with-helm-current-buffer
                        (line-number-at-pos pos))
                      content)))))

(defun helm-headline-goto-position (pos recenter)
  (goto-char pos)
  (unless recenter
    (set-window-start (get-buffer-window helm-current-buffer) (point))))


;; Plug-in: persistent-help
(defun helm-compile-source--persistent-help (source)
  (append source '((header-line . helm-persistent-help-string))))
(add-to-list 'helm-compile-source-functions 'helm-compile-source--persistent-help)

(defun helm-persistent-help-string ()
  (substitute-command-keys
   (concat "\\<helm-map>\\[helm-execute-persistent-action]: "
           (or (helm-interpret-value (helm-attr 'persistent-help))
               (helm-aif (or (assoc-default 'persistent-action
                                                (helm-get-current-source))
                                 (assoc-default 'action
                                                (helm-get-current-source)))
                   (cond ((symbolp it) (symbol-name it))
                         ((listp it) (or (ignore-errors (caar it))  ""))))
               "")
           " (keeping session)")))

(helm-document-attribute 'persistent-help "persistent-help plug-in"
                             "A string to explain persistent-action of this source.
It also accepts a function or a variable name.")

;;; (helm '(((name . "persistent-help test")(candidates "a")(persistent-help . "TEST"))))

;; Plug-in: Type customize
(defun helm-c-uniq-list (lst)
  "Like `remove-duplicates' in CL.
But cut deeper duplicates and test by `equal'. "
  (reverse (remove-duplicates (reverse lst) :test 'equal)))
(defvar helm-additional-type-attributes nil)
(defun helm-c-arrange-type-attribute (type spec)
  "Override type attributes by `define-helm-type-attribute'.

The SPEC is like source. The symbol `REST' is replaced
with original attribute value.

 Example: Set `play-sound-file' as default action
   (helm-c-arrange-type-attribute 'file
      '((action (\"Play sound\" . play-sound-file)
         REST ;; Rest of actions (find-file, find-file-other-window, etc...)."
  (add-to-list 'helm-additional-type-attributes
               (cons type
                     (loop with typeattr = (assoc-default
                                            type helm-type-attributes)
                           for (attr . value) in spec
                           if (listp value)
                           collect (cons attr
                                         (helm-c-uniq-list
                                          (loop for v in value
                                                if (eq v 'REST)
                                                append
                                                (assoc-default attr typeattr)
                                                else
                                                collect v)))
                           else
                           collect (cons attr value)))))
(put 'helm-c-arrange-type-attribute 'lisp-indent-function 1)

(defun helm-compile-source--type-customize (source)
  (helm-aif (assoc-default (assoc-default 'type source)
                               helm-additional-type-attributes)
      (append it source)
    source))
(add-to-list 'helm-compile-source-functions
             'helm-compile-source--type-customize t)

;; Plug-in: default-action
(defun helm-compile-source--default-action (source)
  (helm-aif (assoc-default 'default-action source)
      (append `((action ,it ,@(remove it (assoc-default 'action source))))
              source)
    source))
(add-to-list 'helm-compile-source-functions
             'helm-compile-source--default-action t)
(helm-document-attribute 'default-action "default-action plug-in"
                             "Default action.")


;;; Type Attributes
;;
;;
(define-helm-type-attribute 'buffer
    `((action
       ("Switch to buffer" . helm-c-switch-to-buffer)
       ,(and (locate-library "popwin") '("Switch to buffer in popup window" . popwin:popup-buffer))
       ("Switch to buffer other window" . switch-to-buffer-other-window)
       ("Switch to buffer other frame" . switch-to-buffer-other-frame)
       ,(and (locate-library "elscreen") '("Display buffer in Elscreen" . helm-find-buffer-on-elscreen))
       ("Query replace regexp" . helm-c-buffer-query-replace-regexp)
       ("Query replace" . helm-c-buffer-query-replace)
       ("View buffer" . view-buffer)
       ("Display buffer"   . display-buffer)
       ("Grep buffers (C-u grep all buffers)" . helm-c-zgrep-buffers)
       ("Revert buffer(s)" . helm-revert-marked-buffers)
       ("Insert buffer" . insert-buffer)
       ("Kill buffer(s)" . helm-kill-marked-buffers)
       ("Diff with file" . diff-buffer-with-file)
       ("Ediff Marked buffers" . helm-ediff-marked-buffers)
       ("Ediff Merge marked buffers" . (lambda (candidate)
                                         (helm-ediff-marked-buffers candidate t))))
      (persistent-help . "Show this buffer")
      (candidate-transformer helm-c-skip-boring-buffers
                             helm-c-transform-buffer-display-string))
  "Buffer or buffer name.")

(define-helm-type-attribute 'file
    `((action
       ("Find file" . helm-find-many-files)
       ,(and (locate-library "popwin") '("Find file in popup window" . popwin:find-file))
       ("Find file as root" . helm-find-file-as-root)
       ("Find file other window" . find-file-other-window)
       ("Find file other frame" . find-file-other-frame)
       ("Open dired in file's directory" . helm-c-open-dired)
       ("Grep File(s) `C-u recurse'" . helm-find-files-grep)
       ("Zgrep File(s) `C-u Recurse'" . helm-ff-zgrep)
       ("Pdfgrep File(s)" . helm-ff-pdfgrep)
       ("Checksum File" . helm-ff-checksum)
       ("Ediff File" . helm-find-files-ediff-files)
       ("Ediff Merge File" . helm-find-files-ediff-merge-files)
       ("View file" . view-file)
       ("Insert file" . insert-file)
       ("Delete file(s)" . helm-delete-marked-files)
       ("Open file externally (C-u to choose)" . helm-c-open-file-externally)
       ("Open file with default tool" . helm-c-open-file-with-default-tool)
       ("Find file in hex dump" . hexl-find-file))
      (persistent-help . "Show this file")
      (action-transformer helm-c-transform-file-load-el
                          helm-c-transform-file-browse-url)
      (candidate-transformer helm-c-w32-pathname-transformer
                             helm-c-skip-current-file
                             helm-c-skip-boring-files
                             helm-c-shorten-home-path))
  "File name.")

(let ((actions '(("Describe command" . describe-function)
                 ("Add command to kill ring" . helm-c-kill-new)
                 ("Go to command's definition" . find-function)
                 ("Debug on entry" . debug-on-entry)
                 ("Cancel debug on entry" . cancel-debug-on-entry)
                 ("Trace function" . trace-function)
                 ("Trace function (background)" . trace-function-background)
                 ("Untrace function" . untrace-function))))
  (define-helm-type-attribute 'command
      `((action ("Call interactively" . helm-c-call-interactively)
                ,@actions)
        (coerce . helm-c-symbolify)
        (persistent-action . describe-function))
    "Command. (string or symbol)")

  (define-helm-type-attribute 'function
      `((action . ,actions)
        (action-transformer helm-c-transform-function-call-interactively)
        (candidate-transformer helm-c-mark-interactive-functions)
        (coerce . helm-c-symbolify))
    "Function. (string or symbol)"))

(define-helm-type-attribute 'variable
    '((action ("Describe variable" . describe-variable)
       ("Add variable to kill ring" . helm-c-kill-new)
       ("Go to variable's definition" . find-variable)
       ("Set variable" . helm-c-set-variable))
      (coerce . helm-c-symbolify))
  "Variable.")

(define-helm-type-attribute 'sexp
    '((action ("Eval s-expression" . (lambda (c) (eval (read c))))
       ("Add s-expression to kill ring" . kill-new))
      (action-transformer helm-c-transform-sexp-eval-command-sexp))
  "String representing S-Expressions.")

(define-helm-type-attribute 'bookmark
    `((coerce . helm-bookmark-get-bookmark-from-name)
      (action
       ("Jump to bookmark" . helm-c-bookmark-jump)
       ("Jump to BM other window" . bookmark-jump-other-window)
       ("Bookmark edit annotation" . bookmark-edit-annotation)
       ("Bookmark show annotation" . bookmark-show-annotation)
       ("Delete bookmark(s)" . helm-delete-marked-bookmarks)
       ,@(and (locate-library "bookmark-extensions")
              `(("Edit Bookmark" . bmkext-edit-bookmark)))
       ("Rename bookmark" . bookmark-rename)
       ("Relocate bookmark" . bookmark-relocate))
      (keymap . ,helm-c-bookmark-map)
      (mode-line . helm-bookmark-mode-line-string))
  "Bookmark name.")

(define-helm-type-attribute 'line
    '((display-to-real . helm-c-display-to-real-line)
      (action ("Go to Line" . helm-c-action-line-goto)))
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

(define-helm-type-attribute 'file-line
    `((filtered-candidate-transformer helm-c-filtered-candidate-transformer-file-line)
      (multiline)
      (action ("Go to" . helm-c-action-file-line-goto)))
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

(define-helm-type-attribute 'timer
    '((real-to-display . helm-c-timer-real-to-display)
      (action ("Cancel Timer" . cancel-timer)
       ("Describe Function" . (lambda (tm) (describe-function (timer--function tm))))
       ("Find Function" . (lambda (tm) (find-function (timer--function tm)))))
      (persistent-action . (lambda (tm) (describe-function (timer--function tm))))
      (persistent-help . "Describe Function"))
  "Timer.")


;;; Default `helm-sources'
;; Setting `helm-sources' is DEPRECATED, but it seems that newbies
;; tend to invoke M-x helm directly. So I offer default setting.
(setq helm-sources
      '(helm-c-source-buffers-list
        helm-c-source-recentf
        helm-c-source-files-in-current-dir+))


;;; Preconfigured Helm
;;
;;
;;;###autoload
(defun helm-mini ()
  "Preconfigured `helm' lightweight version \(buffer -> recentf\)."
  (interactive)
  (helm-other-buffer '(helm-c-source-buffers-list
                           helm-c-source-recentf
                           helm-c-source-buffer-not-found)
                         "*helm mini*"))
;;;###autoload
(defun helm-for-files ()
  "Preconfigured `helm' for opening files.
ffap -> recentf -> buffer -> bookmark -> file-cache -> files-in-current-dir -> locate."
  (interactive)
  (helm-other-buffer helm-for-files-prefered-list "*helm for files*"))

;;;###autoload
(defun helm-recentf ()
  "Preconfigured `helm' for `recentf'."
  (interactive)
  (helm-other-buffer 'helm-c-source-recentf "*helm recentf*"))

;;;###autoload
(defun helm-info-at-point (arg)
  "Preconfigured `helm' for searching info at point.
With a prefix-arg insert symbol at point."
  (interactive "P")
  (let ((helm-c-google-suggest-default-function
         'helm-c-google-suggest-emacs-lisp))
    (helm :sources '(helm-c-source-info-elisp
                         helm-c-source-info-cl
                         helm-c-source-info-pages
                         helm-c-source-google-suggest)
              :input (and arg (thing-at-point 'symbol))
              :buffer "*helm info*")))

;;;###autoload
(defun helm-show-kill-ring ()
  "Preconfigured `helm' for `kill-ring'.
It is drop-in replacement of `yank-pop'.
You may bind this command to M-y.
First call open the kill-ring browser, next calls move to next line."
  (interactive)
  (helm :sources 'helm-c-source-kill-ring
            :buffer "*helm kill-ring*"))

;;;###autoload
(defun helm-minibuffer-history ()
  "Preconfigured `helm' for `minibuffer-history'."
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (helm-other-buffer 'helm-c-source-minibuffer-history
                           "*helm minibuffer-history*")))

;;;###autoload
(defun helm-gentoo ()
  "Preconfigured `helm' for gentoo linux."
  (interactive)
  (helm-other-buffer '(helm-c-source-gentoo
                           helm-c-source-use-flags)
                         "*helm gentoo*"))

;;;###autoload
(defun helm-imenu ()
  "Preconfigured `helm' for `imenu'."
  (interactive)
  (helm :sources 'helm-c-source-imenu
            :buffer "*helm imenu*"))

;;;###autoload
(defun helm-google-suggest ()
  "Preconfigured `helm' for google search with google suggest."
  (interactive)
  (helm-other-buffer 'helm-c-source-google-suggest "*helm google*"))

;;;###autoload
(defun helm-yahoo-suggest ()
  "Preconfigured `helm' for Yahoo searching with Yahoo suggest."
  (interactive)
  (helm-other-buffer 'helm-c-source-yahoo-suggest "*helm yahoo*"))

;;; Converted from helm-show-*-only
;;;###autoload
(defun helm-for-buffers ()
  "Preconfigured `helm' for buffers."
  (interactive)
  (helm-other-buffer 'helm-c-source-buffers "*helm for buffers*"))

;;;###autoload
(defun helm-buffers-list ()
  "Preconfigured `helm' to list buffers.
It is an enhanced version of `helm-for-buffers'."
  (interactive)
  (helm :sources '(helm-c-source-buffers-list
                       helm-c-source-buffer-not-found)
            :buffer "*helm buffers*" :keymap helm-c-buffer-map))

(defalias 'helm-buffers+ 'helm-buffers-list
  "Preconfigured `helm' to list buffers.
It is an alias of `helm-buffers-list'.")

;;;###autoload
(defun helm-bbdb ()
  "Preconfigured `helm' for BBDB.

Needs BBDB.

http://bbdb.sourceforge.net/"
  (interactive)
  (helm-other-buffer 'helm-c-source-bbdb "*helm bbdb*"))

;;;###autoload
(defun helm-locate (arg)
  "Preconfigured `helm' for Locate.
Note: you can add locate options after entering pattern.
See 'man locate' for valid options.

You can specify a specific database with prefix argument ARG \(C-u\).
Many databases can be used: navigate and mark them.
See also `helm-locate-with-db'.

To create a user specific db, use
\"updatedb -l 0 -o db_path -U directory\".
Where db_path is a filename matched by
`helm-locate-db-file-regexp'."
  (interactive "P")
  (setq helm-ff-default-directory default-directory)
  (helm-locate-1 arg))

;;;###autoload
(defun helm-w3m-bookmarks ()
  "Preconfigured `helm' for w3m bookmark.

Needs w3m and emacs-w3m.

http://w3m.sourceforge.net/
http://emacs-w3m.namazu.org/"
  (interactive)
  (helm-other-buffer 'helm-c-source-w3m-bookmarks
                         "*helm w3m bookmarks*"))

;;;###autoload
(defun helm-firefox-bookmarks ()
  "Preconfigured `helm' for firefox bookmark.
You will have to enable html bookmarks in firefox:
open about:config in firefox and double click on this line to enable value \
to true:

user_pref(\"browser.bookmarks.autoExportHTML\", false);

You should have now:

user_pref(\"browser.bookmarks.autoExportHTML\", true);

After closing firefox, you will be able to browse you bookmarks.
"
  (interactive)
  (helm-other-buffer 'helm-c-source-firefox-bookmarks
                         "*Helm Firefox*"))

;;;###autoload
(defun helm-colors ()
  "Preconfigured `helm' for color."
  (interactive)
  (helm-other-buffer
   '(helm-c-source-colors helm-c-source-customize-face)
   "*helm colors*"))

;;;###autoload
(defun helm-bookmarks ()
  "Preconfigured `helm' for bookmarks."
  (interactive)
  (helm-other-buffer 'helm-c-source-bookmarks "*helm bookmarks*"))

;;;###autoload
(defun helm-c-pp-bookmarks ()
  "Preconfigured `helm' for bookmarks (pretty-printed)."
  (interactive)
  (helm-other-buffer '(helm-c-source-bookmarks-local
                           helm-c-source-bookmarks-su
                           helm-c-source-bookmarks-ssh)
                         "*helm pp bookmarks*"))

;;;###autoload
(defun helm-c-insert-latex-math ()
  "Preconfigured helm for latex math symbols completion."
  (interactive)
  (helm-other-buffer 'helm-c-source-latex-math "*helm latex*"))

;;;###autoload
(defun helm-register ()
  "Preconfigured `helm' for Emacs registers."
  (interactive)
  (helm-other-buffer 'helm-c-source-register "*helm register*"))

;;;###autoload
(defun helm-man-woman ()
  "Preconfigured `helm' for Man and Woman pages."
  (interactive)
  (helm-other-buffer 'helm-c-source-man-pages "*Helm man woman*"))

;;;###autoload
(defun helm-org-keywords ()
  "Preconfigured `helm' for org keywords."
  (interactive)
  (helm-other-buffer 'helm-c-source-org-keywords "*org keywords*"))

;;;###autoload
(defun helm-emms ()
  "Preconfigured `helm' for emms sources."
  (interactive)
  (helm :sources '(helm-c-source-emms-streams
                       helm-c-source-emms-files
                       helm-c-source-emms-dired)
            :buffer "*Helm Emms*"))

;;;###autoload
(defun helm-eev-anchors ()
  "Preconfigured `helm' for eev anchors."
  (interactive)
  (helm-other-buffer 'helm-c-source-eev-anchor "*Helm eev anchors*"))

;;;###autoload
(defun helm-bm-list ()
  "Preconfigured `helm' for visible bookmarks.

Needs bm.el

http://cvs.savannah.gnu.org/viewvc/*checkout*/bm/bm/bm.el"
  (interactive)
  (let ((helm-outline-using t))
    (helm-other-buffer 'helm-c-source-bm "*helm bm list*")))

;;;###autoload
(defun helm-timers ()
  "Preconfigured `helm' for timers."
  (interactive)
  (helm-other-buffer '(helm-c-source-absolute-time-timers
                           helm-c-source-idle-time-timers)
                         "*helm timers*"))

;;;###autoload
(defun helm-list-emacs-process ()
  "Preconfigured `helm' for emacs process."
  (interactive)
  (helm-other-buffer 'helm-c-source-emacs-process "*helm process*"))

;;;###autoload
(defun helm-occur ()
  "Preconfigured Helm for Occur source.
If region is active, search only in region,
otherwise search in whole buffer."
  (interactive)
  (let ((helm-compile-source-functions
         ;; rule out helm-match-plugin because the input is one regexp.
         (delq 'helm-compile-source--match-plugin
               (copy-sequence helm-compile-source-functions))))
    (helm :sources 'helm-c-source-occur
              :buffer "*Helm Occur*"
              :history 'helm-c-grep-history)))

;;;###autoload
(defun helm-browse-code ()
  "Preconfigured helm to browse code."
  (interactive)
  (helm :sources 'helm-c-source-browse-code
            :buffer "*helm browse code*"
            :default (thing-at-point 'symbol)))

;;;###autoload
(defun helm-org-headlines ()
  "Preconfigured helm to show org headlines."
  (interactive)
  (helm-other-buffer 'helm-c-source-org-headline "*org headlines*"))

;;;###autoload
(defun helm-regexp ()
  "Preconfigured helm to build regexps.
`query-replace-regexp' can be run from there against found regexp."
  (interactive)
  (save-restriction
    (let ((helm-compile-source-functions
           ;; rule out helm-match-plugin because the input is one regexp.
           (delq 'helm-compile-source--match-plugin
                 (copy-sequence helm-compile-source-functions))))
      (when (and (helm-region-active-p)
                 ;; Don't narrow to region if buffer is already narrowed.
                 (not (helm-current-buffer-narrowed-p)))
        (narrow-to-region (region-beginning) (region-end)))
      (helm :sources helm-c-source-regexp
                :buffer "*helm regexp*"
                :prompt "Regexp: "
                :history 'helm-build-regexp-history))))

;;;###autoload
(defun helm-c-copy-files-async ()
  "Preconfigured helm to copy file list FLIST to DEST asynchronously."
  (interactive)
  (let* ((flist (helm-c-read-file-name
                 "Copy File async: "
                 :marked-candidates t))
         (dest  (helm-c-read-file-name
                 "Copy File async To: "
                 :preselect (car flist)
                 :initial-input (car helm-ff-history)
                 :history (helm-find-files-history :comp-read nil))))
    (helm-c-copy-async-with-log flist dest)))

;;;###autoload
(defun helm-find-files (arg)
  "Preconfigured `helm' for helm implementation of `find-file'.
Called with a prefix arg show history if some.
Don't call it from programs, use `helm-find-files-1' instead.
This is the starting point for nearly all actions you can do on files."
  (interactive "P")
  (let ((any-input (if (and arg helm-ff-history)
                       (helm-find-files-history)
                       (helm-find-files-initial-input)))
        (presel    (buffer-file-name (current-buffer))))
    (when (and (eq major-mode 'org-agenda-mode)
               org-directory
               (not any-input))
      (setq any-input (expand-file-name org-directory)))
    (set-text-properties 0 (length any-input) nil any-input)
    (if any-input
        (helm-find-files-1 any-input)
        (setq any-input (expand-file-name (helm-c-current-directory)))
        (helm-find-files-1
         any-input (if helm-ff-transformer-show-only-basename
                       (and presel (helm-c-basename presel))
                       presel)))))

;;;###autoload
(defun helm-write-file ()
  "Preconfigured `helm' providing completion for `write-file'."
  (interactive)
  (let ((helm-mp-highlight-delay nil))
    (helm :sources 'helm-c-source-write-file
              :input (expand-file-name default-directory)
              :prompt "Write buffer to file: "
              :buffer "*Helm write file*")))

;;;###autoload
(defun helm-insert-file ()
  "Preconfigured `helm' providing completion for `insert-file'."
  (interactive)
  (let ((helm-mp-highlight-delay nil))
    (helm :sources 'helm-c-source-insert-file
              :input (expand-file-name default-directory)
              :prompt "Insert file: "
              :buffer "*Helm insert file*")))

;;;###autoload
(defun helm-dired-rename-file ()
  "Preconfigured `helm' to rename files from dired."
  (interactive)
  (helm-dired-do-action-on-file :action 'rename))

;;;###autoload
(defun helm-dired-copy-file ()
  "Preconfigured `helm' to copy files from dired."
  (interactive)
  (helm-dired-do-action-on-file :action 'copy))

;;;###autoload
(defun helm-dired-symlink-file ()
  "Preconfigured `helm' to symlink files from dired."
  (interactive)
  (helm-dired-do-action-on-file :action 'symlink))

;;;###autoload
(defun helm-dired-hardlink-file ()
  "Preconfigured `helm' to hardlink files from dired."
  (interactive)
  (helm-dired-do-action-on-file :action 'hardlink))

;;;###autoload
(defun helm-do-grep ()
  "Preconfigured helm for grep.
Contrarily to Emacs `grep' no default directory is given, but
the full path of candidates in ONLY.
That allow to grep different files not only in `default-directory' but anywhere
by marking them (C-<SPACE>). If one or more directory is selected
grep will search in all files of these directories.
You can use also wildcard in the base name of candidate.
If a prefix arg is given use the -r option of grep.
The prefix arg can be passed before or after start.
See also `helm-do-grep-1'."
  (interactive)
  (let ((only    (helm-c-read-file-name
                  "Search in file(s): "
                  :marked-candidates t
                  :preselect (or (dired-get-filename nil t)
                                 (buffer-file-name (current-buffer)))))
        (prefarg (or current-prefix-arg helm-current-prefix-arg)))
    (helm-do-grep-1 only prefarg)))

;;;###autoload
(defun helm-do-zgrep ()
  "Preconfigured helm for zgrep."
  (interactive)
  (let ((prefarg (or current-prefix-arg helm-current-prefix-arg))
        (ls (helm-c-read-file-name
             "Search in file(s): "
             :marked-candidates t
             :preselect (or (dired-get-filename nil t)
                            (buffer-file-name (current-buffer))))))
    (helm-ff-zgrep-1 ls prefarg)))

;;;###autoload
(defun helm-do-pdfgrep ()
  "Preconfigured helm for pdfgrep."
  (interactive)
  (let ((only (helm-c-read-file-name
               "Search in file(s): "
               :marked-candidates t
               :test #'(lambda (file)
                         (or (string= (file-name-extension file) "pdf")
                             (string= (file-name-extension file) "PDF")
                             (file-directory-p file)))
               :preselect (or (dired-get-filename nil t)
                              (buffer-file-name (current-buffer)))))
        (helm-c-grep-default-function 'helm-c-pdfgrep-init))
    (helm-do-pdfgrep-1 only)))

;;;###autoload
(defun helm-c-etags-select (arg)
  "Preconfigured helm for etags.
Called with one prefix arg use symbol at point as initial input.
Called with two prefix arg reinitialize cache.
If tag file have been modified reinitialize cache."
  (interactive "P")
  (let ((tag  (helm-c-etags-get-tag-file))
        (init (and (equal arg '(4)) (thing-at-point 'symbol)))
        (helm-quit-if-no-candidate t)
        (helm-execute-action-at-once-if-one t)
        (helm-compile-source-functions
         (if helm-c-etags-use-regexp-search
             ;; rule out helm-match-plugin because the input is one regexp.
             (delq 'helm-compile-source--match-plugin
                   (copy-sequence helm-compile-source-functions))
             helm-compile-source-functions)))
    (when (or (equal arg '(16))
              (and helm-c-etags-mtime-alist
                   (helm-c-etags-file-modified-p tag)))
      (remhash tag helm-c-etags-cache))
    (if (and tag (file-exists-p tag))
        (helm :sources 'helm-c-source-etags-select
                  :keymap helm-c-etags-map
                  :input init
                  :buffer "*helm etags*")
        (message "Error: No tag file found, please create one with etags shell command."))))


;;;###autoload
(defun helm-M-x ()
  "Preconfigured `helm' for Emacs commands.
It is `helm' replacement of regular `M-x' `execute-extended-command'."
  (interactive)
  (let* (in-help
         help-cand
         special-display-buffer-names
         special-display-regexps
         helm-persistent-action-use-special-display
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
                     ;; Don't kill it as it is helm-current-buffer.
                     (unless (equal hbuf helm-current-buffer)
                       (kill-buffer hbuf))
                     (setq in-help nil))
                   ;; Be sure helm-current-buffer
                   ;; have not a dedicated window.
                   (set-window-dedicated-p
                    (get-buffer-window helm-current-buffer) nil)
                   (describe-function (intern candidate))
                   (message nil) ; Erase the new stupid message Type "q"[...]
                   (setq in-help t))
               (setq help-cand candidate))))
      (let* ((command (helm-comp-read
                       "M-x " obarray
                       :test 'commandp
                       :requires-pattern helm-M-x-requires-pattern
                       :name "Emacs Commands"
                       :buffer "*helm M-x*"
                       :persistent-action 'pers-help
                       :persistent-help "Describe this command"
                       :history history
                       :must-match t
                       :candidates-in-buffer t
                       :fc-transformer 'helm-M-x-transformer))
             (sym-com (intern command)))
        (unless current-prefix-arg
          (setq current-prefix-arg helm-current-prefix-arg))
        ;; Avoid having `this-command' set to *exit-minibuffer.
        (setq this-command sym-com)
        (call-interactively sym-com)
        (setq extended-command-history
              (cons command (delete command history)))))))

;;;###autoload
(defun helm-manage-advice ()
  "Preconfigured `helm' to disable/enable function advices."
  (interactive)
  (helm-other-buffer 'helm-c-source-advice "*helm advice*"))

;;;###autoload
(defun helm-bookmark-ext ()
  "Preconfigured `helm' for bookmark-extensions sources.
Needs bookmark-ext.el:
<http://mercurial.intuxication.org/hg/emacs-bookmark-extension>.
Contain also `helm-c-source-google-suggest'."
  (interactive)
  (helm
   :sources
   '(helm-c-source-bookmark-files&dirs
     helm-c-source-bookmark-w3m
     helm-c-source-google-suggest
     helm-c-source-bmkext-addressbook
     helm-c-source-bookmark-gnus
     helm-c-source-bookmark-info
     helm-c-source-bookmark-man
     helm-c-source-bookmark-images
     helm-c-source-bookmark-su-files&dirs
     helm-c-source-bookmark-ssh-files&dirs)
   :prompt "SearchBookmark: "
   :buffer "*helm bmkext*"))

;;;###autoload
(defun helm-simple-call-tree ()
  "Preconfigured `helm' for simple-call-tree. List function relationships.

Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el"
  (interactive)
  (helm-other-buffer
   '(helm-c-source-simple-call-tree-functions-callers
     helm-c-source-simple-call-tree-callers-functions)
   "*helm simple-call-tree*"))

;;;###autoload
(defun helm-mark-ring ()
  "Preconfigured `helm' for `helm-c-source-mark-ring'."
  (interactive)
  (helm :sources 'helm-c-source-mark-ring))

;;;###autoload
(defun helm-global-mark-ring ()
  "Preconfigured `helm' for `helm-c-source-global-mark-ring'."
  (interactive)
  (helm :sources 'helm-c-source-global-mark-ring))

;;;###autoload
(defun helm-all-mark-rings ()
  "Preconfigured `helm' for `helm-c-source-global-mark-ring' and \
`helm-c-source-mark-ring'."
  (interactive)
  (helm :sources '(helm-c-source-mark-ring
                       helm-c-source-global-mark-ring)))

;;;###autoload
(defun helm-yaoddmuse-emacswiki-edit-or-view ()
  "Preconfigured `helm' to edit or view EmacsWiki page.

Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el"
  (interactive)
  (helm :sources 'helm-c-source-yaoddmuse-emacswiki-edit-or-view))

;;;###autoload
(defun helm-yaoddmuse-emacswiki-post-library ()
  "Preconfigured `helm' to post library to EmacsWiki.

Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el"
  (interactive)
  (helm :sources 'helm-c-source-yaoddmuse-emacswiki-post-library))

;;;###autoload
(defun helm-eval-expression (arg)
  "Preconfigured helm for `helm-c-source-evaluation-result'."
  (interactive "P")
  (helm :sources 'helm-c-source-evaluation-result
            :input (when arg (thing-at-point 'sexp))
            :buffer "*helm eval*"
            :history 'helm-eval-expression-input-history
            :keymap helm-eval-expression-map))

;;;###autoload
(defun helm-eval-expression-with-eldoc ()
  "Preconfigured helm for `helm-c-source-evaluation-result' with `eldoc' support. "
  (interactive)
  (declare (special eldoc-idle-delay))
  (let ((timer (run-with-idle-timer eldoc-idle-delay
                                    'repeat 'helm-eldoc-show-in-eval))
        (minibuffer-completing-symbol t) ; Enable lisp completion.
        (completion-cycle-threshold t))  ; Always cycle, no pesty completion buffer (emacs24 only).
    (unwind-protect
         (minibuffer-with-setup-hook
             'helm-eldoc-store-minibuffer
           (call-interactively 'helm-eval-expression))
      (and timer (cancel-timer timer))
      (setq helm-eldoc-active-minibuffers-list
            (cdr helm-eldoc-active-minibuffers-list)))))

;;;###autoload
(defun helm-calcul-expression ()
  "Preconfigured helm for `helm-c-source-calculation-result'."
  (interactive)
  (helm-other-buffer 'helm-c-source-calculation-result "*helm calcul*"))

;;;###autoload
(defun helm-surfraw (pattern engine)
  "Preconfigured `helm' to search PATTERN with search ENGINE."
  (interactive (list (read-string "SearchFor: "
                                  nil 'helm-surfraw-input-history)
                     (helm-comp-read
                      "Engine: "
                      (helm-c-build-elvi-list)
                      :must-match t
                      :name "Surfraw Search Engines"
                      :history helm-surfraw-engines-history)))
  (let* ((engine-nodesc (car (split-string engine)))
         (url (with-temp-buffer
                (apply 'call-process "surfraw" nil t nil
		       ;;JAVE
                       (append  (list engine-nodesc "-p") (split-string pattern)))
                (replace-regexp-in-string
                 "\n" "" (buffer-string))))
         (browse-url-browser-function (or helm-surfraw-default-browser-function
                                          browse-url-browser-function)))
    (if (string= engine-nodesc "W")
        (helm-c-browse-url helm-c-home-url)
        (helm-c-browse-url url)
        (setq helm-surfraw-engines-history
              (cons engine (delete engine helm-surfraw-engines-history))))))

;;;###autoload
(defun helm-call-source ()
  "Preconfigured `helm' to call helm source."
  (interactive)
  (helm :sources 'helm-c-source-call-source
            :buffer helm-source-select-buffer))

;;;###autoload
(defun helm-execute-helm-command ()
  "Preconfigured `helm' to execute preconfigured `helm'."
  (interactive)
  (helm-other-buffer 'helm-c-source-helm-commands
                         "*helm commands*"))

;;;###autoload
(defun helm-create (&optional string initial-input)
  "Preconfigured `helm' to do many create actions from STRING.
See also `helm-create--actions'."
  (interactive)
  (setq string (or string (read-string "Create Helm: " initial-input)))
  (helm :sources '(((name . "Helm Create")
                        (header-name . (lambda (_) (format "Action for \"%s\"" string)))
                        (candidates . helm-create--actions)
                        (candidate-number-limit)
                        (action . (lambda (func) (funcall func string)))))))

;;;###autoload
(defun helm-top ()
  "Preconfigured `helm' for top command."
  (interactive)
  (let ((helm-samewindow t)
        (helm-enable-shortcuts)
        (helm-display-function 'helm-default-display-buffer)
        (helm-candidate-number-limit 9999))
    (save-window-excursion
      (delete-other-windows)
      (helm-other-buffer 'helm-c-source-top "*helm top*"))))

;;;###autoload
(defun helm-select-xfont ()
  "Preconfigured `helm' to select Xfont."
  (interactive)
  (helm-other-buffer 'helm-c-source-xfonts "*helm select* xfont"))

;;;###autoload
(defun helm-world-time ()
  "Preconfigured `helm' to show world time."
  (interactive)
  (helm-other-buffer 'helm-c-source-time-world "*helm world time*"))

;;;###autoload
(defun helm-apt (arg)
  "Preconfigured `helm' : frontend of APT package manager.
With a prefix arg reload cache."
  (interactive "P")
  (let ((query (read-string "Search Package: " nil 'helm-c-apt-input-history)))
    (when arg (helm-c-apt-refresh))
    (helm :sources 'helm-c-source-apt
              :prompt "Search Package: "
              :input query
              :history 'helm-c-apt-input-history)))

;;;###autoload
(defun helm-esh-pcomplete ()
  "Preconfigured helm to provide helm completion in eshell."
  (interactive)
  (let* ((helm-quit-if-no-candidate t)
         (helm-execute-action-at-once-if-one t)
         (target (thing-at-point 'symbol))
         (end (point))
         (beg (or (and target (- end (length target)))
                  ;; Nothing at point.
                  (progn (insert " ") (point)))))
    (setq helm-ec-target (or target " "))
    (with-helm-show-completion beg end
      (helm :sources 'helm-c-source-esh
                :buffer "*helm pcomplete*"
                :input (helm-ff-set-pattern ; Handle tramp filenames.
                        (car (last (ignore-errors ; Needed in lisp symbols completion.
                                     (pcomplete-parse-arguments)))))))))

;;;###autoload
(defun helm-eshell-history ()
  "Preconfigured helm for eshell history."
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
         (with-helm-show-completion beg end
           (helm :sources 'helm-c-source-eshell-history
                     :buffer "*Eshell history*"
                     :input input))
      (when (and flag-empty
                 (looking-back " "))
        (delete-char -1)))))

;;;###autoload
(defun helm-c-run-external-command (program)
  "Preconfigured `helm' to run External PROGRAM asyncronously from Emacs.
If program is already running exit with error.
You can set your own list of commands with
`helm-c-external-commands-list'."
  (interactive (list
                (helm-comp-read
                 "RunProgram: "
                 (helm-c-external-commands-list-1 'sort)
                 :must-match t
                 :name "External Commands"
                 :history helm-external-command-history)))
  (helm-run-or-raise program)
  (setq helm-external-command-history
        (cons program (delete program
                              (loop for i in helm-external-command-history
                                    when (executable-find i) collect i)))))

;;;###autoload
(defun helm-ratpoison-commands ()
  "Preconfigured `helm' to execute ratpoison commands."
  (interactive)
  (helm-other-buffer 'helm-c-source-ratpoison-commands
                         "*helm ratpoison commands*"))

;;;###autoload
(defun helm-ucs ()
  "Preconfigured helm for `ucs-names' math symbols."
  (interactive)
  (helm :sources 'helm-c-source-ucs
            :keymap  helm-c-ucs-map))

;;;###autoload
(defun helm-c-apropos ()
  "Preconfigured helm to describe commands, functions, variables and faces."
  (interactive)
  (let ((default (thing-at-point 'symbol)))
    (helm :sources
              `(((name . "Commands")
                 (init . (lambda ()
                           (helm-c-apropos-init 'commandp ,default)))
                 (persistent-action . helm-lisp-completion-persistent-action)
                 (persistent-help . "Show brief doc in mode-line")
                 (candidates-in-buffer)
                 (action . (lambda (candidate)
                             (describe-function (intern candidate)))))
                ((name . "Functions")
                 (init . (lambda ()
                           (helm-c-apropos-init #'(lambda (x) (and (fboundp x)
                                                                       (not (commandp x))))
                                                    ,default)))
                 (persistent-action . helm-lisp-completion-persistent-action)
                 (persistent-help . "Show brief doc in mode-line")
                 (candidates-in-buffer)
                 (action . (lambda (candidate)
                             (describe-function (intern candidate)))))
                ((name . "Variables")
                 (init . (lambda ()
                           (helm-c-apropos-init 'boundp ,default)))
                 (persistent-action . helm-lisp-completion-persistent-action)
                 (persistent-help . "Show brief doc in mode-line")
                 (candidates-in-buffer)
                 (action . (lambda (candidate)
                             (describe-variable (intern candidate)))))
                ((name . "Faces")
                 (init . (lambda ()
                           (helm-c-apropos-init 'facep ,default)))
                 (persistent-action . helm-lisp-completion-persistent-action)
                 (persistent-help . "Show brief doc in mode-line")
                 (candidates-in-buffer)
                 (filtered-candidate-transformer . (lambda (candidates source)
                                                     (loop for c in candidates
                                                           collect (propertize c 'face (intern c)))))
                 (action . (lambda (candidate)
                             (describe-face (intern candidate)))))
                ((name . "Helm attributes")
                 (candidates . (lambda ()
                                 (mapcar 'symbol-name helm-additional-attributes)))
                 (action . (lambda (candidate)
                             (with-output-to-temp-buffer "*Help*"
                               (princ (get (intern candidate) 'helm-attrdoc))))))))))

;;;###autoload
(defun helm-xrandr-set ()
  (interactive)
  (helm :sources 'helm-c-source-xrandr-change-resolution
            :buffer "*helm xrandr*"))


;;; Unit tests are now in ../developer-tools/unit-test-helm-config.el.

(provide 'helm-config)

;; Local Variables:
;; coding: utf-8
;; End:

;;; helm-config.el ends here
