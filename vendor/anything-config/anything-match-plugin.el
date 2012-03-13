;;; anything-match-plugin.el --- Multiple regexp matching methods for anything

;; Author: rubikitch <rubikitch@ruby-lang.org>

;; Maintainers: rubikitch <rubikitch@ruby-lang.org>
;;              Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; Copyright (C) 2008~2012, rubikitch, all rights reserved.
;; Copyright (C) 2011~2012, Thierry Volpiatto, all rights reserved.

;; Keywords: anything, matching
;; X-URL: <http://repo.or.cz/w/anything-config.git>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Auto documentation
;;  ------------------
;;
;;  * User variables
;; [EVAL] (autodoc-document-lisp-buffer :type 'user-variable :prefix "anything-mp" :var-value t)
;; `anything-mp-matching-method'
;; Default Value: multi3
;; `anything-mp-highlight-delay'
;; Default Value: 0.7
;; `anything-mp-highlight-threshold'
;; Default Value: 2
;;
;;  * Internal variables
;; [EVAL] (autodoc-document-lisp-buffer :type 'internal-variable :prefix "anything-mp" :var-value t)
;; `anything-mp-default-match-functions'
;; Default Value:	(anything-mp-exact-match anything-mp-3-match) 
;; `anything-mp-default-search-functions'
;; Default Value:	(anything-mp-exact-search anything-mp-3-search) 
;; `anything-mp-default-search-backward-functions'
;; Default Value:	(anything-mp-exact-search-backward anything-mp-3-search-backward) 
;; `anything-mp-space-regexp'
;; Default Value: "[\\ ] "
;; `anything-mp-exact-pattern-str'
;; Default Value: "autod"
;; `anything-mp-exact-pattern-real'
;; Default Value: "\nautod\n"
;; `anything-mp-prefix-pattern-str'
;; Default Value: nil
;; `anything-mp-prefix-pattern-real'
;; Default Value: nil
;; `anything-mp-1-pattern-str'
;; Default Value: nil
;; `anything-mp-1-pattern-real'
;; Default Value: nil
;; `anything-mp-2-pattern-str'
;; Default Value: nil
;; `anything-mp-2-pattern-real'
;; Default Value: nil
;; `anything-mp-3-pattern-str'
;; Default Value: "autod"
;; `anything-mp-3-pattern-list'
;; Default Value:	((identity . "autod")) 
;; `anything-mp-initial-highlight-delay'
;; Default Value: nil
;;
;;  * Anything match plugin Functions
;; [EVAL] (autodoc-document-lisp-buffer :type 'function :prefix "anything-mp")
;; `anything-mp-set-matching-method'
;; `anything-mp-make-regexps'
;; `anything-mp-1-make-regexp'
;; `anything-mp-exact-get-pattern'
;; `anything-mp-exact-match'
;; `anything-mp-exact-search'
;; `anything-mp-exact-search-backward'
;; `anything-mp-prefix-get-pattern'
;; `anything-mp-prefix-match'
;; `anything-mp-prefix-search'
;; `anything-mp-prefix-search-backward'
;; `anything-mp-1-get-pattern'
;; `anything-mp-1-match'
;; `anything-mp-1-search'
;; `anything-mp-1-search-backward'
;; `anything-mp-2-get-pattern'
;; `anything-mp-2-match'
;; `anything-mp-2-search'
;; `anything-mp-2-search-backward'
;; `anything-mp-3-get-patterns'
;; `anything-mp-3-get-patterns-internal'
;; `anything-mp-3-match'
;; `anything-mp-3-search-base'
;; `anything-mp-3-search'
;; `anything-mp-3-search-backward'
;; `anything-mp-3p-match'
;; `anything-mp-3p-search'
;; `anything-mp-3p-search-backward'
;; `anything-mp-highlight-match'
;; `anything-mp-highlight-region'
;; `anything-mp-highlight-match-internal'

;;  *** END auto-documentation

;;; Commentary:

;; Change anything.el matching algorithm humanely.
;; It gives anything.el search refinement functionality.
;; exact match -> prefix match -> multiple regexp match

;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `anything-grep-candidates-fast-directory-regexp'
;;    *Directory regexp where a RAM disk (or tmpfs) is mounted.
;;    default = nil

;; A query of multiple regexp match is space-delimited string.
;; Anything displays candidates which matches all the regexps.
;; A regexp with "!" prefix means not matching the regexp.
;; To include spaces to a regexp, prefix "\" before space,
;; it is controlled by `anything-mp-space-regexp' variable.

;; If multiple regexps are specified, first one also tries to match the source name.
;; If you want to disable this feature, evaluate
;;   (setq anything-mp-match-source-name nil) .
;; NOTE: This is obsolete and disabled in anything versions >= 1.3.7

;; This file highlights patterns like `occur'. Note that patterns
;; longer than `anything-mp-highlight-threshold' are highlighted. And
;; region out of screen is highlighted after
;; `anything-mp-highlight-delay' seconds.
;;
;; Highlight in Emacs is time-consuming process for slow computers. To
;; disable it is to set nil to `anything-mp-highlight-delay'.

;; anything-match-plugin is enable by default in anything.
;; To disable/enable it use M-x anything-c-toggle-match-plugin.

;;; Code:

(require 'anything)
(require 'cl)


;;;; Match-plugin

;; Internal
(defvar anything-mp-default-match-functions nil)
(defvar anything-mp-default-search-functions nil)
(defvar anything-mp-default-search-backward-functions nil)

(defun anything-mp-set-matching-method (var key)
  "Default function to set matching methods in anything match plugin."
  (set-default var key)
  (case (symbol-value var)
    (multi1 (setq anything-mp-default-match-functions
                  '(anything-mp-exact-match anything-mp-1-match)
                  anything-mp-default-search-functions
                  '(anything-mp-exact-search anything-mp-1-search)
                  anything-mp-default-search-backward-functions
                  '(anything-mp-exact-search-backward
                    anything-mp-1-search-backward)))
    (multi2 (setq anything-mp-default-match-functions
                  '(anything-mp-exact-match anything-mp-2-match)
                  anything-mp-default-search-functions
                  '(anything-mp-exact-search anything-mp-2-search)
                  anything-mp-default-search-backward-functions
                  '(anything-mp-exact-search-backward
                    anything-mp-2-search-backward)))
    (multi3 (setq anything-mp-default-match-functions
                  '(anything-mp-exact-match anything-mp-3-match)
                  anything-mp-default-search-functions
                  '(anything-mp-exact-search anything-mp-3-search)
                  anything-mp-default-search-backward-functions
                  '(anything-mp-exact-search-backward
                    anything-mp-3-search-backward)))
    (multi3p (setq anything-mp-default-match-functions
                   '(anything-mp-exact-match anything-mp-3p-match)
                   anything-mp-default-search-functions
                   '(anything-mp-exact-search anything-mp-3p-search)
                   anything-mp-default-search-backward-functions
                   '(anything-mp-exact-search-backward
                     anything-mp-3p-search-backward)))
    (t (error "Unknow value: %s" anything-mp-matching-method))))

(defgroup anything-match-plugin nil
  "Anything match plugin."
  :group 'anything)

(defcustom anything-mp-matching-method 'multi3
  "Matching method for anything match plugin.
You can set here different methods to match candidates in anything.
Here are the possible value of this symbol and their meaning:
- multi1: Respect order, prefix of pattern must match.
- multi2: Same but with partial match.
- multi3: The best, multiple regexp match, allow negation.
- multi3p: Same but prefix must match.
Default is multi3."
  :type  '(radio :tag "Matching methods for anything"
           (const :tag "Multiple regexp 1 ordered with prefix match"         multi1)
           (const :tag "Multiple regexp 2 ordered with partial match"        multi2)
           (const :tag "Multiple regexp 3 matching no order, partial, best." multi3)
           (const :tag "Multiple regexp 3p matching with prefix match"       multi3p))
  :set   'anything-mp-set-matching-method
  :group 'anything-match-plugin)

(defface anything-match
    '((t (:inherit match)))
  "Face used to highlight matches."
  :group 'anything-match-plugin)

(defcustom anything-mp-highlight-delay 0.7
  "Highlight matches with `anything-match' face after this many seconds.
 If nil, no highlight. "
  :type  'integer
  :group 'anything-match-plugin)

(defcustom anything-mp-highlight-threshold 2
  "Minimum length of pattern to highlight.
The smaller  this value is, the slower highlight is."
  :type  'integer
  :group 'anything-match-plugin)



;;; Build regexps
;;
;;
(defvar anything-mp-space-regexp "[\\ ] "
  "Regexp to represent space itself in multiple regexp match.")

(defun anything-mp-make-regexps (pattern)
  "Split PATTERN if it contain spaces and return resulting list.
If spaces in PATTERN are escaped, don't split at this place.
i.e \"foo bar\"=> (\"foo\" \"bar\")
but \"foo\ bar\"=> (\"foobar\")."
  (if (string= pattern "")
      '("")
      (loop for s in (split-string
                      (replace-regexp-in-string anything-mp-space-regexp
                                                "\000\000" pattern)
                      " " t)
            collect (replace-regexp-in-string "\000\000" " " s))))

(defun anything-mp-1-make-regexp (pattern)
  "Replace spaces in PATTERN with \"\.*\"."
  (mapconcat 'identity (anything-mp-make-regexps pattern) ".*"))


;;; Exact match.
;;
;;
;; Internal.
(defvar anything-mp-exact-pattern-str nil)
(defvar anything-mp-exact-pattern-real nil)

(defun anything-mp-exact-get-pattern (pattern)
  (unless (equal pattern anything-mp-exact-pattern-str)
    (setq anything-mp-exact-pattern-str pattern
          anything-mp-exact-pattern-real (concat "\n" pattern "\n")))
  anything-mp-exact-pattern-real)


(defun anything-mp-exact-match (str &optional pattern)
  (string= str (or pattern anything-pattern)))

(defun anything-mp-exact-search (pattern &rest ignore)
  (and (search-forward (anything-mp-exact-get-pattern pattern) nil t)
       (forward-line -1)))

(defun anything-mp-exact-search-backward (pattern &rest ignore)
  (and (search-backward (anything-mp-exact-get-pattern pattern) nil t)
       (forward-line 1)))


;;; Prefix match
;;
;;
;; Internal
(defvar anything-mp-prefix-pattern-str nil)
(defvar anything-mp-prefix-pattern-real nil)

(defun anything-mp-prefix-get-pattern (pattern)
  (unless (equal pattern anything-mp-prefix-pattern-str)
    (setq anything-mp-prefix-pattern-str pattern
          anything-mp-prefix-pattern-real (concat "\n" pattern)))
  anything-mp-prefix-pattern-real)

(defun anything-mp-prefix-match (str &optional pattern)
  (setq pattern (or pattern anything-pattern))
  (let ((len (length pattern)))
    (and (<= len (length str))
         (string= (substring str 0 len) pattern ))))

(defun anything-mp-prefix-search (pattern &rest ignore)
  (search-forward (anything-mp-prefix-get-pattern pattern) nil t))

(defun anything-mp-prefix-search-backward (pattern &rest ignore)
  (and (search-backward (anything-mp-prefix-get-pattern pattern) nil t)
       (forward-line 1)))


;;; Multiple regexp patterns 1 (order is preserved / prefix).
;;
;;
;; Internal
(defvar anything-mp-1-pattern-str nil)
(defvar anything-mp-1-pattern-real nil)

(defun anything-mp-1-get-pattern (pattern)
  (unless (equal pattern anything-mp-1-pattern-str)
    (setq anything-mp-1-pattern-str pattern
          anything-mp-1-pattern-real
          (concat "^" (anything-mp-1-make-regexp pattern))))
  anything-mp-1-pattern-real)

(defun* anything-mp-1-match (str &optional (pattern anything-pattern))
  (string-match (anything-mp-1-get-pattern pattern) str))

(defun anything-mp-1-search (pattern &rest ignore)
  (re-search-forward (anything-mp-1-get-pattern pattern) nil t))

(defun anything-mp-1-search-backward (pattern &rest ignore)
  (re-search-backward (anything-mp-1-get-pattern pattern) nil t))


;;; Multiple regexp patterns 2 (order is preserved / partial).
;;
;;
;; Internal
(defvar anything-mp-2-pattern-str nil)
(defvar anything-mp-2-pattern-real nil)

(defun anything-mp-2-get-pattern (pattern)
  (unless (equal pattern anything-mp-2-pattern-str)
    (setq anything-mp-2-pattern-str pattern
          anything-mp-2-pattern-real
          (concat "^.*" (anything-mp-1-make-regexp pattern))))
  anything-mp-2-pattern-real)

(defun* anything-mp-2-match (str &optional (pattern anything-pattern))
  (string-match (anything-mp-2-get-pattern pattern) str))

(defun anything-mp-2-search (pattern &rest ignore)
  (re-search-forward (anything-mp-2-get-pattern pattern) nil t))

(defun anything-mp-2-search-backward (pattern &rest ignore)
  (re-search-backward (anything-mp-2-get-pattern pattern) nil t))


;;; Multiple regexp patterns 3 (permutation).
;;
;;
;; Internal
(defvar anything-mp-3-pattern-str nil)
(defvar anything-mp-3-pattern-list nil)

(defun anything-mp-3-get-patterns (pattern)
  "Return `anything-mp-3-pattern-list', a list of predicate/regexp cons cells.
e.g ((identity . \"foo\") (identity . \"bar\")).
This is done only if `anything-mp-3-pattern-str' is same as PATTERN."
  (unless (equal pattern anything-mp-3-pattern-str)
    (setq anything-mp-3-pattern-str pattern
          anything-mp-3-pattern-list
          (anything-mp-3-get-patterns-internal pattern)))
  anything-mp-3-pattern-list)

(defun anything-mp-3-get-patterns-internal (pattern)
  "Return a list of predicate/regexp cons cells.
e.g ((identity . \"foo\") (identity . \"bar\"))."
  (unless (string= pattern "")
    (loop for pat in (anything-mp-make-regexps pattern)
          collect (if (string= "!" (substring pat 0 1))
                      (cons 'not (substring pat 1))
                      (cons 'identity pat)))))

(defun anything-mp-3-match (str &optional pattern)
  "Check if PATTERN match STR.
When PATTERN contain a space, it is splitted and matching is done
with the several resulting regexps against STR.
e.g \"bar foo\" will match \"foobar\" and \"barfoo\".
Argument PATTERN, a string, is transformed in a list of
cons cell with `anything-mp-3-get-patterns' if it contain a space.
e.g \"foo bar\"=>((identity . \"foo\") (identity . \"bar\")).
Then each predicate of cons cell(s) is called with regexp of same
cons cell against STR (a candidate).
i.e (identity (string-match \"foo\" \"foo bar\")) => t."
  (let ((pat (anything-mp-3-get-patterns (or pattern anything-pattern))))
    (loop for (predicate . regexp) in pat
          always (funcall predicate (string-match regexp str)))))

(defun anything-mp-3-search-base (pattern searchfn1 searchfn2)
  (loop with pat = (if (stringp pattern)
                       (anything-mp-3-get-patterns pattern)
                       pattern)
        while (funcall searchfn1 (or (cdar pat) "") nil t)
        for bol = (point-at-bol)
        for eol = (point-at-eol)
        if (loop for (pred . str) in (cdr pat) always
                 (progn (goto-char bol)
                        (funcall pred (funcall searchfn2 str eol t))))
        do (goto-char eol) and return t
        else do (goto-char eol)
        finally return nil))

(defun anything-mp-3-search (pattern &rest ignore)
  (when (stringp pattern)
    (setq pattern (anything-mp-3-get-patterns pattern)))
  (anything-mp-3-search-base
   pattern 're-search-forward 're-search-forward))
  
(defun anything-mp-3-search-backward (pattern &rest ignore)
  (when (stringp pattern)
    (setq pattern (anything-mp-3-get-patterns pattern)))
  (anything-mp-3-search-base
   pattern 're-search-backward 're-search-backward))

  
;;; mp-3p- (multiple regexp pattern 3 with prefix search)
;;
;;
(defun anything-mp-3p-match (str &optional pattern)
  "Check if PATTERN match STR.
Same as `anything-mp-3-match' but more strict, matching against prefix also.
e.g \"bar foo\" will match \"barfoo\" but not \"foobar\" contrarily to
`anything-mp-3-match'."
  (let* ((pat (anything-mp-3-get-patterns (or pattern anything-pattern)))
         (first (car pat)))
    (and (funcall (car first) (anything-mp-prefix-match str (cdr first)))
         (loop for (predicate . regexp) in (cdr pat)
               always (funcall predicate (string-match regexp str))))))
  
(defun anything-mp-3p-search (pattern &rest ignore)
  (when (stringp pattern)
    (setq pattern (anything-mp-3-get-patterns pattern)))
  (anything-mp-3-search-base
   pattern 'anything-mp-prefix-search 're-search-forward))

(defun anything-mp-3p-search-backward (pattern &rest ignore)
  (when (stringp pattern)
    (setq pattern (anything-mp-3-get-patterns pattern)))
  (anything-mp-3-search-base
   pattern 'anything-mp-prefix-search-backward 're-search-backward))


;;; source compiler
;;
;;
(defun anything-compile-source--match-plugin (source)
  (let ((searchers (if (assoc 'search-from-end source)
                       anything-mp-default-search-backward-functions
                       anything-mp-default-search-functions)))
    `(,(if (or (assoc 'candidates-in-buffer source)
               (equal '(identity) (assoc-default 'match source)))
           '(match identity)
           `(match ,@anything-mp-default-match-functions
                   ,@(assoc-default 'match source)))
       (search ,@searchers
               ,@(assoc-default 'search source))
       ,@source)))
(add-to-list 'anything-compile-source-functions 'anything-compile-source--match-plugin t)


;;; Highlight matches.
;;
;;
(defun anything-mp-highlight-match ()
  "Highlight matches after `anything-mp-highlight-delay' seconds."
  (when (and anything-mp-highlight-delay
             (not (string= anything-pattern "")))
    (anything-mp-highlight-match-internal (window-end (anything-window)))
    (run-with-idle-timer anything-mp-highlight-delay nil
                         'anything-mp-highlight-match-internal
                         (with-current-buffer anything-buffer (point-max)))))
(add-hook 'anything-update-hook 'anything-mp-highlight-match)

(defun anything-mp-highlight-region (start end regexp face)
  (save-excursion
    (goto-char start)
    (let (me)
      (while (and (setq me (re-search-forward regexp nil t))
                  (< (point) end)
                  (< 0 (- (match-end 0) (match-beginning 0))))
        (unless (anything-pos-header-line-p)
          (put-text-property (match-beginning 0) me 'face face))))))

(defun anything-mp-highlight-match-internal (end)
  (when (anything-window)
    (set-buffer anything-buffer)
    (let ((requote (loop for (pred . re) in
                         (anything-mp-3-get-patterns anything-pattern)
                         when (and (eq pred 'identity)
                                    (>= (length re)
                                        anything-mp-highlight-threshold))
                         collect re into re-list
                         finally return
                         (if (and re-list (>= (length re-list) 1))
                             (mapconcat 'identity re-list "\\|")
                             (regexp-quote anything-pattern)))))
      (when (>= (length requote) anything-mp-highlight-threshold)
        (anything-mp-highlight-region
         (point-min) end requote 'anything-match)))))


;;; Toggle anything-match-plugin
;;
;;
(defvar anything-mp-initial-highlight-delay nil)

;;;###autoload
(defun anything-mp-toggle-match-plugin ()
  "Turn on/off multiple regexp matching in anything.
i.e anything-match-plugin."
  (interactive)
  (let ((anything-match-plugin-enabled
         (member 'anything-compile-source--match-plugin
                 anything-compile-source-functions)))
    (flet ((disable-match-plugin ()
             (setq anything-compile-source-functions
                   (delq 'anything-compile-source--match-plugin
                         anything-compile-source-functions))
             (setq anything-mp-initial-highlight-delay
                   anything-mp-highlight-delay)
             (setq anything-mp-highlight-delay nil))
           (enable-match-plugin ()
             (unless anything-mp-initial-highlight-delay
               (setq anything-mp-initial-highlight-delay
                     anything-mp-highlight-delay))
             (setq anything-compile-source-functions
                   (cons 'anything-compile-source--match-plugin
                         anything-compile-source-functions))
             (unless anything-mp-highlight-delay
               (setq anything-mp-highlight-delay
                     anything-mp-initial-highlight-delay))))
      (if anything-match-plugin-enabled
          (when (y-or-n-p "Really disable match-plugin? ")
            (disable-match-plugin)
            (message "Anything-match-plugin disabled"))
          (when (y-or-n-p "Really enable match-plugin? ")
            (enable-match-plugin)
            (message "Anything-match-plugin enabled"))))))


;;;; Grep-candidates plug-in

(defcustom anything-grep-candidates-fast-directory-regexp nil
  "*Directory regexp where a RAM disk (or tmpfs) is mounted.

If non-nil, grep-candidates plugin gets faster because it uses
grep as synchronous process.

ex. (setq anything-grep-candidates-fast-directory-regexp \"^/tmp/\")"
  :type 'string
  :group 'anything)

(defun agp-candidates (&optional filter)
  "Normal version of grep-candidates candidates function.
Grep is run by asynchronous process."
  (start-process-shell-command
   "anything-grep-candidates" nil
   (agp-command-line-2 filter (anything-attr-defined 'search-from-end))))

(defun agp-candidates-synchronous-grep (&optional filter)
  "Faster version of grep-candidates candidates function.
Grep is run by synchronous process.
It is faster when candidate files are in ramdisk."
  (split-string
   (shell-command-to-string
    (agp-command-line-2 filter (anything-attr-defined 'search-from-end)))
   "\n"))

(defun agp-candidates-synchronous-grep--direct-insert-match (&optional filter)
  "[EXPERIMENTAL]Fastest version of grep-candidates candidates function at the cost of absense of transformers.
Grep is run by synchronous process.
It is faster when candidate files are in ramdisk.

If (direct-insert-match) is in the source, this function is used."
  (with-current-buffer (anything-candidate-buffer 'global)
    (call-process-shell-command
     (agp-command-line-2 filter (anything-attr-defined 'search-from-end))
     nil t)))

(defun agp-command-line (query files &optional limit filter search-from-end)
  "Build command line used by grep-candidates from QUERY, FILES, LIMIT, and FILTER."
  (let ((allfiles (mapconcat (lambda (f) (shell-quote-argument (expand-file-name f)))
                             files " ")))
    (with-temp-buffer
      (when search-from-end
        (insert "tac " allfiles))
      (if (string= query "")
          (unless search-from-end
            (insert "cat " allfiles))
        (when search-from-end (insert " | "))
        (loop for (flag . re) in (anything-mp-3-get-patterns-internal query)
              for i from 0
              do
              (setq re (replace-regexp-in-string "^-" "\\-" re))
              (unless (zerop i) (insert " | ")) 
              (insert "grep -ih "
                      (if (eq flag 'identity) "" "-v ")
                      (shell-quote-argument re))
              (when (and (not search-from-end) (zerop i))
                (insert " " allfiles))))
      
      (when limit (insert (format " | head -n %d" limit)))
      (when filter (insert " | " filter))
      (buffer-string))))

(defun agp-command-line-2 (filter &optional search-from-end)
  "Build command line used by grep-candidates from FILTER and current source."
  (agp-command-line
   anything-pattern
   (anything-mklist (anything-interpret-value (anything-attr 'grep-candidates)))
   (anything-candidate-number-limit (anything-get-current-source))
   filter search-from-end))

(defun anything-compile-source--grep-candidates (source)
  (anything-aif (assoc-default 'grep-candidates source)
      (append
       source
       (let ((use-fast-directory
              (and anything-grep-candidates-fast-directory-regexp
                   (string-match
                    anything-grep-candidates-fast-directory-regexp
                    (or (car (anything-mklist (anything-interpret-value it))) "")))))
         (cond ((not (anything-interpret-value it)) nil)
               ((and use-fast-directory (assq 'direct-insert-match source))
                (anything-log "fastest version (use-fast-directory and direct-insert-match)")
                `((candidates . agp-candidates-synchronous-grep--direct-insert-match)
                  (match identity)
                  (volatile)))
               (use-fast-directory
                (anything-log "faster version (use-fast-directory)")
                `((candidates . agp-candidates-synchronous-grep)
                  (match identity)
                  (volatile)))
               (t
                (anything-log "normal version")
                '((candidates . agp-candidates)
                  (delayed))))))
    source))
(add-to-list 'anything-compile-source-functions 'anything-compile-source--grep-candidates)

(anything-document-attribute 'grep-candidates "grep-candidates plug-in"
  "grep-candidates plug-in provides anything-match-plugin.el feature with grep and head program.
It is MUCH FASTER than normal match-plugin to search from vary large (> 1MB) candidates.
Make sure to install these programs.

It expands `candidates' and `delayed' attributes.

`grep-candidates' attribute accepts a filename or list of filename.
It also accepts 0-argument function name or variable name.")

;; (anything '(((name . "grep-test")  (grep-candidates . "~/.emacs.el") (action . message))))
;; (let ((a "~/.emacs.el")) (anything '(((name . "grep-test")  (grep-candidates . a) (action . message) (delayed)))))
;; (let ((a "~/.emacs.el")) (anything '(((name . "grep-test")  (grep-candidates . (lambda () a)) (action . message) (delayed)))))
;; (anything '(((name . "grep-test")  (grep-candidates . "~/.emacs.el") (action . message) (delayed) (candidate-number-limit . 2))))
;; (let ((anything-candidate-number-limit 2)) (anything '(((name . "grep-test")  (grep-candidates . "~/.emacs.el") (action . message) (delayed)))))

;;;; unit test
;; unit test for match plugin are now in developper-tools/unit-test-match-plugin.el

(provide 'anything-match-plugin)

;;; anything-match-plugin.el ends here
