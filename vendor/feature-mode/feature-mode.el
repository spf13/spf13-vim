;; cucumber.el -- Emacs mode for editing plain text user stories
;;
;; Copyright (C) 2008 — 2011 Michael Klishin and other contributors
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Suite 500, Boston, MA 02110.
;;
;; Copy files to ~/.emacs.d/elisp/feature-mode and add this to your
;; .emacs to load the mode
;; (add-to-list 'load-path "~/.emacs.d/elisp/feature-mode")
;; ;; optional configurations
;; ;; default language if .feature doesn't have "# language: fi"
;; ;(setq feature-default-language "fi")
;; ;; point to cucumber languages.yml or gherkin i18n.yml to use
;; ;; exactly the same localization your cucumber uses
;; ;(setq feature-default-i18n-file "/path/to/gherkin/gem/i18n.yml")
;; ;; and load it
;; (require 'feature-mode)
;; (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
;; 
;; Language used in feature file is automatically detected from
;; "language: [2-letter ISO-code]" tag in feature file.  You can
;; choose the language feature-mode should use in case autodetection
;; fails.  Just add
;; (setq feature-default-language "en")
;; to your .emacs
;;
;; Translations are loaded from ~/.emacs.d/elisp/feature-mode/i18n.yml
;; by default.  You can configure feature-mode to load translations
;; directly from cucumber languages.yml or gherkin i18n.yml.  Just add
;; (setq feature-default-i18n-file 
;;  "/usr/lib/ruby/gems/1.8/gems/cucumber-0.4.4/lib/cucumber/languages.yml")
;; to your .emacs before
;; (require 'feature-mode)
;;
;;
;; In order to get goto-step-definition to work, you must install the
;; ruby_parser gem (version 2.0.x). For example:
;;
;;    gem install ruby_parser --version=2.0.5
;;
;; (be sure and use the ruby-interpreter that emacs will use based on
;; `exec-path')
;;
;;
;; Key Bindings
;; ------------
;;
;;  \C-c ,v
;;  :   Verify all scenarios in the current buffer file.
;;
;;  \C-c ,s
;;  :   Verify the scenario under the point in the current buffer.
;;
;;  \C-c ,f
;;  :   Verify all features in project. (Available in feature and
;;      ruby files)
;;
;;  \C-c ,r
;;  :   Repeat the last verification process.
;;
;;  \C-c ,g
;;  :   Go to step-definition under point

(eval-when-compile (require 'cl))
(require 'thingatpt)

;;
;; Keywords and font locking
;;

(when (featurep 'font-lock)
  (or (boundp 'font-lock-variable-name-face)
      (setq font-lock-variable-name-face font-lock-type-face)))

(defun load-gherkin-i10n (filename)
  "Read and parse Gherkin l10n from given file."
  (interactive "Load l10n file: ")
  (with-temp-buffer 
    (insert-file-contents filename)
    (parse-gherkin-l10n)))

(defun parse-gherkin-l10n ()
  (let (languages-alist)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (if (try-find-next-language)
            (let ((lang-beg (+ (point) 1))
                  (lang-end (progn (end-of-line) (- (point) 2)))
                  (kwds-beg (+ (point) 1))
                  (kwds-end (progn (try-find-next-language) (point))))
              (add-to-list
               'languages-alist 
               (cons 
                (filter-buffer-substring lang-beg lang-end) 
                (parse-gherkin-l10n-translations kwds-beg kwds-end)))))))
    (nreverse languages-alist)))

(defun try-find-next (regexp)
  (let (search-result)
    (setq search-result (search-forward-regexp regexp nil t))
    (if search-result
        (beginning-of-line)
      (goto-char (point-max)))
    search-result))

(defun try-find-next-language ()
  (try-find-next "^\"[^\"]+\":"))

(defun try-find-next-translation ()
  (try-find-next "^  \\([^ :]+\\): +\"?\\*?|?\\([^\"\n]+\\)\"?"))

(defun parse-gherkin-l10n-translations (beg end)
  (let (translations-alist)
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (while (not (eobp))
          (if (try-find-next-translation)
              (let ((kwname (match-string-no-properties 1))
                    (kw     (match-string-no-properties 2)))
                (add-to-list 
                 'translations-alist
                 (cons 
                  (intern kwname)
                  (if (or (equal kwname "name")
                          (equal kwname "native"))
                      kw
                    (build-keyword-matcher kw))))))
          (end-of-line))))
    (nreverse translations-alist)))

(defun build-keyword-matcher (keyword)
  (concat "^[ \t]*\\(" (replace-regexp-in-string "|" "\\\\|" keyword) "\\):?"))

(defvar feature-default-language "en")
(defvar feature-default-i18n-file "~/.emacs.d/elisp/feature-mode/i18n.yml")

(defconst feature-keywords-per-language
  (if (file-readable-p feature-default-i18n-file)
      (load-gherkin-i10n feature-default-i18n-file)
  '(("en" . ((feature    . "^ *Feature:")
             (background . "^ *Background:")
             (scenario   . "^ *Scenario:")
             (scenario_outline . 
                           "^ *Scenario Outline:")
             (given      . "^ *Given")
             (when       . "^ *When")
             (then       . "^ *Then")
             (but        . "^ *But")
             (and        . "^ *And")
             (examples   . "^ *\\(Examples\\|Scenarios\\):?"))))))

(defconst feature-font-lock-keywords
  '((feature      (0 font-lock-keyword-face)
                  (".*" nil nil (0 font-lock-type-face t)))
    (background . (0 font-lock-keyword-face))
    (scenario     (0 font-lock-keyword-face)
                  (".*" nil nil (0 font-lock-function-name-face t)))
    (scenario_outline
                  (0 font-lock-keyword-face)
                  (".*" nil nil (0 font-lock-function-name-face t)))
    (given      . font-lock-keyword-face)
    (when       . font-lock-keyword-face)
    (then       . font-lock-keyword-face)
    (but        . font-lock-keyword-face)
    (and        . font-lock-keyword-face)
    (examples   . font-lock-keyword-face)
    ("^ *@.*"   . font-lock-preprocessor-face)
    ("^ *#.*"     0 font-lock-comment-face t)))


;;
;; Keymap
;;

(defvar feature-mode-map nil "Keymap used in feature mode")

(if feature-mode-map
    nil
  (setq feature-mode-map (make-sparse-keymap))
  (define-key feature-mode-map "\C-m" 'newline)
  (define-key feature-mode-map  (kbd "C-c ,s") 'feature-verify-scenario-at-pos)
  (define-key feature-mode-map  (kbd "C-c ,v") 'feature-verify-all-scenarios-in-buffer)
  (define-key feature-mode-map  (kbd "C-c ,f") 'feature-verify-all-scenarios-in-project)
  (define-key feature-mode-map  (kbd "C-c ,g") 'feature-goto-step-definition))

;; Add relevant feature keybindings to ruby modes
(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c ,f") 'feature-verify-all-scenarios-in-project)))


;;
;; Syntax table
;;

(defvar feature-mode-syntax-table nil
  "Syntax table in use in ruby-mode buffers.")

(unless feature-mode-syntax-table
  (setq feature-mode-syntax-table (make-syntax-table)))

;; Constants

(defconst feature-blank-line-re "^[ \t]*$"
  "Regexp matching a line containing only whitespace.")

(defun feature-feature-re (language)
  (cdr (assoc 'feature (cdr (assoc language feature-keywords-per-language)))))

(defun feature-scenario-re (language)
  (cdr (assoc 'scenario (cdr (assoc language feature-keywords-per-language)))))

(defun feature-background-re (language)
  (cdr (assoc 'background (cdr (assoc language feature-keywords-per-language)))))

;;
;; Variables
;;

(defvar feature-mode-hook nil
  "Hook run when entering `feature-mode'.")

(defcustom feature-indent-level 2
  "Indentation of feature statements"
  :type 'integer :group 'feature)

(defcustom feature-indent-offset 2
  "*Amount of offset per level of indentation."
  :type 'integer :group 'feature)

(defun feature-compute-indentation ()
  "Calculate the maximum sensible indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (if (bobp) 10
      (forward-line -1)
      (while (and (looking-at feature-blank-line-re)
                  (> (point) (point-min)))
        (forward-line -1))
      (+ (current-indentation)
         (if (or (looking-at (feature-feature-re (feature-detect-language)))
                 (looking-at (feature-scenario-re (feature-detect-language)))
                 (looking-at (feature-background-re (feature-detect-language))))
             feature-indent-offset 0)))))

(defun feature-indent-line ()
  "Indent the current line.
The first time this command is used, the line will be indented to the
maximum sensible indentation.  Each immediately subsequent usage will
back-dent the line by `feature-indent-offset' spaces.  On reaching column
0, it will cycle back to the maximum sensible indentation."
  (interactive "*")
  (let ((ci (current-indentation))
        (cc (current-column))
        (need (feature-compute-indentation)))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (if (and (equal last-command this-command) (/= ci 0))
          (indent-to (* (/ (- ci 1) feature-indent-offset) feature-indent-offset))
        (indent-to need)))
    (if (< (current-column) (current-indentation))
        (forward-to-indentation 0))))

(defun feature-font-lock-keywords-for (language)
  (let ((result-keywords . ()))
    (dolist (pair feature-font-lock-keywords)
      (let* ((keyword (car pair))
             (font-locking (cdr pair))
             (language-keyword (cdr (assoc keyword
                                           (cdr (assoc
                                                 language
                                                 feature-keywords-per-language))))))

        (push (cons (or language-keyword keyword) font-locking) result-keywords)))
    result-keywords))

(defun feature-detect-language ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "language: \\([[:alpha:]-]+\\)"
                           (line-end-position)
                           t)
        (match-string 1)
      feature-default-language)))

(defun feature-mode-variables ()
  (set-syntax-table feature-mode-syntax-table)
  (setq require-final-newline t)
  (setq comment-start "# ")
  (setq comment-start-skip "#+ *")
  (setq comment-end "")
  (setq parse-sexp-ignore-comments t)
  (set (make-local-variable 'indent-tabs-mode) 'nil)
  (set (make-local-variable 'indent-line-function) 'feature-indent-line)
  (set (make-local-variable 'font-lock-defaults)
       (list (feature-font-lock-keywords-for (feature-detect-language)) nil nil))
  (set (make-local-variable 'font-lock-keywords)
       (feature-font-lock-keywords-for (feature-detect-language))))

(defun feature-minor-modes ()
  "Enable all minor modes for feature mode."
  (turn-on-orgtbl))

;;
;; Mode function
;;

;;;###autoload
(defun feature-mode()
  "Major mode for editing plain text stories"
  (interactive)
  (kill-all-local-variables)
  (use-local-map feature-mode-map)
  (setq mode-name "Feature")
  (setq major-mode 'feature-mode)
  (feature-mode-variables)
  (feature-minor-modes)
  (run-mode-hooks 'feature-mode-hook))

(add-to-list 'auto-mode-alist '("\\.feature\\'" . feature-mode))

;;
;; Snippets
;;

(defvar feature-snippet-directory (concat (file-name-directory load-file-name) "snippets")
  "Path to the feature-mode snippets.

If the yasnippet library is loaded, snippets in this directory
are loaded on startup.  If nil, don't load snippets.")

(defvar feature-support-directory (concat (file-name-directory load-file-name) "support")
  "Path to support folder

   The support folder contains a ruby script that takes a step as an
   argument, and outputs a list of all matching step definitions")

(declare-function yas/load-directory "yasnippet" t)
(when (and (featurep 'yasnippet)
           feature-snippet-directory
           (file-exists-p feature-snippet-directory))
  (yas/load-directory feature-snippet-directory))


;;
;; Verifying features
;;

(defun feature-scenario-name-re (language)
  (concat (feature-scenario-re (feature-detect-language)) "[[:space:]]+\\(.*\\)$"))

(defun feature-scenario-name-at-pos (&optional pos)
  "Returns the name of the scenario at the specified position. if pos is not specified the current buffer location will be used."
  (interactive)
  (let ((start (or pos (point))))
    (save-excursion
      (end-of-line)
      (unless (re-search-backward (feature-scenario-name-re (feature-detect-language)) nil t)
        (error "Unable to find an scenario"))
      (match-string-no-properties 1))))

(defun feature-verify-scenario-at-pos (&optional pos)
  "Run the scenario defined at pos.  If post is not specified the current buffer location will be used."
  (interactive)
  (feature-run-cucumber
   (list "-n" (concat "'" (feature-escape-scenario-name (feature-scenario-name-at-pos)) "'"))
   :feature-file (buffer-file-name)))

(defun feature-verify-all-scenarios-in-buffer ()
  "Run all the scenarios defined in current buffer."
  (interactive)
  (feature-run-cucumber '() :feature-file (buffer-file-name)))


(defun feature-verify-all-scenarios-in-project ()
  "Run all the scenarios defined in current project."
  (interactive)
  (feature-run-cucumber '()))

(defun feature-register-verify-redo (redoer)
  "Register a bit of code that will repeat a verification process"
  (let ((redoer-cmd (eval (list 'lambda ()
                                '(interactive)
                                (list 'let (list (list `default-directory
                                                       default-directory))
                                      redoer)))))

    (global-set-key (kbd "C-c ,r") redoer-cmd)))

(defun feature-run-cucumber (cuke-opts &optional &key feature-file)
  "Runs cucumber with the specified options"
  (feature-register-verify-redo (list 'feature-run-cucumber
                                      (list 'quote cuke-opts)
                                      :feature-file feature-file))
  ;; redoer is registered

  (let ((opts-str    (mapconcat 'identity cuke-opts " "))
        (feature-arg (if feature-file
                         (concat " FEATURE='" feature-file "'")
                         "")))
    (ansi-color-for-comint-mode-on)
    (let ((default-directory (feature-project-root)))
      (compile (concat "rake cucumber CUCUMBER_OPTS=\"" opts-str "\"" feature-arg) t)))
  (end-of-buffer-other-window 0))

(defun feature-escape-scenario-name (scenario-name)
  "Escapes all the characaters in a scenario name that mess up using in the -n options"
  (replace-regexp-in-string "\\(\"\\)" "\\\\\\\\\\\\\\1" (replace-regexp-in-string "\\([()\']\\|\\[\\|\\]\\)" "\\\\\\1" scenario-name)))

(defun feature-root-directory-p (a-directory)
  "Tests if a-directory is the root of the directory tree (i.e. is it '/' on unix)."
  (equal a-directory (file-name-directory (directory-file-name a-directory))))

(defun feature-project-root (&optional directory)
  "Finds the root directory of the project by walking the directory tree until it finds Rakefile (presumably, application root)"
  (let ((directory (file-name-as-directory (or directory default-directory))))
    (if (feature-root-directory-p directory) (error "No rakefle found"))
    (if (file-exists-p (concat directory "Rakefile"))
        directory
      (feature-project-root (file-name-directory (directory-file-name directory))))))

(defun feature-goto-step-definition ()
  "Goto the step-definition under (point).  Requires ruby"
  (interactive)
  (let* ((root (feature-project-root))
         (input (thing-at-point 'line))
         (_ (set-text-properties 0 (length input) nil input))
         (result (shell-command-to-string (format "cd %S && ruby %S/go_to_step.rb %S"
                                                  root
                                                  feature-support-directory
                                                  input)))
         (file-and-line (car (split-string result "\n")))
         (matched? (string-match "^\\(.+\\):\\([0-9]+\\)$" file-and-line)))
    (if matched?
        (let ((file                   (format "%s/%s" root (match-string 1 file-and-line)))
              (line-no (string-to-number (match-string 2 file-and-line))))
          (find-file file)
          (goto-char (point-min))
          (forward-line (1- line-no)))
      (if (equal "" result)
          (message "No matching steps found for:\n%s" input)
        (message "An error occurred:\n%s" result)))))

(provide 'cucumber-mode)
(provide 'feature-mode)
