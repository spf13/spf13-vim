;;; inf-ruby.el --- Run a ruby process in a buffer

;; Copyright (C) 1999-2008 Yukihiro Matsumoto, Nobuyoshi Nakada

;; Author: Yukihiro Matsumoto, Nobuyoshi Nakada
;; URL: http://github.com/nonsequitur/inf-ruby
;; Created: 8 April 1998
;; Keywords: languages ruby
;; Version: 2.2.2

;;; Commentary:
;;
;; inf-ruby.el provides a REPL buffer connected to an IRB subprocess.
;;
;; If you're installing manually, you'll need to:
;; * drop the file somewhere on your load path (perhaps ~/.emacs.d)
;; * Add the following lines to your .emacs file:
;;    (autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
;;    (autoload 'inf-ruby-keys "inf-ruby" "" t)
;;    (eval-after-load 'ruby-mode
;;      '(add-hook 'ruby-mode-hook 'inf-ruby-keys))

;;; TODO:
;;
;; inferior-ruby-error-regexp-alist doesn't match this example
;;   SyntaxError: /home/eschulte/united/org/work/arf/arf/lib/cluster.rb:35: syntax error, unexpected '~', expecting kEND
;;               similarity = comparison_cache[m][n] ||= clusters[m] ~ clusters[n]
;;
;; M-p skips the first entry in the input ring.
;;

(require 'comint)
(require 'compile)
(require 'ruby-mode)

(defvar inf-ruby-default-implementation "ruby"
  "Which ruby implementation to use if none is specified.")

(defvar inf-ruby-first-prompt-pattern "^irb(.*)[0-9:]+0> *"
  "First prompt regex pattern of ruby interpreter.")

(defvar inf-ruby-prompt-pattern "^\\(irb(.*)[0-9:]+[>*\"'] *\\)+"
  "Prompt regex pattern of ruby interpreter.")

(defvar inf-ruby-mode-hook nil
  "*Hook for customising inf-ruby mode.")

(defvar inf-ruby-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map (kbd "C-c C-l") 'inf-ruby-load-file)
    (define-key map (kbd "C-x C-e") 'ruby-send-last-sexp)
    (define-key map (kbd "TAB") 'inf-ruby-complete)
    map)
  "*Mode map for inf-ruby-mode")

(defvar inf-ruby-implementations
  '(("ruby"     . "irb --inf-ruby-mode -r irb/completion")
    ("jruby"    . "jruby -S irb -r irb/completion")
    ("rubinius" . "rbx -r irb/completion")
    ("yarv"     . "irb1.9 --inf-ruby-mode -r irb/completion")) ;; TODO: ironruby?
  "An alist of ruby implementations to irb executable names.")

;; TODO: do we need these two defvars?
(defvar ruby-source-modes '(ruby-mode)
  "*Used to determine if a buffer contains Ruby source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a ruby source file by ruby-load-file.
Used by these commands to determine defaults.")

(defvar ruby-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.
Caches the last pair used in the last ruby-load-file command.
Used for determining the default in the
next one.")

(defvar inf-ruby-at-top-level-prompt-p t)

(defconst inf-ruby-error-regexp-alist
  '(("SyntaxError: compile error\n^\\([^\(].*\\):\\([1-9][0-9]*\\):" 1 2)
    ("^\tfrom \\([^\(].*\\):\\([1-9][0-9]*\\)\\(:in `.*'\\)?$" 1 2)))

;;;###autoload
(defun inf-ruby-keys ()
  "Set local key defs to invoke inf-ruby from ruby-mode."
  (define-key ruby-mode-map "\M-\C-x" 'ruby-send-definition)
  (define-key ruby-mode-map "\C-x\C-e" 'ruby-send-last-sexp)
  (define-key ruby-mode-map "\C-c\C-b" 'ruby-send-block)
  (define-key ruby-mode-map "\C-c\M-b" 'ruby-send-block-and-go)
  (define-key ruby-mode-map "\C-c\C-x" 'ruby-send-definition)
  (define-key ruby-mode-map "\C-c\M-x" 'ruby-send-definition-and-go)
  (define-key ruby-mode-map "\C-c\C-r" 'ruby-send-region)
  (define-key ruby-mode-map "\C-c\M-r" 'ruby-send-region-and-go)
  (define-key ruby-mode-map "\C-c\C-z" 'ruby-switch-to-inf)
  (define-key ruby-mode-map "\C-c\C-l" 'ruby-load-file)
  (define-key ruby-mode-map "\C-c\C-s" 'inf-ruby))

(defvar inf-ruby-buffer nil "Current irb process buffer.")

(defun inf-ruby-mode ()
  "Major mode for interacting with an inferior ruby (irb) process.

The following commands are available:
\\{inf-ruby-mode-map}

A ruby process can be fired up with M-x inf-ruby.

Customisation: Entry to this mode runs the hooks on comint-mode-hook and
inf-ruby-mode-hook (in that order).

You can send text to the inferior ruby process from other buffers containing
Ruby source.
    ruby-switch-to-inf switches the current buffer to the ruby process buffer.
    ruby-send-definition sends the current definition to the ruby process.
    ruby-send-region sends the current region to the ruby process.

    ruby-send-definition-and-go, ruby-send-region-and-go,
        switch to the ruby process buffer after sending their text.

Commands:
Return after the end of the process' output sends the text from the
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for ruby; with arugment, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  # start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp inf-ruby-prompt-pattern)
  (ruby-mode-variables)
  (setq major-mode 'inf-ruby-mode)
  (setq mode-name "Inf-Ruby")
  (setq mode-line-process '(":%s"))
  (use-local-map inf-ruby-mode-map)
  (setq comint-input-filter (function inf-ruby-input-filter))
  (add-to-list 'comint-output-filter-functions 'inf-ruby-output-filter)
  (setq comint-get-old-input (function inf-ruby-get-old-input))
  (make-local-variable 'compilation-error-regexp-alist)
  (setq compilation-error-regexp-alist inf-ruby-error-regexp-alist)
  (compilation-shell-minor-mode t)
  (run-hooks 'inf-ruby-mode-hook))

(defvar inf-ruby-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "*Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters.")

(defun inf-ruby-input-filter (str)
  "Don't save anything matching inf-ruby-filter-regexp"
  (not (string-match inf-ruby-filter-regexp str)))

(defun inf-ruby-output-filter (output)
  "Check if the current prompt is a top-level prompt"
  (setq inf-ruby-at-top-level-prompt-p
        (string-match inf-ruby-prompt-pattern
                      (car (last (split-string output "\n"))))))

;; adapted from replace-in-string in XEmacs (subr.el)
(defun inf-ruby-remove-in-string (str regexp)
  "Remove all matches in STR for REGEXP and returns the new string."
  (let ((rtn-str "") (start 0) match prev-start)
    (while (setq match (string-match regexp str start))
      (setq prev-start start
            start (match-end 0)
            rtn-str (concat rtn-str (substring str prev-start match))))
    (concat rtn-str (substring str start))))

(defun inf-ruby-get-old-input ()
  "Snarf the sexp ending at point"
  (save-excursion
    (let ((end (point)))
      (re-search-backward inf-ruby-first-prompt-pattern)
      (inf-ruby-remove-in-string (buffer-substring (point) end)
                                 inf-ruby-prompt-pattern))))

;;;###autoload
(defun inf-ruby (&optional impl)
  "Run an inferior Ruby process in a buffer.
With prefix argument, prompts for which Ruby implementation
\(from the list `inf-ruby-implementations') to use. Runs the
hooks `inf-ruby-mode-hook' \(after the `comint-mode-hook' is
run)."

  (interactive (list (if current-prefix-arg
                         (completing-read "Ruby Implementation: "
                                          (mapc #'car inf-ruby-implementations))
                       inf-ruby-default-implementation)))
  (setq impl (or impl "ruby"))

  (let ((command (cdr (assoc impl inf-ruby-implementations))))
    (run-ruby command impl)))

;;;###autoload
(defun run-ruby (&optional command name)
  "Run an inferior Ruby process, input and output via buffer *ruby*.
If there is a process already running in `*ruby*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `ruby-program-name').  Runs the hooks `inferior-ruby-mode-hook'
\(after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive)
  (setq command (or command (cdr (assoc inf-ruby-default-implementation
                                        inf-ruby-implementations))))
  (setq name (or name "ruby"))

  (if (not (comint-check-proc inf-ruby-buffer))
      (let ((commandlist (split-string command)))
        (set-buffer (apply 'make-comint name (car commandlist)
                           nil (cdr commandlist)))
        (inf-ruby-mode)))
  (pop-to-buffer (setq inf-ruby-buffer (format "*%s*" name))))

(defun inf-ruby-proc ()
  "Returns the current IRB process. See variable inf-ruby-buffer."
  (or (get-buffer-process (if (eq major-mode 'inf-ruby-mode)
                              (current-buffer)
                            inf-ruby-buffer))
      (error "No current process. See variable inf-ruby-buffer")))

;; These commands are added to the ruby-mode keymap:

(defconst ruby-send-terminator "--inf-ruby-%x-%d-%d-%d--"
  "Template for irb here document terminator.
Must not contain ruby meta characters.")

(defconst inf-ruby-eval-binding "IRB.conf[:MAIN_CONTEXT].workspace.binding")

(defconst ruby-eval-separator "")

(defun ruby-send-region (start end)
  "Send the current region to the inferior Ruby process."
  (interactive "r")
  (let (term (file (or buffer-file-name (buffer-name))) line)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char start)
        (setq line (+ start (forward-line (- start)) 1))
        (goto-char start)
        (while (progn
                 (setq term (apply 'format ruby-send-terminator (random) (current-time)))
                 (re-search-forward (concat "^" (regexp-quote term) "$") end t)))))
    ;; compilation-parse-errors parses from second line.
    (save-excursion
      (let ((m (process-mark (inf-ruby-proc))))
        (set-buffer (marker-buffer m))
        (goto-char m)
        (insert ruby-eval-separator "\n")
        (set-marker m (point))))
    (comint-send-string (inf-ruby-proc) (format "eval <<'%s', %s, %S, %d\n"
                                                term inf-ruby-eval-binding
                                                file line))
    (comint-send-region (inf-ruby-proc) start end)
    (comint-send-string (inf-ruby-proc) (concat "\n" term "\n"))))

(defun ruby-send-definition ()
  "Send the current definition to the inferior Ruby process."
  (interactive)
  (save-excursion
    (ruby-end-of-defun)
    (let ((end (point)))
      (ruby-beginning-of-defun)
      (ruby-send-region (point) end))))

(defun ruby-send-last-sexp ()
  "Send the previous sexp to the inferior Ruby process."
  (interactive)
  (ruby-send-region (save-excursion (backward-sexp) (point)) (point)))

(defun ruby-send-block ()
  "Send the current block to the inferior Ruby process."
  (interactive)
  (save-excursion
    (ruby-end-of-block)
    (end-of-line)
    (let ((end (point)))
      (ruby-beginning-of-block)
      (ruby-send-region (point) end))))

(defun ruby-switch-to-inf (eob-p)
  "Switch to the ruby process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer inf-ruby-buffer)
      (pop-to-buffer inf-ruby-buffer)
    (error "No current process buffer. See variable inf-ruby-buffer."))
  (cond (eob-p
         (push-mark)
         (goto-char (point-max)))))

(defun ruby-send-region-and-go (start end)
  "Send the current region to the inferior Ruby process.
Then switch to the process buffer."
  (interactive "r")
  (ruby-send-region start end)
  (ruby-switch-to-inf t))

(defun ruby-send-definition-and-go ()
  "Send the current definition to the inferior Ruby.
Then switch to the process buffer."
  (interactive)
  (ruby-send-definition)
  (ruby-switch-to-inf t))

(defun ruby-send-block-and-go ()
  "Send the current block to the inferior Ruby.
Then switch to the process buffer."
  (interactive)
  (ruby-send-block)
  (ruby-switch-to-inf t))

(defun ruby-load-file (file-name)
  "Load a Ruby file into the inferior Ruby process."
  (interactive (comint-get-source "Load Ruby file: " ruby-prev-l/c-dir/file
                                  ruby-source-modes t)) ;; T because LOAD needs an exact name
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq ruby-prev-l/c-dir/file (cons (file-name-directory    file-name)
                                     (file-name-nondirectory file-name)))
  (comint-send-string (inf-ruby-proc) (concat "(load \""
                                              file-name
                                              "\"\)\n")))

(defun ruby-escape-single-quoted (str)
  (replace-regexp-in-string "'" "\\\\'"
    (replace-regexp-in-string "\n" "\\\\n" 
      (replace-regexp-in-string "\\\\" "\\\\\\\\" str))))

(defun inf-ruby-completions (seed)
  "Return a list of completions for the line of ruby code starting with SEED."
  (let* ((proc (get-buffer-process inf-ruby-buffer))
	 (comint-filt (process-filter proc))
	 (kept "") completions)
    (set-process-filter proc (lambda (proc string) (setq kept (concat kept string))))
    (process-send-string proc (format "puts IRB::InputCompletor::CompletionProc.call('%s').compact\n"
                                      (ruby-escape-single-quoted seed)))
    (while (and (not (string-match inf-ruby-prompt-pattern kept))
                (accept-process-output proc 2)))
    (setq completions (cdr (butlast (split-string kept "\r?\n") 2)))
    (set-process-filter proc comint-filt)
    completions))

(defun inf-ruby-completion-at-point ()
  (if inf-ruby-at-top-level-prompt-p
      (let* ((curr (replace-regexp-in-string "\n$" "" (thing-at-point 'line)))
             (completions (inf-ruby-completions curr)))
        (if completions
            (if (= (length completions) 1)
                (car completions)
              (completing-read "possible completions: "
                               completions nil t curr))))
    (message "Completion aborted: Not at a top-level prompt")
    nil))

(defun inf-ruby-complete (command)
  "Complete the ruby code at point. Relies on the irb/completion
Module used by readline when running irb through a terminal"
  (interactive (list (inf-ruby-completion-at-point)))
  (when command
   (kill-whole-line 0)
   (insert command)))

(defun inf-ruby-complete-or-tab (&optional command)
  "Either complete the ruby code at point or call
`indent-for-tab-command' if no completion is available."
  (interactive (list (inf-ruby-completion-at-point)))
  (if (not command)
      (call-interactively 'indent-for-tab-command)
    (inf-ruby-complete command)))

;;;###autoload
(eval-after-load 'ruby-mode
  '(add-hook 'ruby-mode-hook 'inf-ruby-keys))

(provide 'inf-ruby)
;;; inf-ruby.el ends here
