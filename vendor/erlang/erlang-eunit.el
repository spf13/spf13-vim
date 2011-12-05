;;
;; %CopyrightBegin%
;;
;; Copyright Ericsson AB 2009-2010. All Rights Reserved.
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
;;;
;;; Purpose: Provide EUnit utilities.
;;;
;;; Author: Klas Johansson

(eval-when-compile
  (require 'cl))

(defvar erlang-eunit-src-candidate-dirs '("../src" ".")
  "*Name of directories which to search for source files matching
an EUnit test file.  The first directory in the list will be used,
if there is no match.")

(defvar erlang-eunit-test-candidate-dirs '("../test" ".")
  "*Name of directories which to search for EUnit test files matching
a source file.  The first directory in the list will be used,
if there is no match.")

(defvar erlang-eunit-autosave nil
  "*Set to non-nil to automtically save unsaved buffers before running tests.
This is useful, reducing the save-compile-load-test cycle to one keychord.")

(defvar erlang-eunit-recent-info '((mode . nil) (module . nil) (test . nil) (cover . nil))
  "Info about the most recent running of an EUnit test representation.")

;;;
;;; Switch between src/EUnit test buffers
;;;
(defun erlang-eunit-toggle-src-and-test-file-other-window ()
  "Switch to the src file if the EUnit test file is the current
buffer and vice versa"
  (interactive)
  (if (erlang-eunit-test-file-p buffer-file-name)
      (erlang-eunit-open-src-file-other-window buffer-file-name)
    (erlang-eunit-open-test-file-other-window buffer-file-name)))

;;;
;;; Open the EUnit test file which corresponds to a src file
;;;
(defun erlang-eunit-open-test-file-other-window (src-file-path)
  "Open the EUnit test file which corresponds to a src file"
  (find-file-other-window (erlang-eunit-test-filename src-file-path)))

;;;
;;; Open the src file which corresponds to the an EUnit test file
;;;
(defun erlang-eunit-open-src-file-other-window (test-file-path)
  "Open the src file which corresponds to the an EUnit test file"
    (find-file-other-window (erlang-eunit-src-filename test-file-path)))

;;; Return the name and path of the EUnit test file
;;, (input may be either the source filename itself or the EUnit test filename)
(defun erlang-eunit-test-filename (file-path)
  (if (erlang-eunit-test-file-p file-path)
      file-path
    (erlang-eunit-rewrite-filename file-path erlang-eunit-test-candidate-dirs)))

;;; Return the name and path of the source file
;;, (input may be either the source filename itself or the EUnit test filename)
(defun erlang-eunit-src-filename (file-path)
  (if (erlang-eunit-src-file-p file-path)
      file-path
    (erlang-eunit-rewrite-filename file-path erlang-eunit-src-candidate-dirs)))

;;; Rewrite a filename from the src or test filename to the other
(defun erlang-eunit-rewrite-filename (orig-file-path candidate-dirs)
  (or (erlang-eunit-locate-buddy orig-file-path candidate-dirs)
      (erlang-eunit-buddy-file-path orig-file-path (car candidate-dirs))))

;;; Search for a file's buddy file (a source file's EUnit test file,
;;; or an EUnit test file's source file) in a list of candidate
;;; directories.
(defun erlang-eunit-locate-buddy (orig-file-path candidate-dirs)
  (when candidate-dirs
    (let ((buddy-file-path (erlang-eunit-buddy-file-path
                            orig-file-path
                            (car candidate-dirs))))
      (if (file-readable-p buddy-file-path)
          buddy-file-path
        (erlang-eunit-locate-buddy orig-file-path (cdr candidate-dirs))))))

(defun erlang-eunit-buddy-file-path (orig-file-path buddy-dir-name)
  (let* ((orig-dir-name   (file-name-directory orig-file-path))
         (buddy-dir-name  (file-truename
                           (filename-join orig-dir-name buddy-dir-name)))
         (buddy-base-name (erlang-eunit-buddy-basename orig-file-path)))
    (filename-join buddy-dir-name buddy-base-name)))

;;; Return the basename of the buddy file:
;;;     /tmp/foo/src/x.erl        --> x_tests.erl
;;;     /tmp/foo/test/x_tests.erl --> x.erl
(defun erlang-eunit-buddy-basename (file-path)
  (let ((src-module-name (erlang-eunit-source-module-name file-path)))
    (cond
     ((erlang-eunit-src-file-p file-path)
      (concat src-module-name "_tests.erl"))
     ((erlang-eunit-test-file-p file-path)
      (concat src-module-name ".erl")))))

;;; Checks whether a file is a source file or not
(defun erlang-eunit-src-file-p (file-path)
  (not (erlang-eunit-test-file-p file-path)))

;;; Checks whether a file is a EUnit test file or not
(defun erlang-eunit-test-file-p (file-path)
  (erlang-eunit-string-match-p "^\\(.+\\)_tests.erl$" file-path))

;;; Return the module name of the source file
;;;     /tmp/foo/src/x.erl        --> x
;;;     /tmp/foo/test/x_tests.erl --> x
(defun erlang-eunit-source-module-name (file-path)
  (interactive)
  (let ((module-name (erlang-eunit-module-name file-path)))
    (if (string-match "^\\(.+\\)_tests$" module-name)
        (substring module-name (match-beginning 1) (match-end 1))
      module-name)))

;;; Return the module name of the file
;;;     /tmp/foo/src/x.erl        --> x
;;;     /tmp/foo/test/x_tests.erl --> x_tests
(defun erlang-eunit-module-name (file-path)
  (interactive)
  (file-name-sans-extension (file-name-nondirectory file-path)))

;;; Older emacsen don't have string-match-p.
(defun erlang-eunit-string-match-p (regexp string &optional start)
  (if (fboundp 'string-match-p) ;; appeared in emacs 23
      (string-match-p regexp string start)
    (save-match-data ;; fallback for earlier versions of emacs
      (string-match regexp string start))))

;;; Join filenames
(defun filename-join (dir file)
  (if (or (= (elt file 0) ?/)
	  (= (car (last (append dir nil))) ?/))
      (concat dir file)
    (concat dir "/" file)))

;;; Get info about the most recent running of EUnit
(defun erlang-eunit-recent (key)
  (cdr (assq key erlang-eunit-recent-info)))

;;; Record info about the most recent running of EUnit
;;; Known modes are 'module-mode and 'test-mode
(defun erlang-eunit-record-recent (mode module test)
  (setcdr (assq 'mode erlang-eunit-recent-info) mode)
  (setcdr (assq 'module erlang-eunit-recent-info) module)
  (setcdr (assq 'test erlang-eunit-recent-info) test))

;;; Record whether the most recent running of EUnit included cover
;;; compilation
(defun erlang-eunit-record-recent-compile (under-cover)
  (setcdr (assq 'cover erlang-eunit-recent-info) under-cover))

;;; Determine options for EUnit.
(defun erlang-eunit-opts ()
  (if current-prefix-arg ", [verbose]" ""))

;;; Determine current test function
(defun erlang-eunit-current-test ()
  (save-excursion
    (erlang-end-of-function 1)
    (erlang-beginning-of-function 1)
    (erlang-name-of-function)))

(defun erlang-eunit-simple-test-p (test-name)
  (if (erlang-eunit-string-match-p "^\\(.+\\)_test$" test-name) t nil))

(defun erlang-eunit-test-generator-p (test-name)
  (if (erlang-eunit-string-match-p "^\\(.+\\)_test_$" test-name) t nil))

;;; Run one EUnit test
(defun erlang-eunit-run-test (module-name test-name)
  (let ((command
         (cond ((erlang-eunit-simple-test-p test-name)
                (format "eunit:test({%s, %s}%s)."
                        module-name test-name (erlang-eunit-opts)))
               ((erlang-eunit-test-generator-p test-name)
                (format "eunit:test({generator, %s, %s}%s)."
                        module-name test-name (erlang-eunit-opts)))
               (t (format "%% WARNING: '%s' is not a test function" test-name)))))
    (erlang-eunit-record-recent 'test-mode module-name test-name)
    (erlang-eunit-inferior-erlang-send-command command)))

;;; Run EUnit tests for the current module
(defun erlang-eunit-run-module-tests (module-name)
  (let ((command (format "eunit:test(%s%s)." module-name (erlang-eunit-opts))))
    (erlang-eunit-record-recent 'module-mode module-name nil)
    (erlang-eunit-inferior-erlang-send-command command)))

(defun erlang-eunit-compile-and-run-recent ()
  "Compile the source and test files and repeat the most recent EUnit test run.

With prefix arg, compiles for debug and runs tests with the verbose flag set."
  (interactive)
  (case (erlang-eunit-recent 'mode)
    ('test-mode
     (erlang-eunit-compile-and-test
      'erlang-eunit-run-test (list (erlang-eunit-recent 'module)
                                   (erlang-eunit-recent 'test))))
    ('module-mode
     (erlang-eunit-compile-and-test
      'erlang-eunit-run-module-tests (list (erlang-eunit-recent 'module))
      (erlang-eunit-recent 'cover)))
    (t (error "EUnit has not yet been run.  Please run a test first."))))

(defun erlang-eunit-cover-compile ()
  "Cover compile current module."
  (interactive)
  (let* ((erlang-compile-extra-opts
          (append (list 'debug_info) erlang-compile-extra-opts))
         (module-name
          (erlang-add-quotes-if-needed
           (erlang-eunit-module-name buffer-file-name)))
         (compile-command
          (format "cover:compile_beam(%s)." module-name)))
    (erlang-compile)
    (if (erlang-eunit-last-compilation-successful-p)
        (erlang-eunit-inferior-erlang-send-command compile-command))))

(defun erlang-eunit-analyze-coverage ()
  "Analyze the data collected by cover tool for the module in the
current buffer.

Assumes that the module has been cover compiled prior to this
call.  This function will do two things: print the number of
covered and uncovered functions in the erlang shell and display a
new buffer called *<module name> coverage* which shows the source
code along with the coverage analysis results."
  (interactive)
  (let* ((module-name     (erlang-add-quotes-if-needed
                           (erlang-eunit-module-name buffer-file-name)))
         (tmp-filename    (make-temp-file "cover"))
         (analyze-command (format "cover:analyze_to_file(%s, \"%s\"). "
                                  module-name tmp-filename))
         (buf-name        (format "*%s coverage*" module-name)))
    (erlang-eunit-inferior-erlang-send-command analyze-command)
    ;; The purpose of the following snippet is to get the result of the
    ;; analysis from a file into a new buffer (or an old, if one with
    ;; the specified name already exists).  Also we want the erlang-mode
    ;; *and* view-mode to be enabled.
    (save-excursion
      (let ((buf (get-buffer-create (format "*%s coverage*" module-name))))
        (set-buffer buf)
        (setq buffer-read-only nil)
        (insert-file-contents tmp-filename nil nil nil t)
        (if (= (buffer-size) 0)
            (kill-buffer buf)
          ;; FIXME: this would be a good place to enable (emacs-mode)
          ;;        to get some nice syntax highlighting in the
          ;;        coverage report, but it doesn't play well with
          ;;        flymake.  Leave it off for now.
          (view-buffer buf))))
    (delete-file tmp-filename)))

(defun erlang-eunit-compile-and-run-current-test ()
  "Compile the source and test files and run the current EUnit test.

With prefix arg, compiles for debug and runs tests with the verbose flag set."
  (interactive)
  (let ((module-name (erlang-add-quotes-if-needed
                      (erlang-eunit-module-name buffer-file-name)))
        (test-name (erlang-eunit-current-test)))
    (erlang-eunit-compile-and-test
     'erlang-eunit-run-test (list module-name test-name))))

(defun erlang-eunit-compile-and-run-module-tests ()
  "Compile the source and test files and run all EUnit tests in the module.

With prefix arg, compiles for debug and runs tests with the verbose flag set."
  (interactive)
  (let ((module-name (erlang-add-quotes-if-needed
                      (erlang-eunit-source-module-name buffer-file-name))))
    (erlang-eunit-compile-and-test
     'erlang-eunit-run-module-tests (list module-name))))

;;; Compile source and EUnit test file and finally run EUnit tests for
;;; the current module
(defun erlang-eunit-compile-and-test (test-fun test-args &optional under-cover)
   "Compile the source and test files and run the EUnit test suite.

If under-cover is set to t, the module under test is compile for
code coverage analysis.  If under-cover is left out or not set,
coverage analysis is disabled.  The result of the code coverage
is both printed to the erlang shell (the number of covered vs
uncovered functions in a module) and written to a buffer called
*<module> coverage* (which shows the source code for the module
and the number of times each line is covered).
With prefix arg, compiles for debug and runs tests with the verbose flag set."
  (erlang-eunit-record-recent-compile under-cover)
  (let ((src-filename  (erlang-eunit-src-filename  buffer-file-name))
	(test-filename (erlang-eunit-test-filename buffer-file-name)))

    ;; The purpose of out-maneuvering `save-some-buffers', as is done
    ;; below, is to ask the question about saving buffers only once,
    ;; instead of possibly several: one for each file to compile,
    ;; for instance for both x.erl and x_tests.erl.
    (save-some-buffers erlang-eunit-autosave)
    (flet ((save-some-buffers (&optional any) nil))

      ;; Compilation of the source file is mandatory (the file must
      ;; exist, otherwise the procedure is aborted).  Compilation of the
      ;; test file on the other hand, is optional, since eunit tests may
      ;; be placed in the source file instead.  Any compilation error
      ;; will prevent the subsequent steps to be run (hence the `and')
      (and (erlang-eunit-compile-file src-filename under-cover)
	   (if (file-readable-p test-filename)
	       (erlang-eunit-compile-file test-filename)
	     t)
           (apply test-fun test-args)
           (if under-cover
               (save-excursion
                 (set-buffer (find-file-noselect src-filename))
                 (erlang-eunit-analyze-coverage)))))))

(defun erlang-eunit-compile-and-run-module-tests-under-cover ()
  "Compile the source and test files and run the EUnit test suite and measure
code coverage.

With prefix arg, compiles for debug and runs tests with the verbose flag set."
  (interactive)
  (let ((module-name (erlang-add-quotes-if-needed
                      (erlang-eunit-source-module-name buffer-file-name))))
    (erlang-eunit-compile-and-test
     'erlang-eunit-run-module-tests (list module-name) t)))

(defun erlang-eunit-compile-file (file-path &optional under-cover)
  (if (file-readable-p file-path)
      (save-excursion
        (set-buffer (find-file-noselect file-path))
        ;; In order to run a code coverage analysis on a
        ;; module, we have two options:
        ;;
        ;; * either compile the module with cover:compile instead of the
        ;;   regular compiler
        ;;
        ;; * or first compile the module with the regular compiler (but
        ;;   *with* debug_info) and then compile it for coverage
        ;;   analysis using cover:compile_beam.
        ;;
        ;; We could accomplish the first by changing the
        ;; erlang-compile-erlang-function to cover:compile, but there's
        ;; a risk that that's used for other purposes.  Therefore, a
        ;; safer alternative (although with more steps) is to add
        ;; debug_info to the list of compiler options and go for the
        ;; second alternative.
        (if under-cover
            (erlang-eunit-cover-compile)
          (erlang-compile))
        (erlang-eunit-last-compilation-successful-p))
    (let ((msg (format "Could not read %s" file-path)))
      (erlang-eunit-inferior-erlang-send-command
       (format "%% WARNING: %s" msg))
      (error msg))))

(defun erlang-eunit-last-compilation-successful-p ()
  (save-excursion
    (set-buffer inferior-erlang-buffer)
    (goto-char compilation-parsing-end)
    (erlang-eunit-all-list-elems-fulfill-p
     (lambda (re) (let ((continue t)
			(result   t))
		    (while continue ; ignore warnings, stop at errors
		      (if (re-search-forward re (point-max) t)
			  (if (erlang-eunit-is-compilation-warning)
			      t
			    (setq result nil)
			    (setq continue nil))
			(setq result t)
			(setq continue nil)))
		    result))
     (mapcar (lambda (e) (car e)) erlang-error-regexp-alist))))

(defun erlang-eunit-is-compilation-warning ()
  (erlang-eunit-string-match-p
   "[0-9]+: Warning:"
   (buffer-substring (line-beginning-position) (line-end-position))))

(defun erlang-eunit-all-list-elems-fulfill-p (pred list)
  (let ((matches-p t))
    (while (and list matches-p)
      (if (not (funcall pred (car list)))
	  (setq matches-p nil))
      (setq list (cdr list)))
    matches-p))

;;; Evaluate a command in an erlang buffer
(defun erlang-eunit-inferior-erlang-send-command (command)
  "Evaluate a command in an erlang buffer."
  (interactive "P")
  (inferior-erlang-prepare-for-input)
  (inferior-erlang-send-command command)
  (sit-for 0) ;; redisplay
  (inferior-erlang-wait-prompt))


;;;====================================================================
;;; Key bindings
;;;====================================================================

(defconst erlang-eunit-key-bindings
  '(("\C-c\C-et" erlang-eunit-toggle-src-and-test-file-other-window)
    ("\C-c\C-ek" erlang-eunit-compile-and-run-module-tests)
    ("\C-c\C-ej" erlang-eunit-compile-and-run-current-test)
    ("\C-c\C-el" erlang-eunit-compile-and-run-recent)
    ("\C-c\C-ec" erlang-eunit-compile-and-run-module-tests-under-cover)
    ("\C-c\C-ev" erlang-eunit-cover-compile)
    ("\C-c\C-ea" erlang-eunit-analyze-coverage)))

(defun erlang-eunit-add-key-bindings ()
  (dolist (binding erlang-eunit-key-bindings)
    (erlang-eunit-bind-key (car binding) (cadr binding))))

(defun erlang-eunit-bind-key (key function)
  (erlang-eunit-ensure-keymap-for-key key)
  (local-set-key key function))

(defun erlang-eunit-ensure-keymap-for-key (key-seq)
  (let ((prefix-keys (butlast (append key-seq nil)))
	(prefix-seq  ""))
    (while prefix-keys
      (setq prefix-seq (concat prefix-seq (make-string 1 (car prefix-keys))))
      (setq prefix-keys (cdr prefix-keys))
      (if (not (keymapp (lookup-key (current-local-map) prefix-seq)))
	  (local-set-key prefix-seq (make-sparse-keymap))))))

(add-hook 'erlang-mode-hook 'erlang-eunit-add-key-bindings)


(provide 'erlang-eunit)
;; erlang-eunit ends here
