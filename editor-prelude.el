;; Emacs users obviously have little need for Command and Option keys,
;; but they do need Meta and Super
(when (string= system-type "darwin") 
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta))

;; Death to the tabs!
(setq-default indent-tabs-mode nil)

;; delete the selection with a keypress
(delete-selection-mode t)
;; highlight when searching and replacing
(setq search-highlight t                 
      query-replace-highlight t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; saveplace: save location in file when saving files
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)            ;; activate it for all buffers
(require 'saveplace)                   ;; get the package

;; savehist: save some history
(setq savehist-additional-variables    ;; also save...
      '(search ring regexp-search-ring)    ;; ... my search entries
      savehist-autosave-interval 60        ;; save every minute (default: 5 min)
      savehist-file (concat "~/.emacs.d" "/savehist"))   ;; keep my home clean
(savehist-mode t)                      ;; do customization before activation

;; save recent files
(setq recentf-save-file (concat dotfiles-dir "recentf") ;; keep ~/ clean
      recentf-max-saved-items 100          ;; max save 100
      recentf-max-menu-items 15)         ;; max 15 in menu
(recentf-mode t)                  ;; turn it on

;; time-stamps
;; when there's "Time-stamp: <>" in the first 10 lines of the file
(setq
 time-stamp-active t        ; do enable time-stamps
 time-stamp-line-limit 10   ; check first 10 buffer lines for Time-stamp: <>
 time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ; date format
(add-hook 'write-file-hooks 'time-stamp) ; update when saving

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings 'super)

;; show-paren-mode: subtle highlighting of matching parens
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;; tramp, for sudo access
(require 'tramp)
;; keep in mind known issues with zsh - see emacs wiki
(setq tramp-default-method "ssh")

;; now we can use tramp to open files
;; requiring root access
(defun find-alternative-file-with-sudo ()
  "Open current buffer as root!"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     sudo/su can be used as well, but they
     do not work for me
     (concat "/ssh:root@localhost:"
             buffer-file-name))))

;; ido-mode
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-default-file-method 'selected-window)

;; auto-completion in minibuffer
(icomplete-mode +1)

(set-default 'imenu-auto-rescan t)

;; flyspell-mode
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(add-hook 'message-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(defun turn-on-flyspell ()
  "Force flyspell-mode on using a positive argument.  For use in hooks."
  (interactive)
  (flyspell-mode 1))

;; enable narrow to region
(put 'narrow-to-region 'disabled nil)

;; bookmarks
(setq bookmark-default-file "~/.emacs.d/bookmarks" 
      bookmark-save-flag 1)                        

(provide 'editor-prelude)
