;; the toolbar is just a waste of valuable screen estate
(tool-bar-mode -1)
;; the menu bar is mostly useless as well
;; but removing it under OS X doesn't make much sense
(unless (string= system-type "darwin")
  (menu-bar-mode -1))
;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)                    
(column-number-mode t)                  
(size-indication-mode t)                

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; custom Emacs 24 color themes support
(add-to-list 'custom-theme-load-path (concat dotfiles-dir "themes/"))
(load-theme 'zenburn t)

(provide 'ui-prelude)
