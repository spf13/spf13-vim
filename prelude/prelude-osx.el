;; On OS X Emacs doesn't use the shell PATH if it's not started from
;; the shell.
;; If you're using homebrew or port, modifying the PATH is essential.
(let (osx-paths)
  (dolist (path '("/usr/local/bin" "/opt/local/bin" "/opt/local/sbin") (setenv "PATH" (concat osx-paths (getenv "PATH"))))
    (push path exec-path)
    (setq osx-paths (concat (concat path ":") osx-paths))))

;; Emacs users obviously have little need for Command and Option keys,
;; but they do need Meta and Super
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(provide 'prelude-osx)
