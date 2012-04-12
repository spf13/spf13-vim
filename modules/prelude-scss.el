(require 'prelude-css)

(defun prelude-scss-mode-defaults ()
  (prelude-css-mode-hook)
  ;; turn off annoying auto-compile on save
  (setq scss-compile-at-save nil))

(setq prelude-scss-mode-hook 'prelude-scss-mode-defaults)

(add-hook 'scss-mode-hook (lambda () (run-hooks 'prelude-scss-mode-hook)))

(provide 'prelude-scss)
