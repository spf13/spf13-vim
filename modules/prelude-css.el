(eval-after-load 'css-mode
  '(progn
     (defun prelude-css-mode-defaults ()
       (setq css-indent-offset 2)
       (rainbow-mode +1))

     (setq prelude-css-mode-hook 'prelude-css-mode-defaults)

     (add-hook 'css-mode-hook (lambda () (run-hooks 'prelude-css-mode-hook)))))

(provide 'prelude-css)
