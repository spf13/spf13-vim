(autoload 'feature-mode "feature-mode" "Feature mode." t)
(add-to-list 'auto-mode-alist '("\\.feature$" . feature-mode))

(provide 'prelude-cucumber)
