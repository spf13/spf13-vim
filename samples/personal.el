
;; example_el  -- Rename to example.el to activate, and restart emacs

;; Here are some examples of how to override the defaults for the
;; various prelude-emacs settings.  To *append* to any of the
;; configurations attached to prelude-*-hooks, you can attach a
;; function to the appropriate hook:

(add-hook 'prelude-prog-mode-hook
          (lambda ()
            (prelude-turn-off-whitespace)
            (remove-hook 'before-save-hook 'whitespace-cleanup)) t)

;; For other global settings, just run the appropriate function; all
;; personal/*.el files will be evaluate after prelude-emacs is loaded.

(global-hl-line-mode -1)
(blink-cursor-mode t)
