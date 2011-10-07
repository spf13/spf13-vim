(defvar prelude-dir "~/.emacs.d/")

(add-to-list 'load-path prelude-dir)

(require 'ui-prelude)
(require 'packages-prelude)
(require 'core-prelude)
(require 'editor-prelude)
(require 'global-keybindings-prelude)
