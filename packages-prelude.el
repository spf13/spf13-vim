(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar prelude-packages '(auctex clojure-mode coffee-mode gist haml-mode
                                  haskell-mode magit markdown-mode paredit
                                  sass-mode yasnippet)
  "A list of packages to ensure are installed at launch.")

(dolist (p prelude-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'packages-prelude)
