Emacs Prelude
=============

Prelude is an Emacs distribution that aims to enhance the default
Emacs experience.  Prelude alters a lot of the default settings,
bundles a plethora of additional packages and adds its own core
library to the mix. The final product offers an easy to use Emacs
configuration for Emacs newcomers and lots of additional power for
Emacs power users.

Prelude is compatible **ONLY with GNU Emacs 24.x**. In general you're
advised to always run Prelude with the latest Emacs - currently
**24.3**.

## Fast Forward

Assuming you're using an Unix-like OS (`*BSD`, `GNU/Linux`, `OS X`, `Solaris`,
etc), you already have Emacs 24 installed, as well as `git` & `curl` you
can skip the whole manual and just type in your favorite shell the
following command:

```bash
curl -L http://git.io/epre | sh
```

You can now power up your Emacs, sit back and enjoy Prelude,
forgetting about the rest of this manual.

There are two environment variables you can use to control the
source repository and the installation directory. To change the
installation directory:

```bash
export PRELUDE_INSTALL_DIR="$HOME/.emacs.d" && curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh | sh
```

To change the source repository:

```bash
export PRELUDE_URL="https://github.com/yourname/prelude.git" && curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh | sh
```

Note that the installer will back up any existing `.emacs` file or
`.emacs.d` since it will unpack Prelude's code in `.emacs.d`. If
you're doing a manual install make sure you don't have a `.emacs` file
or back up your existing `.emacs.d` directory manually.

Don't forget to adjust your `prelude-modules.el` file once the installation is done.
By default most of the modules that ship with Prelude are not loaded.

## Installing Emacs 24

Obviously to use the Emacs Prelude you have to install Emacs 24
first. Have a look at the [WikEmacs articles on installing Emacs](http://wikemacs.org/wiki/Installing_Emacs).

## Installation

### Automated

You can install **Emacs Prelude** via the command line with either `curl` or
`wget`. Naturally `git` is also required.

#### Via Curl

If you're using `curl` type the following command:

```bash
curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh | sh
```

#### Via Wget

If you're using `wget` type:

```bash
wget --no-check-certificate https://github.com/bbatsov/prelude/raw/master/utils/installer.sh -O - | sh
```

### Manual

```bash
git clone git://github.com/bbatsov/prelude.git path/to/local/repo
ln -s path/to/local/repo ~/.emacs.d
cd ~/.emacs.d
```

You'd do well to replace `~/.emacs.d` with the value of
`user-emacs-directory` for your OS. You can check the value by doing
`C-h v user-emacs-directory` inside Emacs.

## Updating Prelude

The update procedure is fairly straightforward and consists of 3 steps:

### Update all bundled packages

Just run <kbd>M-x package-list-packages RET U x</kbd>. Unfortunately this step cannot be automated.

### Update Prelude's code

```bash
cd path/to/prelude/installation
git pull
```

The `path/to/prelude/installation` is usually `~/.emacs.d` (at least
on Unix systems).

Alternatively you can run <kbd>M-x prelude-update</kbd> from Emacs itself.

### Restart Prelude

It's generally a good idea to stop Emacs after you do the update. The
next time Prelude starts it will install any new dependencies (if
there are such).

## Enabling additional modules

By default most of the modules that ship with Prelude are not loaded. For more information on the functionality provided by these modules visit the [docs](modules/doc/README.md).

```lisp
;;; Uncomment the modules you'd like to use and restart Prelude afterwards

(require 'prelude-c)
;; (require 'prelude-clojure)
;; (require 'prelude-coffee)
;; (require 'prelude-common-lisp)
;; (require 'prelude-css)
(require 'prelude-emacs-lisp)
(require 'prelude-erc)
;; (require 'prelude-erlang)
;; (require 'prelude-haskell)
(require 'prelude-js)
;; (require 'prelude-latex)
(require 'prelude-lisp)
;; (require 'prelude-markdown)
;; (require 'prelude-mediawiki)
(require 'prelude-org)
(require 'prelude-perl)
;; (require 'prelude-python)
;; (require 'prelude-ruby)
;; (require 'prelude-scala)
(require 'prelude-scheme)
;; (require 'prelude-scss)
;; (require 'prelude-web)
(require 'prelude-xml)
```

You'll need to adjust your `prelude-modules.el` file once the
installation is done. If you are doing a manual install then you first
need to copy the `prelude-modules.el` available in the sample
directory to the root of `path/to/prelude/installation` and then
adjust that one.

After you've uncommented a module you should either restart Emacs or evaluate the module
`require` expression with <kbd>C-x C-e</kbd>.

## Running

Nothing fancy here. Just start Emacs as usual. Personally I run Emacs
in daemon mode:

```bash
emacs --daemon
```

Afterwards I connect to the server with either a terminal or a GUI
client like this:

```bash
emacsclient -t
emacsclient -c
```

You'd probably do well to put a few aliases in your `.zshrc` (or
`.bashrc`):

```bash
alias e=emacsclient -t
alias ec=emacsclient -c
alias vim=emacsclient -t
alias vi=emacsclient -t
```

The last two aliases are helpful if you're used to editing files from
the command line using `vi(m)`.

## Getting to know Prelude

Certainly the best way to understand how Prelude enhances the default
Emacs experience is to peruse Prelude's source code (which is
obviously written in Emacs Lisp). Understanding the code is not
necessary of course. Prelude includes a `prelude-mode` minor Emacs mode
which collects some of the additional functionality added by
Prelude. It also adds an additional keymap that binds many of those
extensions to keybindings.

### Keymap

#### Global

Keybinding         | Description
-------------------|------------------------------------------------------------
<kbd>C-M-h</kbd>   | Kill the previous word(`backward-kill-word`). (as in Bash/Zsh)
<kbd>C-x \\</kbd>   | `align-regexp`
<kbd>C-+</kbd>     | Increase font size(`text-scale-increase`).
<kbd>C--</kbd>     | Decrease font size(`text-scale-decrease`).
<kbd>C-x O</kbd>   | Go back to previous window (the inverse of `other-window` (`C-x o`)).
<kbd>C-^</kbd>     | Join two lines into one(`prelude-top-join-line`).
<kbd>C-x p</kbd>   | Start `proced` (manage processes from Emacs; works only in Linux).
<kbd>C-x m</kbd>   | Start `eshell`.
<kbd>C-x M-m</kbd> | Start your default shell.
<kbd>C-x C-m</kbd> | Alias for `M-x`.
<kbd>C-h A</kbd>   | Run `apropos` (search in all Emacs symbols).
<kbd>M-/</kbd>     | Run `hippie-expand` (a replacement for the default `dabbrev-expand`).
<kbd>C-x C-b</kbd> | Open `ibuffer` (a replacement for the default `buffer-list`).
<kbd>F11</kbd>     | Make the window full screen.
<kbd>F12</kbd>     | Toggle the Emacs menu bar.
<kbd>C-x g</kbd>   | Open Magit's status buffer.
<kbd>C-=</kbd>     | Run `expand-region` (incremental text selection).
<kbd>C-a</kbd>     | Run `prelude-move-beginning-of-line`. Read [this](http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/) for details.

#### Prelude Mode

Keybinding         | Description
-------------------|------------------------------------------------------------
<kbd>C-c o</kbd>   | Open the currently visited file with an external program.
<kbd>C-c i</kbd>   | Search for a symbol, only for buffers that contain code
<kbd>C-c g</kbd>   | Search in Google for the thing under point (or an interactive query).
<kbd>C-c G</kbd>   | Search in GitHub for the thing under point (or an interactive query).
<kbd>C-c y</kbd>   | Search in YouTube for the thing under point (or an interactive query).
<kbd>C-S-RET</kbd> or <kbd>M-o</kbd> | Insert an empty line above the current line and indent it properly
<kbd>S-RET</kbd> or <kbd>M-O</kbd> | Insert an empty line and indent it properly (as in most IDEs).
<kbd>C-S-up</kbd> or <kbd>M-S-up</kbd> | Move the current line or region up.
<kbd>C-S-down</kbd> or <kbd>M-S-down</kbd>| Move the current line or region down.
<kbd>C-c n</kbd> | Fix indentation in buffer and strip whitespace.
<kbd>C-c f</kbd> | Open recently visited file.
<kbd>C-M-\\</kbd> | Indent region (if selected) or the entire buffer.
<kbd>C-c u</kbd> | Open URL in your default browser.
<kbd>C-c e</kbd> | Eval a bit of Emacs Lisp code and replace it with its result.
<kbd>C-c s</kbd> | Swap two active windows.
<kbd>C-c d</kbd> | Duplicate the current line (or region).
<kbd>C-c M-d</kbd> | Duplicate and comment the current line (or region).
<kbd>C-c r</kbd> | Rename the currently visited file and buffer.
<kbd>C-c t</kbd> | Open a terminal emulator (`ansi-term`).
<kbd>C-c k</kbd> | Kill all open buffers except the one you're currently in.
<kbd>C-c h</kbd> | Open Helm (a useful means of navigating your buffers and project files).
<kbd>C-c +</kbd> | Increment integer at point.
<kbd>C-c -</kbd> | Decrement integer at point.
<kbd>Super-r</kbd> | Recent files
<kbd>Super-x</kbd> | Expand region
<kbd>Super-j</kbd> | Join lines
<kbd>Super-k</kbd> | Kill whole line
<kbd>Super-m m</kbd> | Magit status
<kbd>Super-m l</kbd> | Magit log
<kbd>Super-m f</kbd> | Magit file log
<kbd>Super-m b</kbd> | Magit blame mode
<kbd>Super-o</kbd> | Open line above current line

#### OSX modifier keys

Prelude does not mess by default with the standard mapping of `Command` (to `Super`) and `Option` (to `Meta`).

If you want to swap them add this to your personal config:

```lisp
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
```

You can also temporarily swap them with `C-c w` (`M-x prelude-swap-meta-and-super`).

#### Projectile

Here's a list of functionality provided by [Projectile](https://github.com/bbatsov/projectile):

Keybinding         | Description
-------------------|------------------------------------------------------------
<kbd>C-c p f</kbd> | Display a list of all files in the project. With a prefix argument it will clear the cache first.
<kbd>C-c p d</kbd> | Display a list of all directories in the project. With a prefix argument it will clear the cache first.
<kbd>C-c p T</kbd> | Display a list of all test files(specs, features, etc) in the project.
<kbd>C-c p g</kbd> | Run grep on the files in the project.
<kbd>C-c p b</kbd> | Display a list of all project buffers currently open.
<kbd>C-c p o</kbd> | Runs `multi-occur` on all project buffers currently open.
<kbd>C-c p r</kbd> | Runs interactive query-replace on all files in the projects.
<kbd>C-c p i</kbd> | Invalidates the project cache (if existing).
<kbd>C-c p R</kbd> | Regenerates the projects `TAGS` file.
<kbd>C-c p k</kbd> | Kills all project buffers.
<kbd>C-c p D</kbd> | Opens the root of the project in `dired`.
<kbd>C-c p e</kbd> | Shows a list of recently visited project files.
<kbd>C-c p a</kbd> | Runs `ack` on the project. Requires the presence of `ack-and-a-half`.
<kbd>C-c p c</kbd> | Runs a standard compilation command for your type of project.
<kbd>C-c p p</kbd> | Runs a standard test command for your type of project.
<kbd>C-c p z</kbd> | Adds the currently visited to the cache.
<kbd>C-c p s</kbd> | Display a list of known projects you can switch to.

Prelude adds some extra keybindings:

Keybinding         | Command
-------------------|------------------------------------------------------------
<kbd>Super-f</kbd> | Find file in project
<kbd>Super-d</kbd> | Find directory in project
<kbd>Super-g</kbd> | Run grep on project
<kbd>Super-p</kbd> | Switch projects

If you ever forget any of Projectile's keybindings just do a:

<kbd>C-c p C-h</kbd>

#### Key-chords

Keybinding         | Description
-------------------|----------------------------------------------
<kbd>jj</kbd>      | Jump to the beginning of a word(`ace-jump-word-mode`)
<kbd>jk</kbd>      | Jump to a character(`ace-jump-char-mode`)
<kbd>jl</kbd>      | Jump to the beginning of a line(`ace-jump-line-mode`)
<kbd>JJ</kbd>      | Jump back to previous buffer(`prelude-switch-to-previous-buffer`)
<kbd>uu</kbd>      | View edits as a tree(`undo-tree-visualize`)

##### Disabling key-chords

In some cases you may not want to have a key-chord that is defined by prelude,
in which case you can disable the binding in your `personal.el` file by setting
its command to `nil`. For example, to disable the `jj` key-chord add the
following line:

```lisp
(key-chord-define-global "jj" nil)
```

If you're an `evil-mode` user you'll probably do well to disable `key-chord-mode` altogether:

```lisp
(key-chord-mode -1)
```

## Automatic package installation

The default Prelude installation comes with a bare minimum of
functionality. It will however install add-ons for various programming
languages and frameworks on demand. For instance - if you try to open
a `.clj` file `clojure-mode`, `nrepl.el` and prelude's enhanced Lisp
configuration will be installed automatically for you.

You can, of course, install anything you wish manually as well.

### Color Themes

Emacs 24 ships with a new theming facility that effectively renders
the old color-theme package obsolete. Emacs 24 provides a dozen of
built-in themes you can use out-of-the-box by invoking the `M-x
load-theme` command.

[Zenburn](https://github.com/bbatsov/zenburn-emacs) is the default color theme in Prelude, but you can change it
at your discretion. Why Zenburn? I (and lots of hackers around the
world) find it pretty neat for some reason. Personally I find the
default theme pretty tiresome for the eyes, that's why I took that
"controversial" decision to replace it. You can, of course, easily go
back to the default (or select another theme entirely).

To disable Zenburn just put in your personal config the following
line:

```lisp
(disable-theme 'zenburn)
```

Or you can use another theme altogether by adding something like:

```lisp
(load-theme 'solarized-dark t)
```

**P.S.** Solarized is not available by default - you'll have to
  install it from MELPA first (`M-x package-install RET
  solarized-theme`).

### Personalizing

Fork the official Prelude repo and add your own touch to it. You're advised to avoid changing stuff outside of the
personal folder to avoid having to deal with git merge conflicts in the future.

If you'd like to add some auto installation of packages in your
personal config use the following code:

```lisp
(prelude-require-packages '(some-package some-other-package))
```

If you require just a single package you can also use:

```lisp
(prelude-require-package 'some-package)
```

#### Disabling whitespace-mode

Although `whitespace-mode` is awesome some people might find it too
intrusive. You can disable it in your
personal config with the following bit of code:

```lisp
(setq prelude-whitespace nil)
```

If you like `whitespace-mode` but prefer it to not automatically
cleanup your file on save, you can disable that behavior by setting
prelude-clean-whitespace-on-save to nil in your config file with:

```lisp
(setq prelude-clean-whitespace-on-save nil)
```

The prelude-clean-whitespace-on-save setting can also be set on a
per-file or directory basis by using a file variable or a
.dir-locals.el file.


#### Disable flyspell-mode

If you're not fond of spellchecking on the fly:

```lisp
(setq prelude-flyspell nil)
```

## Caveats & Pitfalls

### Updating bundled packages

Currently there is no Emacs Lisp API for updating packages, so you'll
have to update manually the packages that came with Prelude from time
to time.

`M-x package-list-packages RET U x`

Generally it's a good idea to do a package update before running
`prelude-update`, since the latest Prelude code might depend on newer
versions of the bundled packages than you would currently have
installed.

### Problems with flyspell-mode

Prelude makes heavy use of the flyspell-mode package for spell
checking of various things. The proper operation of flyspell depends
on the presence of the `aspell` program and an `en` dictionary on your
system. You can install `aspell` and the dictionary on OS X with
`homebrew` like this:

```bash
brew install aspell --with-lang=en
```

On Linux distros - just use your distro's package manager.

### Ugly colors in the terminal Emacs version

If your Emacs looks considerably uglier in a terminal (compared to the
GUI version) try adding this to your `.bashrc` or `.zshrc`:

```bash
export TERM=xterm-256color
```

Source the `.bashrc` file and start Emacs again.

### MELPA error on initial startup

If you get some http connection error related to the MELPA repo
just do a manual `M-x package-refresh-contents` and restart Emacs
afterwards.

### No arrow navigation in editor buffers

This is not a bug - it's a feature! I firmly believe that the one true
way to use Emacs is by using it the way it was intended to be used (as
far as navigation is concerned at least). That's why I've disabled all
movement commands with arrows (and keys like page up, page down, etc) - to prevent you from being tempted to
use them.

If you'd still like to use the arrow keys just invoke `M-x
guru-mode` to enable them for the duration of your
current Emacs session or add the following snippet to your
personal Emacs customization to enable them permanently:

```lisp
(setq prelude-guru nil)
```

### Customized C-a behavior

Prelude overrides `C-a` to behave as described
[here](http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/). If
you don't like that simply add this to your personal config:

```lisp
(global-set-key [remap move-beginning-of-line]
                'move-beginning-of-line)
```

### Poor ido matching performance on large datasets

Prelude swaps the default `ido` flex matching with the more powerful
[ido-flx](https://github.com/lewang/flx).

The sorting algorithm `flx` uses is more complex, but yields better results.

On slower machines, it may be necessary to lower `flx-ido-threshhold` to
ensure a smooth experience.

```lisp
(setq flx-ido-threshhold 1000)
```

You can always disable the improved sorting algorithm all together like this:

```lisp
(flx-ido-mode -1)
```

### Windows compatibility

While everything in Prelude should work fine in Windows, I test it only
with Linux & OSX, so there are Windows related problems from time to
time. This situation will probably improve over time.

## Share the knowledge

[WikEmacs](http://wikemacs.org) collects useful resources for working
with GNU Emacs. Please, take the time to peruse and improve them as
you accumulate knowledge about Emacs. Prelude makes this especially
easy, since it bundles
[MediaWiki support](http://wikemacs.org/wiki/Mediawiki.el) + the
settings required to access WikEmacs right away.

## Known issues

Check out the project's
[issue list](https://github.com/bbatsov/prelude/issues?sort=created&direction=desc&state=open)
a list of unresolved issues. By the way - feel free to fix any of them
and send me a pull request. :-)

## Support

Support is available via the Prelude Google Group <emacs-prelude@googlegroups.com>.

## Contributors

Here's a [list](https://github.com/bbatsov/prelude/contributors) of all the people who have contributed to the
development of Emacs Prelude.

## Bugs & Improvements

Bug reports and suggestions for improvements are always
welcome. GitHub pull requests are even better! :-)

Cheers,<br/>
[Bozhidar](https://twitter.com/bbatsov)
