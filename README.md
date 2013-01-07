Emacs Prelude
=============

Emacs is probably the best text editor in the world. However, the
process of coming up with a useful Emacs configuration is long and
difficult. It's this process that separates you from truly taking
advantage of Emacs's power. I like to refer to this process as the
**Prelude**. **Emacs Prelude** has the goal to ease the initial Emacs
setup process and to provide you with a much more powerful and
productive experience than the one you get out of the box. By using
**Emacs Prelude** you're basically getting a *"Get me out of the
Prelude, I just want to use Emacs"* card.

Emacs Prelude is compatible **ONLY with GNU Emacs 24.x**.

## Fast Forward

Assuming you're using an Unix-like OS (`*BSD`, `GNU/Linux`, `OS X`, `Solaris`,
etc), you already have Emacs 24 installed, as well as `git` & `curl` you
can skip the whole manual and just type in your favorite shell the
following command:

```bash
$ curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh | sh
```

You can now power up your Emacs, sit back and enjoy Prelude,
forgetting about the rest of this manual.

There are two environment variables you can use to control the
source repository and the installation directory. To change the
installation directory:

```bash
$ export PRELUDE_INSTALL_DIR="$HOME/.emacs.d" && curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh | sh
```

To change the source repository:

```bash
$ export PRELUDE_URL="https://github.com/yourname/prelude.git" && curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh | sh
```

Note that the installer will back up any existing `.emacs` file or
`.emacs.d` since it will unpack Prelude's code in `.emacs.d`. If
you're doing a manual install make sure you don't have a `.emacs` file
or back up your existing `.emacs.d` directory manually.

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
$ curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh | sh
```

#### Via Wget

If you're using `wget` type:

```bash
$ wget --no-check-certificate https://github.com/bbatsov/prelude/raw/master/utils/installer.sh -O - | sh
```

### Manual

```bash
$ git clone git://github.com/bbatsov/prelude.git path/to/local/repo
$ ln -s path/to/local/repo ~/.emacs.d
$ cd ~/emacs.d
```

You'd do well to replace `~/.emacs.d` with the value of
`user-emacs-directory` for your OS. You can check the value by doing
`C-h v user-emacs-directory` inside Emacs.

## Running

Nothing fancy here. Just start Emacs as usual. Personally I run Emacs
in daemon mode:

```bash
$ emacs --daemon
```

Afterwards I connect to the server with either a terminal or a GUI
client like this:

```bash
$ emacsclient -t
$ emacsclient -c
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

* `C-M-h` - `backward-kill-word` (as in Bash/Zsh)
* `C-x \` - `align-regexp`
* `C-+` - `text-scale-increase`
* `C--` - `text-scale-decrease`
* `C-x O` - return you to the previous window (the inverse of `other-window` (`C-x o`))
* `C-x ^` - `join-line`
* `C-x p` - `proced` (manage processes form Emacs, works only in Linux)
* `C-x m` - start eshell
* `C-x M-m` - start your default shell
* `C-x C-m` - sames as `M-x`
* `C-h A` - `apropos` (search in all Emacs symbols)
* `M-\` - `hippie-expand` (a replacement for the default `dabbrev-expand`)
* `C-x C-b` - `ibuffer` (a replacement for the default `buffer-list`)
* `F12` - toggle the Emacs menu bar
* `C-x g` - open Magit's status buffer
* `C-=` - `expand-region` (incremental text selection)

#### Prelude Mode

* `C-c o` - open the currently visited file with external program
* `C-c g` - search in Google for the thing under point (or an interactive query)
* `shift+return` - insert an empty line and indent it properly (as in most IDEs)
* `control+shift+up` - move the current line up
* `control+shift+down` - move the current line down
* `C-c n` - fix indentation in buffer and strip whitespace
* `C-c f` - open recently visitted file
* `C-M-\` - indent region (if selected) or the entire buffer
* `C-c u` - open URL in your default browser
* `C-c e` - eval a bit of Emacs Lisp code and replace it with its result
* `C-c s` - swap two active windows
* `C-c d` - duplicate the current line (or region)
* `C-c r` - rename the currently visited file and buffer
* `C-c t` - open a terminal emulator (`ansi-term`)
* `C-c k` - kill all open buffers except the one you're currently in
* `C-c h` - open Helm (a useful means of navigating your buffers and project files)

#### Projectile

Here's a list of the interactive Emacs Lisp functions, provided by projectile:

* `projectile-find-file` <kbd>C-c p f</kbd>
* `projectile-grep` <kbd>C-c p g</kbd>
* `projectile-switch-to-buffer` <kbd>C-c p b</kbd>
* `projectile-multi-occur` <kbd>C-c p o</kbd>
* `projectile-replace` <kbd>C-c p r</kbd>
* `projectile-invalidate-cache` <kbd>C-c p i</kbd>
* `projectile-regenerate-tags` <kbd>C-c p t</kbd>
* `projectile-kill-buffers` <kbd>C-c p k</kbd>
* `projectile-dired` <kbd>C-c p d</kbd>
* `projectile-recentf` <kbd>C-c p e</kbd>
* `projectile-ack` <kbd>C-c p a</kbd>
* `projectile-compile-project` <kbd>C-c p l</kbd>
* `projectile-test-project` <kbd>C-c p p</kbd>

### Automatic package installation

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

P.S. Solarized is not available by default - you'll have to install it from MELPA first.

### Personalizing

Fork the official Prelude repo and add your own touch to it. You're advised to avoid changing stuff outside of the
personal folder to avoid having to deal with git merge conflicts in the future.

#### Disabling whitespace-mode

Although `whitespace-mode` is awesome some people might find it too
intrusive. You can disable it in your
personal config with the following bit of code:

```lisp
(setq prelude-whitespace nil)
```

#### Disable flyspell-mode

If you're not fond of spellchecking on the fly:

```lisp
(setq prelude-flyspell nil)
```

## Caveats & Pitfalls

### Problems with flyspell-mode

Prelude makes heavy use of the flyspell-mode package for spell
checking of various things. The proper operation of flyspell depends
on the presence of the `aspell` program and an `en` dictionary on your
system. You can install `aspell` and the dictionary on OS X with
`homebrew` like this:

```bash
$ brew install aspell --lang=en
```

On Linux distros - just use your distro's package manager.

### Ugly colors in the terminal Emacs version

If your Emacs looks considerably uglier in a terminal (compared to the
GUI version) try adding this to your `.bashrc` or `.zshrc`:

```bash
$ export TERM=xterm-256color
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
Bozhidar
