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
$ PRELUDE_INSTALL_DIR="$HOME/.emacs.d" && curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh | sh
```

To change the source repository:

```bash
$ PRELUDE_URL="https://github.com/yourname/prelude.git" && curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh | sh
```

Note that the installer will back up any existing `.emacs` file or
`.emacs.d` since it will unpack Prelude's code in `.emacs.d`. If
you're doing a manual install make sure you don't have a `.emacs` file
or back up your existing `.emacs.d` directory manually.

## More goodies

The [Prelude Modules](https://github.com/bbatsov/prelude-modules)
project contains a lot of additional packages for Prelude
(install-able via the `package-list-packages` command) - enhanced programming
mode configs, latex config, erc config, etc.

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

#### Disable whitespace-mode

Some people find `whitespace-mode` too intrusive and might want to
disable it. It come be done from your personal config with the
following bit of code:

```lisp
(add-hook 'prog-mode-hook 'whitespace-turn-off t)
```

#### Disable flyspell-mode

If you're not fond of spellchecking on the fly:

```lisp
(add-hook 'text-mode-hook 'turn-off-flyspell t)
(add-hook 'prog-mode-hook 'turn-off-flyspell t)
```

### Prelude Modules

Additional settings for various programming languages are available for installation via MELPA. You might take a look at the [Prelude Modules project](https://github.com/bbatsov/prelude-modules) for further info.

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
(add-hook 'prog-mode-hook 'turn-off-guru-mode t)
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
