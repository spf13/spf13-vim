# Emacs Prelude

## Prelude

Emacs is probably the best text editor in the world. However, the
process of coming up with a useful Emacs configuration is long and
difficult. It's this process that separates you from truly taking
advantage of Emacs's power. I like to refer to this process as the
**Prelude**. The **Emacs Prelude** has the goal to ease the initial
Emacs setup process and to provide you with a much more powerful and
productive experience than that you get out of the box. By using **Emacs
Prelude** you're basically getting a "Get me out of the Prelude, I
just want to use Emacs" card.

Emacs Prelude is compatible **ONLY with GNU Emacs 24**. While Emacs 24
is not yet officially released it's a rock solid piece of software
more than suitable for everyday work. There is no good excuse not to
use Emacs 24!

Emacs Prelude is not the only reusable Emacs config out there - the
Emacs Starter Kit is fairly popular and there is the Emacs Dev Kit
that I used to maintain. I've decided to abandon the Emacs Dev Kit for
the Emacs Prelude for two reasons - the unfortunate choice a name (too
similar to Emacs Starter Kit) and the totally new philosophy I have in
store for the Prelude (easy to update, easy to personalize, easy to
extend, highly modular, highly comprehensible).

## Getting Emacs 24

Obviously to use the Emacs Prelude you have to install Emacs 24
first. Here's a few tips on doing so:

### OS X

Obtaining Emacs 24 on OS X is really simple. There are two popular
ways to do it. The first is to simply download a pretest (or a nightly
build) from [Emacs for OSX](http://emacsformacosx.com). My personal
recommendation would be to get the latest pretest (which is ironically
the first pretest as well) from
[here](http://emacsformacosx.com/emacs-builds/Emacs-pretest-24.0.90-universal-10.6.7.dmg).

That was really easy, right?

The second easy way to obtain Emacs 24 is via
[homebrew](http://mxcl.github.com/homebrew/). Just type the following
incantation in your shell and you're done:

```
$ brew install emacs --cocoa --use-git-head --HEAD
$ cp -r /usr/local/Cellar/emacs/HEAD/Emacs.app /Applications/
```

The second step is optional, but it's recommended if you like to start
Emacs from the launchpad or from Spotlight. Personally I prefer to
start Emacs in daemon mode (emacs --daemon), so that I could share a
single Emacs instance between several Emacs clients (emacsclient
-c/t).

That's all folk! You may now proceed to the configuration section.

### Linux

Given that Linux is more or less the home os of Emacs it presents us
with the most installation options. Of course, we can build Emacs from
[source](https://github.com/emacsmirror/emacs) on every distribution
out there, but I rarely bother to do so. Using the distribution's
package manager is a better idea for many reasons - you don't need to
install a build chain and lots of dev libraries, you get updated
versions when they are released and you get automated dependency
manager, just to name a few.

That said, few distributions include in their primary repositories
builds of Emacs 24. Luckily there are some unofficial repos that come
to the rescue.

Debian/Ubuntu users should look no further than the amazing
[emacs-snapshot APT repo](http://emacs.naquadah.org/). You'll find
installation instructions there for all the relevant Debian and Ubuntu
versions out there. High quality, highly recommended builds!

Gentoo users have even less to do, since Emacs 24 can be obtained via
the emacs-vcs package in portage, as noted in the official
[Emacs on Gentoo page](http://www.gentoo.org/proj/en/lisp/emacs/emacs.xml).

Unfortunately I wasn't able to find prebuilt Emacs 24 packages for any
of the RPM distros (Fedora, SUSE, Mandriva, etc). Since, I'm Debian
user I have to admit that I didn't look that far, but the source
installation is not particularly hard and is always an option.

### Windows

There are several ways to obtain precompiled Emacs 24 binaries if
you're a Windows users. The most popular are
[EmacsW32](http://ourcomments.org/cgi-bin/emacsw32-dl-latest.pl),
[Emacs for Windows](http://code.google.com/p/emacs-for-windows/) and
of course the official
[Emacs Windows builds](http://alpha.gnu.org/gnu/emacs/windows/). I've
,personally, never used any builds other than the official ones. The
unofficial builds usually include installers and various patches that
might be of use to some users.

Since I rarely use Windows I cannot give you any more advice on the
choice of a binary vendor.

## Enhanced programming experience

The following list will be expanded greatly in the future.

### Additional programming languages support

* Clojure
* CoffeeScript
* Haskell

### Additional markup languages support

* Markdown
* Sass
* Haml
* Yaml
* LaTeX

### Enhanced configuration

* C
* Common Lisp
* Ruby
* Scheme

## Enhanced productivity

## Bundled packages

* auctex (LaTeX editing)
* clojure-mode
* coffee-mode
* deft (note taking)
* gist (snippet sharing on github.com)
* haml-mode
* haskell-mode
* magit (enhanced git integration)
* markdown-mode
* paredit
* projectile (project management mode)
* sass-mode
* scss-mode
* yaml-mode
* yari (ri frontend)
* yasnippet

## Installation

    git clone git://github.com/bbatsov/emacs-prelude.git path/to/local/repo
    ln -s path/to/local/repo ~/.emacs.d

## Running

Nothing fancy here. Just start Emacs as usual. Personally I run Emacs
in daemon mode:

`emacs --daemon`

Afterwards I connect to the server with either a terminal or a GUI
client like this:

    emacsclient -t
    emacsclient -c

## Personalizing

If you'd like to change some of the setting in Prelude (or simply add
more) the proper way to do so would be to create Emacs Lisp files
under the **personal** directory in ~/.emacs.d. They will be loaded
automatically be Prelude on startup.

Avoid modifying the Prelude config itself - this will make it hard for
you to receive automatic updates in the future.

## Caveats & Pitfall

### No arrow navigation in editor buffers

This is not a bug - it's a feature! I firmly believe that the one true
way to use Emacs is by using it the way it was intended to be used (as
far as navigation is concerned at least). That's why I've disabled all
movement commands with arrows - to prevent you from being tempted to
use them.

### What is this terrible default theme?

It's called Zenburn and I (and lots of hackers around) the world find it
pretty neat (I also happen to be the maintainer of its Emacs port). I
find the default theme pretty tiresome for the eyes, that's why I took
that "controversial" decision to replace it. You can, of course,
easily go back to the default (or select another theme entirely).

## Known issues

None so far.

## Bugs & Improvements

Bug reports and suggestions for improvements are always
welcome. github pull requests are even better! ;-)

I'd like to include a nice variety of Emacs 24 themes into Prelude -
so if you've developed (or simply found) one - give me a shout and
I'll take a look at it.

Bozhidar
