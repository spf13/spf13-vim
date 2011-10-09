# Emacs Prelude

## Prelude

Emacs is probably the best text editor in the world. However, the
process of coming up with a useful Emacs configuration is long and
difficult. It's this process that separates you from truly taking
advantage of Emacs's power. I like to refer to this process as the
**Prelude**. The **Emacs Prelude** has the goal to ease the initial
Emacs setup process and to provide you with a much more moreful and
productive experience that you get out of the box. By using **Emacs
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
store for the Prelude.

## Enhanced language support

## Additional programming languages support

* Clojure
* CoffeeScript
* Haskell

## Additional markup languages support

* Markdown
* Sass
* Haml
* Yaml

## Misc

**Emacs Prelude** uses by default the Zenburn color theme (a personal
preference of me and many other hackers), but you can easily disable
(or replace) it.

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

## Known issues

None so far.

## Bugs & Improvements
Bug reports and suggestions for improvements are always welcome. github pull request are even better! ;-)

Bozhidar
