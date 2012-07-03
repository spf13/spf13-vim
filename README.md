```
 ____           _           _
|  _ \ _ __ ___| |_   _  __| | ___
| |_) | '__/ _ \ | | | |/ _` |/ _ \
|  __/| | |  __/ | |_| | (_| |  __/
|_|   |_|  \___|_|\__,_|\__,_|\___|
```

Emacs is probably the best text editor in the world. However, the
process of coming up with a useful Emacs configuration is long and
difficult. It's this process that separates you from truly taking
advantage of Emacs's power. I like to refer to this process as the
**Prelude**. **Prelude** has the goal to ease the initial Emacs setup
process and to provide you with a much more powerful and productive
experience than you get out of the box. By using **Prelude**
you're basically getting a "Get me out of the Prelude, I just want to
use Emacs" card.

**Prelude** is compatible **ONLY with GNU Emacs 24**. 

## Fast Forward

Assuming you're using an Unix-like OS (`*BSD`, `GNU/Linux`, `OS X`, `Solaris`,
etc), you already have Emacs 24 installed, as well as `git` & `curl` you
can skip the whole manual and just type in your favorite shell the
following command:

`curl -L
https://github.com/bbatsov/prelude/raw/master/utils/installer.sh
| sh`

You can now power up your Emacs, sit back and enjoy Prelude.

There are two environment variables you can use to control the 
source repository and the installation directory. To change the 
installation directory:

`PRELUDE_INSTALL_DIR="$HOME/.emacs.d" && 
 curl -L
https://github.com/bbatsov/prelude/raw/master/utils/installer.sh
| sh`

To change the source repository:

`PRELUDE_URL="https://github.com/yourname/prelude.git" &&
 curl -L
https://github.com/bbatsov/prelude/raw/master/utils/installer.sh
| sh`

Note that the installer will back up any existing `.emacs` file or
`.emacs.d` since it will unpack Prelude's code in `.emacs.d`. If
you're doing a manual install make sure you don't have a `.emacs` file
or back up your existing `.emacs.d` directory manually.

## Would you like to know more?

Check out the [Prelude's project page](http://batsov.com/prelude) for
all the gory details.

If you're looking for more info on Emacs in general - consult
[WikEmacs](http://wikemacs.org).

## More goodies

The [Prelude Modules](https://github.com/bbatsov/prelude-modules)
project contains a lot of additional packages for Prelude
(install-able via the `package-list-packages` command) - enhanced programming
mode configs, latex config, erc config, etc.

## Known issues

Check out the project's
[issue list](https://github.com/bbatsov/prelude/issues?sort=created&direction=desc&state=open)
a list of unresolved issues. By the way - feel free to fix any of them
and sent me a pull request. :-)

## Contributors

Here's a [list](https://github.com/bbatsov/prelude/contributors) of all the people who have contributed to the
development of Emacs Prelude.

## Bugs & Improvements

Bug reports and suggestions for improvements are always
welcome. GitHub pull requests are even better! :-)

Cheers,<br>
Bozhidar
