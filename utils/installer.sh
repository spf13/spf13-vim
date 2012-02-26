#!/bin/bash

PRELUDE_INSTALL_DIR="$HOME/.emacs.d"
PRELUDE_URL=git://github.com/bbatsov/emacs-prelude.git

echo -n "Checking to see if git is installed... "
hash git 2>&- || { echo >&2 "not found.  Aborting installation!'."; exit 1; }
echo "found"

echo -n "Looking for an existing Emacs config..."
if [ -f $PRELUDE_INSTALL_DIR ]
then
  echo "Found $PRELUDE_INSTALL_DIR. Backing up to $PRELUDE_INSTALL_DIR.pre-prelude"
  mv $PRELUDE_INSTALL_DIR $PRELUDE_INSTALL_DIR.pre-prelude
fi

if [ -f ~/.emacs ]
then
  echo "Found ~/.emacs. Backing up to ~/.emacs.pre-prelude"
  mv ~/.emacs ~/.emacs.pre-prelude
fi


echo -n "Cloning Emacs Prelude from GitHub... "
/usr/bin/env git clone $PRELUDE_URL $PRELUDE_INSTALL_DIR > /dev/null
echo "done."

echo ' _____                            ____           _           _      '
echo '| ____|_ __ ___   __ _  ___ ___  |  _ \ _ __ ___| |_   _  __| | ___ '
echo '|  _| |  _   _ \ / _  |/ __/ __| | |_) |  __/ _ \ | | | |/ _  |/ _ \'
echo '| |___| | | | | | (_| | (__\__ \ |  __/| | |  __/ | |_| | (_| |  __/'
echo '|_____|_| |_| |_|\__,_|\___|___/ |_|   |_|  \___|_|\__,_|\__,_|\___|'


echo '... is now installed!'
