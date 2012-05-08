#!/bin/bash

PRELUDE_INSTALL_DIR="$HOME/.emacs.d"
PRELUDE_URL=https://github.com/bbatsov/prelude.git

echo -n "Checking to see if git is installed... "
hash git 2>&- || { echo >&2 "not found. Aborting installation!"; exit 1; }
echo "found"

echo -n "Checking to see if aspell is installed... "
hash aspell 2>&- || { echo >&2 "not found. Install aspell to benefit from flyspell-mode!"; }
echo "found"

echo -n "Checking to see if ack is installed... "
hash ack 2>&- || { echo >&2 "not found. You'll need it to use ack-and-a-half!"; }
echo "found"

echo -n "Looking for an existing Emacs config..."
if [ -d $PRELUDE_INSTALL_DIR ]
then
  echo "Found an existing $PRELUDE_INSTALL_DIR. Backing it up to $PRELUDE_INSTALL_DIR.pre-prelude"
  mv $PRELUDE_INSTALL_DIR $PRELUDE_INSTALL_DIR.pre-prelude
fi

if [ -f ~/.emacs ]
then
  echo "Found an existing ~/.emacs. Backing it up to ~/.emacs.pre-prelude"
  mv ~/.emacs ~/.emacs.pre-prelude
fi

echo -n "Cloning Emacs Prelude from GitHub... "
/usr/bin/env git clone $PRELUDE_URL $PRELUDE_INSTALL_DIR > /dev/null
cd $PRELUDE_INSTALL_DIR
echo "done."

echo -e '\e[34m ____           _           _      '
echo -e '\e[34m|  _ \ _ __ ___| |_   _  __| | ___ '
echo -e '\e[34m| |_) |  __/ _ \ | | | |/ _  |/ _ \'
echo -e '\e[34m|  __/| | |  __/ | |_| | (_| |  __/'
echo -e '\e[34m|_|   |_|  \___|_|\__,_|\__,_|\___|'


echo -e '\e[32m... is now installed and ready to do thy bidding!'
echo -e '\e[0m'
