[ -z "$PRELUDE_INSTALL_DIR" ] && PRELUDE_INSTALL_DIR="$HOME/.emacs.d"
[ -z "$PRELUDE_URL" ] && PRELUDE_URL=https://github.com/bbatsov/prelude.git

if [ -d $PRELUDE_INSTALL_DIR/prelude ]
then
    printf "\e[33m You already have Prelude installed.\e[0m You'll need to remove $PRELUDE_INSTALL_DIR/prelude if you want to install Prelude again.\n"
    exit 1;
fi

printf "Checking to see if git is installed... "
hash git 2>&- || { printf >&2 "\e[33mnot found. Aborting installation!\e[0m"; exit 1; }
printf "\e[32mfound.\e[0m\n"

printf "Checking to see if aspell is installed... "
hash aspell 2>&- || { printf >&2 "\e[33mnot found. Install aspell to benefit from flyspell-mode!\e[0m"; }
printf "\e[32mfound.\e[0m\n"

printf "Checking to see if ack is installed... "
hash ack 2>&- || { printf >&2 "\e[33mnot found. You'll need it to use ack-and-a-half!\e[0m"; }
printf "\e[32mfound.\e[0m\n"

printf "Looking for an existing Emacs config... "
if [ -d $PRELUDE_INSTALL_DIR ]
then
    printf "\e[33mfound an existing $PRELUDE_INSTALL_DIR.\e[0m Backing it up to $PRELUDE_INSTALL_DIR.pre-prelude.\n"
    mv $PRELUDE_INSTALL_DIR $PRELUDE_INSTALL_DIR.pre-prelude
elif [ -f ~/.emacs ]
then
    printf "\e[33mfound an existing ~/.emacs.\e[0m Backing it up to ~/.emacs.pre-prelude.\n"
    mv ~/.emacs ~/.emacs.pre-prelude
else
    printf "\e[32mnot found.\e[0m\n"
fi

printf "Cloning Emacs Prelude from GitHub... "
/usr/bin/env git clone $PRELUDE_URL $PRELUDE_INSTALL_DIR > /dev/null 2>&1
cd $PRELUDE_INSTALL_DIR
printf "done.\n"

if which emacs 2>&1 > /dev/null
then
    printf "Byte compiling Prelude... "
    emacs -batch -f batch-byte-compile $PRELUDE_INSTALL_DIR/prelude/*.el
    printf "done.\n\n"
fi

printf "\e[34m ____           _           _       \n"
printf "\e[34m|  _ \ _ __ ___| |_   _  __| | ___  \n"
printf "\e[34m| |_) |  __/ _ \ | | | |/ _  |/ _ \ \n"
printf "\e[34m|  __/| | |  __/ | |_| | (_| |  __/ \n"
printf "\e[34m|_|   |_|  \___|_|\__,_|\__,_|\___| \n"

printf "\e[32m... is now installed and ready to do thy bidding, Master $USER!\n"
printf "\e[0m"
