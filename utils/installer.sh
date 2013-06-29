install_prelude () {
    printf " Cloning the Prelude's GitHub repository...\n$RESET"
    if [ x$PRELUDE_VERBOSE != x ]
    then
        /usr/bin/env git clone $PRELUDE_URL "$PRELUDE_INSTALL_DIR"
    else
        /usr/bin/env git clone $PRELUDE_URL "$PRELUDE_INSTALL_DIR" > /dev/null
    fi
    if ! [ $? -eq 0 ]
    then
        printf "$RED A fatal error occurred during Prelude's installation. Aborting..."
        exit 1
    fi
}

make_prelude_dirs () {
    printf " Making the required directories.\n$RESET"
    mkdir -p "$PRELUDE_INSTALL_DIR/vendor" "$PRELUDE_INSTALL_DIR/personal"
    mkdir -p "$PRELUDE_INSTALL_DIR/themes"
    mkdir -p "$PRELUDE_INSTALL_DIR/savefile"
}

colors () {
    # Reset
    RESET='\e[0m'
    RED='\e[0;31m'          # Red
    GREEN='\e[0;32m'        # Green
    YELLOW='\e[0;33m'       # Yellow
    BLUE='\e[0;34m'         # Blue
    PURPLE='\e[0;35m'       # Purple
    CYAN='\e[0;36m'         # Cyan
    WHITE='\e[0;37m'        # White

    # Bold
    BRED='\e[1;31m'         # Red
    BGREEN='\e[1;32m'       # Green
    BYELLOW='\e[1;33m'      # Yellow
    BBLUE='\e[1;34m'        # Blue
    BPURPLE='\e[1;35m'      # Purple
    BCYAN='\e[1;36m'        # Cyan
    BWHITE='\e[1;37m'       # White
}

# Commandline args:
# -d/--directory [dir]
#   Install prelude into the specified directory. If 'dir' is a relative path prefix it with $HOME.
#   Defaults to '$HOME/.emacs.d'
# -c/--colors
#   Enable colors
# -s/--source [url]
#   Clone prelude from 'url'.
#   Defaults to 'https://github.com/bbatsov/prelude.git'
# -i/--into
#   If one exists, install into the existing config
# -n/--no-bytecompile
#   Skip the compilation of the prelude files.
# -h/--help
#   Print help
# -v/--verbose
#   Verbose output, for debugging

usage() {
    printf "Usage: $0 [OPTION]\n"
    printf "  -c, --colors \t \t \t Enable colors.\n"
    printf "  -d, --directory [dir] \t Install prelude into the specified directory.\n"
    printf "  \t \t \t \t If 'dir' is a relative path prefix with $HOME.\n"
    printf "  \t \t \t \t Defaults to $HOME/.emacs.d\n"
    printf "  -s, --source [url] \t \t Clone prelude from 'url'.\n"
    printf "  \t \t \t \t Defaults to 'https://github.com/bbatsov/prelude.git'.\n"
    printf "  -n, --no-bytecompile \t \t Skip the bytecompilation step of prelude.\n"
    printf "  -i, --into \t \t \t Install Prelude into the existing configuration\n"
    printf "  \t \t \t \t The default behavious is to install prelude into the existing\n"
    printf "  \t \t \t \t emacs configuration.\n"
    printf "  -h, --help \t \t \t Display this help and exit\n"
    printf "  -v, --verbose \t \t Display verbose information\n"
    printf "\n"
}

### Parse cli
while [ $# -gt 0 ]
do
    case $1 in
        -d | --directory)
            PRELUDE_INSTALL_DIR=$2
            shift 2
            ;;
        -c | --colors)
            colors
            shift 1
            ;;
        -s | --source)
            PRELUDE_URL=$2
            shift 2
            ;;
        -i | --into)
            PRELUDE_INTO='true'
            shift 1
            ;;
        -n | --no-bytecompile)
            PRELUDE_SKIP_BC='true'
            shift 1
            ;;
        -h | --help)
            usage
            exit 0
            ;;
        -v | --verbose)
            echo "prelude verbose $PRELUDE_VERBOSE"
            PRELUDE_VERBOSE='true';
            shift 1
            ;;
        *)
            printf "Unkown option: $1\n"
            shift 1
            ;;
    esac
done

VERBOSE_COLOR=$BBLUE

[ -z $PRELUDE_URL ] && PRELUDE_URL="https://github.com/bbatsov/prelude.git"
[ -z "$PRELUDE_INSTALL_DIR" ] && PRELUDE_INSTALL_DIR="$HOME/.emacs.d"

if [ x$PRELUDE_VERBOSE != x ]
then
    printf "$PRELUDE_VERBOSE\n"
    printf "$VERBOSE_COLOR"
    printf "INSTALL_DIR = $PRELUDE_INSTALL_DIR\n"
    printf "SOURCE_URL  = $PRELUDE_URL\n"
    if [ -n $PRELUDE_SKIP_BC ]
    then
        printf "Skipping bytecompilation.\n"
    fi
    if [ -n $PRELUDE_INTO ]
    then
        printf "Replacing existing config (if one exists).\n"
    fi
    printf "$RESET"
fi

# If prelude is already installed
if [ -d "$PRELUDE_INSTALL_DIR/core/prelude-core.el" ]
then
    printf "\n\n$BRED"
    printf "You already have Prelude installed.$RESET\nYou'll need to remove $PRELUDE_INSTALL_DIR/prelude if you want to install Prelude again.\n"
    printf "If you want to update your copy of prelude, run 'git pull origin master' from your prelude directory\n\n"
    exit 1;
fi

### Check dependencies
printf  "$CYAN Checking to see if git is installed... $RESET"
if hash git 2>&-
then
    printf "$GREEN found.$RESET\n"
else
    printf "$RED not found. Aborting installation!$RESET\n"
    exit 1
fi;

printf  "$CYAN Checking to see if aspell is installed... "
if hash aspell 2>&-
then
    printf "$GREEN found.$RESET\n"
else
    print "$RED not found. Install aspell to benefit from flyspell-mode!$RESET\n"
fi

printf  "$CYAN Checking to see if ack is installed... "
if hash ack 2>&-
then
    printf "$GREEN found.$RESET\n"
else
    printf "$RED not found. You'll need it to use ack-and-a-half!$RESET\n"
fi

### Check emacs version
if [ $(emacs --version 2>/dev/null | sed -n 's/.*[^0-9.]\([0-9]*\.[0-9.]*\).*/\1/p;q' | sed 's/\..*//g') -lt 24 ]
then
    printf "$YELLOW WARNING:$RESET Prelude depends on emacs $RED 24$RESET !\n"
fi

if [ -d "$PRELUDE_INSTALL_DIR" ] || [ -f "$PRELUDE_INSTALL_DIR" ]
then
    # Existing file/directory found -> backup
    printf " Backing up the existing config to $PRELUDE_INSTALL_DIR.pre-prelude.tar.\n"
    tar -cf "$PRELUDE_INSTALL_DIR.pre-prelude.tar" "$PRELUDE_INSTALL_DIR" > /dev/null 2>&1
    # Overwrite existing?
    if [ -n $PRELUDE_INTO ]
    then
        # Replace existing config
        install_prelude
        make_prelude_dirs
    else
        # Install into existing config
        PRELUDE_INSTALL_DIR="$PRELUDE_INSTALL_DIR/prelude"
        install_prelude
    fi
elif [ -e "$PRELUDE_INSTALL_DIR" ]
then
    # File exist but not a regular file or directory
    # WTF NOW?
    printf "$BRED $PRELUDE_INSTALL_DIR exist but isn't a file or directory.\n"
    printf "$BRED please remove this file or install prelude in a different directory"
    printf "$BRED (-d flag)\n$RESET"
    exit 1
else
    # Nothing yet so just install prelude
    install_prelude
    make_prelude_dirs
    cp "$PRELUDE_INSTALL_DIR/sample/prelude-modules.el" "$PRELUDE_INSTALL_DIR"
fi

if [ -z $PRELUDE_SKIP_BC ];
then
    if which emacs 2>&1 > /dev/null
    then
        printf " Bytecompiling Prelude.\n"
        if [ x$PRELUDE_VERBOSE != x ]
        then
            emacs -batch -f batch-byte-compile "$PRELUDE_INSTALL_DIR/core/*.el"
        else
            emacs -batch -f batch-byte-compile "$PRELUDE_INSTALL_DIR/core/*.el" > /dev/null 2>&1
        fi
    else
        printf "$YELLOW Emacs not found.$RESET Skipping bytecompilation.\n"
    fi
else
    printf "Skipping bytecompilation.\n"
fi

printf "\n"
printf "$BBLUE  ____           _           _       \n"
printf "$BBLUE |  _ \ _ __ ___| |_   _  __| | ___  \n"
printf "$BBLUE | |_) |  __/ _ \ | | | |/ _  |/ _ \ \n"
printf "$BBLUE |  __/| | |  __/ | |_| | (_| |  __/ \n"
printf "$BBLUE |_|   |_|  \___|_|\__,_|\__,_|\___| \n\n"
printf "$GREEN ... is now installed and ready to do thy bidding, Master $USER!$RESET\n"
printf "$GREEN Don't forget to adjust the modules you want to use in $PRELUDE_INSTALL_DIR/prelude-modules.el!$RESET\n"
