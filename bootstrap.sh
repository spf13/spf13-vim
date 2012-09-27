#!/usr/bin/env sh

endpath="$HOME/.spf13-vim-3"

warn() {
    echo "$1" >&2
}

die() {
    warn "$1"
    exit 1
}

function lnif {
    if [ ! -e $2 ] ; then
        ln -s $1 $2
    fi
}

echo "Thanks for installing spf13-vim\n"

# Backup existing .vim stuff
echo "backing up current vim config\n"
today=`date +%Y%m%d`
for i in $HOME/.vim $HOME/.vimrc $HOME/.gvimrc; do [ -e $i ] && [ ! -L $file ] && mv $i $i.$today; done

mkdir -p $endpath/.vim/bundle
lnif $endpath/.vimrc $HOME/.vimrc
lnif $endpath/.vimrc.bundles $HOME/.vimrc.bundles
lnif $endpath/.vim $HOME/.vim

if [ ! -d $endpath ]; then
    echo "cloning spf13-vim\n"
    git clone --recursive -b 3.0 http://github.com/spf13/spf13-vim.git $endpath
    echo "Installing Vundle"
    git clone http://github.com/gmarik/vundle.git $HOME/.vim/bundle/vundle
else
    echo "updating spf13-vim\n"
    cd $endpath && git pull
fi

echo "installing plugins using Vundle"
vim -u $endpath/.vimrc.bundles - +BundleInstall! +BundleClean +qall
