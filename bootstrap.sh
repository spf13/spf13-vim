#!/bin/env sh

endpath="$HOME/.spf13-vim-3"

warn() {
    echo "$1" >&2
}

die() {
    warn "$1"
    exit 1
}

echo "thanks for installing spf13-vim\n"

# Backup existing .vim stuff
echo "backing up current vim config\n"
today=`date +%Y%m%d`
for i in $HOME/.vim $HOME/.vimrc $HOME/.gvimrc; do [ -e $i ] && mv $i $i.$today; done


echo "cloning spf13-vim\n"
git clone --recursive -b 3.0 git://github.com/spf13/spf13-vim.git $endpath
ln -s $endpath/.vimrc ~/.vimrc
ln -s $endpath/.vim ~/.vim

echo "Installing Vundle"
git clone http://github.com/gmarik/vundle.git .vim/bundle/vundle

echo "installing plugins using Vundle"
vim +BundleInstall! +BundleClean +q

# Build command-t for your system
echo "building command-t executable\n"
echo "command-t depends on ruby and rake to be present\n"
cd $HOME/.vim/bundle/Command-t
(rake make) || warn "Ruby compilation failed. Ruby, GCC or rake not installed?"
