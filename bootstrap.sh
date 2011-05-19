#!/bin/env sh

endpath='~/.spf13-vim'

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
for i in ~/.vim ~/.vimrc ~/.gvimrc; do [ -e $i ] && mv $i $i.old; done


echo "cloning spf13-vim\n"
#git clone --recursive git://github.com/spf13/spf13-vim.git ~/.spf13-vim 
git clone --recursive -b 3.0 git://github.com/spf13/spf13-vim.git $endpath
ln -s $endpath/.vimrc ~/.vimrc
ln -s $endpath/.vim ~/.vim

echo "installing plugins using Vundle"
vim +BundleInstall! +BundleClean +q

# Build command-t for your system
echo "building command-t executable\n"
echo "command-t depends on ruby and rake to be present\n"
cd ~/.vim/bundle/command-t
(rake make) || warn "Ruby compilation failed. Ruby, GCC or rake not installed?"
