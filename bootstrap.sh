#!/bin/env sh

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
git clone --recursive git://github.com/spf13/spf13-vim.git ~/.spf13-vim 
ln -s ~/.spf13-vim/.vimrc ~/.vimrc
ln -s ~/.spf13-vim/.vim ~/.vim


# Build command-t for your system
echo "building command-t executable\n"
echo "command-t depends on ruby and rake to be present\n"
cd ~/.vim/bundle/command-t
(ruby extconf.rb && make clean && make) || warn "Ruby compilation failed. Ruby not installed, maybe?"
