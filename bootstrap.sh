#!/bin/env sh
echo "thanks for installing spf13-vim\n"
echo "backing up current vim config\n"
for i in ~/.vim ~/.vimrc ~/.gvimrc; do [ -e $i ] && mv $i $i.old; done
echo "cloning spf13-vim\n"
git clone --recursive git://github.com/spf13/spf13-vim.git ~/.spf13-vim 
ln -s ~/.spf13-vim/.vimrc ~/.vimrc
ln -s ~/.spf13-vim/.vim ~/.vim
echo "building command-t executable\n"
echo "command-t depends on ruby and rake to be present\n"
cd ~/.vim/bundle/command-t
rake make
