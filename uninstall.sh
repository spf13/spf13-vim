#!/usr/bin/env sh

app_dir="$HOME/.spf13-vim-3"

warn() {
    echo "$1" >&2
}

die() {
    warn "$1"
    exit 1
}

cd $HOME

rm $HOME/.vimrc
rm $HOME/.vimrc.bundles
rm $HOME/.vimrc.before

# Remove fork folder
rm $HOME/.vimrc.before.fork
rm $HOME/.vimrc.bundles.fork
rm $HOME/.vimrc.fork

cd $HOME
rm -rf .vimbackup .vimswap .vimundo .vimviews 

rm -rf $HOME/.vim

rm -rf $app_dir
cd $HOME
