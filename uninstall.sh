#!/usr/bin/env sh

warn() {
    echo "$1" >&2
}

die() {
    warn "$1"
    exit 1
}

rm $HOME/.vimrc
rm $HOME/.vimrc.bundles

rm $HOME/.vimrc.before

# Remove fork folder
rm $HOME/.vimrc.before.fork
rm $HOME/.vimrc.bundles.fork
rm $HOME/.vimrc.fork

rm -rf $HOME/.vimbackup 
rm -rf $HOME/.vimswap 
rm -rf $HOME/.vimundo 
rm -rf $HOME/.vimviews 
rm -rf $HOME/.vim/autoload
