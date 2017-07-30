#!/usr/bin/env sh

app_dir="$HOME/.chanson-vim"

warn() {
    echo "$1" >&2
}

die() {
    warn "$1"
    exit 1
}

rm $HOME/.vimrc.before
rm $HOME/.vimrc.before.fork
rm $HOME/.vimrc
rm $HOME/.vimrc.fork
rm $HOME/.vimrc.bundles
rm $HOME/.vimrc.bundles.fork

rm -rf $HOME/.vim
rm -rf $HOME/.vimswap
rm -rf $HOME/.vimundo
rm -rf $HOME/.vimviews

rm -rf $app_dir
