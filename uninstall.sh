#!/usr/bin/env sh

app_dir="$HOME/Dropbox/vim"

warn() {
    echo "$1" >&2
}

die() {
    warn "$1"
    exit 1
}

rm $HOME/.vimrc
rm $HOME/.vimrc.bundles
rm $HOME/.vim

rm -rf $app_dir
