#!/usr/bin/env sh

app_dir="$HOME/.spf13-vim-3"

warn() {
    echo "$1" >&2
}

die() {
    warn "$1"
    exit 1
}

rm -f $HOME/.vimrc
rm -f $HOME/.vimrc.bundles
rm -rf $HOME/.vim

rm -rf $app_dir
