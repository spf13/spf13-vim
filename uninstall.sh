#!/usr/bin/env sh

if [ -e "$HOME/Dropbox" ]; then
  app_dir="$HOME/Dropbox/.vim_git"
else
  app_dir="$HOME/.vim_git"
fi

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
