#!/usr/bin/env bash

#   Copyright 2014 Steve Francia
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

############################  SETUP PARAMETERS
app_name='spf13-vim'

if [[ -z "$XDG_CONFIG_HOME" ]]; then
    [[ -z "$SPF13_VIM" ]] && SPF13_VIM="$HOME/.spf13-vim-3"
    vim_config_home="$HOME"

    vimrc_before=".vimrc.before"
    vimrc_before_fork=".vimrc.before.fork"
    vimrc_bundles=".vimrc.bundles"
    vimrc_bundles_fork=".vimrc.bundles.fork"
    vimrc_fork=".vimrc.fork"
    vimrc_local=".vimrc.local"
else
    [[ -z "$SPF13_VIM" ]] && SPF13_VIM="$XDG_CONFIG_HOME/spf13-vim-3"
    vim_config_home="$XDG_CONFIG_HOME/vim"

    vimrc_before="vimrc.before"
    vimrc_before_fork="vimrc.before.fork"
    vimrc_bundles="vimrc.bundles"
    vimrc_bundles_fork="vimrc.bundles.fork"
    vimrc_fork="vimrc.fork"
    vimrc_local="vimrc.local"
fi

if [[ -z "$XDG_DATA_HOME" ]]; then
    vim_data_home="$HOME/.vim"
else
    # TODO: I can't seem to get vim/vundle etc to play nicely with XDG_DATA_HOME
    vim_data_home="$HOME/.vim"
fi

[[ -z "$REPO_URI" ]] && REPO_URI='https://github.com/spf13/spf13-vim.git'
[[ -z "$REPO_BRANCH" ]] && REPO_BRANCH='3.0'
debug_mode='1'
fork_maintainer='0'
[[ -z "$VUNDLE_URI" ]] && VUNDLE_URI='https://github.com/gmarik/vundle.git'
############################  BASIC SETUP TOOLS
msg() {
    printf '%b\n' "$1" >&2
}

success() {
    if [[ "$ret" -eq '0' ]]; then
        msg "\33[32m[✔]\33[0m ${1}${2}"
    fi
}

error() {
    msg "\33[31m[✘]\33[0m ${1}${2}"
    exit 1
}

debug() {
    if [[ "$debug_mode" -eq '1' ]] && [[ "$ret" -gt '1' ]]; then
        msg "An error occurred in function \"${FUNCNAME[$i+1]}\" on line ${BASH_LINENO[$i+1]}, we're sorry for that."
    fi
}

program_exists() {
    local ret='0'
    type $1 >/dev/null 2>&1 || { local ret='1'; }

    # throw error on non-zero return value
    if [[ ! "$ret" -eq '0' ]]; then
        error "You must have '$1' installed to continue."
    fi
}

variable_set() {
    if [[ -z "$1" ]]; then
        error "You must have your $1 environmental variable set to continue."
    fi
}

lnif() {
    if [[ -e "$1" ]]; then
        ln -sf "$1" "$2"
    fi
    ret="$?"
    debug
}

############################  SETUP FUNCTIONS
do_backup() {
    if [[ -e "$1" ]] || [[ -e "$2" ]] || [[ -e "$3" ]]; then
        msg "Attempting to back up your original vim configuration."
        today=`date +%Y%m%d_%s`
        for i in "$1" "$2" "$3"; do
            [[ -e "$i" ]] && [[ ! -L "$i" ]] && mv -v "$i" "$i.$today";
        done
        ret="$?"
        success "Your original vim configuration has been backed up."
        debug
   fi
}

sync_repo() {
    local repo_path="$1"
    local repo_uri="$2"
    local repo_branch="$3"
    local repo_name="$4"

    msg "Trying to update $repo_name"

    if [[ ! -e "$repo_path" ]]; then
        mkdir -p "$repo_path"
        git clone -b "$repo_branch" "$repo_uri" "$repo_path"
        ret="$?"
        success "Successfully cloned $repo_name."
    else
        cd "$repo_path" && git pull origin "$repo_branch"
        ret="$?"
        success "Successfully updated $repo_name"
    fi

    debug
}

create_symlinks() {
    local source_path="$1"
    local target_path="$2"

    mkdir -p "$target_path"

    lnif "$source_path/.vimrc"         "$HOME/.vimrc"
    lnif "$source_path/.vimrc.bundles" "$target_path/$vimrc_bundles"
    lnif "$source_path/.vimrc.before"  "$target_path/$vimrc_before"
    lnif "$source_path/.vim"           "$vim_data_home"

    touch  "$target_path/$vimrc_local"

    ret="$?"
    success "Setting up vim symlinks."
    debug
}

setup_fork_mode() {
    local source_path="$2"
    local target_path="$3"

    mkdir -p "$target_path"

    if [[ "$1" -eq '1' ]]; then
        touch "$target_path/$vimrc_fork"
        touch "$target_path/$vimrc_bundles_fork"
        touch "$target_path/$vimrc_before_fork"

        lnif "$source_path/.vimrc.fork"         "$target_path/$vimrc_fork"
        lnif "$source_path/.vimrc.bundles.fork" "$target_path/$vimrc_bundles_fork"
        lnif "$source_path/.vimrc.before.fork"  "$target_path/$vimrc_before_fork"

        ret="$?"
        success "Created fork maintainer files."
        debug
    fi
}

setup_vundle() {
    local system_shell="$SHELL"
    export SHELL='/bin/sh'

    vim \
        -u "$1" \
        "+set nomore" \
        "+BundleInstall!" \
        "+BundleClean" \
        "+qall"

    export SHELL="$system_shell"

    success "Now updating/installing plugins using Vundle"
    debug
}

############################  MAIN()
variable_set    "$HOME"
program_exists  "vim"
program_exists  "git"

do_backup       "$vim_data_home" \
                "$HOME/.vimrc" \
                "$HOME/.gvimrc"

sync_repo       "$SPF13_VIM" \
                "$REPO_URI" \
                "$REPO_BRANCH" \
                "$app_name"

create_symlinks "$SPF13_VIM" \
                "$vim_config_home"

setup_fork_mode "$fork_maintainer" \
                "$SPF13_VIM" \
                "$vim_config_home"

sync_repo       "$vim_data_home/bundle/vundle" \
                "$VUNDLE_URI" \
                "master" \
                "vundle"

setup_vundle    "$SPF13_VIM/.vimrc.bundles.default"

msg             "\nThanks for installing $app_name."
msg             "© `date +%Y` http://vim.spf13.com/"
