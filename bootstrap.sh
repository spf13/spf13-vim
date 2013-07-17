#!/usr/bin/env bash

############################  SETUP PARAMETERS
declare -a required=('vim' 'git' 'vundle');
app_name='spf13-vim'
git_branch='3.0'
debug_mode='0'
fork_maintainer='0'

############################  BASIC SETUP TOOLS
function msg() {
    printf '%b\n' "$1" >&2
}

function success {
    if [[ "$ret" -eq '0' ]]; then
	msg "\e[32m[✔]\033[0m ${1}${2}"
    fi
}

function error() {
    msg "\e[31m[✘]\033[0m ${1}${2}"
    exit 1
}

function debug() {
    if [[ "$debug_mode" -eq '1' && "$ret" -gt '1' ]]; then
      msg "An error occured in function \"${FUNCNAME[$i+1]}\" on line ${BASH_LINENO[$i+1]}, we're sorry for that."
    fi
}

function program_exists {
    local ret='0'
    type $1 >/dev/null 2>&1 || { local ret='1'; }

    # throw error on non-zero return value
    if [[ ! "$ret" -eq '0' ]]; then
	error "$2"
    fi
}

############################ SETUP FUNCTIONS
function lnif() {
    if [[ -e "$1" ]]; then
        ln -sf "$1" "$2"
    fi
    ret="$?"
    debug
}

function do_backup() {
    if [[ -e "$2" || -e "$3" || -e "$4" ]]; then
        today=`date +%Y%m%d_%s`
        for i in "$2" "$3" "$4"; do
            [ -e "$i" ] && [ ! -L "$i" ] && mv "$i" "$i.$today";
        done
        ret="$?"
        success "$1"
        debug
   fi
}

function upgrade_repo() {
      msg "trying to update $1"

      if [[ "$1" == "$app_name" ]]; then
	  cd "$HOME/.$app_name-3" &&
	  git pull origin "$git_branch"
      fi

      if [[ "$1" == "${required[2]}" ]]; then
	  cd "$HOME/.vim/bundle/vundle" &&
	  git pull origin master
      fi

      ret="$?"
      success "$2"
      debug
}

function clone_repo() {
    program_exists "${required[1]}" "Sorry, we cannot continue without GIT, please install it first."
    endpath="$HOME/.$app_name-3"

    if [ ! -e "$endpath/.git" ]; then
	git clone --recursive -b "$git_branch" https://github.com/spf13/spf13-vim.git "$endpath"
	ret="$?"
	success "$1"
	debug
    else
	upgrade_repo "$app_name"	"Successfully updated $app_name"
    fi
}

function clone_vundle() {
    if [[ ! -e "$HOME/.vim/bundle/vundle" ]]; then
	git clone https://github.com/gmarik/vundle.git "$HOME/.vim/bundle/vundle"
    else
	upgrade_repo "${required[2]}"	"Successfully updated ${required[2]}"
    fi
    ret="$?"
    success "$1"
    debug
}

function create_symlinks() {
    endpath="$HOME/.$app_name-3"

    ln -sf "$endpath/.vimrc"              "$HOME/.vimrc"
    ln -sf "$endpath/.vimrc.bundles"      "$HOME/.vimrc.bundles"
    ln -sf "$endpath/.vim"                "$HOME/.vim"

    # Useful for fork maintainers
    touch  "$HOME/.vimrc.local"

    if [[ -e "$endpath/.vimrc.fork" ]]; then
        ln -sf "$HOME/.vimrc.fork"
    elif [[ "$fork_maintainer" -eq '1' ]]; then
       touch "$HOME/.vimrc.fork"
       touch "$HOME/.vimrc.bundles.fork"
    fi

    if [[ -e "$endpath/.vimrc.bundles.fork" ]]; then
        ln -sf "$HOME/.vimrc.bundles.fork"
    fi

    if [[ ! -d "$endpath/.vim/bundle" ]]; then
	mkdir -p "$endpath/.vim/bundle"
    fi

    ret="$?"
    success "$1"
    debug
}

function setup_vundle() {
    system_shell="$SHELL"
    export SHELL='/bin/sh'
    vim -u "$HOME/.vimrc.bundles" +BundleInstall! +BundleClean +qall
    export SHELL="$system_shell"

    success "$1"
    debug
}

############################ MAIN()
program_exists "${required[0]}" "To install $app_name you first need to install Vim."

do_backup	"Your old vim stuff has a suffix now and looks like .vim.`date +%Y%m%d_%s`" \
		"$HOME/.vim" \
		"$HOME/.vimrc" \
		"$HOME/.gvimrc"

clone_repo 	"Successfully cloned $app_name"

clone_vundle 	"Successfully cloned ${required[2]}"

create_symlinks "Setting up vim symlinks"

setup_vundle 	"Now updating/installing plugins using Vundle"

msg 		"\nThanks for installing $app_name."
msg		"© `date +%Y` http://vim.spf13.com/"
