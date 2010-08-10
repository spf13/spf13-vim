# spf13-vim

## My Vim config.
I compile and configure a few popular vim plugins, colors, snippets, etc

This configuration makes use of [pathogen](http://www.vim.org/scripts/script.php?script_id=2332) to have
a well organized vim directory.

Most of the bundles are git submodules facilitating easy updating and configuration. 

The vimrc is quite clean and provides easy configuration of the following plugins.

## Plugins
 * [PIV (PHP Integration for VIM)](http://github.com/spf13/PIV)
 * [Snipmate](http://github.com/msanders/snipmate.vim)
 * [NerdCommenter](http://github.com/scrooloose/nerdcommenter.git)
 * [NerdTree](http://github.com/scrooloose/nerdtree)
 * [SuperTab](http://www.vim.org/scripts/script.php?script_id=1643)
 * [Fugitive](http://github.com/tpope/vim-fugitive.git)
 * [DelimitMate](http://github.com/Raimondi/delimitMate)
 * [Matchit](http://www.vim.org/scripts/script.php?script_id=39)
 * [CheckSyntax](http://www.vim.org/scripts/script.php?script_id=1431)
 * [Surrounding](http://github.com/msanders/vim-files/blob/master/plugin/surrounding.vim)
 * [AutoCloseTag](http://www.vim.org/scripts/script.php?script_id=2591)

## Snippets

It also contains a very complete set of [snippets](http://github.com/spf13/snipmate-snippets) for use with snipmate.


## Installation

    git clone git@github.com:spf13/spf13-vim.git
    cd spf13-vim
    git submodule update --init

I setup symlinks after this so I can maintain the repo outside of my actual config location.

Use ln -s on mac/unix or mklink on windows.

    cd ~
    ln -s /path/to/spf13-vim/vimrc .vimrc
    ln -s /path/to/spf13-vim/vim .vim

