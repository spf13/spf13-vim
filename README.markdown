# spf13-vim

## My Vim config.
The ultimate vim configuration.

This configuration makes use of [pathogen](http://www.vim.org/scripts/script.php?script_id=2332) to have
a well organized vim directory.

It also heavily uses vim submodules where possible for all plugins. This makes for easy updating.

It also works well on Windows, Linux and OSX without even modifying directories. Just clone and run.

## The vimrc file

The vimrc file is suited to programming. It is very well organized and folds in sections.
Each section is labeled and each option is commented.

It fixes many of the inconveniences of vanilla vim including
 * One config can be used across Windows, Mac and linux
 * Eliminates swap and backup files from littering directories, preferring to store in a central location.
 * Fixes common typos like :W, :Q, etc
 * Setup a solid set of settings for Formatting (change to meet your needs) 
 * Setup the interface to take advantage of vim's features including
   * omnicomplete
   * line numbers
   * syntax highlighting
   * A better ruler & status line
   * & more
 * Configuring included plugins

## Plugins
I compile and configure a few popular vim plugins, colors, snippets, etc

Most of the bundles are git submodules facilitating easy updating and configuration. 

 * [PIV (PHP Integration for VIM)](http://github.com/spf13/PIV)
 * [Snipmate](http://github.com/msanders/snipmate.vim)
 * [NerdCommenter](http://github.com/scrooloose/nerdcommenter)
 * [NerdTree](http://github.com/scrooloose/nerdtree)
 * [SuperTab](http://www.vim.org/scripts/script.php?script_id=1643)
 * [Fugitive](http://github.com/tpope/vim-fugitive)
 * [DelimitMate](http://github.com/Raimondi/delimitMate)
 * [Matchit](http://www.vim.org/scripts/script.php?script_id=39)
 * [CheckSyntax](http://www.vim.org/scripts/script.php?script_id=1431)
 * [Surrounding](http://github.com/msanders/vim-files/blob/master/plugin/surrounding.vim)
 * [AutoCloseTag](http://www.vim.org/scripts/script.php?script_id=2591)

## Snippets

It also contains a very complete set of [snippets](http://github.com/spf13/snipmate-snippets) for use with snipmate.

## Installation

    git clone git://github.com/spf13/spf13-vim.git
    cd spf13-vim
    git submodule update --init

I setup symlinks after this so I can maintain the repo outside of my actual config location.

Use ln -s on mac/unix or mklink on windows.

    cd ~
    ln -s /path/to/spf13-vim/vimrc .vimrc
    ln -s /path/to/spf13-vim/vim .vim

