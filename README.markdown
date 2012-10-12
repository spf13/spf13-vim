# spf13-vim : Steve Francia's Vim Distribution

                    __ _ _____              _
         ___ _ __  / _/ |___ /      __   __(_)_ __ ___
        / __| '_ \| |_| | |_ \ _____\ \ / /| | '_ ` _ \
        \__ \ |_) |  _| |___) |_____|\ V / | | | | | | |
        |___/ .__/|_| |_|____/        \_/  |_|_| |_| |_|
            |_|

spf13-vim is a distribution of vim plugins and resources for Vim, Gvim and [MacVim].

It is a good starting point for anyone intending to use VIM for development running equally well on Windows, Linux, *nix and Mac.

The distribution is completely customisable using a `~/.vimrc.local` and `~/.vimrc.bundles.local` Vim RC files.

![spf13-vim image][spf13-vim-img]

Unlike traditional VIM plugin structure, which similar to UNIX throws all files into common directories, making updating or disabling plugins a real mess, spf13-vim 3 uses the [Vundle] plugin management system to have a well organized vim directory (Similar to mac's app folders). Vundle also ensures that the latest versions of your plugins are installed and makes it easy to keep them up to date.

Great care has been taken to ensure that each plugin plays nicely with others, and optional configuration has been provided for what we believe is the most efficient use.

Lastly (and perhaps, most importantly) It is completely cross platform. It works well on Windows, Linux and OSX without any modifications or additional configurations. If you are using [MacVim] or Gvim additional features are enabled. So regardless of your environment just clone and run.

# spf13-vim 3.0
January 2012 spf13-vim released it's third major iteration. **This is important as it requires a reinstall**, but trust me it's worth it.

The biggest change is the switch from using git submodules to using the excellent [Vundle] system. While git submodules seemed like a good idea at the time, it wasn't. It was always problematic. Additionally because a submodule points to a refspec and not a branch, it was a constant maintenance nightmare to keep everything up to date.

[Vundle] has an excellent system built on the same principles as Pathogen, but with an integrated plugin management system that is Git and Github aware.

We have also changed out most of the plugins in favor of newer more stable alternatives. Additionally we have significantly reduced the number of plugins requiring python or ruby.

The goal has always been to add functionality without changing all the features, functionality and keystrokes we all love. Using spf13-vim we've kept all the default behaviors (by and large), so if you ever find yourself on a vanilla environment you'll feel right at home.

# Installation 

## Linux, \*nix, Mac OSX Installation

The easiest way to install spf13-vim is to use our [automatic installer](http://j.mp/spf13-vim3) by simply copying and pasting the following line into a terminal. This will install spf13-vim and backup your existing vim configuration. If you are upgrading from a prior version (before 3.0) this is also the recommended installation.

```bash

    curl http://j.mp/spf13-vim3 -L -o - | sh

```

## Installing on Windows

On Windows and \*nix [Git] and [Curl] are required. 

### Installing dependencies

#### Install [msysgit]

After installation try running `git --version` within _command prompt_ (press Win-R,  type `cmd`, press Enter) to make sure all good:

    C:\> git --version
    git version 1.7.4.msysgit.0

#### Setup [Curl]
_Instructions blatently copied from vundle readme_
Installing Curl on Windows is easy as [Curl] is bundled with [msysgit]!
But before it can be used with [Vundle] it's required make `curl` run in _command prompt_.
The easiest way is to create `curl.cmd` with [this content](https://gist.github.com/912993)

    @rem Do not use "echo off" to not affect any child calls.
    @setlocal

    @rem Get the abolute path to the parent directory, which is assumed to be the
    @rem Git installation root.
    @for /F "delims=" %%I in ("%~dp0..") do @set git_install_root=%%~fI
    @set PATH=%git_install_root%\bin;%git_install_root%\mingw\bin;%PATH%

    @if not exist "%HOME%" @set HOME=%HOMEDRIVE%%HOMEPATH%
    @if not exist "%HOME%" @set HOME=%USERPROFILE%

    @curl.exe %*


And copy it to `C:\Program Files\Git\cmd\curl.cmd`, assuming [msysgit] was installed to `c:\Program Files\Git`

to verify all good, run:

    C:\> curl --version
    curl 7.21.1 (i686-pc-mingw32) libcurl/7.21.1 OpenSSL/0.9.8k zlib/1.2.3
    Protocols: dict file ftp ftps http https imap imaps ldap ldaps pop3 pop3s rtsp smtp smtps telnet tftp
    Features: Largefile NTLM SSL SSPI libz


#### Installing spf13-vim on Windows

The easiest way is to download and run the spf13-vim-windows-install.cmd file.

## Updating to the latest version
The simpliest (and safest) way to update is to simply rerun the installer. It will completely and non destructively upgrade to the latest version. 

```bash

    curl http://j.mp/spf13-vim3 -L -o - | sh

```

Alternatively you can manually perform the following steps. If anything has changed with the structure of the configuration you will need to create the appropriate symlinks.

```bash
    cd $HOME/to/spf13-vim/
    git pull
    vim +BundleInstall! +BundleClean +q
```

### Fork me on GitHub

I'm always happy to take pull requests from others. A good number of people are already [contributors] to [spf13-vim]. Go ahead and fork me.

# A highly optimized .vimrc config file

![spf13-vimrc image][spf13-vimrc-img]

The .vimrc file is suited to programming. It is extremely well organized and folds in sections.
Each section is labeled and each option is commented.

It fixes many of the inconveniences of vanilla vim including

 * A single config can be used across Windows, Mac and linux
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

## Customization

Create `~/.vimrc.local` and `~/.gvimrc.local` for any local
customizations.

For example, to override the default color schemes:

```bash
    echo colorscheme ir_black  >> ~/.vimrc.local
```

### Fork Customization

There is an additional tier of customization available to those who want to maintain a
fork of spf13-vim specialized for a particular group. These users can create `.vimrc.fork`
and `.vimrc.bundles.fork` files in the root of their fork.  The load order for the configuration is:

1. `.vimrc.bundles.local` - local user bundle configuration
2. `.vimrc.bundles.fork` - fork bundle configuration
3. `.vimrc.bundles` - spf13-vim bundle configuration
4. `.vimrc` - spf13-vim vim configuration 
5. `.vimrc.fork` - fork vim configuration
6. `.vimrc.local` - local user configuration 

See `.vimrc.bundles` for specifics on what options can be set to override bundle configuration. See `.vimrc` for specifics 
on what options can be overridden. Most vim configuration options should be set in your `.vimrc.fork` file, bundle configuration
needs to be set in your `.vimrc.bundles.fork` file.

You may also want to update your `README.markdown` file so that the `bootstrap.sh` link points to your repository and your `bootstrap.sh`
file to pull down your fork.

For an example of a fork of spf13-vim that provides customization in this manner see [taxilian's fork](https://github.com/taxilian/spf13-vim).

# Plugins

spf13-vim contains a curated set of popular vim plugins, colors, snippets and
syntaxes. Great care has been made to ensure that these plugins play well
together and have optimal configuration.

The default plugin bundles are listed in the following line, which if you want
to customize, simply copy and place at the top of your .vimrc.bundles.local
file.

```vim
let g:spf13_bundle_groups=['general', 'programming', 'neocomplcache', 'php', 'python', 'javascript', 'scala', 'html', 'ruby', 'misc']
```
The following bundle groups are avaliable:

[General]
programming
snipmate OR neocomplcache (not both)
php
python
javascript
scala
html
ruby
misc
twig
sql


## Adding new plugins

Create `~/.vimrc.bundles.local` for any additional bundles.

To add a new bundle

```bash
    echo Bundle \'spf13/vim-colors\' >> ~/.vimrc.bundles.local
```

Here are a few of the plugins:


### Ack.vim

Ack.vim uses ack to search inside the current directory for a pattern.
You can learn more about it with :help Ack

**QuickStart** :Ack


## General

### [NERDTree]

NERDTree is a file explorer plugin that provides "project drawer"
functionality to your vim editing.  You can learn more about it with
:help NERDTree.

**QuickStart** Launch using `<Leader>e`.

**Customizations**: 

* Use `<C-E>` to toggle NERDTree
* Use `<leader>e` or `<leader>nt` to load NERDTreeFind which opens NERDTree where the current file is located.
* Hide clutter ('\.pyc', '\.git', '\.hg', '\.svn', '\.bzr')
* Treat NERDTree more like a panel than a split.

### Amazing Colors

spf13-vim includes [solarized] and [spf13 vim color pack](https://github.com/spf13/vim-colors/): 

* ir_black
* molokai
* peaksea

Use `:color molokai` to switch to a color scheme.

### [Surround]

This plugin is a tool for dealing with pairs of "surroundings."  Examples
of surroundings include parentheses, quotes, and HTML tags.  They are
closely related to what Vim refers to as text-objects.  Provided
are mappings to allow for removing, changing, and adding surroundings.

Details follow on the exact semantics, but first, consider the following
examples.  An asterisk (*) is used to denote the cursor position.

      Old text                  Command     New text ~
      "Hello *world!"           ds"         Hello world!
      [123+4*56]/2              cs])        (123+456)/2
      "Look ma, I'm *HTML!"     cs"<q>      <q>Look ma, I'm HTML!</q>
      if *x>3 {                 ysW(        if ( x>3 ) {
      my $str = *whee!;         vlllls'     my $str = 'whee!';

For instance, if the cursor was inside `"foo bar"`, you could type
`cs"'` to convert the text to `'foo bar'`.

There's a lot more, check it out at `:help surround` 

### [AutoClose]

AutoClose automatically inserts a closing [, (, {, " and '. Simply type the closing
brace to move the cursor back outside the closed brace.

### [ctrlp]
Ctrlp replaces the Command-T plugin with a 100% viml plugin. It provides an intuitive and fast mechanism to load files from the file system (with regex and fuzzy find), from open buffers, and from recently used files. 

**QuickStart** Launch using `<c-p>`.

### [sessionman]

Session manager

### [matchit]

Easily find that closing tag or function with % across multiple languages.

### [vim-powerline]

Creates a better, more useful status line.

### [EasyMotion]

EasyMotion provides an interactive way to use motions in Vim. 

It quickly maps each possible jump destination to a key allowing very fast and 
straightforward movement.

**QuickStart** EasyMotion is triggered using the normal movements, but prefixing them with `<leader><leader>`

For example this screen shot demonstrates pressing `,,w`

![easymotion image][easymotion-img]

### [csapprox]

Approximate gvim colorschemes to work in terminal vim (88 or 256 colors).

### [bufexplorer]

Easily and quickly switch between buffers.
    '\be' (normal open)  or 
    '\bs' (force horizontal split open)  or 
    '\bv' (force vertical split open) 

### [undotree]

View your undo history in a graph.

In spf13 this is mapped to `,u

### [numbers]

Sets relative line numbering, to toggle hit `F3

### [indent-guides]

Shows visual guides to indent locations, to toggle hit `,ig

## Programming

### [Syntastic]

Syntastic is a syntax checking plugin that runs buffers through external syntax 
checkers as they are saved and opened. If syntax errors are detected, the user 
is notified and is happy because they didn't have to compile their code or 
execute their script to find them.

### [Fugitive]

Fugitive adds pervasive git support to git directories in vim. For more
information, use `:help fugitive`

Use `:Gstatus` to view `git status` and type `-` on any file to stage or
unstage it. Type `p` on a file to enter `git add -p` and stage specific
hunks in the file.

Use `:Gdiff` on an open file to see what changes have been made to that
file

**QuickStart** `<leader>gs` to bring up git status

**Customizations**: 

 * `<leader>gs` :Gstatus<CR>
 * `<leader>gd` :Gdiff<CR>
 * `<leader>gc` :Gcommit<CR>
 * `<leader>gb` :Gblame<CR>
 * `<leader>gl` :Glog<CR>
 * `<leader>gp` :Git push<CR>
 * :Git ___ will pass anything along to git.

![fugitive image][fugitive-img]


### [NERDCommenter]

NERDCommenter allows you to wrangle your code comments, regardless of
filetype. View `help :NERDCommenter` or checkout my post on [NERDCommenter](http://spf13.com/post/vim-plugins-nerd-commenter).

**QuickStart** Toggle comments using `<Leader>c<space>` in Visual or Normal mode.

### Tabularize

Tabularize lets you align statements on their equal signs and other characters

**Customizations**:

 * `<Leader>a=` :Tabularize /=<CR>
 * `<Leader>a:` :Tabularize /:<CR>
 * `<Leader>a::` :Tabularize /:\zs<CR>
 * `<Leader>a,` :Tabularize /,<CR>
 * `<Leader>a<Bar>` :Tabularize /<Bar><CR>

### [Tagbar]

spf13-vim includes the Tagbar plugin. This plugin requires exuberant-ctags and will automatically generate tags for your open files. It also provides a panel to navigate easily via tags

**QuickStart** `CTRL-]` while the cursor is on a keyword (such as a function name) to jump to it's definition.

**Customizations**: spf13-vim binds `<F9>` to toggle the tagbar panel

![tagbar image][tagbar-img]

**Note**: For full language support, run `brew install ctags` to install
exuberant-ctags.

**Tip**: Check out `:help ctags` for information about VIM's built-in
ctag support. Tag navigation creates a stack which can traversed via
`Ctrl-]` (to find the source of a token) and `Ctrl-T` (to jump back up
one level).


## Snippets & AutoComplete

neocomplcache is the defualt for spf13-vim, vim-snipmate is avaliable, simply
change neocomplcache to snipmate in your .vimrc.bundles.local plugin
configuration.

### [vim-snipmate]

Similar to the snippet functionality of textmate.

### [neocomplcache]

NeoComplCache is an amazing autocomplete plugin with additional support for snippets. It can complete simulatiously from the dictionary, buffer, omnicomplete and snippets. This is the one true plugin that brings Vim autocomplete on par with the best editors. 

**QuickStart** Just start typing, it will autocomplete where possible

**Customizations**: 

 * Automatically present the autocomplete menu
 * Support tab and enter for autocomplete
 * `<C-k>` for completing snippets.

![neocomplcache image][autocomplete-img]

## PHP

### [piv]

the most feature complete and up to date php integration for vim with proper support for php 5.3+ including latest syntax, functions, better fold support, etc.

piv provides:

 * php 5.3 support
 * auto generation of php doc (,pd on (function, variable, class) definition line)
 * autocomplete of classes, functions, variables, constants and language keywords
 * better indenting
 * full php documentation manual (hit k on any function for full docs)

![php vim itegration image][phpmanual-img]

## Python

### [python-mode]

### [python.vim]

### [python_match.vim]

### [pythoncomplete]

## JavaScript

### [vim-json]

### [vim-less]

### [vim-javascript]

### [vim-jst]

## Java

### [vim-scala]

### [vim-sbt]

## HTML

### [html-autoclosetag]

### [better-css-syntax-for-vim]

## Ruby

### [vim-rails]

## Misc

### [vim-markdown]

### [vim-preview]

### [vim-cucumber]

### [puppet-syntax-highlighting]

## twig

### [vim-twig]

## SQL
(Add 'sql' to g:spf13_bundle_groups)
### [Align]

This plugin assists SQLUtilities by letting you align statements on their
operator signs, but can also be used to line up comments, declarations etc.

### [dbext.vim]

This helpful database plugin enables Vim to access several types of databases
including MySQL, Oracle, MSSQL, DB2 and many others.  type
`:help dbext-tutorial` to learn the basics.

### [sqlutilities]

Format SQL statements quickly and painlessly.  Highlight your SQL and press
`,sfs`.  Check out the Plugin menu for more options, and check the helpfile for
many other customizations from within .vimrc.local.

Currently in .vimrc:
```vim
let g:sqlutil_align_where = 0           " Disable Align Operators
let g:sqlutil_keyword_case = '\U'       " Uppercase Keywords
let g:sqlutil_align_comma = 1           " Enable Align Comma
let g:sqlutil_align_keyword_right = 0   " Disable Align Keyword Right
```

## additional syntaxes

spf13-vim ships with a few additional syntaxes:

* markdown (bound to \*.markdown, \*.md, and \*.mk)
* twig
* git commits (set your `editor` to `mvim -f`)



## snippets

it also contains a very complete set of [snippets](http://github.com/spf13/snipmate-snippets) for use with snipmate or [neocomplcache].


# intro to vim

here's some tips if you've never used vim before:

## tutorials

* type `vimtutor` into a shell to go through a brief interactive
  tutorial inside vim.
* read the slides at [vim: walking without crutches](http://walking-without-crutches.heroku.com/#1).

## modes

* vim has two (common) modes:
  * insert mode- stuff you type is added to the buffer
  * normal mode- keys you hit are interpreted as commands
* to enter insert mode, hit `i`
* to exit insert mode, hit `<esc>`

## useful commands

* use `:q` to exit vim
* certain commands are prefixed with a `<leader>` key, which by default maps to `\`.
  spf13-vim uses `let mapleader = ","` to change this to `,` which is in a consistent and 
  convenient location.
* keyboard [cheat sheet](http://walking-without-crutches.heroku.com/image/images/vi-vim-cheat-sheet.png).

[git]:http://git-scm.com
[curl]:http://curl.haxx.se
[msysgit]:http://code.google.com/p/msysgit
[macvim]:http://code.google.com/p/macvim/
[spf13-vim]:https://github.com/spf13/spf13-vim
[contributors]:https://github.com/spf13/spf13-vim/contributors

[General]:#general



[vundle]:http://github.com/gmarik/vundle
[NERDTree]:http://github.com/scrooloose/nerdtree
[solarized]:http://github.com/altercation/vim-colors-solarized
[Surround]:https://github.com/tpope/vim-surround
[AutoClose]:https://github.com/vim-scripts/AutoClose
[ctrlp]:https://github.com/kien/ctrlp.vim
[sessionman]:https://github.com/vim-scripts/sessionman.vim
[Matchit]:https://github.com/vim-scripts/matchit.zip
[vim-powerline]:https://github.com/Lokaltog/vim-powerline
[vim-easymotion]:http://github.com/Lokaltog/vim-easymotion
[csapprox]:https://gitgub.com/godlygeek/csapprox
[EasyMotion]:https://github.com/Lokaltog/vim-easymotion
[bufexplorer]:https://github.com/corntrace/bufexplorer
[undotree]:https://github.com/mbbill/undotree
[numbers]:https://github.com/myusuf3/numbers.vim
[indent-guides]:https://github.com/nathanaelkane/vim-indent-guides

[Syntastic]:http://github.com/scrooloose/syntastic
[Fugitive]:http://github.com/tpope/vim-fugitive
[NERDCommenter]:http://github.com/scrooloose/nerdcommenter
[Tabularize]:http://github.com/godlygeek/tabular
[Tagbar]:http://github.com/godlygeek/tagbar

[vim-snipmate]:https://github.com/garbas/vim-snipmate
[neocomplcache]:https://github.com/shougo/neocomplcache

[piv]:https://github.com/spf13/pIV

[python-mode]:https://github.com/klen/python-mode
[python.vim]:https://github.com/vim-scripts/python.vim
[python_match.vim]:https://github.com/vim-scripts/python_match.vim
[pythoncomplete]:https://github.com/vim-scripts/pythoncomplete

[vim-json]:https://github.com/leshill/vim-json
[vim-less]:https://github.com/groenewege/vim-less
[vim-javascript]:https://github.com/pangloss/vim-javascript
[vim-jst]:https://github.com/briancollins/vim-jst

[vim-scala]:https://github.com/derekwyatt/vim-scala
[vim-sbt]:https://github.com/derekwyatt/vim-sbt

[HTML-AutoCloseTag]:https://github.com/amirh/HTML-AutoCloseTag
[Better-CSS-Syntax-for-Vim]:https://github.com/ChrisYip/Better-CSS-Syntax-for-Vim

[vim-rails]:https://github.com/tpope/vim-rails

[vim-markdown]:https://github.com/tpope/vim-markdown
[vim-preview]:https://github.com/spf13/vim-preview
[vim-cucumber]:https://github.com/tpope/vim-cucumber
[Puppet-Syntax-Highlighting]:https://github.com/vim-scripts/Puppet-Syntax-Highlighting

[vim-twig]:https://github.com/beyondwords/vim-twig

[Align]:https://github.com/vim-scripts/Align
[dbext.vim]:https://github.com/vim-scripts/dbext.vim
[SQLUtilities]:https://github.com/vim-scripts/SQLUtilities



[spf13-vim-img]:http://i.imgur.com/UKToY.png
[spf13-vimrc-img]:http://i.imgur.com/kZWj1.png
[autocomplete-img]:http://i.imgur.com/90Gg7.png
[tagbar-img]:http://i.imgur.com/cjbrC.png
[fugitive-img]:http://i.imgur.com/4NrxV.png
[nerdtree-img]:http://i.imgur.com/9xIfu.png
[phpmanual-img]:http://i.imgur.com/c0GGP.png
[easymotion-img]:http://i.imgur.com/ZsrVL.png
