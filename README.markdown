# spf13-vim : Steve Francia's Vim Distribution

This is a distribution of vim plugins and tools intended to be run on top of VIM.  It is a good starting point for anyone intending to use VIM for development. 

Unlike traditional VIM plugin structure, which similar to UNIX throws all files into common directories, making updating or disabling plugins a real mess, spf13-vim uses [pathogen](http://www.vim.org/scripts/script.php?script_id=2332) to have a well organized vim directory (Similar to mac's app folders).

Great care has been taken to ensure that each plugin plays nicely with others, and optional configuration has been provided for what we believe is the most efficient use. 

It heavily uses git submodules where possible for all plugins. This makes for easy updating.

Lastly (and perhaps, most importantly) It is completely cross platform. It works well on Windows, Linux and OSX without any modifications or additional configurations. If you are using [MacVim](http://code.google.com/p/macvim/) or Gvim additional features are enabled. So regardless of your environment just clone and run.

## Pre-requisites

spf13-vim is built to be completely cross platform. It works equally well on console vim as it does on gVim for Windows, \*nix or MacVim. 

spf13-vim is dependent on a semi-recent version of VIM and should work well on anything above VIM 7.0.

Git is required for installation. Certain plugins require python or ruby support to be compiled into VIM. 

To check if you have python or ruby support run

    :echo has('ruby')

If it returns 1 your vim supports ruby.

## Installation

### Easy Installation (*nix only)

    curl https://github.com/spf13/spf13-vim/raw/master/bootstrap.sh -o - | sh
 
or

### Manual Installation

    for i in ~/.vim ~/.vimrc ~/.gvimrc; do [ -e $i ] && mv $i $i.old; done
    git clone --recursive git://github.com/spf13/spf13-vim.git 

I setup symlinks after this so I can maintain the repo outside of my actual config location.

Use ln -s on mac/unix or mklink on windows.

    cd ~
    ln -s /path/to/spf13-vim/.vimrc .vimrc
    ln -s /path/to/spf13-vim/.vim .vim

## Updating to the latest version

    cd /path/to/spf13-vim/
    git pull
    git submodule sync 
    git submodule update --init --recursive

## Customization

Create `~/.vimrc.local` and `~/.gvimrc.local` for any local
customizations.

For example, to override the default color schemes:

    echo color desert  > ~/.vimrc.local
    echo color molokai > ~/.gvimrc.local

### Fork me on GitHub

I'm always happy to take pull requests from others. A good number of people have already contributed to spf13-vim. Go ahead and fork me.


# spf13-vim Features

## A highly optimized .vimrc config file

The .vimrc file is suited to programming. It is extremely well organized and folds in sections.
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

spf13-vim contains a curated set of popular vim plugins, colors, snippets and syntaxes. Great care has been made to ensure that these plugins play well together and have optimal configuration.

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

### NERDTree

NERDTree is a file explorer plugin that provides "project drawer"
functionality to your vim editing.  You can learn more about it with
:help NERDTree or checkout my post on [NERDTree](http://spf13.com/post/vim-plugins-nerd-commenter).

**QuickStart** Launch using `<Leader>e`.

**Customizations**: spf13-vim adds a number of customizations to the core
NERDTree:

* Use `<C-E>` to toggle NERDTree
* Use `<leader>e` or `<leader>nt` to load NERDTreeFind which opens NERDTree 
  where the current file is located.
* Ignore  '\.pyc', '\~$', '\.swo$', '\.swp$', '\.git', '\.hg', '\.svn', '\.bzr' files
* Disallow `:e`ing files into the NERDTree buffer
* Adding Mirroring... Keep your NERDTree window in sync across your tabs (on by default)
* If NERDTree is open in the current tab, open it in a new one.
* In general, assume that there is a single NERDTree buffer on the left
  and one or more editing buffers on the right

### Command-T

The Command-T plug-in provides an extremely fast, intuitive mechanism for
opening files with a minimal number of keystrokes. It's named "Command-T"
because it is inspired by the "Go to File" window bound to Command-T in
TextMate.

**QuickStart** Launch using `<Leader>t`.

### Surround

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

There's a lot more; check it out at `:help surround` 

### NERDCommenter

NERDCommenter allows you to wrangle your code comments, regardless of
filetype. View `help :NERDCommenter` for all the details.

**QuickStart** Toggle comments using `<Leader>c<space>` in Visual or Normal mode.

**Customizations**: spf13-vim binds command-/ (`<D-/>`) to toggle comments.

### SuperTab

In insert mode, start typing something and hit `<TAB>` to tab-complete
based on the current context.

**QuickStart** Hit the tab key in Insert mode.

### SnipMate

Snipmate provides snippet support similar to TextMate.
A snippet is a piece of often-typed text that you can insert into your
document using a trigger word followed by a <tab>.

Spf13-vim ships with a very large library of snippets for dozens of languages. 


For instance, in a C file using the default installation of snipMate.vim, if
you type "for<tab>" in insert mode, it will expand a typical for loop in C: >

    for (i = 0; i < count; i++) {

    }

To go to the next item in the loop, simply <tab> over to it; if there is
repeated code, such as the "i" variable in this example, you can simply
start typing once it's highlighted and all the matches specified in the
snippet will be updated. To go in reverse, use <shift-tab>.

**QuickStart** Type a keyword (try something like `class`) and hit tab in insert mode.

### Git Support (Fugitive)

Fugitive adds pervasive git support to git directories in vim. For more
information, use `:help fugitive`

Use `:Gstatus` to view `git status` and type `-` on any file to stage or
unstage it. Type `p` on a file to enter `git add -p` and stage specific
hunks in the file.

Use `:Gdiff` on an open file to see what changes have been made to that
file

**QuickStart** :Gstatus (in command mode)


### PIV (PHP Integration for VIM)

The most feature complete and up to date PHP Integration for Vim with proper support for PHP 5.3+ including latest syntax, functions, better fold support, etc.

PIV provides 

 * PHP 5.3 support
 * Auto generation of PHP Doc (,pd on (function, variable, class) definition line)
 * Autocomplete of classes, functions, variables, constants and language keywords
 * Better indenting
 * Full PHP documentation manual (hit K on any function for full docs)

### DelimitMate
DelimitMate provides automatic closing of quotes, parenthesis, brackets,
etc.; besides some other related features that should make your time in insert
mode a little bit easier.

Most of the features can be modified or disabled permanently, using global
variables, or on a FileType basis, using autocommands. With a couple of
exceptions and limitations, this features don't brake undo, redo or history.

**QuickStart** Enabled by default, just works. see :help delimitmate for options

### Ack.vim

Ack.vim uses ack to search inside the current directory for a pattern.
You can learn more about it with :help Ack

**Customizations**: spf13-vim rebinds command-shift-f (`<D-F>`) to bring up
`:Ack `.

### Align

Align lets you align statements on their equal signs, make comment
boxes, align comments, align declarations, etc.

* `:5,10Align =>` to align lines 5-10 on `=>`'s

### CTags

spf13-vim includes the TagList plugin, which binds `:Tlist` to an overview
panel that lists all ctags for easy navigation.

**Customizations**: spf13-vim binds `<Leader>rt` to the ctags command to
update tags.

**Note**: For full language support, run `brew install ctags` to install
exuberant-ctags.

**Tip**: Check out `:help ctags` for information about VIM's built-in
ctag support. Tag navigation creates a stack which can traversed via
`Ctrl-]` (to find the source of a token) and `Ctrl-T` (to jump back up
one level).

### EasyTags

Automated tag generation and syntax highlighting in Vim

**Note**: Depends on exuberant Ctags. On OSX, For full language support, run `brew install ctags` to install
exuberant-ctags. If you don't have ctags support disable this plugin.

**QuickStart** CTRL-] while the cursor is on a keyword (such as a function name) to jump to it's definition.

## Additional Syntaxes

spf13-vim ships with a few additional syntaxes:

* Markdown (bound to \*.markdown, \*.md, and \*.mk)
* Twig
* Git commits (set your `EDITOR` to `mvim -f`)

## Color schemes

spf13-vim includes [spf13 vim color pack](https://github.com/spf13/vim-colors/): 

* ir_black
* molokai
* peaksea

Use `:color molokai` to switch to a color scheme.

## Snippets

It also contains a very complete set of [snippets](http://github.com/spf13/snipmate-snippets) for use with snipmate.


# Intro to VIM

Here's some tips if you've never used VIM before:

## Tutorials

* Type `vimtutor` into a shell to go through a brief interactive
  tutorial inside VIM.
* Read the slides at [VIM: Walking Without Crutches](http://walking-without-crutches.heroku.com/#1).

## Modes

* VIM has two (common) modes:
  * insert mode- stuff you type is added to the buffer
  * normal mode- keys you hit are interpreted as commands
* To enter insert mode, hit `i`
* To exit insert mode, hit `<ESC>`

## Useful commands

* Use `:q` to exit vim
* Certain commands are prefixed with a `<Leader>` key, which by default maps to `\`
  by default. Spf13-vim uses `let mapleader = ","` to change this to `,` which is in a consistent and 
  convenient location.
* Keyboard [cheat sheet](http://walking-without-crutches.heroku.com/image/images/vi-vim-cheat-sheet.png).

