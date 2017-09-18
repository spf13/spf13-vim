# leoatchina的vim配置文件,从spf13的配置fork而来
This is leoatchina's vim config forked from [spf13-vim : steve francia's vim distribution](https://github.com/spf13/spf13-vim)
Thanks him for great job.I changed lots of settings and plugins to suit my needs.

# 安装 
## 要求 
如果要安装[neocomplete](https://github.com/Shougo/neocomplete.vim), 需要[vim with lua](https://github.com/Shougo/neocomplete.vim#requirements).
*安装本config 需要 Git 1.7+ 和 Vim 7.3+，部分插件需要Vim8.0*

## Linux, \*nix, Mac OSX 下的安装

```bash
    git clone https://github.com/leoatchina/spf13-vim-leoatchina.git
    cd spf13-vim-leoatchina
    bash bootstrap.sh
```

## windows下的安装 
```bash
    git clone https://github.com/leoatchina/spf13-vim-leoatchina.git
    cd spf13-vim-leoatchina
    双击install.cmd 
```

## 升级到最新版本
```bash
    cd spf13-vim-leoatchina 
    git pull
    vim +BundleInstall! +BundleClean +q
```

# 高度优化的.vimrc配置文件
![spf13-vimrc image][spf13-vimrc-img]
spf13对他作品的介绍是
> The .vimrc file is suited to programming. 
It is extremely well organized and folds in sections.
Each section is labeled and each option is commented.

我在他的基础上，做了一些*微小*的工作
1. 去除了一些比较冗余的插件，如[wildfire](wildfire)，并加入了自己喜欢的插件
2. 修改了安装代码，变成直接从clone下的目录软链接到用户目录下，不再支持XP
3. 按自己习惯修改了快捷键
4. 去除了原来定义的一些函数
5. 重点修改了代码补全插件[YouCompleteMe](YoucompleteME)、[Neocomplete](Neocomplet)的配置和快捷键
6. 去除了fork功能，仅保留before功能
7. 默认不进行代码补全，要使用者在 `~/.vimrc.before.local`里进行配置

## 快捷键

* Use `:q` to exit vim
* Certain commands are prefixed with a `<Leader>` key, which by default maps to `\`.
  Spf13-vim uses `let mapleader = ","` to change this to `,` which is in a consistent and
  convenient location.
* Keyboard [cheat sheet](http://www.viemu.com/vi-vim-cheat-sheet.gif).
* `<leader>`键为空格键
* `F1`: 为`:h `，方便启动帮助
* `F2`: 打开关闭代码折叠
* `F3`: 打开关闭换行
* `F4`: 打开关闭搜索高亮
* `F5`: 运行脚本(python、perl等);`S+F5`:运行脚本并记录时间;`<leader>+F5`: AsyncRun异步运行脚本
* `F6`: 打开NerdTree(文件浏览器)
* `F7`: 打开markdown-preview; `S-F7`:关闭markdown-preview
* `F9`: python调试节点
* `F11`: 全屏
* `F12`: 切换paste模式


## 定制个人配置

建立 `~/.vimrc.local` 或者`~/.gvimrc.local` 
在里面写入自己的配置
如，代替原有的代码高亮
```bash
    echo colorscheme ir_black  >> ~/.vimrc.local
```

### Before文件

[spf13](spf13)设计了一系列巧妙的`配置变量`，在`.vimrc.before.local`里写入配置变量后，可打开/关闭某些配置

如，关闭自动cd到某个目录
```bash
    echo let g:spf13_no_autochdir = 1 >> ~/.vimrc.before.local
```
在 `~/.vimrc.before`文件里可以看到详细的说明



# 插件

可以说，强大的插件系统是spf13的突出优点，通过这些插件，将原版的vim的功能作了极大的丰富。让界面更加美观，操作更加方便。
## 定制自用的插件 

spf13的定制插件配置在`.vimr.bundles`文件里，如果
建立`~/.vimrc.bundles.local` for any additional bundles.

To add a new bundle, just add one line for each bundle you want to install. The line should start with the word "Bundle" followed by a string of either the vim.org project name or the githubusername/githubprojectname. For example, the github project [spf13/vim-colors](https://github.com/spf13/vim-colors) can be added with the following command

```bash
    echo Bundle \'spf13/vim-colors\' >> ~/.vimrc.bundles.local
```

Once new plugins are added, they have to be installed.

```bash
    vim +BundleInstall! +BundleClean +q
```

## Removing (disabling) an included plugin


Add the UnBundle command to this line. It takes the same input as the Bundle line, so simply copy the line you want to disable and add 'Un' to the beginning.

For example, disabling the 'AutoClose' and 'scrooloose/syntastic' plugins

```bash
    echo UnBundle \'AutoClose\' >> ~/.vimrc.bundles.local
    echo UnBundle \'scrooloose/syntastic\' >> ~/.vimrc.bundles.local
```

**Remember to run ':BundleClean!' after this to remove the existing directories**


Here are a few of the plugins:


## [Undotree]

If you undo changes and then make a new change, in most editors the changes you undid are gone forever, as their undo-history is a simple list.
Since version 7.0 vim uses an undo-tree instead. If you make a new change after undoing changes, a new branch is created in that tree.
Combined with persistent undo, this is nearly as flexible and safe as git ;-)

Undotree makes that feature more accessible by creating a visual representation of said undo-tree.

**QuickStart** Launch using `<Leader>u`.

## [NERDTree]

NERDTree is a file explorer plugin that provides "project drawer"
functionality to your vim editing.  You can learn more about it with
`:help NERDTree`.

**QuickStart** Launch using `<Leader>e`.

**Customizations**:

* Use `<C-E>` to toggle NERDTree
* Use `<leader>e` or `<leader>nt` to load NERDTreeFind which opens NERDTree where the current file is located.
* Hide clutter ('\.pyc', '\.git', '\.hg', '\.svn', '\.bzr')
* Treat NERDTree more like a panel than a split.

## [ctrlp]
Ctrlp replaces the Command-T plugin with a 100% viml plugin. It provides an intuitive and fast mechanism to load files from the file system (with regex and fuzzy find), from open buffers, and from recently used files.

**QuickStart** Launch using `<c-p>`.

## [Surround]

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
      my $str = *whee!;         vllllS'     my $str = 'whee!';

For instance, if the cursor was inside `"foo bar"`, you could type
`cs"'` to convert the text to `'foo bar'`.

There's a lot more, check it out at `:help surround`

## [NERDCommenter]
非常好用的注释插件

## [neocomplete]

Neocomplete is an amazing autocomplete plugin with additional support for snippets. It can complete simulatiously from the dictionary, buffer, omnicomplete and snippets. This is the one true plugin that brings Vim autocomplete on par with the best editors.

**QuickStart** Just start typing, it will autocomplete where possible

**Customizations**:

 * Automatically present the autocomplete menu
 * Support tab and enter for autocomplete
 * `<C-k>` for completing snippets using [Neosnippet](https://github.com/Shougo/neosnippet.vim).

![neocomplete image][autocomplete-img]

## [YouCompleteMe]

YouCompleteMe is another amazing completion engine. It is slightly more involved to set up as it contains a binary component that the user needs to compile before it will work. As a result of this however it is very fast.

To enable YouCompleteMe add `youcompleteme` to your list of groups by overriding it in your `.vimrc.before.local` like so: `let g:spf13_bundle_groups=['general', 'programming', 'misc', 'scala', 'youcompleteme']` This is just an example. Remember to choose the other groups you want here.

Once you have done this you will need to get Vundle to grab the latest code from git. You can do this by calling `:BundleInstall!`. You should see YouCompleteMe in the list.

You will now have the code in your bundles directory and can proceed to compile the core. Change to the directory it has been downloaded to. If you have a vanilla install then `cd ~/.spf13-vim-3/.vim/bundle/YouCompleteMe/` should do the trick. You should see a file in this directory called install.sh. There are a few options to consider before running the installer:

  * Do you want clang support (if you don't know what this is then you likely don't need it)?
    * Do you want to link against a local libclang or have the installer download the latest for you?
  * Do you want support for c# via the omnisharp server?

The plugin is well documented on the site linked above. Be sure to give that a read and make sure you understand the options you require.

For java users wanting to use eclim be sure to add `let g:EclimCompletionMethod = 'omnifunc'` to your .vimrc.local.

## [Syntastic]

Syntastic is a syntax checking plugin that runs buffers through external syntax
checkers as they are saved and opened. If syntax errors are detected, the user
is notified and is happy because they didn't have to compile their code or
execute their script to find them.

## [AutoClose]

AutoClose does what you expect. It's simple, if you open a bracket, paren, brace, quote,
etc, it automatically closes it. It handles curlys correctly and doesn't get in the
way of double curlies for things like jinja and twig.


## [PIV]

The most feature complete and up to date PHP Integration for Vim with proper support for PHP 5.3+ including latest syntax, functions, better fold support, etc.

PIV provides:

 * PHP 5.3 support
 * Auto generation of PHP Doc (,pd on (function, variable, class) definition line)
 * Autocomplete of classes, functions, variables, constants and language keywords
 * Better indenting
 * Full PHP documentation manual (hit K on any function for full docs)

![php vim itegration image][phpmanual-img]

## [Ack.vim]

Ack.vim uses ack to search inside the current directory for a pattern.
You can learn more about it with `:help Ack`

**QuickStart** :Ack

## [Tabularize]

Tabularize lets you align statements on their equal signs and other characters

**Customizations**:

 * `<Leader>a= :Tabularize /=<CR>`
 * `<Leader>a: :Tabularize /:<CR>`
 * `<Leader>a:: :Tabularize /:\zs<CR>`
 * `<Leader>a, :Tabularize /,<CR>`
 * `<Leader>a<Bar> :Tabularize /<Bar><CR>`

## [Tagbar]

spf13-vim includes the Tagbar plugin. This plugin requires exuberant-ctags and will automatically generate tags for your open files. It also provides a panel to navigate easily via tags

**QuickStart** `CTRL-]` while the cursor is on a keyword (such as a function name) to jump to its definition.

**Customizations**: spf13-vim binds `<Leader>tt` to toggle the tagbar panel

![tagbar image][tagbar-img]

**Note**: For full language support, run `brew install ctags` to install
exuberant-ctags.

**Tip**: Check out `:help ctags` for information about VIM's built-in
ctag support. Tag navigation creates a stack which can traversed via
`Ctrl-]` (to find the source of a token) and `Ctrl-T` (to jump back up
one level).

## [EasyMotion]

EasyMotion provides an interactive way to use motions in Vim.

It quickly maps each possible jump destination to a key allowing very fast and
straightforward movement.

**QuickStart** EasyMotion is triggered using the normal movements, but prefixing them with `<leader><leader>`

For example this screen shot demonstrates pressing `,,w`

![easymotion image][easymotion-img]

## [Airline]

Airline provides a lightweight themable statusline with no external dependencies. By default this configuration uses the symbols `‹` and `›` as separators for different statusline sections but can be configured to use the same symbols as [Powerline]. An example first without and then with powerline symbols is shown here:

![airline image][airline-img]

To enable powerline symbols first install one of the [Powerline Fonts] or patch your favorite font using the provided instructions. Configure your terminal, MacVim, or Gvim to use the desired font. Finally add `let g:airline_powerline_fonts=1` to your `.vimrc.before.local`.

## Additional Syntaxes

spf13-vim ships with a few additional syntaxes:

* Markdown (bound to \*.markdown, \*.md, and \*.mk)
* Twig
* Git commits (set your `EDITOR` to `mvim -f`)

## Amazing Colors

spf13-vim includes [solarized] and [spf13 vim color pack](https://github.com/spf13/vim-colors/):

* ir_black
* molokai
* peaksea

Use `:color molokai` to switch to a color scheme.

Terminal Vim users will benefit from solarizing their terminal emulators and setting solarized support to 16 colors:

    let g:solarized_termcolors=16
    color solarized

Terminal emulator colorschemes:

* http://ethanschoonover.com/solarized (iTerm2, Terminal.app)
* https://github.com/phiggins/konsole-colors-solarized (KDE Konsole)
* https://github.com/sigurdga/gnome-terminal-colors-solarized (Gnome Terminal)

## Snippets

It also contains a very complete set of [snippets](https://github.com/spf13/snipmate-snippets) for use with snipmate or [neocomplete].


# Intro to VIM

Here's some tips if you've never used VIM before:

## Tutorials

* Type `vimtutor` into a shell to go through a brief interactive
  tutorial inside VIM.
* Read the slides at [VIM: Walking Without Crutches](https://walking-without-crutches.heroku.com/#1).

## Modes

* VIM has two (common) modes:
  * insert mode- stuff you type is added to the buffer
  * normal mode- keys you hit are interpreted as commands
* To enter insert mode, hit `i`
* To exit insert mode, hit `<ESC>`

## Useful commands

* Use `:q` to exit vim
* Certain commands are prefixed with a `<Leader>` key, which by default maps to `\`.
  Spf13-vim uses `let mapleader = ","` to change this to `,` which is in a consistent and
  convenient location.
* Keyboard [cheat sheet](http://www.viemu.com/vi-vim-cheat-sheet.gif).

[![Analytics](https://ga-beacon.appspot.com/UA-7131036-5/spf13-vim/readme)](https://github.com/igrigorik/ga-beacon)
[![Bitdeli Badge](https://d2weczhvl823v0.cloudfront.net/spf13/spf13-vim/trend.png)](https://bitdeli.com/free "Bitdeli Badge")
