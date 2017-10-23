# leoatchina的vim配置文件
This is leoatchina's vim config forked from [spf13-vim:steve francia's vim distribution](https://github.com/spf13/spf13-vim).I sincerely thank him for great job. To meet my needs,I have changed lots of settings and plugins.

                    __ _ _____              _
         ___ _ __  / _/ |___ /      __   __(_)_ __ ___
        / __| '_ \| |_| | |_ \ _____\ \ / /| | '_ ` _ \
        \__ \ |_) |  _| |___) |_____|\ V / | | | | | | |
        |___/ .__/|_| |_|____/        \_/  |_|_| |_| |_|
            |_|
[spf13]()对他自己作品的介绍
> spf13-vim is a distribution of vim plugins and resources for Vim, Gvim and MacVim.
> It is a good starting point for anyone intending to use VIM for development running equally well on Windows, Linux, \*nix and Mac.
> The distribution is completely customisable using a `~/.vimrc.local`, `~/.vimrc.bundles.local`, and `~/.vimrc.before.local` Vim RC files.

## 背景
本人是生信工程师,主要使用的语言是`python`,`R`,`perl`,`shell`,经常要ssh到远程服务器上写代码,因此学习了vim,后来发现了[spf13-vim:steve francia's vim distribution](https://github.com/spf13/spf13-vim),大大提高了写代码的效率。但是,原配置仍然有很多插件和配置不符合我的需要,因此,fork后进行了大量的修改.请访问[leoatchina的vim配置文件](https://github.com/leoatchina/spf13-vim-leoatchina).由于本人水平所限,一定有很多错误和bug,望各位指正.
**注意**使本配置文件后可能会影响vim运行流畅度.

## 对使用者的要求
掌握vim的大部分操作,了解`leader`,`map`,`hjkl`,`d`,`w`,`s`,`i`,`u`,`:`,`\`等操作命令

## 配置文件
**各配置文件执行次序**
* `.vimrc.before`  # 参数配置文件
* `.vimrc.before.local`
* `.vimrc.bundles` # 插件管理文件
* `.vimrc.bundles.local`
* `.vimrc`      # 最主要的配置文件,配色、快捷键、显示方式等参数都在这里设置
* `.vimrc.local`

## 安装
*安装本配置 需要 Git 1.7+ 和 Vim 7.3+（编译时加入对lua和python的支持），部分插件如`Nvim-R`,`AsyncRun`,需要Vim8.0*
如果要安装[neocomplete](https://github.com/Shougo/neocomplete.vim), 需要[vim with lua]().

### Linux, \*nix, Mac OSX 下的安装
```bash
  git clone https://github.com/leoatchina/spf13-vim-leoatchina.git
  cd spf13-vim-leoatchina
  bash bootstrap.sh
```

### windows下的安装
```bash
  git clone https://github.com/leoatchina/spf13-vim-leoatchina.git
  cd spf13-vim-leoatchina
  点击install.cmd
```

### 升级到最新版本
```bash
  vim +BundleUpdate
  或者在vim里直接  :BundleUpdate
```
![](http://oxa21co60.bkt.clouddn.com/markdown-img-paste-20171009103100770.png)

## 一些功能
* 复制内容直接放到系统剪贴本
* 显示行号,多种语法高亮
* Visual模式下用`>`,`<`移动文字不会取消选择
* 不生成backup文件
* 关闭拼写检查
* 关闭声音
* 关闭列光标加亮
* 关闭行光标加亮
* 允许折行
* 不代码折叠
* 开启实时搜索功能
* 显示光标当前位置
* 高亮显示搜索结果
* 折叠模式下翻页的改进
* 智能缩进
* 没有滚动条
* 没有菜单和工具条
* 总是显示状态栏

## 主要改动
我在spf13的基础上，做了一些*微小*的工作
1. 去除了一些比较冗余的插件，如[wildfire](wildfire)，并加入了自己喜欢的插件
2. 修改了安装代码，变成直接从clone的目录中软链接到用户目录下，**不再支持XP**
3. 按自己习惯修改了大量插件的快捷键
4. 去除了原来定义的一些函数
5. 重点修改了代码补全插件[YouCompleteMe](YoucompleteMe)、[Neocomplete](Neocomplete)的配置和快捷键
6. 去除了fork文件功能，仅保留before文件功能
7. 增加对R和Markdown的支持,要在`~/.vimrc.before.local`里加入`markdown`和`R`开启，支持语法高亮，不过暂时这两个语言的支持都有一些问题，要改动。
8. 默认**不进行代码补全**，要使用者在 `~/.vimrc.before.local`里进行配置,如我加入了对`youcompletme`的配置,也可以使用`neocomplete`或`neocomplcache`
9. 我的`~/.vimrc.before.local`,可以看到有对`python`,`R`,`markdown`的支持
```
  let g:spf13_bundle_groups=['general',  'programming', 'python', 'youcompleteme','php', 'javascript', 'html','R','markdown','material']
```

## 基本快捷键
* `<leader>`键改为空格键,这个在键盘上最大的按键就有了更强的作用;
* `<localleader>`改为`\`,`\`在R编写调试时使用率比较高
* `~`作为进入`ex`模式的快捷键,`Q`键map为`<Nop>`
* `F1`: 为`:h `，方便启动帮助
* `F2`: 打开关闭代码折叠 或 `<leader>fd`
* `F3`: 打开关闭换行 或 `<leader>fr`
* `F4`: 打开关闭搜索高亮 或 `<leader>fh`
* `F5`: 运行脚本（python、perl、c等）或 `<leader>R`;`Shift+F5`:运行脚本并记录时间;`<leader>+F5`: AsyncRun命令
* `F11`: 全屏切换,如果是windows下的gvim,要把本目录下的`gvim_fullscreen.dll`放到`gvim`的安装目录下，此时<S+F11>为切换透明度
* `F12`: 切换paste模式,或者`<leader>fp`
* `<leader>fc`:fixed confict markers
* `<leader>fw`:对当前光标下文字进行搜索
* `<leader>mk`:markdown调用chrome生成markdown preview
* 在`Visual`模式下按`.`为退出`Visual`模式
* 标签页控制
```
  nnoremap <silent>-  : tabprevious<CR>
  nnoremap <silent>=  : tabnext<CR>
  nnoremap <leader>tf : tabfirst<CR>
  nnoremap <Leader>tl : tablast<CR>
  nnoremap <leader>tn : tabnew<CR>
  nnoremap <Leader>ts : tabs<CR>
  nnoremap <Leader>tp : tab split<CR>
  nnoremap <Leader>te : tabe<SPACE>
  nnoremap <Leader>tm : tabm<SPACE>
  nnoremap <silent>_  : tabm -1<CR>
  nnoremap <silent>+  : tabm +1<CR>
```
* `Ctrl+e`移到一行的结尾;`Ctrl+y`移到一行的开头
* `Ctrl+m`括号之间跳转
* `<leader>w`保存当前文件;`<leader>W`保存所有文件
* `<leader>q`关闭当然文件;`Q`为`:qa!`,不过给你反悔的机会不直接按下回车
* 复制粘贴等
```
  " 设置快捷键将选中文本块复制至系统剪贴板
  vnoremap  <leader>y  "+y
  nnoremap  <leader>y  "+y
  nnoremap  <leader>Y  "+yg
  nnoremap  <leader>yy  "+yy
  " Yank from the cursor to the end of the line
  nnoremap Y y$
  " p and P for paste
  nnoremap <leader>p "+p
  nnoremap <leader>P "+P
  vnoremap <leader>p "+p
  vnoremap <leader>P "+P
```
* 其他一些快捷键
```
  " buffer switch
  nnoremap <leader>bn :bn<CR>
  nnoremap <leader>bp :bp<CR>
  " 定义快捷键保存当前窗口内容
  nmap <Leader>w :w<CR>
  nmap <Leader>W :wq!<CR>
  " 定义快捷键保存所有窗口内容并退出 vim
  nmap <Leader>WQ :wa<CR>:q<CR>
  " 定义快捷键关闭当前窗口
  nmap <Leader>q :q<CR>
  " 不做任何保存，直接退出 vim
  nmap <Leader>Q :qa!<CR>
  " 设置分割页面
  nmap <Leader>- :split<Space>
  nmap <leader>\ :vsplit<Space>
  nmap <leader>= <C-W>=
  "设置垂直高度减增
  nmap <Leader>{ :resize -3<CR>
  nmap <Leader>} :resize +3<CR>
  "设置水平宽度减增
  nmap <Leader>[ :vertical resize -3<CR>
  nmap <Leader>] :vertical resize +3<CR>
  "至左方的子窗口
  nnoremap <Leader>H <C-W>H
  "至右方的子窗口
  nnoremap <Leader>L <C-W>L
  "至上方的子窗口
  nnoremap <Leader>K <C-W>K
  "至下方的子窗口
  nnoremap <Leader>J <C-W>J
  " Visual shifting (does not exit Visual mode)
  vnoremap < <gv
  vnoremap > >gv
  " Ctrl-m for switch between brackets
  map <C-m> %
```

[spf13](spf13)设计了一系列巧妙的`配置变量`，在`.vimrc.before.local`里写入配置变量后，可打开/关闭某些配置
如，关闭自动cd到某个目录
```bash
    echo let g:spf13_no_autochdir = 1 >> ~/.vimrc.before.local
```
在 `~/.vimrc.before`文件里可以看到各个变量详细说明

## 插件系统
强大的插件系统是[spf13-vim](https://github.com/spf13/spf13-vim)的突出优点，通过这些插件，将原版的vim的功能作了极大的丰富。让界面更加美观，操作更加方便。在原有的基础上，我加入了很多自己用的插件和对配置文件进行了修改
spf13没有选用[pathongen](https://github.com/tpope/vim-pathogen)作为插件管理器，还是选用经典的[vundle](https://github.com/VundleVim/Vundle.vim)

### 使用的插件
#### 内置两套颜色主题
##### [tyrannicaltoucan/vim-quantum](https://github.com/tyrannicaltoucan/vim-quantum)
这是我在mac下的iterm2终端下使用的主题，material配色，配合半透明效果看起来很酷炫。
![](http://oxa21co60.bkt.clouddn.com/markdown-img-paste-20171012074022187.png)
但要求终端支持`True Color`，如果不支持，效果会惨不忍睹。因此改成手动开启，方法：在`~/.vimrc.before.local`里的`g:spf13_bundle_groups`列表里加入`material`
![](http://oxa21co60.bkt.clouddn.com/markdown-img-paste-20171011100637235.png)

##### [altercation/vim-colors-solarized](https://github.com/altercation/vim-colors-solarized)
经典主题，默认开启，给一张官方的图
![](https://raw.githubusercontent.com/altercation/solarized/master/img/solarized-vim.png)
还有三套内置配色方案`ir_black`,`molokai`,`peaksea`,通过`:color`命令开启

#### [scrooloose/nerdtree](https://github.com/scrooloose/nerdtree)
在侧边显示当前目录，Toggle快捷键为`Ctrl-N`或者`<leader>nn`
![](http://oxa21co60.bkt.clouddn.com/markdown-img-paste-20171011101641847.png)

#### [majutsushi/tagbar](https://github.com/majutsushi/tagbar)
显示文档结构，在`python`,`vim`里肯定有用，要求在系统里安装`ctags`
用`Ctrl+T` or `<leader>tt`切换在测边显示文档结构.在bar窗口里按`F1`调出帮助窗口
![](http://oxa21co60.bkt.clouddn.com/markdown-img-paste-20171011102150785.png)


#### [vim-voom/VOoM](https://github.com/vim-voom/VOoM)
另一个显示文档结构的插件，和`TagBar`逻辑不一样，`python`里肯定有用，其他语言我还没有测试出来。快捷键`<leader>vo`打开 `:Voom`命令;`<leader>vt`为`:VoomToggle`voom状态切换
![](http://oxa21co60.bkt.clouddn.com/markdown-img-paste-20171012105213969.png)

#### [mbbill/undotree](https://github.com/mbbill/undotree)
undotree顾名思义,增强版的回退插件，快捷键`<leader>u`

#### [airline](https://github.com/vim-airline-themes)
漂亮的状态栏,能够显示很多状态。
![](http://oxa21co60.bkt.clouddn.com/markdown-img-paste-20171011105655369.png)

#### [ywvim中文输入法](https://github.com/leoatchina/ywvim)
`ywvim`中文输入法,直接在vim里内置,无意中发现要和[fcitx](https://github.com/fcitx/fcitx)配合使用否则会有bug,在`insert`模式下通过`CTRL+@`或`CTRL+\`开启,`CTRL+^`进行配置.`;`临时英文输入法;注意,默认只输入**英文状态**的标点,而且首选是`五笔`;`z`临时拼音;`,.-=`上下翻页;
![](http://oxa21co60.bkt.clouddn.com/markdown-img-paste-20171011215538461.png)
![](http://oxa21co60.bkt.clouddn.com/markdown-img-paste-20171011212612850.png)

#### [markdown]()
默认开户对markdown语言的高亮支持,如`.vimrc.before.local`里指定`markdown`支持,按`<leadr>mk`调用`chrome`打开markdown预览,不过这个功能还要仔细测试过.

#### [fugitive](https://github.com/tpope/vim-fugitive)
对git的支持,具体可以看官方说明,不过我就设置了快捷键`<leader>gi :Git<Space>`,操作体验接近终端下输入`git`命令

#### [scrooloose/nerdcommenter](https://github.com/scrooloose/nerdcommenter)
注释插件,神器,直接上官方的快捷键,最常用的是`<leader>c<space>`
  * `[count]<leader>cc` **|NERDComComment|**
    Comment out the current line or text selected in visual mode.
  * `[count]<leader>cn` **|NERDComNestedComment|**
    Same as <leader>cc but forces nesting.
  * `[count]<leader>c<space>` **|NERDComToggleComment|**
    Toggles the comment state of the selected line(s). If the topmost selected line is commented, all selected lines are uncommented and vice versa.
  * `[count]<leader>cm` **|NERDComMinimalComment|**
    Comments the given lines using only one set of multipart delimiters.
  * `[count]<leader>ci` **|NERDComInvertComment|**
    Toggles the comment state of the selected line(s) individually.
  * `[count]<leader>cs` **|NERDComSexyComment|**
    Comments out the selected lines with a pretty block formatted layout.
  * `[count]<leader>cy` **|NERDComYankComment|**
    Same as <leader>cc except that the commented line(s) are yanked first.
  * `<leader>c$` **|NERDComEOLComment|**
    Comments the current line from the cursor to the end of line.
  * `<leader>cA` **|NERDComAppendComment|**
    Adds comment delimiters to the end of line and goes into insert mode between them.
  * **|NERDComInsertComment|**
    Adds comment delimiters at the current cursor position and inserts between. Disabled by default.
  * `<leader>ca` **|NERDComAltDelim|**
    Switches to the alternative set of delimiters.
  * `[count]<leader>cl`
    `[count]<leader>cb` **|NERDComAlignedComment|**
    Same as **|NERDComComment|** except that the delimiters are aligned down the left side (`<leader>cl`) or both sides (`<leader>cb`).
  * `[count]<leader>cu` **|NERDComUncommentLine|**
    Uncomments the selected line(s).

#### [rking/ag.vim](https://github.com/rking/ag.vim)
 `Ag`是一个非常快的文件/文本搜索工具,通过`<leader>ag`开遍文本搜索,`<leader>af`文件搜索
![](http://oxa21co60.bkt.clouddn.com/markdown-img-paste-20171012075230167.png)

#### [Tabularize](https://github.com/godlygeek/tabular)
自动按特定的符号对齐,快捷键见`.vimrc`里的配置文件
  ```
      nmap <Leader>a& :Tabularize /&<CR>
      vmap <Leader>a& :Tabularize /&<CR>
      nmap <Leader>a= :Tabularize /^[^=]*\zs=<CR>
      vmap <Leader>a= :Tabularize /^[^=]*\zs=<CR>
      nmap <Leader>a=> :Tabularize /=><CR>
      vmap <Leader>a=> :Tabularize /=><CR>
      nmap <Leader>a: :Tabularize /:<CR>
      vmap <Leader>a: :Tabularize /:<CR>
      nmap <Leader>a:: :Tabularize /:\zs<CR>
      vmap <Leader>a:: :Tabularize /:\zs<CR>
      nmap <Leader>a, :Tabularize /,<CR>
      vmap <Leader>a, :Tabularize /,<CR>
      nmap <Leader>a,, :Tabularize /,\zs<CR>
      vmap <Leader>a,, :Tabularize /,\zs<CR>
      nmap <Leader>a<Bar> :Tabularize /<Bar><CR>
      vmap <Leader>a<Bar> :Tabularize /<Bar><CR>
  ```

#### [sessionman](https://github.com/vim-scripts/sessionman.vim)
sessionmanager,`<leader>sl`显示session列表;`<leader>ss`保存session;`<leader>sc`关闭session

#### [ctrlp](https://github.com/ctrlpvim/ctrlp.vim)
杀手级插件,引用网上的一段话对它的介绍
> 在 VIM 世界里，有人是分窗口编辑文件的忠实拥护者，有人则是多文件 tab 页的死忠骨灰粉。但无论哪种人， 在一些大项目内进行编辑工作时，如果要快速打开 './lib/foo/bar/comm/base_utils.py' 这类藏在大山深处的文件，都需像剥粽子一样，一层一层往下找，让人头疼。
>
> ctrlp.vim 则完美帮你解决了这个痛点，当你想打开某个文件时，只要按下 Ctrl + p 快捷键，输入文件名。 所有和这个文件名匹配的文件都会被按照优先级列出来，按下 enter 或者 Ctrl + t 就可以在当前 buffer 或者新 tab 页打开你要的文件了。

网上找来的图
![](http://zuyunfei.com/images/ctrlp-vim-demo.gif)

`ctrl+p`启动插件,`<leader>fu`启动funksky函数查询功能,在启动后,用`Ctrl+f`,`Ctrl+b`在不同模式中切换.
在文件列表中,`Ctrl+k/j`或者方向键向上/下选择文件,`t`在新标签里打开文件.其他快捷键见[ctrlp中文介绍](http://blog.codepiano.com/pages/ctrlp-cn.light.html)

#### [Pymode](https://github.com/python-mode/python-mode)
`python`用的插件,具有语法检查,调试等功能.`F9`: python语法检查,`S+F9`切换语法是否检查.`<leader>R`:运行脚本;`<leader>T`:track_point toggle

#### [surround](https://github.com/tpope/vim-surround)
给一段文字加上括号的插件，下面说明文字引用自[vim中的杀手级别的插件：surround](http://zuyunfei.com/2013/04/17/killer-plugin-of-vim-surround/)
```
   Old text                  Command     New text
   "Hello *world!"           ds"         Hello world!
   [123+4*56]/2              cs])        (123+456)/2
   "Look ma, I'm *HTML!"     cs"<q>      <q>Look ma, I'm HTML!</q>
   if *x>3 {                 ysW(        if ( x>3 ) {
   my $str = *whee!;         vlllls'     my $str = 'whee!';
   <div>Yo!*</div>           dst         Yo!
   <div>Yo!*</div>           cst<p>      <p>Yo!</p>
```
如上面代码块所示，添加替换时使用后半括号)]}，添加的括号和内容间就没有空格（如第2个示例），反之会在内容前后添加一个空格（如第4个实例）。第6个示例中的t代表一对HTML或者xml tag。其他表示范围的符号：w代表word, W代表WORD(被空格分开的连续的字符窜），p代表paragraph。

*命令列表*
```
    Normal mode
    -----------
    ds  - delete a surrounding
    cs  - change a surrounding
    ys  - add a surrounding
    yS  - add a surrounding and place the surrounded text on a new line + indent it
    yss - add a surrounding to the whole line
    ySs - add a surrounding to the whole line, place it on a new line + indent it
    ySS - same as ySs
    Visual mode
    -----------
    s   - in visual mode, add a surrounding
    S   - in visual mode, add a surrounding but place text on new line + indent it
    Insert mode "不建议使用
    -----------
    <CTRL-s> - in insert mode, add a surrounding
    <CTRL-s><CTRL-s> - in insert mode, add a new line + surrounding + indent
    <CTRL-g>s - same as <CTRL-s>
    <CTRL-g>S - same as <CTRL-s><CTRL-s>
```

#### [PIV](https://github.com/spf13/PIV)
The most feature complete and up to date PHP Integration for Vim with proper support for PHP 5.3+ including latest syntax, functions, better fold support, etc.

PIV provides:
 * PHP 5.3 support
 * Auto generation of PHP Doc (,pd on (function, variable, class) definition line)
 * Autocomplete of classes, functions, variables, constants and language keywords
 * Better indenting
 * Full PHP documentation manual (hit K on any function for full docs)
![](https://camo.githubusercontent.com/7650c13f52fd73dc788a218f6d5862be4399cd4d/68747470733a2f2f692e696d6775722e636f6d2f63304747502e706e67)

#### [Nvim-R](https://github.com/jalvesaq/Nvim-R)
支持R语言的插件,需要`Vim8.0+`并在`.vimrc.before.local`里加入`R`.通过`<leader>rr`激活界面,`<leader>rq`退出R程序
![](http://oxa21co60.bkt.clouddn.com/markdown-img-paste-20171012132459533.png)
![](http://oxa21co60.bkt.clouddn.com/markdown-img-paste-20171012132515614.png)
快捷键极多,请自行`:h nvim-r`查询

#### [EasyMotion](https://github.com/easymotion/vim-easymotion)
 又一个杀手级别的插件
 ![](http://www.wklken.me/imgs/vim/easy_motion_search.gif)
 1. 跳转到当前光标前后,快捷键`<leader><leader>w`和`<leader><leader>b`
 2. 搜索跳转,`<leader><leader>s`,然后输入要搜索的字母
 3. 行间/行内级别跳转,`<leader><leader>`再`hjkl`不解释
 4. 重复上一次的动作,`<leader><leader>.`
 5. 还可以`<leader><leader>f`和`<leader><leader>t`,不过不建议使用

#### 代码补全插件
- 用了三种补全插件，要在`~/.vimrc.before.local`里加入`youcomplteme`或`neocomplte`或`neocomplcache`来激活安装。
- 基本快捷键统一为`Tab`、`Shift-Tab`为`向下`，`向上`翻页，`ctrl-n/p`也能进行翻页.`enter`或`Ctrl+k`激活补全。`ctrl+l`显示各种实例可能
##### [YouComplteMe](https://github.com/Valloric/YouCompleteMe)
  ![](https://camo.githubusercontent.com/1f3f922431d5363224b20e99467ff28b04e810e2/687474703a2f2f692e696d6775722e636f6d2f304f50346f6f642e676966)
  - 需要安装一系列编译用软件
  - 跳转键，`Ctrl+f`跳转到下一个待补全处，`Ctrl+b`中转到上一个待补全处。
  - 具体可参考[Vim 自动补全插件 YouCompleteMe 安装与配置](http://howiefh.github.io/2015/05/22/vim-install-youcompleteme-plugin/).
  - 在安装好各种编译用的工具后
  ```
     cd ~/.vim/bundle/YouCompleteMe
     python2 install.py #可能仅支持python等不需要编译的语言
  ```
##### [neocomplete&&neocomplcache]()
  - 这两者是同一个作者编写，`neocomplete`需要`lua`的支持
  - 相对来说，neo系列没ycm稳定，速度也不如，不过配置方便

  ![](https://camo.githubusercontent.com/2e00f5d1f66bcf290533cc0b006a692339dfa4a6/68747470733a2f2f662e636c6f75642e6769746875622e636f6d2f6173736574732f34313439352f3632323435372f66653930616435652d636634322d313165322d386530332d3866313839623565323665352e706e67)
