# leoatchina的vim配置文件，从spf13的配置fork而来
> This is leoatchina's vim config forked from [spf13-vim:steve francia's vim distribution](https://github.com/spf13/spf13-vim)
Thanks him for great job.I changed lots of settings and plugins to suit my needs.


## 前言
本人是生信工程师,主要使用的语文是`python`,`R`,`perl`,`shell`,经常要ssh到远程服务器上写代码,因此学习了vim,后来发现了[spf13-vim:steve francia's vim distribution](https://github.com/spf13/spf13-vim),大大提高了写代码的效率.但是,原配置仍然有很多插件和配置不符合我的需要,因此,fork后进行了大量的修改.

上一张我的工作界面，配合iterm2的半透明效果看起来很酷炫

![](http://oxa21co60.bkt.clouddn.com/markdown-img-paste-20171009103355823.png)


## 安装 
### 要求 
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
    cd spf13-vim-leoatchina 
    git pull
    vim +BundleUpdate
    或者在vim里直接:BundleUpdate
```
![](http://oxa21co60.bkt.clouddn.com/markdown-img-paste-20171009103100770.png)

## 主要改动
我在spf13的基础上，做了一些*微小*的工作
1. 去除了一些比较冗余的插件，如[wildfire](wildfire)，并加入了自己喜欢的插件
2. 修改了安装代码，变成直接从clone下的目录软链接到用户目录下，**不再支持XP**
3. 按自己习惯修改了大量插件的快捷键
4. 去除了原来定义的一些函数
5. 重点修改了代码补全插件[YouCompleteMe](YoucompleteMe)、[Neocomplete](Neocomplet)的配置和快捷键
6. 去除了fork功能，仅保留before功能
7. 增加对R和Markdown的支持
8. 用`ywvim`作为中文输入法,请按`ctrl+\`切换到中文输入法,`ctrl+^`进行输入法配置
9. 默认不进行代码补全，要使用者在 `~/.vimrc.before.local`里进行配置,如我加入了对`youcompletme`的配置
```
  let g:spf13_bundle_groups=['general',  'programming', 'python', 'youcompleteme','php', 'javascript', 'html','R',]
```

## 对使用者的要求
了解基本的vim操作，知道`leader键`,`map`,`hjkl`,`d`,`w`,`s`,`i`,`u`,`:`,`\`等操作命令

## 基本快捷键
  * `<leader>`键改为空格键,`<localleader>`改为`\`,`\`在R编写调试时使用率比较高
  * 集成了`ywvim`输入法,在`insert`模式下通过`CTRL+\`开启,`CTRL+^`进行配置
  * `F1`: 为`:h `，方便启动帮助
  * `F2`: 打开关闭代码折叠
  * `F3`: 打开关闭换行
  * `F4`: 打开关闭搜索高亮
  * `F5`: 运行脚本（python、perl、c等）或生成markdown preview（markdown）;`Shift+F5`:运行脚本并记录时间;`<leader>+F5`: AsyncRun
  * `F9`: python调试节点,`S+F9`进行python语法检查,`<leader>+F9`切换语法是否检查
  * `F11`: gvim里的全屏切换
  * `F12`: 切换paste模式
  * `Ctrl+N` or `<leader>nn`: nerdtreeToggle
  * `Ctrl+T` or `<leader>tt`: tagbarToggle
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
* `Ctrl+e`移到一行的结尾;`Ctrl+y`移到一行的开头。
* `<leader>w`保存当前文件;`<leader>W`保存所有文件
* `<leader>q`关闭当然文件;`Q`为`:qa`,不过给你反悔的机会
* 复制粘贴等
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

* 其他一些快捷键
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

## 定制个人配置
*各配置文件执行次序*
  * `.vimrc.before` 
  * `.vimrc.before.local`
  * `.vimrc.bundles`
  * `.vimrc.bundles.local`
  * `.vimrc`
  * `.vimrc.local`

[spf13](spf13)设计了一系列巧妙的`配置变量`，在`.vimrc.before.local`里写入配置变量后，可打开/关闭某些配置
如，关闭自动cd到某个目录
```bash
    echo let g:spf13_no_autochdir = 1 >> ~/.vimrc.before.local
```
在 `~/.vimrc.before`文件里可以看到各个变量详细说明

## 插件系统
可以说，强大的插件系统是[spf13-vim]()的突出优点，通过这些插件，将原版的vim的功能作了极大的丰富。让界面更加美观，操作更加方便。

由于历史原因，没有选用[pathongen](https://github.com/tpope/vim-pathogen)作为插件管理器，还是选用经典的[vundle]()

## 定制自用的插件 
spf13的定制插件配置在`.vimr.bundles`文件里，如果
