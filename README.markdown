# leoatchina的vim配置文件
This is leoatchina's vim config forked from [spf13-vim:steve francia's vim distribution](https://github.com/spf13/spf13-vim).I sincerely thank him for great job. To meet my needs,I have changed lots of settings and plugins.






## 前言
本人是生信工程师,主要使用的语文是`python`,`R`,`perl`,`shell`,经常要ssh到远程服务器上写代码,因此学习了vim,后来发现了[spf13-vim:steve francia's vim distribution](https://github.com/spf13/spf13-vim),大大提高了写代码的效率。但是,原配置仍然有很多插件和配置不符合我的需要,因此,fork后进行了大量的修改.




## 对使用者的要求
了解`leader`,`map`,`hjkl`,`d`,`w`,`s`,`i`,`u`,`:`,`\`等操作命令

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
  vim +BundleUpdate
  或者在vim里直接  :BundleUpdate
```
![](http://oxa21co60.bkt.clouddn.com/markdown-img-paste-20171009103100770.png)

## 主要改动
我在spf13的基础上，做了一些*微小*的工作
1. 去除了一些比较冗余的插件，如[wildfire](wildfire)，并加入了自己喜欢的插件
2. 修改了安装代码，变成直接从clone的目录中软链接到用户目录下，**不再支持XP**
3. 按自己习惯修改了大量插件的快捷键
4. 去除了原来定义的一些函数
5. 重点修改了代码补全插件[YouCompleteMe](YoucompleteMe)、[Neocomplete](Neocomplete)的配置和快捷键
6. 去除了fork功能，仅保留before功能
7. 增加对R和Markdown的支持,不过要在`~/.vimrc.before.local`里开启
8. 用`ywvim`作为中文输入法,请按`ctrl+\`切换到中文输入法,`ctrl+^`进行输入法配置
9. 默认**不进行代码补全**，要使用者在 `~/.vimrc.before.local`里进行配置,如我加入了对`youcompletme`的配置,也可以使用`neocomplte`和`neocomplcache`
10. 我的`~/.vimrc.before.local`,可以看到有对`python`,`R`,`markdown`的支持
```
  let g:spf13_bundle_groups=['general',  'programming', 'python', 'youcompleteme','php', 'javascript', 'html','R','markdown','material']
```

## 基本快捷键
* `<leader>`键改为空格键,这个在键盘上最大的按键就有了更强的作用;`<localleader>`改为`\`,`\`在R编写调试时使用率比较高
* 集成了`ywvim`输入法,在`insert`模式下通过`CTRL+\`开启,`CTRL+^`进行配置
* `F1`: 为`:h `，方便启动帮助
* `F2`: 打开关闭代码折叠
* `F3`: 打开关闭换行
* `F4`: 打开关闭搜索高亮
* `F5`: 运行脚本（python、perl、c等）或生成markdown preview（markdown）;`Shift+F5`:运行脚本并记录时间;`<leader>+F5`: AsyncRun
* `F9`: python调试节点,`S+F9`进行python语法检查,`<leader>+F9`切换语法是否检查
* `F11`: gvim里的全屏切换
* `F12`: 切换paste模式
* `<leader>fc`:fixed confict markers
* `<leader>fw`:对当前行进行搜索
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
* `<leader>q`关闭当然文件;`Q`为`:qa`,不过给你反悔的机会不直接按下回车
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

## 定制个人配置
**各配置文件执行次序**
* `.vimrc.before`  # 参数配置文件
* `.vimrc.before.local`
* `.vimrc.bundles` # 插件管理文件
* `.vimrc.bundles.local`
* `.vimrc`      # 最主要的配置文件
* `.vimrc.local`

[spf13](spf13)设计了一系列巧妙的`配置变量`，在`.vimrc.before.local`里写入配置变量后，可打开/关闭某些配置
如，关闭自动cd到某个目录
```bash
    echo let g:spf13_no_autochdir = 1 >> ~/.vimrc.before.local
```
在 `~/.vimrc.before`文件里可以看到各个变量详细说明

## 插件
强大的插件系统是[spf13-vim](https://github.com/spf13/spf13-vim)的突出优点，通过这些插件，将原版的vim的功能作了极大的丰富。让界面更加美观，操作更加方便。
在原有的基础上，我加入了很多自己用的插件和对配置文件进行了修改

### 插件管理程序
spf13没有选用[pathongen](https://github.com/tpope/vim-pathogen)作为插件管理器，还是选用经典的[vundle](https://github.com/VundleVim/Vundle.vim)

### 插件网址
大部分插件网址为标题前加上`https://github.com`即可访问原网址

### 使用的插件
#### 颜色主题，内置两套
##### [tyrannicaltoucan/vim-quantum](https://github.com/tyrannicaltoucan/vim-quantum)
这是我在mac下的iterm2终端下使用的主题，material配色，配合半透明效果看起来很酷炫。
![](http://oxa21co60.bkt.clouddn.com/markdown-img-paste-20171009103355823.png)
但要求终端支持`True Color`，如果不支持，效果会惨不忍睹。因此改成手动开启，方法：在`~/.vimrc.before.local`里的`g:spf13_bundle_groups`列表里加入`material`
![](http://oxa21co60.bkt.clouddn.com/markdown-img-paste-20171011100637235.png)

##### [altercation/vim-colors-solarized](https://github.com/altercation/vim-colors-solarized)

经典主题，默认开启，给一张官方的图
![](https://raw.githubusercontent.com/altercation/solarized/master/img/solarized-vim.png)

#### [scrooloose/nerdtree](https://github.com/scrooloose/nerdtree)
在侧边显示当前目录，Toggle快捷键为`Ctrl-N`或者`<leader>nn`
![](http://oxa21co60.bkt.clouddn.com/markdown-img-paste-20171011101641847.png)

#### [majutsushi/tagbar](https://github.com/majutsushi/tagbar)
显示文档结构，在`python`,`vim`里肯定有用，要求在系统里安装`ctags`
用`Ctrl+T` or `<leader>tt`切换在测边显示文档结构
![](http://oxa21co60.bkt.clouddn.com/markdown-img-paste-20171011102150785.png)

#### [vim-voom/VOoM](https://github.com/vim-voom/VOoM)
另一个显示文档结构的插件，和`TagBar`逻辑不一样，`python`里肯定有用，其他语言我还没有测试出来。快捷键`<leader>vo`打开 `:Voom`命令;`<leader>vt`为`:VoomToggle`voom状态切换
![](http://oxa21co60.bkt.clouddn.com/markdown-img-paste-20171011102503348.png)

#### [mbbill/undotree](https://github.com/mbbill/undotree)
undo顾名思义，就是增强版的回退插件，通过`<leader>u`开启

#### [airline]()
漂亮的状态栏,能够显示很多状态。
![](http://oxa21co60.bkt.clouddn.com/markdown-img-paste-20171011105655369.png)
