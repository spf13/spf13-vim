" This is leoatchina's vim config forked from https://github.com/spf13/spf13-vim
" Sincerely thank him for his great job, and I have made some change according to own requires.
" These ones whole use this config can also make changes,
" For my pool English , I sometimes type in Chines comments
"                    __ _ _____              _
"         ___ _ __  / _/ |___ /      __   __(_)_ __ ___
"        / __| '_ \| |_| | |_ \ _____\ \ / /| | '_ ` _ \
"        \__ \ |_) |  _| |___) |_____|\ V / | | | | | | |
"        |___/ .__/|_| |_|____/        \_/  |_|_| |_| |_|
"            |_|
" You can find spf13's greate config at http://spf13.com
" Os detect functions have been move to .vimrc.bundles
" Basics
    set nocompatible        " Must be first line
    set background=dark     " Assume a dark background
    set mouse=a             " Automatically enable mouse usage
    set mousehide           " Hide the mouse cursor while typing
    scriptencoding utf-8
" Gui
    if !has('gui')
        if !has('nvim')
            set term=$TERM          " Make arrow and other keys work
        endif
    else
        set guifont=Consolas:h11
    endif
" Clipboard
    if has('clipboard')
        if has('unnamedplus')  " When possible use + register for copy-paste
            set clipboard=unnamed,unnamedplus
        else         " On mac and Windows, use * register for copy-paste
            set clipboard=unnamed
        endif
    endif
" Arrow Key Fix
    " https://github.com/spf13/spf13-vim/issues/780
    if &term[:4] == "xterm" || &term[:5] == 'screen' || &term[:3] == 'rxvt'
        inoremap <silent> <C-[>OC <RIGHT>
    endif
" set timeout
    set timeoutlen=400
    set ttimeout ttimeoutlen=200
" Use before config
    if filereadable(expand("~/.vimrc.before"))
        source ~/.vimrc.before
    endif
" Use bundles config
    if filereadable(expand("~/.vimrc.bundles"))
        source ~/.vimrc.bundles
    endif
" Key (re)Mappings
    " The default leader is '\', spf13 prefer ';' as it's in a standard location
    " But leatchina prefer space. To override this behavior and set it back to '\'
    " (or any other character) add the following to your .vimrc.before.local file:
    if !exists('g:spf13_leader')
        let mapleader=' '
    else
        let mapleader=g:spf13_leader
    endif
    if !exists('g:spf13_localleader')
        let maplocalleader = '\'
    else
        let maplocalleader=g:spf13_localleader
    endif
    " 不同文件类型加载不同插件
    filetype plugin indent on   " Automatically detect file types.
    filetype on                 " 开启文件类型侦测
    filetype plugin on          " 根据侦测到的不同类型:加载对应的插件
    syntax on
" take config into effect after saving
    au! bufwritepost .vimrc source %
    au! bufwritepost .vimrc.before source %
    au! bufwritepost .vimrc.bundles source %
    au! bufwritepost .vimrc.local source %
    au! bufwritepost .vimrc.before.local source %
    au! bufwritepost .vimrc.bundles.local source %
" Some useful shortcuts by spf13
    " Find merge conflict markers
    map <leader>fc /\v^[<\|=>]{7}( .*\|$)<CR>
    " Allow using the repeat operator with a visual selection (!)
    " http://stackoverflow.com/a/8064607/127816
    vnoremap . :normal .<CR>
    " For when you forget to sudo.. Really Write the file.
    cmap w!! w !sudo tee % >/dev/null
    " Some helpers to edit mode
    " http://vimcasts.org/e/14
    cnoremap %% <C-R>=fnameescape(expand('%:h')).'/'<cr>
    " Map <Leader>fw to display all lines with keyword under cursor
    " and ask which one to jump to
    nmap <Leader>fw [I:let nr = input("Which one: ")<Bar>exe "normal " . nr ."[\t"<CR>
    " Code folding options
    nmap <leader>f0 :set foldlevel=0<CR>
    nmap <leader>f1 :set foldlevel=1<CR>
    nmap <leader>f2 :set foldlevel=2<CR>
    nmap <leader>f3 :set foldlevel=3<CR>
    nmap <leader>f4 :set foldlevel=4<CR>
    nmap <leader>f5 :set foldlevel=5<CR>
    nmap <leader>f6 :set foldlevel=6<CR>
    nmap <leader>f7 :set foldlevel=7<CR>
    nmap <leader>f8 :set foldlevel=8<CR>
    nmap <leader>f9 :set foldlevel=9<CR>
    " fullscreen mode for GVIM and Terminal, need 'wmctrl' in you PATH
    if !WINDOWS()
        map <silent> <F11> :call system("wmctrl -ir " . v:windowid . " -b toggle,fullscreen")<CR>
    else
        autocmd GUIEnter * simalt ~x
        " 按 F11 切换全屏
        noremap <F11> <esc>:call libcallnr('gvim_fullscreen.dll', 'ToggleFullscreen', 0)<cr>
        " 按 S-F11 切换窗口透明度
        noremap <S-F11> <esc>:call libcallnr('gvim_fullscreen.dll', 'ToggleTransparency', "247,180")<cr>
    endif
    set pastetoggle=<F12>      " pastetoggle (sane indentation on pastes)
    noremap <leader>tg :set nopaste! nopaste?<CR>

" shortcuts by leatchina
    if !exists('g:no_leoatchina_config')
        " move to last or first position of a line
        nmap <silent><C-y> ^
        " FIXME: c-y sometime not work in centos
        imap <silent><C-y> <ESC>^i
        nmap <silent><C-e> $
        imap <silent><C-e> <ESC>A
        nmap <silent><C-m> %
        vmap <silent><C-m> %
        if isdirectory(expand("~/.vim/bundle/vim-toggle-quickfix"))
            nmap <F10> <Plug>window:quickfix:toggle
            imap <F10> <Plug>window:quickfix:toggle
        endif
        " tab contral
        set tabpagemax=10 " Only show 10 tabs
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
        " 设置快捷键将选中文本块复制至系统剪贴板
        vnoremap  <leader>y  "+y
        nnoremap  <leader>y  "+y
        nnoremap  <leader>Y  "+yg
        nnoremap  <leader>yy  "+yy
        " Yank from the cursor to the end of the line, to be consistent with C and D.
        nnoremap Y y$
        " p and P for paste
        nnoremap <leader>p "+p
        nnoremap <leader>P "+P
        vnoremap <leader>p "+p
        vnoremap <leader>P "+P
        " Easier horizontal scrolling
        map zl zL
        map zh zH
        " Wrapped lines goes down/up to next row, rather than next line in file.
        noremap j gj
        noremap k gk
        "F1 help
        nmap <F1> :h<SPACE>
        "F2 toggleFold
        noremap <F2> :set nofoldenable! nofoldenable?<CR>
        noremap <leader>fd :set nofoldenable! nofoldenable?<CR>
        "F3 toggleWrap
        noremap <F3> :set nowrap! nowrap?<CR>
        noremap <leader>nw :set nowrap! nowrap?<CR>
        "F4 toggle hlsearch
        noremap <F4> :set nohlsearch! nohlsearch?<CR>
        noremap <leader>hl :set nohlsearch! nohlsearch?<CR>
        " F5运行脚本
        noremap <F5> :call CompileRunGcc()<CR>
        noremap <leader>R :call CompileRunGcc()<CR>
        func! CompileRunGcc()
            exec "w"
            if &filetype == 'c'
                exec "!g++ % -o %<"
                exec "!./%<"
            elseif &filetype == 'cpp'
                exec "!g++ % -o %<"
                exec "!./%<"
            elseif &filetype == 'java'
                exec "!javac %"
                exec "!java %<"
            elseif &filetype == 'sh'
                exec "!bash %"
            elseif &filetype == 'perl'
                exec "!perl %"
            elseif &filetype == 'go'
                exec "!go run %"
            endif
       endfunc
        " S-F5 time the program testing
        noremap <S-F5> :call TimeCompileRunGcc()<CR>
        func! TimeCompileRunGcc()
            exec "w"
            " asyncrun 是一个异步执行脚本的插件，要vim8.0以上才支持
            if isdirectory(expand("~/.vim/bundle/asyncrun.vim"))
                if &filetype == 'c'
                    exec ":AsyncRun g++ % -o %<"
                    exec ":AsyncRun ./%<"
                elseif &filetype == 'cpp'
                    exec ":AsyncRun g++ % -o %<"
                    exec ":AsyncRun ./%<"
                elseif &filetype == 'java'
                    exec ":AsyncRun javac %"
                    exec ":AsyncRun java %<"
                elseif &filetype == 'sh'
                    exec ":AsyncRun bash %"
                elseif &filetype == 'python'
                    exec ":AsyncRun python %"
                elseif &filetype == 'perl'
                    exec ":AsyncRun perl %"
                elseif &filetype == 'go'
                    exec ":AsyncRun go run %"
                endif
            else
                if &filetype == 'c'
                    exec "!g++ % -o %<"
                    exec "!time ./%<"
                elseif &filetype == 'cpp'
                    exec "!g++ % -o %<"
                    exec "!time ./%<"
                elseif &filetype == 'java'
                    exec "!javac %"
                    exec "!time java %<"
                elseif &filetype == 'sh'
                    exec "!time bash %"
                elseif &filetype == 'python'
                    exec "!time python %"
                    elseif &filetype == 'perl'
                        exec "!time perl %"
                    elseif &filetype == 'go'
                        exec "!time go run %"
                    endif
                endif
            endfunc
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
            nmap <Leader>Q :qa!
            " Q
            nnoremap ~ Q
            nmap Q <Nop>
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
        endif
    " Formatting
        " auto close qfixwindows when leave vim
        aug QFClose
          au!
          au WinEnter * if winnr('$') == 1 && getbufvar(winbufnr(winnr()), "&buftype") == "quickfix"|q|endif
        aug END
        set number                      " show line number"
        set autoindent                  " Indent at the same level of the previous line
        set nojoinspaces                " Prevents inserting two spaces after punctuation on a join (J)
        set splitright                  " Puts new vsplit windows to the right of the current
        set splitbelow                  " Puts new split windows to the bottom of the current
        " 不生成back文件
        set nobackup
        set nowritebackup
        " 关闭拼写检查
        set nospell
        " 关闭声音
        set noeb
        set vb
        " 关闭列光标加亮
        set nocursorcolumn
        " 关闭行光标加亮
        set nocursorline
        " 允许折行
        set wrap
        " 不折叠
        set nofoldenable
        " 标签控制
        set showtabline=2
        " 开启实时搜索功能
        set incsearch
        " 显示光标当前位置
        set ruler
        " 高亮显示搜索结果
        set hlsearch
        set incsearch                   " Find as you type search
        set smartcase                   " Case sensitive when uc present
        set ignorecase                  " Case insensitive search
        " 一些格式
        set backspace=indent,eol,start  " Backspace for dummies
        set linespace=0                 " No extra spaces between rows
        set showmatch                   " Show matching brackets/parenthesis
        set winminheight=0              " Windows can be 0 line high
        set wildmenu                    " Show list instead of just completing
        set wildmode=list:longest,full  " Command <Tab> completion, list matches, then longest common part, then all.
        set whichwrap=b,s,h,l,<,>,[,]   " Backspace and cursor keys wrap too
        set scrolljump=5                " Lines to scroll when cursor leaves screen
        set scrolloff=3                 " Minimum lines to keep above and below cursor
        set list
        set listchars=tab:›\ ,trail:•,extends:#,nbsp:. " Highlight problematic whitespacetextwidth=200
        set formatoptions-=tc           " Not aut break a line into multiple lines
        set shiftwidth=4                " Use indents of 4 spaces
        set expandtab                   " Tabs are spaces, not tabs
        set tabstop=4                   " An indentation every four columns
        set softtabstop=4               " Let backspace delete indent
        " 没有滚动条
        set guioptions-=l
        set guioptions-=L
        set guioptions-=r
        set guioptions-=R
        " 没有菜单和工具条
        set guioptions-=m
        set guioptions-=T
        " 总是显示状态栏
        set laststatus=2
        " sepcial setting for different type of files
        au BufNewFile,BufRead *.py
            \set shiftwidth=4
            \set tabstop=4
            \set softtabstop=4
            \set expandtab
            \set autoindent
            \set foldmethod=indent
        "set comments=sl:/*,mb:*,elx:*/  " auto format comment blocks
        " Remove trailing whitespaces and ^M chars
        autocmd FileType markdown,vim,c,cpp,java,go,php,javascript,puppet,python,rust,twig,xml,yml,perl,sql autocmd BufWritePre <buffer> if !exists('g:spf13_keep_trailing_whitespace') | call StripTrailingWhitespace() | endif
        autocmd BufNewFile,BufRead *.html.twig set filetype=html.twig
        autocmd BufNewFile,BufRead *.md,*.markdown set filetype=markdown
        autocmd BufNewFile,BufRead *.pandoc set filetype=pandoc
        autocmd FileType haskell,puppet,ruby,yml setlocal expandtab shiftwidth=2 softtabstop=2
        " preceding line best in a plugin but here for now.
        autocmd BufNewFile,BufRead *.coffee set filetype=coffee
        " Workaround vim-commentary for Haskell
        autocmd FileType haskell setlocal commentstring=--\ %s
        " Workaround broken colour highlighting in Haskell
        autocmd FileType haskell,rust setlocal nospell
    " General
        " Most prefer to automatically switch to the current file directory when
        " a new buffer is opened; to prevent this behavior, add the following to
        " your .vimrc.before.local file:
        "   let g:spf13_no_autochdir = 1
        if !exists('g:spf13_no_autochdir')
            autocmd BufEnter * if bufname("") !~ "^\[A-Za-z0-9\]*://" | lcd %:p:h | endif
            " Always switch to the current file directory
        endif
        " http://vim.wikia.com/wiki/Restore_cursor_to_file_position_in_previous_editing_session
        " Restore cursor to file position in previous editing session
        " To disable this, add the following to your .vimrc.before.local file:
        "   let g:spf13_no_restore_cursor = 1
        if !exists('g:spf13_no_restore_cursor')
            function! ResCur()
                if line("'\"") <= line("$")
                    silent! normal! g`"
                    return 1
                endif
            endfunction
            augroup resCur
                autocmd!
                autocmd BufWinEnter * call ResCur()
            augroup END
        endif
        " To disable views add the following to your .vimrc.before.local file:
        "   let g:spf13_no_views = 1
        if !exists('g:spf13_no_views')
            " Add exclusions to mkview and loadview
            " eg: *.*, svn-commit.tmp
            let g:skipview_files = [
                \ '\[example pattern\]'
                \ ]
        endif
    " Vim UI
        if !exists('g:override_spf13_bundles') && !exists('g:no_colorscheme')
            if count(g:spf13_bundle_groups, 'material') && isdirectory(expand("~/.vim/bundle/vim-quantum"))
                set background=dark
                set termguicolors
                colorscheme quantum
            else
                if  filereadable(expand("~/.vim/bundle/vim-colors-solarized/colors/solarized.vim"))
                    let g:solarized_termcolors=256
                    let g:solarized_termtrans=1
                    let g:solarized_visibility="normal"
                    colorscheme solarized
                    color solarized
                endif
            endif
        endif
        if has('cmdline_info')
            set ruler                   " Show the ruler
            set rulerformat=%30(%=\:b%n%y%m%r%w\ %l,%c%V\ %P%) " A ruler on steroids
            set showcmd                 " Show partial commands in status line and
        endif
        if has('statusline')
            set laststatus=2
            " Broken down into easily includeable segments
            set statusline=%<%f\                     " Filename
            set statusline+=%w%h%m%r                 " Options
            if !exists('g:override_spf13_bundles')
                if isdirectory(expand("~/.vim/bundle/fugitive"))
                    set statusline+=%{fugitive#statusline()} " Git Hotness
                endif
            endif
            set statusline+=\ [%{&ff}/%Y]            " Filetype
            set statusline+=\ [%{getcwd()}]          " Current dir
            set statusline+=%=%-14.(%l,%c%V%)\ %p%%  " Right aligned file nav info
        endif
        " End/Start of line motion keys act relative to row/wrap width in the
        " presence of `:set wrap`, and relative to line for `:set nowrap`.
        " Default vim behaviour is to act relative to text line in both cases
        " If you prefer the default behaviour, add the following to your
        " .vimrc.before.local file:
        "   let g:spf13_no_wrapRelMotion = 1
        if !exists('g:spf13_no_wrapRelMotion')
            " Same for 0, home, end, etc
            function! WrapRelativeMotion(key, ...)
                let vis_sel=""
                if a:0
                    let vis_sel="gv"
                endif
                if &wrap
                    execute "normal!" vis_sel . "g" . a:key
                else
                    execute "normal!" vis_sel . a:key
                endif
            endfunction

            " Map g* keys in Normal, Operator-pending, and Visual+select
            noremap $ :call WrapRelativeMotion("$")<CR>
            noremap <End> :call WrapRelativeMotion("$")<CR>
            noremap 0 :call WrapRelativeMotion("0")<CR>
            noremap <Home> :call WrapRelativeMotion("0")<CR>
            noremap ^ :call WrapRelativeMotion("^")<CR>
            " Overwrite the operator pending $/<End> mappings from above
            " to force inclusive motion with :execute normal!
            onoremap $ v:call WrapRelativeMotion("$")<CR>
            onoremap <End> v:call WrapRelativeMotion("$")<CR>
            " Overwrite the Visual+select mode mappings from above
            " to ensuwe the correct vis_sel flag is passed to function
            vnoremap $ :<C-U>call WrapRelativeMotion("$", 1)<CR>
            vnoremap <End> :<C-U>call WrapRelativeMotion("$", 1)<CR>
            vnoremap 0 :<C-U>call WrapRelativeMotion("0", 1)<CR>
            vnoremap <Home> :<C-U>call WrapRelativeMotion("0", 1)<CR>
            vnoremap ^ :<C-U>call WrapRelativeMotion("^", 1)<CR>
        endif
    " Stupid shift key fixes
        if !exists('g:spf13_no_keyfixes')
            if has("user_commands")
                command! -bang -nargs=* -complete=file E e<bang> <args>
                command! -bang -nargs=* -complete=file W w<bang> <args>
                command! -bang -nargs=* -complete=file Wq wq<bang> <args>
                command! -bang -nargs=* -complete=file WQ wq<bang> <args>
                command! -bang Wa wa<bang>
                command! -bang WA wa<bang>
                command! -bang Q q<bang>
                command! -bang QA qa<bang>
                command! -bang Qa qa<bang>
            endif
            cmap Tabe tabe
        endif
    " Plugins
        " ywvim,vim里的中文输入法
            if isdirectory(expand("~/.vim/bundle/ywvim"))
                let g:ywvim_ims=[
                            \['wb', '五笔', 'wubi.ywvim'],
                            \['py', '拼音', 'pinyin.ywvim'],
                            \]
                let g:ywvim_py = { 'helpim':'wb', 'gb':0 }
                let g:ywvim_zhpunc = 0
                let g:ywvim_listmax = 8
                let g:ywvim_esc_autoff = 1
                let g:ywvim_autoinput = 2
                let g:ywvim_circlecandidates = 1
                let g:ywvim_helpim_on = 0
                let g:ywvim_matchexact = 0
                let g:ywvim_chinesecode = 1
                let g:ywvim_gb = 0
                let g:ywvim_preconv = 'g2b'
                let g:ywvim_conv = ''
                let g:ywvim_lockb = 1
            endif
        " Ag
            if isdirectory(expand("~/.vim/bundle/ag.vim"))
                nnoremap <leader>ag :Ag<space>
                nnoremap <leader>af :AgFile<space>
                let g:ag_working_path_mode="r"
                set runtimepath^=~/.vim/bundle/ag.vim"
            endif
        " NerdTree
            if isdirectory(expand("~/.vim/bundle/nerdtree"))
                nmap <silent><C-n> <plug>NERDTreeTabsToggle<CR>
                nmap <leader>nn <plug>NERDTreeTabsToggle<CR>
                nmap <leader>nt :NERDTreeFind<CR>
                "let g:NERDShutUp=1
                let NERDTreeShowBookmarks=1
                let NERDTreeIgnore=['\.py[cd]$', '\~$', '\.swo$', '\.swp$', '^\.git$', '^\.hg$', '^\.svn$', '\.bzr$']
                let NERDTreeChDirMode=0
                let NERDTreeQuitOnOpen=1
                let NERDTreeMouseMode=2
                let NERDTreeShowHidden=1
                let NERDTreeKeepTreeInNewTab=1
                let g:nerdtree_tabs_open_on_gui_startup=0
            endif
        " VOom
            if isdirectory(expand("~/.vim/bundle/VOom"))
                let g:voom_ft_modes = {'md':'markdown','markdown': 'markdown', 'pandoc': 'pandoc','c':'fmr2', 'cpp':'fmr2', 'python':'python','vim':'vimwiki'}
                nmap <silent><leader>vt :VoomToggle<CR>
                nmap <leader>vo :Voom<Space>
            endif
        " markdown
            if isdirectory(expand("~/.vim/bundle/markdown-preview.vim"))
                nmap <leader>mk <Plug>MarkdownPreview
                if OSX()
                    let g:mkdp_path_to_chrome = "OPEN -a Google\\ Chrome"
                else
                    let g:mkdp_path_to_chrome = "google-chrome"
                endif
            endif
        " PIV
            if isdirectory(expand("~/.vim/bundle/PIV"))
                let g:DisableAutoPHPFolding = 0
                let g:PIVAutoClose = 0
            endif
        " fugitive
            if isdirectory(expand("~/.vim/bundle/vim-fugitive"))
                nmap <Leader>gi :Git<Space>
            endif
        " AsyncRun
            if isdirectory(expand("~/.vim/bundle/asyncrun.vim"))
                nmap <Leader><F5> :AsyncRun<Space>
            endif
        " Misc
            if isdirectory(expand("~/.vim/bundle/matchit.zip"))
                let b:match_ignorecase = 1
            endif
        " Ctags
            set tags=./tags;/,~/.vimtags
            " Make tags placed in .git/tags file available in all levels of a repository
            let gitroot = substitute(system('git rev-parse --show-toplevel'), '[\n\r]', '', 'g')
            if gitroot != ''
                let &tags = &tags . ',' . gitroot . '/.git/tags'
            endif
        " AutoCloseTag
            " Make it so AutoCloseTag works for xml and xhtml files as well
            au FileType xhtml,xml ru ftplugin/html/autoclosetag.vim
            nmap <Leader>ac <Plug>ToggleAutoCloseMappings
        " TagBar
            let s:has_tagbar = 0
            if isdirectory(expand("~/.vim/bundle/tagbar/"))
                nmap <silent><C-T> :TagbarToggle<CR>
                nmap <silent><leader>tt :TagbarToggle<CR>
                nnoremap <silent><leader>jt :TagbarOpen j<CR>
                let s:has_nerdtree = 1
                let g:NERDTreeWinSize=30
                let g:NERDTreeShowBookmarks=1
                let g:NERDTreeIgnore=['\.py[cd]$', '\~$', '\.swo$', '\.swp$', '^\.git$', '^\.hg$', '^\.svn$', '\.bzr$']
                let g:NERDTreeChDirMode=0
                let g:NERDTreeQuitOnOpen=1
                let g:NERDTreeMouseMode=2
                let g:NERDTreeShowHidden=1
                let g:NERDTreeKeepTreeInNewTab=1
                let g:nerdtree_tabs_focus_on_files = 1
                let g:nerdtree_tabs_open_on_gui_startup = 0
                let g:NERDTreeWinPos=0
                let g:NERDTreeDirArrowExpandable = '▸'
                let g:NERDTreeDirArrowCollapsible = '▾'
                autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") &&b:NERDTreeType == "primary") | q | endif
                " nerdtree-git
                if isdirectory(expand("~/.vim/bundle/nerdtree-git-plugin"))
                    let g:NERDTreeIndicatorMapCustom = {
                            \ "Modified"  : "*",
                            \ "Staged"    : "+",
                            \ "Untracked" : "★",
                            \ "Renamed"   : "→ ",
                            \ "Unmerged"  : "=",
                            \ "Deleted"   : "X",
                            \ "Dirty"     : "●",
                            \ "Clean"     : "√",
                            \ "Unknown"   : "?"
                    \ }
                endif
            endif
        " Tabularize
            if isdirectory(expand("~/.vim/bundle/tabular"))
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
            endif
        " Session List
            set sessionoptions=blank,buffers,curdir,folds,tabpages,winsize
            if isdirectory(expand("~/.vim/bundle/sessionman.vim/"))
                nmap <leader>sl :SessionList<CR>
                nmap <leader>ss :SessionSave<CR>
                nmap <leader>sc :SessionClose<CR>n
            endif
        " Nvim-R
            if isdirectory(expand("~/.vim/bundle/Nvim-R"))
                let R_rconsole_width = 0
                map <leader>rr \rf\ro<C-w>h
                map <leader>rq \rq
                let R_objbr_place = "script,right"
                autocmd VimResized * let R_rconsole_height = winheight(0) /3
                let R_objbr_h = 25
                let R_objbr_opendf = 1    " Show data.frames elements
                let R_objbr_openlist = 1  " Show lists elements
                let R_objbr_allnames = 0  " Show .GlobalEnv hidden objects
                let R_objbr_labelerr = 1  " Warn if label is not a valid text
                let R_in_buffer = 0
                let R_hl_term = 1
                let R_close_term = 1
                let Rout_more_colors = 1
                let R_hi_fun_paren = 1
                let R_rmd_environment = "new.env()"
            endif
        " GoLang
            if count(g:spf13_bundle_groups, 'go')
                let g:go_highlight_functions = 1
                let g:go_highlight_methods = 1
                let g:go_highlight_structs = 1
                let g:go_highlight_operators = 1
                let g:go_highlight_build_constraints = 1
                let g:go_fmt_command = "goimports"
                let g:syntastic_go_checkers = ['golint', 'govet', 'errcheck']
                let g:syntastic_mode_map = { 'mode': 'active', 'passive_filetypes': ['go'] }
                au FileType go nmap <Leader>S <Plug>(go-implements)
                au FileType go nmap <Leader>S <Plug>(go-info)
                au FileType go nmap <Leader>E <Plug>(go-rename)
                au FileType go nmap <leader>R <Plug>(go-run)
                au FileType go nmap <leader>B <Plug>(go-build)
                au FileType go nmap <leader>T <Plug>(go-test)
                au FileType go nmap <Leader>gd <Plug>(go-doc)
                au FileType go nmap <Leader>gv <Plug>(go-doc-vertical)
                au FileType go nmap <leader>co <Plug>(go-coverage)
            endif
        " JSON
            nmap <leader>jst <Esc>:%!python -m json.tool<CR><Esc>:set filetype=json<CR>
            let g:vim_json_syntax_conceal = 0
        " PyMode
            if isdirectory(expand("~/.vim/bundle/python-mode"))
                " pymode check
                let g:pymode_lint = 1
                let g:pymode_lint_on_write = 1
                let g:pymode_lint_checkers = ['pyflakes','pep8']
                let g:pymode_lint_ignore = "E128,E2,E3,E501"
                let g:pymode_lint_cwindow = 1
                let g:pymode_lint_message = 0
                nmap <F9> :PymodeLint<CR>
                imap <F9> <ESC>:PymodeLint<CR>i
                nmap <S-F9> :PymodeLintToggle<cr>
                " motion
                let g:pymode_motion = 1
                " no doc for python
                let g:pymode_doc = 1
                " run python
                let g:pymode_run_bind = '<leader>R'
                " breakpoint
                let g:pymode_breakpoint_bind = '<leader>T'
                let g:pymode_trim_whitespaces = 1
                let g:pymode_options = 0
                let g:pymode_rope = 0
                let g:pymode_rope_completion = 0
            endif
            " Disable if python support not present
            if !has('python') && !has('python3')
                let g:pymode = 0
            endif
        " ctrlp
            if isdirectory(expand("~/.vim/bundle/ctrlp.vim/"))
                let g:ctrlp_working_path_mode = 'ar'
                nnoremap <silent> <D-t> :CtrlP<CR>
                nnoremap <silent> <D-r> :CtrlPMRU<CR>
                let g:ctrlp_custom_ignore = {
                    \ 'dir':  '\.git$\|\.hg$\|\.svn$',
                    \ 'file': '\.exe$\|\.so$\|\.dll$\|\.pyc$' }
                if executable('ag')
                    let s:ctrlp_fallback = 'ag %s --nocolor -l -g ""'
                elseif executable('ack-grep')
                    let s:ctrlp_fallback = 'ack-grep %s --nocolor -f'
                elseif executable('ack')
                    let s:ctrlp_fallback = 'ack %s --nocolor -f'
                " On Windows use "dir" as fallback command.
                elseif WINDOWS()
                    let s:ctrlp_fallback = 'dir %s /-n /b /s /a-d'
                else
                    let s:ctrlp_fallback = 'find %s -type f'
                endif
                if exists("g:ctrlp_user_command")
                    unlet g:ctrlp_user_command
                endif
                let g:ctrlp_user_command = {
                    \ 'types': {
                        \ 1: ['.git', 'cd %s && git ls-files . --cached --exclude-standard --others'],
                        \ 2: ['.hg', 'hg --cwd %s locate -I .'],
                    \ },
                    \ 'fallback': s:ctrlp_fallback
                \ }
                if isdirectory(expand("~/.vim/bundle/ctrlp-funky/"))
                    " CtrlP extensions
                    let g:ctrlp_extensions = ['funky']
                    " funky
                    nnoremap <Leader>fu :CtrlPFunky<Cr>
                endif
            endif
        " Rainbow
            if isdirectory(expand("~/.vim/bundle/rainbow/"))
                let g:rainbow_active = 1 "0 if you want to enable it later via :RainbowToggle
            endif
        " YouCompleteMe
            if count(g:spf13_bundle_groups, 'youcompleteme')
                set completeopt=longest,menu
                au InsertLeave * if pumvisible() == 0|pclose|endif "离开插入模式后关闭预览窗口
                let g:ycm_python_binary_path = 'python'
                let g:acp_enableAtStartup = 0
                let g:ycm_add_preview_to_completeopt = 1
                "  补全后关键窗口
                let g:ycm_autoclose_preview_window_after_completion = 1
                "  插入后关键窗口
                let g:ycm_autoclose_preview_window_after_insertion = 1
                " enable completion from tags
                let g:ycm_collect_identifiers_from_tags_files = 1
                let g:ycm_key_invoke_completion = '<Nop>'
                let g:ycm_key_list_select_completion = ['<Tab>','<Down>']
                let g:ycm_key_list_previous_completion = ['<S-Tab>','<Up>']
                " remap Ultisnips for compatibility for YCM
                let g:UltiSnipsListSnippets="<C-l>"
                let g:UltiSnipsExpandTrigger = '<C-k>'
                let g:UltiSnipsJumpForwardTrigger = '<C-f>'
                let g:UltiSnipsJumpBackwardTrigger = '<C-b>'
                " Ctrl+j for enter or stop pum
                inoremap <expr> <C-j> pumvisible() ? "\<C-y>\<C-y>" : "\<CR>"
                " cr for ExpandTrigger
                function! g:UltiSnips_CR()
                    if pumvisible()
                        call UltiSnips#ExpandSnippet()
                        " 0:ExpandSnippet failed
                        if g:ulti_expand_res == 0
                            return "\<C-y>"
                        else
                            call feedkeys("\<C-c>")
                            return "\<Right>"
                        endif
                    else
                        return "\<CR>"
                    endif
                endfunction
                au BufEnter * exec "inoremap <silent> <CR> <C-R>=g:UltiSnips_CR()<cr>"
                let g:UltiSnipsUsePythonVersion = 2
                " Ulti的代码片段的文件夹
                let g:UtiSnipsSnippetDirectories=["bundle/vim-snippets/UltiSnips"]
                " 自定义代码片段的文件夹
                let g:UltiSnipsSnippetsDir = "~/.vim/UltiSnips"
                let g:ycm_filetype_blacklist = {
                      \ 'tagbar' : 1,
                      \ 'nerdtree' : 1,
                      \}
                let g:ycm_filetype_whitelist = {
                    \ 'cpp': 1,
                    \ 'c': 1,
                    \ 'perl':1,
                    \ 'python':1,
                    \ 'js':1,
                    \ 'html':1,
                    \ 'php':1,
                \}
                " Haskell post write lint and check with ghcmod
                " $ `cabal install ghcmod` if missing and ensure
                " ~/.cabal/bin is in your $PATH.
                if !executable("ghcmod")
                    autocmd BufWritePost *.hs GhcModCheckAndLintAsync
                endif
                " For snippet_complete marker.
                if !exists("g:spf13_no_conceal")
                    if has('conceal')
                        set conceallevel=2 concealcursor=i
                    endif
                endif
                let g:ycm_confirm_extra_conf=1 "加载.ycm_extra_conf.py提示
                let g:ycm_collect_identifiers_from_tags_files=1    " 开启 YC基于标签引擎
                let g:ycm_min_num_of_chars_for_completion=2   " 从第2个键入字符就开始罗列匹配项
                let g:ycm_cache_omnifunc=0 " 禁止缓存匹配项,每次都重新生成匹配项
                let g:ycm_seed_identifiers_with_syntax=1   " 语法关键字补全
                ""在注释输入中也能补全
                let g:ycm_complete_in_comments = 1
                "在字符串输入中也能补全
                let g:ycm_complete_in_strings = 1
                "注释和字符串中的文字也会被收入补全
                let g:ycm_collect_identifiers_from_comments_and_strings = 0
                " 跳转到定义处
                nnoremap <leader>jd :YcmCompleter GoToDefinitionElseDeclaration<CR>
        " neocomplete
            elseif count(g:spf13_bundle_groups, 'neocomplete')
                let g:neocomplete_enable_insert_char_pre = 1
                let g:neocomplete_enable_at_startup = 1
                let g:neocomplete_enable_auto_select = 1
                let g:neocomplete_enable_camel_case_completion = 1
                let g:neocomplete_enable_smart_case = 1
                let g:neocomplete_enable_underbar_completion = 1
                let g:neocomplete_enable_auto_delimiter = 1
                let g:neocomplete_max_list = 15
                let g:neocomplete_force_overwrite_completefunc = 1
                " Define dictionary.
                let g:neocomplete_dictionary_filetype_lists = {
                            \ 'default' : '',
                            \ 'vimshell' : $HOME.'/.vimshell_hist',
                            \ 'scheme' : $HOME.'/.gosh_completions'
                            \ }
                " Define keyword.
                if !exists('g:neocomplete_keyword_patterns')
                    let g:neocomplete_keyword_patterns = {}
                endif
                let g:neocomplete_keyword_patterns._ = '\h\w*'

                " Enable heavy omni completion.
                if !exists('g:neocomplete_omni_patterns')
                    let g:neocomplete_omni_patterns = {}
                endif
                let g:neocomplete_omni_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
                let g:neocomplete_omni_patterns.perl = '\h\w*->\h\w*\|\h\w*::'
                let g:neocomplete_omni_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
                let g:neocomplete_omni_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'
                let g:neocomplete_omni_patterns.ruby = '[^. *\t]\.\h\w*\|\h\w*::'
                let g:neocomplete_omni_patterns.go = '\h\w*\.\?'
         " neocomplcache
            elseif count(g:spf13_bundle_groups, 'neocomplcache')
                let g:neocomplcache_enable_insert_char_pre = 1
                let g:neocomplcache_enable_at_startup = 1
                let g:neocomplcache_enable_auto_select = 1
                let g:neocomplcache_enable_camel_case_completion = 1
                let g:neocomplcache_enable_smart_case = 1
                let g:neocomplcache_enable_underbar_completion = 1
                let g:neocomplcache_enable_auto_delimiter = 1
                let g:neocomplcache_max_list = 15
                let g:neocomplcache_force_overwrite_completefunc = 1
                " Define dictionary.
                let g:neocomplcache_dictionary_filetype_lists = {
                            \ 'default' : '',
                            \ 'vimshell' : $HOME.'/.vimshell_hist',
                            \ 'scheme' : $HOME.'/.gosh_completions'
                            \ }
                " Enable heavy omni completion.
                if !exists('g:neocomplcache_omni_patterns')
                    let g:neocomplcache_omni_patterns = {}
                endif
                let g:neocomplcache_omni_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
                let g:neocomplcache_omni_patterns.perl = '\h\w*->\h\w*\|\h\w*::'
                let g:neocomplcache_omni_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
                let g:neocomplcache_omni_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'
                let g:neocomplcache_omni_patterns.ruby = '[^. *\t]\.\h\w*\|\h\w*::'
                let g:neocomplcache_omni_patterns.go = '\h\w*\.\?'
                " Define keyword.
                if !exists('g:neocomplcache_keyword_patterns')
                    let g:neocomplcache_keyword_patterns = {}
                endif
                let g:neocomplcache_keyword_patterns._ = '\h\w*'
        " Normal Vim omni-completion ,if not set completion method , it works
        " To disable omni complete, add the following to your .vimrc.before.local file:
        " let g:spf13_no_omni_complete = 1
            elseif !exists('g:spf13_no_omni_complete')
                " Enable omni-completion.
                autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
                autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
                autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
                autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
                autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
                autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete
                autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

                if has("autocmd") && exists("+omnifunc")
                autocmd Filetype *
                    \if &omnifunc == "" |
                    \setlocal omnifunc=syntaxcomplete#Complete |
                    \endif
                endif
                hi Pmenu  guifg=#000000 guibg=#F8F8F8 ctermfg=black ctermbg=Lightgray
                hi PmenuSbar  guifg=#8A95A7 guibg=#F8F8F8 gui=NONE ctermfg=darkcyan ctermbg=lightgray cterm=NONE
                hi PmenuThumb  guifg=#F8F8F8 guibg=#8A95A7 gui=NONE ctermfg=lightgray ctermbg=darkcyan cterm=NONE
                " Some convenient mappings
                "inoremap <expr> <Esc>      pumvisible() ? "\<C-e>" : "\<Esc>"
                if exists('g:spf13_map_cr_omni_complete')
                    inoremap <expr> <CR>     pumvisible() ? "\<C-y>" : "\<CR>"
                endif
                inoremap <expr> <Down>     pumvisible() ? "\<C-n>" : "\<Down>"
                inoremap <expr> <Up>       pumvisible() ? "\<C-p>" : "\<Up>"
                inoremap <expr> <C-d>      pumvisible() ? "\<PageDown>\<C-p>\<C-n>" : "\<C-d>"
                inoremap <expr> <C-u>      pumvisible() ? "\<PageUp>\<C-p>\<C-n>" : "\<C-u>"
                " Automatically open and close the popup menu / preview window
                au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
                set completeopt=menu,preview,longest
            endif
        " Snippets  and key map for neocomplete && neocomplcache
            if count(g:spf13_bundle_groups, 'neocomplcache') || count(g:spf13_bundle_groups, 'neocomplete')
                " <C-h>, <BS>: close popup and delete backword char.
                if count(g:spf13_bundle_groups,'neocomplcache')
                    inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
                    " c-l to list
                    inoremap <expr><C-l> neocomplcache#complete_common_string()
                    snoremap <expr><C-l> neocomplcache#complete_common_string()
                    " c-j to complete pum or cr
                    inoremap <expr> <C-j> pumvisible() ? neocomplcache#close_popup(): "\<CR>"
                    snoremap <expr> <C-j> pumvisible() ? neocomplcache#close_popup(): "\<CR>"
                else
                    inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
                    " c-l to list
                    inoremap <expr><C-l> neocomplete#complete_common_string()
                    snoremap <expr><C-l> neocomplete#complete_common_string()
                    inoremap <expr> <C-j> pumvisible() ? neocomplete#close_popup(): "\<CR>"
                    snoremap <expr> <C-j> pumvisible() ? neocomplete#close_popup(): "\<CR>"
                endif
                " c-k to expand
                imap <C-k> <Plug>(neosnippet_expand)
                smap <C-k> <Plug>(neosnippet_expand)
                " c-f tu jump
                imap <C-f> <Right><Plug>(neosnippet_jump)
                smap <C-f> <Right><Plug>(neosnippet_jump)
                " <TAB><S-Tab> to select
                inoremap <expr><Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
                inoremap <expr><S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
                " Ctrl+j for enter or stop pum
                " cr to expand when available,or just enter
                function! g:Neo_Complete()
                    if pumvisible()
                        if neosnippet#expandable()
                            return neosnippet#mappings#expand_impl()
                        else
                            if count(g:spf13_bundle_groups,'neocomplcache')
                                return neocomplcache#close_popup()
                            else
                                return neocomplete#close_popup()
                            endif
                        endif
                    else
                        return "\<CR>"
                    endif
                endfunction
                au BufEnter * exec "inoremap <CR> <C-R>=g:Neo_Complete()<cr><C-c>"
                " Use honza's snippets.
                let g:neosnippet#snippets_directory='~/.vim/bundle/vim-snippets/snippets'
                " Enable neosnippet snipmate compatibility mode
                let g:neosnippet#enable_snipmate_compatibility = 1
                " For snippet_complete marker.
                if !exists("g:spf13_no_conceal")
                    if has('conceal')
                        set conceallevel=2 concealcursor=i
                    endif
                endif
                " Enable neosnippets when using go
                let g:go_snippet_engine = "neosnippet"
            endif
        " UndoTree
            if isdirectory(expand("~/.vim/bundle/undotree/"))
                nnoremap <Leader>u :UndotreeToggle<CR>
                " If undotree is opened, it is likely one wants to interact with it.
                let g:undotree_SetFocusWhenToggle=1
            endif
        " indent_guides
            if isdirectory(expand("~/.vim/bundle/vim-indent-guides/"))
                let g:indent_guides_start_level = 2
                let g:indent_guides_guide_size = 1
                let g:indent_guides_enable_on_vim_startup = 1
            endif
        " vim-airline
            " Set configuration options for the statusline plugin vim-airline.
            " Use the powerline theme and optionally enable powerline symbols.
            " To use the symbols , , , , , , and .in the statusline
            " segments add the following to your .vimrc.before.local file:
            " let g:airline_powerline_fonts=1
            " If the previous symbols do not render for you then install a
            " powerline enabled font.
            " See `:echo g:airline_theme_map` for some more choices
            " Default in terminal vim is 'dark'
            if isdirectory(expand("~/.vim/bundle/vim-airline-themes/"))
                if !exists('g:no_colorscheme')
                    if count(g:spf13_bundle_groups, 'material') && isdirectory(expand("~/.vim/bundle/vim-quantum"))
                        let g:airline_theme = 'quantum'
                    else
                        if  filereadable(expand("~/.vim/bundle/vim-colors-solarized/colors/solarized.vim"))
                            let g:airline_theme = 'solarized'
                        endif
                    endif
                    if !exists('g:airline_powerline_fonts')
                        " Use the default set of separators with a few customizations
                        let g:airline_left_sep='›'  " Slightly fancier than '>'
                        let g:airline_right_sep='‹' " Slightly fancier than '<'
                    endif
                endif
            endif
" GUI Settings
    " GVIM- (here instead of .gvimrc)
    if has('gui_running')
        set guioptions-=T           " Remove the toolbar
        set lines=40                " 40 lines of text instead of 24
    endif
" Functions
    " Initialize directories
    function! InitializeDirectories()
        let parent = $HOME
        let prefix = 'vim'
        let dir_list = {
            \ 'backup': 'backupdir',
            \ 'views': 'viewdir',
            \ 'swap': 'directory' }
        if has('persistent_undo')
            let dir_list['undo'] = 'undodir'
        endif
        " To specify a different directory in which to place the vimbackup,
        " vimviews, vimundo, and vimswap files/directories, add the following to
        " your .vimrc.before.local file:
        "   let g:spf13_consolidated_directory = <full path to desired directory>
        "   eg: let g:spf13_consolidated_directory = $HOME . '/.vim/'
        if exists('g:spf13_consolidated_directory')
            let common_dir = g:spf13_consolidated_directory . prefix
        else
            let common_dir = parent . '/.' . prefix
        endif
        for [dirname, settingname] in items(dir_list)
            let directory = common_dir . dirname . '/'
            if exists("*mkdir")
                if !isdirectory(directory)
                    call mkdir(directory)
                endif
            endif
            if !isdirectory(directory)
                echo "Warning: Unable to create backup directory: " . directory
                echo "Try: mkdir -p " . directory
            else
                let directory = substitute(directory, " ", "\\\\ ", "g")
                exec "set " . settingname . "=" . directory
            endif
        endfor
    endfunction
    call InitializeDirectories()
    " Strip whitespace
    function! StripTrailingWhitespace()
        let _s=@/
        let l = line(".")
        let c = col(".")
        " do the business:
        %s/\s\+$//e
        " clean up: restore previous search history, and cursor position
        let @/=_s
        call cursor(l, c)
    endfunction
    " Shell command
    function! s:RunShellCommand(cmdline)
        botright new
        setlocal buftype=nofile
        setlocal bufhidden=delete
        setlocal nobuflisted
        setlocal noswapfile
        setlocal nowrap
        setlocal filetype=shell
        setlocal syntax=shell
        call setline(1, a:cmdline)
        call setline(2, substitute(a:cmdline, '.', '=', 'g'))
        execute 'silent $read !' . escape(a:cmdline, '%#')
        setlocal nomodifiable
        1
    endfunction
    command! -complete=file -nargs=+ Shell call s:RunShellCommand(<q-args>)
    function! s:ExpandFilenameAndExecute(command, file)
        execute a:command . " " . expand(a:file, ":p")
    endfunction
" Use local vimrc if available
    if filereadable(expand("~/.vimrc.local"))
        source ~/.vimrc.local
    endif
" Use local gvimrc if available and gui is running
    if has('gui_running')
        if filereadable(expand("~/.gvimrc.local"))
            source ~/.gvimrc.local
        endif
    endif

