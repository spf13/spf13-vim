" FuDesign2008's vimrc

" Environment {
    " Basics {
        set nocompatible        " must be first line
    " }

    " Windows Compatible {
        " On Windows, also use '.vim' instead of 'vimfiles'; this makes synchronization
        " across (heterogeneous) systems easier.
        if has('win32') || has('win64')
          set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after
        endif
    " }

    " Setup Bundle Support {
    " The next three lines ensure that the ~/.vim/bundle/ system works
        filetype on
        filetype off
        set rtp+=~/.vim/bundle/vundle
        call vundle#rc()
    " }

" }

" Bundles {
    " Use local bundles if available {
        if filereadable(expand("~/.vimrc.bundles.local"))
            source ~/.vimrc.bundles.local
        endif
    " }
    " Use fork bundles if available {
        if filereadable(expand("~/.vimrc.bundles.fork"))
            source ~/.vimrc.bundles.fork
        endif
    " }
    " Use bundles config {
        if filereadable(expand("~/.vimrc.bundles"))
            source ~/.vimrc.bundles
        endif
    " }
" }

" General {
    set background=dark         " Assume a dark background
    "if !has('gui')
        "set term=$TERM          " Make arrow and other keys work
    "endif
    filetype plugin indent on   " Automatically detect file types.
    syntax on                   " syntax highlighting
    "set mouse=a                 " automatically enable mouse usage
    "set mousehide               " hide the mouse cursor while typing
    scriptencoding utf-8

    "if has ('x') && has ('gui') " on Linux use + register for copy-paste
        "set clipboard=unnamedplus
    "elseif has ('gui') " one mac and windows, use * register for copy-paste
        "set clipboard=unnamed
    "endif


    set noautochdir
    " Set to auto read when a file is changed from the outside
    set autoread
    "set autowrite                  " automatically write a file when leaving a modified buffer
    "set shortmess+=filmnrxoOtT      " abbrev. of messages (avoids 'hit enter')
    "set viewoptions=folds,options,cursor,unix,slash " better unix / windows compatibility
    "set virtualedit=onemore         " allow for cursor beyond last character
    set history=1000                " Store a ton of history (default is 20)
    "set spell                       " spell checking on
    set nospell
    " Git commits, Subversion commits.
    autocmd FileType gitcommit,svn,markdown setlocal spell
    "autocmd FileType text,wiki,markdown,mkd setlocal spell
    "set hidden                      " allow buffer switching without saving

    " Setting up the directories {
        set backup                      " backups are nice ...
        if has('persistent_undo')
            set undofile                "so is persistent undo ...
            set undolevels=1000         "maximum number of changes that can be undone
            set undoreload=10000        "maximum number lines to save for undo on a buffer reload
        endif

    " }
" }
"
" File {
    "Setup bomb "保留bomb头a
    set nobomb "去掉bomb头
    let &termencoding=&encoding  "vim 在与屏幕/键盘交互使用的编码
    set encoding=utf-8
    if has('win32') || has('win64')
        "set encoding=utf-8 will cause displaying invalid characters
        "fix it in windows
        "@see http://www.douban.com/note/145491549/
        source $VIMRUNTIME/delmenu.vim
        source $VIMRUNTIME/menu.vim
        language message zh_CN.UTF-8
    endif
    set fileencoding=utf-8  "vim当前编辑的文件在存储时的编码
    set fileencodings=utf-8,gb2312,gbk,gb18030,big5   "vim 打开文件时的尝试使用的编码
    "set fileformat=unix,dos
    set fileformats=unix,dos
    set ambiwidth=double
"}

" Vim UI {
    "color scheme will be set by GoodColors.vim
    "colo molokai
    set tabpagemax=1               " only show 1 tabs
    set showmode                    " display the current mode

    set cursorline                  " highlight current line
    set cursorcolumn
    set colorcolumn=80,120  "显示right margin, 7.3+

    if has('cmdline_info')
        set ruler                   " show the ruler
        set rulerformat=%30(%=\:b%n%y%m%r%w\ %l,%c%V\ %P%) " a ruler on steroids
        set showcmd                 " show partial commands in status line and
                                    " selected characters/lines in visual mode
    endif

    if has('statusline')
        " Always show status line, even for one window
        set laststatus=2
        "The commandbar height
        set cmdheight=2

        " Broken down into easily includeable segments
        set statusline=%<%f\    " Filename
        set statusline+=%w%h%m%r " Options
        "set statusline+=%{fugitive#statusline()} "  Git Hotness
        set statusline+=\ [%{&ff}/%Y]            " filetype
        set statusline+=\ %{&fenc!=''?&fenc:&enc} "encoding
        "set statusline+=\ [%{getcwd()}]          " current dir
        set statusline+=%=%-14.(%l,%c%V%)\ %p%%  " Right aligned file nav info
    endif

    set backspace=indent,eol,start  " backspace for dummies
    set linespace=0                 " No extra spaces between rows
    "set nu                          " Line numbers on
    set relativenumber               " Relative number on
    "set showmatch                   " show matching brackets/parenthesis
    set noshowmatch
    set incsearch                   " find as you type search
    set hlsearch                    " highlight search terms
    set magic                       "显示括号配对情况
    "set winminheight=5              " windows can be 5 line high
    "set ignorecase                  " case insensitive search
    set noignorecase
    "set smartcase                   " case sensitive when uc present
    set nosmartcase
    set wildmenu                    " show list instead of just completing
    set wildmode=list:longest,full  " command <Tab> completion, list matches, then longest common part, then all.
    set whichwrap=b,s,h,l,<,>,[,]   " backspace and cursor keys wrap to
    set scrolljump=5                " lines to scroll when cursor leaves screen
    set scrolloff=3                 " minimum lines to keep above and below cursor
    set nofoldenable                "不启用折叠

    "making folding enabled for the file that has more than 100 lines
    function! SetFolding()
        let lines = line('$')
        " help may be set as `text`
        let types = ['text', 'help']
        if lines < 100 || index(types, tolower(&filetype)) > -1
            setlocal nofoldenable
        else
            setlocal foldenable
            setlocal foldlevelstart=1
            setlocal foldmethod=indent
            setlocal foldnestmax=5
        endif
    endfunction
    autocmd BufNewFile,BufRead * call SetFolding()

    set list
    set listchars=tab:\:\ ,trail:~,extends:>,precedes:<,nbsp:.
    set path=.,,,**                 " set path for find file


" }

" Formatting {
    "set nowrap                      " wrap long lines
    set wrap                         "折行显示
    "set autoindent                  " indent at the same level of the previous line
    set cindent
    set shiftwidth=4                " use indents of 4 spaces
    set expandtab                   " tabs are spaces, not tabs
    set tabstop=4                   " an indentation every four columns
    set softtabstop=4               " let backspace delete indent
    set matchpairs+=<:>                " match, to be used with %
    "set pastetoggle=<F12>           " pastetoggle (sane indentation on pastes)
    set comments=sl:/*,mb:*,elx:*/  " auto format comment blocks
    "autocmd BufNewFile,BufRead *.html.twig set filetype=html.twig
    autocmd BufNewFile,BufRead *.wiki set filetype=wiki
" }

" Key (re)Mappings {

    "The default leader is '\', but many people prefer ',' as it's in a standard
    "location. To override this behavior and set it back to '\' (or any other
    "character) add let g:spf13_leader='\' in your .vimrc.bundles.local file
    "if !exists('g:spf13_leader')
    let mapleader = ','
    "else
        "let mapleader=g:spf13_leader
    "endif

    "
    "vim tips
    "@see http://www.vimbits.com/bits?sort=top
    "
    " Easier moving in tabs and windows
    map <C-J> <C-W>j<C-W>_
    map <C-K> <C-W>k<C-W>_
    map <C-L> <C-W>l<C-W>_
    map <C-H> <C-W>h<C-W>_

    " Wrapped lines goes down/up to next row, rather than next line in file.
    nnoremap j gj
    nnoremap k gk

    "format line easily
    nnoremap gq gqgq

    "clear search hilight
    noremap <silent><Leader>/ :nohls<CR>

    "use sane regular expresion
    nnoremap / /\v
    vnoremap / /\v

    " The following two lines conflict with moving to top and bottom of the
    " screen
    " If you prefer that functionality, add let g:spf13_no_fastTabs = 1 in
    " your .vimrc.bundles.local file

    "if !exists('g:spf13_no_fastTabs')
        "map <S-H> gT
        "map <S-L> gt
    "endif

    " Stupid shift key fixes
    "if !exists('g:spf13_no_keyfixes')
        "if has("user_commands")
            "command! -bang -nargs=* -complete=file E e<bang> <args>
            "command! -bang -nargs=* -complete=file W w<bang> <args>
            "command! -bang -nargs=* -complete=file Wq wq<bang> <args>
            "command! -bang -nargs=* -complete=file WQ wq<bang> <args>
            "command! -bang Wa wa<bang>
            "command! -bang WA wa<bang>
            "command! -bang Q q<bang>
            "command! -bang QA qa<bang>
            "command! -bang Qa qa<bang>
        "endif

        "cmap Tabe tabe
    "endif

    " Yank from the cursor to the end of the line, to be consistent with C and D.
    nnoremap Y y$




    " Adjust viewports to the same size
    map <Leader>= <C-w>=

    " map <Leader>ff to display all lines with keyword under cursor
    " and ask which one to jump to
    nmap <Leader>ff [I:let nr = input("Which one: ")<Bar>exe "normal " . nr ."[\t"<CR>

    " Easier horizontal scrolling
    map zl zL
    map zh zH
" }

" Plugins {
    " ack.vim {
        "set grepprg=ack
    " }

    " superTab.vim {
        let g:SuperTabDefaultCompletionType = "context"
        let g:SuperTabLongestEnhanced = 1
    " }

    " AutoClose.vim {
        "let g:AutoClosePairs = {'(': ')', '{': '}', '[': ']', '"': '"', "'": "'", '`': '`'}
    " }

    " FuDesign2008/MPlan.vim {
        let cur_year = strftime('%Y')
        "01-12
        let cur_month = strftime('%m')
        let cur_month = cur_year . '-' . cur_month
        "let g:plan_file = '/Users/new/fuyg/Dropbox/月度规划/2013/2013-04/2013-04.md'
        let g:plan_file = '/Users/new/fuyg/Dropbox/月度规划/' . cur_year .'/' . cur_month . '/' . cur_month. '.md'
    "}

    " FuDesign2008/vimKit {
        "  include syntax  jquery underscore
        au BufRead,BufNewFile *.js set ft=javascript syntax=jslib
    "}
    " FuDesign2008/webSearch.vim {
        let g:webSearchEngines = {
            \ 'google': 'https://www.google.com.hk/search?hl=en&q=<QUERY>',
            \ 'iyoudao': 'http://home.iyoudao.net/search.php#q=<QUERY>',
            \ 'github': 'https://github.com/search?q=<QUERY>',
            \ 'mozilla': 'https://developer.mozilla.org/en-US/search?q=<QUERY>',
            \ 'dottoro': 'http://www.dottoro.com/search.php?query=<QUERY>',
            \ 'overapi' : 'http://overapi.com/<QUERY>/',
            \ 'jquery': 'http://api.jquery.com/?s=<QUERY>',
            \ 'underscore': 'http://underscorejs.org/#<QUERY>',
            \ 'backbone': 'http://backbonejs.org/#Model-<QUERY>',
            \ 'dict': 'http://dict.youdao.com/search?q=<QUERY>',
            \ 'wiki': 'http://en.wikipedia.org/w/index.php?search=<QUERY>',
            \ 'bmap': 'http://map.baidu.com/?newmap=1&ie=utf-8&s=s%26wd%3D<QUERY>',
            \ 'bimg': 'http://image.baidu.com/i?word=<QUERY>&ie=utf-8',
            \ 'ganji': 'http://bj.ganji.com/site/s/_<QUERY>',
            \ '58': 'http://bj.58.com/sou/jh_<QUERY>/'
            \}
    " }


    " FuDesign2008/translator.vim {
    " }

    " FuDesign2008/GoodColors.vim {
        let g:random_color_schemes = ['pyte',
                        \ 'summerfruit',
                        \ 'codeschool',
                        \ 'jellybeans',
                        \ 'railscasts',
                        \ 'twilight',
                        \ 'ir_black',
                        \ 'molokai',
                        \ 'zenburn',
                        \ 'desert',
                        \ 'wombat',
                        \ 'lucius',
                        \ 'peaksea']
    " }


    " FuDesign2008/WriteJSDocComment {
        "au FileType javascript nnoremap <leader>cc :call WriteJSDocComment()<CR>
    "}
    " coffee plugin {
        " http://www.vim.org/scripts/script.php?script_id=3590
        autocmd BufRead,BufNewFile *.coffee set filetype=coffee
    "}
    "less.vim {
        autocmd BufRead,BufNewFile *.less set filetype=less
    "}

    "calendar {
        let g:calendar_mark = 'right'
    "}

    " jsLint {
        let $JS_CMD='node'
        "toggle and update
        autocmd FileType javascript nnoremap <leader>j :JSLintToggle<CR>:JSLintUpdate<CR>
    "}
    "
    "zoom.vim {
        nnoremap = :ZoomIn<CR>
        nnoremap - :ZoomOut<CR>
        nnoremap 0 :ZoomReset<CR>
    "}

    " PIV {
        "let g:DisableAutoPHPFolding = 0
        "let g:PIVAutoClose = 0
    " }

    " Misc  {
        "let g:NERDShutUp=1
        "let b:match_ignorecase = 1
        if has('mac')
            let g:markdown_preview_app = '/Applications/Mou.app'
        elseif has('win32') || has('win64')
            let g:markdown_preview_app = 'D:/Program Files/MarkdownPad 2/MarkdownPad2.exe'
        endif
        autocmd FileType markdown,mkd nnoremap <buffer> <silent> <leader>p :PreviewMarkdown<CR>
    " }



    " tagbar & ctags {
        "set tags=./tags;/,~/.vimtags
        let g:tagbar_ctags_bin = "/usr/local/Cellar/ctags/5.8/bin/ctags"
        let g:tagbar_sort = 0
        let g:tagbar_compact = 1
        let g:tagbar_show_visibility = 0
        let g:tagbar_expand = 1

        nnoremap <silent> <leader>tb :TagbarToggle<CR>
    " }

    " AutoCloseTag {
        " Make it so AutoCloseTag works for xml and xhtml files as well
        au FileType xhtml,xml ru ftplugin/html/autoclosetag.vim
        nnoremap <Leader>ac <Plug>ToggleAutoCloseMappings
    " }

    " SnipMate {
        " Setting the author var
        " If forking, please overwrite in your .vimrc.local file
        "let g:snips_author = 'Steve Francia <steve.francia@gmail.com>'
    " }

    " NerdTree {
        nnoremap <leader>tt :NERDTreeToggle <CR>
        let NERDTreeWinPos='right' "NerdTree窗口显示在右边
        "map <C-e> :NERDTreeToggle<CR>:NERDTreeMirror<CR>
        "map <leader>e :NERDTreeFind<CR>
        "nmap <leader>nt :NERDTreeFind<CR>

        "let NERDTreeShowBookmarks=1
        let NERDTreeIgnore=['\.pyc', '\~$', '\.swo$', '\.swp$', '\.git', '\.hg', '\.svn', '\.bzr']
        "let NERDTreeChDirMode=0
        "let NERDTreeQuitOnOpen=1
        "let NERDTreeMouseMode=2
        "let NERDTreeShowHidden=1
        "let NERDTreeKeepTreeInNewTab=1
        "let g:nerdtree_tabs_open_on_gui_startup=0
    " }

    " Tabularize {
        nmap <Leader>a= :Tabularize /=<CR>
        vmap <Leader>a= :Tabularize /=<CR>
        nmap <Leader>a: :Tabularize /:<CR>
        vmap <Leader>a: :Tabularize /:<CR>
        nmap <Leader>a:: :Tabularize /:\zs<CR>
        vmap <Leader>a:: :Tabularize /:\zs<CR>
        nmap <Leader>a, :Tabularize /,<CR>
        vmap <Leader>a, :Tabularize /,<CR>
        nmap <Leader>a<Bar> :Tabularize /<Bar><CR>
        vmap <Leader>a<Bar> :Tabularize /<Bar><CR>
     " }

     " Session List {
        "set sessionoptions=blank,buffers,curdir,folds,tabpages,winsize
        "nmap <leader>sl :SessionList<CR>
        "nmap <leader>ss :SessionSave<CR>
     " }

     " Buffer explorer {
        "nmap <leader>b :BufExplorer<CR>
     " }

     " JSON {
        au BufNewFile,BufRead .jshintrc set filetype=json
        "nmap <leader>jt <Esc>:%!python -m json.tool<CR><Esc>:set filetype=json<CR>
     " }

     " PyMode {
        "let g:pymode_lint_checker = "pyflakes"
        "let g:pymode_utils_whitespaces = 0
     " }

     " ctrlp {
        set wildignore+=*/.git/*,*/.hg/*,*/.svn/*
        let g:ctrlp_root_markers = ['src']
        let g:ctrlp_working_path_mode = 2
        "nnoremap <silent> <D-t> :CtrlP<CR>
        "nnoremap <silent> <D-r> :CtrlPMRU<CR>
        let g:ctrlp_custom_ignore = {
            \ 'dir':  '\.git$\|\.hg$\|\.svn$',
            \ 'file': '\.exe$\|\.so$\|\.dll$' }

        let g:ctrlp_user_command = {
            \ 'types': {
                \ 1: ['.git', 'cd %s && git ls-files'],
                \ 2: ['.hg', 'hg --cwd %s locate -I .'],
            \ },
            \ 'fallback': 'find %s -type f'
        \ }
     "}

     " TagBar {
        nnoremap <silent> <leader>b :TagbarToggle<CR>
     "}

     " PythonMode {
     " Disable if python support not present
        "if !has('python')
           "let g:pymode = 1
        "endif
     " }
     "ios devement {
        let g:clang_library_path = '/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang'

     "}




     " indent_guides {
        "if !exists('g:spf13_no_indent_guides_autocolor')
            "let g:indent_guides_auto_colors = 1
        "else
            " for some colorscheme ,autocolor will not work,like 'desert','ir_black'.
            "autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=#212121   ctermbg=3
            "autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=#404040 ctermbg=4
        "endif
        "set ts=4 sw=4 et
        "let g:indent_guides_start_level = 2
        "let g:indent_guides_guide_size = 1
        "let g:indent_guides_enable_on_vim_startup = 1
     " }

" }

" GUI Settings {
    " GVIM- (here instead of .gvimrc)
    if has('gui_running')
        set guioptions-=T           " remove the toolbar
        set lines=40                " 40 lines of text instead of 24,
        "字体是否好看与字号有很大关系
        "13-14好看
        set guifont=Consolas:h14  "font
        "gVim on windows
        "set guifont=Droid\ Sans\ Mono:h16  "font
        "
        if has('gui_macvim')
            "macVim
            set guifont=Monaco:h16
            "set transparency=5          " Make the window slightly transparent
        endif
    else
        if &term == 'xterm' || &term == 'screen'
            set t_Co=256                 " Enable 256 colors to stop the CSApprox warning and make xterm vim shine
        endif
        "set term=builtin_ansi       " Make arrow and other keys work
    endif
" }

 " Functions {

function! UnBundle(arg, ...)
  let bundle = vundle#config#init_bundle(a:arg, a:000)
  call filter(g:bundles, 'v:val["name_spec"] != "' . a:arg . '"')
endfunction

com! -nargs=+         UnBundle
\ call UnBundle(<args>)

function! InitializeDirectories()
    let separator = "."
    let parent = $HOME
    let prefix = '.vim'
    let dir_list = {
                \ 'backup': 'backupdir',
                \ 'views': 'viewdir',
                \ 'swap': 'directory' }

    if has('persistent_undo')
        let dir_list['undo'] = 'undodir'
    endif

    for [dirname, settingname] in items(dir_list)
        let directory = parent . '/' . prefix . dirname . "/"
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

"function! NERDTreeInitAsNeeded()
    "redir => bufoutput
    "buffers!
    "redir END
    "let idx = stridx(bufoutput, "NERD_tree")
    "if idx > -1
        "NERDTreeMirror
        "NERDTreeFind
        "wincmd l
    "endif
"endfunction


" }

" Use fork vimrc if available {
    if filereadable(expand("~/.vimrc.fork"))
        source ~/.vimrc.fork
    endif
" }

" Use local vimrc if available {
    if filereadable(expand("~/.vimrc.local"))
        source ~/.vimrc.local
    endif
" }

" Use local gvimrc if available and gui is running {
    if has('gui_running')
        if filereadable(expand("~/.gvimrc.local"))
            source ~/.gvimrc.local
        endif
    endif
" }
