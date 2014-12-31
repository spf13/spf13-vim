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
        else
            " use bash as the default shell for vim
            " @see http://dailyvim.tumblr.com/post/66708941289/fish
            set shell=/bin/bash
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
    autocmd FileType gitcommit,svn setlocal spell
    autocmd BufNewFile,BufReadPost *.md set filetype=markdown
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
    set number                      " Line numbers on
    set relativenumber               " Relative number on
    "set nonumber
    "set norelativenumber
    "set showmatch                   " show matching brackets/parenthesis
    set noshowmatch
    set incsearch                   " find as you type search
    set hlsearch                    " highlight search terms
    set magic                       "显示括号配对情况
    "set winminheight=5              " windows can be 5 line high
    set ignorecase                  " case insensitive search
    "set noignorecase
    set smartcase                   " case sensitive when uc present
    "set nosmartcase
    set fileignorecase              " ignore case for file
    "set nofileignorecase           " do not ignore case for file
    set wildmenu                    " show list instead of just completing
    set wildmode=list:longest,full  " command <Tab> completion, list matches, then longest common part, then all.
    set whichwrap=b,s,h,l,<,>,[,]   " backspace and cursor keys wrap to
    set scrolljump=5                " lines to scroll when cursor leaves screen
    set scrolloff=3                 " minimum lines to keep above and below cursor
    set lazyredraw                  " for performance

    "making folding enabled for the file that has more than 100 lines
    function! SetFolding()
        let lines = line('$')
        "types that disable folding
        "help may be set as `text`
        let folding_disabled_types = ['text', 'help', 'javascript', 'objc', 'markdown', 'vim']
        let folding_start = 300
        if lines < folding_start || index(folding_disabled_types, tolower(&filetype)) > -1
            setlocal nofoldenable
        else
            setlocal foldenable
            setlocal foldlevelstart=1
            setlocal foldmethod=indent
            setlocal foldnestmax=5
        endif
    endfunction
    "set nofoldenable                "不启用折叠
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
    set formatoptions+=mB              "break at a multi-byte character above 255, see https://groups.google.com/forum/#!topic/vim_use/HYy7sqje3bQ
    "set pastetoggle=<F12>           " pastetoggle (sane indentation on pastes)
    set comments=sl:/*,mb:*,elx:*/  " auto format comment blocks
    autocmd BufNewFile,BufRead,BufWritePre *.wiki setf wiki
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
    "
    " <C-J> <C-K> are keep for UltiSnips
    "map <C-J> <C-W>j
    "map <C-K> <C-W>k
    map <C-L> <C-W>l
    map <C-H> <C-W>h

    " Wrapped lines goes down/up to next row, rather than next line in file.
    nnoremap j gj
    nnoremap k gk

    "format line easily
    nnoremap gq gqgq

    "clear search hilight
    "noremap <silent><Leader>/ :nohls<CR>

    " ctrl + [ remember-able back for tags
    nnoremap <C-\[> <C-T>

    "use sane regular expresion
    "nnoremap / /\v
    "vnoremap / /\v

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
    " ag.vim {
        " @see http://robots.thoughtbot.com/faster-grepping-in-vim
        "
        " The Silver Searcher
        if executable('ag')
          " Use ag over grep
          set grepprg=ag\ --nogroup\ --nocolor
        endif

        " bind K to grep word under cursor
        nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>

        " bind \ (backward slash) to grep shortcut
        "command -nargs=+ -complete=file -bar Ag silent! grep! <args>|cwindow|redraw!
        nnoremap \ :Ag<SPACE>
    " }

    " autoHighlight.vim {
        let g:auto_highlight_enable = 0
    " }

    " superTab.vim {
        "let g:SuperTabDefaultCompletionType = "context"
        "let g:SuperTabLongestEnhanced = 1
     "}

    " autoHEADER {
        let g:autoHEADER_default_author = 'fuyg<fuyg@rd.netease.com>'
    " }

     " YCM.vim {
        " the default .ycm_extra_conf.py
        let g:ycm_global_ycm_extra_conf = expand('~/.vim/bundle/YouCompleteMe/cpp/ycm/.ycm_extra_conf.py')
        let g:ycm_confirm_extra_conf = 0
        let g:ycm_filetype_blacklist = {}
        let g:ycm_complete_in_comments = 1
        let g:ycm_collect_identifiers_from_comments_and_strings = 1
        let g:ycm_min_num_of_chars_for_completion = 1
        let g:ycm_min_num_identifier_candidate_chars = 3
        let g:ycm_always_populate_location_list = 0
     "}

     "ultisnips {
        " ultisnips conflicts with ycm, so change the key
        let g:UltiSnipsListSnippets="<s-cr>"
        let g:UltiSnipsExpandTrigger="<c-cr>"
        let g:UltiSnipsJumpForwardTrigger="<c-j>"
        let g:UltiSnipsJumpBackwardTrigger="<c-k>"
     "}


     " js-complete.vim {
         "autocmd FileType javascript  :setl omnifunc=jscomplete#CompleteJS
     "}

    " AutoClose.vim {
        "let g:AutoClosePairs = {'(': ')', '{': '}', '[': ']', '"': '"', "'": "'", '`': '`'}
    " }

    " FuDesign2008/MPlan.vim {
    "
        let cur_year = strftime('%Y')
        "01-12
        let cur_month = strftime('%m')
        let cur_month = cur_year . '-' . cur_month

        "
        "  ~/Dropbox/plan/2013/2013-04/2013-04.*
        "  the plan file may has different file extension
        "
        let plan_file_pattern = '~/Dropbox/plan/' . cur_year .'/' . cur_month . '/' . cur_month . '.*'
        let fileList = glob(plan_file_pattern, 0, 1)
        let plan_file_path = get(fileList, 0, '')

        if strlen(plan_file_path) > 0
            let g:plan_file = plan_file_path
        endif

        " regular task
        let g:plan_week_work = {
            \ 1 : '1. 10:00 - 11:00 @8层珍珠 YNote Editor Weekly meeting;',
            \ 2 : '1. weekly report;',
            \ 5 : '1. 16:00 - 18:00 @2层朱砂 编辑器技术分享与交流;',
            \ 6 : '1. 本周整理/整顿;1. 下周计划;'
            \}
        "0 = sunday
        "1 = monday
        "...
        "6 = sat
        let g:plan_week_personal = {
            \ 0 : '1. 看望/call 父母;1. 锻炼身体;',
            \ 3 : '1. call 父母;',
            \ 4 : '1. 20:30 ~ 火影;'
            \}
        let g:plan_month_work = {
            \ 2 : '1. 确认上月考勤;',
            \ 15: '1. 查看有道云笔记的新闻, 浏览论坛;',
            \ 27: '1. 本月整理/整顿;1. 下月计划;'
            \}
        let g:plan_month_personal = {
            \ 5 : '1. 查询薪水发放;',
            \ 7 : '1. 交房租;1. 还房贷;',
            \ 28 : '1. 收房租;'
            \}

    "}

    " FuDesign2008/backbone.vim {
        "  include syntax  jquery underscore and backbone
        autocmd BufRead,BufNewFile *.js set ft=javascript syntax=backbone
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
    "
    "easymotion.vim{
        "let g:EasyMotion_leader_key = '<Leader>'
    "}

    " FuDesign2008/translator.vim {
    " }

    " FuDesign2008/GoodColors.vim {
        "let g:random_color_schemes = ['pyte',
                        "\ 'summerfruit',
                        "\ 'codeschool',
                        "\ 'jellybeans',
                        "\ 'railscasts',
                        "\ 'twilight',
                        "\ 'ir_black',
                        "\ 'molokai',
                        "\ 'zenburn',
                        "\ 'desert',
                        "\ 'wombat',
                        "\ 'lucius',
                        "\ 'peaksea']
        let g:random_color_schemes = ['pyte',
                        \ 'summerfruit',
                        \ 'codeschool',
                        \ 'jellybeans',
                        \ 'molokai',
                        \ 'lucius']
    " }


    " FuDesign2008/WriteJSDocComment {
        "autocmd FileType javascript nnoremap <leader>cc :call WriteJSDocComment()<CR>
    "}
    " coffee plugin {
        " http://www.vim.org/scripts/script.php?script_id=3590
        autocmd BufRead,BufNewFile,BufWritePre *.coffee setf coffee
    "}
    "less.vim {
        autocmd BufRead,BufNewFile,BufWritePre *.less setf less
    "}

    "calendar {
        let g:calendar_mark = 'right'
    "}

    "zoom.vim {
        nnoremap = :ZoomIn<CR>
        nnoremap - :ZoomOut<CR>
        nnoremap 0 :ZoomReset<CR>
    "}

    "ZoomWin {
        nnoremap <C-W>z <Plug>ZoomWin
    "}
    "
    "Syntastic {
        let g:syntastic_objc_compiler = 'clang'
    "}

    " PIV {
        "let g:DisableAutoPHPFolding = 0
        "let g:PIVAutoClose = 0
    " }

    " Misc  {
        "let g:NERDShutUp=1
        "let b:match_ignorecase = 1
        "MarkdownViewer.vim
        let g:mdv_html = 0
    " }



    " tagbar & ctags {
        "set tags=./tags;/,~/.vimtags
        "let g:tagbar_ctags_bin = "/usr/local/Cellar/ctags/5.8/bin/ctags"
        "let g:tagbar_type_javascript = {
            "\ 'ctagsbin' : '~/workspace/github/tools/doctorjs/bin'
        "\ }
        let g:tagbar_sort = 0
        let g:tagbar_compact = 1
        let g:tagbar_show_visibility = 0
        let g:tagbar_expand = 1
        "for markdown
        let g:tagbar_type_markdown = {
            \ 'ctagstype' : 'markdown',
            \ 'kinds' : [
                \ 'h:Heading_L1',
                \ 'i:Heading_L2',
                \ 'k:Heading_L3'
            \ ]
        \ }

        " add a definition for Objective-C to tagbar
        let g:tagbar_type_objc = {
            \ 'ctagstype' : 'ObjectiveC',
            \ 'kinds'     : [
                \ 'i:interface',
                \ 'I:implementation',
                \ 'p:Protocol',
                \ 'm:Object_method',
                \ 'c:Class_method',
                \ 'v:Global_variable',
                \ 'F:Object field',
                \ 'f:function',
                \ 'p:property',
                \ 't:type_alias',
                \ 's:type_structure',
                \ 'e:enumeration',
                \ 'M:preprocessor_macro',
            \ ],
            \ 'sro'        : ' ',
            \ 'kind2scope' : {
                \ 'i' : 'interface',
                \ 'I' : 'implementation',
                \ 'p' : 'Protocol',
                \ 's' : 'type_structure',
                \ 'e' : 'enumeration'
            \ },
            \ 'scope2kind' : {
                \ 'interface'      : 'i',
                \ 'implementation' : 'I',
                \ 'Protocol'       : 'p',
                \ 'type_structure' : 's',
                \ 'enumeration'    : 'e'
            \ }
        \ }

        nnoremap <silent> <leader>tb :TagbarToggle<CR>
    " }

    " AutoCloseTag {
        " Make it so AutoCloseTag works for xml and xhtml files as well
        autocmd FileType xhtml,xml ru ftplugin/html/autoclosetag.vim
        nnoremap <Leader>ac <Plug>ToggleAutoCloseMappings
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

     "Matchmaker{
         let g:matchmaker_enable_startup = 1
     "}

     " Session List {
        "set sessionoptions=blank,buffers,curdir,folds,tabpages,winsize
        "nmap <leader>sl :SessionList<CR>
        "nmap <leader>ss :SessionSave<CR>
     " }

     " Buffer explorer {
        "nmap <leader>b :BufExplorer<CR>
     " }

     " JSON {
        autocmd BufNewFile,BufRead,BufWritePre .jshintrc setf json
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

        "let g:ctrlp_user_command = {
            "\ 'types': {
                "\ 1: ['.git', 'cd %s && git ls-files'],
                "\ 2: ['.hg', 'hg --cwd %s locate -I .'],
            "\ },
            "\ 'fallback': 'find %s -type f'
        "\ }

        " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
        if exists('g:ctrlp_user_command')
            unlet g:ctrlp_user_command
        endif
        let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

        " ag is fast enough that CtrlP doesn't need to cache
        let g:ctrlp_use_caching = 0

        " for ctrl+p conflicts with
        " https://github.com/terryma/vim-multiple-cursors
        " use :CtrlP command or ctrl+shift+p
        let g:ctrlp_map = '<c-s-p>'
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
        try
            set guifont=Consolas:h12  "font
            "set guifont=Droid\ Sans\ Mono:h16  "font
        catch
            "do nothing
        endtry
    else
        if &term == 'xterm' || &term == 'screen'
            set t_Co=256                 " Enable 256 colors to stop the CSApprox warning and make xterm vim shine
        endif
        "set term=builtin_ansi       " Make arrow and other keys work
    endif
    "macVim
    if has('gui_macvim')
        "enter full mode when start
        set fu
        "set guifont=Monaco:h12
        set guifont=Menlo:h13
        "set transparency=5          " Make the window slightly transparent
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
