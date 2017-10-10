set nocompatible        " must be first line
" FuDesign2008's vimrc

" Environment {
    " Windows Compatible {
        " On Windows, also use '.vim' instead of 'vimfiles'; this makes synchronization
        " across (heterogeneous) systems easier.
        let is_win = has('win32') || has('win64')
        if is_win
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
        if filereadable(expand('~/.vimrc.bundles.local'))
            source ~/.vimrc.bundles.local
        endif
    " }
    " Use fork bundles if available {
        if filereadable(expand('~/.vimrc.bundles.fork'))
            source ~/.vimrc.bundles.fork
        endif
    " }
    " Use bundles config {
        if filereadable(expand('~/.vimrc.bundles'))
            source ~/.vimrc.bundles
        endif
    " }
" }

augroup vimrc
    autocmd!
augroup END

" General {
    set background=dark         " Assume a dark background
    "if !has('gui')
        "set term=$TERM          " Make arrow and other keys work
    "endif
    filetype plugin indent on   " Automatically detect file types.
    syntax on                   " syntax highlighting
    set synmaxcol=256
    "set mouse=a                 " automatically enable mouse usage
    "set mousehide               " hide the mouse cursor while typing
    set encoding=utf-8
    scriptencoding utf-8

    "if has ('x') && has ('gui') " on Linux use + register for copy-paste
        "set clipboard=unnamedplus
    "elseif has ('gui') " one mac and windows, use * register for copy-paste
        "set clipboard=unnamed
    "endif


    set noautochdir
    " Set to auto read when a file is changed from the outside
    set autoread
    set autowrite                  " automatically write a file when leaving a modified buffer
    "set shortmess+=filmnrxoOtT      " abbrev. of messages (avoids 'hit enter')
    "set viewoptions=folds,options,cursor,unix,slash " better unix / windows compatibility
    "set virtualedit=onemore         " allow for cursor beyond last character
    set history=1000                " Store a ton of history (default is 20)
    "set spell                       " spell checking on
    set nospell
    " Git commits, Subversion commits.
    autocmd vimrc FileType gitcommit,svn setlocal spell
    autocmd vimrc BufNewFile,BufReadPost *.md,*.mkd set filetype=markdown
    autocmd vimrc BufNewFile,BufReadPost *.mmd,*.mermaid set filetype=mermaid
    autocmd vimrc BufNewFile,BufReadPost *.conf     set filetype=conf
    "autocmd vimrc FileType text,wiki,markdown,mkd setlocal spell
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
    set tabpagemax=1               " only show 1 tabs
    set showmode                    " display the current mode

    set cursorline                  " highlight current line
    set cursorcolumn
    set colorcolumn=80,100,120  "显示right margin, 7.3+

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
        set cmdheight=1

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

    "making folding enabled for the file that has more than 1000 lines
    function! SetFolding()
        let lines = line('$')
        "types that disable folding
        "help may be set as `text`
        let folding_disabled_types = ['text', 'help', 'javascript', 'objc', 'markdown', 'vim']
        let folding_start = 1000
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
    autocmd vimrc BufNewFile,BufRead * call SetFolding()

    set list
    set listchars=tab:\:\ ,trail:~,extends:>,precedes:<,nbsp:.
    set path=.,,,**                 " set path for find file


" }

" Formatting {
    "set nowrap                      " wrap long lines
    set wrap                         "折行显示

    " @see http://stackoverflow.com/questions/16840433/forcing-vimdiff-to-wrap-lines
    autocmd vimrc FilterWritePre * if &diff | setlocal wrap< | endif

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
    autocmd vimrc BufNewFile,BufRead,BufWritePre *.wiki setf wiki
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
    nnoremap <silent> \ :nohls<CR>

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
    " ack.vim/ag.vim {
        " @see http://robots.thoughtbot.com/faster-grepping-in-vim
        "set grepprg=ack
        "
        " The Silver Searcher
        if executable('ag')
            " Use ag over grep
            set grepprg=ag\ --nogroup\ --nocolor
            let g:ackprg = 'ag --vimgrep --smart-case'
        endif

    " }

    " autoHighlight.vim {
        let g:auto_highlight_enable = 0
    " }

    " superTab.vim {
        "let g:SuperTabDefaultCompletionType = "context"
        "let g:SuperTabLongestEnhanced = 1
     "}

     " YCM.vim {
        " the default .ycm_extra_conf.py
        "let g:ycm_global_ycm_extra_conf = expand('~/.vim/bundle/YouCompleteMe/cpp/ycm/.ycm_extra_conf.py')
        let g:ycm_confirm_extra_conf = 0
        let g:ycm_complete_in_comments = 1
        let g:ycm_collect_identifiers_from_comments_and_strings = 1
        let g:ycm_min_num_of_chars_for_completion = 1
        let g:ycm_min_num_identifier_candidate_chars = 0
        let g:ycm_always_populate_location_list = 0

        " default
        "let g:ycm_key_list_select_completion = ['<TAB>', '<DOWN>'] " with vim default: <c-n>
        "let g:ycm_key_list_previous_completion = ['<S-TAB>', '<UP>'] " with vim default: <c-p>
        let g:ycm_key_invoke_completion = '<C-S-N>'


        "the default setting
              "\ 'markdown' : 1,
        let g:ycm_filetype_blacklist = {
              \ 'tagbar' : 1,
              \ 'qf' : 1,
              \ 'notes' : 1,
              \ 'unite' : 1,
              \ 'text' : 1,
              \ 'vimwiki' : 1,
              \ 'pandoc' : 1,
              \ 'infolog' : 1,
              \ 'mail' : 1
              \}

        " support for typescript
        if !exists('g:ycm_semantic_triggers')
            let g:ycm_semantic_triggers = {}
        endif
        let g:ycm_semantic_triggers['typescript'] = ['.']
     "}

     "ultisnips {
        let g:UltiSnipsExpandTrigger='<C-CR>'
        " default
        "let g:UltiSnipsListSnippets='<c-tab>'
        "let g:UltiSnipsJumpForwardTrigger='<c-j>'
        "let g:UltiSnipsJumpBackwardTrigger='<c-k>'
     "}
     "
     " vim-javacomplete2 {
        " autocmd vimrc FileType java set omnifunc=javacomplete#Complete
     " }


     " js-complete.vim {
         "autocmd vimrc FileType javascript  :setl omnifunc=jscomplete#CompleteJS
     "}

    " AutoClose.vim {
        "let g:AutoClosePairs = {'(': ')', '{': '}', '[': ']', '"': '"', "'": "'", '`': '`'}
    " }

    " FuDesign2008/plan.vim {
    "
        if !is_win

            let g:p_edit_files = {
                \ 'book': expand('~/百度云同步盘/books'),
                \ 'todo': expand('~/Dropbox/plan/fuyg-todo'),
                \ 'dev' : expand('~/Dropbox/plan/family-dev'),
                \ 'pdp' : expand('~/Dropbox/plan/pdp'),
                \ 'pwd' : expand('~/Dropbox/common/pwd.markdown'),
                \ 'youdao': expand('~/Dropbox/common/youdao.mkd')
                \}

            let cur_year = strftime('%Y')
            "01-12
            let cur_month = strftime('%m')
            let cur_month = cur_year . '-' . cur_month

            let plan_file_pattern = '~/Dropbox/plan/' . cur_year .'/' . cur_month . '/plan.*'
            "  ~/Dropbox/plan/2013/2013-04/2013-04.*
            "  the plan file may has different file extension
            let plan_file_pattern_old = '~/Dropbox/plan/' . cur_year .'/' . cur_month . '/' . cur_month . '.*'
            let diary_file_pattern = '~/Dropbox/plan/' . cur_year .'/' . cur_month . '/diary.*'
            unlet cur_year
            unlet cur_month

            "
            let fileList = glob(plan_file_pattern, 0, 1)
            let plan_file_path = get(fileList, 0, '')
            if strlen(plan_file_path) > 0
                let g:p_edit_files['plan'] = plan_file_path
            else
                let fileList = glob(plan_file_pattern_old, 0, 1)
                let plan_file_path = get(fileList, 0, '')
                if strlen(plan_file_path) > 0
                    let g:p_edit_files['plan'] = plan_file_path
                endif
            endif

            unlet plan_file_pattern
            unlet plan_file_pattern_old
            unlet plan_file_path

            let fileList = glob(diary_file_pattern, 0, 1)
            let diary_file_path = get(fileList, 0, '')
            if strlen(diary_file_path) > 0
                let g:p_edit_files['diary'] = diary_file_path
            endif

            unlet diary_file_pattern
            unlet diary_file_path
            unlet fileList

            " regular task

            let g:plan_week_keypoint = [
                \ '1. 技术博客;',
                \ '    - 每周一篇;',
                \ '1. 知乎;',
                \ '    - 每周一答;',
                \ '    - 每天一赞一评;',
                \ '1. 微信朋友圈;',
                \ '    - 每天一发;',
                \ '    - 每天一赞一评;'
                \]

            "0 = sunday
            "1 = monday
            "...
            "6 = sat
            let g:plan_week_work = {
                \ 1 : '1. 11:00 Sprint例会;',
                \ 2 : '1. weekly report;',
                \ 3 : '1. 11:00 Sprint例会;',
                \ 5 : '1. Sprint计划;'
                \}
            let g:plan_week_personal = {
                \ 0 : '1. 看望/call 父母;1. 锻炼身体;',
                \ 3 : '1. call 父母;'
                \}

            let g:plan_week_review = [
                \ '1. (Invest & Finance);',
                \ '1. (Tech & Managment);',
                \ '1. (Enjoy Life);'
                \]

            let g:plan_month_keypoint = [
                \ '1. (Invest & Finance):;',
                \ '1. (Enjoy Life):;',
                \ '1. (Tech & Managment):;',
                \ ';',
                \ '|  客户端 | 收入 | 日／周／月活 | 产品进展 | 产品上线 |;',
                \ '|:-------:|:----:|:------------:|:--------:|:--------:|;',
                \ '| PC      |      |              |          |          |;',
                \ '| Mac     |      |              |          |          |;',
                \ '| Android |      |              |          |          |;',
                \ '| iPhone  |      |              |          |          |;',
                \ '| iPad    |      |              |          |          |;',
                \ '| 总计    |      |              |          |          |;',
                \ '|:-------:|:----:|:------------:|:--------:|:--------:|;',
                \ ';'
                \]

            let g:plan_month_work = {
                \ 2 : '1. 确认上月考勤;',
                \ 15: '1. 查看有道云笔记的新闻, 浏览论坛;',
                \ 27: '1. 月回顾与计划;'
                \}
            let g:plan_month_personal = {
                \ 1 : '1. 还农行房贷(6);',
                \ 5 : '1. 查询薪水发放;',
                \ 10 : '1. 还工行房贷(17);'
                \}

            let g:plan_month_review = g:plan_week_review

            let g:plan_year_personal = {
                \ '01-01': '1. 修改密-码: corp 邮箱, rd邮箱, wifi网络;',
                \ '05-01': '1. 修改密-码: corp 邮箱, rd邮箱, wifi网络;',
                \ '06-24': '1. 半年回顾与规划;',
                \ '09-01': '1. 修改密-码: corp 邮箱, rd邮箱, wifi网络;',
                \ '12-24': '1. 半年回顾与规划;',
                \ '12-31': '1. 结婚纪念日;'
                \}
        endif

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


    " FuDesign2008/randomColor.vim {
        "let g:favorite_color_schemes = ['pyte',
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
        let g:favorite_color_schemes = [
                        \ 'Tomorrow',
                        \ 'Tomorrow-Night',
                        \ 'Tomorrow-Night-Blue',
                        \ 'Tomorrow-Night-Bright',
                        \ 'Tomorrow-Night-Eighties',
                        \ 'apprentice',
                        \ 'atom',
                        \ 'autumnleaf',
                        \ 'bclear',
                        \ 'beauty256',
                        \ 'codeschool',
                        \ 'github',
                        \ 'hybrid',
                        \ 'jellybeans',
                        \ 'lucius',
                        \ 'molokai',
                        \ 'pencil',
                        \ 'pyte',
                        \ 'solarized',
                        \ 'summerfruit'
                        \]

        let g:random_color_start = 2
        if !has('gui_running')
            colo lucius
        endif
    " }

    " FuDesign2008/openUrl.vim {

        " the url prefix for jira issue item
        let g:open_jira_prefix='http://jira.corp.youdao.com/browse/'

    " }



    " pangloss/vim-javascript {
        let g:javascript_enable_domhtmlcss = 1

        let g:javascript_conceal_function   = 'ƒ'
        let g:javascript_conceal_null       = 'ø'
        let g:javascript_conceal_this       = '@'
        let g:javascript_conceal_return     = '⇚'
        let g:javascript_conceal_undefined  = '¿'
        let g:javascript_conceal_NaN        = 'ℕ'
        let g:javascript_conceal_prototype  = '¶'
        let g:javascript_conceal_static     = '•'
        let g:javascript_conceal_super      = 'Ω'
    " }

    " tern_for_vim {
        let g:tern_show_argument_hints = 'on_hold'
        let g:tern_map_keys = 1

    " }

    " vim-jsdoc {
        let g:jsdoc_allow_input_prompt = 1
        let g:jsdoc_input_description = 1
        let g:jsdoc_access_descriptions = 1
        let g:jsdoc_underscore_private = 1
        let g:jsdoc_enable_es6 = 1
    " }

    " NERDCommenter {
        let g:NERDCustomDelimiters = {
            \ 'python' : { 'left': '#'}
            \}
        let g:NERDSpaceDelims = 1

    " }

    " Raimondi/delimitMate {
        let g:delimitMate_expand_cr = 2
        let g:delimitMate_expand_space = 1
        let g:delimitMate_balance_matchpairs = 1
    "}

    " coffeescript plugin {
        " http://www.vim.org/scripts/script.php?script_id=3590
        autocmd vimrc BufRead,BufNewFile,BufWritePre *.coffee setf coffee
    "}

    " typescript plugin {
        autocmd vimrc BufRead,BufNewFile,BufWritePre *.ts setf typescript
    " }


    "less.vim {
        autocmd vimrc BufRead,BufNewFile,BufWritePre *.less setf less
    "}

    "calendar {
        "let g:calendar_mark = 'right'
    "}

    "zoom.vim {

        if has('gui_running')
            if has('gui_macvim')
                set guifont=Monaco:h11
                "\ 'DejaVu Sans Mono:h12',
                "\ 'Unbuntu Mono:h12'
                "\ 'Consolas:h12',
                let g:zoom_favorite_fonts = [
                    \ 'Inconsolata:h14',
                    \ 'Menlo:h12',
                    \ 'Monaco:h11',
                    \ 'Mononoki:h12'
                    \ ]
                let g:zoom_key_map = 1
                let g:zoom_random_font = 0
            else
                set guifont=Consolas:h12
            endif
        endif

    "}

    "ZoomWin {
        nnoremap <C-W>z <Plug>ZoomWin
    "}
    "
    "Syntastic {
        " the recommend setting form README
        set statusline+=%#warningmsg#
        set statusline+=%{SyntasticStatuslineFlag()}
        set statusline+=%*

        function! SyntasticCheckHook(errors)
            if !empty(a:errors)
                let g:syntastic_loc_list_height = min([len(a:errors), 3])
            endif
        endfunction

        if &diff
            let g:syntastic_always_populate_loc_list = 0
            let g:syntastic_auto_loc_list = 0
            let g:syntastic_check_on_open = 0
        else
            let g:syntastic_always_populate_loc_list = 1
            let g:syntastic_auto_loc_list = 1
            let g:syntastic_check_on_open = 1
        endif

        " @param {String} fileName
        " @param {Integer} limitTimes
        " @return {String}
        function! FindFileUp(fileName, limitTimes)
            let tempDir = fnamemodify(getcwd(), ':p:h')
            let tempFile = ''
            let counter = a:limitTimes
            let isExist = 0

            while counter > 0
                let tempFile = tempDir . '/' . a:fileName
                let isExist = filereadable(tempFile)
                if isExist
                    return tempFile
                endif
                let tempDir = fnamemodify(tempDir, ':p:h:h')
                let counter = counter - 1
            endwhile

            return ''
        endfunction

        let g:syntastic_check_on_wq = 1

        let g:syntastic_objc_compiler = 'clang'
        let g:syntastic_php_checkers = ['phpmd']
        let g:syntastic_vim_checkers = ['vint']

        let g:syntastic_java_checkers = ['javac', 'checkstyle']
        let g:syntastic_java_javac_config_file_enabled = 1

        let g:find_file_path = FindFileUp('.syntastic_javac_config', 10)
        if strlen(g:find_file_path) > 1
            let g:syntastic_java_javac_config_file = g:find_file_path
        endif

        let g:find_file_path = FindFileUp('.jshintrc', 5)
        if strlen(g:find_file_path) > 1
            let g:syntastic_javascript_checkers = ['jshint', 'tern-lint']
        else
            let g:syntastic_javascript_checkers = ['eslint', 'tern-lint']
            let g:syntastic_javascript_eslint_exec = 'eslint_d'
        endif
        unlet g:find_file_path

        let g:syntastic_typescript_checkers = ['tslint']

        let g:syntastic_mode_map = {
                    \ 'mode': 'passive',
                    \ 'active_filetypes': [
                        \ 'css',
                        \ 'html',
                        \ 'javascript',
                        \ 'typescript',
                        \ 'json',
                        \ 'less',
                        \ 'markdown',
                        \ 'php',
                        \ 'python',
                        \ 'sh',
                        \ 'vim',
                        \ 'xhtml',
                        \ 'xml',
                        \ 'zsh'
                    \],
                    \ 'passive_filetypes': [
                        \ 'c',
                        \ 'cpp',
                        \ 'java'
                    \]
                \}
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


    "easytags {
        let g:easytags_async = 1
        let g:easytags_syntax_keyword = 'always'

        set tags=./.tags;,~/.vimtags
        set cpoptions=aAceFsBd
        let g:easytags_dynamic_files = 2

        let g:easytags_languages = {
        \   'javascript': {
        \     'cmd': 'jsctags',
        \       'args': [],
        \       'fileoutput_opt': '-f',
        \       'stdout_opt': '-f-',
        \       'recurse_flag': '-R'
        \   }
        \}


    "}

    " tagbar {
        "let g:tagbar_ctags_bin = "/usr/local/Cellar/ctags/5.8/bin/ctags"
        "let g:tagbar_type_javascript = {
            "\ 'ctagsbin' : '~/workspace/github/tools/doctorjs/bin'
        "\ }
        let g:tagbar_sort = 0
        let g:tagbar_compact = 0
        let g:tagbar_show_visibility = 1
        let g:tagbar_expand = 1
        let g:tagbar_left = 1
        let g:tagbar_show_linenumbers = 2
        let g:tagbar_iconchars = ['▸', '▾']

        function! OpenTagbarIfAvailable()
            if exists(':Tagbar')
                call tagbar#autoopen(1)
            endif
        endfunction

        autocmd vimrc VimEnter * nested :call OpenTagbarIfAvailable()

        " from https://github.com/jszakmeister/markdown2ctags
        " Add support for markdown files in tagbar.
        let g:tagbar_type_markdown = {
            \ 'ctagstype': 'markdown',
            \ 'ctagsbin' : expand('~/.vim/bundle/markdown2ctags/markdown2ctags.py'),
            \ 'ctagsargs' : '-f - --sort=yes',
            \ 'kinds' : [
                \ 's:sections',
                \ 'i:images'
            \ ],
            \ 'sro' : '|',
            \ 'kind2scope' : {
                \ 's' : 'section',
            \ },
            \ 'sort': 0,
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
    " }

    " AutoCloseTag {
        " Make it so AutoCloseTag works for xml and xhtml files as well
        autocmd vimrc FileType xhtml,xml ru ftplugin/html/autoclosetag.vim
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
        autocmd vimrc BufNewFile,BufRead,BufWritePre .jshintrc setf json
        autocmd vimrc BufNewFile,BufRead,BufWritePre .eslintrc setf json
        "nmap <leader>jt <Esc>:%!python -m json.tool<CR><Esc>:set filetype=json<CR>
     " }

     " PyMode {
        "let g:pymode_lint_checker = "pyflakes"
        "let g:pymode_utils_whitespaces = 0
     " }

     " ctrlp {
        let g:ctrlp_working_path_mode = 'a'
        let g:ctrlp_by_filename = 0
        let g:ctrlp_match_current_file = 1
        let g:ctrlp_lazy_update = 1
        let g:ctrlp_match_current_file = 1

        " @param {String} type  available values:
        "
        "  wildignore
        "  ctrlp_user_command
        "  ctrlp_custom_ignore_dir
        "  ctrlp_custom_ignore_file
        "
        " @return {String}
        "
        function! CreateIgnoredCommand(type)
            let directoryList = [
                            \ '.git',
                            \ '.gitmodules',
                            \ '.svn',
                            \ '.settings',
                            \ 'node_modules',
                            \ 'bower_components',
                            \ 'node_modules',
                            \ 'dist',
                            \ 'libs'
                        \]
            let fileListWithFullName = [
                    \ '.DS_Store'
                    \]

            let fileListWithEndName = [
                    \ 'png',
                    \ 'jpg',
                    \ 'gif',
                    \ 'class',
                    \ 'jar',
                    \ 'so'
                    \]

            let prefix = ''
            let suffix = ''
            let splitter = ' '
            let strList = []

            if a:type ==# 'wildignore'
                let splitter = ','
                for item in directoryList
                    call add(strList, '*/' . item . '/*')
                endfor
                for item in fileListWithFullName
                    call add(strList, item)
                endfor
                for item in fileListWithEndName
                    call add(strList, '*.' . item)
                endfor
            elseif a:type ==# 'ctrlp_user_command'
                let prefix = 'ag %s -i --nocolor --nogroup '
                let suffix = ' --hidden -g ""'
                let splitter = ' '
                for item in directoryList
                    call add(strList, "--ignore '" . item . "'")
                endfor
                for item in fileListWithFullName
                    call add(strList, "--ignore '" . item . "'")
                endfor
                for item in fileListWithEndName
                    call add(strList, "--ignore '*." . item . "'")
                endfor
            elseif a:type ==# 'ctrlp_custom_ignore_dir'
                let prefix = '\v[\/]('
                let suffix = ')$'
                let splitter = '|'
                for item in directoryList
                    if stridx(item, '.') == 0
                        call add(strList, '\' . item)
                    else
                        call add(strList, item)
                    endif
                endfor
            elseif a:type ==# 'ctrlp_custom_ignore_file'
                let prefix = '\v('
                let suffix = ')$'
                let splitter = '|'
                for item in fileListWithFullName
                    if stridx(item, '.') == 0
                        call add(strList,  '\' . item)
                    else
                        call add(strList, item)
                    endif
                endfor
                for item in fileListWithEndName
                    call add(strList, '\.' .item)
                endfor
            endif

            let commandStr = prefix . join(strList, splitter) . suffix
            return commandStr
        endfunction

        let vimrc_temp_str = 'set wildignore+=' .  CreateIgnoredCommand('wildignore')
        exec vimrc_temp_str


        if executable('ag')
            let g:ctrlp_user_command = CreateIgnoredCommand('ctrlp_user_command')
            let g:ctrlp_use_caching = 0
        else
            let g:ctrlp_use_caching = 1
            let g:ctrlp_clear_cache_on_exit = 1
            let g:ctrlp_cache_dir = expand('~/.cache/ctrlp')
            let g:ctrlp_show_hidden = 1
            let g:ctrlp_max_files = 0
            let g:ctrlp_custom_ignore = {}
            let g:ctrlp_custom_ignore['dir'] = CreateIgnoredCommand('ctrlp_custom_ignore_dir')
            let g:ctrlp_custom_ignore['file'] = CreateIgnoredCommand('ctrlp_custom_ignore_file')
        endif


     "}


     " nvie/vim-flake8 {
        autocmd vimrc BufWritePost *.py call Flake8()
     " }

     " fs111/pydoc.vim {
        let g:pydoc_perform_mappings = 0
     " }

     " PythonMode {
        let g:pymode_rope_complete_on_dot = 0
        let g:pymode_rope_lookup_project = 0
        let g:pymode_lint_write = 0
        let g:pymode_lint_checkers = ['pyflakes', 'pep8', 'mccabe']
         " Disable if python support not present
        "if !has('python')
           "let g:pymode = 1
        "endif
     " }


     "ios devement {
        let g:clang_library_path = '/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang'

     " will133/vim-dirdiff' {
        let g:DirDiffExcludes = '.*'
        let g:DirDiffPreventSyntasticOpenLocationList = 1
     "}




     " indent_guides {
        "if !exists('g:spf13_no_indent_guides_autocolor')
            "let g:indent_guides_auto_colors = 1
        "else
            " for some colorscheme ,autocolor will not work,like 'desert','ir_black'.
            "autocmd vimrc VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=#212121   ctermbg=3
            "autocmd vimrc VimEnter,Colorscheme * :hi IndentGuidesEven guibg=#404040 ctermbg=4
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
        set guioptions=egm           " remove the toolbar
        set lines=40                " 40 lines of text instead of 24,
    else
        if &term ==? 'xterm' || &term ==? 'screen'
            set t_Co=256                 " Enable 256 colors to stop the CSApprox warning and make xterm vim shine
        endif
        "set term=builtin_ansi       " Make arrow and other keys work
    endif

    "macVim - full screen in high sierra is buggy, so disable it
    " if has('gui_macvim')
        " set fullscreen
    " endif
" }

 " Functions {

function! UnBundle(arg, ...)
  let bundle = vundle#config#init_bundle(a:arg, a:000)
  call filter(g:bundles, 'v:val["name_spec"] != "' . a:arg . '"')
endfunction

com! -nargs=+         UnBundle
\ call UnBundle(<args>)

function! InitializeDirectories()
    let separator = '.'
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
        let directory = parent . '/' . prefix . dirname . '/'
        if exists('*mkdir')
            if !isdirectory(directory)
                call mkdir(directory)
            endif
        endif
        if !isdirectory(directory)
            echo 'Warning: Unable to create backup directory: ' . directory
            echo 'Try: mkdir -p ' . directory
        else
            let directory = substitute(directory, ' ', '\\\\ ', 'g')
            exec 'set ' . settingname . '=' . directory
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
    if filereadable(expand('~/.vimrc.fork'))
        source ~/.vimrc.fork
    endif
" }

" Use local vimrc if available {
    if filereadable(expand('~/.vimrc.local'))
        source ~/.vimrc.local
    endif
" }

" Use local gvimrc if available and gui is running {
    if has('gui_running')
        if filereadable(expand('~/.gvimrc.local'))
            source ~/.gvimrc.local
        endif
    endif
" }
