" Modeline and Notes {
" vim: set foldmarker={,} foldlevel=0 foldmethod=marker spell:
"
"	This is the personal .vimrc file of Steve Francia.
"	While much of it is beneficial for general use, I would
"	recommend picking out the parts you want and understand.
"
"	You can find me at http://spf13.com
" }

" Environment {
	" Basics {
		set nocompatible 		" must be first line
	" }

" Setup Bundle Support {
" The next two lines ensure that the ~/.vim/bundle/ system works
	runtime! autoload/pathogen.vim
	silent! call pathogen#helptags()
	silent! call pathogen#runtime_append_all_bundles()
" }

" Basics {
	set nocompatible		" must be first line
	set background=dark     " Assume a dark background
" }
 
" General {
	filetype plugin indent on	" Automatically detect file types.
	syntax on					" syntax highlighting
    set hidden                      " allow leaving a buffer when it is unsaved
	" Windows Compatible {
		" On Windows, also use '.vim' instead of 'vimfiles'; this makes synchronization
		" across (heterogeneous) systems easier. 
		if has('win32') || has('win64')
		  set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after
		endif
	" }

	" Setup Bundle Support {
	" The next two lines ensure that the ~/.vim/bundle/ system works
		"runtime! autoload/pathogen.vim
		silent! call pathogen#runtime_append_all_bundles()
		call pathogen#helptags()
	" }
" } 
	
" General {
	set background=dark         " Assume a dark background
	filetype plugin indent on  	" Automatically detect file types.
	syntax on 					" syntax highlighting
	scriptencoding utf-8
	" set autowrite                  " automatically write a file when leaving a modified buffer
	set shortmess+=filmnrxoOtT     	" abbrev. of messages (avoids 'hit enter')
	set viewoptions=folds,options,cursor,unix,slash " better unix / windows compatibility
	set virtualedit=onemore 	   	" allow for cursor beyond last character
	set history=1000  				" Store a ton of history (default is 20)
	" set spell 		 	        	" spell checking on
	
	" Setting up the directories {
		set backup 						" backups are nice ...
        " Moved to function at bottom of the file
		"set backupdir=$HOME/.vimbackup//  " but not when they clog .
		"set directory=$HOME/.vimswap// 	" Same for swap files
		"set viewdir=$HOME/.vimviews// 	" same for view files
		
		"" Creating directories if they don't exist
		"silent execute '!mkdir -p $HVOME/.vimbackup'
		"silent execute '!mkdir -p $HOME/.vimswap'
		"silent execute '!mkdir -p $HOME/.vimviews'
		au BufWinLeave * silent! mkview  "make vim save view (state) (folds, cursor, etc)
		au BufWinEnter * silent! loadview "make vim load view (state) (folds, cursor, etc)
	" }
" }

" Vim UI {
	color blackboard				" load a colorscheme
	set tabpagemax=15				" only show 15 tabs
	set showmode					" display the current mode

	set cursorline					" highlight current line
	hi cursorline guibg=#333333		" highlight bg color of current line
	hi CursorColumn guibg=#333333   " highlight cursor

	if has('cmdline_info')
		set ruler					" show the ruler
		set rulerformat=%30(%=\:b%n%y%m%r%w\ %l,%c%V\ %P%) " a ruler on steroids
		set showcmd					" show partial commands in status line and
									" selected characters/lines in visual mode
	endif

    set laststatus=2
    set statusline=%f%m%r%h%w\ [%{&ff}/%Y]\ [%{getcwd()}]\ [A=\%03.3b/H=\%02.2B]\ [%04l,%04v]\ [%p%%]\ [LEN=%L]

	set backspace=indent,eol,start	" backspace for dummys
	set linespace=0					" No extra spaces between rows
	set nu							" Line numbers on
	set showmatch					" show matching brackets/parenthesis
	set incsearch					" find as you type search
	set hlsearch					" highlight search terms
	set winminheight=0				" windows can be 0 line high 
	set ignorecase					" case insensitive search
	set smartcase					" case sensitive when uc present
	set wildmenu					" show list instead of just completing
	set wildmode=list:longest,full	" comand <Tab> completion, list matches, then longest common part, then all.
	set whichwrap=b,s,h,l,<,>,[,]	" backspace and cursor keys wrap to
	set scrolljump=5				" lines to scroll when cursor leaves screen
	set scrolloff=3					" minimum lines to keep above and below cursor
	set nofoldenable				" disable auto fold code
	set gdefault					" the /g flag on :s substitutions by default

" }

" Formatting {
	set nowrap                     	" wrap long lines
	set autoindent                 	" indent at the same level of the previous line
	set shiftwidth=4               	" use indents of 4 spaces
	set noexpandtab 	       		" tabs are tabs, not spaces
	set tabstop=4 					" an indentation every four columns
	"set matchpairs+=<:>            	" match, to be used with % 
	set pastetoggle=<F12>          	" pastetoggle (sane indentation on pastes)
	"set comments=sl:/*,mb:*,elx:*/  " auto format comment blocks
	" Remove trailing whitespaces and ^M chars
	autocmd FileType c,cpp,java,php,js,twig,xml,yml autocmd BufWritePre <buffer> :call setline(1,map(getline(1,"$"),'substitute(v:val,"\\s\\+$","","")'))
" }

" Python helpers {
	highlight BadWhitespace ctermbg=red guibg=red
	" Display tabs at the beginning of a line in Python mode as bad.
	au BufRead,BufNewFile *.py,*.pyw match BadWhitespace /^\t\+/
	au BufNewFile *.py,*.pyw,*.c,*.h set fileformat=unix
	let python_highlight_all=1
" }

" Key Mappings {

	"The default leader is '\', but many people prefer ',' as it's in a standard
	"location
	let mapleader = ','

	" Easier moving in tabs and windows
	map <C-J> :2 tabn
	map <C-K> :2 tabp
	map <C-L> :tabn
	map <C-H> :tabp 

	" Yank from the cursor to the end of the line, to be consistent with C and D.
	nnoremap Y y$
		
	""" Code folding options
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

	" Shortcuts
	" Change Working Directory to that of the current file
    cmap cwd lcd %:p:h
	cmap cd. lcd %:p:h

	" visual shifting (does not exit Visual mode)
	vnoremap < <gv
	vnoremap > >gv 

	" Fix home and end keybindings for screen, particularly on mac
	" - for some reason this fixes the arrow keys too. huh.
	map [F $
	imap [F $
	map [H g0
	imap [H g0
		
	" For when you forget to sudo.. Really Write the file.
	cmap w!! w !sudo tee % >/dev/null
" }

" Plugins {

	" VCSCommand {
"		let b:VCSCommandMapPrefix=',v'
"		let b:VCSCommandVCSType='git'
	" } 
	
	" PIV {
		let g:DisableAutoPHPFolding = 0
		let cfu=phpcomplete#CompletePHP
	" }
	
	" Supertab {
		let g:SuperTabDefaultCompletionType = "context"
		let g:SuperTabContextDefaultCompletionType = "<c-x><c-o>"
	" }

	" Misc { 
		:map <C-F10> <Esc>:vsp<CR>:VTree<CR>
		" map Control + F10 to Vtree

        noremap <leader><F5> :CheckSyntax<cr>
		let g:checksyntax_auto = 1

		"comment out line(s) in visual mode -RB: If you do this, you can't
        "switch sides of the comment block in visual mode.
		"vmap  o  :call NERDComment(1, 'toggle')<CR>
		let g:NERDShutUp=1

		let b:match_ignorecase = 1
	" }
	
	" ShowMarks {
		let showmarks_include = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
		" Don't leave on by default, use :ShowMarksOn to enable
		let g:showmarks_enable = 1
		" For marks a-z
		highlight ShowMarksHLl gui=bold guibg=LightBlue guifg=Blue
		" For marks A-Z
		highlight ShowMarksHLu gui=bold guibg=LightRed guifg=DarkRed
		" For all other marks
		highlight ShowMarksHLo gui=bold guibg=LightYellow guifg=DarkYellow
		" For multiple marks on the same line.
		highlight ShowMarksHLm gui=bold guibg=LightGreen guifg=DarkGreen
	" }
	
	" OmniComplete {
		"if has("autocmd") && exists("+omnifunc")
			"autocmd Filetype *
				"\if &omnifunc == "" |
				"\setlocal omnifunc=syntaxcomplete#Complete |
				"\endif
		"endif

		" Popup menu hightLight Group
		"highlight Pmenu	ctermbg=13	guibg=DarkBlue
        "highlight PmenuSel	ctermbg=7	guibg=DarkBlue		guifg=LightBlue
		"highlight PmenuSbar ctermbg=7	guibg=DarkGray
		"highlight PmenuThumb			guibg=Black

		hi Pmenu  guifg=#000000 guibg=#F8F8F8 ctermfg=black ctermbg=Lightgray
		hi PmenuSbar  guifg=#8A95A7 guibg=#F8F8F8 gui=NONE ctermfg=darkcyan ctermbg=lightgray cterm=NONE
		hi PmenuThumb  guifg=#F8F8F8 guibg=#8A95A7 gui=NONE ctermfg=lightgray ctermbg=darkcyan cterm=NONE

		" some convenient mappings 
		inoremap <expr> <Esc>      pumvisible() ? "\<C-e>" : "\<Esc>"
		inoremap <expr> <CR>       pumvisible() ? "\<C-y>" : "\<CR>"
		inoremap <expr> <Down>     pumvisible() ? "\<C-n>" : "\<Down>"
		inoremap <expr> <Up>       pumvisible() ? "\<C-p>" : "\<Up>"
		inoremap <expr> <C-d>	   pumvisible() ? "\<PageDown>\<C-p>\<C-n>" : "\<C-d>"
		inoremap <expr> <C-u>      pumvisible() ? "\<PageUp>\<C-p>\<C-n>" : "\<C-u>"

        " and make sure that it doesn't break supertab
        let g:SuperTabCrMapping = 0
        
		" automatically open and close the popup menu / preview window
		au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
		set completeopt=menu,preview,longest
	" }
	
	" Ctags {
		set tags=./tags;/,~/.vimtags
	" }

	" EasyTags {
		let g:easytags_cmd = 'ctags'
	" }

	" Delimitmate {
		"au FileType * let b:delimitMate_autoclose = 1

		" If using html auto complete (complete closing tag)
        au FileType xml,html,xhtml let b:delimitMate_matchpairs = "(:),[:],{:}"
	" }
	
	" AutoCloseTag {
		" Make it so AutoCloseTag works for xml and xhtml files as well
		au FileType xhtml,xml ru ftplugin/html/autoclosetag.vim
	" }

	" SnipMate {
		" Setting the author var
		let g:snips_author = 'Richard Bateman <taxilian@gmail.com>'
		" Shortcut for reloading snippets, useful when developing
		nnoremap ,smr <esc>:exec ReloadAllSnippets()<cr>
	" }

	" NerdTree {
		map <C-e> :NERDTreeToggle<CR>:NERDTreeMirror<CR>
		map <leader>e :NERDTreeFind<CR>
		nmap <leader>nt :NERDTreeFind<CR>

		let NERDTreeShowBookmarks=1
		let NERDTreeIgnore=['\.pyc', '\~$', '\.swo$', '\.swp$', '\.git', '\.hg', '\.svn', '\.bzr']
		let NERDTreeChDirMode=0
		let NERDTreeQuitOnOpen=0
		let NERDTreeShowHidden=1
		let NERDTreeKeepTreeInNewTab=1
	
	" Richard's plugins {
		" Fuzzy Finder {
			""" Fuzzy Find file, tree, buffer, line
			nmap <leader>ff :FufFile **/<CR>
			nmap <leader>ft :FufFile<CR>
			nmap <leader>fb :FufBuffer<CR>
			nmap <leader>fl :FufLine<CR>
			nmap <leader>fr :FufRenewCache<CR>
		" }

		" Session List {
			set sessionoptions=blank,buffers,curdir,folds,tabpages,winsize
			nmap <leader>sl :SessionList<CR>
			nmap <leader>ss :SessionSave<CR>
		" }
		
		" Buffer explorer {
			nmap <leader>b :BufExplorer<CR>
		" }
		
		" Project related {
			map <C-p> <Plug>ToggleProject
			map <C-e> :NERDTreeToggle<CR>:NERDTreeMirror<CR>
			map <leader>e :NERDTreeFind<CR>
			nmap <leader>nt :NERDTreeFind<CR>

            let NERDTreeShowBookmarks=1
            let NERDTreeIgnore=['\.pyc', '\~$', '\.swo$', '\.swp$', '\.git', '\.hg', '\.svn', '\.bzr']
            let NERDTreeChDirMode=2
            let NERDTreeQuitOnOpen=0
            let NERDTreeShowHidden=1
            let NERDTreeKeepTreeInNewTab=1
			"au BufEnter * call NERDTreeInitAsNeeded()
		" }
		
		" VCS commands {
			nmap <leader>vs :VCSStatus<CR>
			nmap <leader>vc :VCSCommit<CR>
			nmap <leader>vb :VCSBlame<CR>
			nmap <leader>va :VCSAdd<CR>
			nmap <leader>vd :VCSVimDiff<CR>
			nmap <leader>vl :VCSLog<CR>
			nmap <leader>vu :VCSUpdate<CR>
		" }
		" php-doc commands {
			nmap <leader>pd :call PhpDocSingle()<CR>
			vmap <leader>pd :call PhpDocRange()<CR>
		" }
		
		" Debugging with VimDebugger {
			map <F11> :DbgStepInto<CR>
			map <F10> :DbgStepOver<CR>
			map <S-F11> :DbgStepOut<CR>
			map <F5> :DbgRun<CR>
			map <F6> :DbgDetach<CR>
			map <F8> :DbgToggleBreakpoint<CR>
			map <S-F8> :DbgFlushBreakpoints<CR>
			map <F9> :DbgRefreshWatch<CR>
			map <S-F9> :DbgAddWatch<CR>
		" }

		" Taglist Variables {
			let Tlist_Auto_Highlight_Tag = 1
			let Tlist_Auto_Update = 1
			let Tlist_Exit_OnlyWindow = 1
			let Tlist_File_Fold_Auto_Close = 1
			let Tlist_Highlight_Tag_On_BufEnter = 1
			let Tlist_Use_Right_Window = 1
			let Tlist_Use_SingleClick = 1

			let g:ctags_statusline=1
			" Override how taglist does javascript
			let g:tlist_javascript_settings = 'javascript;f:function;c:class;m:method;p:property;v:global'
		 " }
	" }
" }

" GUI Settings {
	" GVIM- (here instead of .gvimrc)
	if has('gui_running')
		set guioptions-=T          	" remove the toolbar
		set lines=40               	" 40 lines of text instead of 24,
		set transparency=5          " Make the window slightly transparent
	else
		set term=builtin_ansi       " Make arrow and other keys work
	endif
" }

function! InitializeDirectories()
  let separator = "."
  let parent = $HOME 
  let prefix = '.vim'
  let dir_list = { 
			  \ 'backup': 'backupdir', 
			  \ 'views': 'viewdir', 
			  \ 'swap': 'directory' }

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
          if has('win32') || has('win64')
              " Adding an extra trailing slash so it stores the path and not just the
              " filename so there aren't collisions for backups
              " Windows Vista / 7 has UAC issues, so setting $temp as fallback
              exec "set " . settingname . "=\"" . directory . "\""
          else
              " For Linux/Mac OS (others?) these directives must not be quoted
              exec "set " . settingname . "=" . directory
          endif
	  endif
  endfor
endfunction
call InitializeDirectories() 

function! NERDTreeInitAsNeeded()
    redir => bufoutput
    buffers!
    redir END
    let idx = stridx(bufoutput, "NERD_tree")
    if idx > -1
        NERDTreeMirror
        NERDTreeFind
        wincmd l
    endif
endfunction

" Use local vimrc if available {
    if filereadable(expand("~/.vimrc.local"))
        source ~/.vimrc.local
    endif
" }
