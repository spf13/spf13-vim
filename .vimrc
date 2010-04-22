runtime! autoload/pathogen.vim
silent! call pathogen#runtime_append_all_bundles()

" Modeline and Notes {
" 	vim: set foldmarker={,} foldlevel=0 foldmethod=marker spell:
"
" 	This is the personal .vimrc file of Steve Francia.
" 	While much of it is beneficial for general use, I would
" 	recommend picking out the parts you want and understand.
"
" 	You can find me at http://spf13.com
" }

" Basics {
	set nocompatible 		" must be first line
	set background=dark     " Assume a dark background
" }
 
" General {
	filetype plugin indent on  	" Automatically detect file types.
	syntax on 					" syntax highlighting
	set mouse=a					" automatically enable mouse usage
	"set autochdir 				" always switch to the current file directory.. 
	" not every vim is compiled with this, use the following line instead
 	autocmd BufEnter * if bufname("") !~ "^\[A-Za-z0-9\]*://" | lcd %:p:h | endif
	scriptencoding utf-8
	set autowrite
	set shortmess+=filmnrxoOtT     	" abbrev. of messages (avoids 'hit enter')
	" set spell 		 	     	" spell checking on
	set backup 						" backups are nice ...
	set backupdir=$HOME/.vimbackup  " but not when they clog .
	set directory=$HOME/.vimswap 	" Same for swap files
	set viewdir=$HOME/.vimviewdir   " same but for view files
	silent execute '!mkdir -p $HOME/.vimbackup'
	silent execute '!mkdir -p $HOME/.vimswap'
	silent execute '!mkdir -p $HOME/.vimviewdir'
	au BufWinLeave * mkview 		 "make vim save and load the folding of the document each time it loads
	au BufWinEnter * silent loadview "also places the cursor in the last place that it was left.
" }

" Vim UI {
"	color tango        	       		" load a colorscheme
	set tabpagemax=15 				" only show 15 tabs
	set showmode                   	" display the current mode

	set cursorline  				" highlight current line
	hi cursorline guibg=#333333 	" highlight bg color of current line
	hi CursorColumn guibg=#333333   " highlight cursor

	if has('cmdline_info')
		set ruler                  	" show the ruler
		set rulerformat=%30(%=\:b%n%y%m%r%w\ %l,%c%V\ %P%) " a ruler on steroids
		set showcmd                	" show partial commands in status line and
									" selected characters/lines in visual mode
	endif

	if has('statusline')
		set laststatus=1           	" show statusline only if there are > 1 windows
		set statusline=%<%f\ %=\:\b%n%y%m%r%w\ %l,%c%V\ %P " a statusline, also on steroids
	endif

	set backspace=indent,eol,start 	" backspace for dummys
	set linespace=0 				" No extra spaces between rows
	set nu 							" Line numbers on
	set showmatch                  	" show matching brackets/parenthesis
	set incsearch 					" find as you type search
	set hlsearch 					" highlight search terms
	set winminheight=0 				" windows can be 0 line high 
	set ignorecase 					" case insensitive search
	set smartcase 					" case sensitive when uc present
	set wildmenu 					" show list instead of just completing
	set wildmode=list:longest,full 	" comand <Tab> completion, list matches, then longest common part, then all.
	set whichwrap=b,s,h,l,<,>,[,]	" backspace and cursor keys wrap to
	set scrolljump=5 				" lines to scroll when cursor leaves screen
	set scrolloff=3 				" minimum lines to keep above and below cursor
	set foldenable  				" auto fold code
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
" }

" Key Mappings {

	" Easier moving in tabs and windows
	map <C-J> <C-W>j<C-W>_
	map <C-K> <C-W>k<C-W>_
	map <C-L> <C-W>l<C-W>_
	map <C-H> <C-W>h<C-W>_
	map <C-K> <C-W>k<C-W>_
	map <S-H> gT
	map <S-L> gt

	" Stupid shift key fixes
	map:W :w 						
	map:WQ :wq
	map:wQ :wq
	map:Q :q

	" Yank from the cursor to the end of the line, to be consistent with C and D.
	nnoremap Y y$
" }

" Plugins {

	" VCSCommand {
		let b:VCSCommandMapPrefix=',v'
		let b:VCSCommandVCSType='git'
		""let mapleader = "-"
	" } 

	" Misc { 
		:map <C-F10> <Esc>:vsp<CR>:VTree<CR>
		" map Control + F10 to Vtree

		let g:checksyntax_auto = 0

		"comment out line(s) in visual mode
		vmap  o  :call NERDComment(1, 'toggle')<CR>
		let g:NERDShutUp=1

		let b:match_ignorecase = 1
	" }

	" ShowMarks {
		let showmarks_include = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
		" Don't leave on by default, use :ShowMarksOn to enable
		let g:showmarks_enable = 0
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
		if has("autocmd") && exists("+omnifunc")
			autocmd Filetype *
				\if &omnifunc == "" |
				\setlocal omnifunc=syntaxcomplete#Complete |
				\endif
		endif

		" Popup menu hightLight Group
		"highlight Pmenu 	ctermbg=13 	guibg=DarkBlue
		highlight PmenuSel 	ctermbg=7 	guibg=DarkBlue 		guifg=LightBlue
		"highlight PmenuSbar ctermbg=7 	guibg=DarkGray
		"highlight PmenuThumb 			guibg=Black

		hi Pmenu  guifg=#000000 guibg=#F8F8F8 ctermfg=black ctermbg=Lightgray
		hi PmenuSbar  guifg=#8A95A7 guibg=#F8F8F8 gui=NONE ctermfg=darkcyan ctermbg=lightgray cterm=NONE
		hi PmenuThumb  guifg=#F8F8F8 guibg=#8A95A7 gui=NONE ctermfg=lightgray ctermbg=darkcyan cterm=NONE

		" some convenient mappings 
		inoremap <expr> <Esc>      pumvisible() ? "\<C-e>" : "\<Esc>"
		inoremap <expr> <CR>       pumvisible() ? "\<C-y>" : "\<CR>"
		inoremap <expr> <Down>     pumvisible() ? "\<C-n>" : "\<Down>"
		inoremap <expr> <Up>       pumvisible() ? "\<C-p>" : "\<Up>"
		inoremap <expr> <C-d> 	   pumvisible() ? "\<PageDown>\<C-p>\<C-n>" : "\<C-d>"
		inoremap <expr> <C-u>      pumvisible() ? "\<PageUp>\<C-p>\<C-n>" : "\<C-u>"

		" automatically open and close the popup menu / preview window
		au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
		set completeopt=menu,longest,preview
	" }
	
	" Ctags {
		set tags=./tags;/	
	" }

" }

" GUI Settings {
	" GVIM- (here instead of .gvimrc)
	if has('gui_running')
		set guioptions-=T          	" remove the toolbar
		set lines=40               	" 40 lines of text instead of 24,
	endif
" }

" Windows Compatible {
	" On Windows, also use '.vim' instead of 'vimfiles'; this makes synchronization
	" across (heterogeneous) systems easier. 
	if has('win32') || has('win64')
	  set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after
	endif
" }
