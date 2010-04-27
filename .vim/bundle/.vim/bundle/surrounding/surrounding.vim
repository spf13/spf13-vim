" File: surrounding.vim
" Author: Michael Sanders
" Description: A heavily modified, simplified fork of surround.vim by Tim Pope.
" Updated: February 10, 2010

if exists('s:did_surrounding') || &cp || version < 700
	finish
endif
let s:did_surrounding = 1

nn cs :<c-u>call <SID>ModifySurround(<SID>Input(), <SID>Input())<cr>
nn ds :<c-u>call <SID>ModifySurround(<SID>Input())<cr>
xno s :<c-u>call <SID>AddSurround(visualmode(), 1)<cr>
nn <silent> ys :<c-u>set opfunc=<SID>AddSurround<cr>g@
nm yss 0ys$

" Return next character typed by user.
fun s:Input()
	echoh ModeMsg | echo '-- SURROUND --' | echoh None
	let c = nr2char(getchar())
	if c == ' '
		let c .= nr2char(getchar())
	endif
	echo '' | redraw
	return c == "\<esc>" || c == "\<cr>" ? '' : c
endf

" Surround selected text with input from user.
" If a:0 is given, visual mode is assumed to be used; otherwise, an opfunc
fun s:AddSurround(type, ...)
	" Deal with motions, if used in normal mode.
	if !a:0
		let sel_save = &sel | let &sel = 'inclusive'
		exe 'norm! `['.(a:type == 'char' ? 'v' : 'V')."`]\<esc>"
		let sel_save = &sel
	endif
	let char = s:Input()

	if a:type ==# 'V' " Line mode
		let chars = s:Wrap('', char) | let half = len(chars)/2
		call setline("'<", strpart(chars, 0, half).getline("'<"))
		call setline("'>", getline("'>").strpart(chars, half))
	else " Just visual mode
		let old = @"
		norm! gvc
		let @" = s:Wrap(@", char)
		exe 'norm! '.(virtcol('.') == 1 ? 'P' : 'p')
		let @" = old
	endif

	if a:0
		sil! call repeat#set('1vs'.char)
		" NOTE: This needs to be fixed -- it needs to save the motion/char used
		"       An opfunc does not appear to give us access to the motion
		"       used, so I'm not sure how to do this.
		sil! call repeat#set('ys')
	endif
endf

" Returns list with pair of characters matching given character.
"
" If character is not supported, a list with two empty strings ["", ""]
" is returned.
"
" If character is a "t", the user is asked to input an XML tag and the
" beginning and end tags are returned in a list. If the character is a
" capitalized "T", the beginning and end tags returned are separated
" by newlines.
fun s:MatchChar(char)
	if a:char ==# 'b' || a:char == '(' || a:char == ')'
		return ['(', ')']
	elseif a:char ==# 'r' || a:char == ']' || a:char == '['
		return ['[', ']']
	elseif a:char ==# 'B' || a:char == '{' || a:char == '}'
		return ['{', '}']
	elseif a:char ==# 'a' || a:char == '<' || a:char == '>'
		return ['<', '>']
	elseif a:char == 't' " Insert tag
		if !hasmapto('>', 'c')
			let remapped = 1
			cno > <cr>
		endif

		let tag = input('<')
		if !remapped | let tag = substitute(tag, '>*$', '', '') | endif
		echo '<'.tag.'>'
		if remapped
			sil! cunmap >
		endif

		let cl = '</'.substitute(tag, ' .*', '', '').'>'

		return a:char ==# 'T' ? ['<'.tag.">\n", "\n".cl] : ['<'.tag.'>', cl]
	endif

	return a:char =~ '\W' ? [a:char, a:char] : ['', '']
endf

" Change or delete surrounding characters.
" "orig" should be the original character to be changed, and a:1 should be the
" new character is substituting or not given if deleting.
fun s:ModifySurround(orig, ...)
	if a:orig == '' | return | endif
	if a:0 " Save position if replacing characters.
		if a:1 == '' | return | endif
		let line = line('.') | let col = col('.')
	endif

	let char = a:orig
	if len(char) > 1
		let space = ' '
		let char = strpart(char, 1)
	endif

	if char ==# 'a'
		let char = '>'
	elseif char ==# 'r'
		let char = ']'
	endif

	let old = @"
	let @" = ''
	exe 'norm! d'.v:count1.'i'.char

	" If di<char> failed, quit.
	if @" == ''
		let @" = old | return
	elseif exists('space')
		let @" = substitute(@",'^\s+\|\s+$', '', 'g')
	endif

	let oldLine = getline('.')
	let oldLnum = line('.')
	let oldLineLen = col('$')
	if char == '"' || char == "'" || char == '`'
		exe "norm! i \<esc>\"_d2i".char
	else
		exe 'norm! "_da'.char
	endif

	if a:0 | let @" = s:Wrap(@", a:1) | endif
	let endLine = col('$')
	sil exe 'norm! ""'.(col("']") == endLine && col('.')+1 == endLine ? 'p' : 'P')

	let @" = old
	if exists('space') | let char = ' '.char | endif
	if a:0
		sil! call repeat#set('cs'.char.a:1)
		call cursor(line, col)
	else
		sil! call repeat#set('ds'.char)
	endif
endf

" Returns string wrapped in the pair of the given character.
" If character contains a space, it is also wrapped appropriately.
fun s:Wrap(string, char)
	if len(a:char) > 1 " Parsing character with a space
		let space = ' '
		let chars = s:MatchChar(strpart(a:char, 1))
	else
		let space = ''
		let chars = s:MatchChar(a:char)
	endif
	return chars[0].space.a:string.space.chars[1]
endf

" vim:noet:sw=4:ts=4:ft=vim
