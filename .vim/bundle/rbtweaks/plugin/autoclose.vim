" File: autoclose.vim
" Author: Karl Guertin <grayrest@gr.ayre.st>
" Version: 1.2
" Last Modified: June 18, 2009
" Description: AutoClose, closes what's opened.
"
"    This plugin closes opened parenthesis, braces, brackets, quotes as you
"    type them. As of 1.1, if you type the open brace twice ({{), the closing
"    brace will be pushed down to a new line.
"
"    You can enable or disable this plugin by typing \a (or <Leader>a if you
"    changed your Leader char). You can define your own mapping and will need
"    to do so if you have something else mapped to \a since this plugin won't
"    clobber your mapping. Here's how to map \x:
"
"       nmap <Leader>x <Plug>ToggleAutoCloseMappings
"
"    You'll also probably want to know you can type <C-V> (<C-Q> if mswin is
"    set) and the next character you type doesn't have mappings applied. This
"    is useful when you want to insert only an opening paren or something.
"
"    NOTE: If you're using this on a terminal and your arrow keys are broken,
"          be sure to :set ttimeout and :set ttimeoutlen=100
"
"    Version Changes: --------------------------------------------------{{{2
"    1.2   -- Fixed some edge cases where double the closing characters are
"             entered when exiting insert mode.
"             Finally (!) reproduced the arrow keys problem other people were
"             running into and fixed.
"             Typing a closing character will now behave consistently (jump
"             out) regardless of the plugin's internal state.
"
"             As a part of the close fix, I've opted to not try tracking the
"             position of the closing characters through all the things that
"             could be done with them, so arrowing/jumping around and not
"             winding up back where you started will cause the input to not be
"             repeatable.
"             June 18, 2009
"    1.1.2 -- Fixed a mapping typo and caught a double brace problem,
"             September 20, 2007
"    1.1.1 -- Missed a bug in 1.1, September 19, 2007
"    1.1   -- When not inserting at the end, previous version would eat chars
"             at end of line, added double open->newline, September 19, 2007
"    1.0.1 -- Cruft from other parts of the mapping, knew I shouldn't have
"             released the first as 1.0, April 3, 2007

" Setup -----------------------------------------------------{{{2
if exists('g:autoclose_loaded') || &cp
    finish
endif


let g:autoclose_loaded = 1
let s:cotstate = &completeopt

if !exists('g:autoclose_on')
    let g:autoclose_on = 1
endif

" (Toggle) Mappings -----------------------------{{{1
"
nmap <Plug>ToggleAutoCloseMappings :call <SID>ToggleAutoCloseMappings()<CR>
if (!hasmapto( '<Plug>ToggleAutoCloseMappings', 'n' ))
    nmap <unique> <Leader>a <Plug>ToggleAutoCloseMappings
endif
fun <SID>ToggleAutoCloseMappings() " --- {{{2
    if g:autoclose_on
        iunmap "
        iunmap '
        iunmap (
        iunmap )
        iunmap [
        iunmap ]
        iunmap {
        iunmap }
        iunmap <BS>
        iunmap <C-h>
        iunmap <Esc>
        let g:autoclose_on = 0
        echo "AutoClose Off"
    else
        inoremap <silent> " <C-R>=<SID>QuoteDelim('"')<CR>
        inoremap <silent> ' <C-R>=match(getline('.')[col('.') - 2],'\w') == 0 && getline('.')[col('.')-1] != "'" ? "'" : <SID>QuoteDelim("'")<CR>
        inoremap <silent> ( (<C-R>=<SID>CloseStackPush(')')<CR>
        inoremap ) <C-R>=<SID>CloseStackPop(')')<CR>
        inoremap <silent> [ [<C-R>=<SID>CloseStackPush(']')<CR>
        inoremap <silent> ] <C-R>=<SID>CloseStackPop(']')<CR>
        "inoremap <silent> { {<C-R>=<SID>CloseStackPush('}')<CR>
        inoremap <silent> { <C-R>=<SID>OpenSpecial('{','}')<CR>
        inoremap <silent> } <C-R>=<SID>CloseStackPop('}')<CR>
        inoremap <silent> <BS> <C-R>=<SID>OpenCloseBackspace()<CR>
        inoremap <silent> <C-h> <C-R>=<SID>OpenCloseBackspace()<CR>
        inoremap <silent> <Esc> <C-R>=<SID>CloseStackPop('')<CR><Esc>
        inoremap <silent> <C-[> <C-R>=<SID>CloseStackPop('')<CR><C-[>
        "the following simply creates an ambiguous mapping so vim fully
        "processes the escape sequence for terminal keys, see 'ttimeout' for a
        "rough explanation, this just forces it to work
        if &term[:4] == "xterm"
            inoremap <silent> <C-[>OC <RIGHT>
        endif
        let g:autoclose_on = 1
        if a:0 == 0
            "this if is so this message doesn't show up at load
            echo "AutoClose On"
        endif
    endif
endf
let s:closeStack = []

" AutoClose Utilities -----------------------------------------{{{1
function <SID>OpenSpecial(ochar,cchar) " ---{{{2
    let line = getline('.')
    let col = col('.') - 2
    "echom string(col).':'.line[:(col)].'|'.line[(col+1):]
    if a:ochar == line[(col)] && a:cchar == line[(col+1)] "&& strlen(line) - (col) == 2
        "echom string(s:closeStack)
        while len(s:closeStack) > 0
            call remove(s:closeStack, 0)
        endwhile
        return "\<esc>a\<CR>;\<CR>".a:cchar."\<esc>\"_xk$\"_xa"
    endif
    return a:ochar.<SID>CloseStackPush(a:cchar)
endfunction

function <SID>CloseStackPush(char) " ---{{{2
    "echom "push"
    let line = getline('.')
    let col = col('.')-2
    if (col) < 0
        call setline('.',a:char.line)
    else
        "echom string(col).':'.line[:(col)].'|'.line[(col+1):]
        call setline('.',line[:(col)].a:char.line[(col+1):])
    endif
    call insert(s:closeStack, a:char)
    "echom join(s:closeStack,'').' -- '.a:char
    return ''
endf

function <SID>JumpOut(char) " ----------{{{2
    let column = col('.') - 1
    let line = getline('.')
    let mcol = match(line[column :], a:char)
    if a:char != '' &&  mcol >= 0
        "Yeah, this is ugly but vim actually requires each to be special
        "cased to avoid screen flashes/not doing the right thing.
        echom len(line).' '.(column+mcol)
        if line[column] == a:char
            return "\<Right>"
        elseif column+mcol == len(line)-1
            return "\<C-O>A"
        else
            return "\<C-O>f".a:char."\<Right>"
        endif
    else
        return a:char
    endif
endf
function <SID>CloseStackPop(char) " ---{{{2
    "echom "pop"
    if(a:char == '')
        pclose
    endif
    if len(s:closeStack) == 0
        return <SID>JumpOut(a:char)
    endif
    let column = col('.') - 1
    let line = getline('.')
    let popped = ''
    let lastpop = ''
    "echom join(s:closeStack,'').' || '.lastpop
    while len(s:closeStack) > 0 && ((lastpop == '' && popped == '') || lastpop != a:char)
        let lastpop = remove(s:closeStack,0)
        let popped .= lastpop
        "echom join(s:closeStack,'').' || '.lastpop.' || '.popped
    endwhile
    "echom ' --> '.popped
    if line[column : column+strlen(popped)-1] != popped
        return <SID>JumpOut('')
    endif
    if column > 0
        call setline('.',line[:column-1].line[(column+strlen(popped)):])
    else
        call setline('.','')
    endif
    return popped
endf

function <SID>QuoteDelim(char) " ---{{{2
  let line = getline('.')
  let col = col('.')
  if line[col - 2] == "\\"
    "Inserting a quoted quotation mark into the string
    return a:char
  elseif line[col - 1] == a:char
    "Escaping out of the string
    return "\<C-R>=".s:SID()."CloseStackPop(\"\\".a:char."\")\<CR>"
  else
    "Starting a string
    return a:char."\<C-R>=".s:SID()."CloseStackPush(\"\\".a:char."\")\<CR>"
  endif
endf

" The strings returned from QuoteDelim aren't in scope for <SID>, so I
" have to fake it using this function (from the Vim help, but tweaked)
function s:SID()
    return matchstr(expand('<sfile>'), '<SNR>\d\+_\zeSID$')
endfun

function <SID>OpenCloseBackspace() " ---{{{2
    "if pumvisible()
    "    pclose
    "    call <SID>StopOmni()
    "    return "\<C-E>"
    "else
        let curline = getline('.')
        let curpos = col('.')
        let curletter = curline[curpos-1]
        let prevletter = curline[curpos-2]
        if (prevletter == '"' && curletter == '"') ||
\          (prevletter == "'" && curletter == "'") ||
\          (prevletter == "(" && curletter == ")") ||
\          (prevletter == "{" && curletter == "}") ||
\          (prevletter == "[" && curletter == "]")
            if len(s:closeStack) > 0
                call remove(s:closeStack,0)
            endif
            return "\<Delete>\<BS>"
        else
            return "\<BS>"
        endif
    "endif
endf

" Initialization ----------------------------------------{{{1
if g:autoclose_on
    let g:autoclose_on = 0
    silent call <SID>ToggleAutoCloseMappings()
endif
" vim: set ft=vim ff=unix et sw=4 ts=4 :
" vim600: set foldmethod=marker foldmarker={{{,}}} foldlevel=1 :
