"=============================================================================
" Copyright (C) 2009-2010 Takeshi NISHIDA
"
"=============================================================================
" LOAD GUARD {{{1

if !l9#guardScriptLoading(expand('<sfile>:p'), 0, 0, [])
  finish
endif

" }}}1
"=============================================================================
" QUICKFIX {{{1

" Returns non-zero if quickfix window is opened.
function l9#quickfix#isWindowOpened()
  return count(map(range(1, winnr('$')), 'getwinvar(v:val, "&buftype")'), 'quickfix') > 0
endfunction

" Opens quickfix window if quickfix is not empty, and echo the number of errors.
"
" a:onlyRecognized: if non-zero, opens only if quickfix has recognized errors.
" a:holdCursor: if non-zero, the cursor won't move to quickfix window.
function l9#quickfix#openIfNotEmpty(onlyRecognized, holdCursor)
  let numErrors = len(filter(getqflist(), 'v:val.valid'))
  let numOthers = len(getqflist()) - numErrors
  if numErrors > 0 || (!a:onlyRecognized && numOthers > 0)
    copen
    if a:holdCursor
      wincmd p
    endif
  else
    cclose
  endif
  redraw
  if numOthers > 0
    echo printf('Quickfix: %d(+%d)', numErrors, numOthers)
  else
    echo printf('Quickfix: %d', numErrors)
  endif
endfunction

" Toggles Quickfix window
function l9#quickfix#toggleWindow()
  if l9#quickfix#isWindowOpened()
    cclose
  else
    call l9#quickfix#openIfNotEmpty(0, 0)
  endif
endfunction

" Creates quickfix list form given lines and opens the quickfix window if
" errors exists.
"
" a:lines: 
" a:jump: if non-zero, jump to the first error.
function l9#quickfix#setMakeResult(lines)
  cexpr a:lines
  call l9#quickfix#openIfNotEmpty(0, 1)
endfunction

" Compares quickfix entries for sorting.
function l9#quickfix#compareEntries(e0, e1)
  if     a:e0.bufnr != a:e1.bufnr
    let i0 = bufname(a:e0.bufnr)
    let i1 = bufname(a:e1.bufnr)
  elseif a:e0.lnum != a:e1.lnum
    let i0 = a:e0.lnum
    let i1 = a:e1.lnum
  elseif a:e0.col != a:e1.col
    let i0 = a:e0.col
    let i1 = a:e1.col
  else
    return 0
  endif
  return (i0 > i1 ? +1 : -1)
endfunction

" Sorts quickfix
function l9#quickfix#sort()
  call setqflist(sort(getqflist(), 'l9#quickfix#compareEntries'), 'r')
endfunction

" Highlights Quickfix lines by :sign.
" Inspired by errormarker plugin.
" 
" You can customize the highlighting via L9ErrorLine and L9WarningLine
" highlight groups.
function l9#quickfix#placeSign()
  let warnings = []
  let errors = []
  for e in filter(getqflist(), 'v:val.valid')
    let warning = (e.type ==? 'w' || e.text =~? '^\s*warning:')
    call add((warning ? warnings : errors), [e.bufnr, e.lnum])
  endfor
  sign unplace *
  call l9#placeSign('L9WarningLine', '>>', '', warnings)
  call l9#placeSign('L9ErrorLine', '>>', '', errors)
endfunction

highlight default L9ErrorLine   ctermfg=white ctermbg=52 guibg=#5F0000
highlight default L9WarningLine ctermfg=white ctermbg=17 guibg=#00005F

" }}}1
"=============================================================================
" vim: set fdm=marker:

