"=============================================================================
" Copyright (C) 2009-2010 Takeshi NISHIDA
"
"=============================================================================
" LOAD GUARD {{{1

if !l9#guardScriptLoading(expand('<sfile>:p'), 0, 0, ['has("python")'])
  finish
endif

" }}}1
"=============================================================================
" ASYNC EXECUTE {{{1

"
function s:checkKey(key)
  if a:key =~ '\n' || a:key !~ '\S'
    throw "Asyncer: Invalid key: " . a:key
  endif
endfunction

" 
function l9#async#execute(key, cmd, cwd, input, appends)
  call s:checkKey(a:key)
  python asyncer.execute('a:key', 'a:cmd', 'a:cwd', 'a:input', 'a:appends')
endfunction

"
function l9#async#read(key)
  call s:checkKey(a:key)
  redir => result
  silent python asyncer.print_output('a:key')
  redir END
  " NOTE: "\n" is somehow inserted by redir.
  return (result[0] ==# "\n" ? result[1:] : result)
endfunction

"
function l9#async#listWorkers()
  redir => result
  silent python asyncer.print_worker_keys()
  redir END
  return split(result, "\n")
endfunction

"
function l9#async#listActiveWorkers()
  redir => result
  silent python asyncer.print_active_worker_keys()
  redir END
  return split(result, "\n")
endfunction

" }}}1
"=============================================================================
" INITIALIZATION {{{1

let s:ASYNC_PY_PATH = fnamemodify(expand('<sfile>:p:h'), ':p') . 'async.py'

pyfile `=s:ASYNC_PY_PATH`
python asyncer = Asyncer()

" }}}1
"=============================================================================
" vim: set fdm=marker:


