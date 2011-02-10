"=============================================================================
" Copyright (C) 2010 Takeshi NISHIDA
"
"=============================================================================
" LOAD GUARD {{{1

if !l9#guardScriptLoading(expand('<sfile>:p'), 0, 0, [])
  finish
endif

" }}}1
"=============================================================================
" TEMPORARY VARIABLES {{{1

"
let s:origMap = {}

" set temporary variables
function l9#tempvariables#set(group, name, value)
  if !exists('s:origMap[a:group]')
    let s:origMap[a:group] = {}
  endif
  if !exists('s:origMap[a:group][a:name]')
    let s:origMap[a:group][a:name] = eval(a:name)
  endif
  execute 'let ' . a:name . ' = a:value'
endfunction

" set temporary variables
function l9#tempvariables#setList(group, variables)
  for [name, value] in a:variables
    call l9#tempvariables#set(a:group, name, value)
    unlet value " to avoid E706
  endfor
endfunction

" get temporary variables
function l9#tempvariables#getList(group)
  if !exists('s:origMap[a:group]')
    return []
  endif
  return map(keys(s:origMap[a:group]), '[v:val, eval(v:val)]')
endfunction

" restore original variables and clean up.
function l9#tempvariables#end(group)
  if !exists('s:origMap[a:group]')
    return
  endif
  for [name, value] in items(s:origMap[a:group])
    execute 'let ' . name . ' = value'
    unlet value " to avoid E706
  endfor
  unlet s:origMap[a:group]
endfunction

" }}}1
"=============================================================================
" vim: set fdm=marker:

