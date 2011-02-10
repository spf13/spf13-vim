"=============================================================================
" Copyright (C) 2009-2010 Takeshi NISHIDA
"
" GetLatestVimScripts: 3252 1 :AutoInstall: L9
"=============================================================================
" LOAD GUARD {{{1

if !l9#guardScriptLoading(expand('<sfile>:p'), 702, 0, [])
  finish
endif

" }}}1
"=============================================================================
" OPTIONS: {{{1

call l9#defineVariableDefault('g:l9_balloonly', 'balloonly.exe')

" }}}1
"=============================================================================
" ASSERTION: {{{1

" This command has effect only if $L9_DEBUG is non-zero.
" Used as follows:
"   L9Assert a:i > 0
" This command can't interpret script-local variables directly.
"   NG: L9Assert s:a == 1
"   OK: execute 'L9Assert ' . s:a . ' == 1'
"
if $L9_DEBUG
  command -nargs=* L9Assert call eval((<args>) ? 0 : s:handleFailedAssersion(<q-args>))

  function s:handleFailedAssersion(expr)
    echoerr '[L9Assert] Assersion failure: ' . a:expr
    if input('[L9Assert] Continue? (Y/N) ', 'Y') !=? 'Y'
      throw 'L9Assert ' . a:expr
    endif
  endfunction

else
  command -nargs=* L9Assert :
endif

" }}}1
"=============================================================================
" TIMER: {{{1

" These commands have effect only if $L9_TIMER is non-zero.
" Used as follows:
"   L9Timer foo
"     ... (1)
"   L9Timer bar
"     ... (2)
"   L9TimerStop
"     ...
"   L9TimerDump  <- shows each elapsed time of (1) and (2)
"
if $L9_TIMER
  command -nargs=1 L9Timer call s:timerBegin(<q-args>)
  command -nargs=0 L9TimerStop call s:timerStop()
  command -nargs=0 L9TimerDump call s:timerDump()

  let s:timerData = []
  let s:timerTagMaxLen = 0

  function s:timerBegin(tag)
    L9TimerStop
    let s:timerCurrent = {'tag': strftime('%c ') . a:tag . ' ', 'time': reltime()}
    let s:timerTagMaxLen = max([len(s:timerCurrent.tag), s:timerTagMaxLen])
  endfunction

  function s:timerStop()
    if !exists('s:timerCurrent')
      return
    endif
    let s:timerCurrent.time = reltimestr(reltime(s:timerCurrent.time))
    call add(s:timerData, s:timerCurrent)
    unlet s:timerCurrent
  endfunction

  function s:timerDump()
    L9TimerStop
    let lines = map(s:timerData, 'v:val.tag . repeat(" ", s:timerTagMaxLen - len(v:val.tag)) . v:val.time')
    call l9#tempbuffer#openReadOnly('[l9-timer]', '', lines, 0, 0, 0, {})
    let s:timerData = []
    let s:timerTagMaxLen = 0
  endfunction

else
  command -nargs=1 L9Timer :
  command -nargs=0 L9TimerStop :
  command -nargs=0 L9TimerDump :
endif

" }}}1
"=============================================================================
" GREP BUFFER: {{{1

" Grep for current buffer by l9#grepBuffers()
" Used as :L9GrepBuffer/pattern
command -nargs=? L9GrepBuffer    call l9#grepBuffers(<q-args>, [bufnr('%')])

" Grep for all buffers by l9#grepBuffers()
" Used as :L9GrepBufferAll/pattern
command -nargs=? L9GrepBufferAll call l9#grepBuffers(<q-args>, range(1, bufnr('$')))

" }}}1
"=============================================================================
" vim: set fdm=marker:
