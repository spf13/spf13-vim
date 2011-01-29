"=============================================================================
" Copyright (c) 2007-2010 Takeshi NISHIDA
"
"=============================================================================
" LOAD GUARD {{{1

if !l9#guardScriptLoading(expand('<sfile>:p'), 0, 0, [])
  finish
endif

" }}}1
"=============================================================================
" GLOBAL FUNCTIONS {{{1

"
function fuf#dir#createHandler(base)
  return a:base.concretize(copy(s:handler))
endfunction

"
function fuf#dir#getSwitchOrder()
  return g:fuf_dir_switchOrder
endfunction

"
function fuf#dir#getEditableDataNames()
  return []
endfunction

"
function fuf#dir#renewCache()
  let s:cache = {}
endfunction

"
function fuf#dir#requiresOnCommandPre()
  return 0
endfunction

"
function fuf#dir#onInit()
  call fuf#defineLaunchCommand('FufDir'                    , s:MODE_NAME, '""', [])
  call fuf#defineLaunchCommand('FufDirWithFullCwd'         , s:MODE_NAME, 'fnamemodify(getcwd(), '':p'')', [])
  call fuf#defineLaunchCommand('FufDirWithCurrentBufferDir', s:MODE_NAME, 'expand(''%:~:.'')[:-1-len(expand(''%:~:.:t''))]', [])
endfunction

" }}}1
"=============================================================================
" LOCAL FUNCTIONS/VARIABLES {{{1

let s:MODE_NAME = expand('<sfile>:t:r')

"
function s:enumItems(dir)
  let key = getcwd() . g:fuf_ignoreCase . g:fuf_dir_exclude . "\n" . a:dir
  if !exists('s:cache[key]')
    let s:cache[key] = fuf#enumExpandedDirsEntries(a:dir, g:fuf_dir_exclude)
    call filter(s:cache[key], 'v:val.word =~# ''[/\\]$''')
    if isdirectory(a:dir)
      call insert(s:cache[key], fuf#makePathItem(a:dir . '.', '', 0))
    endif
    call fuf#mapToSetSerialIndex(s:cache[key], 1)
    call fuf#mapToSetAbbrWithSnippedWordAsPath(s:cache[key])
  endif
  return s:cache[key]
endfunction

" }}}1
"=============================================================================
" s:handler {{{1

let s:handler = {}

"
function s:handler.getModeName()
  return s:MODE_NAME
endfunction

"
function s:handler.getPrompt()
  return fuf#formatPrompt(g:fuf_dir_prompt, self.partialMatching, '')
endfunction

"
function s:handler.getPreviewHeight()
  return g:fuf_previewHeight
endfunction

"
function s:handler.isOpenable(enteredPattern)
  return a:enteredPattern =~# '[^/\\]$'
endfunction

"
function s:handler.makePatternSet(patternBase)
  return fuf#makePatternSet(a:patternBase, 's:interpretPrimaryPatternForPathTail',
        \                   self.partialMatching)
endfunction

"
function s:handler.makePreviewLines(word, count)
  return fuf#makePreviewLinesAround(
        \ fuf#glob(fnamemodify(a:word, ':p') . '*'),
        \ [], a:count, self.getPreviewHeight())
  return 
endfunction

"
function s:handler.getCompleteItems(patternPrimary)
  return s:enumItems(fuf#splitPath(a:patternPrimary).head)
endfunction

"
function s:handler.onOpen(word, mode)
  execute ':cd ' . fnameescape(a:word)
endfunction

"
function s:handler.onModeEnterPre()
endfunction

"
function s:handler.onModeEnterPost()
endfunction

"
function s:handler.onModeLeavePost(opened)
endfunction

" }}}1
"=============================================================================
" vim: set fdm=marker:
