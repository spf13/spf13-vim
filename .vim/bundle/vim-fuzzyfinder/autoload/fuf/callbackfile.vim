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
function fuf#callbackfile#createHandler(base)
  return a:base.concretize(copy(s:handler))
endfunction

"
function fuf#callbackfile#getSwitchOrder()
  return -1
endfunction

"
function fuf#callbackfile#getEditableDataNames()
  return []
endfunction

"
function fuf#callbackfile#renewCache()
  let s:cache = {}
endfunction

"
function fuf#callbackfile#requiresOnCommandPre()
  return 0
endfunction

"
function fuf#callbackfile#onInit()
endfunction

"
function fuf#callbackfile#launch(initialPattern, partialMatching, prompt, exclude, listener)
  let s:prompt = (empty(a:prompt) ? '>' : a:prompt)
  let s:exclude = a:exclude
  let s:listener = a:listener
  call fuf#launch(s:MODE_NAME, a:initialPattern, a:partialMatching)
endfunction

" }}}1
"=============================================================================
" LOCAL FUNCTIONS/VARIABLES {{{1

let s:MODE_NAME = expand('<sfile>:t:r')

"
function s:enumItems(dir)
  let key = getcwd() . g:fuf_ignoreCase . s:exclude . "\n" . a:dir
  if !exists('s:cache[key]')
    let s:cache[key] = fuf#enumExpandedDirsEntries(a:dir, s:exclude)
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
  return fuf#formatPrompt(s:prompt, self.partialMatching, '')
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
  return fuf#makePreviewLinesForFile(a:word, a:count, self.getPreviewHeight())
endfunction

"
function s:handler.getCompleteItems(patternPrimary)
  let items = copy(s:enumItems(fuf#splitPath(a:patternPrimary).head))
  return filter(items, 'bufnr("^" . v:val.word . "$") != self.bufNrPrev')
endfunction

"
function s:handler.onOpen(word, mode)
  call s:listener.onComplete(a:word, a:mode)
endfunction

"
function s:handler.onModeEnterPre()
endfunction

"
function s:handler.onModeEnterPost()
endfunction

"
function s:handler.onModeLeavePost(opened)
  if !a:opened && exists('s:listener.onAbort()')
    call s:listener.onAbort()
  endif
endfunction

" }}}1
"=============================================================================
" vim: set fdm=marker:
