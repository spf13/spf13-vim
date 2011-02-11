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
function fuf#callbackitem#createHandler(base)
  return a:base.concretize(copy(s:handler))
endfunction

"
function fuf#callbackitem#getSwitchOrder()
  return -1
endfunction

"
function fuf#callbackitem#getEditableDataNames()
  return []
endfunction

"
function fuf#callbackitem#renewCache()
endfunction

"
function fuf#callbackitem#requiresOnCommandPre()
  return 0
endfunction

"
function fuf#callbackitem#onInit()
endfunction

"
function fuf#callbackitem#launch(initialPattern, partialMatching, prompt, listener, items, forPath)
  let s:prompt = (empty(a:prompt) ? '>' : a:prompt)
  let s:listener = a:listener
  let s:forPath = a:forPath
  let s:items = copy(a:items)
  if s:forPath
    call map(s:items, 'fuf#makePathItem(v:val, "", 1)')
    call fuf#mapToSetSerialIndex(s:items, 1)
    call fuf#mapToSetAbbrWithSnippedWordAsPath(s:items)
  else
    call map(s:items, 'fuf#makeNonPathItem(v:val, "")')
    call fuf#mapToSetSerialIndex(s:items, 1)
    call map(s:items, 'fuf#setAbbrWithFormattedWord(v:val, 1)')
  endif
  call fuf#launch(s:MODE_NAME, a:initialPattern, a:partialMatching)
endfunction

" }}}1
"=============================================================================
" LOCAL FUNCTIONS/VARIABLES {{{1

let s:MODE_NAME = expand('<sfile>:t:r')

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
  if s:forPath
    return g:fuf_previewHeight
  endif
  return 0
endfunction

"
function s:handler.isOpenable(enteredPattern)
  return 1
endfunction

"
function s:handler.makePatternSet(patternBase)
  let parser = (s:forPath
        \       ? 's:interpretPrimaryPatternForPath'
        \       : 's:interpretPrimaryPatternForNonPath')
  return fuf#makePatternSet(a:patternBase, parser, self.partialMatching)
endfunction

"
function s:handler.makePreviewLines(word, count)
  if s:forPath
    return fuf#makePreviewLinesForFile(a:word, a:count, self.getPreviewHeight())
  endif
  return []
endfunction

"
function s:handler.getCompleteItems(patternPrimary)
  return s:items
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
