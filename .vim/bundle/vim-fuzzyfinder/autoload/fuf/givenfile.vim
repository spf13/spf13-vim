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
function fuf#givenfile#createHandler(base)
  return a:base.concretize(copy(s:handler))
endfunction

"
function fuf#givenfile#getSwitchOrder()
  return -1
endfunction

"
function fuf#givenfile#getEditableDataNames()
  return []
endfunction

"
function fuf#givenfile#renewCache()
endfunction

"
function fuf#givenfile#requiresOnCommandPre()
  return 0
endfunction

"
function fuf#givenfile#onInit()
endfunction

"
function fuf#givenfile#launch(initialPattern, partialMatching, prompt, items)
  let s:prompt = (empty(a:prompt) ? '>' : a:prompt)
  let s:items = map(copy(a:items), 'fuf#makePathItem(v:val, "", 0)')
  call fuf#mapToSetSerialIndex(s:items, 1)
  call map(s:items, 'fuf#setAbbrWithFormattedWord(v:val, 1)')
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
  return g:fuf_previewHeight
endfunction

"
function s:handler.isOpenable(enteredPattern)
  return 1
endfunction

"
function s:handler.makePatternSet(patternBase)
  return fuf#makePatternSet(a:patternBase, 's:interpretPrimaryPatternForPath',
        \                   self.partialMatching)
endfunction

"
function s:handler.makePreviewLines(word, count)
  return fuf#makePreviewLinesForFile(a:word, a:count, self.getPreviewHeight())
endfunction

"
function s:handler.getCompleteItems(patternPrimary)
  return s:items
endfunction

"
function s:handler.onOpen(word, mode)
  call fuf#openFile(a:word, a:mode, g:fuf_reuseWindow)
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
