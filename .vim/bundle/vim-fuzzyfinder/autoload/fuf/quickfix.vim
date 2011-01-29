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
function fuf#quickfix#createHandler(base)
  return a:base.concretize(copy(s:handler))
endfunction

"
function fuf#quickfix#getSwitchOrder()
  return g:fuf_quickfix_switchOrder
endfunction

"
function fuf#quickfix#getEditableDataNames()
  return []
endfunction

"
function fuf#quickfix#renewCache()
endfunction

"
function fuf#quickfix#requiresOnCommandPre()
  return 0
endfunction

"
function fuf#quickfix#onInit()
  call fuf#defineLaunchCommand('FufQuickfix', s:MODE_NAME, '""', [])
endfunction

" }}}1
"=============================================================================
" LOCAL FUNCTIONS/VARIABLES {{{1

let s:MODE_NAME = expand('<sfile>:t:r')

"
function s:getJumpsLines()
  redir => result
  :silent jumps
  redir END
  return split(result, "\n")
endfunction

"
function s:parseJumpsLine(line)
  return matchlist(a:line, '^\(.\)\s\+\(\d\+\)\s\(.*\)$')
endfunction

"
function s:makeItem(qfItem)
  if !a:qfItem.valid
    return {}
  endif
  let item = fuf#makeNonPathItem(
        \ printf('%s|%d:%d|%s', bufname(a:qfItem.bufnr), a:qfItem.lnum,
        \        a:qfItem.col, matchstr(a:qfItem.text, '\s*\zs.*\S'))
        \ , '')
  let item.bufnr = a:qfItem.bufnr
  let item.lnum = a:qfItem.lnum
  return item
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
  return fuf#formatPrompt(g:fuf_quickfix_prompt, self.partialMatching, '')
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
  return fuf#makePatternSet(a:patternBase, 's:interpretPrimaryPatternForNonPath',
        \                   self.partialMatching)
endfunction

"
function s:handler.makePreviewLines(word, count)
  let items = filter(copy(self.items), 'v:val.word ==# a:word')
  if empty(items)
    return []
  endif
  let lines = fuf#getFileLines(items[0].bufnr)
  return fuf#makePreviewLinesAround(
        \ lines, [items[0].lnum - 1], a:count, self.getPreviewHeight())
endfunction

"
function s:handler.getCompleteItems(patternPrimary)
  return self.items
endfunction

"
function s:handler.onOpen(word, mode)
  call fuf#prejump(a:mode)
  call filter(self.items, 'v:val.word ==# a:word')
  if !empty(self.items)
    execute 'cc ' . self.items[0].index
  endif
endfunction

"
function s:handler.onModeEnterPre()
endfunction

"
function s:handler.onModeEnterPost()
  let self.items = getqflist()
  call map(self.items, 's:makeItem(v:val)')
  call fuf#mapToSetSerialIndex(self.items, 1)
  call filter(self.items, 'exists("v:val.word")')
  call map(self.items, 'fuf#setAbbrWithFormattedWord(v:val, 1)')
endfunction

"
function s:handler.onModeLeavePost(opened)
endfunction

" }}}1
"=============================================================================
" vim: set fdm=marker:

