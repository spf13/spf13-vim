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
function fuf#jumplist#createHandler(base)
  return a:base.concretize(copy(s:handler))
endfunction

"
function fuf#jumplist#getSwitchOrder()
  return g:fuf_jumplist_switchOrder
endfunction

"
function fuf#jumplist#getEditableDataNames()
  return []
endfunction

"
function fuf#jumplist#renewCache()
endfunction

"
function fuf#jumplist#requiresOnCommandPre()
  return 0
endfunction

"
function fuf#jumplist#onInit()
  call fuf#defineLaunchCommand('FufJumpList', s:MODE_NAME, '""', [])
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
function s:parseJumpsLine(line, bufnrPrev)
  "return matchlist(a:line, '^\(.\)\s\+\(\d\+\)\s\(.*\)$')
  let elements = matchlist(a:line, '\v^(.)\s*(\d+)\s+(\d+)\s+(\d+)\s*(.*)$')
  if empty(elements)
    return {}
  endif
  let linePrevBuffer = join(getbufline(a:bufnrPrev, elements[3]))
  if stridx(linePrevBuffer, elements[5]) >= 0
    let fname = bufname(a:bufnrPrev)
    let text  = elements[5]
  else
    let fname = elements[5]
    let text  = join(getbufline('^' . elements[5] . '$', elements[3]))
  endif
  return  {
        \   'prefix': elements[1],
        \   'count' : elements[2],
        \   'lnum'  : elements[3],
        \   'fname' : fname,
        \   'text'  : printf('%s|%d:%d|%s', fname, elements[3], elements[4], text),
        \ }
endfunction

"
function s:makeItem(line, bufnrPrev)
  let parsed = s:parseJumpsLine(a:line, a:bufnrPrev)
  if empty(parsed)
    return {}
  endif
  let item = fuf#makeNonPathItem(parsed.text, '')
  let item.abbrPrefix = parsed.prefix
  let item.lnum = parsed.lnum
  let item.fname = parsed.fname
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
  return fuf#formatPrompt(g:fuf_jumplist_prompt, self.partialMatching, '')
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
  let lines = fuf#getFileLines(items[0].fname)
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
  let older = 0
  for line in reverse(s:getJumpsLines())
    if stridx(line, '>') == 0
      let older = 1
    endif
    let parsed = s:parseJumpsLine(line, self.bufNrPrev)
    if !empty(parsed) && parsed.text ==# a:word
      if parsed.count != 0
        execute 'normal! ' . parsed.count . (older ? "\<C-o>" : "\<C-i>") . 'zvzz'
      endif
      break
    endif
  endfor
endfunction

"
function s:handler.onModeEnterPre()
  let self.items = s:getJumpsLines()
endfunction

"
function s:handler.onModeEnterPost()
  call map(self.items, 's:makeItem(v:val, self.bufNrPrev)')
  call filter(self.items, '!empty(v:val)')
  call reverse(self.items)
  call fuf#mapToSetSerialIndex(self.items, 1)
  call map(self.items, 'fuf#setAbbrWithFormattedWord(v:val, 1)')
endfunction

"
function s:handler.onModeLeavePost(opened)
endfunction

" }}}1
"=============================================================================
" vim: set fdm=marker:

