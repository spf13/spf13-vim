"=============================================================================
" Copyright (c) 2007-2009 Takeshi NISHIDA
"
"=============================================================================
" LOAD GUARD {{{1

if exists('g:loaded_autoload_fuf_changelist') || v:version < 702
  finish
endif
let g:loaded_autoload_fuf_changelist = 1

" }}}1
"=============================================================================
" GLOBAL FUNCTIONS {{{1

"
function fuf#changelist#createHandler(base)
  return a:base.concretize(copy(s:handler))
endfunction

"
function fuf#changelist#getSwitchOrder()
  return g:fuf_changelist_switchOrder
endfunction

"
function fuf#changelist#renewCache()
endfunction

"
function fuf#changelist#requiresOnCommandPre()
  return 0
endfunction

"
function fuf#changelist#onInit()
  call fuf#defineLaunchCommand('FufChangeList', s:MODE_NAME, '""')
endfunction

" }}}1
"=============================================================================
" LOCAL FUNCTIONS/VARIABLES {{{1

let s:MODE_NAME = expand('<sfile>:t:r')

"
function s:getChangesLines()
  redir => result
  :silent changes
  redir END
  return split(result, "\n")
endfunction

"
function s:parseChangesLine(line)
  " return matchlist(a:line, '^\(.\)\s\+\(\d\+\)\s\(.*\)$')
  let elements = matchlist(a:line, '\v^(.)\s*(\d+)\s+(\d+)\s+(\d+)\s*(.*)$')
  if empty(elements)
    return {}
  endif
  return  {
        \   'prefix': elements[1],
        \   'count' : elements[2],
        \   'lnum'  : elements[3],
        \   'text'  : printf('|%d:%d|%s', elements[3], elements[4], elements[5]),
        \ }
endfunction

"
function s:makeItem(line)
  let parsed = s:parseChangesLine(a:line)
  if empty(parsed)
    return {}
  endif
  let item = fuf#makeNonPathItem(parsed.text, '')
  let item.abbrPrefix = parsed.prefix
  let item.lnum = parsed.lnum
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
  return fuf#formatPrompt(g:fuf_changelist_prompt, self.partialMatching)
endfunction

"
function s:handler.getPreviewHeight()
  return g:fuf_previewHeight
endfunction

"
function s:handler.targetsPath()
  return 0
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
  let lines = fuf#getFileLines(self.bufNrPrev)
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
  for line in reverse(s:getChangesLines())
    if stridx(line, '>') == 0
      let older = 1
    endif
    let parsed = s:parseChangesLine(line)
    if !empty(parsed) && parsed.text ==# a:word
      if parsed.count != 0
        execute 'normal! ' . parsed.count . (older ? 'g;' : 'g,') . 'zvzz'
      endif
      break
    endif
  endfor
endfunction

"
function s:handler.onModeEnterPre()
  let self.items = s:getChangesLines()
endfunction

"
function s:handler.onModeEnterPost()
  call map(self.items, 's:makeItem(v:val)')
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

