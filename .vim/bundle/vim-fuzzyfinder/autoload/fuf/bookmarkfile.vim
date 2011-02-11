"=============================================================================
" Copyright (c) 2010 Takeshi NISHIDA
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
function fuf#bookmarkfile#createHandler(base)
  return a:base.concretize(copy(s:handler))
endfunction

"
function fuf#bookmarkfile#getSwitchOrder()
  return g:fuf_bookmarkfile_switchOrder
endfunction

"
function fuf#bookmarkfile#getEditableDataNames()
  return ['items']
endfunction

"
function fuf#bookmarkfile#renewCache()
endfunction

"
function fuf#bookmarkfile#requiresOnCommandPre()
  return 0
endfunction

"
function fuf#bookmarkfile#onInit()
  call fuf#defineLaunchCommand('FufBookmarkFile', s:MODE_NAME, '""', [])
  command! -bang -narg=?        FufBookmarkFileAdd               call s:bookmarkHere(<q-args>)
  command! -bang -narg=0 -range FufBookmarkFileAddAsSelectedText call s:bookmarkHere(l9#getSelectedText())
endfunction

" }}}1
"=============================================================================
" LOCAL FUNCTIONS/VARIABLES {{{1

let s:MODE_NAME = expand('<sfile>:t:r')
let s:OPEN_TYPE_DELETE = -1

" opens a:path and jumps to the line matching to a:pattern from a:lnum within
" a:range. if not found, jumps to a:lnum.
function s:jumpToBookmark(path, mode, pattern, lnum)
  call fuf#openFile(a:path, a:mode, g:fuf_reuseWindow)
  call cursor(s:getMatchingLineNumber(getline(1, '$'), a:pattern, a:lnum), 0)
  normal! zvzz
endfunction

"
function s:getMatchingLineNumber(lines, pattern, lnumBegin)
  let l = min([a:lnumBegin, len(a:lines)])
  for [l0, l1] in map(range(0, g:fuf_bookmarkfile_searchRange),
        \             '[l + v:val, l - v:val]')
    if l0 <= len(a:lines) && a:lines[l0 - 1] =~# a:pattern
      return l0
    elseif l1 >= 0 && a:lines[l1 - 1] =~# a:pattern
      return l1
    endif
  endfor
  return l
endfunction

"
function s:getLinePattern(lnum)
  return '\C\V\^' . escape(getline(a:lnum), '\') . '\$'
endfunction

"
function s:bookmarkHere(word)
  if !empty(&buftype) || expand('%') !~ '\S'
    call fuf#echoWarning('Can''t bookmark this buffer.')
    return
  endif
  let item = {
        \   'word' : (a:word =~# '\S' ? substitute(a:word, '\n', ' ', 'g')
        \                             : pathshorten(expand('%:p:~')) . '|' . line('.') . '| ' . getline('.')),
        \   'path' : expand('%:p'),
        \   'lnum' : line('.'),
        \   'pattern' : s:getLinePattern(line('.')),
        \   'time' : localtime(),
        \ }
  let item.word = l9#inputHl('Question', '[fuf] Bookmark as:', item.word)
  if item.word !~ '\S'
    call fuf#echoWarning('Canceled')
    return
  endif
  let items = fuf#loadDataFile(s:MODE_NAME, 'items')
  call insert(items, item)
  call fuf#saveDataFile(s:MODE_NAME, 'items', items)
endfunction

"
function s:findItem(items, word)
  for item in a:items
    if item.word ==# a:word
      return item
    endif
  endfor
  return {}
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
  return fuf#formatPrompt(g:fuf_bookmarkfile_prompt, self.partialMatching, '')
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
  let item = s:findItem(fuf#loadDataFile(s:MODE_NAME, 'items'), a:word)
  let lines = fuf#getFileLines(item.path)
  if empty(lines)
    return []
  endif
  let index = s:getMatchingLineNumber(lines, item.pattern, item.lnum) - 1
  return fuf#makePreviewLinesAround(
        \ lines, [index], a:count, self.getPreviewHeight())
endfunction

"
function s:handler.getCompleteItems(patternPrimary)
  return self.items
endfunction

"
function s:handler.onOpen(word, mode)
  if a:mode ==# s:OPEN_TYPE_DELETE
    let items = fuf#loadDataFile(s:MODE_NAME, 'items')
    call filter(items, 'v:val.word !=# a:word')
    call fuf#saveDataFile(s:MODE_NAME, 'items', items)
    let self.reservedMode = self.getModeName()
    return
  else
    let item = s:findItem(fuf#loadDataFile(s:MODE_NAME, 'items'), a:word)
    if !empty(item)
        call s:jumpToBookmark(item.path, a:mode, item.pattern, item.lnum)
    endif
  endif
endfunction

"
function s:handler.onModeEnterPre()
endfunction

"
function s:handler.onModeEnterPost()
  call fuf#defineKeyMappingInHandler(g:fuf_bookmarkfile_keyDelete,
        \                            'onCr(' . s:OPEN_TYPE_DELETE . ')')
  let self.items = fuf#loadDataFile(s:MODE_NAME, 'items')
  call map(self.items, 'fuf#makeNonPathItem(v:val.word, strftime(g:fuf_timeFormat, v:val.time))')
  call fuf#mapToSetSerialIndex(self.items, 1)
  call map(self.items, 'fuf#setAbbrWithFormattedWord(v:val, 1)')
endfunction

"
function s:handler.onModeLeavePost(opened)
endfunction

" }}}1
"=============================================================================
" vim: set fdm=marker:
