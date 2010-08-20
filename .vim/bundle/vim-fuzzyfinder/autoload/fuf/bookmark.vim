"=============================================================================
" Copyright (c) 2007-2009 Takeshi NISHIDA
"
"=============================================================================
" LOAD GUARD {{{1

if exists('g:loaded_autoload_fuf_bookmark') || v:version < 702
  finish
endif
let g:loaded_autoload_fuf_bookmark = 1

" }}}1
"=============================================================================
" GLOBAL FUNCTIONS {{{1

"
function fuf#bookmark#createHandler(base)
  return a:base.concretize(copy(s:handler))
endfunction

"
function fuf#bookmark#getSwitchOrder()
  return g:fuf_bookmark_switchOrder
endfunction

"
function fuf#bookmark#renewCache()
endfunction

"
function fuf#bookmark#requiresOnCommandPre()
  return 0
endfunction

"
function fuf#bookmark#onInit()
  call fuf#defineLaunchCommand('FufBookmark', s:MODE_NAME, '""')
  command! -bang -narg=?        FufAddBookmark               call s:bookmarkHere(<q-args>)
  command! -bang -narg=0 -range FufAddBookmarkAsSelectedText call s:bookmarkHere(s:getSelectedText())
endfunction

" }}}1
"=============================================================================
" LOCAL FUNCTIONS/VARIABLES {{{1

let s:MODE_NAME = expand('<sfile>:t:r')
let s:OPEN_TYPE_DELETE = -1

"
function s:getSelectedText()
  let reg_ = [@", getregtype('"')]
  let regA = [@a, getregtype('a')]
  if mode() =~# "[vV\<C-v>]"
    silent normal! "aygv
  else
    let pos = getpos('.')
    silent normal! gv"ay
    call setpos('.', pos)
  endif
  let text = @a
  call setreg('"', reg_[0], reg_[1])
  call setreg('a', regA[0], regA[1])
  return text
endfunction

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
  for [l0, l1] in map(range(0, g:fuf_bookmark_searchRange),
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
    call fuf#echoWithHl('Can''t bookmark this buffer.', 'WarningMsg')
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
  let item.word = fuf#inputHl('Bookmark as:', item.word, 'Question')
  if item.word !~ '\S'
    call fuf#echoWithHl('Canceled', 'WarningMsg')
    return
  endif
  let info = fuf#loadInfoFile(s:MODE_NAME)
  call insert(info.data, item)
  call fuf#saveInfoFile(s:MODE_NAME, info)
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
  return fuf#formatPrompt(g:fuf_bookmark_prompt, self.partialMatching)
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
  let item = s:findItem(self.info.data, a:word)
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
    call filter(self.info.data, 'v:val.word !=# a:word')
    call fuf#saveInfoFile(self.getModeName(), self.info)
    let self.reservedMode = self.getModeName()
    return
  else
    let item = s:findItem(self.info.data, a:word)
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
  call fuf#defineKeyMappingInHandler(g:fuf_bookmark_keyDelete,
        \                            'onCr(' . s:OPEN_TYPE_DELETE . ', 0)')
  let self.items = copy(self.info.data)
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
