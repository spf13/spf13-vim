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
function fuf#buffer#createHandler(base)
  return a:base.concretize(copy(s:handler))
endfunction

"
function fuf#buffer#getSwitchOrder()
  return g:fuf_buffer_switchOrder
endfunction

"
function fuf#buffer#getEditableDataNames()
  return []
endfunction

"
function fuf#buffer#renewCache()
endfunction

"
function fuf#buffer#requiresOnCommandPre()
  return 0
endfunction

"
function fuf#buffer#onInit()
  call fuf#defineLaunchCommand('FufBuffer', s:MODE_NAME, '""', [])
  augroup fuf#buffer
    autocmd!
    autocmd BufEnter     * call s:updateBufTimes()
    autocmd BufWritePost * call s:updateBufTimes()
  augroup END
endfunction

" }}}1
"=============================================================================
" LOCAL FUNCTIONS/VARIABLES {{{1

let s:MODE_NAME = expand('<sfile>:t:r')
let s:OPEN_TYPE_DELETE = -1

let s:bufTimes = {}

"
function s:updateBufTimes()
  let s:bufTimes[bufnr('%')] = localtime()
endfunction

"
function s:makeItem(nr)
  let fname = (empty(bufname(a:nr))
        \      ? '[No Name]'
        \      : fnamemodify(bufname(a:nr), ':p:~:.'))
  let time = (exists('s:bufTimes[a:nr]') ? s:bufTimes[a:nr] : 0)
  let item = fuf#makePathItem(fname, strftime(g:fuf_timeFormat, time), 0)
  let item.index = a:nr
  let item.bufNr = a:nr
  let item.time = time
  let item.abbrPrefix = s:getBufIndicator(a:nr) . ' '
  return item
endfunction

"
function s:getBufIndicator(bufNr)
  if !getbufvar(a:bufNr, '&modifiable')
    return '[-]'
  elseif getbufvar(a:bufNr, '&modified')
    return '[+]'
  elseif getbufvar(a:bufNr, '&readonly')
    return '[R]'
  else
    return '   '
  endif
endfunction

"
function s:compareTimeDescending(i1, i2)
  return a:i1.time == a:i2.time ? 0 : a:i1.time > a:i2.time ? -1 : +1
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
  return fuf#formatPrompt(g:fuf_buffer_prompt, self.partialMatching, '')
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
  let item = s:findItem(self.items, a:word)
  if empty(item)
    return []
  endif
  return fuf#makePreviewLinesForFile(item.bufNr, a:count, self.getPreviewHeight())
endfunction

"
function s:handler.getCompleteItems(patternPrimary)
  return self.items
endfunction

"
function s:handler.onOpen(word, mode)
  " not use bufnr(a:word) in order to handle unnamed buffer
  let item = s:findItem(self.items, a:word)
  if empty(item)
    " do nothing
  elseif a:mode ==# s:OPEN_TYPE_DELETE
    execute item.bufNr . 'bdelete'
    let self.reservedMode = self.getModeName()
  else
    call fuf#openBuffer(item.bufNr, a:mode, g:fuf_reuseWindow)
  endif
endfunction

"
function s:handler.onModeEnterPre()
endfunction

"
function s:handler.onModeEnterPost()
  call fuf#defineKeyMappingInHandler(g:fuf_buffer_keyDelete,
        \                            'onCr(' . s:OPEN_TYPE_DELETE . ')')
  let self.items = range(1, bufnr('$'))
  call filter(self.items, 'buflisted(v:val) && v:val != self.bufNrPrev && v:val != bufnr("%")')
  call map(self.items, 's:makeItem(v:val)')
  if g:fuf_buffer_mruOrder
    call sort(self.items, 's:compareTimeDescending')
    call fuf#mapToSetSerialIndex(self.items, 1)
  endif
  let self.items = fuf#mapToSetAbbrWithSnippedWordAsPath(self.items)
endfunction

"
function s:handler.onModeLeavePost(opened)
endfunction

" }}}1
"=============================================================================
" vim: set fdm=marker:
