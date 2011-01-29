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
function fuf#bookmarkdir#createHandler(base)
  return a:base.concretize(copy(s:handler))
endfunction

"
function fuf#bookmarkdir#getSwitchOrder()
  return g:fuf_bookmarkdir_switchOrder
endfunction

"
function fuf#bookmarkdir#getEditableDataNames()
  return ['items']
endfunction

"
function fuf#bookmarkdir#renewCache()
endfunction

"
function fuf#bookmarkdir#requiresOnCommandPre()
  return 0
endfunction

"
function fuf#bookmarkdir#onInit()
  call fuf#defineLaunchCommand('FufBookmarkDir', s:MODE_NAME, '""', [])
  command! -bang -narg=?        FufBookmarkDirAdd call s:bookmark(<q-args>)
endfunction

" }}}1
"=============================================================================
" LOCAL FUNCTIONS/VARIABLES {{{1

let s:MODE_NAME = expand('<sfile>:t:r')
let s:OPEN_TYPE_DELETE = -1

"
function s:bookmark(word)
  let item = {
        \   'time' : localtime(),
        \ }

  let item.path = l9#inputHl('Question', '[fuf] Directory to bookmark:',
        \              fnamemodify(getcwd(), ':p:~'), 'dir')
  if item.path !~ '\S'
    call fuf#echoWarning('Canceled')
    return
  endif
  let item.word = l9#inputHl('Question', '[fuf] Bookmark as:',
        \               fnamemodify(getcwd(), ':p:~'))
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
  return fuf#formatPrompt(g:fuf_bookmarkdir_prompt, self.partialMatching, '')
endfunction

"
function s:handler.getPreviewHeight()
  return 0
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
  return []
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
        execute ':cd ' . fnameescape(item.path)
    endif
  endif
endfunction

"
function s:handler.onModeEnterPre()
endfunction

"
function s:handler.onModeEnterPost()
  call fuf#defineKeyMappingInHandler(g:fuf_bookmarkdir_keyDelete,
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
