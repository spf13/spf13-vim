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
function fuf#file#createHandler(base)
  return a:base.concretize(copy(s:handler))
endfunction

"
function fuf#file#getSwitchOrder()
  return g:fuf_file_switchOrder
endfunction

"
function fuf#file#getEditableDataNames()
  return []
endfunction

"
function fuf#file#renewCache()
  let s:cache = {}
endfunction

"
function fuf#file#requiresOnCommandPre()
  return 0
endfunction

"
function fuf#file#onInit()
  call fuf#defineLaunchCommand('FufFile'                    , s:MODE_NAME, '""', [])
  call fuf#defineLaunchCommand('FufFileWithFullCwd'         , s:MODE_NAME, 'fnamemodify(getcwd(), '':p'')', [])
  call fuf#defineLaunchCommand('FufFileWithCurrentBufferDir', s:MODE_NAME, 'expand(''%:~:.'')[:-1-len(expand(''%:~:.:t''))]', [])
endfunction

" }}}1
"=============================================================================
" LOCAL FUNCTIONS/VARIABLES {{{1

let s:MODE_NAME = expand('<sfile>:t:r')

"
function s:enumItems(dir)
  let key = join([getcwd(), g:fuf_ignoreCase, g:fuf_file_exclude, a:dir], "\n")
  if !exists('s:cache[key]')
    let s:cache[key] = fuf#enumExpandedDirsEntries(a:dir, g:fuf_file_exclude)
    call fuf#mapToSetSerialIndex(s:cache[key], 1)
    call fuf#mapToSetAbbrWithSnippedWordAsPath(s:cache[key])
  endif
  return s:cache[key]
endfunction

"
function s:enumNonCurrentItems(dir, bufNrPrev, cache)
  let key = a:dir . 'AVOIDING EMPTY KEY'
  if !exists('a:cache[key]')
    " NOTE: Comparing filenames is faster than bufnr('^' . fname . '$')
    let bufNamePrev = bufname(a:bufNrPrev)
    let a:cache[key] =
          \ filter(copy(s:enumItems(a:dir)), 'v:val.word !=# bufNamePrev')
  endif
  return a:cache[key]
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
  return fuf#formatPrompt(g:fuf_file_prompt, self.partialMatching, '')
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
  return s:enumNonCurrentItems(
        \ fuf#splitPath(a:patternPrimary).head, self.bufNrPrev, self.cache)
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
  let self.cache = {}
endfunction

"
function s:handler.onModeLeavePost(opened)
endfunction

" }}}1
"=============================================================================
" vim: set fdm=marker:
