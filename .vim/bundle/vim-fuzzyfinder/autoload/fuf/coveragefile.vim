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
function fuf#coveragefile#createHandler(base)
  return a:base.concretize(copy(s:handler))
endfunction

"
function fuf#coveragefile#getSwitchOrder()
  return g:fuf_coveragefile_switchOrder
endfunction

"
function fuf#coveragefile#getEditableDataNames()
  return ['coverages']
endfunction

"
function fuf#coveragefile#renewCache()
  let s:cache = {}
endfunction

"
function fuf#coveragefile#requiresOnCommandPre()
  return 0
endfunction

"
function fuf#coveragefile#onInit()
  call fuf#defineLaunchCommand('FufCoverageFile', s:MODE_NAME, '""', [])
  call l9#defineVariableDefault('g:fuf_coveragefile_name', '') " private option
  command! -bang -narg=0        FufCoverageFileRegister call s:registerCoverage()
  command! -bang -narg=?        FufCoverageFileChange call s:changeCoverage(<q-args>)
endfunction

" }}}1
"=============================================================================
" LOCAL FUNCTIONS/VARIABLES {{{1

let s:MODE_NAME = expand('<sfile>:t:r')

"
function s:enumItems()
  let key = join([getcwd(), g:fuf_ignoreCase, g:fuf_coveragefile_exclude,
        \         g:fuf_coveragefile_globPatterns], "\n")
  if !exists('s:cache[key]')
    let s:cache[key] = l9#concat(map(copy(g:fuf_coveragefile_globPatterns),
          \                          'fuf#glob(v:val)'))
    call filter(s:cache[key], 'filereadable(v:val)') " filter out directories
    call map(s:cache[key], 'fuf#makePathItem(fnamemodify(v:val, ":~:."), "", 0)')
    if len(g:fuf_coveragefile_exclude)
      call filter(s:cache[key], 'v:val.word !~ g:fuf_coveragefile_exclude')
    endif
    call fuf#mapToSetSerialIndex(s:cache[key], 1)
    call fuf#mapToSetAbbrWithSnippedWordAsPath(s:cache[key])
  endif
  return s:cache[key]
endfunction

"
function s:registerCoverage()
  let patterns = []
  while 1
    let pattern = l9#inputHl('Question', '[fuf] Glob pattern for coverage (<Esc> and end):',
          \                  '', 'file')
    if pattern !~ '\S'
      break
    endif
    call add(patterns, pattern)
  endwhile
  if empty(patterns)
    call fuf#echoWarning('Canceled')
    return
  endif
  echo '[fuf] patterns: ' . string(patterns)
  let name = l9#inputHl('Question', '[fuf] Coverage name:')
  if name !~ '\S'
    call fuf#echoWarning('Canceled')
    return
  endif
  let coverages = fuf#loadDataFile(s:MODE_NAME, 'coverages')
  call insert(coverages, {'name': name, 'patterns': patterns})
  call fuf#saveDataFile(s:MODE_NAME, 'coverages', coverages)
endfunction

"
function s:createChangeCoverageListener()
  let listener = {}

  function listener.onComplete(name, method)
    call s:changeCoverage(a:name)
  endfunction

  return listener
endfunction

"
function s:changeCoverage(name)
  let coverages = fuf#loadDataFile(s:MODE_NAME, 'coverages')
  if a:name !~ '\S'
    let names = map(copy(coverages), 'v:val.name')
    call fuf#callbackitem#launch('', 0, '>Coverage>', s:createChangeCoverageListener(), names, 0)
    return
  else
    let name = a:name
  endif
  call filter(coverages, 'v:val.name ==# name')
  if empty(coverages)
      call fuf#echoError('Coverage not found: ' . name)
    return
  endif
  call fuf#setOneTimeVariables(
        \   ['g:fuf_coveragefile_globPatterns', coverages[0].patterns],
        \   ['g:fuf_coveragefile_name'        , a:name]
        \ )
  FufCoverageFile
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
  let nameString = (empty(g:fuf_coveragefile_name) ? ''
        \           : '[' . g:fuf_coveragefile_name . ']')
  return fuf#formatPrompt(g:fuf_coveragefile_prompt, self.partialMatching,
        \                 nameString)
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
  return self.items
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
  " NOTE: Comparing filenames is faster than bufnr('^' . fname . '$')
  let bufNamePrev = fnamemodify(bufname(self.bufNrPrev), ':~:.')
  let self.items = copy(s:enumItems())
  call filter(self.items, 'v:val.word !=# bufNamePrev')
endfunction

"
function s:handler.onModeLeavePost(opened)
endfunction

" }}}1
"=============================================================================
" vim: set fdm=marker:
