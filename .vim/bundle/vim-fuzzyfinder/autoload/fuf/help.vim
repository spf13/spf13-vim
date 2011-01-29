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
function fuf#help#createHandler(base)
  return a:base.concretize(copy(s:handler))
endfunction

"
function fuf#help#getSwitchOrder()
  return g:fuf_help_switchOrder
endfunction

"
function fuf#help#getEditableDataNames()
  return []
endfunction

"
function fuf#help#renewCache()
  let s:cache = {}
endfunction

"
function fuf#help#requiresOnCommandPre()
  return 0
endfunction

"
function fuf#help#onInit()
  call fuf#defineLaunchCommand('FufHelp'              , s:MODE_NAME, '""', [])
  call fuf#defineLaunchCommand('FufHelpWithCursorWord', s:MODE_NAME, 'expand(''<cword>'')', [])
endfunction

" }}}1
"=============================================================================
" LOCAL FUNCTIONS/VARIABLES {{{1

let s:MODE_NAME = expand('<sfile>:t:r')

"
function s:getCurrentHelpTagFiles()
  let prefix = 'doc' . l9#getPathSeparator()
  let tagFiles = split(globpath(&runtimepath, prefix . 'tags'   ), "\n")
        \      + split(globpath(&runtimepath, prefix . 'tags-??'), "\n")
  return sort(map(tagFiles, 'fnamemodify(v:val, ":p")'))
endfunction

"
function s:parseHelpTagEntry(line, tagFile)
  let elements = split(a:line, "\t")
  if len(elements) != 3 || elements[0][0] ==# '!'
    return {}
  endif
  let suffix = matchstr(a:tagFile, '-\zs..$')
  if empty(suffix) 
    let suffix = '@en'
  else
    let suffix = '@' . suffix
  endif
  let dir = fnamemodify(a:tagFile, ':h') . l9#getPathSeparator()
  return {
        \   'word'   : elements[0] . suffix,
        \   'path'   : dir . elements[1],
        \   'pattern': elements[2][1:],
        \ }
endfunction

"
function s:getHelpTagEntries(tagFile)
  let names = map(l9#readFile(a:tagFile), 's:parseHelpTagEntry(v:val, a:tagFile)')
  return filter(names, '!empty(v:val)')
endfunction

"
function s:parseHelpTagFiles(tagFiles, key)
  let cacheName = 'cache-' . l9#hash224(a:key)
  let cacheTime = fuf#getDataFileTime(s:MODE_NAME, cacheName)
  if cacheTime != -1 && fuf#countModifiedFiles(a:tagFiles, cacheTime) == 0
    return fuf#loadDataFile(s:MODE_NAME, cacheName)
  endif
  let items = l9#unique(l9#concat(map(copy(a:tagFiles), 's:getHelpTagEntries(v:val)')))
  let items = map(items, 'extend(v:val, fuf#makeNonPathItem(v:val.word, ""))')
  call fuf#mapToSetSerialIndex(items, 1)
  let items = map(items, 'fuf#setAbbrWithFormattedWord(v:val, 1)')
  call fuf#saveDataFile(s:MODE_NAME, cacheName, items)
  return items
endfunction

"
function s:enumHelpTags(tagFiles)
  if !len(a:tagFiles)
    return []
  endif
  let key = join([g:fuf_ignoreCase] + a:tagFiles, "\n")
  if !exists('s:cache[key]') || fuf#countModifiedFiles(a:tagFiles, s:cache[key].time)
    let s:cache[key] = {
          \   'time'  : localtime(),
          \   'items' : s:parseHelpTagFiles(a:tagFiles, key)
          \ }
  endif
  return s:cache[key].items
endfunction

"
function s:getMatchingIndex(lines, pattern)
  if empty(a:pattern)
    return -1
  endif
  for i in range(len(a:lines))
    if stridx(a:lines[i], a:pattern) >= 0
      return i
    endif
  endfor
  return -1
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
  return fuf#formatPrompt(g:fuf_help_prompt, self.partialMatching, '')
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
  let items = filter(copy(s:enumHelpTags(self.tagFiles)), 'v:val.word ==# a:word')
  if empty(items)
    return []
  endif
  let lines = fuf#getFileLines(items[0].path)
  let index = s:getMatchingIndex(lines, items[0].pattern)
  return [items[0].path . ':'] + fuf#makePreviewLinesAround(
        \ lines, (index < 0 ? [] : [index]), a:count, self.getPreviewHeight() - 1)
endfunction

"
function s:handler.getCompleteItems(patternPrimary)
  return s:enumHelpTags(self.tagFiles)
endfunction

"
function s:handler.onOpen(word, mode)
  call fuf#openHelp(a:word, a:mode)
endfunction

"
function s:handler.onModeEnterPre()
  let self.tagFiles = s:getCurrentHelpTagFiles()
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
