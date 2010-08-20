"=============================================================================
" Copyright (c) 2007-2009 Takeshi NISHIDA
"
"=============================================================================
" LOAD GUARD {{{1

if exists('g:loaded_autoload_fuf_taggedfile') || v:version < 702
  finish
endif
let g:loaded_autoload_fuf_taggedfile = 1

" }}}1
"=============================================================================
" GLOBAL FUNCTIONS {{{1

"
function fuf#taggedfile#createHandler(base)
  return a:base.concretize(copy(s:handler))
endfunction

"
function fuf#taggedfile#getSwitchOrder()
  return g:fuf_taggedfile_switchOrder
endfunction

"
function fuf#taggedfile#renewCache()
  let s:cache = {}
endfunction

"
function fuf#taggedfile#requiresOnCommandPre()
  return 0
endfunction

"
function fuf#taggedfile#onInit()
  call fuf#defineLaunchCommand('FufTaggedFile', s:MODE_NAME, '""')
endfunction

" }}}1
"=============================================================================
" LOCAL FUNCTIONS/VARIABLES {{{1

let s:MODE_NAME = expand('<sfile>:t:r')

"
function s:getTaggedFileList(tagfile)
  execute 'cd ' . fnamemodify(a:tagfile, ':h')
  let result = map(readfile(a:tagfile), 'matchstr(v:val, ''^[^!\t][^\t]*\t\zs[^\t]\+'')')
  call map(readfile(a:tagfile), 'fnamemodify(v:val, ":p")')
  cd -
  call map(readfile(a:tagfile), 'fnamemodify(v:val, ":~:.")')
  return filter(result, 'v:val =~# ''[^/\\ ]$''')
endfunction

"
function s:parseTagFiles(tagFiles, key)
  if !empty(g:fuf_taggedfile_cache_dir)
    if !isdirectory(expand(g:fuf_taggedfile_cache_dir))
      call mkdir(expand(g:fuf_taggedfile_cache_dir), 'p')
    endif
    " NOTE: fnamemodify('a/b', ':p') returns 'a/b/' if the directory exists.
    let cacheFile = fnamemodify(g:fuf_taggedfile_cache_dir, ':p') . fuf#hash224(a:key)
    if filereadable(cacheFile) && fuf#countModifiedFiles(a:tagFiles, getftime(cacheFile)) == 0
      return map(readfile(cacheFile), 'eval(v:val)')
    endif
  endif
  let items = fuf#unique(fuf#concat(map(copy(a:tagFiles), 's:getTaggedFileList(v:val)')))
  call map(items, 'fuf#makePathItem(v:val, "", 0)')
  call fuf#mapToSetSerialIndex(items, 1)
  call fuf#mapToSetAbbrWithSnippedWordAsPath(items)
  if !empty(g:fuf_taggedfile_cache_dir)
    call writefile(map(copy(items), 'string(v:val)'), cacheFile)
  endif
  return items
endfunction

"
function s:enumTaggedFiles(tagFiles)
  if !len(a:tagFiles)
    return []
  endif
  let key = join([getcwd(), g:fuf_ignoreCase] + a:tagFiles, "\n")
  if !exists('s:cache[key]') || fuf#countModifiedFiles(a:tagFiles, s:cache[key].time)
    let s:cache[key] = {
          \   'time'  : localtime(),
          \   'items' : s:parseTagFiles(a:tagFiles, key)
          \ }
  endif
  return s:cache[key].items
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
  return fuf#formatPrompt(g:fuf_taggedfile_prompt, self.partialMatching)
endfunction

"
function s:handler.getPreviewHeight()
  return g:fuf_previewHeight
endfunction

"
function s:handler.targetsPath()
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
  let self.tagFiles = fuf#getCurrentTagFiles()
endfunction

"
function s:handler.onModeEnterPost()
  " NOTE: Comparing filenames is faster than bufnr()
  let bufNamePrev = fnamemodify(bufname(self.bufNrPrev), ':~')
  " NOTE: Don't do this in onModeEnterPre()
  "       because that should return in a short time.
  let self.items = copy(s:enumTaggedFiles(self.tagFiles))
  call filter(self.items, 'v:val.word !=# bufNamePrev')
endfunction

"
function s:handler.onModeLeavePost(opened)
endfunction

" }}}1
"=============================================================================
" vim: set fdm=marker:
