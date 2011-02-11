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


" returns list of paths.
" An argument for glob() is normalized in order to avoid a bug on Windows.
function fuf#glob(expr)
  " Substitutes "\", because on Windows, "**\" doesn't include ".\",
  " but "**/" include "./". I don't know why.
  return split(glob(substitute(a:expr, '\', '/', 'g')), "\n")
endfunction

"
function fuf#countModifiedFiles(files, time)
  return len(filter(copy(a:files), 'getftime(expand(v:val)) > a:time'))
endfunction

"
function fuf#getCurrentTagFiles()
  return sort(filter(map(tagfiles(), 'fnamemodify(v:val, '':p'')'), 'filereadable(v:val)'))
endfunction

"
function fuf#mapToSetSerialIndex(in, offset)
  for i in range(len(a:in))
    let a:in[i].index = i + a:offset
  endfor
  return a:in
endfunction

"
function fuf#updateMruList(mrulist, newItem, maxItem, exclude)
  let result = copy(a:mrulist)
  let result = filter(result,'v:val.word !=# a:newItem.word')
  let result = insert(result, a:newItem)
  if len(a:exclude)
    let result = filter(result, 'v:val.word !~ a:exclude')
  endif
  return result[0 : a:maxItem - 1]
endfunction

" takes suffix number. if no digits, returns -1
function fuf#suffixNumber(str)
  let s = matchstr(a:str, '\d\+$')
  return (len(s) ? str2nr(s) : -1)
endfunction

" "foo/bar/buz/hoge" -> { head: "foo/bar/buz/", tail: "hoge" }
function fuf#splitPath(path)
  let head = matchstr(a:path, '^.*[/\\]')
  return  {
        \   'head' : head,
        \   'tail' : a:path[strlen(head):]
        \ }
endfunction

" "foo/.../bar/...hoge" -> "foo/.../bar/../../hoge"
function fuf#expandTailDotSequenceToParentDir(pattern)
  return substitute(a:pattern, '^\(.*[/\\]\)\?\zs\.\(\.\+\)\ze[^/\\]*$',
        \           '\=repeat(".." . l9#getPathSeparator(), len(submatch(2)))', '')
endfunction

"
function fuf#formatPrompt(prompt, partialMatching, otherString)
  let indicator = escape((a:partialMatching ? '!' : '') . a:otherString, '\')
  return substitute(a:prompt, '[]', indicator, 'g')
endfunction

"
function fuf#getFileLines(file)
  let bufnr = (type(a:file) ==# type(0) ? a:file : bufnr('^' . a:file . '$'))
  let lines = getbufline(bufnr, 1, '$')
  if !empty(lines)
    return lines
  endif
  return l9#readFile(a:file)
endfunction

"
function fuf#makePreviewLinesAround(lines, indices, page, maxHeight)
  let index = ((empty(a:indices) ? 0 : a:indices[0])
        \ + a:page * a:maxHeight) % len(a:lines)
  if empty(a:lines) || a:maxHeight <= 0
    return []
  endif
  let beg = max([0, index - a:maxHeight / 2])
  let end = min([beg + a:maxHeight, len(a:lines)])
  let beg = max([0, end - a:maxHeight])
  let lines = []
  for i in range(beg, end - 1)
    let mark = (count(a:indices, i) ? '>' : ' ')
    call add(lines, printf('%s%4d ', mark, i + 1) . a:lines[i])
  endfor
  return lines
endfunction

" a:file: a path string or a buffer number
function fuf#makePreviewLinesForFile(file, count, maxHeight)
  let lines = fuf#getFileLines(a:file)
  if empty(lines)
    return []
  endif
  let bufnr = (type(a:file) ==# type(0) ? a:file : bufnr('^' . a:file . '$'))
  if exists('s:bufferCursorPosMap[bufnr]')
    let indices = [s:bufferCursorPosMap[bufnr][1] - 1]
  else
    let indices = []
  endif
  return fuf#makePreviewLinesAround(
        \ lines, indices, a:count, a:maxHeight)
endfunction

"
function fuf#echoWarning(msg)
  call l9#echoHl('WarningMsg', a:msg, '[fuf] ', 1)
endfunction

"
function fuf#echoError(msg)
  call l9#echoHl('ErrorMsg', a:msg, '[fuf] ', 1)
endfunction

"
function fuf#openBuffer(bufNr, mode, reuse)
  if a:reuse && ((a:mode ==# s:OPEN_TYPE_SPLIT &&
        \         l9#moveToBufferWindowInCurrentTabpage(a:bufNr)) ||
        \        (a:mode ==# s:OPEN_TYPE_VSPLIT &&
        \         l9#moveToBufferWindowInCurrentTabpage(a:bufNr)) ||
        \        (a:mode ==# s:OPEN_TYPE_TAB &&
        \         l9#moveToBufferWindowInOtherTabpage(a:bufNr)))
    return
  endif
  execute printf({
        \   s:OPEN_TYPE_CURRENT : '%sbuffer'          ,
        \   s:OPEN_TYPE_SPLIT   : '%ssbuffer'         ,
        \   s:OPEN_TYPE_VSPLIT  : 'vertical %ssbuffer',
        \   s:OPEN_TYPE_TAB     : 'tab %ssbuffer'     ,
        \ }[a:mode], a:bufNr)
endfunction

"
function fuf#openFile(path, mode, reuse)
  let bufNr = bufnr('^' . a:path . '$')
  if bufNr > -1
    call fuf#openBuffer(bufNr, a:mode, a:reuse)
  else
    execute {
          \   s:OPEN_TYPE_CURRENT : 'edit '   ,
          \   s:OPEN_TYPE_SPLIT   : 'split '  ,
          \   s:OPEN_TYPE_VSPLIT  : 'vsplit ' ,
          \   s:OPEN_TYPE_TAB     : 'tabedit ',
          \ }[a:mode] . fnameescape(fnamemodify(a:path, ':~:.'))
  endif
endfunction

"
function fuf#openTag(tag, mode)
  execute {
        \   s:OPEN_TYPE_CURRENT : 'tjump '          ,
        \   s:OPEN_TYPE_SPLIT   : 'stjump '         ,
        \   s:OPEN_TYPE_VSPLIT  : 'vertical stjump ',
        \   s:OPEN_TYPE_TAB     : 'tab stjump '     ,
        \ }[a:mode] . a:tag
endfunction

"
function fuf#openHelp(tag, mode)
  execute {
        \   s:OPEN_TYPE_CURRENT : 'help '         ,
        \   s:OPEN_TYPE_SPLIT   : 'help '         ,
        \   s:OPEN_TYPE_VSPLIT  : 'vertical help ',
        \   s:OPEN_TYPE_TAB     : 'tab help '   ,
        \ }[a:mode] . a:tag
endfunction

"
function fuf#prejump(mode)
  execute {
        \   s:OPEN_TYPE_CURRENT : ''         ,
        \   s:OPEN_TYPE_SPLIT   : 'split'    ,
        \   s:OPEN_TYPE_VSPLIT  : 'vsplit'   ,
        \   s:OPEN_TYPE_TAB     : 'tab split',
        \ }[a:mode] 
endfunction

"
function fuf#compareRanks(i1, i2)
  if exists('a:i1.ranks') && exists('a:i2.ranks')
    for i in range(min([len(a:i1.ranks), len(a:i2.ranks)]))
      if     a:i1.ranks[i] > a:i2.ranks[i]
        return +1
      elseif a:i1.ranks[i] < a:i2.ranks[i]
        return -1
      endif
    endfor
  endif
  return 0
endfunction

"
function fuf#makePathItem(fname, menu, appendsDirSuffix)
  let pathPair = fuf#splitPath(a:fname)
  let dirSuffix = (a:appendsDirSuffix && isdirectory(expand(a:fname))
        \          ? l9#getPathSeparator()
        \          : '')
  return {
        \   'word'              : a:fname . dirSuffix,
        \   'wordForPrimaryHead': s:toLowerForIgnoringCase(pathPair.head),
        \   'wordForPrimaryTail': s:toLowerForIgnoringCase(pathPair.tail),
        \   'wordForBoundary'   : s:toLowerForIgnoringCase(s:getWordBoundaries(pathPair.tail)),
        \   'wordForRefining'   : s:toLowerForIgnoringCase(a:fname . dirSuffix),
        \   'wordForRank'       : s:toLowerForIgnoringCase(pathPair.tail),
        \   'menu'              : a:menu,
        \ }
endfunction

"
function fuf#makeNonPathItem(word, menu)
  let wordL = s:toLowerForIgnoringCase(a:word)
  return {
        \   'word'           : a:word,
        \   'wordForPrimary' : wordL,
        \   'wordForBoundary': s:toLowerForIgnoringCase(s:getWordBoundaries(a:word)),
        \   'wordForRefining': wordL,
        \   'wordForRank'    : wordL,
        \   'menu'           : a:menu,
        \ }
endfunction

"
function fuf#makePatternSet(patternBase, interpreter, partialMatching)
  let MakeMatchingExpr = function(a:partialMatching
        \                         ? 's:makePartialMatchingExpr'
        \                         : 's:makeFuzzyMatchingExpr')
  let [primary; refinings] = split(a:patternBase, g:fuf_patternSeparator, 1)
  let elements = call(a:interpreter, [primary])
  let primaryExprs  = map(elements.matchingPairs, 'MakeMatchingExpr(v:val[0], v:val[1])')
  let refiningExprs = map(refinings, 's:makeRefiningExpr(v:val)')
  return  {
        \   'primary'       : elements.primary,
        \   'primaryForRank': elements.primaryForRank,
        \   'filteringExpr' : join(primaryExprs + refiningExprs, ' && '),
        \ }
endfunction

"
function fuf#enumExpandedDirsEntries(dir, exclude)
  let entries = fuf#glob(a:dir . '*') + fuf#glob(a:dir . '.*')
  " removes "*/." and "*/.."
  call filter(entries, 'v:val !~ ''\v(^|[/\\])\.\.?$''')
  call map(entries, 'fuf#makePathItem(v:val, "", 1)')
  if len(a:exclude)
    call filter(entries, 'v:val.word !~ a:exclude')
  endif
  return entries
endfunction

"
function fuf#mapToSetAbbrWithSnippedWordAsPath(items)
  let maxLenStats = {}
  call map(a:items, 's:makeFileAbbrInfo(v:val, maxLenStats)')
  let snippedHeads =
        \ map(maxLenStats, 's:getSnippedHead(v:key[: -2], v:val)')
  return map(a:items, 's:setAbbrWithFileAbbrData(v:val, snippedHeads)')
endfunction

"
function fuf#setAbbrWithFormattedWord(item, abbrIndex)
  let lenMenu = (exists('a:item.menu') ? len(a:item.menu) + 2 : 0)
  let abbrPrefix = (exists('a:item.abbrPrefix') ? a:item.abbrPrefix : '')
  let a:item.abbr = abbrPrefix . a:item.word
  if a:abbrIndex
    let a:item.abbr = printf('%4d: ', a:item.index) . a:item.abbr
  endif
  let a:item.abbr = l9#snipTail(a:item.abbr, g:fuf_maxMenuWidth - lenMenu, s:ABBR_SNIP_MASK)
  return a:item
endfunction

"
function s:onCommandPre()
  for m in filter(copy(fuf#getModeNames()), 'fuf#{v:val}#requiresOnCommandPre()')
      call fuf#{m}#onCommandPre(getcmdtype() . getcmdline())
  endfor
  " lets last entry become the newest in the history
  call histadd(getcmdtype(), getcmdline())
  " this is not mapped again (:help recursive_mapping)
  return "\<CR>"
endfunction

"
let s:modeNames = []

"
function fuf#addMode(modeName)
  if count(g:fuf_modesDisable, a:modeName) > 0
    return
  endif
  call add(s:modeNames, a:modeName)
  call fuf#{a:modeName}#renewCache()
  call fuf#{a:modeName}#onInit()
  if fuf#{a:modeName}#requiresOnCommandPre()
    " cnoremap has a problem, which doesn't expand cabbrev.
    cmap <silent> <expr> <CR> <SID>onCommandPre()
  endif
endfunction

"
function fuf#getModeNames()
  return s:modeNames
endfunction

"
function fuf#defineLaunchCommand(CmdName, modeName, prefixInitialPattern, tempVars)
  if empty(a:tempVars)
    let preCmd = ''
  else
    let preCmd = printf('call l9#tempvariables#setList(%s, %s) | ',
          \             string(s:TEMP_VARIABLES_GROUP), string(a:tempVars))
  endif
  execute printf('command! -range -bang -narg=? %s %s call fuf#launch(%s, %s . <q-args>, len(<q-bang>))',
        \        a:CmdName, preCmd, string(a:modeName), a:prefixInitialPattern)
endfunction

"
function fuf#defineKeyMappingInHandler(key, func)
  " hacks to be able to use feedkeys().
  execute printf(
        \ 'inoremap <buffer> <silent> %s <C-r>=fuf#getRunningHandler().%s ? "" : ""<CR>',
        \ a:key, a:func)
endfunction

"
let s:oneTimeVariables = []

" 
function fuf#setOneTimeVariables(...)
  let s:oneTimeVariables += a:000
endfunction

"
function fuf#launch(modeName, initialPattern, partialMatching)
  if exists('s:runningHandler')
    call fuf#echoWarning('FuzzyFinder is running.')
  endif
  if count(fuf#getModeNames(), a:modeName) == 0
    echoerr 'This mode is not available: ' . a:modeName
    return
  endif
  let s:runningHandler = fuf#{a:modeName}#createHandler(copy(s:handlerBase))
  let s:runningHandler.stats = fuf#loadDataFile(s:runningHandler.getModeName(), 'stats')
  let s:runningHandler.partialMatching = a:partialMatching
  let s:runningHandler.bufNrPrev = bufnr('%')
  let s:runningHandler.lastCol = -1
  let s:runningHandler.windowRestoringCommand = winrestcmd()
  call s:runningHandler.onModeEnterPre()
  " NOTE: updatetime is set, because in Buffer-Tag mode on Vim 7.3 on Windows,
  " Vim keeps from triggering CursorMovedI for updatetime after system() is
  " called. I don't know why.
  call fuf#setOneTimeVariables(
        \  ['&completeopt', 'menuone'],
        \  ['&ignorecase', 0],
        \  ['&updatetime', 10],
        \ )
  if s:runningHandler.getPreviewHeight() > 0
    call fuf#setOneTimeVariables(
          \ ['&cmdheight', s:runningHandler.getPreviewHeight() + 1])
  endif
  call l9#tempvariables#setList(s:TEMP_VARIABLES_GROUP, s:oneTimeVariables)
  let s:oneTimeVariables = []
  call s:activateFufBuffer()
  augroup FufLocal
    autocmd!
    autocmd CursorMovedI <buffer>        call s:runningHandler.onCursorMovedI()
    autocmd InsertLeave  <buffer> nested call s:runningHandler.onInsertLeave()
  augroup END
  for [key, func] in [
        \   [ g:fuf_keyOpen          , 'onCr(' . s:OPEN_TYPE_CURRENT . ')' ],
        \   [ g:fuf_keyOpenSplit     , 'onCr(' . s:OPEN_TYPE_SPLIT   . ')' ],
        \   [ g:fuf_keyOpenVsplit    , 'onCr(' . s:OPEN_TYPE_VSPLIT  . ')' ],
        \   [ g:fuf_keyOpenTabpage   , 'onCr(' . s:OPEN_TYPE_TAB     . ')' ],
        \   [ '<BS>'                 , 'onBs()'                            ],
        \   [ '<C-h>'                , 'onBs()'                            ],
        \   [ '<C-w>'                , 'onDeleteWord()'                    ],
        \   [ g:fuf_keyPreview       , 'onPreviewBase(1)'                  ],
        \   [ g:fuf_keyNextMode      , 'onSwitchMode(+1)'                  ],
        \   [ g:fuf_keyPrevMode      , 'onSwitchMode(-1)'                  ],
        \   [ g:fuf_keySwitchMatching, 'onSwitchMatching()'                ],
        \   [ g:fuf_keyPrevPattern   , 'onRecallPattern(+1)'               ],
        \   [ g:fuf_keyNextPattern   , 'onRecallPattern(-1)'               ],
        \ ]
    call fuf#defineKeyMappingInHandler(key, func)
  endfor
  " Starts Insert mode and makes CursorMovedI event now. Command prompt is
  " needed to forces a completion menu to update every typing.
  call setline(1, s:runningHandler.getPrompt() . a:initialPattern)
  call s:runningHandler.onModeEnterPost()
  call feedkeys("A", 'n') " startinsert! does not work in InsertLeave event handler
  redraw
endfunction

"
function fuf#loadDataFile(modeName, dataName)
  if !s:dataFileAvailable
    return []
  endif
  let lines = l9#readFile(l9#concatPaths([g:fuf_dataDir, a:modeName, a:dataName]))
  return map(lines, 'eval(v:val)')
endfunction

" 
function fuf#saveDataFile(modeName, dataName, items)
  if !s:dataFileAvailable
    return -1
  endif
  let lines = map(copy(a:items), 'string(v:val)')
  return l9#writeFile(lines, l9#concatPaths([g:fuf_dataDir, a:modeName, a:dataName]))
endfunction

" 
function fuf#getDataFileTime(modeName, dataName)
  if !s:dataFileAvailable
    return -1
  endif
  return getftime(expand(l9#concatPaths([g:fuf_dataDir, a:modeName, a:dataName])))
endfunction

"
function s:createDataBufferListener(dataFile)
  let listener = { 'dataFile': a:dataFile }

  function listener.onWrite(lines)
    let [modeName, dataName] = split(self.dataFile, l9#getPathSeparator())
    let items = map(filter(a:lines, '!empty(v:val)'), 'eval(v:val)')
    call fuf#saveDataFile(modeName, dataName, items)
    echo "Data files updated"
    return 1
  endfunction

  return listener
endfunction

"
function s:createEditDataListener()
  let listener = {}

  function listener.onComplete(dataFile, method)
    let bufName = '[fuf-info]'
    let lines = l9#readFile(l9#concatPaths([g:fuf_dataDir, a:dataFile]))
    call l9#tempbuffer#openWritable(bufName, 'vim', lines, 0, 0, 0,
          \                         s:createDataBufferListener(a:dataFile))
  endfunction

  return listener
endfunction

"
function s:getEditableDataFiles(modeName)
  let dataFiles = fuf#{a:modeName}#getEditableDataNames()
  call filter(dataFiles, 'fuf#getDataFileTime(a:modeName, v:val) != -1')
  return map(dataFiles, 'l9#concatPaths([a:modeName, v:val])')
endfunction

"
function fuf#editDataFile()
  let dataFiles = map(copy(fuf#getModeNames()), 's:getEditableDataFiles(v:val)')
  let dataFiles = l9#concat(dataFiles)
  call fuf#callbackitem#launch('', 0, '>Mode>', s:createEditDataListener(), dataFiles, 0)
endfunction

" 
function fuf#getRunningHandler()
  return s:runningHandler
endfunction

" 
function fuf#onComplete(findstart, base)
  return s:runningHandler.onComplete(a:findstart, a:base)
endfunction

" }}}1
"=============================================================================
" LOCAL FUNCTIONS/VARIABLES {{{1

let s:TEMP_VARIABLES_GROUP = expand('<sfile>:p')
let s:ABBR_SNIP_MASK = '...'
let s:OPEN_TYPE_CURRENT = 1
let s:OPEN_TYPE_SPLIT   = 2
let s:OPEN_TYPE_VSPLIT  = 3
let s:OPEN_TYPE_TAB     = 4

" a:pattern: 'str' -> '\V\.\*s\.\*t\.\*r\.\*'
function s:makeFuzzyMatchingExpr(target, pattern)
  let wi = ''
  for c in split(a:pattern, '\zs')
    if wi =~# '[^*?]$' && c !~ '[*?]'
      let wi .= '*'
    endif
    let wi .= c
  endfor
  return s:makePartialMatchingExpr(a:target, wi)
endfunction

" a:pattern: 'str' -> '\Vstr'
"            'st*r' -> '\Vst\.\*r'
function s:makePartialMatchingExpr(target, pattern)
  let patternMigemo = s:makeAdditionalMigemoPattern(a:pattern)
  if a:pattern !~ '[*?]' && empty(patternMigemo)
    " NOTE: stridx is faster than regexp matching
    return 'stridx(' . a:target . ', ' . string(a:pattern) . ') >= 0'
  endif
  return a:target . ' =~# ' .
        \ string(l9#convertWildcardToRegexp(a:pattern)) . patternMigemo
endfunction

" 
function s:makeRefiningExpr(pattern)
  if g:fuf_fuzzyRefining
    let expr = s:makeFuzzyMatchingExpr('v:val.wordForRefining', a:pattern)
  else
    let expr = s:makePartialMatchingExpr('v:val.wordForRefining', a:pattern)
  endif
  if a:pattern =~# '\D'
    return expr
  else
    return '(' . expr . ' || v:val.index == ' . string(a:pattern) . ')'
  endif
endfunction

" 
function s:makeAdditionalMigemoPattern(pattern)
  if !g:fuf_useMigemo || a:pattern =~# '[^\x01-\x7e]'
    return ''
  endif
  return '\|\m' . substitute(migemo(a:pattern), '\\_s\*', '.*', 'g')
endfunction

"
function s:interpretPrimaryPatternForPathTail(pattern)
  let pattern = fuf#expandTailDotSequenceToParentDir(a:pattern)
  let pairL = fuf#splitPath(s:toLowerForIgnoringCase(pattern))
  return {
        \   'primary'       : pattern,
        \   'primaryForRank': pairL.tail,
        \   'matchingPairs' : [['v:val.wordForPrimaryTail', pairL.tail],],
        \ }
endfunction

"
function s:interpretPrimaryPatternForPath(pattern)
  let pattern = fuf#expandTailDotSequenceToParentDir(a:pattern)
  let patternL = s:toLowerForIgnoringCase(pattern)
  let pairL = fuf#splitPath(patternL)
  if g:fuf_splitPathMatching
    let matches = [
          \     ['v:val.wordForPrimaryHead', pairL.head],
          \     ['v:val.wordForPrimaryTail', pairL.tail],
          \   ]
  else
    let matches = [
          \     ['v:val.wordForPrimaryHead . v:val.wordForPrimaryTail', patternL],
          \   ]
  endif
  return {
        \   'primary'       : pattern,
        \   'primaryForRank': pairL.tail,
        \   'matchingPairs' : matches,
        \ }
endfunction

"
function s:interpretPrimaryPatternForNonPath(pattern)
  let patternL = s:toLowerForIgnoringCase(a:pattern)
  return {
        \   'primary'       : a:pattern,
        \   'primaryForRank': patternL,
        \   'matchingPairs' : [['v:val.wordForPrimary', patternL],],
        \ }
endfunction

"
function s:getWordBoundaries(word)
  return substitute(a:word, '\a\zs\l\+\|\zs\A', '', 'g')
endfunction

"
function s:toLowerForIgnoringCase(str)
  return (g:fuf_ignoreCase ? tolower(a:str) : a:str)
endfunction

"
function s:setRanks(item, pattern, exprBoundary, stats)
  "let word2 = substitute(a:eval_word, '\a\zs\l\+\|\zs\A', '', 'g')
  let a:item.ranks = [
        \   s:evaluateLearningRank(a:item.word, a:stats),
        \   -s:scoreSequentialMatching(a:item.wordForRank, a:pattern),
        \   -s:scoreBoundaryMatching(a:item.wordForBoundary, 
        \                            a:pattern, a:exprBoundary),
        \   a:item.index,
        \ ]
  return a:item
endfunction

" 
function s:evaluateLearningRank(word, stats)
  for i in range(len(a:stats))
    if a:stats[i].word ==# a:word
      return i
    endif
  endfor
  return len(a:stats)
endfunction

" range of return value is [0.0, 1.0]
function s:scoreSequentialMatching(word, pattern)
  if empty(a:pattern)
    return str2float('0.0')
  endif
  let pos = stridx(a:word, a:pattern)
  if pos < 0
    return str2float('0.0')
  endif
  let lenRest = len(a:word) - len(a:pattern) - pos
  return str2float(pos == 0 ? '0.5' : '0.0') + str2float('0.5') / (lenRest + 1)
endfunction

" range of return value is [0.0, 1.0]
function s:scoreBoundaryMatching(wordForBoundary, pattern, exprBoundary)
  if empty(a:pattern)
    return str2float('0.0')
  endif
  if !eval(a:exprBoundary)
    return 0
  endif
  return (s:scoreSequentialMatching(a:wordForBoundary, a:pattern) + 1) / 2
endfunction

"
function s:highlightPrompt(prompt)
  syntax clear
  execute printf('syntax match %s /^\V%s/', g:fuf_promptHighlight, escape(a:prompt, '\/'))
endfunction

"
function s:highlightError()
  syntax clear
  syntax match Error  /^.*$/
endfunction

"
function s:expandAbbrevMap(pattern, abbrevMap)
  let result = [a:pattern]
  for [pattern, subs] in items(a:abbrevMap)
    let exprs = result
    let result = []
    for expr in exprs
      let result += map(copy(subs), 'substitute(expr, pattern, escape(v:val, ''\''), "g")')
    endfor
  endfor
  return l9#unique(result)
endfunction

"
function s:makeFileAbbrInfo(item, maxLenStats)
  let head = matchstr(a:item.word, '^.*[/\\]\ze.')
  let a:item.abbr = { 'head' : head,
        \             'tail' : a:item.word[strlen(head):],
        \             'key' : head . '.',
        \             'prefix' : printf('%4d: ', a:item.index), }
  if exists('a:item.abbrPrefix')
    let a:item.abbr.prefix .= a:item.abbrPrefix
  endif
  let len = len(a:item.abbr.prefix) + len(a:item.word) +
        \   (exists('a:item.menu') ? len(a:item.menu) + 2 : 0)
  if !exists('a:maxLenStats[a:item.abbr.key]') || len > a:maxLenStats[a:item.abbr.key]
    let a:maxLenStats[a:item.abbr.key] = len
  endif
  return a:item
endfunction

"
function s:getSnippedHead(head, baseLen)
  return l9#snipMid(a:head, len(a:head) + g:fuf_maxMenuWidth - a:baseLen, s:ABBR_SNIP_MASK)
endfunction

"
function s:setAbbrWithFileAbbrData(item, snippedHeads)
  let lenMenu = (exists('a:item.menu') ? len(a:item.menu) + 2 : 0)
  let abbr = a:item.abbr.prefix . a:snippedHeads[a:item.abbr.key] . a:item.abbr.tail
  let a:item.abbr = l9#snipTail(abbr, g:fuf_maxMenuWidth - lenMenu, s:ABBR_SNIP_MASK)
  return a:item
endfunction

"
let s:FUF_BUF_NAME = '[fuf]'

"
function s:activateFufBuffer()
  " lcd . : To avoid the strange behavior that unnamed buffer changes its cwd
  "         if 'autochdir' was set on.
  lcd .
  let cwd = getcwd()
  call l9#tempbuffer#openScratch(s:FUF_BUF_NAME, 'fuf', [], 1, 0, 1, {})
  resize 1 " for issue #21 
  " lcd ... : countermeasure against auto-cd script
  lcd `=cwd`
  setlocal nocursorline   " for highlighting
  setlocal nocursorcolumn " for highlighting
  setlocal omnifunc=fuf#onComplete
  redraw " for 'lazyredraw'
  if exists(':AcpLock')
    AcpLock
  elseif exists(':AutoComplPopLock')
    AutoComplPopLock
  endif
endfunction

"
function s:deactivateFufBuffer()
  if exists(':AcpUnlock')
    AcpUnlock
  elseif exists(':AutoComplPopUnlock')
    AutoComplPopUnlock
  endif
  call l9#tempbuffer#close(s:FUF_BUF_NAME)
endfunction

" }}}1
"=============================================================================
" s:handlerBase {{{1

let s:handlerBase = {}

"-----------------------------------------------------------------------------
" PURE VIRTUAL FUNCTIONS {{{2
"
" "
" s:handler.getModeName()
" 
" "
" s:handler.getPrompt()
" 
" "
" s:handler.getCompleteItems(patternSet)
" 
" "
" s:handler.onOpen(word, mode)
" 
" " Before entering FuzzyFinder buffer. This function should return in a short time.
" s:handler.onModeEnterPre()
"
" " After entering FuzzyFinder buffer.
" s:handler.onModeEnterPost()
"
" " After leaving FuzzyFinder buffer.
" s:handler.onModeLeavePost(opened)
"
" }}}2
"-----------------------------------------------------------------------------

"
function s:handlerBase.concretize(deriv)
  call extend(self, a:deriv, 'error')
  return self
endfunction

"
function s:handlerBase.addStat(pattern, word)
  let stat = { 'pattern' : a:pattern, 'word' : a:word }
  call filter(self.stats, 'v:val !=# stat')
  call insert(self.stats, stat)
  let self.stats = self.stats[0 : g:fuf_learningLimit - 1]
endfunction

"
function s:handlerBase.getMatchingCompleteItems(patternBase)
  let MakeMatchingExpr = function(self.partialMatching
        \                         ? 's:makePartialMatchingExpr'
        \                         : 's:makeFuzzyMatchingExpr')
  let patternSet = self.makePatternSet(a:patternBase)
  let exprBoundary = s:makeFuzzyMatchingExpr('a:wordForBoundary', patternSet.primaryForRank)
  let stats = filter(
        \ copy(self.stats), 'v:val.pattern ==# patternSet.primaryForRank')
  let items = self.getCompleteItems(patternSet.primary)
  " NOTE: In order to know an excess, plus 1 to limit number
  let items = l9#filterWithLimit(
        \ items, patternSet.filteringExpr, g:fuf_enumeratingLimit + 1)
  return map(items,
        \ 's:setRanks(v:val, patternSet.primaryForRank, exprBoundary, stats)')
endfunction

"
function s:handlerBase.onComplete(findstart, base)
  if a:findstart
    return 0
  elseif  !self.existsPrompt(a:base)
    return []
  endif
  call s:highlightPrompt(self.getPrompt())
  let items = []
  for patternBase in s:expandAbbrevMap(self.removePrompt(a:base), g:fuf_abbrevMap)
    let items += self.getMatchingCompleteItems(patternBase)
    if len(items) > g:fuf_enumeratingLimit
      let items = items[ : g:fuf_enumeratingLimit - 1]
      call s:highlightError()
      break
    endif
  endfor
  if empty(items)
    call s:highlightError()
  else
    call sort(items, 'fuf#compareRanks')
    if g:fuf_autoPreview
      call feedkeys("\<C-p>\<Down>\<C-r>=fuf#getRunningHandler().onPreviewBase(0) ? '' : ''\<CR>", 'n')
    else
      call feedkeys("\<C-p>\<Down>", 'n')
    endif
    let self.lastFirstWord = items[0].word
  endif
  return items
endfunction

"
function s:handlerBase.existsPrompt(line)
  return  strlen(a:line) >= strlen(self.getPrompt()) &&
        \ a:line[:strlen(self.getPrompt()) -1] ==# self.getPrompt()
endfunction

"
function s:handlerBase.removePrompt(line)
  return a:line[(self.existsPrompt(a:line) ? strlen(self.getPrompt()) : 0):]
endfunction

"
function s:handlerBase.restorePrompt(line)
  let i = 0
  while i < len(self.getPrompt()) && i < len(a:line) && self.getPrompt()[i] ==# a:line[i]
    let i += 1
  endwhile
  return self.getPrompt() . a:line[i : ]
endfunction

"
function s:handlerBase.onCursorMovedI()
  if !self.existsPrompt(getline('.'))
    call setline('.', self.restorePrompt(getline('.')))
    call feedkeys("\<End>", 'n')
  elseif col('.') <= len(self.getPrompt())
    " if the cursor is moved before command prompt
    call feedkeys(repeat("\<Right>", len(self.getPrompt()) - col('.') + 1), 'n')
  elseif col('.') > strlen(getline('.')) && col('.') != self.lastCol
    " if the cursor is placed on the end of the line and has been actually moved.
    let self.lastCol = col('.')
    let self.lastPattern = self.removePrompt(getline('.'))
    call feedkeys("\<C-x>\<C-o>", 'n')
  endif
endfunction

"
function s:handlerBase.onInsertLeave()
  unlet s:runningHandler
  let tempVars = l9#tempvariables#getList(s:TEMP_VARIABLES_GROUP)
  call l9#tempvariables#end(s:TEMP_VARIABLES_GROUP)
  call s:deactivateFufBuffer()
  call fuf#saveDataFile(self.getModeName(), 'stats', self.stats)
  execute self.windowRestoringCommand
  let fOpen = exists('s:reservedCommand')
  if fOpen
    call self.onOpen(s:reservedCommand[0], s:reservedCommand[1])
    unlet s:reservedCommand
  endif
  call self.onModeLeavePost(fOpen)
  if exists('self.reservedMode')
    call l9#tempvariables#setList(s:TEMP_VARIABLES_GROUP, tempVars)
    call fuf#launch(self.reservedMode, self.lastPattern, self.partialMatching)
  endif
endfunction

"
function s:handlerBase.onCr(openType)
  if pumvisible()
    call feedkeys(printf("\<C-y>\<C-r>=fuf#getRunningHandler().onCr(%d) ? '' : ''\<CR>",
          \              a:openType), 'n')
    return
  endif
  if !empty(self.lastPattern)
    call self.addStat(self.lastPattern, self.removePrompt(getline('.')))
  endif
  if !self.isOpenable(getline('.'))
    " To clear i_<C-r> expression (fuf#getRunningHandler().onCr...)
    echo ''
    return
  endif
  let s:reservedCommand = [self.removePrompt(getline('.')), a:openType]
  call feedkeys("\<Esc>", 'n') " stopinsert behavior is strange...
endfunction

"
function s:handlerBase.onBs()
  call feedkeys((pumvisible() ? "\<C-e>\<BS>" : "\<BS>"), 'n')
endfunction

"
function s:getLastBlockLength(pattern, patternIsPath)
  let separatorPos = strridx(a:pattern, g:fuf_patternSeparator)
  if separatorPos >= 0
    return len(a:pattern) - separatorPos
  endif
  if a:patternIsPath && a:pattern =~# '[/\\].'
    return len(matchstr(a:pattern, '[^/\\]*.$'))
  endif
  return len(a:pattern)
endfunction

"
function s:handlerBase.onDeleteWord()
  let pattern = self.removePrompt(getline('.')[ : col('.') - 2])
  let numBs = s:getLastBlockLength(pattern, 1)
  call feedkeys((pumvisible() ? "\<C-e>" : "") . repeat("\<BS>", numBs), 'n')
endfunction

"
function s:handlerBase.onPreviewBase(repeatable)
  if self.getPreviewHeight() <= 0
    return
  elseif !pumvisible()
    return
  elseif !self.existsPrompt(getline('.'))
    let word = self.removePrompt(getline('.'))
  elseif !exists('self.lastFirstWord')
    return
  else
    let word = self.lastFirstWord
  endif
  redraw
  if a:repeatable && exists('self.lastPreviewInfo') && self.lastPreviewInfo.word ==# word
    let self.lastPreviewInfo.count += 1
  else
    let self.lastPreviewInfo = {'word': word, 'count': 0}
  endif
  let lines = self.makePreviewLines(word, self.lastPreviewInfo.count)
  let lines = lines[: self.getPreviewHeight() - 1]
  call map(lines, 'substitute(v:val, "\t", repeat(" ", &tabstop), "g")')
  call map(lines, 'strtrans(v:val)')
  call map(lines, 'l9#snipTail(v:val, &columns - 1, s:ABBR_SNIP_MASK)')
  echo join(lines, "\n")
endfunction

"
function s:handlerBase.onSwitchMode(shift)
  let modes = copy(fuf#getModeNames())
  call map(modes, '{ "ranks": [ fuf#{v:val}#getSwitchOrder(), v:val ] }')
  call filter(modes, 'v:val.ranks[0] >= 0')
  call sort(modes, 'fuf#compareRanks')
  let self.reservedMode = self.getModeName()
  for i in range(len(modes))
    if modes[i].ranks[1] ==# self.getModeName()
      let self.reservedMode = modes[(i + a:shift) % len(modes)].ranks[1]
      break
    endif
  endfor
  call feedkeys("\<Esc>", 'n') " stopinsert doesn't work.
endfunction

"
function s:handlerBase.onSwitchMatching()
  let self.partialMatching = !self.partialMatching
  let self.lastCol = -1
  call setline('.', self.restorePrompt(self.lastPattern))
  call feedkeys("\<End>", 'n')
  "call self.onCursorMovedI()
endfunction

"
function s:handlerBase.onRecallPattern(shift)
  let patterns = map(copy(self.stats), 'v:val.pattern')
  if !exists('self.indexRecall')
    let self.indexRecall = -1
  endif
  let self.indexRecall += a:shift
  if self.indexRecall < 0
    let self.indexRecall = -1
  elseif self.indexRecall >= len(patterns)
    let self.indexRecall = len(patterns) - 1
  else
    call setline('.', self.getPrompt() . patterns[self.indexRecall])
    call feedkeys("\<End>", 'n')
  endif
endfunction

" }}}1
"=============================================================================
" INITIALIZATION {{{1

augroup FufGlobal
  autocmd!
  autocmd BufLeave * let s:bufferCursorPosMap[bufnr('')] = getpos('.')
augroup END

let s:bufferCursorPosMap = {}

"
let s:DATA_FILE_VERSION = 400

"
function s:checkDataFileCompatibility()
  if empty(g:fuf_dataDir)
    let s:dataFileAvailable = 0
    return
  endif
  let versionPath = l9#concatPaths([g:fuf_dataDir, 'VERSION'])
  let lines = l9#readFile(versionPath)
  if empty(lines)
    call l9#writeFile([s:DATA_FILE_VERSION], versionPath)
    let s:dataFileAvailable = 1
  elseif str2nr(lines[0]) == s:DATA_FILE_VERSION
    let s:dataFileAvailable = 1
  else
    call fuf#echoWarning(printf(
          \ "=======================================================\n" .
          \ "  Existing data files for FuzzyFinder is no longer     \n" .
          \ "  compatible with this version of FuzzyFinder. Remove  \n" .
          \ "  %-53s\n" .
          \ "=======================================================\n" ,
          \ string(g:fuf_dataDir)))
    call l9#inputHl('Question', 'Press Enter')
    let s:dataFileAvailable = 0
  endif
endfunction

call s:checkDataFileCompatibility()

" }}}1
"=============================================================================
" vim: set fdm=marker:

