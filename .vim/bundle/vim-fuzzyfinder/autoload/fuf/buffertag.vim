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
function fuf#buffertag#createHandler(base)
  return a:base.concretize(copy(s:handler))
endfunction

"
function fuf#buffertag#getSwitchOrder()
  return g:fuf_buffertag_switchOrder
endfunction

"
function fuf#buffertag#getEditableDataNames()
  return []
endfunction

"
function fuf#buffertag#renewCache()
  let s:tagItemsCache = {}
  let s:tagDataCache = {}
endfunction

"
function fuf#buffertag#requiresOnCommandPre()
  return 0
endfunction

"
function fuf#buffertag#onInit()
  call fuf#defineLaunchCommand('FufBufferTag', s:MODE_NAME, '""',
        \                      [['g:fuf_buffertag_forAll', 0]])
  call fuf#defineLaunchCommand('FufBufferTagAll', s:MODE_NAME, '""',
        \                      [['g:fuf_buffertag_forAll', 1]])
  call fuf#defineLaunchCommand('FufBufferTagWithCursorWord', s:MODE_NAME,
        \                      'expand(''<cword>'')', [['g:fuf_buffertag_forAll', 0]])
  call fuf#defineLaunchCommand('FufBufferTagAllWithCursorWord', s:MODE_NAME,
        \                      'expand(''<cword>'')', [['g:fuf_buffertag_forAll', 1]])
  call fuf#defineLaunchCommand('FufBufferTagWithSelectedText', s:MODE_NAME,
        \                      'l9#getSelectedText()', [['g:fuf_buffertag_forAll', 0]])
  call fuf#defineLaunchCommand('FufBufferTagAllWithSelectedText', s:MODE_NAME,
        \                      'l9#getSelectedText()', [['g:fuf_buffertag_forAll', 1]])
  call l9#defineVariableDefault('g:fuf_buffertag_forAll', 0) " private option
  " the following settings originate from taglist.vim
  call l9#defineVariableDefault('g:fuf_buffertag__asm'       , '--language-force=asm --asm-types=dlmt')
  call l9#defineVariableDefault('g:fuf_buffertag__aspperl'   , '--language-force=asp --asp-types=fsv')
  call l9#defineVariableDefault('g:fuf_buffertag__aspvbs'    , '--language-force=asp --asp-types=fsv')
  call l9#defineVariableDefault('g:fuf_buffertag__awk'       , '--language-force=awk --awk-types=f')
  call l9#defineVariableDefault('g:fuf_buffertag__beta'      , '--language-force=beta --beta-types=fsv')
  call l9#defineVariableDefault('g:fuf_buffertag__c'         , '--language-force=c --c-types=dgsutvf')
  call l9#defineVariableDefault('g:fuf_buffertag__cpp'       , '--language-force=c++ --c++-types=nvdtcgsuf')
  call l9#defineVariableDefault('g:fuf_buffertag__cs'        , '--language-force=c# --c#-types=dtncEgsipm')
  call l9#defineVariableDefault('g:fuf_buffertag__cobol'     , '--language-force=cobol --cobol-types=dfgpPs')
  call l9#defineVariableDefault('g:fuf_buffertag__eiffel'    , '--language-force=eiffel --eiffel-types=cf')
  call l9#defineVariableDefault('g:fuf_buffertag__erlang'    , '--language-force=erlang --erlang-types=drmf')
  call l9#defineVariableDefault('g:fuf_buffertag__expect'    , '--language-force=tcl --tcl-types=cfp')
  call l9#defineVariableDefault('g:fuf_buffertag__fortran'   , '--language-force=fortran --fortran-types=pbceiklmntvfs')
  call l9#defineVariableDefault('g:fuf_buffertag__html'      , '--language-force=html --html-types=af')
  call l9#defineVariableDefault('g:fuf_buffertag__java'      , '--language-force=java --java-types=pcifm')
  call l9#defineVariableDefault('g:fuf_buffertag__javascript', '--language-force=javascript --javascript-types=f')
  call l9#defineVariableDefault('g:fuf_buffertag__lisp'      , '--language-force=lisp --lisp-types=f')
  call l9#defineVariableDefault('g:fuf_buffertag__lua'       , '--language-force=lua --lua-types=f')
  call l9#defineVariableDefault('g:fuf_buffertag__make'      , '--language-force=make --make-types=m')
  call l9#defineVariableDefault('g:fuf_buffertag__pascal'    , '--language-force=pascal --pascal-types=fp')
  call l9#defineVariableDefault('g:fuf_buffertag__perl'      , '--language-force=perl --perl-types=clps')
  call l9#defineVariableDefault('g:fuf_buffertag__php'       , '--language-force=php --php-types=cdvf')
  call l9#defineVariableDefault('g:fuf_buffertag__python'    , '--language-force=python --python-types=cmf')
  call l9#defineVariableDefault('g:fuf_buffertag__rexx'      , '--language-force=rexx --rexx-types=s')
  call l9#defineVariableDefault('g:fuf_buffertag__ruby'      , '--language-force=ruby --ruby-types=cfFm')
  call l9#defineVariableDefault('g:fuf_buffertag__scheme'    , '--language-force=scheme --scheme-types=sf')
  call l9#defineVariableDefault('g:fuf_buffertag__sh'        , '--language-force=sh --sh-types=f')
  call l9#defineVariableDefault('g:fuf_buffertag__csh'       , '--language-force=sh --sh-types=f')
  call l9#defineVariableDefault('g:fuf_buffertag__zsh'       , '--language-force=sh --sh-types=f')
  call l9#defineVariableDefault('g:fuf_buffertag__slang'     , '--language-force=slang --slang-types=nf')
  call l9#defineVariableDefault('g:fuf_buffertag__sml'       , '--language-force=sml --sml-types=ecsrtvf')
  call l9#defineVariableDefault('g:fuf_buffertag__sql'       , '--language-force=sql --sql-types=cFPrstTvfp')
  call l9#defineVariableDefault('g:fuf_buffertag__tcl'       , '--language-force=tcl --tcl-types=cfmp')
  call l9#defineVariableDefault('g:fuf_buffertag__vera'      , '--language-force=vera --vera-types=cdefgmpPtTvx')
  call l9#defineVariableDefault('g:fuf_buffertag__verilog'   , '--language-force=verilog --verilog-types=mcPertwpvf')
  call l9#defineVariableDefault('g:fuf_buffertag__vim'       , '--language-force=vim --vim-types=avf')
  call l9#defineVariableDefault('g:fuf_buffertag__yacc'      , '--language-force=yacc --yacc-types=l')
endfunction

" }}}1
"=============================================================================
" LOCAL FUNCTIONS/VARIABLES {{{1

let s:MODE_NAME = expand('<sfile>:t:r')

"
function s:parseTagLine(line)
  " tag	W:\Win32\SRC7\NCSIM\NCVW32\CUBEFACE.H	/^#define CUBEFACE_H$/;"	macro	line:4
  let fields = matchlist(a:line, '\v^([^\t]+)\t(.+)\t\/\^(.+)\$\/\;\"\t(.+)\tline\:(\d+)')
  if empty(fields)
    return {}
  endif
  return {
        \   'tag'    : fields[1],
        \   'fname'  : fields[2],
        \   'pattern': fields[3],
        \   'kind'   : fields[4],
        \   'lnum'   : str2nr(fields[5]),
        \ }
endfunction

"
let s:TEMP_VARIABLES_GROUP = expand('<sfile>:p')

"
function s:getFileType(bufNr)
  let ft = getbufvar(a:bufNr, '&filetype')
  if !empty(ft) || bufloaded(a:bufNr)
    return ft
  endif
  let ft = getbufvar(a:bufNr, 'fuf_buffertag_filetype')
  if !empty(ft)
    return ft
  endif
  call l9#tempvariables#set(s:TEMP_VARIABLES_GROUP, '&eventignore', 'FileType')
  call l9#tempvariables#set(s:TEMP_VARIABLES_GROUP, '&filetype', &filetype)
  " from taglist.vim
  execute 'doautocmd filetypedetect BufRead ' . bufname(a:bufNr)
  let ft = &filetype
  call l9#tempvariables#end(s:TEMP_VARIABLES_GROUP)
  call setbufvar(a:bufNr, 'fuf_buffertag_filetype', ft)
  return ft
endfunction

"
function s:makeCtagsCmd(bufNr)
  let ft = s:getFileType(a:bufNr)
  if !exists('g:fuf_buffertag__{ft}')
    return ''
  endif
  "
  let cmd = join([g:fuf_buffertag_ctagsPath,
        \         '-f - --sort=no --excmd=pattern --fields=nKs',
        \         g:fuf_buffertag__{ft},
        \         shellescape(fnamemodify(bufname(a:bufNr), ':p'))])
  return cmd
endfunction

"
function s:getTagItems(bufNr)
  let cmd = s:makeCtagsCmd(a:bufNr)
  if empty(cmd)
    return []
  elseif !exists('s:tagItemsCache[cmd]') ||
        \ s:tagItemsCache[cmd].time < getftime(expand(bufname(a:bufNr)))
    let items = split(system(cmd), "\n")
    if v:shell_error
      call fuf#echoError([cmd] + items)
      throw "Command error"
    endif
    call map(items, 's:parseTagLine(v:val)')
    call filter(items, '!empty(v:val)')
    let s:tagItemsCache[cmd] = {
          \   'time'  : localtime(),
          \   'items' : items,
          \ }
  endif
  return s:tagItemsCache[cmd].items
endfunction

"
function s:makeItem(tag, itemMap)
  let menu = fnamemodify(a:itemMap[a:tag][0].fname, ':t')
        \ . ' [' . a:itemMap[a:tag][0].kind . ']'
  if len(a:itemMap[a:tag]) > 1
    let menu .= ' (' . len(a:itemMap[a:tag]) . ')'
  endif
  let item = fuf#makeNonPathItem(a:tag, menu)
  return item
endfunction

"
function s:getTagData(bufNrs)
  let key = join([0] + sort(copy(a:bufNrs)), "\n")
  let bufNames = map(copy(a:bufNrs), 'bufname(v:val)')
  if !exists('s:tagDataCache[key]') ||
        \ fuf#countModifiedFiles(bufNames, s:tagDataCache[key].time) > 0
    let itemMap = {}
    for item in l9#concat(map(copy(a:bufNrs), 's:getTagItems(v:val)'))
      if !exists('itemMap[item.tag]')
        let itemMap[item.tag] = []
      endif
      call add(itemMap[item.tag], item)
    endfor
    let items = sort(keys(itemMap))
    call map(items, 's:makeItem(v:val, itemMap)')
    call fuf#mapToSetSerialIndex(items, 1)
    call map(items, 'fuf#setAbbrWithFormattedWord(v:val, 1)')
    let s:tagDataCache[key] = {
          \   'time'   : localtime(),
          \   'itemMap': itemMap,
          \   'items'  : items,
          \ }
  endif
  return [s:tagDataCache[key].items, s:tagDataCache[key].itemMap]
endfunction

"
function s:jumpToTag(item, mode)
  call fuf#openFile(a:item.fname, a:mode, g:fuf_reuseWindow)
  call cursor(a:item.lnum, 1)
  normal! zvzz
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
  return fuf#formatPrompt(g:fuf_buffertag_prompt, self.partialMatching, '')
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
  if !exists('self.itemMap[a:word][0]')
    call fuf#echoError('Definition not found:' . a:word)
    return
  elseif len(self.itemMap[a:word]) == 1
    let i = 0
  else
    let list = map(fuf#mapToSetSerialIndex(copy(self.itemMap[a:word]), 1),
          \        'printf(" %2d: %s|%d| [%s] %s",v:val.index, fnamemodify(v:val.fname, ":~:."), v:val.lnum, v:val.kind, v:val.pattern)')
    let i = inputlist(['Select a definition of "' . a:word . '":'] + list) - 1
  endif
  if 0 <= i && i < len(self.itemMap[a:word])
    call s:jumpToTag(self.itemMap[a:word][i], a:mode)
  endif
endfunction

"
function s:handler.onModeEnterPre()
endfunction

"
function s:handler.onModeEnterPost()
  if g:fuf_buffertag_forAll
    let bufNrs = filter(range(1, bufnr('$')), 'buflisted(v:val)')
  else
    let bufNrs = [self.bufNrPrev]
  endif
  let [self.items, self.itemMap] = s:getTagData(bufNrs)
endfunction

"
function s:handler.onModeLeavePost(opened)
endfunction

" }}}1
"=============================================================================
" vim: set fdm=marker:
