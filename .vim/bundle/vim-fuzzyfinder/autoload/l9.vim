"=============================================================================
" Copyright (c) 2009-2010 Takeshi NISHIDA
"
"=============================================================================
" LOAD GUARD {{{1

if exists('g:loaded_autoload_l9')
  finish
endif
let g:loaded_autoload_l9 = 1

" }}}1
"=============================================================================
" COMPATIBILITY TEST {{{1

"
let s:L9_VERSION_CURRENT  = 101
let s:L9_VERSION_PASSABLE = 101

" returns true if given version is compatible.
function l9#isCompatible(ver)
  return 
endfunction

let s:VERSION_FACTOR = str2float('0.01')

" returns false if the caller script should finish.
" a:vimVersion: if 0, don't check vim version
" a:l9Version: same rule as v:version
function l9#guardScriptLoading(path, vimVersion, l9Version, exprs)
  let loadedVarName = 'g:loaded_' . substitute(a:path, '\W', '_', 'g')
  if exists(loadedVarName)
    return 0
  elseif a:vimVersion > 0 && a:vimVersion > v:version
    echoerr a:path . ' requires Vim version ' . string(a:vimVersion * s:VERSION_FACTOR)
    return 0
  elseif a:l9Version > 0 && (a:l9Version > s:L9_VERSION_CURRENT ||
        \                    a:l9Version < s:L9_VERSION_PASSABLE)
    echoerr a:path . ' requires L9 library version ' . string(a:l9Version * s:VERSION_FACTOR)
    return 0
  endif
  for expr in a:exprs
    if !eval(expr)
      echoerr a:path . ' requires: ' . expr
      return 0
    endif
  endfor
  let {loadedVarName} = 1
  return 1
endfunction

" 
function l9#getVersion()
  return s:L9_VERSION_CURRENT
endfunction

" }}}1
"=============================================================================
" LIST {{{1

" Removes duplicates (unstable)
" This function doesn't change the list of argument.
function l9#unique(items)
  let sorted = sort(a:items)
  if len(sorted) < 2
    return sorted
  endif
  let last = remove(sorted, 0)
  let result = [last]
  for item in sorted
    if item != last
      call add(result, item)
      let last = item
    endif
  endfor
  return result
endfunction

" Removes duplicates (stable)
" This function doesn't change the list of argument.
function l9#uniqueStably(items)
  let result = []
  for item in a:items
    if count(result, item, &ignorecase) == 0
      call add(result, item)
    endif
  endfor
  return result
endfunction

" [ [0], [1,2], [3] ] -> [ 0, 1, 2, 3 ]
" This function doesn't change the list of argument.
function l9#concat(items)
  let result = []
  for l in a:items
    let result += l
  endfor
  return result
endfunction

" [ [0,1,2], [3,4], [5,6,7,8] ] -> [ [0,3,5],[1,4,6] ]
" This function doesn't change the list of argument.
function l9#zip(items)
  let result = []
  for i in range(min(map(copy(a:items), 'len(v:val)')))
    call add(result, map(copy(a:items), 'v:val[i]'))
  endfor
  return result
endfunction

" filter() with the maximum number of items
" This function doesn't change the list of argument.
function l9#filterWithLimit(items, expr, limit)
  if a:limit <= 0
    return filter(copy(a:items), a:expr)
  endif
  let result = []
  let stride = a:limit * 3 / 2 " x1.5
  for i in range(0, len(a:items) - 1, stride)
    let result += filter(a:items[i : i + stride - 1], a:expr)
    if len(result) >= a:limit
      return remove(result, 0, a:limit - 1)
    endif
  endfor
  return result
endfunction

" Removes if a:expr is evaluated as non-zero and returns removed items.
" This function change the list of argument.
function l9#removeIf(items, expr)
  let removed = filter(copy(a:items), a:expr)
  call filter(a:items, '!( ' . a:expr . ')')
  return removed
endfunction

" }}}1
"=============================================================================
" NUMERIC {{{1

" }}}1
"=============================================================================
" STRING {{{1

" Snips a:str and add a:mask if the length of a:str is more than a:len
function l9#snipHead(str, len, mask)
  if a:len >= len(a:str)
    return a:str
  elseif a:len <= len(a:mask)
    return a:mask
  endif
  return a:mask . a:str[-a:len + len(a:mask):]
endfunction

" Snips a:str and add a:mask if the length of a:str is more than a:len
function l9#snipTail(str, len, mask)
  if a:len >= len(a:str)
    return a:str
  elseif a:len <= len(a:mask)
    return a:mask
  endif
  return a:str[:a:len - 1 - len(a:mask)] . a:mask
endfunction

" Snips a:str and add a:mask if the length of a:str is more than a:len
function l9#snipMid(str, len, mask)
  if a:len >= len(a:str)
    return a:str
  elseif a:len <= len(a:mask)
    return a:mask
  endif
  let len_head = (a:len - len(a:mask)) / 2
  let len_tail = a:len - len(a:mask) - len_head
  return  (len_head > 0 ? a:str[: len_head - 1] : '') . a:mask .
        \ (len_tail > 0 ? a:str[-len_tail :] : '')
endfunction

"
function l9#hash224(str)
  let a = 0x00000800 " shift 11 bit (if unsigned)
  let b = 0x001fffff " extract 11 bit (if unsigned)
  let nHash = 7
  let hashes = repeat([0], nHash)
  for i in range(len(a:str))
    let iHash = i % nHash
    let hashes[iHash] = hashes[iHash] * a + hashes[iHash] / b
    let hashes[iHash] += char2nr(a:str[i])
  endfor
  return join(map(hashes, 'printf("%08x", v:val)'), '')
endfunction

" wildcard -> regexp
function l9#convertWildcardToRegexp(expr)
  let re = escape(a:expr, '\')
  for [pat, sub] in [ [ '*', '\\.\\*' ], [ '?', '\\.' ], [ '[', '\\[' ], ]
    let re = substitute(re, pat, sub, 'g')
  endfor
  return '\V' . re
endfunction

" }}}1
"=============================================================================
" LINES {{{1

" Removes from the line matching with a:begin first to the line matching with
" a:end next and returns removed lines.
" If matching range is not found, returns []
function l9#removeLinesBetween(lines, begin, end)
  for i in range(len(a:lines) - 1)
    if a:lines[i] =~ a:begin
      break
    endif
  endfor
  for j in range(i + 1, len(a:lines) - 1)
    if a:lines[j] =~ a:end
      let g:l0 += [a:lines[i : j]]
      return remove(a:lines, i, j)
    endif
  endfor
  return []
endfunction

" }}}1
"=============================================================================
" PATH {{{1

" returns the path separator charactor.
function l9#getPathSeparator()
  return (!&shellslash && (has('win32') || has('win64')) ? '\' : '/')
endfunction

" [ 'a', 'b/', '/c' ] -> 'a/b/c'
function l9#concatPaths(paths)
  let result = ''
  for p in a:paths
    if empty(p)
      continue
    elseif empty(result)
      let result = p
    else
      let result = substitute(result, '[/\\]$', '', '') . l9#getPathSeparator()
            \    . substitute(p, '^[/\\]', '', '')
    endif
  endfor
  return result
endfunction

" path: '/a/b/c/d', dir: '/a/b' => 'c/d'
function l9#modifyPathRelativeToDir(path, dir)
  let pathFull = fnamemodify(a:path, ':p')
  let dirFull = fnamemodify(a:dir, ':p')
  if len(pathFull) < len(dirFull) || pathFull[:len(dirFull) - 1] !=# dirFull
    return pathFull
  endif
  return pathFull[len(dirFull):]
endfunction

" }}}1
"=============================================================================
" FILE {{{1

" Almost same as readfile().
function l9#readFile(...)
  let args = copy(a:000)
  let args[0] = expand(args[0])
  try
    return call('readfile', args)
  catch
  endtry
  return []
endfunction

" Almost same as writefile().
function l9#writeFile(...)
  let args = copy(a:000)
  let args[1] = expand(args[1])
  let dir = fnamemodify(args[1], ':h')
  try
    if !isdirectory(dir)
      call mkdir(dir, 'p')
    endif
    return call('writefile', args)
  catch
  endtry
  return -1 " -1 is error code.
endfunction

" }}}1
"=============================================================================
" BUFFER {{{1

" :wall/:wall! wrapper. Useful for writing readonly buffers.
function l9#writeAll()
  try
    silent update " NOTE: avoiding a problem with a buftype=acwrite buffer.
    silent wall
  catch /^Vim/ " E45, E505
    if l9#inputHl('Question', v:exception . "\nWrite readonly files? (Y/N) : ", 'Y') ==? 'y'
      redraw
      :wall!
    endif
  endtry
endfunction

" Loads given files with :edit command
function l9#loadFilesToBuffers(files)
  for file in filter(copy(a:files), '!bufloaded(v:val)')
    execute 'edit ' . fnameescape(file)
    if !exists('bufNrFirst')
      let bufNrFirst = bufnr('%')
    endif
  endfor
  if exists('bufNrFirst')
    execute bufNrFirst . 'buffer'
  endif
endfunction

" Deletes all buffers except given files with :bdelete command
function l9#deleteAllBuffersExcept(files)
  let bufNrExcepts = map(copy(a:files), 'bufnr("^" . v:val . "$")')
  for bufNr in filter(range(1, bufnr('$')), 'bufloaded(v:val)')
    if count(bufNrExcepts, bufNr) == 0
      execute bufNr . 'bdelete'
    endif
  endfor
endfunction

" }}}1
"=============================================================================
" WINDOW {{{1

" move current window to next tabpage.
function l9#shiftWinNextTabpage()
  if tabpagenr('$') < 2
    return
  endif
  let bufnr = bufnr('%')
  tabnext
  execute bufnr . 'sbuffer'
  tabprevious
  if winnr('$') > 1
    close
    tabnext
  else
    close " if tabpage is closed, next tabpage will become current
  endif
endfunction

" move current window to previous tabpage.
function l9#shiftWinPrevTabpage()
  if tabpagenr('$') < 2
    return
  endif
  let bufnr = bufnr('%')
  tabprevious
  execute bufnr . 'sbuffer'
  tabnext
  close
  tabprevious
endfunction

" move to a window containing specified buffer.
" returns 0 if the buffer is not found.
function l9#moveToBufferWindowInCurrentTabpage(bufNr)
  if bufnr('%') == a:bufNr
    return 1
  elseif count(tabpagebuflist(), a:bufNr) == 0
    return 0
  endif
  execute bufwinnr(a:bufNr) . 'wincmd w'
  return 1
endfunction

" returns 0 if the buffer is not found.
function s:moveToOtherTabpageOpeningBuffer(bufNr)
  for tabNr in range(1, tabpagenr('$'))
    if tabNr != tabpagenr() && count(tabpagebuflist(tabNr), a:bufNr) > 0
      execute 'tabnext ' . tabNr
      return 1
    endif
  endfor
  return 0
endfunction

" move to a window containing specified buffer.
" returns 0 if the buffer is not found.
function l9#moveToBufferWindowInOtherTabpage(bufNr)
  if !s:moveToOtherTabpageOpeningBuffer(a:bufNr)
    return 0
  endif
  return l9#moveToBufferWindowInCurrentTabpage(a:bufNr)
endfunction

" }}}1
"=============================================================================
" COMMAND LINE {{{1

" echo/echomsg with highlighting.
function l9#echoHl(hl, msg, prefix, addingHistory)
  let echoCmd = (a:addingHistory ? 'echomsg' : 'echo')
  execute "echohl " . a:hl
  try
    for l in (type(a:msg) == type([]) ? a:msg : split(a:msg, "\n"))
      execute echoCmd . ' a:prefix . l'
    endfor
  finally
    echohl None
  endtry
endfunction

" input() with highlighting.
" This function can take list as {completion} argument.
function l9#inputHl(hl, ...)
  execute "echohl " . a:hl
  try
    let args = copy(a:000)
    if len(args) > 2 && type(args[2]) == type([])
      let s:candidatesForInputHl = args[2]
      let args[2] = 'custom,l9#completeForInputHl'
    endif
    let s = call('input', args)
    unlet! s:candidatesForInputHl
  finally
    echohl None
  endtry
  redraw " needed to show following echo to next line.
  return s
endfunction

" only called by l9#inputHl() for completion.
function l9#completeForInputHl(lead, line, pos)
  return join(s:candidatesForInputHl, "\n")
endfunction

" }}}1
"=============================================================================
" VISUAL MODE {{{1

" returns last selected text in Visual mode.
function l9#getSelectedText()
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


" }}}1
"=============================================================================
" EVAL {{{1

" loads given text as Vim script with :source command
function l9#loadScript(text)
  let lines = (type(a:text) == type([]) ? a:text : split(a:text, "\n"))
  let fname = tempname()
  call writefile(lines, fname)
  source `=fname`
  call delete(fname)
endfunction


" }}}1
"=============================================================================
" VARIABLES {{{1

" 
function l9#defineVariableDefault(name, default)
  if !exists(a:name)
    let {a:name} = a:default
  endif
endfunction

" }}}1
"=============================================================================
" GREP {{{1

" Execute :vimgrep and opens the quickfix window if matches are found.
"
" a:pattern: search pattern. If ommitted, last search pattern (@/) is used.
" a:files: List of files
function l9#grepFiles(pattern, files)
  let target = join(map(a:files, 'escape(v:val, " ")'), ' ')
  let pattern = (a:pattern[0] ==# '/' ? a:pattern[1:] : a:pattern)
  let pattern = (empty(pattern)  ? @/ : pattern)
  try
    execute printf('vimgrep/%s/j %s', pattern, target)
  catch /^Vim/
    call setqflist([])
  endtry
  call l9#quickfix#sort()
  call l9#quickfix#openIfNotEmpty(1, 0)
endfunction

" Execute :vimgrep for buffers using l9#grepFiles()
" See also: :L9GrepBuffer :L9GrepBufferAll
function l9#grepBuffers(pattern, bufNrs)
  let files = map(filter(a:bufNrs, 'bufloaded(v:val)'), 'bufname(v:val)')
  call l9#grepFiles(a:pattern, files)
endfunction

" }}}1
"=============================================================================
" SIGN {{{1

" Highlights lines using :sign define and :sign place.
" 
" a:linehl, a:text, a:texthl: See |signs|. Ignored if empty string.
" a:locations: List of [{buffer number}, {line number}] for highlighting
function l9#placeSign(linehl, text, texthl, locations)
  let argLinehl = (empty(a:linehl) ? '' : 'linehl=' . a:linehl)
  let argText = (empty(a:text) ? '' : 'text=' . a:text)
  let argTexthl = (empty(a:texthl) ? '' : 'texthl=' . a:texthl)
  let name = 'l9--' . a:linehl . '--' . a:text . '--' . a:texthl
  execute printf('sign define %s linehl=%s text=%s texthl=%s',
        \        name, a:linehl, a:text, a:texthl)
  for [bufNr, lnum] in a:locations
    execute printf('sign place 1 line=%d name=%s buffer=%d', lnum, name, bufNr)
  endfor
endfunction

" }}}1
"=============================================================================
" NOTIFY EXTERNALLY {{{1

" Notify a message using an external program.
" Currently supports Balloonly, Screen, and Tmux.
function l9#notifyExternally(msg)
  return     l9#notifyBalloonly(a:msg)
        \ || l9#notifyScreen(a:msg)
        \ || l9#notifyTmux(a:msg)
endfunction

"
function l9#notifyBalloonly(msg)
  if !(has('win32') || has('win64')) || !executable(g:l9_balloonly)
    return 0
  endif
  execute 'silent !start ' . shellescape(g:l9_balloonly) . ' 4000 "l9" ' . shellescape(a:msg)
  return 1
endfunction

"
function l9#notifyScreen(msg)
  if !has('unix') || has('gui_running') || $WINDOW !~ '\d' || !executable('screen')
    return 0
  endif
  call system('screen -X wall ' . shellescape('l9: ' . a:msg))
  return 1
endfunction

"
function l9#notifyTmux(msg)
  if !has('unix') || has('gui_running') || empty($TMUX) || !executable('tmux')
    return 0
  endif
  call system('tmux display-message ' . shellescape('l9: ' . a:msg))
  return 1
endfunction

" }}}1
"=============================================================================
" vim: set fdm=marker:
