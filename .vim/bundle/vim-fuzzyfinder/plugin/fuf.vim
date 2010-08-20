"=============================================================================
" Copyright (c) 2007-2009 Takeshi NISHIDA
"
" GetLatestVimScripts: 1984 1 :AutoInstall: FuzzyFinder
"=============================================================================
" LOAD GUARD {{{1

if exists('g:loaded_fuf')
  finish
elseif v:version < 702
  echoerr 'FuzzyFinder does not support this version of vim (' . v:version . ').'
  finish
endif
let g:loaded_fuf = 1

" }}}1
"=============================================================================
" LOCAL FUNCTIONS {{{1

"
function s:initialize()
  "---------------------------------------------------------------------------
  call s:defineOption('g:fuf_modes'  , [
        \   'buffer', 'file', 'dir', 'mrufile', 'mrucmd',
        \   'bookmark', 'tag', 'taggedfile',
        \   'jumplist', 'changelist', 'quickfix', 'line', 'help',
        \   'givenfile', 'givendir', 'givencmd',
        \   'callbackfile', 'callbackitem',
        \ ])
  call s:defineOption('g:fuf_modesDisable'     , [ 'mrufile', 'mrucmd', ])
  call s:defineOption('g:fuf_keyOpen'          , '<CR>')
  call s:defineOption('g:fuf_keyOpenSplit'     , '<C-j>')
  call s:defineOption('g:fuf_keyOpenVsplit'    , '<C-k>')
  call s:defineOption('g:fuf_keyOpenTabpage'   , '<C-l>')
  call s:defineOption('g:fuf_keyPreview'       , '<C-@>')
  call s:defineOption('g:fuf_keyNextMode'      , '<C-t>')
  call s:defineOption('g:fuf_keyPrevMode'      , '<C-y>')
  call s:defineOption('g:fuf_keyPrevPattern'   , '<C-s>')
  call s:defineOption('g:fuf_keyNextPattern'   , '<C-_>')
  call s:defineOption('g:fuf_keySwitchMatching', '<C-\><C-\>')
  call s:defineOption('g:fuf_infoFile'         , '~/.vim-fuf')
  call s:defineOption('g:fuf_abbrevMap'        , {})
  call s:defineOption('g:fuf_patternSeparator' , ';')
  call s:defineOption('g:fuf_promptHighlight'  , 'Question')
  call s:defineOption('g:fuf_ignoreCase'       , 1)
  call s:defineOption('g:fuf_splitPathMatching', 1)
  call s:defineOption('g:fuf_smartBs'          , 1)
  call s:defineOption('g:fuf_reuseWindow'      , 1)
  call s:defineOption('g:fuf_timeFormat'       , '(%Y-%m-%d %H:%M:%S)')
  call s:defineOption('g:fuf_learningLimit'    , 100)
  call s:defineOption('g:fuf_enumeratingLimit' , 50)
  call s:defineOption('g:fuf_maxMenuWidth'     , 78)
  call s:defineOption('g:fuf_previewHeight'    , 10)
  call s:defineOption('g:fuf_autoPreview'      , 1)
  call s:defineOption('g:fuf_useMigemo'        , 0)
  "---------------------------------------------------------------------------
  call s:defineOption('g:fuf_buffer_prompt'     , '>Buffer[]>')
  call s:defineOption('g:fuf_buffer_switchOrder', 10)
  call s:defineOption('g:fuf_buffer_mruOrder'   , 1)
  "---------------------------------------------------------------------------
  call s:defineOption('g:fuf_file_prompt'     , '>File[]>')
  call s:defineOption('g:fuf_file_switchOrder', 20)
  call s:defineOption('g:fuf_file_exclude'    , '\v\~$|\.(o|exe|dll|bak|sw[po])$|(^|[/\\])\.(hg|git|bzr)($|[/\\])')
  "---------------------------------------------------------------------------
  call s:defineOption('g:fuf_dir_prompt'     , '>Dir[]>')
  call s:defineOption('g:fuf_dir_switchOrder', 30)
  call s:defineOption('g:fuf_dir_exclude'    , '\v(^|[/\\])\.(hg|git|bzr)($|[/\\])')
  "---------------------------------------------------------------------------
  call s:defineOption('g:fuf_mrufile_prompt'     , '>Mru-File[]>')
  call s:defineOption('g:fuf_mrufile_switchOrder', 40)
  call s:defineOption('g:fuf_mrufile_exclude'    , '\v\~$|\.(bak|sw[po])$|^(\/\/|\\\\|\/mnt\/|\/media\/)')
  call s:defineOption('g:fuf_mrufile_maxItem'    , 200)
  "---------------------------------------------------------------------------
  call s:defineOption('g:fuf_mrucmd_prompt'     , '>Mru-Cmd[]>')
  call s:defineOption('g:fuf_mrucmd_switchOrder', 50)
  call s:defineOption('g:fuf_mrucmd_exclude'    , '^$')
  call s:defineOption('g:fuf_mrucmd_maxItem'    , 200)
  "---------------------------------------------------------------------------
  call s:defineOption('g:fuf_bookmark_prompt'     , '>Bookmark[]>')
  call s:defineOption('g:fuf_bookmark_switchOrder', 60)
  call s:defineOption('g:fuf_bookmark_searchRange', 400)
  call s:defineOption('g:fuf_bookmark_keyDelete'  , '<C-]>')
  "---------------------------------------------------------------------------
  call s:defineOption('g:fuf_tag_prompt'     , '>Tag[]>')
  call s:defineOption('g:fuf_tag_switchOrder', 70)
  call s:defineOption('g:fuf_tag_cache_dir'  , '~/.vim-fuf-cache/tag')
  "---------------------------------------------------------------------------
  call s:defineOption('g:fuf_taggedfile_prompt'     , '>Tagged-File[]>')
  call s:defineOption('g:fuf_taggedfile_switchOrder', 80)
  call s:defineOption('g:fuf_taggedfile_cache_dir'  , '~/.vim-fuf-cache/taggedfile')
  "---------------------------------------------------------------------------
  call s:defineOption('g:fuf_jumplist_prompt'     , '>Jump-List[]>')
  call s:defineOption('g:fuf_jumplist_switchOrder', 90)
  "---------------------------------------------------------------------------
  call s:defineOption('g:fuf_changelist_prompt'     , '>Change-List[]>')
  call s:defineOption('g:fuf_changelist_switchOrder', 100)
  "---------------------------------------------------------------------------
  call s:defineOption('g:fuf_quickfix_prompt'     , '>Quickfix[]>')
  call s:defineOption('g:fuf_quickfix_switchOrder', 110)
  "---------------------------------------------------------------------------
  call s:defineOption('g:fuf_line_prompt'     , '>Line[]>')
  call s:defineOption('g:fuf_line_switchOrder', 120)
  "---------------------------------------------------------------------------
  call s:defineOption('g:fuf_help_prompt'     , '>Help[]>')
  call s:defineOption('g:fuf_help_switchOrder', 130)
  call s:defineOption('g:fuf_help_cache_dir'  , '~/.vim-fuf-cache/help')
  "---------------------------------------------------------------------------
  call filter(g:fuf_modes, 'count(g:fuf_modesDisable, v:val) == 0')
  for m in g:fuf_modes
    call fuf#{m}#renewCache()
    call fuf#{m}#onInit()
  endfor
  "---------------------------------------------------------------------------
  command! -bang -narg=0 FufEditInfo   call fuf#editInfoFile()
  command! -bang -narg=0 FufRenewCache call s:renewCachesOfAllModes()
  "---------------------------------------------------------------------------
  for m in g:fuf_modes
    if fuf#{m}#requiresOnCommandPre()
      " cnoremap has a problem, which doesn't expand cabbrev.
      cmap <silent> <expr> <CR> <SID>onCommandPre()
      break
    endif
  endfor
  "---------------------------------------------------------------------------
endfunction

"
function s:initMisc()
endfunction

"
function s:defineOption(name, default)
  if !exists(a:name)
    let {a:name} = a:default
  endif
endfunction

"
function s:renewCachesOfAllModes()
  for m in g:fuf_modes 
    call fuf#{m}#renewCache()
  endfor
endfunction

"
function s:onBufEnter()
  for m in g:fuf_modes 
    call fuf#{m}#onBufEnter()
  endfor
endfunction

"
function s:onBufWritePost()
  for m in g:fuf_modes
    call fuf#{m}#onBufWritePost()
  endfor
endfunction

"
function s:onCommandPre()
  for m in filter(copy(g:fuf_modes), 'fuf#{v:val}#requiresOnCommandPre()')
      call fuf#{m}#onCommandPre(getcmdtype() . getcmdline())
  endfor
  " lets last entry become the newest in the history
  call histadd(getcmdtype(), getcmdline())
  " this is not mapped again (:help recursive_mapping)
  return "\<CR>"
endfunction

" }}}1
"=============================================================================
" INITIALIZATION {{{1

call s:initialize()

" }}}1
"=============================================================================
" vim: set fdm=marker:
