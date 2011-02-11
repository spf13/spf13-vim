"=============================================================================
" Copyright (c) 2007-2010 Takeshi NISHIDA
"
" GetLatestVimScripts: 1984 1 :AutoInstall: FuzzyFinder
"=============================================================================
" LOAD GUARD {{{1

try
  if !l9#guardScriptLoading(expand('<sfile>:p'), 702, 101, [])
    finish
  endif
catch /E117/
  echoerr '***** L9 library must be installed! *****'
  finish
endtry

" }}}1
"=============================================================================
" LOCAL FUNCTIONS {{{1

"
function s:initialize()
  "---------------------------------------------------------------------------
  call l9#defineVariableDefault('g:fuf_modesDisable'     , [ 'mrufile', 'mrucmd', ])
  call l9#defineVariableDefault('g:fuf_keyOpen'          , '<CR>')
  call l9#defineVariableDefault('g:fuf_keyOpenSplit'     , '<C-j>')
  call l9#defineVariableDefault('g:fuf_keyOpenVsplit'    , '<C-k>')
  call l9#defineVariableDefault('g:fuf_keyOpenTabpage'   , '<C-l>')
  call l9#defineVariableDefault('g:fuf_keyPreview'       , '<C-@>')
  call l9#defineVariableDefault('g:fuf_keyNextMode'      , '<C-t>')
  call l9#defineVariableDefault('g:fuf_keyPrevMode'      , '<C-y>')
  call l9#defineVariableDefault('g:fuf_keyPrevPattern'   , '<C-s>')
  call l9#defineVariableDefault('g:fuf_keyNextPattern'   , '<C-_>')
  call l9#defineVariableDefault('g:fuf_keySwitchMatching', '<C-\><C-\>')
  call l9#defineVariableDefault('g:fuf_dataDir'          , '~/.vim-fuf-data')
  call l9#defineVariableDefault('g:fuf_abbrevMap'        , {})
  call l9#defineVariableDefault('g:fuf_patternSeparator' , ';')
  call l9#defineVariableDefault('g:fuf_promptHighlight'  , 'Question')
  call l9#defineVariableDefault('g:fuf_ignoreCase'       , 1)
  call l9#defineVariableDefault('g:fuf_splitPathMatching', 1)
  call l9#defineVariableDefault('g:fuf_fuzzyRefining'    , 0)
  call l9#defineVariableDefault('g:fuf_smartBs'          , 1)
  call l9#defineVariableDefault('g:fuf_reuseWindow'      , 1)
  call l9#defineVariableDefault('g:fuf_timeFormat'       , '(%Y-%m-%d %H:%M:%S)')
  call l9#defineVariableDefault('g:fuf_learningLimit'    , 100)
  call l9#defineVariableDefault('g:fuf_enumeratingLimit' , 50)
  call l9#defineVariableDefault('g:fuf_maxMenuWidth'     , 78)
  call l9#defineVariableDefault('g:fuf_previewHeight'    , 0)
  call l9#defineVariableDefault('g:fuf_autoPreview'      , 0)
  call l9#defineVariableDefault('g:fuf_useMigemo'        , 0)
  "---------------------------------------------------------------------------
  call l9#defineVariableDefault('g:fuf_buffer_prompt'     , '>Buffer[]>')
  call l9#defineVariableDefault('g:fuf_buffer_switchOrder', 10)
  call l9#defineVariableDefault('g:fuf_buffer_mruOrder'   , 1)
  call l9#defineVariableDefault('g:fuf_buffer_keyDelete'  , '<C-]>')
  "---------------------------------------------------------------------------
  call l9#defineVariableDefault('g:fuf_file_prompt'     , '>File[]>')
  call l9#defineVariableDefault('g:fuf_file_switchOrder', 20)
  call l9#defineVariableDefault('g:fuf_file_exclude'    , '\v\~$|\.(o|exe|dll|bak|orig|sw[po])$|(^|[/\\])\.(hg|git|bzr)($|[/\\])')
  "---------------------------------------------------------------------------
  call l9#defineVariableDefault('g:fuf_coveragefile_prompt'     , '>CoverageFile[]>')
  call l9#defineVariableDefault('g:fuf_coveragefile_switchOrder', 30)
  call l9#defineVariableDefault('g:fuf_coveragefile_exclude'    , '\v\~$|\.(o|exe|dll|bak|orig|sw[po])$|(^|[/\\])\.(hg|git|bzr)($|[/\\])')
  call l9#defineVariableDefault('g:fuf_coveragefile_globPatterns', ['**/.*', '**/*'])
  "---------------------------------------------------------------------------
  call l9#defineVariableDefault('g:fuf_dir_prompt'     , '>Dir[]>')
  call l9#defineVariableDefault('g:fuf_dir_switchOrder', 40)
  call l9#defineVariableDefault('g:fuf_dir_exclude'    , '\v(^|[/\\])\.(hg|git|bzr)($|[/\\])')
  "---------------------------------------------------------------------------
  call l9#defineVariableDefault('g:fuf_mrufile_prompt'     , '>MRU-File[]>')
  call l9#defineVariableDefault('g:fuf_mrufile_switchOrder', 50)
  call l9#defineVariableDefault('g:fuf_mrufile_exclude'    , '\v\~$|\.(o|exe|dll|bak|orig|sw[po])$|^(\/\/|\\\\|\/mnt\/|\/media\/)')
  call l9#defineVariableDefault('g:fuf_mrufile_maxItem'    , 200)
  call l9#defineVariableDefault('g:fuf_mrufile_maxItemDir' , 50)
  call l9#defineVariableDefault('g:fuf_mrufile_keyExpand'  , '<C-]>')
  "---------------------------------------------------------------------------
  call l9#defineVariableDefault('g:fuf_mrucmd_prompt'     , '>MRU-Cmd[]>')
  call l9#defineVariableDefault('g:fuf_mrucmd_switchOrder', 60)
  call l9#defineVariableDefault('g:fuf_mrucmd_exclude'    , '^$')
  call l9#defineVariableDefault('g:fuf_mrucmd_maxItem'    , 200)
  "---------------------------------------------------------------------------
  call l9#defineVariableDefault('g:fuf_bookmarkfile_prompt'     , '>Bookmark-File[]>')
  call l9#defineVariableDefault('g:fuf_bookmarkfile_switchOrder', 70)
  call l9#defineVariableDefault('g:fuf_bookmarkfile_searchRange', 400)
  call l9#defineVariableDefault('g:fuf_bookmarkfile_keyDelete'  , '<C-]>')
  "---------------------------------------------------------------------------
  call l9#defineVariableDefault('g:fuf_bookmarkdir_prompt'     , '>Bookmark-Dir[]>')
  call l9#defineVariableDefault('g:fuf_bookmarkdir_switchOrder', 80)
  call l9#defineVariableDefault('g:fuf_bookmarkdir_keyDelete'  , '<C-]>')
  "---------------------------------------------------------------------------
  call l9#defineVariableDefault('g:fuf_tag_prompt'     , '>Tag[]>')
  call l9#defineVariableDefault('g:fuf_tag_switchOrder', 90)
  "---------------------------------------------------------------------------
  call l9#defineVariableDefault('g:fuf_buffertag_prompt'     , '>Buffer-Tag[]>')
  call l9#defineVariableDefault('g:fuf_buffertag_switchOrder', 100)
  call l9#defineVariableDefault('g:fuf_buffertag_ctagsPath'  , 'ctags')
  "---------------------------------------------------------------------------
  call l9#defineVariableDefault('g:fuf_taggedfile_prompt'     , '>Tagged-File[]>')
  call l9#defineVariableDefault('g:fuf_taggedfile_switchOrder', 110)
  "---------------------------------------------------------------------------
  call l9#defineVariableDefault('g:fuf_jumplist_prompt'     , '>Jump-List[]>')
  call l9#defineVariableDefault('g:fuf_jumplist_switchOrder', 120)
  "---------------------------------------------------------------------------
  call l9#defineVariableDefault('g:fuf_changelist_prompt'     , '>Change-List[]>')
  call l9#defineVariableDefault('g:fuf_changelist_switchOrder', 130)
  "---------------------------------------------------------------------------
  call l9#defineVariableDefault('g:fuf_quickfix_prompt'     , '>Quickfix[]>')
  call l9#defineVariableDefault('g:fuf_quickfix_switchOrder', 140)
  "---------------------------------------------------------------------------
  call l9#defineVariableDefault('g:fuf_line_prompt'     , '>Line[]>')
  call l9#defineVariableDefault('g:fuf_line_switchOrder', 150)
  "---------------------------------------------------------------------------
  call l9#defineVariableDefault('g:fuf_help_prompt'     , '>Help[]>')
  call l9#defineVariableDefault('g:fuf_help_switchOrder', 160)
  "---------------------------------------------------------------------------
  command! -bang -narg=0 FufEditDataFile call fuf#editDataFile()
  command! -bang -narg=0 FufRenewCache   call s:renewCachesOfAllModes()
  "---------------------------------------------------------------------------
  call fuf#addMode('buffer')
  call fuf#addMode('file')
  call fuf#addMode('coveragefile')
  call fuf#addMode('dir')
  call fuf#addMode('mrufile')
  call fuf#addMode('mrucmd')
  call fuf#addMode('bookmarkfile')
  call fuf#addMode('bookmarkdir')
  call fuf#addMode('tag')
  call fuf#addMode('buffertag')
  call fuf#addMode('taggedfile')
  call fuf#addMode('jumplist')
  call fuf#addMode('changelist')
  call fuf#addMode('quickfix')
  call fuf#addMode('line')
  call fuf#addMode('help')
  call fuf#addMode('givenfile')
  call fuf#addMode('givendir')
  call fuf#addMode('givencmd')
  call fuf#addMode('callbackfile')
  call fuf#addMode('callbackitem')
  "---------------------------------------------------------------------------
endfunction

"
function s:renewCachesOfAllModes()
  for m in fuf#getModeNames()
    call fuf#{m}#renewCache()
  endfor
endfunction

" }}}1
"=============================================================================
" INITIALIZATION {{{1

call s:initialize()

" }}}1
"=============================================================================
" vim: set fdm=marker:
