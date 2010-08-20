*showmarks.txt* Visually show the location of marks

               By Anthony Kruize <trandor@labyrinth.net.au>
                  Michael Geddes <michaelrgeddes@optushome.com.au>


ShowMarks provides a visual representation of |marks| local to a buffer.
Marks are useful for jumping back and forth between interesting points in a
buffer, but can be hard to keep track of without any way to see where you have
placed them.

ShowMarks hopefully makes life easier by placing a |sign| in the
leftmost column of the buffer.  The sign indicates the label of the mark and
its location.

ShowMarks is activated by the |CursorHold| |autocommand| which is triggered
every |updatetime| milliseconds.  This is set to 4000(4 seconds) by default.
If this is too slow, setting it to a lower value will make it more responsive.

Note: This plugin requires Vim 6.x compiled with the |+signs| feature.

===============================================================================
1. Contents                                    *showmarks* *showmarks-contents*

    1. Contents	             |showmarks-contents|
    2. Configuration         |showmarks-configuration|
    3. Highlighting          |showmarks-highlighting|
    4. Key mappings          |showmarks-mappings|
    5. Commands              |showmarks-commands|
    6. ChangeLog             |showmarks-changelog|

    Appendix
    A. Using marks           |marks|
    B. Using signs           |sign|
    C. Defining updatetime   |updatetime|
    D. Defining a mapleader  |mapleader|
    E. Defining highlighting |highlight|

===============================================================================
2. Configuration                                      *showmarks-configuration*

ShowMarks can be configured to suit your needs.
The following options can be added to your |vimrc| to change how ShowMarks
behaves:

                                                           *'showmarks_enable'*
'showmarks_enable'      boolean (default: 1)
                        global
   This option enables or disables ShowMarks on startup. Normally ShowMarks
   will be enabled when Vim starts, setting this to 0 will disable ShowMarks
   by default.
   ShowMarks can be turned back on using the |ShowMarksToggle| command.

                                                          *'showmarks_include'*
'showmarks_include'     string  (default:
"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.'`^<>[]{}()\"")
                        global or local to buffer
   This option specifies which marks will be shown and in which order if
   placed on the same line. Marks earlier in the list take precedence over
   marks later in the list.
   This option can also be specified as a buffer option which will override
   the global version.

   NOTE: When including the " mark, it must be escaped with a \.

   For example to only include marks 'abcdefzxABHJio', in that order:
>
     let g:showmarks_include="abcdefzxABJio"
<
   To override this for a particular buffer with 'ABCDhj.'^':
>
     let b:showmarks_include="abcdefzxABJio"
<
                                                      *'showmarks_ignore_type'*
'showmarks_ignore_type' string  (default: "hq")
                        global
   This option defines which types of buffers should be ignored.
   Each type is represented by a letter. This option is not case-sensitive.
   Valid buffer types are:
   - h : Help
   - m : Non-modifiable
   - p : Preview
   - q : Quickfix
   - r : Readonly

   For example to ignore help, preview and readonly files:
>
     let g:showmarks_ignore_type="hpr"
<
                                                      *'showmarks_ignore_name'*
'showmarks_textlower'   string  (default: ">" )
                        global
   This option defines how the marks a-z will be displayed.
   A maximum of two characters can be defined.
   To include the mark in the text use a tab(\t) character. A single
   character will display as the mark with the character suffixed (same as
   "\t<character>"). Specifying two characters will simply display those two
   characters.

   Some examples:
     To display the mark with a > suffixed: >
       let g:showmarks_textlower="\t>"
<         or >
       let g:showmarks_textlower=">"
<
     To display the mark with a ( prefixed: >
       let g:showmarks_textlower="(\t"
<
     To display two > characters: >
       let g:showmarks_textlower=">>"
<
                                                        *'showmarks_textupper'*
'showmarks_textupper'   string  (default: ">")
                        global
   This option defines how the marks A-Z will be displayed. It behaves the same
   as the |'showmarks_textlower'| option.

                                                        *'showmarks_textother'*
'showmarks_textother'   string  (default: ">")
                        global
   This option defines how all other marks will be displayed. It behaves the
   same as the |'showmarks_textlower'| option.

'showmarks_hlline_lower' boolean (default: 0)        *'showmarks_hlline_lower'*
                        global
   This option defines whether the entire line a lowercase mark is on will
   be highlighted.

'showmarks_hlline_upper' boolean (default: 0)        *'showmarks_hlline_upper'*
                        global
   This option defines whether the entire line an uppercase mark is on will
   be highlighted.

'showmarks_hlline_other' boolean (default: 0)        *'showmarks_hlline_other'*
                        global
   This option defines whether the entire line other marks are on will be
   highlighted.

===============================================================================
3. Highlighting                                        *showmarks-highlighting*

Four highlighting groups are used by ShowMarks to define the colours used to
highlight each of the marks.

  - ShowMarksHLl : This group is used to highlight all the lowercase marks.
  - ShowMarksHLu : This group is used to highlight all the uppercase marks.
  - ShowMarksHLo : This group is used to highlight all other marks.
  - ShowMarksHLm : This group is used when multiple marks are on the same line.

You can define your own highlighting by overriding these groups in your |vimrc|.
For example: >

  highlight ShowMarksHLl guifg=red guibg=green
<
Will set all lowercase marks to be red on green when running in GVim.
See |highlight| for more information.

===============================================================================
4. Mappings                                                *showmarks-mappings*

The following mappings are setup by default:

  <Leader>mt   - Toggles ShowMarks on and off.
  <Leader>mo   - Forces ShowMarks on.
  <Leader>mh   - Clears the mark at the current line.
  <Leader>ma   - Clears all marks in the current buffer.
  <Leader>mm   - Places the next available mark on the current line.

(see |mapleader| for how to setup the mapleader variable.)

===============================================================================
5. Commands                                                *showmarks-commands*

                                                              *ShowMarksToggle*
:ShowMarksToggle
   This command will toggle the display of marks on or off.


:ShowMarksOn                                                      *ShowMarksOn*
   This command will force the display of marks on.

                                                           *ShowMarksClearMark*
:ShowMarksClearMark
   This command will clear the mark on the current line.
   It doesn't actually remove the mark, it simply moves it to line 1 and
   removes the sign.

                                                            *ShowMarksClearAll*
:ShowMarksClearAll
   This command will clear all marks in the current buffer.
   It doesn't actually remove the marks, it simply moves them to line 1 and
   removes the signs.

                                                           *ShowMarksPlaceMark*
:ShowMarksPlaceMark
   This command will place the next available mark on the current line. This
   effectively automates mark placement so you don't have to remember which
   marks are placed or not. Hidden marks are considered to be available.
   NOTE: Only marks a-z are supported by this function.

===============================================================================
6. ChangeLog                                              *showmarks-changelog*

2.2 - 2004-08-17
   Fixed highlighting of the A-Z marks when ignorecase is on. (Mike Kelly)
   Fixed the delay with ShowMarks triggering when entering a buffer for the
     first time. (Mikolaj Machowski)
   Added support for highlighting the entire line where a mark is placed.
   Now uses HelpExtractor by Charles E. Campbell to install the help file.

2.1 - 2004-03-04
   Added ShowMarksOn. It forces ShowMarks to be enabled whether it's on or not.
     (Gary Holloway)
   Marks now have a definable order of precedence for when mulitple alpha marks
     have been placed on the same line. A new highlight group, ShowMarksHLm is
     used to identify this situation. (Gary Holloway)
       - showmarks_include has changed accordingly.
       - ShowMarksHL is now ShowMarksHLl.
   ShowMarksPlaceMark now places marks in the order specified by
     showmarks_include. (Gary Holloway)
   showmarks_include can now be specified per buffer. (Gary Holloway)

2.0 - 2003-08-11
   Added ability to ignore buffers by type.
   Fixed toggling ShowMarks off when switching buffers.
   ShowMarksHideMark and ShowMarksHideAll have been renamed to
     ShowMarksClearMark and ShowMarksClearAll.
   Marks a-z, A-Z and others now have different highlighting from each other.
   Added support for all other marks. (Gary Holloway)
   Enhanced customization of how marks are displayed by allowing a prefix to
     be specified.(Gary Holloway & Anthony Kruize)
   Fixed CursorHold autocmd triggering even when ShowMarks is disabled.
     (Charles E. Campbell)

1.5 - 2002-07-16
   Added ability to customize how the marks are displayed.

1.4 - 2002-05-29
   Added support for placing the next available mark.
     (Thanks to Shishir Ramam for the idea)
   Added support for hiding all marks.
   Marks on line 1 are no longer shown. This stops hidden marks from
     reappearing when the file is opened again.
   Added a help file.

1.3 - 2002-05-20
   Fixed toggling ShowMarks not responding immediately.
   Added user commands for toggling/hiding marks.
   Added ability to disable ShowMarks by default.

1.2 - 2002-03-06
   Added a check that Vim was compiled with +signs support.
   Added the ability to define which marks are shown.
   Removed debugging code that was accidently left in.

1.1 - 2002-02-05
   Added support for the A-Z marks.
   Fixed sign staying placed if the line it was on is deleted.
   Clear autocommands before making new ones.

1.0 - 2001-11-20
   First release.

vim:tw=78:ts=8:ft=help
