;; .yas-setup.el for html-mode

(defvar yas/html-default-tag "p")

(defvar yas/html-xhtml-attr "")

(defvar yas/html-just-like-tm nil
  "Html-mode snippets behave as close to TextMate as possible.")

(defun yas/html-activate ()
  (add-to-list (make-local-variable 'yas/mode-symbol) 'html-mode))

(add-hook 'nxml-mode-hook 'yas/html-activate)
(add-hook 'rhtml-mode-hook 'yas/html-activate)

(defun yas/html-remove-preceding-word ()
  (interactive)
  (let (word-begin
        word-end
        (line-beginning-position (line-beginning-position))
        (orig-point (point))
        retval)
    (save-excursion
      (when (and (forward-word -1)
                 (setq word-begin (point))
                 (forward-word 1)
                 (setq word-end (point))
                 (< word-begin orig-point)
                 (>= word-end orig-point)
                 (<= (line-beginning-position) word-begin)
                 ;; (not (string-match "^[\s\t]+$" "          "))
                 )
      (setq retval
            (cons
             (buffer-substring-no-properties word-begin orig-point)
             (buffer-substring-no-properties word-end orig-point)))
      (delete-region word-begin word-end)
      retval))))


(defun yas/html-first-word (string)
  (replace-regexp-in-string "\\\W.*" "" string))

(defun yas/html-insert-tag-pair-snippet ()
  (let* ((tag-and-suffix (or (and yas/selected-text
                                  (cons yas/selected-text nil))
                             (yas/html-remove-preceding-word)))
         (tag    (car tag-and-suffix))
         (suffix (or (cdr tag-and-suffix) ""))
         (single-no-arg "\\(br\\|hr\\)")
         (single        "\\(img\\|meta\\|link\\|input\\|base\\|area\\|col\\|frame\\|param\\)"))
    (cond ((null tag)
           (yas/expand-snippet (format "<${1:%s}>%s</${1:$(yas/html-first-word yas/text)}>%s"
                                       (or yas/html-default-tag
                                           "p")
                                       (if yas/html-just-like-tm "$2" "$0")
                                       suffix)))
          ((string-match single-no-arg tag)
           (insert (format "<%s%s/>%s" tag yas/html-xhtml-attr suffix)))
          ((string-match single tag)
           (yas/expand-snippet (format "<%s $1%s/>%s" tag yas/html-xhtml-attr suffix)))
          (t
           (yas/expand-snippet (format "<%s>%s</%s>%s"
                                       tag
                                       (if yas/html-just-like-tm "$1" "$0")
                                       (replace-regexp-in-string "\\\W.*" "" tag)
                                       suffix))))))

(defun yas/html-wrap-each-line-in-openclose-tag ()
  (let* ((mirror "${1:$(yas/html-first-word yas/text)}")
         (yas/html-wrap-newline (when (string-match "\n" yas/selected-text) "\n"))
         (template (concat (format "<${1:%s}>" (or yas/html-default-tag "p"))
                           yas/selected-text
                           "</" mirror ">")))
    (setq template (replace-regexp-in-string "\n" (concat "</" mirror ">\n<$1>") template))
    (yas/expand-snippet template)))

(defun yas/html-toggle-wrap (string wrap)
  (or (and string
           (string-match (format "<%s>\\(.*\\)</%s>" wrap wrap)
                         string)
           (match-string 1 string))
      (concat wrap string wrap)))

(defun yas/html-between-tag-pair-p ()
  (save-excursion
    (backward-word)
    (looking-at "\\\w+></\\\w+>")))

(defun yas/html-id-from-string (string)
  (replace-regexp-in-string " " "_" (downcase string)))

(defun yas/html-tidy ()
  (interactive)
  (let ((start (or (and mark-active
                        (region-beginning))
                   (point-min)))
        (end (or (and mark-active
                      (region-end))
                 (point-max)))
        (orig (point))
        (orig-line (count-screen-lines (window-start) (line-beginning-position))))
    (shell-command-on-region start end "tidy" (current-buffer) t (get-buffer-create "*tidy errors*") t)
    (goto-char (min (point-max) orig))
    (recenter (1- orig-line))))

(defun yas/html-tag-description ()
  (interactive)
  (let* ((tag-at-point (sgml-beginning-of-tag))
         (fragment (and tag-at-point
                        (aget yas/html-tag-description-urls (upcase tag-at-point)))))
    (if fragment
        (browse-url (concat "http://www.w3.org/TR/html4/index/"
                            fragment))
      (if tag-at-point
          (message "No documentation for " tag-at-point)
        (message "Not on a HTML tag.")))))

(defvar yas/html-tag-description-urls
  '(("A"           . "../struct/links.html#edef-A")
    ("ABBR"        . "../struct/text.html#edef-ABBR")
    ("ACRONYM"     . "../struct/text.html#edef-ACRONYM")
    ("ADDRESS"     . "../struct/global.html#edef-ADDRESS")
    ("APPLET"      . "../struct/objects.html#edef-APPLET")
    ("AREA"        . "../struct/objects.html#edef-AREA")
    ("B"           . "../present/graphics.html#edef-B")
    ("BASE"        . "../struct/links.html#edef-BASE")
    ("BASEFONT"    . "../present/graphics.html#edef-BASEFONT")
    ("BDO"         . "../struct/dirlang.html#edef-BDO")
    ("BIG"         . "../present/graphics.html#edef-BIG")
    ("BLOCKQUOTE"  . "../struct/text.html#edef-BLOCKQUOTE")
    ("BODY"        . "../struct/global.html#edef-BODY")
    ("BR"          . "../struct/text.html#edef-BR")
    ("BUTTON"      . "../interact/forms.html#edef-BUTTON")
    ("CAPTION"     . "../struct/tables.html#edef-CAPTION")
    ("CENTER"      . "../present/graphics.html#edef-CENTER")
    ("CITE"        . "../struct/text.html#edef-CITE")
    ("CODE"        . "../struct/text.html#edef-CODE")
    ("COL"         . "../struct/tables.html#edef-COL")
    ("COLGROUP"    . "../struct/tables.html#edef-COLGROUP")
    ("DD"          . "../struct/lists.html#edef-DD")
    ("DEL"         . "../struct/text.html#edef-del")
    ("DFN"         . "../struct/text.html#edef-DFN")
    ("DIR"         . "../struct/lists.html#edef-DIR")
    ("DIV"         . "../struct/global.html#edef-DIV")
    ("DL"          . "../struct/lists.html#edef-DL")
    ("DT"          . "../struct/lists.html#edef-DT")
    ("EM"          . "../struct/text.html#edef-EM")
    ("FIELDSET"    . "../interact/forms.html#edef-FIELDSET")
    ("FONT"        . "../present/graphics.html#edef-FONT")
    ("FORM"        . "../interact/forms.html#edef-FORM")
    ("FRAME"       . "../present/frames.html#edef-FRAME")
    ("FRAMESET"    . "../present/frames.html#edef-FRAMESET")
    ("H1"          . "../struct/global.html#edef-H1")
    ("H2"          . "../struct/global.html#edef-H2")
    ("H3"          . "../struct/global.html#edef-H3")
    ("H4"          . "../struct/global.html#edef-H4")
    ("H5"          . "../struct/global.html#edef-H5")
    ("H6"          . "../struct/global.html#edef-H6")
    ("HEAD"        . "../struct/global.html#edef-HEAD")
    ("HR"          . "../present/graphics.html#edef-HR")
    ("HTML"        . "../struct/global.html#edef-HTML")
    ("I"           . "../present/graphics.html#edef-I")
    ("IFRAME"      . "../present/frames.html#edef-IFRAME")
    ("IMG"         . "../struct/objects.html#edef-IMG")
    ("INPUT"       . "../interact/forms.html#edef-INPUT")
    ("INS"         . "../struct/text.html#edef-ins")
    ("ISINDEX"     . "../interact/forms.html#edef-ISINDEX")
    ("KBD"         . "../struct/text.html#edef-KBD")
    ("LABEL"       . "../interact/forms.html#edef-LABEL")
    ("LEGEND"      . "../interact/forms.html#edef-LEGEND")
    ("LI"          . "../struct/lists.html#edef-LI")
    ("LINK"        . "../struct/links.html#edef-LINK")
    ("MAP"         . "../struct/objects.html#edef-MAP")
    ("MENU"        . "../struct/lists.html#edef-MENU")
    ("META"        . "../struct/global.html#edef-META")
    ("NOFRAMES"    . "../present/frames.html#edef-NOFRAMES")
    ("NOSCRIPT"    . "../interact/scripts.html#edef-NOSCRIPT")
    ("OBJECT"      . "../struct/objects.html#edef-OBJECT")
    ("OL"          . "../struct/lists.html#edef-OL")
    ("OPTGROUP"    . "../interact/forms.html#edef-OPTGROUP")
    ("OPTION"      . "../interact/forms.html#edef-OPTION")
    ("P"           . "../struct/text.html#edef-P")
    ("PARAM"       . "../struct/objects.html#edef-PARAM")
    ("PRE"         . "../struct/text.html#edef-PRE")
    ("Q"           . "../struct/text.html#edef-Q")
    ("S"           . "../present/graphics.html#edef-S")
    ("SAMP"        . "../struct/text.html#edef-SAMP")
    ("SCRIPT"      . "../interact/scripts.html#edef-SCRIPT")
    ("SELECT"      . "../interact/forms.html#edef-SELECT")
    ("SMALL"       . "../present/graphics.html#edef-SMALL")
    ("SPAN"        . "../struct/global.html#edef-SPAN")
    ("STRIKE"      . "../present/graphics.html#edef-STRIKE")
    ("STRONG"      . "../struct/text.html#edef-STRONG")
    ("STYLE"       . "../present/styles.html#edef-STYLE")
    ("SUB"         . "../struct/text.html#edef-SUB")
    ("SUP"         . "../struct/text.html#edef-SUP")
    ("TABLE"       . "../struct/tables.html#edef-TABLE")
    ("TBODY"       . "../struct/tables.html#edef-TBODY")
    ("TD"          . "../struct/tables.html#edef-TD")
    ("TEXTAREA"    . "../interact/forms.html#edef-TEXTAREA")
    ("TFOOT"       . "../struct/tables.html#edef-TFOOT")
    ("TH"          . "../struct/tables.html#edef-TH")
    ("THEAD"       . "../struct/tables.html#edef-THEAD")
    ("TITLE"       . "../struct/global.html#edef-TITLE")
    ("TR"          . "../struct/tables.html#edef-TR")
    ("TT"          . "../present/graphics.html#edef-TT")
    ("U"           . "../present/graphics.html#edef-U")
    ("UL"          . "../struct/lists.html#edef-UL")
    ("VAR"         . "../struct/text.html#edef-VAR")))

;;
;;
;; Substitutions for: content
;; # as in Snippets/Emphasize.yasnippet
;; ${TM_SELECTED_TEXT/\A<em>(.*)<\/em>\z|.*/(?1:$1:<em>$0<\/em>)/m}                    =yyas> `(yas/html-toggle-wrap yas/selected-text "em")`
;; ${TM_SELECTED_TEXT/\A<strong>(.*)<\/strong>\z|.*/(?1:$1:<strong>$0<\/strong>)/m}    =yyas> `(yas/html-toggle-wrap yas/selected-text "strong")`
;; ${1/\s.*//}                                                                         =yyas> ${1:$(replace-regexp-in-string "[\s\t\n].*" "" yas/text)}
;; ${1/[[:alpha:]]+|( )/(?1:_:\L$0)/g}                                                 =yyas> ${1:$(replace-regexp-in-string " " "_" (downcase yas/text))}
;; ${TM_XHTML}                                                                         =yyas> `yas/html-xhtml-attr`


;; # as in Commands/Preview in All Active Browsers.yasnippet
;; 970EE6B4-A091-11D9-A5A2-000D93C8BE28                                                       =yyas> (browse-url-of-buffer)
;; 637CEA2B-578C-429C-BB74-30E8D42BFA22                                                       =yyas> (yas/html-tag-description)
;; 2ED44A32-C353-447F-BAE4-E3522DB6944D                                                       =yyas> (yas/html-insert-tag-pair-snippet)
;; 991E7EBD-F3F5-469A-BA01-DC30E04AD472                                                       =yyas> (yas/html-wrap-each-line-in-openclose-tag)

;; Substitutions for: binding
;; 
;; # as in Snippets/Strong.yasnippet
;; @b                                                                                         =yyas> s-b
;; 
;; # as in Snippets/Emphasize.yasnippet
;; ^@i                                                                                        =yyas>   
;; @i                                                                                         =yyas> s-i
;; 
;; # as in Snippets/Wrap Selection In Tag.yasnippet
;; ^W                                                                                         =yyas> C-c M-w
;; 
;; # as in Commands/Insert Tag Pair.yasnippet
;; ^<                                                                                         =yyas> C-<
;;
;; # as in Commands/Documentation for Tag.yasnippet
;; ^h                                                                                         =yyas> C-c M-h
;; 
;; # as in Commands/Wrap Each Selected Line in OpenClose Tag.yasnippet
;; ^@W                                                                                        =yyas> C-c M-W
;; 
;; # as in Snippets/XHTML &nbsp NonBreakingSpace.yasnippet
;; ~                                                                                          =yyas> (yas/unknown)
;; 
;; # as in Commands/Insert Entity.yasnippet
;; @&                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Refresh All Active Browsers.yasnippet
;; @r                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Persistent Include.yasnippet
;; ^@i                                                                                        =yyas> (yas/unknown)
;; 
;; # as in Commands/CodeCompletion HTML Tags.yasnippet
;; ~                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Update Includes.yasnippet
;; ^@u                                                                                        =yyas> (yas/unknown)
;; 
;; # as in Macros/Delete whitespace between tags.yasnippet
;; ^~                                                                                      =yyas> (yas/unknown)
;; 
;; # as in Commands/Tidy.yasnippet
;; ^H                                                                                         =yyas> (yas/unknown)
;; 
;;
;; --**--
;; Automatically generated code, do not edit this part
;; 
;; Translated menu
;; 
(yas/define-menu 'html-mode
                 '(;; Documentation for Tag
                   (yas/item "637CEA2B-578C-429C-BB74-30E8D42BFA22")
                   ;; Ignoring Validate Syntax (W3C)
                   (yas/ignore-item "3F26240E-6E4A-11D9-B411-000D93589AF6")
                   
                   ;; Open Document in Running Browser(s)
                   (yas/item "970EE6B4-A091-11D9-A5A2-000D93C8BE28")
                   ;; Ignoring Refresh Running Browser(s)
                   (yas/ignore-item "B8651C6E-A05E-11D9-86AC-000D93C8BE28")
                   
                   (yas/submenu "Entities"
                                (;; Ignoring Convert Character / Selection to Entities
                                 (yas/ignore-item "3DD8406C-A116-11D9-A5A2-000D93C8BE28")
                                 ;; Ignoring Convert Character / Selection to Entities Excl. Tags
                                 (yas/ignore-item "43C9E8AE-3E53-4B82-A1AF-56697BB3EF09")
                                 ;; Ignoring Decode Entities in Line / Selection
                                 (yas/ignore-item "C183920D-A126-11D9-A5A2-000D93C8BE28")
                                 
                                 ;; Non-Breaking Space
                                 (yas/item "73B40BAE-A295-11D9-87F7-000D93C8BE28")
                                 ;; →
                                 (yas/item "C70BB693-0954-4440-AEB4-F2ADD6D923F0")
                                 ;; ←
                                 (yas/item "C0418A4A-7E42-4D49-8F86-6E339296CB84")
                                 ;; ⇤
                                 (yas/item "7F102705-27D8-4029-BF61-2F042FB61E06")
                                 ;; ⌅
                                 (yas/item "7062316B-4236-4793-AD35-05E4A6577393")
                                 ;; ⌃
                                 (yas/item "B4987DA5-9C2F-4D2D-AC14-678115079205")
                                 ;; ⌦
                                 (yas/item "44E448B6-37CE-4BFE-8611-C5113593B74B")
                                 ;; ↩
                                 (yas/item "9B216475-D73D-4518-851F-CACD0066A909")
                                 ;; ⇥
                                 (yas/item "ADC78A82-40C2-4AAC-8968-93AF0ED98DF0")
                                 ;; ⌫
                                 (yas/item "38E50882-27AF-4246-A039-355C3E1A699E")
                                 ;; ⌘
                                 (yas/item "7214ACD1-93D9-4D3F-A428-8A7302E0A35E")
                                 ;; ↓
                                 (yas/item "35654B4E-2D76-4CD3-8FBB-2DA1F314BA19")
                                 ;; →
                                 (yas/item "AC15621A-8A16-40DD-A671-EA4C37637215")
                                 ;; ↑
                                 (yas/item "0E2F4A47-EADE-4A05-931E-FC874FA28FC3")
                                 ;; ⇧
                                 (yas/item "1B8D58B9-D9DB-484C-AACD-5D5DF5385308")
                                 ;; ⎋
                                 (yas/item "D7CC7C7C-CD01-4357-AF91-AEFFD914DF98")
                                 ;; ⌥
                                 (yas/item "980A8D39-CA8B-4EC2-9739-DC36A262F28E")
                                 (yas/separator)
                                 ;; Ignoring Insert Entity…
                                 (yas/ignore-item "89E5CC0A-3EFF-4DEF-A299-2E9651DE6529")))
                   (yas/submenu "URL Escapes"
                                (;; Ignoring URL Escape Line / Selection
                                 (yas/ignore-item "6B024865-6095-4CE3-8EDD-DC6F2230C2FF")
                                 ;; Ignoring URL Unescape Line / Selection
                                 (yas/ignore-item "2C4C9673-B166-432A-8938-75A5CA622481")))
                   ;; Ignoring Encrypt Line / Selection (ROT 13)
                   (yas/ignore-item "9B13543F-8356-443C-B6E7-D9259B604927")
                   
                   ;; Ignoring CodeCompletion HTML Attributes
                   (yas/ignore-item "CBD82CF3-74E9-4E7A-B3F6-9348754EB5AA")
                   ;; Insert Open/Close Tag (With Current Word)
                   (yas/item "2ED44A32-C353-447F-BAE4-E3522DB6944D")
                   ;; Ignoring Insert Close Tag
                   (yas/ignore-item "0658019F-3635-462E-AAC2-74E4FE508A9B")
                   (yas/submenu "Insert DocType"
                                (;; HTML — 4.01 Strict
                                 (yas/item "944F1410-188C-4D70-8340-CECAA56FC7F2")
                                 ;; HTML — 4.01 Transitional
                                 (yas/item "B2AAEE56-42D8-42C3-8F67-865473F50E8D")
                                 (yas/separator)
                                 ;; XHTML — 1.0 Frameset
                                 (yas/item "9ED6ABBE-A802-11D9-BFC8-000D93C8BE28")
                                 ;; XHTML — 1.0 Strict
                                 (yas/item "C8B83564-A802-11D9-BFC8-000D93C8BE28")
                                 ;; XHTML — 1.0 Transitional
                                 (yas/item "7D8C2F74-A802-11D9-BFC8-000D93C8BE28")
                                 ;; XHTML — 1.1
                                 (yas/item "5CE8FC6E-A802-11D9-BFC8-000D93C8BE28")))
                   (yas/submenu "Insert Tag"
                                (;; Ignoring CodeCompletion HTML Tags
                                 (yas/ignore-item "3463E85F-F500-49A0-8631-D78ED85F9D60")
                                 
                                 ;; Base
                                 (yas/item "4462A6B8-A08A-11D9-A5A2-000D93C8BE28")
                                 ;; Body
                                 (yas/item "4905D47B-A08B-11D9-A5A2-000D93C8BE28")
                                 ;; Br
                                 (yas/item "3E008E42-A5C9-11D9-9BCD-000D93C8BE28")
                                 ;; Div
                                 (yas/item "576036C0-A60E-11D9-ABD6-000D93C8BE28")
                                 ;; Embed QT Movie
                                 (yas/item "42F15753-9B6D-4DD8-984C-807B94363277")
                                 ;; Fieldset
                                 (yas/item "9BD2BE01-A854-4D55-B584-725D04C075C0")
                                 ;; Form
                                 (yas/item "232C2E8B-A08E-11D9-A5A2-000D93C8BE28")
                                 ;; Head
                                 (yas/item "9CF008C4-A086-11D9-A5A2-000D93C8BE28")
                                 ;; Heading
                                 (yas/item "65BA66DC-A07F-11D9-A5A2-000D93C8BE28")
                                 ;; Input
                                 (yas/item "44180979-A08E-11D9-A5A2-000D93C8BE28")
                                 ;; Input with Label
                                 (yas/item "D8DCCC81-749A-4E2A-B4BC-D109D5799CAA")
                                 ;; Link
                                 (yas/item "77BFD0C0-A08A-11D9-A5A2-000D93C8BE28")
                                 ;; Mail Anchor
                                 (yas/item "81DA4C74-A530-11D9-9BCD-000D93C8BE28")
                                 ;; Meta
                                 (yas/item "DA99AC44-A083-11D9-A5A2-000D93C8BE28")
                                 ;; Option
                                 (yas/item "5820372E-A093-4F38-B25C-B0CCC50A0FC4")
                                 ;; Script
                                 (yas/item "6592050A-A087-11D9-A5A2-000D93C8BE28")
                                 ;; Script With External Source
                                 (yas/item "7D676C4C-A087-11D9-A5A2-000D93C8BE28")
                                 ;; Select Box
                                 (yas/item "26023CFF-C73F-4EF5-9803-E4DBA2CBEADD")
                                 ;; Style
                                 (yas/item "3C518074-A088-11D9-A5A2-000D93C8BE28")
                                 ;; Table
                                 (yas/item "57176082-A12F-11D9-A5A2-000D93C8BE28")
                                 ;; Text Area
                                 (yas/item "AAC9D7B8-A12C-11D9-A5A2-000D93C8BE28")
                                 ;; Title
                                 (yas/item "B62ECABE-A086-11D9-A5A2-000D93C8BE28")))
                   
                   (yas/submenu "Includes"
                                (;; Ignoring Add Persistent Include
                                 (yas/ignore-item "0D814247-7A00-46EE-A2A4-45FBBF4B1181")
                                 ;; Ignoring Update Document
                                 (yas/ignore-item "4400BCE9-20E3-426E-B1D7-2C0BCA53BCF8")
                                 ;; Ignoring Help: Persistent Includes
                                 (yas/ignore-item "9AFDEB2C-D9F0-423E-8211-EBB089F51F0C")))
                   (yas/submenu "Format"
                                (;; Strong
                                 (yas/item "4117D930-B6FA-4022-97E7-ECCAF4E70F63")
                                 ;; Emphasize
                                 (yas/item "EBB98620-3292-4621-BA38-D8A9A65D9551")))
                   (yas/submenu "Conditional Comments"
                                (;; IE Conditional Comment: Internet Explorer
                                 (yas/item "0ED6DA73-F38F-4A65-B18F-3379D2BA9387")
                                 ;; IE Conditional Comment: Internet Explorer 5.0 only
                                 (yas/item "3A517A94-001E-464D-8184-1FE56D0D0D70")
                                 ;; IE Conditional Comment: Internet Explorer 5.5 only
                                 (yas/item "E3F8984E-7269-4981-9D30-967AB56A6ACE")
                                 ;; IE Conditional Comment: Internet Explorer 5.x
                                 (yas/item "F3512848-7889-45DA-993B-0547976C8E6D")
                                 ;; IE Conditional Comment: Internet Explorer 6 and below
                                 (yas/item "32BBB9AB-8732-4F91-A587-354941A27B69")
                                 ;; IE Conditional Comment: Internet Explorer 6 only
                                 (yas/item "48DF7485-52EA-49B3-88AF-3A41F933F325")
                                 ;; IE Conditional Comment: Internet Explorer 7 and above
                                 (yas/item "CBC24AF4-88E0-498B-BE50-934B9CF29EC7")
                                 ;; IE Conditional Comment: NOT Internet Explorer
                                 (yas/item "F00170EE-4A82-413F-A88B-85293E69A88B")))
                   
                   ;; Wrap Selection in Open/Close Tag
                   (yas/item "BC8B8AE2-5F16-11D9-B9C3-000D93589AF6")
                   ;; Wrap Each Selected Line in Open/Close Tag
                   (yas/item "991E7EBD-F3F5-469A-BA01-DC30E04AD472")
                   ;; Wrap in <?= … ?>
                   (yas/item "912906A0-9A29-434B-AE98-E9DFDE6E48B4")
                   (yas/separator)
                   ;; Ignoring Strip HTML Tags from Document / Selection
                   (yas/ignore-item "20D760B5-A127-11D9-A5A2-000D93C8BE28")
                   ;; Ignoring Tidy
                   (yas/ignore-item "45F92B81-6F0E-11D9-A1E4-000D9332809C"))
                    '("7B7E945E-A112-11D9-A5A2-000D93C8BE28"
                       "3C44EABE-8D6F-4B1B-AB91-F419FAD1A0AD"
                       "9AFDEB2C-D9F0-423E-8211-EBB089F51F0C"
                       "CBD82CF3-74E9-4E7A-B3F6-9348754EB5AA"
                       "3463E85F-F500-49A0-8631-D78ED85F9D60"
                       "9B13543F-8356-443C-B6E7-D9259B604927"
                       "0D814247-7A00-46EE-A2A4-45FBBF4B1181"
                       "4400BCE9-20E3-426E-B1D7-2C0BCA53BCF8"
                       "6B024865-6095-4CE3-8EDD-DC6F2230C2FF"
                       "3DD8406C-A116-11D9-A5A2-000D93C8BE28"
                       "43C9E8AE-3E53-4B82-A1AF-56697BB3EF09"
                       "C183920D-A126-11D9-A5A2-000D93C8BE28"
                       "2C4C9673-B166-432A-8938-75A5CA622481"
                       "0658019F-3635-462E-AAC2-74E4FE508A9B"
                       "89E5CC0A-3EFF-4DEF-A299-2E9651DE6529"
                       "B8651C6E-A05E-11D9-86AC-000D93C8BE28"
                       "20D760B5-A127-11D9-A5A2-000D93C8BE28"
                       "45F92B81-6F0E-11D9-A1E4-000D9332809C"
                       "3F26240E-6E4A-11D9-B411-000D93589AF6"
                       "B23D6E15-6B33-11D9-86C1-000D93589AF6"
                       "C8B717C2-6B33-11D9-BB47-000D93589AF6"
                       "CD6D2CC6-6B33-11D9-BDFD-000D93589AF6"
                       "7B7E945E-A112-11D9-A5A2-000D93C8BE28"
                       "04332FA8-8157-46C4-9854-8C190FFD96C6"
                       "E6F19171-F664-4B4F-92DA-3E15E6CAD35C"
                       "26068A55-4C84-409D-BA00-162B55AF6961"
                       "EBEE6B51-29C7-4362-818F-A190CACD5296"
                       "65D38039-6B0A-48E9-9E49-43832ECC4107"
                       "CDE8EFD6-9DE2-4E8C-BB6A-52E8CCD2E977"))

;; Unknown substitutions
;; 
;; Substitutions for: content
;; 
;; # as in Templates/XHTML 1.1/info.yasnippet
;; CDE8EFD6-9DE2-4E8C-BB6A-52E8CCD2E977                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/IE Conditional Comment Internet Explorer.yasnippet
;; `(or (yas/selected-text) "       IE Conditional Comment: Internet Explorer          ")`    =yyas> (yas/unknown)
;; 
;; # as in Snippets/IE Conditional Comment Internet Explorer 5_0 only.yasnippet
;; `(or (yas/selected-text) "   IE Conditional Comment: Internet Explorer 5.0 only ")`        =yyas> (yas/unknown)
;; 
;; # as in Commands/CodeCompletion HTML Tags.yasnippet
;; 3463E85F-F500-49A0-8631-D78ED85F9D60                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/Emphasize.yasnippet
;; `(yas/html-toggle-wrap yas/selected-text "em")`                                            =yyas> (yas/unknown)
;; 
;; # as in Templates/HTML 4.0 Transitional/info.yasnippet
;; E6F19171-F664-4B4F-92DA-3E15E6CAD35C                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/XHTML form.yasnippet
;; ${TM_FILENAME/(.*?)\..*/$1_submit/}                                                        =yyas> (yas/unknown)
;; 
;; # as in Snippets/XHTML body.yasnippet
;; ${TM_FILENAME/(.*)\..*/\L$1/}                                                              =yyas> (yas/unknown)
;; 
;; # as in Commands/Insert Entity.yasnippet
;; 89E5CC0A-3EFF-4DEF-A299-2E9651DE6529                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Convert to HTML Entities.yasnippet
;; 3DD8406C-A116-11D9-A5A2-000D93C8BE28                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Persistent Include.yasnippet
;; 0D814247-7A00-46EE-A2A4-45FBBF4B1181                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Refresh All Active Browsers.yasnippet
;; B8651C6E-A05E-11D9-86AC-000D93C8BE28                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/Strong.yasnippet
;; `(yas/html-toggle-wrap yas/selected-text "strong")`                                        =yyas> (yas/unknown)
;; 
;; # as in Snippets/IE Conditional Comment Internet Explorer 5_5 only.yasnippet
;; `(or (yas/selected-text) "   IE Conditional Comment: Internet Explorer 5.5 only ")`        =yyas> (yas/unknown)
;; 
;; # as in DragCommands/CSS Link.yasnippet
;; C8B717C2-6B33-11D9-BB47-000D93589AF6                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Insert Close Tag.yasnippet
;; 0658019F-3635-462E-AAC2-74E4FE508A9B                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Decode Numeric URL Escapes in Line Selection.yasnippet
;; 2C4C9673-B166-432A-8938-75A5CA622481                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Convert to named entities excl tags.yasnippet
;; 43C9E8AE-3E53-4B82-A1AF-56697BB3EF09                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/About Persistent Includes.yasnippet
;; 9AFDEB2C-D9F0-423E-8211-EBB089F51F0C                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/XHTML meta.yasnippet
;; `yas/html-xhtml-attr`                                                                      =yyas> (yas/unknown)
;; 
;; # as in Templates/HTML 4.0 Strict/info.yasnippet
;; 04332FA8-8157-46C4-9854-8C190FFD96C6                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Delete whitespace between tags.yasnippet
;; 7B7E945E-A112-11D9-A5A2-000D93C8BE28                                                       =yyas> (yas/unknown)
;; 
;; # as in DragCommands/Anchor Tag.yasnippet
;; B23D6E15-6B33-11D9-86C1-000D93589AF6                                                       =yyas> (yas/unknown)
;; 
;; # as in Templates/XHTML 1.0 Transitional/info.yasnippet
;; 65D38039-6B0A-48E9-9E49-43832ECC4107                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/XHTML title.yasnippet
;; ${TM_FILENAME/((.+)\..*)?/(?2:$2:Page Title)/}                                             =yyas> (yas/unknown)
;; 
;; # as in Commands/Tidy.yasnippet
;; 45F92B81-6F0E-11D9-A1E4-000D9332809C                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Strip HTML tags.yasnippet
;; 20D760B5-A127-11D9-A5A2-000D93C8BE28                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Encrypt Line Selection (ROT 13).yasnippet
;; 9B13543F-8356-443C-B6E7-D9259B604927                                                       =yyas> (yas/unknown)
;; 
;; # as in Templates/XHTML 1.0 Strict/info.yasnippet
;; EBEE6B51-29C7-4362-818F-A190CACD5296                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/W3C validation.yasnippet
;; 3F26240E-6E4A-11D9-B411-000D93589AF6                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Convert Line Selection to URL Escapes.yasnippet
;; 6B024865-6095-4CE3-8EDD-DC6F2230C2FF                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Update Includes.yasnippet
;; 4400BCE9-20E3-426E-B1D7-2C0BCA53BCF8                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/CodeCompletion HTML Attributes.yasnippet
;; CBD82CF3-74E9-4E7A-B3F6-9348754EB5AA                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/IE Conditional Comment Internet Explorer 6 and below.yasnippet
;; `(or (yas/selected-text) " IE Conditional Comment: Internet Explorer 6 and below ")`       =yyas> (yas/unknown)
;; 
;; # as in Snippets/IE Conditional Comment Internet Explorer 5_x.yasnippet
;; `(or (yas/selected-text) "  IE Conditional Comment: Internet Explorer 5.x      ")`         =yyas> (yas/unknown)
;; 
;; # as in DragCommands/Image Tag.yasnippet
;; CD6D2CC6-6B33-11D9-BDFD-000D93589AF6                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/IE Conditional Comment NOT Internet Explorer.yasnippet
;; `(or (yas/selected-text) "  IE Conditional Comment: NOT Internet Explorer      ")`         =yyas> (yas/unknown)
;; 
;; # as in Snippets/XHTML h1.yasnippet
;; `yas/selected-text`                                                                        =yyas> (yas/unknown)
;; 
;; # as in Templates/XHTML 1.0 Frameset/info.yasnippet
;; 26068A55-4C84-409D-BA00-162B55AF6961                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Decode HTML Entities.yasnippet
;; C183920D-A126-11D9-A5A2-000D93C8BE28                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/IE Conditional Comment Internet Explorer 7+.yasnippet
;; `(or (yas/selected-text) " IE Conditional Comment: Internet Explorer 7 and above ")`       =yyas> (yas/unknown)
;; 
;; # as in Snippets/IE Conditional Comment Internet Explorer 6 only.yasnippet
;; `(or (yas/selected-text) "     IE Conditional Comment: Internet Explorer 6 only   ")`      =yyas> (yas/unknown)
;; 
;; 

;; Substitutions for: condition
;; 
;; # as in Snippets/XHTML head.yasnippet
;; text.html - text.html source                                                               =yyas> (yas/unknown)
;; 
;; # as in Commands/CodeCompletion HTML Attributes.yasnippet
;; text.html punctuation.definition.tag -source, text.html meta.tag -entity.other.attribute-name -source  =yyas> (yas/unknown)
;; 
;; # as in Snippets/Smart returnindent for tag pairs.yasnippet
;; meta.scope.between-tag-pair                                                                =yyas> (yas/unknown)
;; 
;; # as in Commands/CodeCompletion HTML Tags.yasnippet
;; text.html -entity.other.attribute-name -string.quoted, invalid.illegal.incomplete.html     =yyas> (yas/unknown)
;; 
;; # as in Snippets/Wrap Selection In Tag.yasnippet
;; text.html,                                                                                 =yyas> (yas/unknown)
;; 
;; # as in Commands/Refresh All Active Browsers.yasnippet
;; text.html, source.css                                                                      =yyas> (yas/unknown)
;; 
;; # as in Templates/XHTML 1.1/info.yasnippet
;; text.html                                                                                  =yyas> (yas/unknown)
;; 
;; # as in Commands/Documentation for Tag.yasnippet
;; text.html, text.html entity.name.tag                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/XHTML title.yasnippet
;; text.html - text.blog                                                                      =yyas> (yas/unknown)
;; 
;; # as in Snippets/Wrap in =.yasnippet
;; text.html string                                                                           =yyas> (yas/unknown)
;; 
;; 

;; Substitutions for: binding
;; 
;; # as in Commands/Persistent Include.yasnippet
;;                                                                                            =yyas> (yas/unknown)
;; 
;; # as in Snippets/XHTML &nbsp NonBreakingSpace.yasnippet
;; ~                                                                                          =yyas> (yas/unknown)
;; 
;; # as in Snippets/Smart returnindent for tag pairs.yasnippet
;;                                                                                           =yyas> (yas/unknown)
;; 
;; # as in Commands/W3C validation.yasnippet
;; ^V                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Insert Close Tag.yasnippet
;; ~@.                                                                                        =yyas> (yas/unknown)
;; 
;; # as in Snippets/XHTML br.yasnippet
;; ^                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Insert Entity.yasnippet
;; @&                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Refresh All Active Browsers.yasnippet
;; @r                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/CodeCompletion HTML Tags.yasnippet
;; ~                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Update Includes.yasnippet
;; ^@u                                                                                        =yyas> (yas/unknown)
;; 
;; # as in Macros/Delete whitespace between tags.yasnippet
;; ^~                                                                                      =yyas> (yas/unknown)
;; 
;; # as in Commands/Tidy.yasnippet
;; ^H                                                                                         =yyas> (yas/unknown)
;; 
;; 

;; .yas-setup.el for html-mode ends here
