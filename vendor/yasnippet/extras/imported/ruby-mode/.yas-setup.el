;; .yas-setup.el for ruby-mode
;; -*- coding: utf-8 -*-
;;
(defvar yas/ruby-snippet-open-paren " "
  "The open parenthesis used in ruby-mode snippets. Normally blank but could be (")
(defvar yas/ruby-snippet-close-paren " "
  "The close parenthesis used in ruby-mode snippets. Normally blank but could be )")
(defvar yas/ruby-shebang-args " -wKU"
  "Arguments for the ruby shebang line.")

(defun yas/ruby-infer-class-name ()
  "Infer the class name from the buffer. Thanks to hitesh <hitesh.jasani@gmail.com>"
  (if buffer-file-name
      (let ((fn (capitalize (file-name-nondirectory
                             (file-name-sans-extension
                              (buffer-file-name))))))
        (cond
         ((string-match "_" fn) (replace-match "" nil nil fn))
         (t fn)))
    "SomeClass"))

(defun yas/ruby-chomp (x)
  "Chomp string X, return nil if X became empty"
  (let ((len (length x))
        (start 0)
        (end (1- (length x))))
    (unless (zerop len)
      (while (and (< start len)
                  (memq (aref x start)
                        '(?  ?\t ?\n)))
        (setq start (1+ start)))
      (while (and (> end start)
                  (memq (aref x end)
                        '(?  ?\t ?\n)))
        (setq end (1- end)))
      (unless (<= end start)
        (substring x start (1+ end))))))

(defvar yas/ruby-block-start-regexp "\\(^\\|[\s\t\n^]\\)\\(do\\)[\s\t\n]\\(|.*|\\)?")

(defun yas/ruby-toggle-single-multi-line-block ()
  "Toggle \"do .. end\" blocks into  \"{ .. }\" blocks back and forth."
  ;;
  ;; TODO: Some code to be refactored here.
  ;; 
  ;; FIXME: correctly detect statements in { .. } block, split-string(";") is no good
  ;;
  (interactive)
  (let* ((do-block-bounds (save-excursion
                            (when (or (save-excursion (beginning-of-line)
                                                      (looking-at yas/ruby-block-start-regexp))
                                      (save-excursion (ruby-beginning-of-block)
                                                      (looking-at yas/ruby-block-start-regexp)))
                              (cons (match-beginning 1)
                                    (progn (goto-char (match-beginning 1))
                                           (ruby-end-of-block) (point))))))
         (brace-block-bounds (condition-case nil
                                 (let ((syntax-info (syntax-ppss)))
                                   (if (fourth syntax-info)
                                       (goto-char (ninth syntax-info)))
                                   (while (progn (up-list -1) (not (eq (char-after) ?{))))
                                   (cons (point)
                                         (progn (forward-sexp) (point))))
                               (error nil)))
         (block-region)
         (statements))
    (if (and do-block-bounds brace-block-bounds)
        (if (< (car do-block-bounds) (car brace-block-bounds))
            (setq do-block-bounds nil)
          (setq brace-block-bounds nil)))
    (cond (do-block-bounds
           (goto-char (car do-block-bounds))
           (setq block-region (buffer-substring-no-properties (+ 2 (car do-block-bounds)) (cdr do-block-bounds)))
           (delete-region (car do-block-bounds) (+ 3 (cdr do-block-bounds)))
           (insert "{")
           (when (string-match "\\(|.*|\\).*" block-region)
             (insert " " (match-string 1 block-region))
             (setq block-region (substring block-region (match-end 1))))
           (setq statements (remove nil (mapcar #'yas/ruby-chomp
                                                (split-string block-region "\n"))))
           (mapc #'(lambda (string)
                     (insert " " string)
                     (if (member (aref string (1- (length string))) '(?;
                                                                      ?|))
                         (insert " ")
                       (insert ";")))
                 statements)
           (when statements (delete-backward-char 1))
           (save-excursion
             (insert " }")))
          (brace-block-bounds
           ;; (message "found a brace block")
           (goto-char (car brace-block-bounds))
           (setq block-region (buffer-substring (1+ (car brace-block-bounds)) (1- (cdr brace-block-bounds))))
           (delete-region (car brace-block-bounds) (cdr brace-block-bounds))
           (insert "do")
           (when (string-match "\\(|.*|\\).*" block-region)
             (insert " " (match-string 1 block-region))
             (setq block-region (substring block-region (match-end 1))))
           (setq statements (remove nil (mapcar #'yas/ruby-chomp
                                                (split-string block-region ";"))))
           (mapc #'(lambda (string)
                     (insert "\n" string)
                     (indent-according-to-mode))
                 statements)
           (unless statements (insert "\n") (indent-according-to-mode))
           (save-excursion
             (insert "\nend")
             (indent-according-to-mode)))
          (t
           (message "No enclosing block found.")))))

(defvar yas/ruby-require-regexps
  '(("abbrev"                            . ("abbrev"))
    ("base64"                            . ("Base64"))
    ("benchmark"                         . ("Benchmark"))
    ("bigdecimal"                        . ("BigDecimal"))
    ("bigdecimal/math"                   . ("BigMath"))
    ("cgi"                               . ("CGI"))
    ("complex"                           . ("Complex"))
    ("csv"                               . ("CSV"))
    ("curses"                            . ("Curses"))
    ("date"                              . ("Date(?:Time)?"))
    ("dbm"                               . ("DBM"))
    ("delegate"                          . ("DelegateClass" "Delegator" "SimpleDelegator "))
    ("digest"                            . ("MD5" "SHA1"))
    ("dl"                                . ("DL"))
    ("enumerator"                        . ("(?:enum|each)_(?:cons|slice)" "enum_(?:for|with_index)" "to_enum "))
    ("erb"                               . ("ERB"))
    ("etc"                               . ("Etc"))
    ("fcntl"                             . ("Fcntl"))
    ("fileutils"                         . ("FileUtils"))
    ("find"                              . ("Find(?:\.|::)find"))
    ("forwardable"                       . ("(?:Single)?Forwardable"))
    ("gdbm"                              . ("GDBM"))
    ("generator"                         . ("Generator" "SyncEnumerator"))
    ("getoptlong"                        . ("GetoptLong"))
    ("gserver"                           . ("GServer"))
    ("iconv"                             . ("Iconv"))
    ("ipaddr"                            . ("IpAddr"))
    ("logger"                            . ("Logger"))
    ("matrix"                            . ("Matrix" "Vector"))
    ("monitor"                           . ("Monitor(?:Mixin)?"))
    ("net/ftp"                           . ("Net::FTP"))
    ("net/http"                          . ("Net::HTTP"))
    ("net/imap"                          . ("Net::IMAP"))
    ("net/pop"                           . ("Net::(?:APOP|POP3)"))
    ("net/smtp"                          . ("Net::SMTP"))
    ("net/telnet"                        . ("Net::Telnet"))
    ("nkf"                               . ("NKF"))
    ("observer"                          . ("Observable"))
    ("open3"                             . ("Open3"))
    ("optparse"                          . ("OptionParser"))
    ("ostruct"                           . ("OpenStruct"))
    ("pathname"                          . ("Pathname"))
    ("ping"                              . ("Ping"))
    ("pp"                                . ("pp"))
    ("pstore"                            . ("PStore"))
    ("rational"                          . ("Rational"))
    ("rdoc/usage"                        . ("RDoc(?:\.|::)usage"))
    ("rdoc/markup/simple_markup"         . ("SM::SimpleMarkup"))
    ("rdoc/markup/simple_markup/to_html" . ("SM::SimpleMarkup"))
    ("rdoc/usage"                        . ("RDoc(?:\.|::)usage"))
    ("resolv"                            . ("Resolv"))
    ("rexml/document"                    . ("REXML"))
    ("rinda/tuplespace"                  . ("Rinda::TupleSpace(?:Proxy)?"))
    ("rinda/ring"                        . ("Rinda::Ring(?:Finger|Server)?"))
    ("rss"                               . ("RSS"))
    ("scanf"                             . ("scanf"))
    ("sdbm"                              . ("SDBM"))
    ("set"                               . ("(?:Sorted)?Set"))
    ("singleton"                         . ("Singleton"))
    ("soap"                              . ("SOAP"))
    ("socket"                            . (" (?:TCP|UNIX)(?:Socket|Server)" "(?:UDP)?Socket"))
    ("stringio"                          . ("StringIO"))
    ("strscan"                           . ("StringScanner"))
    ("syslog"                            . ("Syslog"))
    ("tempfile"                          . ("Tempfile"))
    ("test/unit"                         . ("Test::Unit"))
    ("thread"                            . (" ConditionVariable" "Mutex" "(?:Sized)?Queue "))
    ("time"                              . ("Time(?:\.|::)parse"))
    ("timeout"                           . ("Timeout(?:\.|::)timeout"))
    ("tk"                                . ("TK"))
    ("tmpdir"                            . ("Dir(?:\.|::)tmpdir"))
    ("tracer"                            . ("Tracer"))
    ("tsort"                             . ("TSort"))
    ("uri"                               . ("URI"))
    ("weakref"                           . ("WeakRef"))
    ("webrick"                           . ("WEBrick"))
    ("Win32API"                          . ("Win32(?:API)?"))
    ("win32ole"                          . ("WIN32OLE"))
    ("wsdl"                              . ("WSDL"))
    ("xmlrpc"                            . ("XMLRPC"))
    ("yaml"                              . ("YAML"))
    ("zlib"                              . ("Zlib"))))

(defun yas/ruby-require (package)
  (save-excursion
    (goto-char (point-min))
    (unless (search-forward-regexp (format "^[\s\t]*require[( ][ ]*\"%s\"[ )]*$"
                                           package) nil t)
      (unless (search-forward-regexp "^[\s\t]*require.*\n" nil t)
        (search-forward-regexp "^[\s\t]*[^#]" nil t)
        (goto-char (line-beginning-position)))
      (insert "require \"" package "\"\n"))))

(defun yas/ruby-pipe-through-xmpfilter ()
  (interactive)
  (let ((start (or (and mark-active
                        (region-beginning))
                   (point-min)))
        (end (or (and mark-active
                      (region-end))
                 (point-max)))
        (orig (point))
        retval
        (orig-line (count-screen-lines (window-start) (line-beginning-position))))
    
    (unless (zerop (shell-command-on-region start end "xmpfilter" (get-buffer-create "*xmpfilter*") t (get-buffer-create "*xmpfilter errors*") t))
      (undo)
      )
    (goto-char (min (point-max) orig))
    (recenter orig-line)
    retval))

(put (intern "ruby-thing") 'bounds-of-thing-at-point 'yas/ri-ruby-thing-bounds)
(defun yas/ri-ruby-thing-bounds ()
  (let ((start (point))
        (end (point)))
    (save-excursion
      (while (not (and (zerop (skip-syntax-forward "\w\_"))
                       (zerop (skip-chars-forward "#:"))))
        (setq end (point)))
      (while (not (and (zerop (skip-syntax-backward "\w\_"))
                       (zerop (skip-chars-backward "#:"))))
        (setq start (point))))
    (unless (= start end)
      (cons start end))))

(defvar yas/ri-history nil
  "History of yas/ri queries.")
(defvar yas/ri-executable "ri")
(require 'ansi-color)
(defun yas/ri (query)
  (interactive (list (read-from-minibuffer "Ri query: "
                                           (thing-at-point 'ruby-thing)
                                           nil
                                           nil
                                           'ri-history)))
  (with-current-buffer (get-buffer-create "*Ri*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq default-directory "~")
    (setq buffer-read-only nil)
    (shell-command (concat yas/ri-executable " -f ansi " query) "*Ri*")
    (ansi-color-apply-on-region (point-min) (point-max))
    (yas/ri-mode)
    (display-buffer (current-buffer)))
  t)

(defun yas/ri-mode ()
  "Mode for viewing Ruby documentation."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (local-set-key (kbd "q") 'quit-window)
  (local-set-key (kbd "RET") 'yas/ri)
  (setq mode-name "ri")
  (setq major-mode 'yas/ri-mode)
  (setq buffer-read-only t)
  (run-hooks 'yas/ri-mode-hook))

;; conditions
;; 
(yas/define-condition-cache yas/ruby-in-interpolated-string-p (member (fourth (syntax-ppss)) (list ?\" ?\`)))
(yas/define-condition-cache yas/ruby-in-comment-p (fifth (syntax-ppss)))
(yas/define-condition-cache yas/ruby-in-string-p (fourth (syntax-ppss)))
(yas/define-condition-cache yas/ruby-end-is-block-end-p
                            (save-excursion
                              (ruby-backward-sexp)
                              (not (eq (point) (point-min)))))

(provide 'yas/ruby)

;; My work in progress substitutions
;;
;; Substitutions for: content
;;
;; ${1/.+/(/}                                                                        =yyas> ${1:$(and (yas/text) "(")}
;; ${1/.+/)/}                                                                        =yyas> ${1:$(and (yas/text) ")")}
;; ${2/.+/ => /}                                                                     =yyas> ${2:$(and (yas/text) " => ")}
;; ${1:${TM_FILENAME/\.\w+//}                                                        =yyas> ${1:$(and buffer-file-name (file-name-sans-extension buffer-file-name))}
;; ${1/(^.*?\S.*)|.*/(?1:\()/}                                                       =yyas> ${1:$(and (string-match "[^\s\t]" yas/text) "(" )}
;; ${1/(^.*?\S.*)|.*/(?1:\))/}                                                       =yyas> ${1:$(and (string-match "[^\s\t]" yas/text) ")" )}
;; ${2/(^.*?\S.*)|.*/(?1:\()/}                                                       =yyas> ${2:$(and (string-match "[^\s\t]" yas/text) "(" )}
;; ${2/(^.*?\S.*)|.*/(?1:\))/}                                                       =yyas> ${2:$(and (string-match "[^\s\t]" yas/text) ")" )}
;; ${3/(^.*?\S.*)|.*/(?1:\()/}                                                       =yyas> ${3:$(and (string-match "[^\s\t]" yas/text) "(" )}
;; ${3/(^.*?\S.*)|.*/(?1:\))/}                                                       =yyas> ${3:$(and (string-match "[^\s\t]" yas/text) ")" )}
;; ${2/^\s*$|(.*\S.*)/(?1: )/}                                                       =yyas> ${2:$(and (string-match "[^\s\t]" yas/text) " " )}
;; ${3/^\s*$|(.*\S.*)/(?1: )/}                                                       =yyas> ${3:$(and (string-match "[^\s\t]" yas/text) " " )}
;; ${3/(^[rwab+]+$)|.*/(?1:, ")/}                                                    =yyas> ${3:$(and (string-match "^[rwab+]+$" yas/text) ", \\"" )}
;; ${3/(^[rwab+]+$)|.*/(?1:")/}                                                      =yyas> ${3:$(and (string-match "^[rwab+]+$" yas/text) "\\"" )}
;; ${3/^\s*$|(.*\S.*)/(?1:, )/}                                                      =yyas> ${3:$(and (string-match "[^\s\t]" (yas/text) ", ")}
;; ${TM_SELECTED_TEXT/([\t ]*).*/$1/m}                                               =yyas>
;; ${TM_SELECTED_TEXT/(\A.*)|(.+)|\n\z/(?1:$0:(?2:\t$0))/g}                          =yyas> `yas/selected-text`
;; (yas/multi-line-unknown BF487539-8085-4FF4-8601-1AD20FABAEDC)                     =yyas> `(yas/ruby-infer-class-name)`
;; (yas/multi-line-unknown 2B73EC5F-06D2-460C-A14F-6FA05AFCF0CC)                     =yyas> `(yas/ruby-infer-class-name)`
;; 
;; ${TM_FILENAME/(?:\A|_)([A-Za-z0-9]+)(?:\.rb)?/(?2::\u$1)/g}                       =yyas> `(yas/ruby-infer-class-name)`
;; 
;; ${1/(^(?<var>\s*[a-z_][a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:|)/}             =yyas> ${1:$(and (yas/text) "|")}
;; ${1/(^(?<var>\s*[a-z_][a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1: |)/}            =yyas> ${1:$(and (yas/text) " |")}
;; ${1/(^(?<var>\s*[a-z_][a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:| )/}            =yyas> ${1:$(and (yas/text) "| ")}
;;
;; ${1/(^(?<var>\s*(?:\*|\*?[a-z_])[a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:|)/}   =yyas> ${1:$(and (yas/text) "|")}
;; ${1/(^(?<var>\s*(?:\*|\*?[a-z_])[a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:| )/}  =yyas> ${1:$(and (yas/text) "| ")}
;; ${2/(^(?<var>\s*(?:\*|\*?[a-z_])[a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:|)/}   =yyas> ${2:$(and (yas/text) "|")}
;; ${2/(^(?<var>\s*(?:\*|\*?[a-z_])[a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:| )/}  =yyas> ${2:$(and (yas/text) "| ")}
;; 
;; ${1/([\w&&[^_]]+)|./\u$1/g}                                                       =yyas> ${1:$(replace-regexp-in-string "[_/]" "" (capitalize yas/text))}
;;
;; 7990EE60-C850-4779-A8C0-7FD2C853B99B               =yyas> (yas/ruby-toggle-single-multi-line-block)
;; 7E084412-80E6-4B70-8092-C03D1ECE4CD2               =yyas> (yas/ruby-require "eac")(yas/expand-uuid 'ruby-mode "FDD73070-6D32-4301-A86A-C55B77C3D8ED")
;; FBFC214F-B019-4967-95D2-028F374A3221               =yyas> (yas/ruby-pipe-through-xmpfilter)
;; 63F3B3B7-CBE2-426B-B551-657733F3868B               =yyas> (call-interactively (if (featurep 'yari) 'yari 'yas/ri))

;;
;; `[[ $TM_LINE_INDEX != 0 ]] && echo; echo`                                         =yyas> `(concat (if (eq 0 current-line) "\n" "") "\n")`
;; `snippet_paren.rb`                                                                =yyas> `yas/ruby-snippet-open-paren`
;; `snippet_paren.rb end`                                                            =yyas> `yas/ruby-snippet-close-paren`
;; ${TM_RUBY_SWITCHES: -wKU}                                                         =yyas> `yas/ruby-shebang-args`
;; 
;; Substitutions for: condition
;;
;; 7990EE60-C850-4779-A8C0-7FD2C853B99B                                              =yyas> 'force-in-comment
;; FBFC214F-B019-4967-95D2-028F374A3221                                              =yyas> 'force-in-comment
;; 88BC3896-DC39-4307-A271-21D33340F15A                                              =yyas> 'force-in-comment
;; 0F940CBC-2173-49FF-B6FD-98A62863F8F2                                              =yyas> 'force-in-comment
;; 451A0596-1F72-4AFB-AF2F-45900FABB0F7                                              =yyas> (not (yas/ruby-end-is-block-end-p))
;; (string.quoted.double.ruby|string.interpolated.ruby) - string source              =yyas> (and (yas/ruby-in-interpolated-string-p) 'force-in-comment)
;; text.html.ruby, text.html source.ruby                                             =yyas> (yas/unimplemented)
;; text.html, source.yaml, meta.erb                                                  =yyas> (yas/unimplemented)
;; keyword.control.start-block.ruby, meta.syntax.ruby.start-block                    =yyas>
;; 
;; Substitutions for: binding
;;
;; # as in Commands/New Method.yasnippet
;; $                                                                               =yyas> C-c M-m
;; ^W                                                                                =yyas> C-c M-w
;; #                                                                                 =yyas> #
;; ^{                                                                                =yyas> C-c M-{
;; @R                                                                                =yyas> C-c M-R
;; @r                                                                                =yyas> C-c M-r
;; ^R                                                                                =yyas> C-c M-S-r
;; @i                                                                                =yyas> s-i
;; @b                                                                                =yyas> s-b
;; ^@E                                                                               =yyas> C-c M-e
;; ^:                                                                                =yyas> C-c M-:
;; ^>                                                                                =yyas> C-c M->
;; ^h                                                                                =yyas> C-c M-h
;;
;;
;; # as in Commands/Enclose in + (RDoc comments).yasnippet
;; @k                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Check Ruby Syntax.yasnippet
;; ^V                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Omit from RDoc.yasnippet
;; ^@O                                                                                        =yyas> (yas/unknown)
;; 
;; # as in Commands/Enclose in (RDoc comments).yasnippet
;; @b                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Snippets/hash pointer.yasnippet
;; ^l                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Make Destructive Call.yasnippet
;; ^!                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Toggle Quote Style.yasnippet
;; ^"                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Open Require.yasnippet
;; @D                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Execute Line with Ruby.yasnippet
;; ^E                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Completion Ruby (rcodetools).yasnippet
;; ~                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Macros/Delete forwardbackward.yasnippet
;;                                                                                           =yyas> (yas/unknown)
;; 
;; --**--
;; Automatically generated code, do not edit this part
;; 
;; Translated menu
;; 
(yas/define-menu 'ruby-mode
                 '(;; Ignoring Run
                   (yas/ignore-item "35222962-C50D-4D58-A6AE-71E7AD980BE4")
                   ;; Ignoring Run Focused Unit Test
                   (yas/ignore-item "5289EE40-86B8-11D9-A8D4-000A95E13C98")
                   ;; Ignoring Run Rake Task
                   (yas/ignore-item "569C9822-8C41-4907-94C7-1A8A0031B66D")
                   
                   ;; Documentation for Word / Selection
                   (yas/item "63F3B3B7-CBE2-426B-B551-657733F3868B")
                   (yas/submenu "RDoc"
                                (;; Ignoring Show for Current File / Project
                                 (yas/ignore-item "1AD6A138-2E89-4D6A-AB3F-416BF9CE968D")
                                 
                                 (yas/submenu "Format"
                                              (;; Ignoring Bold
                                               (yas/ignore-item "931DD73E-615E-476E-9B0D-8341023AE730")
                                               ;; Ignoring Italic
                                               (yas/ignore-item "DAA69A0C-FC1E-4509-9931-DFFB38B4D6AE")
                                               ;; Ignoring Typewriter
                                               (yas/ignore-item "2DDB6FE0-6111-4C40-A149-8E67E76F8272")))
                                 
                                 ;; New Block
                                 (yas/item "05984208-D559-4C04-A69C-2019361A985A")
                                 ;; Ignoring Omit
                                 (yas/ignore-item "BF4CA9F1-51CD-48D4-8357-852234F59046")
                                 
                                 ;; :yields:
                                 (yas/item "ED6368FB-A11D-4622-9F42-7879481094F1")))
                   (yas/separator)
                   (yas/submenu "Rake"
                                (;; namespace :name ... end
                                 (yas/item "A3D89AAA-9156-4077-A026-37BB7358C3BA")
                                 ;; namespace :name ... task :default ... end
                                 (yas/item "2031FC41-CBD3-41CC-B9A9-7F068E607A05")
                                 ;; desc ...
                                 (yas/item "F686E1AD-B03D-45A6-BD51-6E3FD1298FE0")
                                 ;; task :name ... end
                                 (yas/item "CB81DA55-F3BC-4BFB-B0C5-29F0EE6F8081")
                                 ;; desc ... task :name ... end
                                 (yas/item "FE9A8EDA-C243-4068-8F38-A615B82D08C9")
                                 ;; Ignoring Rake/Sake task using file path
                                 (yas/ignore-item "E07FF68B-C87D-4332-8477-D026929FDADA")))
                   (yas/separator)
                   ;; Ignoring Open Require
                   (yas/ignore-item "8646378E-91F5-4771-AC7C-43FC49A93576")
                   ;; Ignoring Validate Syntax
                   (yas/ignore-item "EE5F19BA-6C02-11D9-92BA-0011242E4184")
                   
                   ;; Ignoring Execute Line / Selection as Ruby
                   (yas/ignore-item "EE5F1FB2-6C02-11D9-92BA-0011242E4184")
                   ;; Execute and Update ‘# =>’ Markers
                   (yas/item "FBFC214F-B019-4967-95D2-028F374A3221")
                   ;; Add ‘# =>’ Marker
                   (yas/item "88BC3896-DC39-4307-A271-21D33340F15A")
                   (yas/separator)
                   ;; Ignoring Insert Missing Requires
                   (yas/ignore-item "9FB64639-F776-499B-BA6F-BB45F86F80FD")
                   ;; Ignoring Add ! to Method in Line / Selection
                   (yas/ignore-item "7F79BC8D-8A4F-4570-973B-05DFEC25747F")
                   ;; Ignoring Toggle String / Symbol
                   (yas/ignore-item "B297E4B8-A8FF-49CE-B9C4-6D4911724D43")
                   ;; Insert ERb’s <% .. %> or <%= .. %>
                   (yas/item "FDFABCB9-DF58-4469-AE11-5407A4FF4D70")
                   (yas/separator)
                   (yas/submenu "Declarations"
                                (;; begin … rescue … end
                                 (yas/item "0F940CBC-2173-49FF-B6FD-98A62863F8F2")
                                 ;; case … end
                                 (yas/item "667083EE-62C3-11D9-B8CF-000D93589AF6")
                                 ;; when …
                                 (yas/item "48D8E498-C9A5-4B1B-9A18-71A5860276FB")
                                 ;; if … end
                                 (yas/item "6670835F-62C3-11D9-B8CF-000D93589AF6")
                                 ;; if … else … end
                                 (yas/item "667082E6-62C3-11D9-B8CF-000D93589AF6")
                                 ;; elsif ...
                                 (yas/item "CD1609FA-47DA-4EE4-9C5B-5C56D953F5B1")
                                 ;; unless … end
                                 (yas/item "F53E098D-D08E-4CE2-990A-B0BD70E60614")
                                 ;; while ... end
                                 (yas/item "D121FC61-96A4-4B8F-8709-280EDA876FF3")
                                 ;; until ... end
                                 (yas/item "488B387C-50C0-4B2D-9260-5A7E7EAF9B42")
                                 (yas/separator)
                                 (yas/submenu "Classes and Modules"
                                              (;; class .. end
                                               (yas/item "BF487539-8085-4FF4-8601-1AD20FABAEDC")
                                               ;; class .. initialize .. end
                                               (yas/item "83EED068-8C1C-4BAF-9893-902DC00616AB")
                                               ;; class .. < ParentClass .. initialize .. end
                                               (yas/item "0CCBE04E-F4E2-4E55-9506-7DE67ACF8388")
                                               ;; ClassName = Struct .. do .. end
                                               (yas/item "05DFF82C-5A29-4EBD-93FE-C165FFFB5EA8")
                                               ;; class BlankSlate .. initialize .. end
                                               (yas/item "E98FB8F9-7302-431D-8BF2-275A68A6126C")
                                               ;; Ignoring class .. < DelegateClass .. initialize .. end
                                               (yas/ignore-item "121B334B-2AA6-4E9A-A8B8-BF93B627982B")
                                               ;; class .. < DelegateClass .. initialize .. end
                                               (yas/item "AFE1D078-EA16-45F5-AD8A-FAC1B523D861")
                                               ;; class << self .. end
                                               (yas/item "C7AAAE45-487A-4B61-8962-D47675AAC05F")
                                               (yas/separator)
                                               ;; module .. end
                                               (yas/item "2B73EC5F-06D2-460C-A14F-6FA05AFCF0CC")
                                               ;; module .. module_function .. end
                                               (yas/item "0E85EC81-2FAB-4648-B590-119CC1BB6E41")
                                               ;; module .. ClassMethods .. end
                                               (yas/item "A71A18CF-2D71-4BFF-AA0C-D9B8C59BC4EB")))
                                 (yas/submenu "Methods"
                                              (;; Ignoring New Method
                                               (yas/ignore-item "0275EF39-9357-408F-AF20-79E415CA9504")
                                               
                                               ;; attr_reader ..
                                               (yas/item "A150C2D8-25B3-4339-BC92-8A0160A70486")
                                               ;; attr_writer ..
                                               (yas/item "3D383096-A03F-4EF8-9060-3C727045AB34")
                                               ;; attr_accessor ..
                                               (yas/item "D7A7D3C9-1714-4C50-8CC0-D83A03883E8F")
                                               (yas/separator)
                                               ;; include Enumerable ..
                                               (yas/item "AAD5D511-6BE7-41DA-8F2B-1593A48FBB08")
                                               ;; include Comparable ..
                                               (yas/item "6C9D6B3D-D8E9-4606-9534-577C8D21FFF6")
                                               (yas/separator)
                                               ;; Ignoring extend Forwardable
                                               (yas/ignore-item "58FDEA60-10AF-4C49-AA09-29B77030DB25")
                                               ;; extend Forwardable
                                               (yas/item "7F46C90A-595B-4B83-A4F7-058F63CE4218")
                                               (yas/separator)
                                               ;; def … end
                                               (yas/item "4E9A7A73-875C-11D9-897C-000393CBCE2E")
                                               ;; def self .. end
                                               (yas/item "7C6E88FA-CA0E-4110-8C75-A94E54286A75")
                                               ;; def method_missing .. end
                                               (yas/item "87D5F8AD-8DA6-4AED-A0D8-B51CAC980445")
                                               ;; def_delegator ..
                                               (yas/item "C44ED391-614F-4BA2-BB0F-87668EEA9954")
                                               ;; def_delegators ..
                                               (yas/item "4A6EFD6B-88E2-4822-AD48-03460EDBC796")
                                               (yas/separator)
                                               ;; alias_method ..
                                               (yas/item "988C8AEF-FC71-4455-9C4F-9338C05685A4")))
                                 ;; __END__
                                 (yas/item "451A0596-1F72-4AFB-AF2F-45900FABB0F7")
                                 (yas/separator)
                                 ;; #!/usr/bin/env ruby -wKU
                                 (yas/item "A05CBDD6-845D-45EB-94FB-F8787F5456BE")
                                 ;; require ".."
                                 (yas/item "97DE939B-D243-4D5C-B953-1C9090912E7C")
                                 ;; application { .. }
                                 (yas/item "E16D24D2-CC7E-4786-BE0B-1725FC865D78")
                                 ;; usage_if()
                                 (yas/item "21C0D711-F32A-4665-AA0D-B136F9DD3945")
                                 ;; usage_unless()
                                 (yas/item "49D69DEC-6991-49F4-8D9B-BA60BFDD3D17")))
                   (yas/submenu "Iterators"
                                ((yas/submenu "Arrays"
                                              (;; Array.new(10) { |i| .. }
                                               (yas/item "DAE6A754-D906-4763-B816-CE67125CEF08")
                                               (yas/separator)
                                               ;; delete_if { |e| .. }
                                               (yas/item "263C94DC-63CF-4BA3-9692-C5582CA8F1AB")
                                               ;; fill(range) { |i| .. }
                                               (yas/item "6021BBDC-4AAD-447B-A0C2-A4BB31721558")
                                               ;; flatten_once()
                                               (yas/item "3DDB99C4-486D-4C11-A217-5680FDD8EC19")
                                               ;; zip(enums) { |row| .. }
                                               (yas/item "FD010022-E0E7-44DB-827F-33F7D9310DA2")))
                                 (yas/submenu "Counting"
                                              (;; downto(0) { |n| .. }
                                               (yas/item "4991BB86-736E-4758-B9B2-E4FA90B9368F")
                                               ;; step(2) { |e| .. }
                                               (yas/item "36853A11-0307-4AE7-B835-7CE6358717A5")
                                               ;; times { |n| .. }
                                               (yas/item "206D54AF-E67A-4DF0-B7F4-3D42FEB81685")
                                               ;; upto(1.0/0.0) { |n| .. }
                                               (yas/item "51954118-81D7-42B6-9A10-BE23D8B9FFE2")
                                               (yas/separator)
                                               ;; loop { .. }
                                               (yas/item "567E3D18-BF2B-4379-8927-2777EC9F495E")))
                                 (yas/submenu "Each Element"
                                              (;; each { |e| .. }
                                               (yas/item "ECBA4CA0-275F-460E-85BE-E82FEA2E2B26")
                                               ;; each_byte { |byte| .. }
                                               (yas/item "338EC03D-3FF4-4435-94E8-1CEF20CEC75D")
                                               ;; each_char { |chr| .. }
                                               (yas/item "7E084412-80E6-4B70-8092-C03D1ECE4CD2")
                                               ;; each_char { |chr| .. }
                                               (yas/item "FDD73070-6D32-4301-A86A-C55B77C3D8ED")
                                               ;; Ignoring each_cons(..) { |group| .. }
                                               (yas/ignore-item "EC73D5CC-5F05-46B9-A6F4-82037E4A38C9")
                                               ;; each_cons(..) { |group| .. }
                                               (yas/item "3C04589C-5127-478E-97B3-CA7DD2EA7ECD")
                                               ;; each_index { |i| .. }
                                               (yas/item "689120C9-AB40-4081-8268-9362E00FA4A0")
                                               ;; each_key { |key| .. }
                                               (yas/item "E54F7077-3C33-4B53-A4F7-21E16132D3AD")
                                               ;; each_line { |line| .. }
                                               (yas/item "02913388-EE8E-4C55-AC94-94F3D751F47E")
                                               ;; each_pair { |name, val| .. }
                                               (yas/item "7A3CECED-452B-438E-A5C6-95B6BDC43243")
                                               ;; Ignoring each_slice(..) { |group| .. }
                                               (yas/ignore-item "825B721D-4367-4DF7-98C0-F005695DF9E3")
                                               ;; each_slice(..) { |group| .. }
                                               (yas/item "CD748479-D2A4-4AB5-95BD-4C89512BA210")
                                               ;; each_value { |val| .. }
                                               (yas/item "844DBD70-BC23-4FBF-9C18-F4A610239DF2")
                                               ;; each_with_index { |e, i| .. }
                                               (yas/item "1DD13CF5-39C0-4F10-B655-56DACEBC7F94")
                                               ;; reverse_each { |e| .. }
                                               (yas/item "F3C5F719-EF03-4FF7-A777-4A8402FE3B6B")
                                               (yas/separator)
                                               ;; inject(init) { |mem, var| .. }
                                               (yas/item "B563E0D7-513D-49B4-9733-1B04A6F25A74")
                                               (yas/separator)
                                               ;; map { |e| .. }
                                               (yas/item "5A3754FC-43A3-462B-AB42-E3E951872E6F")
                                               ;; Ignoring map_with_index { |e, i| .. }
                                               (yas/ignore-item "BFB65D1C-62F1-485D-8A67-3E5A2E55107C")
                                               ;; map_with_index { |e, i| .. }
                                               (yas/item "BD4CFD7B-1AC0-4569-9BDA-FD491F41F4E6")))
                                 (yas/submenu "Files"
                                              (;; Dir.glob("..") { |file| .. }
                                               (yas/item "332AA973-AA71-48CB-AEE9-1D71E11019AC")
                                               ;; File.foreach ("..") { |line| .. }
                                               (yas/item "8F594E5E-6F46-4E98-B5FB-1C8F3BA9828F")
                                               ;; open("path/or/url", "w") { |io| .. }
                                               (yas/item "418F1817-255F-430A-B09A-222964ED66A7")
                                               ;; unix_filter { .. }
                                               (yas/item "8CEF9711-88D5-4202-AFB9-29EF4EFD25C1")
                                               (yas/separator)
                                               ;; option_parse { .. }
                                               (yas/item "C3C48948-4F49-484E-A8DE-DEB44723099E")
                                               ;; option(..)
                                               (yas/item "209D5D73-7A77-4931-A158-3FB6D5B48A88")))
                                 (yas/submenu "Ordering"
                                              (;; sort { |a, b| .. }
                                               (yas/item "9E0B4D4B-2956-4B3A-800A-3D8CE54E66BF")
                                               ;; sort_by { |e| .. }
                                               (yas/item "BA9440C9-36C3-4031-BB61-67B581D5B179")
                                               (yas/separator)
                                               ;; randomize()
                                               (yas/item "B0CE57EC-FB2E-4482-8CCE-448DC2588715")))
                                 (yas/submenu "Searching and Selection"
                                              (;; all? { |e| .. }
                                               (yas/item "07D1F987-7CDB-4EAD-B64A-27A93051700E")
                                               ;; any? { |e| .. }
                                               (yas/item "A3B9B76B-2BC5-425C-AB24-9FAAFC375798")
                                               ;; classify { |e| .. }
                                               (yas/item "5DA9E1E8-2C54-420A-9B84-B040A1AF2B9E")
                                               ;; collect { |e| .. }
                                               (yas/item "669A86AD-936F-4EDA-8E4E-6863804072DA")
                                               ;; detect { |e| .. }
                                               (yas/item "6C6B9849-9631-49FF-A9F9-F0E94A1512C5")
                                               ;; fetch(name) { |key| .. }
                                               (yas/item "1F72122A-35AD-4BA1-AA01-889A10319666")
                                               ;; find { |e| .. }
                                               (yas/item "E23FE534-8061-4828-98A5-46270B6910B0")
                                               ;; find_all { |e| .. }
                                               (yas/item "197709C5-8382-4A59-B6D7-31A0CC0F23B7")
                                               ;; grep(/pattern/) { |match| .. }
                                               (yas/item "9D9E7BA3-8C5D-4532-83EA-326358C2F5BB")
                                               ;; max { |a, b| .. }
                                               (yas/item "98182B9E-7C61-4824-BE4C-9CD69C816037")
                                               ;; min { |a, b| .. }
                                               (yas/item "CB03D11A-7204-48D0-92C1-E109034403E7")
                                               ;; partition { |e| .. }
                                               (yas/item "52B8BF63-F09E-4789-8407-06168A8AE666")
                                               ;; reject { |e| .. }
                                               (yas/item "B79B9DAB-ABEF-44F6-BF7E-635E7BA11DFD")
                                               ;; select { |e| .. }
                                               (yas/item "4E409AA4-E7D4-46B7-A4E9-E32F992B33E9")))
                                 (yas/submenu "Strings"
                                              (;; sub(/../) { |match| .. }
                                               (yas/item "8021944C-CEA4-4983-8D1C-78D18D4004A1")
                                               ;; gsub(/../) { |match| .. }
                                               (yas/item "2514FC26-468C-4D08-A788-494A444C4286")
                                               (yas/separator)
                                               ;; scan(/../) { |match| .. }
                                               (yas/item "66802933-B49F-479B-9DF9-1D898FF1FA90")))))
                   (yas/submenu "Blocks"
                                (;; Toggle ‘do … end’ / ‘{ … }’
                                 (yas/item "7990EE60-C850-4779-A8C0-7FD2C853B99B")
                                 (yas/separator)
                                 ;; Insert { |variable| … }
                                 (yas/item "855FC4EF-7B1E-48EE-AD4E-5ECB8ED79D1C")
                                 ;; Insert do |variable| … end
                                 (yas/item "4B72C5C3-6CA7-41AC-B2F9-51DEA25D469E")
                                 (yas/separator)
                                 ;; lambda { |args| .. }
                                 (yas/item "21E75321-0CF7-45E8-A297-BCC7C0DDDD15")))
                   (yas/submenu "Hashes"
                                (;; Hash.new { |hash, key| hash[key] = .. }
                                 (yas/item "E16EE658-1CA0-4950-954B-B962E50B754F")
                                 (yas/separator)
                                 ;; Hash Pair — :key => "value"
                                 (yas/item "840B9C4C-7037-4C3B-9028-EB9DC75EDB3E")
                                 ;; Hash Pointer — =>
                                 (yas/item "B9E3A6DF-875D-11D9-897C-000393CBCE2E")))
                   (yas/submenu "Tests"
                                (;; class .. < Test::Unit::TestCase .. end
                                 (yas/item "31D1F145-33AB-4441-BA11-4D1C46928C4C")
                                 ;; def test_ .. end
                                 (yas/item "00F66D41-25AF-4597-B67D-E540965A5222")
                                 ;; require "tc_.." ..
                                 (yas/item "5297FD0C-98B1-4514-BBD1-1516810BECA6")
                                 (yas/separator)
                                 ;; assert(..)
                                 (yas/item "B32C147D-44A6-478A-9D5D-189D7831E9A7")
                                 ;; assert_equal(..)
                                 (yas/item "43A61A22-6BEE-4997-961C-1CDE739C05FE")
                                 ;; assert_not_equal(..)
                                 (yas/item "A243E96F-DC21-4AA0-B340-13A7674F6AFF")
                                 ;; assert_in_delta(..)
                                 (yas/item "429D0EF5-580D-4166-8F79-713DE96B77F1")
                                 ;; assert_instance_of(..)
                                 (yas/item "0E831E03-67E1-4357-8323-C60685C23C4F")
                                 ;; assert_kind_of(..)
                                 (yas/item "671F05E2-D9CC-485E-BB1B-B13EF20FAC65")
                                 ;; assert_nil(..)
                                 (yas/item "4C79256C-480A-459C-BDE8-BB0D972811DB")
                                 ;; assert_not_nil(..)
                                 (yas/item "79FEC3CC-2A40-4611-9A85-ECDB22FE0701")
                                 ;; assert_match(..)
                                 (yas/item "711ED6C3-0F18-41FB-9A7D-3094BB319A85")
                                 ;; assert_no_match(..)
                                 (yas/item "A072BB1E-1DD1-45D3-9346-8CA3BA21B364")
                                 ;; assert_operator(..)
                                 (yas/item "1B925A4D-8EE4-442B-9254-293599F5717F")
                                 ;; assert_raise(..) { .. }
                                 (yas/item "68B21F6F-5D89-41FA-A19C-F29C2F912B4E")
                                 ;; assert_nothing_raised(..) { .. }
                                 (yas/item "82F8EEE0-2452-411E-8102-7BFDDBCA2E72")
                                 ;; assert_respond_to(..)
                                 (yas/item "09A11FDA-49FC-4466-8787-8D1D5D111A89")
                                 ;; assert_same(..)
                                 (yas/item "29340695-E426-4F77-8CF7-C59360A549F4")
                                 ;; assert_not_same(..)
                                 (yas/item "F91C25EC-EC76-498B-BFB5-FDA8F57C5875")
                                 ;; assert_send(..)
                                 (yas/item "7850AD5C-A90D-4E2C-A931-EADFF8D3D9A3")
                                 ;; assert_throws(..) { .. }
                                 (yas/item "05655BD8-23C6-445F-BFD1-420BF25C3030")
                                 ;; assert_nothing_thrown { .. }
                                 (yas/item "33639D7A-BD8C-4396-9C44-307B8AC87C9E")
                                 ;; flunk(..)
                                 (yas/item "DB457094-1AC9-4856-AEFC-43A9576B6775")
                                 (yas/separator)
                                 ;; Ignoring Benchmark.bmbm do .. end
                                 (yas/ignore-item "C649F945-DAB8-4DA2-B73C-2EFF9D7D34F3")
                                 ;; Benchmark.bmbm do .. end
                                 (yas/item "942F20E2-C40A-44B8-A3F2-99AAC68CB534")
                                 ;; results.report(..) { .. }
                                 (yas/item "1C60D589-DD46-4109-90CA-6B34AEA2F298")))
                   (yas/submenu "Serialization"
                                (;; Marshal.dump(.., file)
                                 (yas/item "0CB48BCA-3F6E-4AE0-85BC-08A1D2508216")
                                 ;; Marshal.load(obj)
                                 (yas/item "20AAD0BC-075D-4EC0-9057-E3E5E62C4125")
                                 (yas/separator)
                                 ;; Ignoring PStore.new( .. )
                                 (yas/ignore-item "5AE7CFB4-418E-4E00-AD76-06DB755EE876")
                                 ;; PStore.new( .. )
                                 (yas/item "5B46ECFD-23A4-4F0C-9951-F64C19C72C2B")
                                 ;; transaction( .. ) { .. }
                                 (yas/item "46BF99AD-E172-4D49-BCF7-072F4730E1D9")
                                 (yas/separator)
                                 ;; Ignoring YAML.dump(.., file)
                                 (yas/ignore-item "9460392B-C036-4A76-A5AE-1191F10E4B1B")
                                 ;; YAML.dump(.., file)
                                 (yas/item "3BA6762A-BB6B-489E-8006-F30F386AEF48")
                                 ;; Ignoring YAML.load(file)
                                 (yas/ignore-item "2C07D4E7-D74F-4AE4-82BE-B0BA82247AFA")
                                 ;; YAML.load(file)
                                 (yas/item "8343ACF4-EEB7-44B5-B835-94826466D4D5")
                                 (yas/separator)
                                 ;; Ignoring xmlread(..)
                                 (yas/ignore-item "F6BF907E-FDF7-4D9B-9E57-BE159561349D")
                                 ;; xmlread(..)
                                 (yas/item "B904D4AA-D15D-48A4-8EB2-563BAF489332")
                                 ;; xpath(..) { .. }
                                 (yas/item "CC300D44-6C3F-4F6C-A8AB-86F5A2DC57CF")))
                   (yas/submenu "Idioms"
                                (;; class_from_name()
                                 (yas/item "2DBEE50B-3097-4A57-AB48-3586CF392D8B")
                                 ;; deep_copy(..)
                                 (yas/item "0BA2B2F1-E767-4A03-9791-0AC0183251F1")
                                 ;; path_from_here( .. )
                                 (yas/item "A4E89D97-D5ED-48BB-B5FF-1BFB79211FCD")
                                 ;; singleton_class()
                                 (yas/item "B46D35B8-5DEB-4C10-A110-BA1965A2EB9C")
                                 ;; Ignoring word_wrap()
                                 (yas/ignore-item "97054C4D-E4A3-45B1-9C00-B82DBCB30CAD")))
                   (yas/submenu "File"
                                (;; require File.dirname(__FILE__) + "/.."
                                 (yas/item "7C42D878-FD0F-4181-A71A-57A091C0154A")
                                 (yas/separator)
                                 ;; File.dirname(__FILE__)
                                 (yas/item "16920DC1-6FA6-48C8-90C5-C19E2C734303")
                                 (yas/separator)
                                 ;; File.read(filename)
                                 (yas/item "FAFE9F5C-BF9C-4416-8623-2CB8EBC31B3C")
                                 ;; File.open(filename, 'r') { |f| f.read }
                                 (yas/item "005EB926-4BFE-4BFA-93B2-C9030636289C")))
                   ;; class .. < Test::Unit::TestCase with test_helper
                   (yas/item "228CAB3A-E221-4727-B430-31E94F76C9D3"))
                    '("E5158F94-CC52-4424-A495-14EF9272653F"
                       "EEE6D060-C5A0-400D-A2E0-0835013C5365"
                       "76FCF165-54CB-4213-BC55-BD60B9C6A3EC"
                       "6519CB08-8326-4B77-A251-54722FFBFC1F"
                       "835FAAC6-5431-436C-998B-241F7226B99B"
                       "A83F68A9-F751-4BB4-AE16-56812878C16A"
                       "47D203ED-EB9B-4653-A07B-A897800CEB76"
                       "47D203ED-EB9B-4653-A07B-A897800CEB76"
                       "2DDB6FE0-6111-4C40-A149-8E67E76F8272"
                       "DAA69A0C-FC1E-4509-9931-DFFB38B4D6AE"
                       "931DD73E-615E-476E-9B0D-8341023AE730"
                       "B3875596-723C-41EE-9E6F-F84930C3B568"
                       "BF4CA9F1-51CD-48D4-8357-852234F59046"
                       "8646378E-91F5-4771-AC7C-43FC49A93576"
                       "E07FF68B-C87D-4332-8477-D026929FDADA"
                       "569C9822-8C41-4907-94C7-1A8A0031B66D"
                       "35222962-C50D-4D58-A6AE-71E7AD980BE4"
                       "835FAAC6-5431-436C-998B-241F7226B99B"
                       "B297E4B8-A8FF-49CE-B9C4-6D4911724D43"
                       "E0E058FC-0DC3-4872-A1C2-0B1A322A0CF5"
                       "76FCF165-54CB-4213-BC55-BD60B9C6A3EC"
                       "EE5F19BA-6C02-11D9-92BA-0011242E4184"
                       "EE5F1FB2-6C02-11D9-92BA-0011242E4184"
                       "9FB64639-F776-499B-BA6F-BB45F86F80FD"
                       "7F79BC8D-8A4F-4570-973B-05DFEC25747F"
                       "0275EF39-9357-408F-AF20-79E415CA9504"
                       "5289EE40-86B8-11D9-A8D4-000A95E13C98"
                       "1AD6A138-2E89-4D6A-AB3F-416BF9CE968D"
                       "6519CB08-8326-4B77-A251-54722FFBFC1F"
                       "97054C4D-E4A3-45B1-9C00-B82DBCB30CAD"
                       "121B334B-2AA6-4E9A-A8B8-BF93B627982B"
                       "EC73D5CC-5F05-46B9-A6F4-82037E4A38C9"
                       "825B721D-4367-4DF7-98C0-F005695DF9E3"
                       "58FDEA60-10AF-4C49-AA09-29B77030DB25"
                       "BFB65D1C-62F1-485D-8A67-3E5A2E55107C"
                       "E5158F94-CC52-4424-A495-14EF9272653F"
                       "9460392B-C036-4A76-A5AE-1191F10E4B1B"
                       "2C07D4E7-D74F-4AE4-82BE-B0BA82247AFA"
                       "C649F945-DAB8-4DA2-B73C-2EFF9D7D34F3"
                       "A83F68A9-F751-4BB4-AE16-56812878C16A"
                       "5AE7CFB4-418E-4E00-AD76-06DB755EE876"
                       "F6BF907E-FDF7-4D9B-9E57-BE159561349D"))

;; Unknown substitutions
;; 
;; Substitutions for: content
;; 
;; # as in Snippets/flunk(..) (fl).yasnippet
;; `yas/ruby-snippet-open-paren`                                                              =yyas> (yas/unknown)
;; 
;; # as in Snippets/flunk(..) (fl).yasnippet
;; `yas/ruby-snippet-close-paren`                                                             =yyas> (yas/unknown)
;; 
;; # as in Snippets/class __ TestUnitTestCase with test_helper.tmSnippet.yasnippet
;; (yas/multi-line-unknown 228CAB3A-E221-4727-B430-31E94F76C9D3)                              =yyas> (yas/unknown)
;; 
;; # as in Snippets/Wrap in Begin Rescue End.yasnippet
;; `yas/selected-text`                                                                        =yyas> (yas/unknown)
;; 
;; # as in Snippets/RDoc documentation block.yasnippet
;; `(concat (if (eq 0 current-line) "\n" "") "\n")`                                           =yyas> (yas/unknown)
;; 
;; # as in Commands/Completion Ruby (rcodetools).tmCommand.yasnippet
;; 47D203ED-EB9B-4653-A07B-A897800CEB76                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Enclose in + (RDoc comments).yasnippet
;; 2DDB6FE0-6111-4C40-A149-8E67E76F8272                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Enclose in _ (RDoc comments).yasnippet
;; DAA69A0C-FC1E-4509-9931-DFFB38B4D6AE                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Enclose in (RDoc comments).yasnippet
;; 931DD73E-615E-476E-9B0D-8341023AE730                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/gsub - remove whitespace from front of line.yasnippet
;; B3875596-723C-41EE-9E6F-F84930C3B568                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Omit from RDoc.yasnippet
;; BF4CA9F1-51CD-48D4-8357-852234F59046                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Open Require.yasnippet
;; 8646378E-91F5-4771-AC7C-43FC49A93576                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/RakeSake task using file path.tmCommand.yasnippet
;; E07FF68B-C87D-4332-8477-D026929FDADA                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Run Rake Task.yasnippet
;; 569C9822-8C41-4907-94C7-1A8A0031B66D                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Run.yasnippet
;; 35222962-C50D-4D58-A6AE-71E7AD980BE4                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Toggle ERb Tags.yasnippet
;; 835FAAC6-5431-436C-998B-241F7226B99B                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Toggle StringSymbol.tmCommand.yasnippet
;; B297E4B8-A8FF-49CE-B9C4-6D4911724D43                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Validate and Save.yasnippet
;; E0E058FC-0DC3-4872-A1C2-0B1A322A0CF5                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Check ERB Syntax.yasnippet
;; 76FCF165-54CB-4213-BC55-BD60B9C6A3EC                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Check Ruby Syntax.yasnippet
;; EE5F19BA-6C02-11D9-92BA-0011242E4184                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Execute Line with Ruby.yasnippet
;; EE5F1FB2-6C02-11D9-92BA-0011242E4184                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Insert Missing Requires.yasnippet
;; 9FB64639-F776-499B-BA6F-BB45F86F80FD                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Make Destructive Call.yasnippet
;; 7F79BC8D-8A4F-4570-973B-05DFEC25747F                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/New Method.yasnippet
;; 0275EF39-9357-408F-AF20-79E415CA9504                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Run focused unit test.yasnippet
;; 5289EE40-86B8-11D9-A8D4-000A95E13C98                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Show RDoc for this file.yasnippet
;; 1AD6A138-2E89-4D6A-AB3F-416BF9CE968D                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Toggle Quote Style.yasnippet
;; 6519CB08-8326-4B77-A251-54722FFBFC1F                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/word_wrap() (worw).yasnippet
;; 97054C4D-E4A3-45B1-9C00-B82DBCB30CAD                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/class .. DelegateClass .. initialize .. end (class).yasnippet
;; 121B334B-2AA6-4E9A-A8B8-BF93B627982B                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/each_cons(..) { group .. } (eac).yasnippet
;; EC73D5CC-5F05-46B9-A6F4-82037E4A38C9                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/each_slice(..) { group .. } (eas).yasnippet
;; 825B721D-4367-4DF7-98C0-F005695DF9E3                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/extend Forwardable (Forw).yasnippet
;; 58FDEA60-10AF-4C49-AA09-29B77030DB25                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/map_with_index { e, i .. } (mapwi).yasnippet
;; BFB65D1C-62F1-485D-8A67-3E5A2E55107C                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Overwrite } in #{ .. }.yasnippet
;; E5158F94-CC52-4424-A495-14EF9272653F                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/YAML.dump(.., file) (Yd).yasnippet
;; 9460392B-C036-4A76-A5AE-1191F10E4B1B                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/YAML.load(file) (Yl).yasnippet
;; 2C07D4E7-D74F-4AE4-82BE-B0BA82247AFA                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/module .. end.yasnippet
;; `(yas/ruby-infer-class-name)`                                                              =yyas> (yas/unknown)
;; 
;; # as in Snippets/class .. TestUnitTestCase .. end (tc).yasnippet
;; (yas/multi-line-unknown 31D1F145-33AB-4441-BA11-4D1C46928C4C)                              =yyas> (yas/unknown)
;; 
;; # as in Macros/Benchmark_bmbm(__) do __ end.yasnippet
;; C649F945-DAB8-4DA2-B73C-2EFF9D7D34F3                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Delete forwardbackward.tmMacro.yasnippet
;; A83F68A9-F751-4BB4-AE16-56812878C16A                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/PStore_new( __ ).yasnippet
;; 5AE7CFB4-418E-4E00-AD76-06DB755EE876                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/xmlread(__).yasnippet
;; F6BF907E-FDF7-4D9B-9E57-BE159561349D                                                       =yyas> (yas/unknown)
;; 
;; 

;; Substitutions for: condition
;; 
;; # as in Macros/xmlread(__).yasnippet
;;                                                                                            =yyas> (yas/unknown)
;; 
;; # as in Snippets/Insert ERb's __ or = __.yasnippet
;; text.html, source.yaml                                                                     =yyas> (yas/unknown)
;; 
;; 

;; Substitutions for: binding
;; 
;; # as in Commands/Completion Ruby (rcodetools).tmCommand.yasnippet
;; ~                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Enclose in + (RDoc comments).yasnippet
;; @k                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Omit from RDoc.yasnippet
;; ^@O                                                                                        =yyas> (yas/unknown)
;; 
;; # as in Commands/Open Require.yasnippet
;; @D                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Validate and Save.yasnippet
;; @s                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Check Ruby Syntax.yasnippet
;; ^V                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Execute Line with Ruby.yasnippet
;; ^E                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Make Destructive Call.yasnippet
;; ^!                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Toggle Quote Style.yasnippet
;; ^"                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Macros/Overwrite } in #{ .. }.yasnippet
;; }                                                                                          =yyas> (yas/unknown)
;; 
;; # as in Snippets/hash pointer.yasnippet
;; ^l                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Macros/Delete forwardbackward.tmMacro.yasnippet
;;                                                                                           =yyas> (yas/unknown)
;; 
;; 

;; .yas-setup.el for ruby-mode ends here
