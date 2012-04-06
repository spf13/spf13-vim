;;; groovy-mode.el --- Groovy mode derived mode

;;  Author: Russel Winder <russel@winder.org.uk>
;;  Created: 2006-08-01
;;  Version: 201203310931

;;;; NB Version number is date and time yyyymmddhhMM in GMT (aka UTC).

;;  Copyright (C) 2006,2009-10,2012 Russel Winder

;;  This program is free software; you can redistribute it and/or modify it under the terms of the GNU
;;  General Public License as published by the Free Software Foundation; either version 2 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
;;  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;;  License for more details.
;;
;;  You should have received a copy of the GNU General Public License along with this program; if not, write
;;  to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

;;; Authors:
;;
;;  Russel Winder <russel@winder.org.uk>, 2006--
;;  Jim Morris <morris@wolfman.com>, 2009--

;;; Commentary:
;;
;;  This mode was initially developed using the Java and Awk modes that are part of CC Mode (the 5.31 source
;;  was used) and C# Mode from Dylan R. E. Moonfire <contact@mfgames.com> (the 0.5.0 source was used).  This
;;  code may contain some code fragments from those sources that was cut-and-pasted then edited.  All other
;;  code was newly entered by the author.  Obviously changes have been made since then.
;;
;; NB  This derived mode requires CC Mode 5.31 or later for the virtual semicolon code to work.
;;
;;  There appears to be a problem in CC Mode 5.31 such that csharp-mode and groovy-mode crash XEmacs 21.4 if
;;  the files are byte compiled.

;;; Bugs:
;;
;;  Bug tracking is currently (2009-11-26) handled using the Groovy JIRA via the Emacs Mode component.
;;  cf. http://jira.codehaus.org/browse/GROOVY/component/14245

;;; Versions:
;;
;;    0.1.0 - will be the initial release when it is ready :-)

;;; Notes:
;;
;;  Need to think about the `*.', `?.', `.&' and `.@' operators.  Also, `..' and `..<'.  This probably means
;;  changing `c-after-id-concat-ops' but also `c-operators'.
;;
;;  Need to deal with operator overloading (groovy has this but Java does not) so `c-overloadable-operators'
;;  needs investigating.
;;
;;  Need to investigate how to support the triple string delimiters for multi-line strings.
;;
;;  Should we support GString / template markup ( e.g. `<%' and `%>') specially?
;;
;;  Need to think whether Groovy needs a different c-decl-prefix-re compared to Java.  Certainly, Java will
;;  have to change to handle the generics.
;;
;;  Probably need to change `c-block-prefix-disallowed-chars' as Groovy is not the same as Java.
;;
;;  Probably need to change `c-type-decl-suffix-key' as Groovy is not the same as Java.

;;;  Changes:
;;
;;  See the history in the Bazaar branch.

;;; Code:

(require 'cc-mode)

;; CSharp mode comment says: These are only required at compile time to get the sources for the language
;; constants.  (The cc-fonts require and the font-lock related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is necessary to get them compiled.)
(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (load "cc-mode" nil t) ; C# mode has this
    (load "cc-fonts" nil t) ; C# mode has this
    (load "cc-langs" nil t) ; C# mode has this
    (load "cc-bytecomp" nil t) ; Awk mode has this
))

(eval-and-compile
  (c-add-language 'groovy-mode 'java-mode))

;;  Groovy allows `?.' as well as `.' for creating identifiers.
(c-lang-defconst c-identifier-ops
                 groovy '((left-assoc "." "?.")))

;; Groovy allows operators such as `*.', `?.', `.&' and `.@'.  Java mode puts `*' here to deal with
;; import statement usage which we need for Groovy.
(c-lang-defconst c-after-id-concat-ops
  groovy '( "*" "&" "@" ))

;;;;  Should really do something with `c-string-escaped-newlines' and `c-multiline-string-start-char' to
;;;;  handle the triple delimeter multiline strings.

;; Because of the above we have to redefine `c_operators' because no other language has `.&' and
;; `.@' operators.

(c-lang-defconst c-operators
  "List describing all operators, along with their precedence and
associativity.  The order in the list corresponds to the precedence of
the operators: The operators in each element is a group with the same
precedence, and the group has higher precedence than the groups in all
following elements.  The car of each element describes the type of of
the operator group, and the cdr is a list of the operator tokens in
it.  The operator group types are:

'prefix         Unary prefix operators.
'postfix        Unary postfix operators.
'postfix-if-paren
		Unary postfix operators if and only if the chars have
		parenthesis syntax.
'left-assoc     Binary left associative operators (i.e. a+b+c means (a+b)+c).
'right-assoc    Binary right associative operators (i.e. a=b=c means a=(b=c)).
'right-assoc-sequence
                Right associative operator that constitutes of a
                sequence of tokens that separate expressions.  All the
                tokens in the group are in this case taken as
                describing the sequence in one such operator, and the
                order between them is therefore significant.

Operators containing a character with paren syntax are taken to match
with a corresponding open/close paren somewhere else.  A postfix
operator with close paren syntax is taken to end a postfix expression
started somewhere earlier, rather than start a new one at point.  Vice
versa for prefix operators with open paren syntax.

Note that operators like \".\" and \"->\" which in language references
often are described as postfix operators are considered binary here,
since CC Mode treats every identifier as an expression."

  groovy `(
           ;; Primary.
           ,@(c-lang-const c-identifier-ops)
             
             (postfix-if-paren "<" ">") ; Templates.
             
             (prefix "super")
             
             ;; Postfix.
             (left-assoc "." "*." "?." ".&" ".@")
             
             (postfix "++" "--" "[" "]" "(" ")" "<:" ":>")
             
             ;; Unary.
             (prefix "++" "--" "+" "-" "!" "~" "new" "(" ")")
             
             ;; Multiplicative.
             (left-assoc "*" "/" "%")
             
             ;; Additive.
             (left-assoc "+" "-")
             
             ;; Shift.
             (left-assoc "<<" ">>" ">>>")
             
             ;; Relational.
             (left-assoc "<" ">" "<=" ">=" "instanceof" "<=>")
             
             ;; Matching.
             (left-assoc "=~" "==~" )

             ;; Equality.
             (left-assoc "==" "!=" )
             
             ;; Bitwise and.
             (left-assoc "&")
             
             ;; Bitwise exclusive or.
             (left-assoc "^")
             
             ;; Bitwise or.
             (left-assoc "|")
             
             ;; Logical and.
             (left-assoc "&&")
             
             ;; Logical or.
             (left-assoc "||")
             
             ;; Conditional.
             (right-assoc-sequence "?" ":")
             
             ;; Assignment.
             (right-assoc ,@(c-lang-const c-assignment-operators))
             
             ;; Exception.
             ;(prefix "throw") ; Java mode didn't have this but c++ mode does.  Humm...
             
             ;; Sequence.
             (left-assoc ",")

             ;; Separator for parameter list and code in a closure.
             (left-assoc "->")
             ))

;;  Groovy can overload operators where Java cannot.
(c-lang-defconst c-overloadable-operators
                 groovy '("+" "-" "*" "/" "%"
                          "&" "|" "^" "~" "<<" ">>" ">>>"
                          "==" "!=" ">" "<" ">=" "<="
                          "<=>"
                          "=~" "==~" 
                          "++" "--" "+=" "-=" "*=" "/=" "%="
                          "&=" "|=" "^=" "~=" "<<=" ">>=" ">>>="
                          "!" "&&" "||"))

;; Groovy allows newline to terminate a statement unlike Java and like Awk.  We draw on the Awk
;; Mode `Virtual semicolon material.  The idea is to say when an EOL is a `virtual semicolon,
;; i.e. a statement terminator.

(c-lang-defconst c-stmt-delim-chars
                 groovy "^;{}\n\r?:")

(c-lang-defconst c-stmt-delim-chars-with-comma
                 groovy "^;,{}\n\r?:")

;;  Is there a virtual semicolon at POS or point?
;;
;;  A virtual semicolon is considered to lie just after the last non-syntactic-whitespace
;; character on a line where the EOL is the statement terminator.  A real semicolon never
;; counts as a virtual one.
(defun groovy-at-vsemi-p ( &optional pos )
  (save-excursion
	(let ((pos-or-point (if pos (goto-char pos) (point))))
	  (if (eq pos-or-point (point-min))
		  nil
		(and
		 (not (char-equal (char-before) ?\;))
		 (groovy-ws-or-comment-to-eol-p pos-or-point)
		 (groovy-not-in-statement-p pos-or-point)
		 (groovy-not-if-or-else-etc-p pos-or-point))))))

(c-lang-defconst c-at-vsemi-p-fn
                 groovy 'groovy-at-vsemi-p)

;; see if end of line or comment on rest of line
(defun groovy-ws-or-comment-to-eol-p ( pos )
  (save-excursion
    (goto-char pos)
    (skip-chars-forward " \t")
       (or
        (char-equal (char-after) ?\n)
        (looking-at "/[/*].*"))))

(defun groovy-not-in-statement-p ( pos )
  (save-excursion
    (goto-char pos)
    (if (equal (point) (point-min))
        nil
      (backward-char 1)
      (or
       (not (looking-at "[=+*%<{:]"))
       (if (char-equal (char-after) ?>)
           (if (equal (point) (point-min))
               nil
             (char-equal (char-before) ?-)))))))

;; check for case of if(stuff) and nothing else on line
;; ie
;; if(x > y)
;;
;; if(x < y) do somehting will not match
;; else blah blah will not match either
(defun groovy-not-if-or-else-etc-p ( pos )
  (save-excursion
    (goto-char pos)
	(back-to-indentation)
	(not
	 (or 
	  (and (looking-at "if") ; make sure nothing else on line
		   (progn (forward-sexp 2)
				  (groovy-ws-or-comment-to-eol-p (point))))
	  (and (looking-at "}?else")
		   (progn (forward-char)
				  (forward-sexp 1)
				  (groovy-ws-or-comment-to-eol-p (point))))))))

(defun groovy-vsemi-status-unknown-p () nil)

(c-lang-defconst c-vsemi-status-unknown-p-fn
                 groovy 'c-groovy-vsemi-status-unknown-p)


;;  Java does not do this but perhaps it should?
(c-lang-defconst c-type-modifier-kwds
                 groovy '("volatile" "transient"))

(c-lang-defconst c-typeless-decl-kwds
                 groovy (append (c-lang-const c-class-decl-kwds)
                                (c-lang-const c-brace-list-decl-kwds)
                                '("def")))

;;;;  Should we be tinkering with `c-block-stmt-1-key' or `c-block-stmt-2-key' to deal with closures
;;;;  following what appears to be function calls or even field names?

;; Groovy allows use of `<%' and `%>' in template expressions.
;(c-lang-defconst c-other-op-syntax-tokens
;  groovy '( "<%" "%>" ))

;; Groovy does not allow the full set of Java keywords in the moifier category and, of course, there is the
;; `def' modifier which Groovy introduces to support dynamic typing.  Should `const' be treated
;; as reserved here as it is in Java?
(c-lang-defconst c-modifier-kwds
                 groovy '( "abstract" "def" "final" "private" "protected" "public" "static" "synchronized" ))

;;  Java does not define these pseudo-kewords as keywords, why not?

(c-lang-defconst c-constant-kwds
  groovy '( "true" "false" "null" ))

;;  Why does Java mode not put `super' into the `c-primary-expr-kwds?

(c-lang-defconst c-primary-expr-kwds
  groovy '( "this" "super" ))

;;  Groovy does not allow anonymous classes as Java does.
(c-lang-defconst c-inexpr-class-kwds
                 groovy nil)

(c-lang-defconst c-inexpr-brace-list-kwds
                 groovy nil)

;;;;  Should we be changing `c-opt-inexpr-brace-list-key' to deal with closures after function calls and
;;;;  field expressions?

;; We need to include the "as" for the cast and "in" for for.
(c-lang-defconst c-other-kwds
                 groovy '( "in" "as" ))


(defconst groovy-font-lock-keywords-1 (c-lang-const c-matchers-1 groovy)
  "Minimal highlighting for Groovy mode.
Fontifies nothing except the syntactic fontification of strings and
comments.")

(defconst groovy-font-lock-keywords-2 (c-lang-const c-matchers-2 groovy)
  "Fast normal highlighting for Groovy mode.
In addition to `java-font-lock-keywords-1', this adds fontification of
keywords, simple types, declarations that are easy to recognize, the
user defined types on `java-font-lock-extra-types', and the doc
comment styles specified by `c-doc-comment-style'.")

(defconst groovy-font-lock-keywords-3 (c-lang-const c-matchers-3 groovy)
  "Accurate normal highlighting for Groovy mode.
Like `java-font-lock-keywords-2' but detects declarations in a more
accurate way that works in most cases for arbitrary types without the
need for `java-font-lock-extra-types'.")

(defvar groovy-font-lock-keywords groovy-font-lock-keywords-3
  "Default expressions to highlight in Groovy mode.")

(defun groovy-font-lock-keywords-2 ()
  (c-compose-keywords-list groovy-font-lock-keywords-2))
(defun groovy-font-lock-keywords-3 ()
  (c-compose-keywords-list groovy-font-lock-keywords-3))
(defun groovy-font-lock-keywords ()
  (c-compose-keywords-list groovy-font-lock-keywords))

(defvar groovy-mode-syntax-table nil
  "Syntax table used in Groovy mode buffers.")
(or groovy-mode-syntax-table
    (setq groovy-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table groovy))))

(defvar groovy-mode-abbrev-table nil
  "Abbreviation table used in groovy-mode buffers.")
(c-define-abbrev-table 'groovy-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the syntactic context, and which
  ;; therefore should trigger reindentation when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)
    ("finally" "finally" c-electric-continued-statement 0)))

;;  Jim Morris proposed changing to the following definition of groovy-mode-map 2009-11-27, but this change
;;  has not made so as to continue to use the same code structure as still used in the Java mode.

;(defvar groovy-mode-map (let ((map (c-make-inherited-keymap)))
;                                                  ;; Add bindings which are only useful for Groovy
;                                                  map)
;  "Keymap used in groovy-mode buffers.")

(defvar groovy-mode-map ()
  "Keymap used in groovy-mode buffers.")
(if groovy-mode-map
    nil
  (setq groovy-mode-map (c-make-inherited-keymap))
  ;; add bindings which are only useful for Groovy
  )

;(easy-menu-define c-groovy-menu groovy-mode-map "Groovy Mode Commands"
;                (cons "Groovy" (c-lang-const c-mode-menu groovy)))

;;; Autoload mode trigger
;;;###autoload
;(eval-after-load 'groovy-mode
;  (add-to-list 'auto-mode-alist '("\\.groovy" . groovy-mode)))
(add-to-list 'auto-mode-alist '("\\.groovy$" . groovy-mode))

;; Custom variables
;;;###autoload
(defcustom groovy-mode-hook nil
  "*Hook called by `groovy-mode'."
  :type 'hook
  :group 'c)


;;; The following are used to overide cc-mode indentation behavior to match groovy

;; if we are in a closure that has an argument eg ends with -> (excluding comment) then
;; change indent else lineup with previous one
(defun groovy-mode-fix-closure-with-argument (langelem)
  (save-excursion 
	(back-to-indentation)	
	(c-backward-syntactic-ws)
	(backward-char 2)
	(if (looking-at "->")                                  ; if the line has a -> in it 
		(vector (+ (current-indentation) c-basic-offset))  ; then indent from base
	  0)))

;; A helper function from: http://mihai.bazon.net/projects/emacs-javascript-mode/javascript.el
;; Originally named js-lineup-arglist, renamed to groovy-lineup-arglist
(defun groovy-lineup-arglist (langelem)
  ;; the "DWIM" in c-mode doesn't Do What I Mean.
  ;; see doc of c-lineup-arglist for why I redefined this
  (save-excursion
    (let ((indent-pos (point)))
      ;; Normal case.  Indent to the token after the arglist open paren.
      (goto-char (c-langelem-2nd-pos c-syntactic-element))
      (if (and c-special-brace-lists
               (c-looking-at-special-brace-list))
          ;; Skip a special brace list opener like "({".
          (progn (c-forward-token-2)
                 (forward-char))
        (forward-char))
      (let ((arglist-content-start (point)))
        (c-forward-syntactic-ws)
        (when (< (point) indent-pos)
          (goto-char arglist-content-start)
          (skip-chars-forward " \t"))
        (vector (current-column))))))

(defun is-groovy-mode ()
  "return t if we are in groovy mode else nil"
  (eq major-mode 'groovy-mode))

;; use defadvice to override the syntactic type if we have a
;; statement-cont, see if previous line has a virtual semicolon and if
;; so make it statement.
(defadvice c-guess-basic-syntax (after c-guess-basic-syntax-groovy activate)
  (when (is-groovy-mode)
	(save-excursion
	  (let* ((ankpos (progn 
					   (beginning-of-line)
					   (c-backward-syntactic-ws)
					   (beginning-of-line)
					   (c-forward-syntactic-ws)
					   (point))) ; position to previous non-blank line
			 (curelem (c-langelem-sym (car ad-return-value))))
		(end-of-line)
		(cond
		 ((eq 'statement-cont curelem)
		  (when (groovy-at-vsemi-p) ; if there is a virtual semi there then make it a statement
			(setq ad-return-value `((statement ,ankpos)))))
		 
		 ((eq 'topmost-intro-cont curelem)
		  (when (groovy-at-vsemi-p) ; if there is a virtual semi there then make it a top-most-intro
			(setq ad-return-value `((topmost-intro ,ankpos)))))
		
		 )))))

;; This disables bracelists, as most of the time in groovy they are closures
;; We need to check we are currently in groovy mode
(defadvice c-inside-bracelist-p (around groovy-c-inside-bracelist-p activate)
  (if (not (is-groovy-mode))
	  ad-do-it
 	(setq ad-return-value nil)))


;; based on java-function-regexp
;; Complicated regexp to match method declarations in interfaces or classes
;; A nasty test case is:
;;    else if(foo instanceof bar) {
;; which will get mistaken for a function as Groovy does not require types on arguments
;; so we need to check for empty parens or comma separated list, or type args
(defvar groovy-function-regexp
  (concat
   "^[ \t]*"                                   ; leading white space
   "\\(public\\|private\\|protected\\|"        ; some of these 8 keywords
   "abstract\\|final\\|static\\|"
   "synchronized\\|native|def"
   "\\|[ \t\n\r]\\)*"                          ; or whitespace
   "[a-zA-Z0-9_$]*"                            ; optional return type
   "[ \t\n\r]*[[]?[]]?"                        ; (could be array)
   "[ \t\n\r]+"                                ; whitespace
   "\\([a-zA-Z0-9_$]+\\)"                      ; the name we want
   "[ \t\n\r]*"                                ; optional whitespace
   "("                                         ; open the param list
   "[ \t]*"                                    ; optional whitespace
   "\\("
   "[ \t\n\r]*\\|"                             ; empty parens or
   "[a-zA-Z0-9_$]+\\|"                         ; single param or
   ".+?,.+?\\|"                                ; multi comma separated params or
   "[a-zA-Z0-9_$]+"                            ; a type
   "[ \t\n\r]*[[]?[]]?"                        ; optional array
   "[ \t\n\r]+[a-zA-Z0-9_$]+"                  ; and param
   "\\)"
   "[ \t\n\r]*"                                ; optional whitespace
   ")"                                         ; end the param list
   "[ \t\n\r]*"                                ; whitespace
;   "\\(throws\\([, \t\n\r]\\|[a-zA-Z0-9_$]\\)+\\)?{"
   "\\(throws[^{;]+\\)?"                       ; optional exceptions
   "[;{]"                                      ; ending ';' (interfaces) or '{' 
										       ; TODO groovy interfaces don't need to end in ;
   )
  "Matches method names in groovy code, select match 2")

(defvar groovy-class-regexp
  "^[ \t\n\r]*\\(final\\|abstract\\|public\\|[ \t\n\r]\\)*class[ \t\n\r]+\\([a-zA-Z0-9_$]+\\)[^;{]*{"
  "Matches class names in groovy code, select match 2")

(defvar groovy-interface-regexp
  "^[ \t\n\r]*\\(abstract\\|public\\|[ \t\n\r]\\)*interface[ \t\n\r]+\\([a-zA-Z0-9_$]+\\)[^;]*;"
  "Matches interface names in groovy code, select match 2")

(defvar groovy-imenu-regexp
  (list (list nil groovy-function-regexp 2)
        (list ".CLASSES." groovy-class-regexp 2)
        (list ".INTERFACES." groovy-interface-regexp 2)
		(list ".CLOSURES." 	"def[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]*=[ \t]*{" 1))
  "Imenu expression for Groovy")


;; Setup imenu to extract functions, classes, interfaces and closures assigned to variables
(defvar cc-imenu-groovy-generic-expression
  groovy-imenu-regexp
  "Imenu generic expression for Groovy mode.  See `imenu-generic-expression'.")

;;; The entry point into the mode
;;;###autoload
(defun groovy-mode ()
  "Major mode for editing Groovy code.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `groovy-mode-hook'.

Key bindings:
\\{groovy-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table groovy-mode-syntax-table)
  (setq major-mode 'groovy-mode
	mode-name "Groovy"
	local-abbrev-table groovy-mode-abbrev-table
	abbrev-mode t)
  (use-local-map groovy-mode-map)
  (c-init-language-vars groovy-mode)
  (c-common-init 'groovy-mode)
  ;;(easy-menu-add groovy-menu)
  (cc-imenu-init cc-imenu-groovy-generic-expression)
  (c-run-mode-hooks 'c-mode-common-hook 'groovy-mode-hook)

  ;; quick fix for misalignment of statements with =
  (setq c-label-minimum-indentation 0)

  ;; fix for indentation after a closure param list
  (c-set-offset 'statement 'groovy-mode-fix-closure-with-argument)

  ;; get arglists (in groovy lists or maps) to align properly
  (c-set-offset 'arglist-close '(c-lineup-close-paren))
  (c-set-offset 'arglist-cont 0)
  (c-set-offset 'arglist-cont-nonempty '(groovy-lineup-arglist))
  (c-set-offset 'arglist-intro '+)

  (c-update-modeline))


(provide 'groovy-mode)

;;; groovy-mode.el ends here
