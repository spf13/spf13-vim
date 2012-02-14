;;;    -*- Emacs-Lisp -*- 
;;;    File:	erlang_appwiz.el  
;;;    Author:	Johan Bevermyr
;;;    Created:	Tue Dec  9 13:14:24 1997
;;;    Purpose:	Adds a simple application wizard to erlang.el.

;; OBS! Must be loaded before the erlang.el file is loaded. 
;; Add the following to your .emacs file before erlang.el is loaded.
;;
;;     (load "erlang_appwiz" t nil)
;;
;; Customisation of makefile generation:
;;
;; The templates for generating makefiles are stored in the 
;; variables erlang-skel-makefile-src and erlang-skel-makefile-middle.
;; 
;; These can be modified by setting the variables before or after this
;; file is loaded.
;; 
;; For example, to generate OTP-style make files:
;;
;;
;;(defvar erlang-skel-makefile-src 
;;  '((erlang-skel-include erlang-skel-nomodule-header)
;;    "CC_ROOT := $(shell pwd | sed 's/erts.*$$//')" n
;;    "AUTOCONF := $(CC_ROOT)/erts/autoconf" n
;;    "TARGET := $(shell $(AUTOCONF)/config.guess)"
;;    "include $(CC_ROOT)/internal_tools/make/$(TARGET)/otp.mk" n
;;    n
;;    "# ----------------------------------------------------" n
;;    "# Application version " n
;;    "# ----------------------------------------------------" n
;;    "include ../vsn.mk" n
;;    "VSN=$(KERNEL_VSN)" n
;;    n
;;    "# ----------------------------------------------------" n
;;    "# Release directory specification" n
;;    "# ----------------------------------------------------" n
;;    "RELEASE_PATH= ../../../release/$(TARGET)" n
;;    "RELSYSDIR = $(RELEASE_PATH)/lib/kernel-$(VSN)" n
;;    n
;;    "# ----------------------------------------------------" n
;;    "# Target Specs" n
;;    "# ----------------------------------------------------" n
;;    n
;;    "MODULES= " appwiz-erlang-modulename n
;;    n
;;    "HRL_FILES=" 
;;    n
;;    INTERNAL_HRL_FILES= appwiz-erlang-modulename "_sup.hrl" n
;;    n
;;    "ERL_FILES= $(MODULES:%=%.erl)" n
;;    n
;;    "TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET)" n
;;    n
;;    "APP_FILE= " appwiz-erlang-modulename ".app" n
;;    n
;;    "APP_SRC= $(APP_FILE).src" n
;;    "APP_TARGET= ../ebin/$(APP_FILE)" n
;;    n
;;    "# ----------------------------------------------------" n
;;    "# FLAGS						   " n
;;    "# ----------------------------------------------------" n
;;    "ERL_FLAGS += 					   " n
;;    "ERL_COMPILE_FLAGS += -I../include" n
;;    n
;;    "# ----------------------------------------------------" n
;;    "# Targets" n
;;    "# ----------------------------------------------------" n
;;    n
;;    "debug opt: $(TARGET_FILES)" n
;;    n
;;    "clean:" n
;;    "	rm -f $(TARGET_FILES) $(GEN_FILES)" n
;;    "	rm -f core" n
;;    n
;;    "docs:" n
;;    n
;;    "# ----------------------------------------------------" n
;;    "# Special Build Targets				   " n
;;    "# ----------------------------------------------------" n
;;    "							   " n
;;    "$(APP_TARGET): $(APP_SRC)				   " n
;;    "	sed -e 's;%VSN%;$(VSN);' $(APP_SRC) > $(APP_TARGET)" n
;;    "							   " n
;;    "# ----------------------------------------------------" n
;;    "# Release Target					   " n
;;    "# ----------------------------------------------------" n 
;;    "include $(CC_ROOT)/internal_tools/make/otp_release_targets.mk" n
;;    n
;;    "release_spec: opt" n
;;    "	$(INSTALL_DIR) $(RELSYSDIR)/src                        " n
;;    "	$(INSTALL_DATA) $(ERL_FILES) $(RELSYSDIR)/src	       " n
;;    "	$(INSTALL_DATA) $(INTERNAL_HRL_FILES) $(RELSYSDIR)/src " n
;;    "	$(INSTALL_DIR) $(RELSYSDIR)/include		       " n
;;    "	$(INSTALL_DATA) $(HRL_FILES) $(RELSYSDIR)/include      " n
;;    "	$(INSTALL_DIR) $(RELSYSDIR)/ebin		       " n
;;    "	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin      " n
;;    n
;;    "release_docs_spec:" n
;;    ))
;;    
;;  
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Erlang application wizard
;;

(defun erlang-application-wizard (directory name)
  "Creates all files and directories needed for an application.
The top-level directory is placed in DIRECTORY. NAME is used when 
creating the root directory and for naming application files."

  (interactive "DApplication root directory: \nsName of application: ")
  (let ((dir nil)
	(lastchar (substring directory (- (length directory) 1)))
	(apptype (completing-read "Type of application: "
				  '(("gen_server" 1)
				    ("gen_event" 2)
				    ("gen_fsm" 3)
				    ("other" 4))
				  nil t "gen_server"))
	(appname      nil)
	(apptemplate  nil)
	(apitemplate  nil)
	(extension nil))

    (if (string= lastchar "/")
	(setq dir directory)
      (setq dir (concat directory "/")))

    ;; determine type of application
    (cond ((string= apptype "gen_server")
	   (setq extension "_server")
	   (setq appname (concat name extension))
	   (setq apptemplate 'tempo-template-erlang-generic-server)
	   (setq apitemplate 'tempo-template-erlang-large-header))
	  ((string= apptype "gen_event")
	   (setq extension "_event")
	   (setq appname (concat name extension))
	   (setq apptemplate 'tempo-template-erlang-gen-event)
	   (setq apitemplate 'tempo-template-erlang-large-header))
	  ((string= apptype "gen_fsm")
	   (setq extension "_fsm")
	   (setq appname (concat name extension))
	   (setq apptemplate 'tempo-template-erlang-gen-fsm)
	   (setq apitemplate 'tempo-template-large-header))
	  (t
	   ;; use defaults _work
	   (setq extension "_work")
	   (setq appname (concat name extension))
	   (setq apptemplate 'tempo-template-erlang-large-header)
	   (setq apitemplate 'tempo-template-erlang-large-header)))

    (setq appwiz-erlang-modulename appname)
    (setq appwiz-erlang-ext extension)

    ;; create directories
    (make-directory (concat dir name "/" "src") t)
    (make-directory (concat dir name "/" "ebin") t)
    (make-directory (concat dir name "/" "include") t)
    
    ;; create directory content
    ;;;;;;;;; .erl 
    (find-file (concat dir name "/" "src/" name ".erl"))
    (funcall apitemplate)
    (insert "API module for the application " name ".")
    (save-buffer)

    ;;;;;;;;; _app.erl
    (find-file (concat dir name "/" "src/" name "_app.erl"))
    (tempo-template-erlang-application)
    (insert "Application callback module for the application " name ".")

    (let ((quotedname (erlang-add-quotes-if-needed
		       (concat name "_sup")))
	  (start (point)))
      (while (search-forward "'TopSupervisor':start_link" nil t)
	(replace-match (concat quotedname ":start_link") nil t))
      (goto-char start))

    (save-buffer)

    ;;;;;;;;; _sup.erl
    (find-file (concat dir name "/" "src/" name "_sup.erl"))
    (tempo-template-erlang-supervisor)
    (insert "Top level supervisor for the application " name ".")


    (let ((quotedname (erlang-add-quotes-if-needed appname))
	  (start (point)))
      (while (search-forward "'AName'" nil t)
	(replace-match quotedname nil t))
      (goto-char start))

    (let ((quotedname (erlang-add-quotes-if-needed appname))
	  (start (point)))
      (goto-char 0)
      (while (search-forward "'AMODULE'" nil t)
	(replace-match quotedname nil t))
      (goto-char start))

    (save-buffer)

    ;;;;;;;;; _sup.hrl
    (find-file (concat dir name "/" "src/" name "_sup.hrl"))
    (tempo-template-erlang-nomodule-header)
    (save-buffer)

    ;;;;;;;;; _(application).erl
    (find-file (concat dir name "/" "src/" appname ".erl"))
    (funcall apptemplate)
    (save-buffer)
    
    ;;;;;;;;; makefile (src)
    (find-file (concat dir name "/" "src/makefile"))
    (setq appwiz-erlang-modulename name)
    (setq appwiz-erlang-ext extension)
    (tempo-template-erlang-makefile-src)
    (insert "Makefile for application " name ".")
    (let ((start (point)))
      (goto-char 0)
      (while (search-forward "%" nil t)
	(replace-match "#" nil t))
      (goto-char start))
    (save-buffer)

    ;;;;;;;;; makefile (middle)
    (find-file (concat dir name "/" "makefile"))
    (tempo-template-erlang-makefile-middle)
    (insert "Makefile for application " name ".")
    (let ((start (point)))
      (goto-char 0)
      (while (search-forward "%" nil t)
	(replace-match "#" nil t))
      (goto-char start))
    (save-buffer)

    ;;;;;;;;; .app
    (find-file (concat dir name "/" "ebin/" name ".app"))
    (erlang-mode)
    (tempo-template-erlang-app)
    (insert "Application specification file for " name ".")
    (save-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; These are setq:ed 
;;

(defvar appwiz-erlang-modulename "foo")
(defvar appwiz-erlang-ext "_work")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Skeletons. 
;;  Skeletons for nomodule header and .app file added by JB.
;; 

(defvar erlang-skel
  '(("If"            "if"            erlang-skel-if)
    ("Case"          "case"          erlang-skel-case)
    ("Receive"       "receive"       erlang-skel-receive)
    ("Receive After" "after"         erlang-skel-receive-after)
    ("Receive Loop"  "loop"          erlang-skel-receive-loop)
    ("Module"        "module"        erlang-skel-module)
    ("Author"        "author"        erlang-skel-author)
    ("Query"         "query"         erlang-skel-query)
    ()
    ("Small Header"  "small-header" 
     erlang-skel-small-header erlang-skel-header)
    ("Normal Header" "normal-header"
     erlang-skel-normal-header erlang-skel-header)
    ("Large Header"  "large-header"
     erlang-skel-large-header erlang-skel-header)
    ("No Moudle Header"  "nomodule-header"
     erlang-skel-nomodule-header erlang-skel-header)
    ()
    ("Small Server"   "small-server"
     erlang-skel-small-server erlang-skel-header)
    ()
    ("application" "application"
     erlang-skel-application erlang-skel-header)
    ("app" "app"
     erlang-skel-app erlang-skel-header)
    ("supervisor" "supervisor"
     erlang-skel-supervisor erlang-skel-header)
    ("supervisor_bridge" "supervisor-bridge"
     erlang-skel-supervisor-bridge erlang-skel-header)
    ("gen_server" "generic-server"
     erlang-skel-generic-server erlang-skel-header)
    ("gen_event" "gen-event"
     erlang-skel-gen-event erlang-skel-header)
    ("gen_fsm" "gen-fsm"
     erlang-skel-gen-fsm erlang-skel-header))
  "*Description of all skeletons templates.
Both functions and menu entries will be created.

Each entry in `erlang-skel' should be a list with three or four
elements, or the empty list.

The first element is the name which shows up in the menu.  The second
is the `tempo' identfier (The string \"erlang-\" will be added in
front of it).  The third is the skeleton descriptor, a variable
containing `tempo' attributes as described in the function
`tempo-define-template'.  The optinal fourth elements denotes a
function which should be called when the menu is selected.

Functions corresponding to every template will be created.  The name
of the function will be `tempo-template-erlang-X' where `X' is the
tempo identifier as specified in the second argument of the elements
in this list.

A list with zero elemets means that the a horisontal line should
be placed in the menu.")  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Template for .app file skeleton
;;

(defvar erlang-skel-app
  '((erlang-skel-include erlang-skel-nomodule-header)
    "{application, "
    (erlang-add-quotes-if-needed (erlang-get-module-from-file-name)) "," n>
    "[{description, \"" (erlang-get-module-from-file-name) "\"}," n>
    "{vsn, \"0.1\"}," n>
    "{modules, ["
    (erlang-add-quotes-if-needed (erlang-get-module-from-file-name)) "," n>
    (erlang-add-quotes-if-needed
     (concat (erlang-get-module-from-file-name) "_app")) "," n>
    (erlang-add-quotes-if-needed
     (concat (erlang-get-module-from-file-name) "_sup")) "," n>
    (erlang-add-quotes-if-needed
     (concat (erlang-get-module-from-file-name) appwiz-erlang-ext)) "]}," n>
    "{registered, ["
    (erlang-add-quotes-if-needed
     (concat (erlang-get-module-from-file-name) appwiz-erlang-ext)) ","
    (erlang-add-quotes-if-needed
     (concat (erlang-get-module-from-file-name) "_sup")) "]}," n>
    "{applications, [kernel," n>
    "stdlib," n> 
    "sasl," n>
    "mnesia]}," n>
    "{env, []}," n>
    "{mod, {"
    (erlang-add-quotes-if-needed
     (concat (erlang-get-module-from-file-name) "_app"))
    ", []}}]}." n
    )
  "*The template of an application file
Please see the function `tempo-define-template'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Template for no-module header skeleton.
;;

(defvar erlang-skel-nomodule-header
  '(o (erlang-skel-separator)
      (erlang-skel-include erlang-skel-copyright-comment
			   erlang-skel-file-comment
			   erlang-skel-author-comment)
      "%%% Purpose : " p n
      (erlang-skel-include erlang-skel-created-comment)
      (erlang-skel-separator) n)
  "*The template of a normal header.
Please see the function `tempo-define-template'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; .app extension added.
;;

(defvar erlang-file-name-extension-regexp "\\.\\(erl\\|hrl\\|app\\)$"
  "*Regexp which should match an erlang file name.

This regexp is used when an Erlang module name is extracted from the
name of an Erlang source file.

The regexp should only match the section of the file name which should
be excluded from the module name.

To match all files set this variable to \"\\\\(\\\\..*\\\\|\\\\)$\".
The matches all except the extension.  This is useful if the Erlang
tags system should interpretate tags on the form `module:tag' for
files written in other languages than Erlang.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Wizard menu added. 
;;

(defvar erlang-menu-items
  '(("Indent"
     (("Indent Line" erlang-indent-command)
      ("Indent Region " erlang-indent-region 
       (if erlang-xemacs-p (mark) mark-active))
      ("Indent Clause" erlang-indent-caluse)
      ("Indent Function" erlang-indent-function)
      ("Indent Buffer" erlang-indent-current-buffer)))
    ("Edit"
     (("Fill Comment" erlang-fill-paragraph)
      ("Comment Region" comment-region
       (if erlang-xemacs-p (mark) mark-active))     
      ("Uncomment Region" erlang-uncomment-region
       (if erlang-xemacs-p (mark) mark-active))     
      nil
      ("beginning of Function" erlang-beginning-of-function)
      ("End of Function" erlang-end-of-function)
      ("Mark Function" erlang-mark-function)
      nil
      ("beginning of Clause" erlang-beginning-of-clause)
      ("End of Clause" erlang-end-of-clause)
      ("Mark Clause" erlang-mark-clause)
      nil
      ("New Clause" erlang-generate-new-clause)
      ("Clone Arguments" erlang-clone-arguments)))
    ("Font Lock Mode"
     (("Level 3" erlang-font-lock-level-3)
      ("Level 2" erlang-font-lock-level-2)
      ("Level 1" erlang-font-lock-level-1)
      ("Off" erlang-font-lock-level-0)))
    ("TAGS"
     (("Find Tag" find-tag)
      ("Find Next Tag" erlang-find-next-tag)
      ;("Find Regexp" find-tag-regexp)
      ("Complete Word" erlang-complete-tag)
      ("Tags Apropos" tags-apropos)
      ("Search Files" tags-search)))
    nil
    ("Erlang Shell" inferior-erlang-run-or-select)
    ("Compile" erlang-compile)
    ("Next Error" inferior-erlang-next-error)
    nil
    ("Version" erlang-version)
    nil
    ("Wizards" 
     (("Application Wizard" erlang-application-wizard))))
  "*Description of menu used in Erlang mode.

This variable must be a list. The elements are either nil representing
a horisontal line or a list with two or three elements.  The first is
the name of the menu item, the second is the function to call, or a
submenu, on the same same form as ITEMS.  The third optional argument
is an expression which is evaluated every time the menu is displayed.
Should the expression evaluate to nil the menu item is ghosted.

Example:
    '((\"Func1\" function-one)
      (\"SubItem\" 
       ((\"Yellow\" function-yellow)
        (\"Blue\" function-blue)))
      nil
      (\"Region Funtion\" spook-function midnight-variable))

Call the function `erlang-menu-init' after modifying this variable.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Prefixing space removed from date string
;;

(defun erlang-skel-d-mmm-yyyy ()
  "Return the current date as a string in \"DD Mon YYYY\" form.
The first character of DD is *not* space if the value is less than 10."
  (let ((date (current-time-string)))
    (format "%d %s %s"
	    (string-to-int (substring date 8 10))
	    (substring date 4 7)
	    (substring date -4))))

(defvar erlang-skel-date-function 'erlang-skel-d-mmm-yyyy
  "*Function which returns date string.
Look in the module `time-stamp' for a battery of functions.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Fixed skeletons. erlang-add-quotes-if-needed introduced where needed.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server templates.

(defvar erlang-skel-small-server
  '((erlang-skel-include erlang-skel-large-header)
    "-export([start/0,init/1])." n n n
    "start() ->" n> "spawn("
    (erlang-add-quotes-if-needed (erlang-get-module-from-file-name))
    ", init, [self()])." n n
    "init(From) ->" n>
    "loop(From)." n n
    "loop(From) ->" n>
    "receive" n>
    p "_ ->" n>
    "loop(From)" n>
    "end."
    )
  "*Template of a small server.
Please see the function `tempo-define-template'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Behaviour templates.

(defvar erlang-skel-application
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(application)." n
    n
    "%% application callbacks" n
    "-export([start/2, stop/1])." n n
    (erlang-skel-separator)
    "%%% Callback functions from application" n
    (erlang-skel-separator)
    n
    (erlang-skel-separator 2)
    "%% Func: start/2" n
    "%% Returns: {ok, Pid}        |" n
    "%%          {ok, Pid, State} |" n
    "%%          {error, Reason}   " n
    (erlang-skel-separator 2)
    "start(Type, StartArgs) ->" n>
    "case 'TopSupervisor':start_link(StartArgs) of" n>
    "{ok, Pid} -> " n>
    "{ok, Pid};" n>
    "Error ->" n>
    "Error" n>
    "end." n
    n
    (erlang-skel-separator 2)
    "%% Func: stop/1" n
    "%% Returns: any "n
    (erlang-skel-separator 2)
    "stop(State) ->" n>
    "ok." n
    n
    (erlang-skel-separator)
    "%%% Internal functions" n
    (erlang-skel-separator)
    )
  "*The template of an application behaviour.
Please see the function `tempo-define-template'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar erlang-skel-supervisor
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(supervisor)." n
    n
    "%% External exports" n
    "-export([start_link/1])." n
    n
    "%% supervisor callbacks" n
    "-export([init/1])." n n
    (erlang-skel-separator)
    "%%% API" n
    (erlang-skel-separator)
    "start_link(StartArgs) ->" n>
    "supervisor:start_link({local, " 
    (erlang-add-quotes-if-needed (erlang-get-module-from-file-name)) "}, "
    (erlang-add-quotes-if-needed (erlang-get-module-from-file-name)) 
    ", StartArgs)." n 
    n
    (erlang-skel-separator)
    "%%% Callback functions from supervisor" n
    (erlang-skel-separator)
    n
    (erlang-skel-separator 2)
    "%% Func: init/1" n
    "%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |" n
    "%%          ignore                          |" n
    "%%          {error, Reason}   " n
    (erlang-skel-separator 2)
    "init(StartArgs) ->" n>
    "AChild = {'AName',{'AModule',start_link,[]}," n>
    "permanent,2000,worker,['AModule']}," n>
    "{ok,{{one_for_all,4,3600}, [AChild]}}." n
    n
    (erlang-skel-separator)
    "%%% Internal functions" n
    (erlang-skel-separator)
    )
  "*The template of an supervisor behaviour.
Please see the function `tempo-define-template'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar erlang-skel-supervisor-bridge
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(supervisor_bridge)." n
    n
    "%% External exports" n
    "-export([start_link/0])." n
    n
    "%% supervisor callbacks" n
    "-export([init/1, terminate/2])." n n
    "-record(state, {})." n 
    n
    (erlang-skel-separator)
    "%%% API" n
    (erlang-skel-separator)
    "start_link() -> " n>
    "supervisor_bridge:start_link({local, " 
    (erlang-add-quotes-if-needed (erlang-get-module-from-file-name)) "}, "
    (erlang-add-quotes-if-needed (erlang-get-module-from-file-name)) 
    ", [])." n 
    n
    (erlang-skel-separator)
    "%%% Callback functions from supervisor_bridge" n
    (erlang-skel-separator)
    n
    (erlang-skel-separator 2)
    "%% Func: init/1" n
    "%% Returns: {ok,  Pid, State} |" n
    "%%          ignore            |" n
    "%%          {error, Reason}    " n
    (erlang-skel-separator 2)
    "init([]) ->" n>
    "case 'AModule':start_link() of" n>
    "{ok, Pid} ->" n>
    "{ok, Pid, #state{}};" n>
    "Error ->" n>
    "Error" n>
    "end." n
    n
    (erlang-skel-separator 2)
    "%% Func: terminate/2" n
    "%% Purpose: Synchronized shutdown of the underlying sub system." n
    "%% Returns: any" n
    (erlang-skel-separator 2)
    "terminate(Reason, State) ->" n>
    "'AModule':stop()," n>
    "ok." n
    n
    (erlang-skel-separator)
    "%%% Internal functions" n
    (erlang-skel-separator)
    )
  "*The template of an supervisor_bridge behaviour.
Please see the function `tempo-define-template'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar erlang-skel-generic-server
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(gen_server)." n
    n
    "%% External exports" n
    "-export([start_link/0])." n
    n
    "%% gen_server callbacks" n
    "-export([init/1, handle_call/3, handle_cast/2, "
    "handle_info/2, terminate/2])." n n
    "-record(state, {})." n 
    n
    (erlang-skel-separator)
    "%%% API" n
    (erlang-skel-separator)
    "start_link() -> " n>
    "gen_server:start_link({local, " 
    (erlang-add-quotes-if-needed (erlang-get-module-from-file-name)) "}, "
    (erlang-add-quotes-if-needed (erlang-get-module-from-file-name)) 
    ", [], [])." n 
    n
    (erlang-skel-separator)
    "%%% Callback functions from gen_server" n
    (erlang-skel-separator)
    n
    (erlang-skel-separator 2)
    "%% Func: init/1" n
    "%% Returns: {ok, State}          |" n
    "%%          {ok, State, Timeout} |" n
    "%%          ignore               |" n
    "%%          {stop, Reason}" n
    (erlang-skel-separator 2)
    "init([]) ->" n>
    "{ok, #state{}}." n
    n
    (erlang-skel-separator 2)
    "%% Func: handle_call/3" n
    "%% Returns: {reply, Reply, State}          |" n
    "%%          {reply, Reply, State, Timeout} |" n
    "%%          {noreply, State}               |" n
    "%%          {noreply, State, Timeout}      |" n
    "%%          {stop, Reason, Reply, State}   | (terminate/2 is called)" n
    "%%          {stop, Reason, State}            (terminate/2 is called)" n
    (erlang-skel-separator 2)
    "handle_call(Request, From, State) ->" n>
    "Reply = ok," n>
    "{reply, Reply, State}." n
    n
    (erlang-skel-separator 2)
    "%% Func: handle_cast/2" n
    "%% Returns: {noreply, State}          |" n
    "%%          {noreply, State, Timeout} |" n
    "%%          {stop, Reason, State}            (terminate/2 is called)" n
    (erlang-skel-separator 2)    
    "handle_cast(Msg, State) ->" n>
    "{noreply, State}." n
    n
    (erlang-skel-separator 2)
    "%% Func: handle_info/2" n
    "%% Returns: {noreply, State}          |" n
    "%%          {noreply, State, Timeout} |" n
    "%%          {stop, Reason, State}            (terminate/2 is called)" n
    (erlang-skel-separator 2)
    "handle_info(Info, State) ->" n>
    "{noreply, State}." n
    n
    (erlang-skel-separator 2)
    "%% Func: terminate/2" n
    "%% Purpose: Shutdown the server" n
    "%% Returns: any (ignored by gen_server)" n
    (erlang-skel-separator 2)
    "terminate(Reason, State) ->" n>
    "ok." n
    n
    (erlang-skel-separator)
    "%%% Internal functions" n
    (erlang-skel-separator)
    )
  "*The template of a generic server.
Please see the function `tempo-define-template'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar erlang-skel-gen-event
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(gen_event)." n
    n
    "%% External exports" n
    "-export([start_link/0, add_handler/0])." n
    n
    "%% gen_event callbacks" n
    "-export([init/1, handle_event/2, handle_call/2, "
    "handle_info/2, terminate/2])." n n
    "-record(state, {})." n 
    n
    (erlang-skel-separator)
    "%%% API" n
    (erlang-skel-separator)
    "start_link() ->" n>
    "gen_event:start_link({local, " 
    (erlang-add-quotes-if-needed (erlang-get-module-from-file-name)) "}). " n
    n
    "add_handler() ->" n>
    "gen_event:add_handler("
    (erlang-add-quotes-if-needed (erlang-get-module-from-file-name)) ", "
    (erlang-add-quotes-if-needed (erlang-get-module-from-file-name)) 
    ", [])." n 
    n
    (erlang-skel-separator)
    "%%% Callback functions from gen_event" n
    (erlang-skel-separator)
    n
    (erlang-skel-separator 2)
    "%% Func: init/1" n
    "%% Returns: {ok, State}          |" n
    "%%          Other" n
    (erlang-skel-separator 2)
    "init([]) ->" n>
    "{ok, #state{}}." n
    n
    (erlang-skel-separator 2)
    "%% Func: handle_event/2" n
    "%% Returns: {ok, State}                                |" n
    "%%          {swap_handler, Args1, State1, Mod2, Args2} |" n
    "%%          remove_handler                              " n
    (erlang-skel-separator 2)
    "handle_event(Event, State) ->" n>
    "{ok, State}." n
    n
    (erlang-skel-separator 2)
    "%% Func: handle_call/2" n
    "%% Returns: {ok, Reply, State}                                |" n
    "%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |" n
    "%%          {remove_handler, Reply}                            " n
    (erlang-skel-separator 2)
    "handle_call(Request, State) ->" n>
    "Reply = ok," n>
    "{ok, Reply, State}." n
    n
    (erlang-skel-separator 2)
    "%% Func: handle_info/2" n
    "%% Returns: {ok, State}                                |" n
    "%%          {swap_handler, Args1, State1, Mod2, Args2} |" n
    "%%          remove_handler                              " n
    (erlang-skel-separator 2)
    "handle_info(Info, State) ->" n>
    "{ok, State}." n
    n
    (erlang-skel-separator 2)
    "%% Func: terminate/2" n
    "%% Purpose: Shutdown the server" n
    "%% Returns: any" n
    (erlang-skel-separator 2)
    "terminate(Reason, State) ->" n>
    "ok." n
    n
    (erlang-skel-separator)
    "%%% Internal functions" n
    (erlang-skel-separator)
    )
  "*The template of a gen_event.
Please see the function `tempo-define-template'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar erlang-skel-gen-fsm
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(gen_fsm)." n
    n
    "%% External exports" n
    "-export([start_link/0])." n
    n
    "%% gen_fsm callbacks" n
    "-export([init/1, state_name/2, state_name/3, handle_event/3," n>
    "handle_sync_event/4, handle_info/3, terminate/3])." n n
    "-record(state, {})." n 
    n
    (erlang-skel-separator)
    "%%% API" n
    (erlang-skel-separator)
    "start_link() ->" n>
    "gen_fsm:start_link({local, " 
    (erlang-add-quotes-if-needed (erlang-get-module-from-file-name)) "}, "
    (erlang-add-quotes-if-needed (erlang-get-module-from-file-name)) 
    ", [], [])." n 
    n
    (erlang-skel-separator)
    "%%% Callback functions from gen_fsm" n
    (erlang-skel-separator)
    n
    (erlang-skel-separator 2)
    "%% Func: init/1" n
    "%% Returns: {ok, StateName, StateData}          |" n
    "%%          {ok, StateName, StateData, Timeout} |" n
    "%%          ignore                              |" n
    "%%          {stop, StopReason}                   " n
    (erlang-skel-separator 2)
    "init([]) ->" n>
    "{ok, state_name, #state{}}." n
    n
    (erlang-skel-separator 2)
    "%% Func: StateName/2" n
    "%% Returns: {next_state, NextStateName, NextStateData}          |" n
    "%%          {next_state, NextStateName, NextStateData, Timeout} |" n
    "%%          {stop, Reason, NewStateData}                         " n
    (erlang-skel-separator 2)
    "state_name(Event, StateData) ->" n>
    "{nextstate, state_name, StateData}." n
    n
    (erlang-skel-separator 2)
    "%% Func: StateName/3" n
    "%% Returns: {next_state, NextStateName, NextStateData}            |" n
    "%%          {next_state, NextStateName, NextStateData, Timeout}   |" n
    "%%          {reply, Reply, NextStateName, NextStateData}          |" n
    "%%          {reply, Reply, NextStateName, NextStateData, Timeout} |" n
    "%%          {stop, Reason, NewStateData}                          |" n
    "%%          {stop, Reason, Reply, NewStateData}                    " n
    (erlang-skel-separator 2)
    "state_name(Event, From, StateData) ->" n>
    "Reply = ok," n>
    "{reply, Reply, state_name, StateData}." n
    n
    (erlang-skel-separator 2)
    "%% Func: handle_event/3" n
    "%% Returns: {next_state, NextStateName, NextStateData}          |" n
    "%%          {next_state, NextStateName, NextStateData, Timeout} |" n
    "%%          {stop, Reason, NewStateData}                         " n
    (erlang-skel-separator 2)
    "handle_event(Event, StateName, StateData) ->" n>
    "{nextstate, StateName, StateData}." n
    n
    (erlang-skel-separator 2)
    "%% Func: handle_sync_event/4" n
    "%% Returns: {next_state, NextStateName, NextStateData}            |" n
    "%%          {next_state, NextStateName, NextStateData, Timeout}   |" n
    "%%          {reply, Reply, NextStateName, NextStateData}          |" n
    "%%          {reply, Reply, NextStateName, NextStateData, Timeout} |" n
    "%%          {stop, Reason, NewStateData}                          |" n
    "%%          {stop, Reason, Reply, NewStateData}                    " n
    (erlang-skel-separator 2)
    "handle_sync_event(Event, From, StateName, StateData) ->" n>
    "Reply = ok," n>
    "{reply, Reply, StateName, StateData}." n
    n
    (erlang-skel-separator 2)
    "%% Func: handle_info/3" n
    "%% Returns: {next_state, NextStateName, NextStateData}          |" n
    "%%          {next_state, NextStateName, NextStateData, Timeout} |" n
    "%%          {stop, Reason, NewStateData}                         " n
    (erlang-skel-separator 2)
    "handle_info(Info, StateName, StateData) ->" n>
    "{nextstate, StateName, StateData}." n
    n
    (erlang-skel-separator 2)
    "%% Func: terminate/3" n
    "%% Purpose: Shutdown the fsm" n
    "%% Returns: any" n
    (erlang-skel-separator 2)
    "terminate(Reason, StateName, StatData) ->" n>
    "ok." n
    n
    (erlang-skel-separator)
    "%%% Internal functions" n
    (erlang-skel-separator)
    )
  "*The template of a gen_fsm.
Please see the function `tempo-define-template'.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Original erlang-add-quotes-if-needed is broken, we install a 
;; new version.
;;

(add-hook 'erlang-load-hook 'my-erlang-load-mods)

(defun fixed-erlang-add-quotes-if-needed (str)
  "Return STR, possibly with quotes."
  (let ((saved-case-fold-search case-fold-search)
	(result nil))
    (setq case-fold-search nil)
    (setq result (if (string-match (concat "\\`" erlang-atom-regexp "\\'") str)
		     str
		   (concat "'" str "'")))
    (setq case-fold-search saved-case-fold-search)
    result))

(defun my-erlang-load-mods ()
 (fset 'erlang-add-quotes-if-needed
	(symbol-function 'fixed-erlang-add-quotes-if-needed))
 (appwiz-skel-init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Additional skeletons which are not shown in the Erlang menu.
;;

(defvar appwiz-skel
  '(
;    ("generic-server-no-api" erlang-skel-generic-server-no-api)
;    ("generic-server-api"    erlang-skel-generic-server-api)
;    ("gen-event-no-api"      erlang-skel-gen-event-no-api)
;    ("gen-event-api"         erlang-skel-gen-event-api)
;    ("gen-fsm-no-api"	     erlang-skel-gen-fsm-no-api)
;    ("gen-fsm-api"	     erlang-skel-gen-fsm-api)
     ("makefile-middle"      erlang-skel-makefile-middle)
     ("makefile-src"         erlang-skel-makefile-src)))

(defun appwiz-skel-init ()
  "Generate the skeleton functions."
  (interactive)
  (condition-case nil
      (require 'tempo)
    (error t))
  (if (featurep 'tempo)
      (let ((skel appwiz-skel))
	(while skel
	  (funcall (symbol-function 'tempo-define-template)
		   (concat "erlang-" (nth 0 (car skel)))
		   ;; The tempo template used contains an `include'
		   ;; function call only, hence changes to the
		   ;; variables describing the templates take effect
		   ;; immdiately.
		   (list (list 'erlang-skel-include (nth 1 (car skel))))
		   (nth 0 (car skel)))
	  (setq skel (cdr skel))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; 
;;
;;(defvar erlang-skel-generic-server-no-api
;;  '((erlang-skel-include erlang-skel-large-header)
;;    "-behaviour(gen_server)." n
;;    n
;;    "%% gen_server callbacks" n
;;    "-export([init/1, handle_call/3, handle_cast/2, "
;;    "handle_info/2, terminate/2])." n n
;;    "-record(state, {})." n 
;;    n
;;    (erlang-skel-separator)
;;    "%%% Callback functions from gen_server" n
;;    (erlang-skel-separator)
;;    n
;;    (erlang-skel-separator 2)
;;    "%% Func: init/1" n
;;    "%% Returns: {ok, State}          |" n
;;    "%%          {ok, State, Timeout} |" n
;;    "%%          ignore               |" n
;;    "%%          {stop, Reason}" n
;;    (erlang-skel-separator 2)
;;    "init([]) ->" n>
;;    "{ok, #state{}}." n
;;    n
;;    (erlang-skel-separator 2)
;;    "%% Func: handle_call/3" n
;;    "%% Returns: {reply, Reply, State}          |" n
;;    "%%          {reply, Reply, State, Timeout} |" n
;;    "%%          {noreply, State}               |" n
;;    "%%          {noreply, State, Timeout}      |" n
;;    "%%          {stop, Reason, Reply, State}   | (terminate/2 is called)" n
;;    "%%          {stop, Reason, State}            (terminate/2 is called)" n
;;    (erlang-skel-separator 2)
;;    "handle_call(Request, From, State) ->" n>
;;    "Reply = ok," n>
;;    "{reply, Reply, State}." n
;;    n
;;    (erlang-skel-separator 2)
;;    "%% Func: handle_cast/2" n
;;    "%% Returns: {noreply, State}          |" n
;;    "%%          {noreply, State, Timeout} |" n
;;    "%%          {stop, Reason, State}            (terminate/2 is called)" n
;;    (erlang-skel-separator 2)    
;;    "handle_cast(Msg, State) ->" n>
;;    "{noreply, State}." n
;;    n
;;    (erlang-skel-separator 2)
;;    "%% Func: handle_info/2" n
;;    "%% Returns: {noreply, State}          |" n
;;    "%%          {noreply, State, Timeout} |" n
;;    "%%          {stop, Reason, State}            (terminate/2 is called)" n
;;    (erlang-skel-separator 2)
;;    "handle_info(Info, State) ->" n>
;;    "{noreply, State}." n
;;    n
;;    (erlang-skel-separator 2)
;;    "%% Func: terminate/2" n
;;    "%% Purpose: Shutdown the server" n
;;    "%% Returns: any (ignored by gen_server)" n
;;    (erlang-skel-separator 2)
;;    "terminate(Reason, State) ->" n>
;;    "ok." n
;;    n
;;    (erlang-skel-separator)
;;    "%%% Internal functions" n
;;    (erlang-skel-separator)
;;    )
;;  "*The template of a generic server.
;;Please see the function `tempo-define-template'.")
;;
;;(defvar erlang-skel-generic-server-api
;;  '((erlang-skel-include erlang-skel-large-header)
;;    "%% External exports" n
;;    "-export([start_link/0])." n
;;    n
;;    (erlang-skel-separator)
;;    "%%% API" n
;;    (erlang-skel-separator)
;;    "start_link() ->" n>
;;    "gen_server:start_link({local, " 
;;    (erlang-add-quotes-if-needed
;;     (concat (erlang-get-module-from-file-name) "_server")) "}, "
;;    (erlang-add-quotes-if-needed
;;     (concat (erlang-get-module-from-file-name) "_server")) ", [], [])." n 
;;    n
;;    ))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;
;;
;;(defvar erlang-skel-gen-event-no-api
;;  '((erlang-skel-include erlang-skel-large-header)
;;    "-behaviour(gen_event)." n
;;    n
;;    "%% gen_event callbacks" n
;;    "-export([init/1, handle_event/2, handle_call/2, "
;;    "handle_info/2, terminate/2])." n n
;;    "-record(state, {})." n 
;;    n
;;    (erlang-skel-separator)
;;    "%%% Callback functions from gen_event" n
;;    (erlang-skel-separator)
;;    n
;;    (erlang-skel-separator 2)
;;    "%% Func: init/1" n
;;    "%% Returns: {ok, State}          |" n
;;    "%%          Other" n
;;    (erlang-skel-separator 2)
;;    "init([]) ->" n>
;;    "{ok, #state{}}." n
;;    n
;;    (erlang-skel-separator 2)
;;    "%% Func: handle_event/2" n
;;    "%% Returns: {ok, State}                                |" n
;;    "%%          {swap_handler, Args1, State1, Mod2, Args2} |" n
;;    "%%          remove_handler                              " n
;;    (erlang-skel-separator 2)
;;    "handle_event(Event, State) ->" n>
;;    "{ok, State}." n
;;    n
;;    (erlang-skel-separator 2)
;;    "%% Func: handle_call/2" n
;;    "%% Returns: {ok, Reply, State}                                |" n
;;    "%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |" n
;;    "%%          {remove_handler, Reply}                            " n
;;    (erlang-skel-separator 2)
;;    "handle_call(Request, State) ->" n>
;;    "Reply = ok," n>
;;    "{ok, Reply, State}." n
;;    n
;;    (erlang-skel-separator 2)
;;    "%% Func: handle_info/2" n
;;    "%% Returns: {ok, State}                                |" n
;;    "%%          {swap_handler, Args1, State1, Mod2, Args2} |" n
;;    "%%          remove_handler                              " n
;;    (erlang-skel-separator 2)
;;    "handle_info(Info, State) ->" n>
;;    "{ok, State}." n
;;    n
;;    (erlang-skel-separator 2)
;;    "%% Func: terminate/2" n
;;    "%% Purpose: Shutdown the server" n
;;    "%% Returns: any" n
;;    (erlang-skel-separator 2)
;;    "terminate(Reason, State) ->" n>
;;    "ok." n
;;    n
;;    (erlang-skel-separator)
;;    "%%% Internal functions" n
;;    (erlang-skel-separator)
;;    )
;;  "*The template of a gen_event.
;;Please see the function `tempo-define-template'.")
;;
;;(defvar erlang-skel-gen-event-api
;;  '((erlang-skel-include erlang-skel-large-header)
;;    "%% External exports" n
;;    "-export([start_link/0, add_handler/0])." n
;;    n
;;    (erlang-skel-separator)
;;    "%%% API" n
;;    (erlang-skel-separator)
;;    "start_link() ->" n>
;;    "gen_event:start_link({local, " 
;;    (erlang-add-quotes-if-needed
;;     (concat (erlang-get-module-from-file-name) "_event")) "}). " n
;;    n
;;    "add_handler() ->" n>
;;    "gen_event:add_handler("
;;    (erlang-add-quotes-if-needed
;;     (concat (erlang-get-module-from-file-name) "_event")) ", "
;;    (erlang-add-quotes-if-needed
;;     (concat (erlang-get-module-from-file-name) "_event")) ", [])." n 
;;    n))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;
;;
;;(defvar erlang-skel-gen-fsm
;;  '((erlang-skel-include erlang-skel-large-header)
;;    "-behaviour(gen_fsm)." n
;;    n
;;    "%% gen_fsm callbacks" n
;;    "-export([init/1, state_name/2, state_name/3, handle_event/3," n>
;;    "handle_sync_event/4, handle_info/3, terminate/3])." n n
;;    "-record(state, {})." n 
;;    n
;;    (erlang-skel-separator)
;;    "%%% Callback functions from gen_fsm" n
;;    (erlang-skel-separator)
;;    n
;;    (erlang-skel-separator 2)
;;    "%% Func: init/1" n
;;    "%% Returns: {ok, StateName, StateData}          |" n
;;    "%%          {ok, StateName, StateData, Timeout} |" n
;;    "%%          ignore                              |" n
;;    "%%          {stop, StopReason}                   " n
;;    (erlang-skel-separator 2)
;;    "init([]) ->" n>
;;    "{ok, state_name, #state{}}." n
;;    n
;;    (erlang-skel-separator 2)
;;    "%% Func: StateName/2" n
;;    "%% Returns: {next_state, NextStateName, NextStateData}          |" n
;;    "%%          {next_state, NextStateName, NextStateData, Timeout} |" n
;;    "%%          {stop, Reason, NewStateData}                         " n
;;    (erlang-skel-separator 2)
;;    "state_name(Event, StateData) ->" n>
;;    "{nextstate, state_name, StateData}." n
;;    n
;;    (erlang-skel-separator 2)
;;    "%% Func: StateName/3" n
;;    "%% Returns: {next_state, NextStateName, NextStateData}            |" n
;;    "%%          {next_state, NextStateName, NextStateData, Timeout}   |" n
;;    "%%          {reply, Reply, NextStateName, NextStateData}          |" n
;;    "%%          {reply, Reply, NextStateName, NextStateData, Timeout} |" n
;;    "%%          {stop, Reason, NewStateData}                          |" n
;;    "%%          {stop, Reason, Reply, NewStateData}                    " n
;;    (erlang-skel-separator 2)
;;    "state_name(Event, From, StateData) ->" n>
;;    "Reply = ok," n>
;;    "{reply, Reply, state_name, StateData}." n
;;    n
;;    (erlang-skel-separator 2)
;;    "%% Func: handle_event/3" n
;;    "%% Returns: {next_state, NextStateName, NextStateData}          |" n
;;    "%%          {next_state, NextStateName, NextStateData, Timeout} |" n
;;    "%%          {stop, Reason, NewStateData}                         " n
;;    (erlang-skel-separator 2)
;;    "handle_event(Event, StateName, StateData) ->" n>
;;    "{nextstate, StateName, StateData}." n
;;    n
;;    (erlang-skel-separator 2)
;;    "%% Func: handle_sync_event/4" n
;;    "%% Returns: {next_state, NextStateName, NextStateData}            |" n
;;    "%%          {next_state, NextStateName, NextStateData, Timeout}   |" n
;;    "%%          {reply, Reply, NextStateName, NextStateData}          |" n
;;    "%%          {reply, Reply, NextStateName, NextStateData, Timeout} |" n
;;    "%%          {stop, Reason, NewStateData}                          |" n
;;    "%%          {stop, Reason, Reply, NewStateData}                    " n
;;    (erlang-skel-separator 2)
;;    "handle_sync_event(Event, From, StateName, StateData) ->" n>
;;    "Reply = ok," n>
;;    "{reply, Reply, StateName, StateData}." n
;;    n
;;    (erlang-skel-separator 2)
;;    "%% Func: handle_info/3" n
;;    "%% Returns: {next_state, NextStateName, NextStateData}          |" n
;;    "%%          {next_state, NextStateName, NextStateData, Timeout} |" n
;;    "%%          {stop, Reason, NewStateData}                         " n
;;    (erlang-skel-separator 2)
;;    "handle_info(Info, StateName, StateData) ->" n>
;;    "{nextstate, StateName, StateData}." n
;;    n
;;    (erlang-skel-separator 2)
;;    "%% Func: terminate/3" n
;;    "%% Purpose: Shutdown the fsm" n
;;    "%% Returns: any" n
;;    (erlang-skel-separator 2)
;;    "terminate(Reason, StateName, StatData) ->" n>
;;    "ok." n
;;    n
;;    (erlang-skel-separator)
;;    "%%% Internal functions" n
;;    (erlang-skel-separator)
;;    )
;;  "*The template of a gen_fsm.
;;Please see the function `tempo-define-template'.")
;;
;;(defvar erlang-skel-gen-fsm-no-api
;;  '((erlang-skel-include erlang-skel-large-header)
;;        "%% External exports" n
;;    "-export([start_link/0])." n
;;    n
;;    (erlang-skel-separator)
;;    "%%% API" n
;;    (erlang-skel-separator)
;;    "start_link() ->" n>
;;    "gen_fsm:start_link({local, " 
;;    (erlang-add-quotes-if-needed
;;     (concat (erlang-get-module-from-file-name) "_fsm")) "}, "
;;    (erlang-add-quotes-if-needed
;;     (concat (erlang-get-module-from-file-name) "_fsm")) ", [], [])." n 
;;    n
;;    ))
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; requires that the variables appwiz-erlang-modulename and
;; appwiz-erlang-ext are defined.
;;

(defvar erlang-skel-makefile-src
  '((erlang-skel-include erlang-skel-nomodule-header)
    "MAKE = make" n
    n
    "ERL = erlc" n
    n
    "EBIN = ../ebin" n
    n
    (erlang-skel-makefile-separator)
    n
    (upcase appwiz-erlang-modulename) "_HEADER_FILES = "
    appwiz-erlang-modulename "_sup.hrl" n
    n
    (upcase appwiz-erlang-modulename) "_SOURCE_FILES = \\" n
    "	" appwiz-erlang-modulename ".erl" " "
    appwiz-erlang-modulename "_sup.erl \\" n
    "	" appwiz-erlang-modulename "_app.erl" " "
    appwiz-erlang-modulename appwiz-erlang-ext ".erl" n
    n
    (upcase appwiz-erlang-modulename) "_OBJECT_FILES = $("
    (upcase appwiz-erlang-modulename) "_SOURCE_FILES:.erl=.jam)" n
    n
    n
    (erlang-skel-makefile-separator)
    "#" n
    "# Transformations " n
    "#" n
    n
    ".erl.jam:" n
    "	$(ERL) $<" n
    n
    (erlang-skel-makefile-separator) n
    n
    n
    "def : "
    appwiz-erlang-modulename n
    n
    appwiz-erlang-modulename ": $("
    (upcase appwiz-erlang-modulename) "_OBJECT_FILES)" n
    "	cp $(" (upcase appwiz-erlang-modulename) "_OBJECT_FILES) "
    "$(EBIN)" n
    n
    "clean :" n
    "	/bin/rm -f $(" (upcase appwiz-erlang-modulename)
    "_OBJECT_FILES)" n
    n
    "$(" (upcase appwiz-erlang-modulename) "_OBJECT_FILES): $("
    (upcase appwiz-erlang-modulename) "_HEADER_FILES)" n
    n
    ".SUFFIXES : .erl .jam" n
    n
    ))

(defvar erlang-skel-makefile-middle
  '((erlang-skel-include erlang-skel-nomodule-header)
    "MAKE = make" n
    n
    (erlang-skel-makefile-separator)
    n
    "def:" n
    "	(cd src ; $(MAKE))" n
    n
    "clean:" n
    "	(cd src ; $(MAKE) clean)" n
    n
    ))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun erlang-skel-makefile-separator ()
  "Return a comment separator."
  (concat (make-string 70 ?\#) "\n"))
