;;
;; %CopyrightBegin%
;;
;; Copyright Ericsson AB 2010. All Rights Reserved.
;;
;; The contents of this file are subject to the Erlang Public License,
;; Version 1.1, (the "License"); you may not use this file except in
;; compliance with the License. You should have received a copy of the
;; Erlang Public License along with this software. If not, it can be
;; retrieved online at http://www.erlang.org/.
;;
;; Software distributed under the License is distributed on an "AS IS"
;; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
;; the License for the specific language governing rights and limitations
;; under the License.
;;
;; %CopyrightEnd%
;;;
;;; Purpose: Provide Erlang code skeletons.
;;; See 'erlang-skel-file' variable.

(defvar erlang-tempo-tags nil
  "Tempo tags for erlang mode")

(defvar erlang-skel
  '(("If"            "if"            erlang-skel-if)
    ("Case"          "case"          erlang-skel-case)
    ("Receive"       "receive"       erlang-skel-receive)
    ("Receive After" "after"         erlang-skel-receive-after)
    ("Receive Loop"  "loop"          erlang-skel-receive-loop)
    ("Module"        "module"        erlang-skel-module)
    ("Author"        "author"        erlang-skel-author)
    ()
    ("Small Header"  "small-header"
     erlang-skel-small-header erlang-skel-header)
    ("Normal Header" "normal-header"
     erlang-skel-normal-header erlang-skel-header)
    ("Large Header"  "large-header"
     erlang-skel-large-header erlang-skel-header)
    ()
    ("Small Server"   "small-server"
     erlang-skel-small-server erlang-skel-header)
    ()
    ("Application" "application"
     erlang-skel-application erlang-skel-header)
    ("Supervisor" "supervisor"
     erlang-skel-supervisor erlang-skel-header)
    ("supervisor_bridge" "supervisor-bridge"
     erlang-skel-supervisor-bridge erlang-skel-header)
    ("gen_server" "generic-server"
     erlang-skel-generic-server erlang-skel-header)
    ("gen_event" "gen-event"
     erlang-skel-gen-event erlang-skel-header)
    ("gen_fsm" "gen-fsm"
     erlang-skel-gen-fsm erlang-skel-header)
    ("Library module" "gen-lib"
     erlang-skel-lib erlang-skel-header)
    ("Corba callback" "gen-corba-cb"
     erlang-skel-corba-callback erlang-skel-header)
    ("Small Common Test suite" "ct-test-suite-s"
     erlang-skel-ct-test-suite-s erlang-skel-header)
    ("Large Common Test suite" "ct-test-suite-l"
     erlang-skel-ct-test-suite-l erlang-skel-header)
    ("Erlang TS test suite" "ts-test-suite"
     erlang-skel-ts-test-suite erlang-skel-header)
  )
  "*Description of all skeleton templates.
Both functions and menu entries will be created.

Each entry in `erlang-skel' should be a list with three or four
elements, or the empty list.

The first element is the name which shows up in the menu.  The second
is the `tempo' identifier (The string \"erlang-\" will be added in
front of it).  The third is the skeleton descriptor, a variable
containing `tempo' attributes as described in the function
`tempo-define-template'.  The optional fourth elements denotes a
function which should be called when the menu is selected.

Functions corresponding to every template will be created.  The name
of the function will be `tempo-template-erlang-X' where `X' is the
tempo identifier as specified in the second argument of the elements
in this list.

A list with zero elements means that the a horizontal line should
be placed in the menu.")

;; In XEmacs `user-mail-address' returns "x@y.z (Foo Bar)" ARGH!
;; What's wrong with that? RFC 822 says it's legal.   [sverkerw]
;; This needs to use the customized value.  If that's not sane, things like
;; add-log will lose anyhow.  Avoid it if there _is_ a paren.
(defvar erlang-skel-mail-address
  (if (or (not user-mail-address) (string-match "(" user-mail-address))
      (concat (user-login-name) "@"
	      (or (and (boundp 'mail-host-address)
		       mail-host-address)
		  (system-name)))
    user-mail-address)
  "Mail address of the user.")

;; Expression templates:
(defvar erlang-skel-case
  '((erlang-skel-skip-blank) o >
    "case " p " of" n> p "_ ->" n> p "ok" n> "end" p)
  "*The skeleton of a `case' expression.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-if
  '((erlang-skel-skip-blank) o >
    "if"  n> p " ->" n> p "ok" n> "end" p)
  "The skeleton of an `if' expression.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-receive
  '((erlang-skel-skip-blank) o >
    "receive" n> p "_ ->" n> p "ok" n> "end" p)
  "*The skeleton of a `receive' expression.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-receive-after
  '((erlang-skel-skip-blank) o >
    "receive" n> p "_ ->" n> p "ok" n> "after " p "T ->" n>
    p "ok" n> "end" p)
  "*The skeleton of a `receive' expression with an `after' clause.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-receive-loop
  '(& o "loop(" p ") ->" n> "receive" n> p "_ ->" n>
      "loop(" p ")" n> "end.")
  "*The skeleton of a simple `receive' loop.
Please see the function `tempo-define-template'.")


;; Attribute templates

(defvar erlang-skel-module
  '(& "-module("
      (erlang-add-quotes-if-needed (erlang-get-module-from-file-name))
      ")." n)
  "*The skeleton of a `module' attribute.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-author
  '(& "-author('" erlang-skel-mail-address "')." n)
  "*The skeleton of a `author' attribute.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-vc nil
  "*The skeleton template to generate a version control attribute.
The default is to insert nothing.  Example of usage:

    (setq erlang-skel-vc '(& \"-rcs(\\\"$\Id: $ \\\").\") n)

Please see the function `tempo-define-template'.")

(defvar erlang-skel-export
  '(& "-export([" n> "])." n)
  "*The skeleton of an `export' attribute.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-import
  '(& "%%-import(Module, [Function/Arity, ...])." n)
  "*The skeleton of an `import' attribute.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-compile nil
  ;;  '(& "%%-compile(export_all)." n)
  "*The skeleton of a `compile' attribute.
Please see the function `tempo-define-template'.")


;; Comment templates.

(defvar erlang-skel-date-function 'erlang-skel-dd-mmm-yyyy
  "*Function which returns date string.
Look in the module `time-stamp' for a battery of functions.")

(defvar erlang-skel-copyright-comment '()
  "*The template for a copyright line in the header, normally empty.
This variable should be bound to a `tempo' template, for example:
  '(& \"%%% Copyright (C) 2000, Yoyodyne, Inc.\" n)

Please see the function `tempo-define-template'.")

(defvar erlang-skel-created-comment
  '(& "%%% Created : " (funcall erlang-skel-date-function) " by "
      (user-full-name) " <" erlang-skel-mail-address ">" n)
  "*The template for the \"Created:\" comment line.")

(defvar erlang-skel-author-comment
  '(& "%%% Author  : " (user-full-name) " <" erlang-skel-mail-address ">" n)
  "*The template for creating the \"Author:\" line in the header.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-file-comment
  '(& "%%% File    : " (file-name-nondirectory buffer-file-name) n)
"*The template for creating the \"Module:\" line in the header.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-small-header
  '(o (erlang-skel-include erlang-skel-module)
      ;;                           erlang-skel-author)
      n
      (erlang-skel-include erlang-skel-compile
			   ;;			   erlang-skel-export
			   erlang-skel-vc))
  "*The template of a small header without any comments.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-normal-header
  '(o (erlang-skel-include erlang-skel-copyright-comment
			   erlang-skel-file-comment
			   erlang-skel-author-comment)
      "%%% Description : " p n
      (erlang-skel-include erlang-skel-created-comment) n
      (erlang-skel-include erlang-skel-small-header) n)
  "*The template of a normal header.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-large-header
  '(o (erlang-skel-separator)
      (erlang-skel-include erlang-skel-copyright-comment
			   erlang-skel-file-comment
			   erlang-skel-author-comment)
      "%%% Description : " p n
      "%%%" n
      (erlang-skel-include erlang-skel-created-comment)
      (erlang-skel-separator)
      (erlang-skel-include erlang-skel-small-header) )
  "*The template of a large header.
Please see the function `tempo-define-template'.")


;; Server templates.

(defvar erlang-skel-small-server
  '((erlang-skel-include erlang-skel-large-header)
    "-export([start/0,init/1])." n n n
    "start() ->" n> "spawn(" (erlang-get-module-from-file-name)
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

;; Behaviour templates.

(defvar erlang-skel-application
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(application)." n n
    "%% Application callbacks" n
    "-export([start/2, stop/1])." n n
    (erlang-skel-double-separator 2)
    "%% Application callbacks" n
    (erlang-skel-double-separator 2)
    (erlang-skel-separator 2)
    "%% Function: start(Type, StartArgs) -> {ok, Pid} |" n
    "%%                                     {ok, Pid, State} |" n
    "%%                                     {error, Reason}" n
    "%% Description: This function is called whenever an application " n
    "%% is started using application:start/1,2, and should start the processes" n
    "%% of the application. If the application is structured according to the" n
    "%% OTP design principles as a supervision tree, this means starting the" n
    "%% top supervisor of the tree." n
    (erlang-skel-separator 2)
    "start(_Type, StartArgs) ->" n>
    "case 'TopSupervisor':start_link(StartArgs) of" n>
    "{ok, Pid} -> " n>
    "{ok, Pid};" n>
    "Error ->" n>
    "Error" n>
    "end." n
    n
    (erlang-skel-separator 2)
    "%% Function: stop(State) -> void()" n
    "%% Description: This function is called whenever an application" n
    "%% has stopped. It is intended to be the opposite of Module:start/2 and" n
    "%% should do any necessary cleaning up. The return value is ignored. "n
    (erlang-skel-separator 2)
    "stop(_State) ->" n>
    "ok." n
    n
    (erlang-skel-double-separator 2)
    "%% Internal functions" n
    (erlang-skel-double-separator 2)
    )
  "*The template of an application behaviour.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-supervisor
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(supervisor)." n n

    "%% API" n
    "-export([start_link/0])." n n

    "%% Supervisor callbacks" n
    "-export([init/1])." n n

    "-define(SERVER, ?MODULE)." n n

    (erlang-skel-double-separator 2)
    "%% API functions" n
    (erlang-skel-double-separator 2)
    (erlang-skel-separator 2)
    "%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}" n
    "%% Description: Starts the supervisor" n
    (erlang-skel-separator 2)
    "start_link() ->" n>
    "supervisor:start_link({local, ?SERVER}, ?MODULE, [])." n
    n
    (erlang-skel-double-separator 2)
    "%% Supervisor callbacks" n
    (erlang-skel-double-separator 2)
    (erlang-skel-separator 2)
    "%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |" n
    "%%                     ignore                          |" n
    "%%                     {error, Reason}" n
    "%% Description: Whenever a supervisor is started using "n
    "%% supervisor:start_link/[2,3], this function is called by the new process "n
    "%% to find out about restart strategy, maximum restart frequency and child "n
    "%% specifications." n
    (erlang-skel-separator 2)
    "init([]) ->" n>
    "AChild = {'AName',{'AModule',start_link,[]}," n>
    "permanent,2000,worker,['AModule']}," n>
    "{ok,{{one_for_all,0,1}, [AChild]}}." n
    n
    (erlang-skel-double-separator 2)
    "%% Internal functions" n
    (erlang-skel-double-separator 2)
    )
  "*The template of an supervisor behaviour.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-supervisor-bridge
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(supervisor_bridge)." n n

    "%% API" n
    "-export([start_link/0])." n n

    "%% supervisor_bridge callbacks" n
    "-export([init/1, terminate/2])." n n

    "-define(SERVER, ?MODULE)." n n

    "-record(state, {})." n n

    (erlang-skel-double-separator 2)
    "%% API" n
    (erlang-skel-double-separator 2)
    (erlang-skel-separator 2)
    "%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}" n
    "%% Description: Starts the supervisor bridge" n
    (erlang-skel-separator 2)
    "start_link() ->" n>
    "supervisor_bridge:start_link({local, ?SERVER}, ?MODULE, [])." n
    n
    (erlang-skel-double-separator 2)
    "%% supervisor_bridge callbacks" n
    (erlang-skel-double-separator 2)
    (erlang-skel-separator 2)
    "%% Funcion: init(Args) -> {ok,  Pid, State} |" n
    "%%                        ignore            |" n
    "%%                        {error, Reason}    " n
    "%% Description:Creates a supervisor_bridge process, linked to the calling" n
    "%% process, which calls Module:init/1 to start the subsystem. To ensure a" n
    "%% synchronized start-up procedure, this function does not return until" n
    "%% Module:init/1 has returned. "  n
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
    "%% Func: terminate(Reason, State) -> void()" n
    "%% Description:This function is called by the supervisor_bridge when it is"n
    "%% about to terminate. It should be the opposite of Module:init/1 and stop"n
    "%% the subsystem and do any necessary cleaning up.The return value is ignored."
    (erlang-skel-separator 2)
    "terminate(Reason, State) ->" n>
    "'AModule':stop()," n>
    "ok." n
    n
    (erlang-skel-double-separator 2)
    "%% Internal functions" n
    (erlang-skel-double-separator 2)
    )
  "*The template of an supervisor_bridge behaviour.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-generic-server
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(gen_server)." n n

    "%% API" n
    "-export([start_link/0])." n n

    "%% gen_server callbacks" n
    "-export([init/1, handle_call/3, handle_cast/2, "
    "handle_info/2," n>
    "terminate/2, code_change/3])." n n

    "-record(state, {})." n n

    (erlang-skel-double-separator 2)
    "%% API" n
    (erlang-skel-double-separator 2)
    (erlang-skel-separator 2)
    "%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}" n
    "%% Description: Starts the server" n
    (erlang-skel-separator 2)
    "start_link() ->" n>
    "gen_server:start_link({local, ?SERVER}, ?MODULE, [], [])." n
    n
    (erlang-skel-double-separator 2)
    "%% gen_server callbacks" n
    (erlang-skel-double-separator 2)
    n
    (erlang-skel-separator 2)
    "%% Function: init(Args) -> {ok, State} |" n
    "%%                         {ok, State, Timeout} |" n
    "%%                         ignore               |" n
    "%%                         {stop, Reason}" n
    "%% Description: Initializes the server" n
    (erlang-skel-separator 2)
    "init([]) ->" n>
    "{ok, #state{}}." n
    n
    (erlang-skel-separator 2)
    "%% Function: "
    "%% handle_call(Request, From, State) -> {reply, Reply, State} |" n
    "%%                                      {reply, Reply, State, Timeout} |" n
    "%%                                      {noreply, State} |" n
    "%%                                      {noreply, State, Timeout} |" n
    "%%                                      {stop, Reason, Reply, State} |" n
    "%%                                      {stop, Reason, State}" n
    "%% Description: Handling call messages" n
    (erlang-skel-separator 2)
    "handle_call(_Request, _From, State) ->" n>
    "Reply = ok," n>
    "{reply, Reply, State}." n
    n
    (erlang-skel-separator 2)
    "%% Function: handle_cast(Msg, State) -> {noreply, State} |" n
    "%%                                      {noreply, State, Timeout} |" n
    "%%                                      {stop, Reason, State}" n
    "%% Description: Handling cast messages" n

    (erlang-skel-separator 2)
    "handle_cast(_Msg, State) ->" n>
    "{noreply, State}." n
    n
    (erlang-skel-separator 2)
    "%% Function: handle_info(Info, State) -> {noreply, State} |" n
    "%%                                       {noreply, State, Timeout} |" n
    "%%                                       {stop, Reason, State}" n
    "%% Description: Handling all non call/cast messages" n
    (erlang-skel-separator 2)
    "handle_info(_Info, State) ->" n>
    "{noreply, State}." n
    n
    (erlang-skel-separator 2)
    "%% Function: terminate(Reason, State) -> void()" n
    "%% Description: This function is called by a gen_server when it is about to"n
    "%% terminate. It should be the opposite of Module:init/1 and do any necessary"n
    "%% cleaning up. When it returns, the gen_server terminates with Reason." n
    "%% The return value is ignored." n

    (erlang-skel-separator 2)
    "terminate(_Reason, _State) ->" n>
    "ok." n
    n
    (erlang-skel-separator 2)
    "%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}" n
    "%% Description: Convert process state when code is changed" n
    (erlang-skel-separator 2)
    "code_change(_OldVsn, State, _Extra) ->" n>
    "{ok, State}." n
    n
    (erlang-skel-separator 2)
    "%%% Internal functions" n
    (erlang-skel-separator 2)
    )
  "*The template of a generic server.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-gen-event
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(gen_event)." n

    "%% API" n
    "-export([start_link/0, add_handler/0])." n n

    "%% gen_event callbacks" n
    "-export([init/1, handle_event/2, handle_call/2, " n>
    "handle_info/2, terminate/2, code_change/3])." n n

    "-record(state, {})." n n

    (erlang-skel-double-separator 2)
    "%% gen_event callbacks" n
    (erlang-skel-double-separator 2)
    (erlang-skel-separator 2)
    "%% Function: start_link() -> {ok,Pid} | {error,Error} " n
    "%% Description: Creates an event manager." n
    (erlang-skel-separator 2)
    "start_link() ->" n>
    "gen_event:start_link({local, ?SERVER}). " n
    n
    (erlang-skel-separator 2)
    "%% Function: add_handler() -> ok | {'EXIT',Reason} | term()" n
    "%% Description: Adds an event handler" n
    (erlang-skel-separator 2)
    "add_handler() ->" n>
    "gen_event:add_handler(?SERVER, ?MODULE, [])." n
    n
    (erlang-skel-double-separator 2)
    "%% gen_event callbacks" n
    (erlang-skel-double-separator 2)
    (erlang-skel-separator 2)
    "%% Function: init(Args) -> {ok, State}" n
    "%% Description: Whenever a new event handler is added to an event manager,"n
    "%% this function is called to initialize the event handler." n
    (erlang-skel-separator 2)
    "init([]) ->" n>
    "{ok, #state{}}." n
    n
    (erlang-skel-separator 2)
    "%% Function:  "n
    "%% handle_event(Event, State) -> {ok, State} |" n
    "%%                               {swap_handler, Args1, State1, Mod2, Args2} |"n
    "%%                               remove_handler" n
    "%% Description:Whenever an event manager receives an event sent using"n
    "%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for"n
    "%% each installed event handler to handle the event. "n
    (erlang-skel-separator 2)
    "handle_event(_Event, State) ->" n>
    "{ok, State}." n
    n
    (erlang-skel-separator 2)
    "%% Function: " n
    "%% handle_call(Request, State) -> {ok, Reply, State} |" n
    "%%                                {swap_handler, Reply, Args1, State1, "n
    "%%                                  Mod2, Args2} |" n
    "%%                                {remove_handler, Reply}" n
    "%% Description: Whenever an event manager receives a request sent using"n
    "%% gen_event:call/3,4, this function is called for the specified event "n
    "%% handler to handle the request."n
    (erlang-skel-separator 2)
    "handle_call(_Request, State) ->" n>
    "Reply = ok," n>
    "{ok, Reply, State}." n
    n
    (erlang-skel-separator 2)
    "%% Function: " n
    "%% handle_info(Info, State) -> {ok, State} |" n
    "%%                             {swap_handler, Args1, State1, Mod2, Args2} |" n
    "%%                              remove_handler" n
    "%% Description: This function is called for each installed event handler when"n
    "%% an event manager receives any other message than an event or a synchronous"n
    "%% request (or a system message)."n
    (erlang-skel-separator 2)
    "handle_info(_Info, State) ->" n>
    "{ok, State}." n
    n
    (erlang-skel-separator 2)
    "%% Function: terminate(Reason, State) -> void()" n
    "%% Description:Whenever an event handler is deleted from an event manager,"n
    "%% this function is called. It should be the opposite of Module:init/1 and "n
    "%% do any necessary cleaning up. " n
    (erlang-skel-separator 2)
    "terminate(_Reason, _State) ->" n>
    "ok." n
    n
    (erlang-skel-separator 2)
    "%% Function: code_change(OldVsn, State, Extra) -> {ok, NewState} " n
    "%% Description: Convert process state when code is changed" n
    (erlang-skel-separator 2)
    "code_change(_OldVsn, State, _Extra) ->" n>
    "{ok, State}." n
    n
    (erlang-skel-separator 2)
    "%%% Internal functions" n
    (erlang-skel-separator 2)
    )
  "*The template of a gen_event.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-gen-fsm
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(gen_fsm)." n n

    "%% API" n
    "-export([start_link/0])." n n

    "%% gen_fsm callbacks" n
    "-export([init/1, state_name/2, state_name/3, handle_event/3," n>
    "handle_sync_event/4, handle_info/3, terminate/3, code_change/4])." n n

    "-record(state, {})." n n

    (erlang-skel-double-separator 2)
    "%% API" n
    (erlang-skel-double-separator 2)
    (erlang-skel-separator 2)
    "%% Function: start_link() -> ok,Pid} | ignore | {error,Error}" n
    "%% Description:Creates a gen_fsm process which calls Module:init/1 to"n
    "%% initialize. To ensure a synchronized start-up procedure, this function" n
    "%% does not return until Module:init/1 has returned.  " n
    (erlang-skel-separator 2)
    "start_link() ->" n>
    "gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], [])." n
    n
    (erlang-skel-double-separator 2)
    "%% gen_fsm callbacks" n
    (erlang-skel-double-separator 2)
    (erlang-skel-separator 2)
    "%% Function: init(Args) -> {ok, StateName, State} |" n
    "%%                         {ok, StateName, State, Timeout} |" n
    "%%                         ignore                              |" n
    "%%                         {stop, StopReason}                   " n
    "%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or"n
    "%% gen_fsm:start_link/3,4, this function is called by the new process to "n
    "%% initialize. " n
    (erlang-skel-separator 2)
    "init([]) ->" n>
    "{ok, state_name, #state{}}." n
    n
    (erlang-skel-separator 2)
    "%% Function: "n
    "%% state_name(Event, State) -> {next_state, NextStateName, NextState}|" n
    "%%                             {next_state, NextStateName, " n
    "%%                                NextState, Timeout} |" n
    "%%                             {stop, Reason, NewState}" n
    "%% Description:There should be one instance of this function for each possible"n
    "%% state name. Whenever a gen_fsm receives an event sent using" n
    "%% gen_fsm:send_event/2, the instance of this function with the same name as"n
    "%% the current state name StateName is called to handle the event. It is also "n
    "%% called if a timeout occurs. " n
    (erlang-skel-separator 2)
    "state_name(_Event, State) ->" n>
    "{next_state, state_name, State}." n
    n
    (erlang-skel-separator 2)
    "%% Function:" n
    "%% state_name(Event, From, State) -> {next_state, NextStateName, NextState} |"n
    "%%                                   {next_state, NextStateName, " n
    "%%                                     NextState, Timeout} |" n
    "%%                                   {reply, Reply, NextStateName, NextState}|"n
    "%%                                   {reply, Reply, NextStateName, " n
    "%%                                    NextState, Timeout} |" n
    "%%                                   {stop, Reason, NewState}|" n
    "%%                                   {stop, Reason, Reply, NewState}" n
    "%% Description: There should be one instance of this function for each" n
    "%% possible state name. Whenever a gen_fsm receives an event sent using" n
    "%% gen_fsm:sync_send_event/2,3, the instance of this function with the same"n
    "%% name as the current state name StateName is called to handle the event." n
    (erlang-skel-separator 2)
    "state_name(_Event, _From, State) ->" n>
    "Reply = ok," n>
    "{reply, Reply, state_name, State}." n
    n
    (erlang-skel-separator 2)
    "%% Function: " n
    "%% handle_event(Event, StateName, State) -> {next_state, NextStateName, "n
    "%%						  NextState} |" n
    "%%                                          {next_state, NextStateName, "n
    "%%					          NextState, Timeout} |" n
    "%%                                          {stop, Reason, NewState}" n
    "%% Description: Whenever a gen_fsm receives an event sent using"n
    "%% gen_fsm:send_all_state_event/2, this function is called to handle"n
    "%% the event." n
    (erlang-skel-separator 2)
    "handle_event(_Event, StateName, State) ->" n>
    "{next_state, StateName, State}." n
    n
    (erlang-skel-separator 2)
    "%% Function: " n
    "%% handle_sync_event(Event, From, StateName, "n
    "%%                   State) -> {next_state, NextStateName, NextState} |" n
    "%%                             {next_state, NextStateName, NextState, " n
    "%%                              Timeout} |" n
    "%%                             {reply, Reply, NextStateName, NextState}|" n
    "%%                             {reply, Reply, NextStateName, NextState, " n
    "%%                              Timeout} |" n
    "%%                             {stop, Reason, NewState} |" n
    "%%                             {stop, Reason, Reply, NewState}" n
    "%% Description: Whenever a gen_fsm receives an event sent using"n
    "%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle"n
    "%% the event."n
    (erlang-skel-separator 2)
    "handle_sync_event(Event, From, StateName, State) ->" n>
    "Reply = ok," n>
    "{reply, Reply, StateName, State}." n
    n
    (erlang-skel-separator 2)
    "%% Function: " n
    "%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|" n
    "%%                                     {next_state, NextStateName, NextState, "n
    "%%                                       Timeout} |" n
    "%%                                     {stop, Reason, NewState}" n
    "%% Description: This function is called by a gen_fsm when it receives any"n
    "%% other message than a synchronous or asynchronous event"n
    "%% (or a system message)." n
    (erlang-skel-separator 2)
    "handle_info(_Info, StateName, State) ->" n>
    "{next_state, StateName, State}." n
    n
    (erlang-skel-separator 2)
    "%% Function: terminate(Reason, StateName, State) -> void()" n
    "%% Description:This function is called by a gen_fsm when it is about"n
    "%% to terminate. It should be the opposite of Module:init/1 and do any"n
    "%% necessary cleaning up. When it returns, the gen_fsm terminates with"n
    "%% Reason. The return value is ignored." n
    (erlang-skel-separator 2)
    "terminate(_Reason, _StateName, _State) ->" n>
    "ok." n
    n
    (erlang-skel-separator 2)
    "%% Function:" n
    "%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}" n
    "%% Description: Convert process state when code is changed" n
    (erlang-skel-separator 2)
    "code_change(_OldVsn, StateName, State, _Extra) ->" n>
    "{ok, StateName, State}." n
    n
    (erlang-skel-separator 2)
    "%%% Internal functions" n
    (erlang-skel-separator 2)
    )
  "*The template of a gen_fsm.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-lib
  '((erlang-skel-include erlang-skel-large-header)

    "%% API" n
    "-export([])." n n

    (erlang-skel-double-separator 2)
    "%% API" n
    (erlang-skel-double-separator 2)
    (erlang-skel-separator 2)
    "%% Function: " n
    "%% Description:" n
    (erlang-skel-separator 2)
    n
    (erlang-skel-double-separator 2)
    "%% Internal functions" n
    (erlang-skel-double-separator 2)
    )
  "*The template of a library module.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-corba-callback
  '((erlang-skel-include erlang-skel-large-header)
    "%% Include files" n n

    "%% API" n
    "-export([])." n n

    "%% Corba callbacks" n
    "-export([init/1, terminate/2, code_change/3])." n n

    "-record(state, {})." n n

    (erlang-skel-double-separator 2)
    "%% Corba callbacks" n
    (erlang-skel-double-separator 2)
    (erlang-skel-separator 2)
    "%% Function: init(Args) -> {ok, State} |" n
    "%%                         {ok, State, Timeout} |" n
    "%%                         ignore               |" n
    "%%                         {stop, Reason}" n
    "%% Description: Initializes the server" n
    (erlang-skel-separator 2)
    "init([]) ->" n>
    "{ok, #state{}}." n
    n
    (erlang-skel-separator 2)
    "%% Function: terminate(Reason, State) -> void()" n
    "%% Description: Shutdown the server" n
    (erlang-skel-separator 2)
    "terminate(_Reason, _State) ->" n>
    "ok." n
    n
    (erlang-skel-separator 2)
    "%% Function: code_change(OldVsn, State, Extra) -> {ok, NewState} " n
    "%% Description: Convert process state when code is changed" n
    (erlang-skel-separator 2)
    "code_change(_OldVsn, State, _Extra) ->" n>
    "{ok, State}." n
    n
    (erlang-skel-double-separator 2)
    "%% Internal functions" n
    (erlang-skel-double-separator 2)
    )
  "*The template of a library module.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-ts-test-suite
 '((erlang-skel-include erlang-skel-large-header)
   "%% Note: This directive should only be used in test suites." n
    "-compile(export_all)." n n

    "-include_lib(\"test_server/include/test_server.hrl\")." n n

    (erlang-skel-separator 2)
    "%% TEST SERVER CALLBACK FUNCTIONS" n
    (erlang-skel-separator 2)
    n
    (erlang-skel-separator 2)
    "%% Function: init_per_suite(Config0) -> Config1 | {skip,Reason}" n
    "%%" n
    "%% Config0 = Config1 = [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%% Reason = term()" n
    "%%   The reason for skipping the suite." n
    "%%" n
    "%% Description: Initialization before the suite." n
    "%%" n
    "%% Note: This function is free to add any key/value pairs to the Config" n
    "%% variable, but should NOT alter/remove any existing entries." n
    (erlang-skel-separator 2)
    "init_per_suite(Config) ->" n >
    "Config." n n

    (erlang-skel-separator 2)
    "%% Function: end_per_suite(Config) -> void()" n
    "%%" n
    "%% Config = [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%%" n
    "%% Description: Cleanup after the suite." n
    (erlang-skel-separator 2)
    "end_per_suite(_Config) ->" n >
    "ok." n n

    (erlang-skel-separator 2)
    "%% Function: init_per_testcase(TestCase, Config0) -> Config1 |" n
    "%%                                                   {skip,Reason}" n
    "%% TestCase = atom()" n
    "%%   Name of the test case that is about to run." n
    "%% Config0 = Config1 = [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%% Reason = term()" n
    "%%   The reason for skipping the test case." n
    "%%" n
    "%% Description: Initialization before each test case." n
    "%%" n
    "%% Note: This function is free to add any key/value pairs to the Config" n
    "%% variable, but should NOT alter/remove any existing entries." n
    (erlang-skel-separator 2)
    "init_per_testcase(_TestCase, Config) ->" n >
    "Config." n n

    (erlang-skel-separator 2)
    "%% Function: end_per_testcase(TestCase, Config) -> void()" n
    "%%" n
    "%% TestCase = atom()" n
    "%%   Name of the test case that is finished." n
    "%% Config = [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%%" n
    "%% Description: Cleanup after each test case." n
    (erlang-skel-separator 2)
    "end_per_testcase(_TestCase, _Config) ->" n >
    "ok."n n

    (erlang-skel-separator 2)
    "%% Function: all(Clause) -> Descr | Spec | {skip,Reason}" n
    "%%" n
    "%% Clause = doc | suite" n
    "%%   Indicates expected return value." n
    "%% Descr = [string()] | []" n
    "%%   String that describes the test suite." n
    "%% Spec = [TestCase]" n
    "%%   A test specification." n
    "%% TestCase = ConfCase | atom()" n
    "%%   Configuration case, or the name of a test case function." n
    "%% ConfCase = {conf,Init,Spec,End} |" n
    "%%            {conf,Properties,Init,Spec,End}" n
    "%% Init = End = {Mod,Func} | Func" n
    "%%   Initialization and cleanup function." n
    "%% Mod = Func = atom()" n
    "%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]" n
    "%%   Execution properties of the test cases (may be combined)." n
    "%% Shuffle = shuffle | {shuffle,Seed}" n
    "%%   To get cases executed in random order." n
    "%% Seed = {integer(),integer(),integer()}" n
    "%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |" n
    "%%              repeat_until_any_ok | repeat_until_any_fail" n
    "%%   To get execution of cases repeated." n
    "%% N = integer() | forever" n
    "%% Reason = term()" n
    "%%   The reason for skipping the test suite." n
    "%%" n
    "%% Description: Returns a description of the test suite when" n
    "%%              Clause == doc, and a test specification (list" n
    "%%              of the conf and test cases in the suite) when" n
    "%%              Clause == suite." n
    (erlang-skel-separator 2)
    "all(doc) -> " n >
    "[\"Describe the main purpose of this suite\"];" n n
    "all(suite) -> " n >
    "[a_test_case]." n n
    n
    (erlang-skel-separator 2)
    "%% TEST CASES" n
    (erlang-skel-separator 2)
    n
    (erlang-skel-separator 2)
    "%% Function: TestCase(Arg) -> Descr | Spec | ok | exit() | {skip,Reason}" n
    "%%" n
    "%% Arg = doc | suite | Config" n
    "%%   Indicates expected behaviour and return value." n
    "%% Config = [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%% Descr = [string()] | []" n
    "%%   String that describes the test case." n
    "%% Spec = [tuple()] | []" n
    "%%   A test specification, see all/1." n
    "%% Reason = term()" n
    "%%   The reason for skipping the test case." n
    "%%" n
    "%% Description: Test case function. Returns a description of the test" n
    "%%              case (doc), then returns a test specification (suite)," n
    "%%              or performs the actual test (Config)." n
    (erlang-skel-separator 2)
    "a_test_case(doc) -> " n >
    "[\"Describe the main purpose of this test case\"];" n n
    "a_test_case(suite) -> " n >
    "[];" n n
    "a_test_case(Config) when is_list(Config) -> " n >
    "ok." n
   )
 "*The template of a library module.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-ct-test-suite-l
 '((erlang-skel-include erlang-skel-large-header)
   "%% Note: This directive should only be used in test suites." n
    "-compile(export_all)." n n

    "-include_lib(\"common_test/include/ct.hrl\")." n n

    (erlang-skel-separator 2)
    "%% COMMON TEST CALLBACK FUNCTIONS" n
    (erlang-skel-separator 2)
    n
    (erlang-skel-separator 2)
    "%% Function: suite() -> Info" n
    "%%" n
    "%% Info = [tuple()]" n
    "%%   List of key/value pairs." n
    "%%" n
    "%% Description: Returns list of tuples to set default properties" n
    "%%              for the suite." n
    "%%" n
    "%% Note: The suite/0 function is only meant to be used to return" n
    "%% default data values, not perform any other operations." n
    (erlang-skel-separator 2)
    "suite() ->" n >
    "[{timetrap,{minutes,10}}]." n n

    (erlang-skel-separator 2)
    "%% Function: init_per_suite(Config0) ->" n
    "%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}" n
    "%%" n
    "%% Config0 = Config1 = [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%% Reason = term()" n
    "%%   The reason for skipping the suite." n
    "%%" n
    "%% Description: Initialization before the suite." n
    "%%" n
    "%% Note: This function is free to add any key/value pairs to the Config" n
    "%% variable, but should NOT alter/remove any existing entries." n
    (erlang-skel-separator 2)
    "init_per_suite(Config) ->" n >
    "Config." n n

    (erlang-skel-separator 2)
    "%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}" n
    "%%" n
    "%% Config0 = Config1 = [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%%" n
    "%% Description: Cleanup after the suite." n
    (erlang-skel-separator 2)
    "end_per_suite(_Config) ->" n >
    "ok." n n

    (erlang-skel-separator 2)
    "%% Function: init_per_group(GroupName, Config0) ->" n
    "%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}" n
    "%%" n
    "%% GroupName = atom()" n
    "%%   Name of the test case group that is about to run." n
    "%% Config0 = Config1 = [tuple()]" n
    "%%   A list of key/value pairs, holding configuration data for the group." n
    "%% Reason = term()" n
    "%%   The reason for skipping all test cases and subgroups in the group." n
    "%%" n
    "%% Description: Initialization before each test case group." n
    (erlang-skel-separator 2)
    "init_per_group(_GroupName, Config) ->" n >
    "Config." n n

    (erlang-skel-separator 2)
    "%% Function: end_per_group(GroupName, Config0) ->" n
    "%%               void() | {save_config,Config1}" n
    "%%" n
    "%% GroupName = atom()" n
    "%%   Name of the test case group that is finished." n
    "%% Config0 = Config1 = [tuple()]" n
    "%%   A list of key/value pairs, holding configuration data for the group." n
    "%%" n
    "%% Description: Cleanup after each test case group." n
    (erlang-skel-separator 2)
    "end_per_group(_GroupName, _Config) ->" n >
    "ok." n n

    (erlang-skel-separator 2)
    "%% Function: init_per_testcase(TestCase, Config0) ->" n
    "%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}" n
    "%%" n
    "%% TestCase = atom()" n
    "%%   Name of the test case that is about to run." n
    "%% Config0 = Config1 = [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%% Reason = term()" n
    "%%   The reason for skipping the test case." n
    "%%" n
    "%% Description: Initialization before each test case." n
    "%%" n
    "%% Note: This function is free to add any key/value pairs to the Config" n
    "%% variable, but should NOT alter/remove any existing entries." n
    (erlang-skel-separator 2)
    "init_per_testcase(_TestCase, Config) ->" n >
    "Config." n n

    (erlang-skel-separator 2)
    "%% Function: end_per_testcase(TestCase, Config0) ->" n
    "%%               void() | {save_config,Config1} | {fail,Reason}" n
    "%%" n
    "%% TestCase = atom()" n
    "%%   Name of the test case that is finished." n
    "%% Config0 = Config1 = [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%% Reason = term()" n
    "%%   The reason for failing the test case." n
    "%%" n
    "%% Description: Cleanup after each test case." n
    (erlang-skel-separator 2)
    "end_per_testcase(_TestCase, _Config) ->" n >
    "ok." n n

    (erlang-skel-separator 2)
    "%% Function: groups() -> [Group]" n
    "%%" n
    "%% Group = {GroupName,Properties,GroupsAndTestCases}" n
    "%% GroupName = atom()" n
    "%%   The name of the group." n
    "%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]" n
    "%%   Group properties that may be combined." n
    "%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]" n
    "%% TestCase = atom()" n
    "%%   The name of a test case." n
    "%% Shuffle = shuffle | {shuffle,Seed}" n
    "%%   To get cases executed in random order." n
    "%% Seed = {integer(),integer(),integer()}" n
    "%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |" n
    "%%              repeat_until_any_ok | repeat_until_any_fail" n
    "%%   To get execution of cases repeated." n
    "%% N = integer() | forever" n
    "%%" n
    "%% Description: Returns a list of test case group definitions." n
    (erlang-skel-separator 2)
    "groups() ->" n >
    "[]." n n

    (erlang-skel-separator 2)
    "%% Function: all() -> GroupsAndTestCases | {skip,Reason}" n
    "%%" n
    "%% GroupsAndTestCases = [{group,GroupName} | TestCase]" n
    "%% GroupName = atom()" n
    "%%   Name of a test case group." n
    "%% TestCase = atom()" n
    "%%   Name of a test case." n
    "%% Reason = term()" n
    "%%   The reason for skipping all groups and test cases." n
    "%%" n
    "%% Description: Returns the list of groups and test cases that" n
    "%%              are to be executed." n
    (erlang-skel-separator 2)
    "all() -> " n >
    "[my_test_case]." n n

    n
    (erlang-skel-separator 2)
    "%% TEST CASES" n
    (erlang-skel-separator 2)
    n

    (erlang-skel-separator 2)
    "%% Function: TestCase() -> Info" n
    "%%" n
    "%% Info = [tuple()]" n
    "%%   List of key/value pairs." n
    "%%" n
    "%% Description: Test case info function - returns list of tuples to set" n
    "%%              properties for the test case." n
    "%%" n
    "%% Note: This function is only meant to be used to return a list of" n
    "%% values, not perform any other operations." n
    (erlang-skel-separator 2)
    "my_test_case() -> " n >
    "[]." n n

    (erlang-skel-separator 2)
    "%% Function: TestCase(Config0) ->" n
    "%%               ok | exit() | {skip,Reason} | {comment,Comment} |" n
    "%%               {save_config,Config1} | {skip_and_save,Reason,Config1}" n
    "%%" n
    "%% Config0 = Config1 = [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%% Reason = term()" n
    "%%   The reason for skipping the test case." n
    "%% Comment = term()" n
    "%%   A comment about the test case that will be printed in the html log." n
    "%%" n
    "%% Description: Test case function. (The name of it must be specified in" n
    "%%              the all/0 list or in a test case group for the test case" n
    "%%              to be executed)." n
    (erlang-skel-separator 2)
    "my_test_case(_Config) -> " n >
    "ok." n
    )
 "*The template of a library module.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-ct-test-suite-s
 '((erlang-skel-include erlang-skel-large-header)
    "-compile(export_all)." n n

    "-include_lib(\"common_test/include/ct.hrl\")." n n

    (erlang-skel-separator 2)
    "%% Function: suite() -> Info" n
    "%% Info = [tuple()]" n
    (erlang-skel-separator 2)
    "suite() ->" n >
    "[{timetrap,{seconds,30}}]." n n

    (erlang-skel-separator 2)
    "%% Function: init_per_suite(Config0) ->" n
    "%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}" n
    "%% Config0 = Config1 = [tuple()]" n
    "%% Reason = term()" n
    (erlang-skel-separator 2)
    "init_per_suite(Config) ->" n >
    "Config." n n

    (erlang-skel-separator 2)
    "%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}" n
    "%% Config0 = Config1 = [tuple()]" n
    (erlang-skel-separator 2)
    "end_per_suite(_Config) ->" n >
    "ok." n n

    (erlang-skel-separator 2)
    "%% Function: init_per_group(GroupName, Config0) ->" n
    "%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}" n
    "%% GroupName = atom()" n
    "%% Config0 = Config1 = [tuple()]" n
    "%% Reason = term()" n
    (erlang-skel-separator 2)
    "init_per_group(_GroupName, Config) ->" n >
    "Config." n n

    (erlang-skel-separator 2)
    "%% Function: end_per_group(GroupName, Config0) ->" n
    "%%               void() | {save_config,Config1}" n
    "%% GroupName = atom()" n
    "%% Config0 = Config1 = [tuple()]" n
    (erlang-skel-separator 2)
    "end_per_group(_GroupName, _Config) ->" n >
    "ok." n n

    (erlang-skel-separator 2)
    "%% Function: init_per_testcase(TestCase, Config0) ->" n
    "%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}" n
    "%% TestCase = atom()" n
    "%% Config0 = Config1 = [tuple()]" n
    "%% Reason = term()" n
    (erlang-skel-separator 2)
    "init_per_testcase(_TestCase, Config) ->" n >
    "Config." n n

    (erlang-skel-separator 2)
    "%% Function: end_per_testcase(TestCase, Config0) ->" n
    "%%               void() | {save_config,Config1} | {fail,Reason}" n
    "%% TestCase = atom()" n
    "%% Config0 = Config1 = [tuple()]" n
    "%% Reason = term()" n
    (erlang-skel-separator 2)
    "end_per_testcase(_TestCase, _Config) ->" n >
    "ok." n n

    (erlang-skel-separator 2)
    "%% Function: groups() -> [Group]" n
    "%% Group = {GroupName,Properties,GroupsAndTestCases}" n
    "%% GroupName = atom()" n
    "%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]" n
    "%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]" n
    "%% TestCase = atom()" n
    "%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}" n
    "%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |" n
    "%%              repeat_until_any_ok | repeat_until_any_fail" n
    "%% N = integer() | forever" n
    (erlang-skel-separator 2)
    "groups() ->" n >
    "[]." n n

    (erlang-skel-separator 2)
    "%% Function: all() -> GroupsAndTestCases | {skip,Reason}" n
    "%% GroupsAndTestCases = [{group,GroupName} | TestCase]" n
    "%% GroupName = atom()" n
    "%% TestCase = atom()" n
    "%% Reason = term()" n
    (erlang-skel-separator 2)
    "all() -> " n >
    "[my_test_case]." n n

    (erlang-skel-separator 2)
    "%% Function: TestCase() -> Info" n
    "%% Info = [tuple()]" n
    (erlang-skel-separator 2)
    "my_test_case() -> " n >
    "[]." n n

    (erlang-skel-separator 2)
    "%% Function: TestCase(Config0) ->" n
    "%%               ok | exit() | {skip,Reason} | {comment,Comment} |" n
    "%%               {save_config,Config1} | {skip_and_save,Reason,Config1}" n
    "%% Config0 = Config1 = [tuple()]" n
    "%% Reason = term()" n
    "%% Comment = term()" n
    (erlang-skel-separator 2)
    "my_test_case(_Config) -> " n >
    "ok." n
    )
 "*The template of a library module.
Please see the function `tempo-define-template'.")
