%%%-------------------------------------------------------------------
%%% @doc Utils functions for the reversible semantics for Erlang
%%% @end
%%%-------------------------------------------------------------------

-module(utils).
-export([fundef_lookup/2, fundef_rename/1,
         temp_variable/0, fresh_time/0, pid_exists/2,
         select_proc/2, select_msg/2,
         select_proc_with_time/2, select_proc_with_send/2,
         select_proc_with_spawn/2, select_proc_with_rec/2,
         select_proc_with_var/2,
         merge_env/2,
         replace_variable/3, replace_all/2,
         funNames/1,
         stringToNameAndArity/1, stringToArgs/1,
         filter_options/2, filter_procs_opts/1,
         has_fwd/1, has_bwd/1, has_norm/1, has_var/2,
         is_queue_minus_msg/3, topmost_rec/1, last_msg_rest/1,
         gen_log_send/4, gen_log_spawn/2, empty_log/1, must_focus_log/1,
         extract_replay_data/1, extract_pid_log_data/2, get_mod_name/1, fresh_pid/0]).

-include("cauder.hrl").


%%--------------------------------------------------------------------
%% @doc Searches a function definition in FunDefs with the specified Name and Arity
%% @end
%%--------------------------------------------------------------------

-spec fundef_lookup({atom(), arity()}, [cauder_types:af_function_decl()]) -> {value, cauder_types:af_function_decl()} | false.

fundef_lookup({Name, Arity}, FunDefs) ->
  lists:search(fun({'function', _, N, A, _}) -> Name == N andalso Arity == A end, FunDefs).


%%--------------------------------------------------------------------
%% @doc Renames all the variables in function definition FunDef
%% @end
%%--------------------------------------------------------------------
-spec fundef_rename(FunDef) -> NewFunDef when
  FunDef :: erl_parse:abstract_form(), % erl_parse:af_function_decl()
  NewFunDef :: erl_parse:abstract_form(). % erl_parse:af_function_decl()

fundef_rename(FunDef) ->
  {RenamedFunDef, _} = erl_syntax_lib:mapfold(
    fun(Node, RenameMap) ->
      case Node of
        % Underscore variable doesn't need to be renamed
        {var, _, '_'} ->
          {Node, RenameMap};
        % Normal variable is renamed
        {var, Line, Name} ->
          case dict:find(Name, RenameMap) of
            % Has already been renamed, reuse name
            {ok, NewName} ->
              NewNode = {var, Line, NewName},
              {NewNode, RenameMap};
            % Has not been renamed yet, get new name
            error ->
              NewName = fresh_variable_name(Name),
              NewRenameMap = dict:store(Name, NewName, RenameMap),
              NewNode = {var, Line, NewName},
              {NewNode, NewRenameMap}
          end;
        % Other types of nodes are skipped
        _ -> {Node, RenameMap}
      end
    end,
    dict:new(),
    FunDef
  ),
  erl_syntax:revert(RenamedFunDef).


%% =====================================================================
%% @doc Creates a variable with the name 'k_<num>' being <num> a unique
%% positive number.
%%
%% This variable is intended to be used as a temporal variable in calls
%% where a resulting value is not known immediately, so when the value
%% is calculated it can replace this variable.
%%
%% @see replace_variable/3

-spec temp_variable() -> {'var', erl_anno:anno(), atom()}.

temp_variable() ->
  Number = fresh_variable_number(),
  Name = "k_" ++ integer_to_list(Number),
  erl_syntax:revert(erl_syntax:variable(Name)).


-spec fresh_time() -> non_neg_integer().

fresh_time() ->
  Time = ref_lookup(?FRESH_TIME),
  ref_add(?FRESH_TIME, Time + 1),
  Time.


-spec pid_exists([cauder_types:process()], cauder_types:af_integer()) -> boolean().

pid_exists([#proc{pid = Pid} | _], Pid) -> true;
pid_exists([_ | Procs], Pid)            -> pid_exists(Procs, Pid);
pid_exists([], _)                       -> false.


%%--------------------------------------------------------------------
%% @doc Returns a tuple with a process with pid Pid from Procs and
%% the rest of processes from Procs
%% @end
%%--------------------------------------------------------------------

-spec select_proc([cauder_types:process()], cauder_types:af_integer()) -> {cauder_types:process(), [cauder_types:process()]}.

select_proc(Procs, Pid) ->
  {[Proc], RestProcs} = lists:partition(fun(P) -> P#proc.pid == Pid end, Procs),
  {Proc, RestProcs}.


%%--------------------------------------------------------------------
%% @doc Returns a tuple with a message with id Time from Msgs and
%% the rest of messages from Msgs
%% @end
%%--------------------------------------------------------------------

-spec select_msg([cauder_types:message()], non_neg_integer()) -> {cauder_types:message(), [cauder_types:message()]}.

select_msg(Messages, Time) ->
  {[Message | []], RestMessages} = lists:partition(fun(M) -> M#msg.time == Time end, Messages),
  {Message, RestMessages}.


%%--------------------------------------------------------------------
%% @doc Returns the process that contains a message with id Time
%% from Procs
%% @end
%%--------------------------------------------------------------------

-spec select_proc_with_time([cauder_types:process()], non_neg_integer()) -> {value, cauder_types:process()} | false.

select_proc_with_time(Procs, Time) ->
  CheckTime = fun({_, MsgTime}) -> MsgTime == Time end,
  CheckProc = fun(#proc{mail = Mail}) -> lists:any(CheckTime, Mail) end,
  lists:search(CheckProc, Procs).

%%--------------------------------------------------------------------
%% @doc Returns the processes that contain a send item in history
%% with time Time
%% @end
%%--------------------------------------------------------------------

-spec select_proc_with_send([cauder_types:process()], non_neg_integer()) -> [cauder_types:process()].

select_proc_with_send(Procs, Time) -> lists:filter(fun(#proc{hist = Hist}) -> has_send(Hist, Time) end, Procs).

%%--------------------------------------------------------------------
%% @doc Returns the processes that contain a spawn item in history
%% with pid Pid
%% @end
%%--------------------------------------------------------------------

-spec select_proc_with_spawn([cauder_types:process()], cauder_types:af_integer()) -> [cauder_types:process()].

select_proc_with_spawn(Procs, Pid) -> lists:filter(fun(#proc{hist = Hist}) -> has_spawn(Hist, Pid) end, Procs).

%%--------------------------------------------------------------------
%% @doc Returns the processes that contain a spawn item in history
%% with pid Pid
%% @end
%%--------------------------------------------------------------------

-spec select_proc_with_rec([cauder_types:process()], non_neg_integer()) -> [cauder_types:process()].

select_proc_with_rec(Procs, Time) -> lists:filter(fun(#proc{hist = Hist}) -> has_rec(Hist, Time) end, Procs).

%%--------------------------------------------------------------------
%% @doc Returns the processes that contain a binding for Var in
%% its environment Env
%% @end
%%--------------------------------------------------------------------

-spec select_proc_with_var([cauder_types:process()], cauder_types:binding()) -> [cauder_types:process()].

select_proc_with_var(Procs, Var) -> lists:filter(fun(#proc{env = Env}) -> has_var(Env, Var) end, Procs).


%%--------------------------------------------------------------------
%% @doc Update the environment Env with multiple bindings
%% @end
%%--------------------------------------------------------------------

-spec merge_env(cauder_types:environment(), cauder_types:environment()) -> cauder_types:environment().

merge_env(Env, []) -> Env;
merge_env(Env, [{Name, Value} | RestBindings]) ->
  NewEnv = erl_eval:add_binding(Name, Value, Env),
  merge_env(NewEnv, RestBindings).


%%--------------------------------------------------------------------
%% @doc A typical substitution application
%% @end
%%--------------------------------------------------------------------

replace_all([], Exp) -> Exp;
replace_all([{Var, Val} | R], Exp) ->
  %io:format("replace: ~p~n~p~n~p~n",[Var,Val,Exp]),
  NewExp = utils:replace_variable(Var, Val, Exp),
  %io:format("--result: p~n",[NewExp]),
  replace_all(R, NewExp).


%% =====================================================================
%% @doc Replaces all occurrences of the variable Target in each one of
%% the Expressions with the literal value Replacement.

-spec replace_variable(Target, Replacement, Expressions) -> Result when
  Target :: {'var', erl_anno:anno(), atom()},
  Replacement :: erl_parse:abstract_expr() | [erl_parse:abstract_expr()], % erl_parse:af_literal()
  Expressions :: [erl_parse:abstract_expr()],
  Result :: [erl_parse:abstract_expr()].

% TODO Is necessary to accept a list of expression instead on a single expression?
replace_variable(Target, Replacement, Expressions) when is_list(Expressions) ->
  lists:map(
    fun(Expr) ->
      erl_syntax:revert(
        erl_syntax_lib:map(
          fun(Node) ->
            case Node of
              Target -> Replacement;
              Other -> Other
            end
          end,
          Expr
        )
      )
    end,
    Expressions
  ).


%%--------------------------------------------------------------------
%% @doc Returns a list with the names of the functions defined in the given FunForms
%% @end
%%--------------------------------------------------------------------
-spec funNames(FunForms) -> FunNames when
  FunForms :: [erl_parse:abstract_form()], % [erl_parse:af_function_decl()]
  FunNames :: [string()].

funNames(FunForms) ->
  [atom_to_list(Name) ++ "/" ++ integer_to_list(Arity) || {function, _, Name, Arity, _} <- FunForms].

%%--------------------------------------------------------------------
%% @doc Converts a String into tuple with the Name and Arity of a function
%% @end
%%--------------------------------------------------------------------
-spec stringToNameAndArity(String) -> {Name, Arity} when
  String :: string(),
  Name :: atom(),
  Arity :: arity().

stringToNameAndArity(String) ->
  FunParts = string:tokens(String, "/"),
  Name = list_to_atom(lists:nth(1, FunParts)),
  Arity = list_to_integer(lists:nth(2, FunParts)),
  {Name, Arity}.

%%--------------------------------------------------------------------
%% @doc Parses a string Str that represents a list of arguments
%% and transforms these arguments to their equivalent in Abstract Syntax
%% @end
%%--------------------------------------------------------------------
-spec stringToArgs(String) -> Args when
  String :: string(),
  Args :: [erl_parse:abstract_expr()].

stringToArgs([]) -> [];
stringToArgs(Str) ->
  {ok, Tokens, _} = erl_scan:string(Str ++ "."),
  {ok, Args} = erl_parse:parse_exprs(Tokens),
  Args.


%%--------------------------------------------------------------------
%% @doc Filters the options with identifier Id
%% @end
%%--------------------------------------------------------------------
filter_options([], _) -> [];
filter_options([CurOpt | RestOpts], Id) ->
  #opt{id = OptId} = CurOpt,
  case (OptId == Id) of
    true -> [CurOpt | filter_options(RestOpts, Id)];
    false -> filter_options(RestOpts, Id)
  end.

%%--------------------------------------------------------------------
%% @doc Filters the process options from a list of Options
%% @end
%%--------------------------------------------------------------------
filter_procs_opts([]) -> [];
filter_procs_opts([CurOpt | RestOpts]) ->
  #opt{type = Type} = CurOpt,
  case Type of
    ?TYPE_MSG -> filter_procs_opts(RestOpts);
    ?TYPE_PROC -> [CurOpt | filter_procs_opts(RestOpts)]
  end.

%%--------------------------------------------------------------------
%% @doc Returns true if a list of Options has a forward option,
%% and false otherwise
%% @end
%%--------------------------------------------------------------------
has_fwd([])                                 -> false;
has_fwd([#opt{sem = ?FWD_SEM} | _RestOpts]) -> true;
has_fwd([_CurOpt | RestOpts])               -> has_fwd(RestOpts).

%%--------------------------------------------------------------------
%% @doc Returns true if a list of Options has a backward option,
%% and false otherwise
%% @end
%%--------------------------------------------------------------------
has_bwd([])                                 -> false;
has_bwd([#opt{sem = ?BWD_SEM} | _RestOpts]) -> true;
has_bwd([_CurOpt | RestOpts])               -> has_bwd(RestOpts).

%%--------------------------------------------------------------------
%% @doc Returns true if a list of Options has a normalizing option,
%% and false otherwise
%% @end
%%--------------------------------------------------------------------
has_norm([])                                                    -> false;
has_norm([#opt{sem = ?FWD_SEM, rule = ?RULE_SCHED} | RestOpts]) -> has_norm(RestOpts);
has_norm([#opt{sem = ?FWD_SEM} | _RestOpts])                    -> true;
has_norm([_CurOpt | RestOpts])                                  -> has_norm(RestOpts).

%%--------------------------------------------------------------------
%% @doc Returns true if Queue\Msg == OtherQueue, and false otherwise
%% @end
%%--------------------------------------------------------------------
is_queue_minus_msg(Queue, Msg, OtherQueue) ->
  ThisQueue = lists:delete(Msg, Queue),
  ThisQueue == OtherQueue.

%%--------------------------------------------------------------------
%% @doc Retrieves the topmost item in a history
%% @end
%%--------------------------------------------------------------------
topmost_rec([]) -> no_rec;
topmost_rec([CurHist | RestHist]) ->
  case CurHist of
    {rec, _, _, _, _} -> CurHist;
    _Other -> topmost_rec(RestHist)
  end.

has_send([], _)                                  -> false;
has_send([{send, _, _, _, {_, Time}} | _], Time) -> true;
has_send([_ | RestHist], Time)                   -> has_send(RestHist, Time).

has_spawn([], _)                         -> false;
has_spawn([{spawn, _, _, Pid} | _], Pid) -> true;
has_spawn([_ | RestHist], Pid)           -> has_spawn(RestHist, Pid).

has_rec([], _)                                 -> false;
has_rec([{rec, _, _, {_, Time}, _} | _], Time) -> true;
has_rec([_ | RestHist], Time)                  -> has_rec(RestHist, Time).

has_var(Env, Var) ->
  case proplists:get_value(Var, Env) of
    undefined -> false;
    _ -> true
  end.

fresh_variable_name(Name) ->
  Number = fresh_variable_number(),
  list_to_atom(atom_to_list(Name) ++ "_" ++ integer_to_list(Number)).

fresh_variable_number() ->
  Number = ref_lookup(?FRESH_VAR),
  ref_add(?FRESH_VAR, Number + 1),
  Number.

last_msg_rest(Mail) ->
  LastMsg = lists:last(Mail),
  RestMail = lists:droplast(Mail),
  {LastMsg, RestMail}.

gen_log_send(Pid, OtherPid, MsgValue, Time) ->
  [["Roll send from ", pretty_print:pid(Pid), " of ", pretty_print:expression(MsgValue), " to ", pretty_print:pid(OtherPid), " (", integer_to_list(Time), ")"]].

gen_log_spawn(_Pid, OtherPid) ->
  % [["Roll SPAWN of ",pp_pid(OtherPid)," from ",pp_pid(Pid)]].
  [["Roll spawn of ", pretty_print:pid(OtherPid)]].

empty_log(System) -> System#sys{roll = []}.

must_focus_log(System) ->
  Trace = System#sys.roll,
  case Trace of
    [] -> false;
    _ -> true
  end.

parse_replay_info(Line) ->
  Words = string:split(Line, " "),
  case hd(Words) of
    "call" ->
      {call, lists:nth(2, Words)};
    "main_pid" ->
      {pid, lists:nth(2, Words)};
    _ ->
      none
  end.

add_replay_info({pid, Pid}, Data) -> Data#replay{main_pid = Pid};
add_replay_info({call, Call}, Data) ->
  NCall = lists:flatten(string:replace(Call, "\n", "")),
  ECall = lists:flatten(string:replace(NCall, "\"", "", all)),
  Data#replay{call = ECall};
add_replay_info(_, Data)          -> Data.

read_replay_data(File, Data) ->
  case file:read_line(File) of
    eof ->
      Data;
    {ok, Line} ->
      ReplayInfo = parse_replay_info(Line),
      NData = add_replay_info(ReplayInfo, Data),
      read_replay_data(File, NData)
  end.

extract_replay_data(Path) ->
  ReplayData = #replay{log_path = Path},
  ResPath = Path ++ "/trace_result.log",
  {ok, FileHandler} = file:open(ResPath, [read]),
  NReplayData = read_replay_data(FileHandler, ReplayData),
  put(replay_data, NReplayData),
  % io:format("~p~n", [NReplayData]),
  file:close(FileHandler).

parse_proc_data(Line) ->  Line.

read_replay_proc_data(File, Data) ->
  case file:read_line(File) of
    eof ->
      lists:reverse(Data);
    {ok, Line} ->
      ProcData = parse_proc_data(Line),
      NData = [ProcData | Data],
      read_replay_proc_data(File, NData)
  end.

extract_pid_log_data(Path, Pid) ->
  PidPath = Path ++ "/trace_" ++ Pid ++ ".log",
  {ok, FileHandler} = file:open(PidPath, [read]),
  ReplayProcData = read_replay_proc_data(FileHandler, []),
  file:close(FileHandler),
  ReplayProcData.

get_mod_name(Call) ->
  AExpr =
  case is_list(Call) of
    true ->
      hd(parse_expr(Call ++ "."));
    false ->
      Call
  end,
  {call, _, {remote, _, {atom, _, ModName}, {atom, _, FunName}}, Args} = AExpr,
  {ModName, FunName, Args}.

parse_expr(Func) ->
  case erl_scan:string(Func) of
    {ok, Toks, _} ->
      case erl_parse:parse_exprs(Toks) of
        {ok, _Term} ->
          _Term;
        _Err ->
          {error, parse_error}
      end;
    _Err ->
      {error, parse_error}
  end.

ref_add(Id, Ref) ->  ets:insert(?APP_REF, {Id, Ref}).

ref_lookup(Id) ->  ets:lookup_element(?APP_REF, Id, 2).


fresh_pid() ->
  Pid = ref_lookup(?FRESH_PID),
  ref_add(?FRESH_PID, Pid + 1),
  Pid.
