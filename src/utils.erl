%%%-------------------------------------------------------------------
%%% @doc Utils functions for the reversible semantics for Erlang
%%% @end
%%%-------------------------------------------------------------------

-module(utils).
-export([fundef_lookup/3, fundef_rename/1,
         temp_variable/1, is_temp_variable_name/1, fresh_time/0, pid_exists/2,
         select_proc/2, select_msg/2,
         select_proc_with_time/2, select_proc_with_send/2,
         select_proc_with_spawn/2, select_proc_with_rec/2,
         select_proc_with_var/2,
         merge_env/2,
         stringToMFA/1, stringToArgs/1,
         filter_options/2, filter_procs_opts/1,
         has_fwd/1, has_bwd/1, has_norm/1, has_var/2,
         is_queue_minus_msg/3, topmost_rec/1, last_msg_rest/1,
         gen_log_send/4, gen_log_spawn/2, empty_log/1, must_focus_log/1,
         extract_replay_data/1, extract_pid_log_data/2, get_mod_name/1, fresh_pid/0, parse_file/1]).

-include("cauder.hrl").


%%--------------------------------------------------------------------
%% @doc Searches a function definition in FunDefs with the specified Name and Arity
%% @end
%%--------------------------------------------------------------------

-spec fundef_lookup(Module, Function, Arity) -> {Exported, Clauses} | error when
  Module :: atom(),
  Function :: atom(),
  Arity :: arity(),
  Exported :: boolean(),
  Clauses :: cauder_types:af_clause_seq().

fundef_lookup(M, F, A) ->
  case cauder:ref_match_object({{M, F, A, '_'}, '_'}) of
    [{{M, F, A, Exported}, Cs}] -> {Exported, Cs};
    [] ->
      Dir = cauder:ref_lookup(?LAST_PATH),
      File = filename:join(Dir, atom_to_list(M) ++ ".erl"),
      case filelib:is_regular(File) of
        true ->
          cauder_load:load_module(M, File),
          [{{M, F, A, Exported}, Cs}] = cauder:ref_match_object({{M, F, A, '_'}, '_'}),
          {Exported, Cs};
        false -> error
      end
  end.


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

-spec temp_variable(non_neg_integer()) -> cauder_types:af_variable().

temp_variable(Line) ->
  Number = fresh_variable_number(),
  Name = "k_" ++ integer_to_list(Number),
  {var, Line, list_to_atom(Name)}.

-spec is_temp_variable_name(atom()) -> boolean().

is_temp_variable_name(Name) ->
  case atom_to_list(Name) of
    "k_" ++ _ -> true
    ;_ -> false
  end.


-spec fresh_time() -> non_neg_integer().

fresh_time() ->
  Time = cauder:ref_lookup(?FRESH_TIME),
  cauder:ref_add(?FRESH_TIME, Time + 1),
  Time.


-spec pid_exists([cauder_types:process()], pos_integer()) -> boolean().

pid_exists([#proc{pid = Pid} | _], Pid) -> true;
pid_exists([_ | Procs], Pid)            -> pid_exists(Procs, Pid);
pid_exists([], _)                       -> false.


%%--------------------------------------------------------------------
%% @doc Returns a tuple where the first element is the process whose
%% Pid matches the given one, and the second element is a list with
%% the rest of processes from `Procs`
%% @end
%%--------------------------------------------------------------------

-spec select_proc([cauder_types:process()], pos_integer()) -> {cauder_types:process(), [cauder_types:process()]}.

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

-spec select_proc_with_spawn([cauder_types:process()], pos_integer()) -> [cauder_types:process()].

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
%% @doc Converts a String into MFA tuple
%% @end
%%--------------------------------------------------------------------
-spec stringToMFA(String) -> {Module, Function, Arity} when
  String :: string(),
  Module :: atom(),
  Function :: atom(),
  Arity :: arity().

stringToMFA(String) ->
  [M, F, A] = string:lexemes(String, ":/"),
  {list_to_atom(M), list_to_atom(F), list_to_integer(A)}.

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
  cauder_syntax:expr_list(Args).


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
    {rec, _, _, _, _, _} -> CurHist;
    _Other -> topmost_rec(RestHist)
  end.

has_send([], _)                                     -> false;
has_send([{send, _, _, _, _, {_, Time}} | _], Time) -> true;
has_send([_ | RestHist], Time)                      -> has_send(RestHist, Time).

has_spawn([], _)                            -> false;
has_spawn([{spawn, _, _, _, Pid} | _], Pid) -> true;
has_spawn([_ | RestHist], Pid)              -> has_spawn(RestHist, Pid).

has_rec([], _)                                    -> false;
has_rec([{rec, _, _, _, {_, Time}, _} | _], Time) -> true;
has_rec([_ | RestHist], Time)                     -> has_rec(RestHist, Time).

has_var(Env, Var) ->
  case proplists:get_value(Var, Env) of
    undefined -> false;
    _ -> true
  end.

fresh_variable_name(Name) ->
  Number = fresh_variable_number(),
  list_to_atom(atom_to_list(Name) ++ "_" ++ integer_to_list(Number)).

fresh_variable_number() ->
  Number = cauder:ref_lookup(?FRESH_VAR),
  cauder:ref_add(?FRESH_VAR, Number + 1),
  Number.

last_msg_rest(Mail) ->
  LastMsg = lists:last(Mail),
  RestMail = lists:droplast(Mail),
  {LastMsg, RestMail}.

gen_log_send(Pid, OtherPid, MsgValue, Time) ->
  [["Roll send from ", pretty_print:pid(Pid), " of ", pretty_print:to_string(MsgValue), " to ", pretty_print:pid(OtherPid), " (", integer_to_list(Time), ")"]].

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

parse_proc_data(Line) -> Line.

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



-spec fresh_pid() -> pos_integer().

fresh_pid() ->
  Pid = cauder:ref_lookup(?FRESH_PID),
  cauder:ref_add(?FRESH_PID, Pid + 1),
  Pid.


parse_file(File) ->
  case epp:parse_file(File, [], []) of
    {ok, Forms} ->
      % Extract attributes, but removing the 'file' attribute so it doesn't appear in the 'Code' tab
      Attributes = [Form || Form = {attribute, _, Name, _} <- Forms, Name /= file],
      % Extract the name of the current module
      {attribute, _, module, Module} = lists:keyfind(module, 3, Attributes),
      % Extract function definitions
      Functions = [Form || Form = {function, _, _, _, _} <- Forms],
      % Extract comments from the file
      Comments = erl_comment_scan:file(File),
      % Generate forms that will appear in the 'Code' tab
      FinalForms = erl_recomment:recomment_forms(Attributes ++ Functions, Comments),

      % TODO erl_expand_records:module/2

      {ok, Module, Functions, FinalForms};
    Error ->
      Error
  end.
