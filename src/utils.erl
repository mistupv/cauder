%%%-------------------------------------------------------------------
%%% @doc Utils functions for the reversible semantics for Erlang
%%% @end
%%%-------------------------------------------------------------------

-module(utils).
-export([fundef_lookup/2, fundef_lookup/3, fundef_rename/1, substitute/2,
         build_var/1, build_var/2, pid_exists/2,
         select_proc/2, select_msg/2,
         select_proc_with_time/2, select_proc_with_send/2,
         select_proc_with_spawn/2, select_proc_with_rec/2,
         select_proc_with_var/2, list_from_core/1,
         merge_env/2,
         replace/3, replace_all/2, pp_system/2, pp_trace/1, pp_roll_log/1,
         funNames/1,
         stringToNameAndArity/1,stringToArgs/1, toCore/1, toErlang/1,
         filter_options/2, filter_procs_opts/1,
         has_fwd/1, has_bwd/1, has_norm/1, has_var/2,
         is_queue_minus_msg/3, topmost_rec/1, last_msg_rest/1,
         gen_log_send/4, gen_log_spawn/2, empty_log/1, must_focus_log/1,
         extract_replay_data/1, extract_pid_log_data/2, get_mod_name/1]).

-include("cauder.hrl").
-include_lib("wx/include/wx.hrl").

%%--------------------------------------------------------------------
%% @doc Searches a function definition in FunDefs with name FunName
%% @end
%%--------------------------------------------------------------------
fundef_lookup(FunName, FunDefs) ->
    %io:fwrite("---------------~n"),
    %io:write(FunName),
    %io:fwrite("~n---------------~n"),
    %io:write(FunDefs),
    %io:fwrite("~n---------------~n"),
  case lists:keyfind(FunName, 1, FunDefs) of
      {_, FunDef} -> FunDef;
      false -> io:fwrite("Funzione non trovata", [])
  end.


%%--------------------------------------------------------------------
%% @doc Searches a function definition in FunDefs with the specified Name and Arity
%% @end
%%--------------------------------------------------------------------
-spec fundef_lookup(Name, Arity, FunDefs) -> FunDef | not_found when
  Name :: atom(),
  Arity :: arity(),
  FunDefs :: [FunDef],
  FunDef :: erl_parse:af_function_decl().

fundef_lookup(FunName, FunArity, FunDefs) ->
  case lists:search(
    fun({'function', _, Name, Arity, _}) ->
      Name == FunName andalso Arity == FunArity
    end,
    FunDefs
  ) of
    {value, FunDef} ->
      FunDef;
    false ->
      not_found
  end.


%%--------------------------------------------------------------------
%% @doc Renames all the variables in function definition FunDef
%% @end
%%--------------------------------------------------------------------
-spec fundef_rename(FunDef) -> NewFunDef when
  FunDef :: erl_parse:af_function_decl(),
  NewFunDef :: erl_parse:af_function_decl().

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

substitute(SuperExp, Env) ->
  cerl_trees:map(
    fun (Exp) ->
      case cerl:type(Exp) of
        var ->
          case proplists:get_value(Exp, Env) of
            undefined -> Exp;
            Value -> Value
          end;
        _   -> Exp
      end
    end, SuperExp).
%%--------------------------------------------------------------------
%% @doc Builds a variable from a given number Num
%% @end
%%--------------------------------------------------------------------
build_var(Num) ->
  NumAtom = list_to_atom("k_" ++ integer_to_list(Num)),
  cerl:c_var(NumAtom).

%%------------ It is not working in Erlang 22-----------------------------
%%build_var(Name,Num) ->
%%  NumAtom = list_to_atom(atom_to_list(Name) ++ "_" ++ integer_to_list(Num)),
%%  cerl:c_var(NumAtom).
%%------------------------------------------------------------------------
build_var(Name,Num) ->
  NewName =
    case Name of
      UserVarName when is_atom(UserVarName) ->
        atom_to_list(UserVarName);
      % Core variable names are just numbers in the last update
       CoreVarName ->
         "c" ++ integer_to_list(CoreVarName)
    end,
  NumAtom = list_to_atom(NewName ++ "_" ++ integer_to_list(Num)),
  cerl:c_var(NumAtom).

pid_exists(Procs, Pid) ->
  case [ P || P <- Procs, P#proc.pid == Pid] of
    [] -> false;
    _ -> true
  end.

%%--------------------------------------------------------------------
%% @doc Returns a tuple with a process with pid Pid from Procs and
%% the rest of processes from Procs
%% @end
%%--------------------------------------------------------------------
-spec select_proc(Procs, Pid) -> {Proc, RestProcs} when
  Procs :: [Proc],
  Pid :: erl_syntax:syntaxTree(), % TODO Less generic type
  Proc :: #proc{},
  RestProcs :: [Proc].

select_proc(Procs, Pid) ->
  {[Proc | []], RestProcs} = lists:partition(fun(P) -> P#proc.pid == Pid end, Procs),
  {Proc, RestProcs}.

%%--------------------------------------------------------------------
%% @doc Returns a tuple with a message with id Time from Msgs and
%% the rest of messages from Msgs
%% @end
%%--------------------------------------------------------------------
select_msg(Msgs, Time) ->
  [Msg] = [ M || M <- Msgs, M#msg.time == Time],
  RestMsgs = [ M || M <- Msgs, M#msg.time /= Time],
  {Msg, RestMsgs}.

%%--------------------------------------------------------------------
%% @doc Returns the process that contains a message with id Time
%% from Procs
%% @end
%%--------------------------------------------------------------------
select_proc_with_time(Procs, Time) ->
  ProcWithTime =
    lists:filter( fun (Proc) ->
                    Mail = Proc#proc.mail,
                    length([ ok || {_,MsgTime} <- Mail, MsgTime == Time]) > 0
                  end, Procs),
  hd(ProcWithTime).

%%--------------------------------------------------------------------
%% @doc Returns the processes that contain a send item in history
%% with time Time
%% @end
%%--------------------------------------------------------------------
select_proc_with_send(Procs, Time) ->
  ProcWithSend =
    lists:filter( fun (Proc) ->
                    Hist = Proc#proc.hist,
                    has_send(Hist, Time)
                  end, Procs),
  ProcWithSend.

%%--------------------------------------------------------------------
%% @doc Returns the processes that contain a spawn item in history
%% with pid Pid
%% @end
%%--------------------------------------------------------------------
select_proc_with_spawn(Procs, Pid) ->
  ProcWithSpawn =
    lists:filter( fun (Proc) ->
                    Hist = Proc#proc.hist,
                    has_spawn(Hist, Pid)
                  end, Procs),
  ProcWithSpawn.

%%--------------------------------------------------------------------
%% @doc Returns the processes that contain a spawn item in history
%% with pid Pid
%% @end
%%--------------------------------------------------------------------
select_proc_with_rec(Procs, Time) ->
  ProcWithRec =
    lists:filter( fun (Proc) ->
                    Hist = Proc#proc.hist,
                    has_rec(Hist, Time)
                  end, Procs),
  ProcWithRec.

%%--------------------------------------------------------------------
%% @doc Returns the processes that contain a binding for Var in
%% its environment Env
%% @end
%%--------------------------------------------------------------------
select_proc_with_var(Procs, Var) ->
  ProcWithRec =
    lists:filter( fun (Proc) ->
                    Env = Proc#proc.env,
                    has_var(Env, Var)
                  end, Procs),
  ProcWithRec.

%%--------------------------------------------------------------------
%% @doc Transforms a Core Erlang list to a regular list
%% @end
%%--------------------------------------------------------------------
list_from_core(Exp) ->
  case  cerl:is_c_nil(Exp) of
    true -> [];
    false -> [cerl:cons_hd(Exp)|list_from_core(cerl:cons_tl(Exp))]
  end.

%%--------------------------------------------------------------------
%% @doc Update the environment Env with multiple bindings
%% @end
%%--------------------------------------------------------------------
-spec merge_env(Env, Bindings) -> NewEnv when
  Env :: erl_eval:binding_struct(),
  Bindings :: erl_eval:binding_struct(),
  NewEnv :: erl_eval:binding_struct().

merge_env(Env, []) ->
  Env;
merge_env(Env, [{Name, Value} | RestBindings]) ->
  NewEnv = erl_eval:add_binding(Name, Value, Env),
  merge_env(NewEnv, RestBindings).

%%--------------------------------------------------------------------
%% @doc A typical substitution application
%% @end
%%--------------------------------------------------------------------

replace_all([],Exp) -> Exp;
replace_all([{Var,Val}|R],Exp) ->
  %io:format("replace: ~p~n~p~n~p~n",[Var,Val,Exp]),
  NewExp = utils:replace(Var,Val,Exp),
  %io:format("--result: p~n",[NewExp]),
  replace_all(R,NewExp).


%%--------------------------------------------------------------------
%% @doc Replaces a variable Var by SubExp (subexpression) in SuperExp
%% (expression)
%% @end
%%--------------------------------------------------------------------
%replace(Var, SubExp, SuperExp) ->
%  VarName = cerl:var_name(Var),
%  case cerl:type(SuperExp) of
%    var -> case cerl:var_name(SuperExp) of
%             VarName -> SubExp;
%             _Other -> SuperExp
%           end;
%    call -> NewArgs = lists:map(fun (E) -> replace(Var,SubExp,E) end, cerl:call_args(SuperExp)),
%            CallModule = cerl:call_module(SuperExp),
%            CallName = cerl:call_name(SuperExp),
%            cerl:c_call(CallModule,CallName,NewArgs);
%    %_Other -> SuperExp
%    _Other -> cerl_trees:map(
%                fun (Exp) ->
%                  case cerl:type(Exp) of
%                    var ->
%                      case cerl:var_name(Exp) of
%                          VarName -> SubExp;
%                          _Other -> Exp
%                      end;
%                    _Other -> Exp
%                  end
%                end, SuperExp)
%  end.

%%--------------------------------------------------------------------
%% @doc Replaces a variable Var by SubExp (subexpression) in SuperExp
%% (expression)
%% @end
%%--------------------------------------------------------------------
replace(Var, SubExp, SuperExp) ->
  VarName = cerl:var_name(Var),
  cerl_trees:map(
    fun (Exp) ->
      case cerl:type(Exp) of
        var ->
          case cerl:var_name(Exp) of
            VarName -> SubExp;
            _Other -> Exp
          end;
        _Other -> Exp
      end
    end, SuperExp).

%%--------------------------------------------------------------------
%% @doc Pretty-prints a given System
%% @end
%%--------------------------------------------------------------------
pp_system(#sys{msgs = Msgs, procs = Procs}, Opts) ->
  pp_msgs(Msgs) ++ "\n" ++ pp_procs(Procs, Opts).

pp_msgs(Msgs) ->
  MsgsList = [pp_msg(Msg) || Msg <- Msgs],
  "GM : [" ++ string:join(MsgsList,",") ++ "]\n".

pp_msg(#msg{dest = DestPid, val = MsgValue, time = Time}) ->
  "(" ++ pp(DestPid) ++ ",{" ++ pp(MsgValue) ++ "," ++ [{?wxRED, integer_to_list(Time)}] ++ "})".

pp_procs(Procs, Opts) ->
  SortProcs = lists:sort(fun(P1, P2) -> P1#proc.pid < P2#proc.pid end, Procs),
  ProcsList = [pp_proc(Proc, Opts) || Proc <- SortProcs],
  string:join(ProcsList,"\n").

pp_proc(#proc{pid = Pid, hist = Hist, env = Env, exp = Exp, mail = Mail, spf = Fun}, Opts) ->
  pp_pre(Pid, Fun) ++
  pp_mail(Mail, Opts) ++
  pp_hist(Hist, Opts) ++
  pp_env(Env, Exp, Opts)++
  pp(Exp, Opts).

pp_pre(Pid, Fun) ->
  "=============== " ++ pp_pid(Pid) ++ ": " ++ pp_fun(Fun)++ " ===============\n".

pp_pid(Pid) ->
  "Proc. " ++ pp(Pid).

pp_fun(undef) ->
  "";
pp_fun({Name, Arity}) ->
  atom_to_list(Name) ++ "/" ++ integer_to_list(Arity).


pp_env(Env, Exp, Opts) ->
  case proplists:get_value(?PRINT_ENV, Opts) of
    false -> "";
    true  -> "ENV: " ++ pp_env_1(Env, Exp, Opts) ++ "\n"
  end.

pp_env_1(Env, Exp, Opts) ->
  PrintEnv =
    case proplists:get_value(?PRINT_FULL_ENV, Opts) of
      true  -> Env;
      false -> relevant_bindings(Env, Exp)
    end,
  PairsList = [pp_pair(Var,Val) || {Var,Val} <- PrintEnv],
  "{" ++ string:join(PairsList,", ") ++ "}".

pp_pair(Var, Val) ->
  atom_to_list(Var) ++ " -> " ++ pp(Val).

is_conc_item({spawn,_,_,_}) -> true;
is_conc_item({send,_,_,_,_}) -> true;
is_conc_item({rec,_,_,_,_}) -> true;
is_conc_item(_) -> false.

pp_hist(Hist, Opts) ->
  case proplists:get_value(?PRINT_HIST, Opts) of
    false -> "";
    true  -> pp_hist_1(Hist, Opts) ++ "\n"
  end.

pp_hist_1(Hist, Opts) ->
  FiltHist =
    case proplists:get_value(?PRINT_FULL, Opts) of
      false -> lists:filter(fun is_conc_item/1, Hist);
      true  -> Hist
    end,
  StrItems = [pp_hist_2(Item) || Item <- FiltHist],
  "H  : [" ++ string:join(StrItems, ",") ++ "]".

pp_hist_2({tau,_,_}) ->
  "seq";
pp_hist_2({self,_,_}) ->
  "self";
pp_hist_2({spawn,_,_,Pid}) ->
  "spawn(" ++ [{?CAUDER_GREEN, pp(Pid)}] ++ ")";
pp_hist_2({send,_,_,_,{Value,Time}}) ->
  "send(" ++ pp(Value) ++ "," ++ [{?wxRED, integer_to_list(Time)}] ++ ")";
pp_hist_2({rec,_,_,{Value,Time},_}) ->
  "rec(" ++ pp(Value) ++ "," ++ [{?wxBLUE, integer_to_list(Time)}] ++ ")".

pp_mail(Mail, Opts) ->
  case proplists:get_value(?PRINT_MAIL, Opts) of
    false -> "";
    true  -> "LM : " ++ pp_mail_1(Mail) ++ "\n"
  end.

pp_mail_1([]) -> "[]";
pp_mail_1(Mail) ->
  MailList = [pp_msg_mail(Val, Time) || {Val, Time} <- Mail],
  "[" ++ string:join(MailList,",") ++ "]".

pp_msg_mail(Val, Time) ->
  "{" ++ pp(Val) ++ "," ++  [{?CAUDER_GREEN, integer_to_list(Time)}] ++ "}".


pp(Exprs, Opts) ->
  case proplists:get_value(?PRINT_EXP, Opts) of
    false -> "";
    true -> "EXP: " ++ lists:join(",\n     ", [pp(Expr) || Expr <- Exprs]) ++ "\n"
  end.

pp(Expr) ->
  lists:flatten(erl_prettypr:format(Expr)).

%%--------------------------------------------------------------------
%% @doc Pretty-prints a given system trace
%% @end
%%--------------------------------------------------------------------
pp_trace(#sys{trace = Trace}) ->
  % Trace is built as a stack (newest item is first)
  % and we must reverse it to print it
  RevTrace = lists:reverse(Trace),
  TraceStr = [pp_trace_item(Item) || Item <- RevTrace],
  string:join(TraceStr,"\n").

pp_trace_item(#trace{type = Type,
                     from = From,
                     to   = To,
                     val  = Val,
                     time = Time}) ->
  case Type of
    ?RULE_SEND    -> pp_trace_send(From, To, Val, Time);
    ?RULE_SPAWN   -> pp_trace_spawn(From, To);
    ?RULE_RECEIVE -> pp_trace_receive(From, Val, Time)
  end.

pp_trace_send(From, To, Val, Time) ->
  [pp_pid(From)," sends ",pp(Val)," to ",pp_pid(To)," (",integer_to_list(Time),")"].

pp_trace_spawn(From, To) ->
  [pp_pid(From)," spawns ",pp_pid(To)].

pp_trace_receive(From, Val, Time) ->
  [pp_pid(From)," receives ",pp(Val)," (",integer_to_list(Time),")"].

%%--------------------------------------------------------------------
%% @doc Prints a given system roll log
%% @end
%%--------------------------------------------------------------------
pp_roll_log(#sys{roll = RollLog}) ->
  string:join(RollLog,"\n").

%%--------------------------------------------------------------------
%% @doc Returns a list with the names of the functions defined in the given FunForms
%% @end
%%--------------------------------------------------------------------
-spec funNames(FunForms) -> FunNames when
  FunForms :: [erl_parse:af_function_decl()],
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
-spec stringToArgs(Str) -> [Args] when
  Str :: string(),
  Args :: [erl_parse:abstract_expr()].

stringToArgs([]) ->
  [];
stringToArgs(Str) ->
  {ok, Tokens, _} = erl_scan:string(Str ++ "."),
  {ok, Args} = erl_parse:parse_exprs(Tokens),
  Args.

%%--------------------------------------------------------------------
%% @doc Transforms an Erlang expression Expr to its equivalent in
%% Core Erlang
%% @end
%%--------------------------------------------------------------------
toCore(Expr) ->
  case Expr of
    {atom, _, Atom} ->
      cerl:c_atom(Atom);
    {integer, _, Int} ->
      cerl:c_int(Int);
    {op, _, '-',{integer, _, Int}} ->
      cerl:c_int(-Int);
    {float, _, Float} ->
      cerl:c_float(Float);
    {string, _, String} ->
      cerl:c_string(String);
    {tuple, _, TupleEs} ->
      cerl:c_tuple_skel([toCore(E) || E <- TupleEs]);
    {cons, _, Head, Tail} ->
      cerl:c_cons_skel(toCore(Head), toCore(Tail));
    {nil, _} ->
      cerl:c_nil()
  end.

toErlang(Expr) ->
  LitExpr =
    case cerl:is_literal(Expr) of
      true -> Expr;
      false -> cerl:fold_literal(Expr)
    end,
  cerl:concrete(LitExpr).

%%--------------------------------------------------------------------
%% @doc Filters the options with identifier Id
%% @end
%%--------------------------------------------------------------------
filter_options([], _) -> [];
filter_options([CurOpt|RestOpts], Id) ->
  #opt{id = OptId} = CurOpt,
  case (OptId == Id) of
    true -> [CurOpt|filter_options(RestOpts,Id)];
    false -> filter_options(RestOpts,Id)
  end.

%%--------------------------------------------------------------------
%% @doc Filters the process options from a list of Options
%% @end
%%--------------------------------------------------------------------
filter_procs_opts([]) -> [];
filter_procs_opts([CurOpt|RestOpts]) ->
  #opt{type = Type} = CurOpt,
  case Type of
    ?TYPE_MSG  -> filter_procs_opts(RestOpts);
    ?TYPE_PROC -> [CurOpt|filter_procs_opts(RestOpts)]
  end.

%%--------------------------------------------------------------------
%% @doc Returns true if a list of Options has a forward option,
%% and false otherwise
%% @end
%%--------------------------------------------------------------------
has_fwd([]) -> false;
has_fwd([#opt{sem = ?FWD_SEM}|_RestOpts]) -> true;
has_fwd([_CurOpt|RestOpts]) -> has_fwd(RestOpts).

%%--------------------------------------------------------------------
%% @doc Returns true if a list of Options has a backward option,
%% and false otherwise
%% @end
%%--------------------------------------------------------------------
has_bwd([]) -> false;
has_bwd([#opt{sem = ?BWD_SEM}|_RestOpts]) -> true;
has_bwd([_CurOpt|RestOpts]) -> has_bwd(RestOpts).

%%--------------------------------------------------------------------
%% @doc Returns true if a list of Options has a normalizing option,
%% and false otherwise
%% @end
%%--------------------------------------------------------------------
has_norm([]) -> false;
has_norm([#opt{sem = ?FWD_SEM, rule = Rule}|RestOpts]) ->
  case Rule of
    ?RULE_SCHED -> has_norm(RestOpts);
    _OtherRule -> true
  end;
has_norm([_CurOpt|RestOpts]) -> has_norm(RestOpts).

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
topmost_rec([CurHist|RestHist]) ->
  case CurHist of
    {rec,_,_,_,_} -> CurHist;
    _Other -> topmost_rec(RestHist)
  end.

has_send([], _) -> false;
has_send([{send,_,_,_,{_,Time}}|_], Time) -> true;
has_send([_|RestHist], Time) -> has_send(RestHist, Time).

has_spawn([], _) -> false;
has_spawn([{spawn,_,_,Pid}|_], Pid) -> true;
has_spawn([_|RestHist], Pid) -> has_spawn(RestHist, Pid).

has_rec([], _) -> false;
has_rec([{rec,_,_, {_, Time},_}|_], Time) -> true;
has_rec([_|RestHist], Time) -> has_rec(RestHist, Time).

has_var(Env, Var) ->
  case proplists:get_value(Var, Env) of
    undefined -> false;
    _ -> true
  end.

fresh_variable_name(Name) ->
  VarNum = ref_lookup(?FRESH_VAR),
  ref_add(?FRESH_VAR, VarNum + 1),
  list_to_atom(atom_to_list(Name) ++ "_" ++ integer_to_list(VarNum)).

last_msg_rest(Mail) ->
  LastMsg = lists:last(Mail),
  LenMail = length(Mail),
  RestMail = lists:sublist(Mail,LenMail-1),
  {LastMsg, RestMail}.

relevant_bindings(Env, Exprs) ->
  Vars = sets:union([erl_syntax_lib:variables(Expr) || Expr <- Exprs]),
  [Bind || Bind = {Var, _} <- erl_eval:bindings(Env), sets:is_element(Var, Vars)].

gen_log_send(Pid, OtherPid, MsgValue, Time) ->
[["Roll send from ",pp_pid(Pid), " of ",pp(MsgValue), " to ",pp_pid(OtherPid), " (",integer_to_list(Time),")"]].

gen_log_spawn(_Pid, OtherPid) ->
  % [["Roll SPAWN of ",pp_pid(OtherPid)," from ",pp_pid(Pid)]].
  [["Roll spawn of ",pp_pid(OtherPid)]].

empty_log(System) ->
  System#sys{roll = []}.

must_focus_log(System) ->
  Trace = System#sys.roll,
  case Trace of
      [] -> false;
      _  -> true
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

add_replay_info({pid, Pid}, Data) ->
  Data#replay{main_pid = Pid};
add_replay_info({call, Call}, Data) ->
  NCall = lists:flatten(string:replace(Call, "\n", "")),
  ECall = lists:flatten(string:replace(NCall, "\"", "", all)),
  Data#replay{call = ECall};
add_replay_info(_, Data) ->
  Data.

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

parse_proc_data(Line) ->
  Line.

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
                hd(parse_expr(Call++"."));
            false ->
                Call
        end,
    {call,_,{remote,_,{atom,_,ModName},{atom,_,FunName}},Args} = AExpr,
    {ModName,FunName,Args}.

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

ref_add(Id, Ref) ->
    ets:insert(?APP_REF, {Id, Ref}).

ref_lookup(Id) ->
    ets:lookup_element(?APP_REF, Id, 2).
