%%%-------------------------------------------------------------------
%%% @doc Utils functions for the reversible semantics for Erlang
%%% @end
%%%-------------------------------------------------------------------

-module(utils).
-export([fundef_lookup/3, fundef_rename/1,
         temp_variable/1, is_temp_variable_name/1, fresh_uid/0, select_msg/2,
         find_proc_with_send/2,
         find_proc_with_spawn/2, find_proc_with_rec/2,
         find_proc_with_var/2,
         merge_bindings/2,
         stringToMFA/1, stringToExprs/1,
         filter_options/2,
         has_var/2,
         is_queue_minus_msg/3, topmost_rec/1, last_msg_rest/1,
         gen_log_send/4, gen_log_spawn/1, clear_log/1, must_focus_log/1,
         load_replay_data/1, get_log_data/2, parse_call/1, fresh_pid/0, check_log/1, find_spawn_parent/2, find_msg_sender/2, find_msg_receiver/2, check_msg/2, current_line/1]).

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
  case cauder:db_match({{M, F, A, '_'}, '_'}) of
    [{{M, F, A, Exported}, Cs}] -> {Exported, Cs};
    [] ->
      File = filename:join(get(path), atom_to_list(M) ++ ".erl"),
      case filelib:is_regular(File) of
        true ->
          {ok, M} = cauder_load:file(File),
          [{{M, F, A, Exported}, Cs}] = cauder:db_match({{M, F, A, '_'}, '_'}),
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


%%--------------------------------------------------------------------
%% @doc Returns a tuple with a message with id UID from Msgs and
%% the rest of messages from Msgs
%% @end
%%--------------------------------------------------------------------

-spec select_msg([cauder_types:message()], non_neg_integer()) -> {cauder_types:message(), [cauder_types:message()]}.

select_msg(Mail, UID) ->
  {[Message], RestMessages} = lists:partition(fun(M) -> M#msg.uid == UID end, Mail),
  {Message, RestMessages}.


-spec check_msg([cauder_types:message()], pos_integer()) -> cauder_types:message() | none.

check_msg(Msgs, UID) ->
  MsgsT = [M || M <- Msgs, M#msg.uid == UID],
  case MsgsT of
    [] -> none;
    [Msg] -> Msg
  end.


-spec check_log([cauder_types:log_entry()]) -> cauder_types:log_entry() | none.

check_log([])         -> none;
check_log([Item | _]) -> Item.


-spec find_item(cauder_types:process_dict(), cauder_types:log_entry()) -> [pos_integer()].

find_item(PDict, Item) ->
  lists:filtermap(
    fun({_, #proc{pid = Pid, log = Log}}) ->
      case lists:member(Item, Log) of
        true -> {true, Pid};
        false -> false
      end
    end, PDict).


-spec find_spawn_parent(cauder_types:process_dict(), pos_integer()) -> [pos_integer()].

find_spawn_parent(PDict, Pid) -> find_item(PDict, {spawn, Pid}).

-spec find_msg_sender(cauder_types:process_dict(), pos_integer()) -> [pos_integer()].

find_msg_sender(PDict, UID) -> find_item(PDict, {send, UID}).

-spec find_msg_receiver(cauder_types:process_dict(), pos_integer()) -> [pos_integer()].

find_msg_receiver(PDict, UID) -> find_item(PDict, {'receive', UID}).


%% =====================================================================
%% @doc Returns the processes that contain a spawn item in history with
%% pid `Pid`

-spec find_proc_with_spawn(cauder_types:process_dict(), pos_integer()) -> {value, cauder_types:process()} | false.

find_proc_with_spawn(PDict, Pid) ->
  {_, Ps} = lists:unzip(orddict:to_list(PDict)),
  lists:search(fun(#proc{hist = H}) -> has_spawn(H, Pid) end, Ps).


%% =====================================================================
%% @doc Returns the processes that contain a send item in history with
%% time `UID`

-spec find_proc_with_send(cauder_types:process_dict(), non_neg_integer()) -> {value, cauder_types:process()} | false.

find_proc_with_send(PDict, UID) ->
  {_, Ps} = lists:unzip(orddict:to_list(PDict)),
  lists:search(fun(#proc{hist = H}) -> has_send(H, UID) end, Ps).


%% =====================================================================
%% @doc Returns the processes that contain a rec item in history with
%% time `UID`

-spec find_proc_with_rec(cauder_types:process_dict(), non_neg_integer()) -> {value, cauder_types:process()} | false.

find_proc_with_rec(PDict, UID) ->
  {_, Ps} = lists:unzip(orddict:to_list(PDict)),
  lists:search(fun(#proc{hist = H}) -> has_rec(H, UID) end, Ps).

%% =====================================================================
%% @doc Returns the process that contain a binding for Var in its
%% environment

-spec find_proc_with_var(cauder_types:process_dict(), atom()) -> {value, cauder_types:process()} | false.

find_proc_with_var(PDict, Name) ->
  {_, Ps} = lists:unzip(orddict:to_list(PDict)),
  lists:search(fun(#proc{env = Bs}) -> has_var(Bs, Name) end, Ps).


%%--------------------------------------------------------------------
%% @doc Update the environment Env with multiple bindings


-spec merge_bindings(cauder_types:environment(), cauder_types:environment()) -> cauder_types:environment().

merge_bindings(Bs1, Bs2) ->
  lists:foldl(
    fun({Name, Val}, Bs) ->
      case orddict:find(Name, Bs) of
        {ok, Val} -> Bs; % Already with SAME value
        {ok, V1} -> erlang:error(badmatch, V1);
        error -> orddict:store(Name, Val, Bs)
      end
    end,
    Bs2, orddict:to_list(Bs1)).


%%--------------------------------------------------------------------
%% @doc Converts a String into MFA tuple

-spec stringToMFA(String) -> {Module, Function, Arity} when
  String :: string(),
  Module :: atom(),
  Function :: atom(),
  Arity :: arity().

stringToMFA(String) ->
  [M, F, A] = string:lexemes(String, ":/"),
  {list_to_atom(M), list_to_atom(F), list_to_integer(A)}.

%%--------------------------------------------------------------------
%% @doc Parses a string Str that represents a list of expressions
%% and transforms these expressions to their equivalent in Abstract Syntax

-spec stringToExprs(String) -> Expressions | error when
  String :: string(),
  Expressions :: [erl_parse:abstract_expr()].

stringToExprs([]) -> [];
stringToExprs(Str) ->
  case erl_scan:string(Str ++ ".") of
    {ok, Tokens, _} ->
      case erl_parse:parse_exprs(Tokens) of
        {ok, Value} -> cauder_syntax:expr_list(Value);
        _ -> error
      end;
    _ -> error
  end.


%%--------------------------------------------------------------------
%% @doc Filters the options with identifier Id

filter_options([Opt | Opts], Id) when Opt#opt.pid =:= Id -> [Opt | filter_options(Opts, Id)];
filter_options([_ | Opts], Id)                           -> filter_options(Opts, Id);
filter_options([], _)                                    -> [].


%%--------------------------------------------------------------------
%% @doc Returns true if Queue\Msg == OtherQueue, and false otherwise

is_queue_minus_msg(Queue, Msg, OtherQueue) ->
  ThisQueue = lists:delete(Msg, Queue),
  ThisQueue == OtherQueue.


%%--------------------------------------------------------------------
%% @doc Retrieves the topmost item in a history

topmost_rec([]) -> no_rec;
topmost_rec([CurHist | RestHist]) ->
  case CurHist of
    {rec, _, _, _, _, _} -> CurHist;
    _Other -> topmost_rec(RestHist)
  end.


has_spawn([], _)                                             -> false;
has_spawn([{spawn, _Bs, _Es, _Stk, SpawnPid} | _], SpawnPid) -> true;
has_spawn([_ | RestHist], Pid)                               -> has_spawn(RestHist, Pid).

has_send([], _)                                              -> false;
has_send([{send, _Bs, _Es, _Stk, #msg{uid = UID}} | _], UID) -> true;
has_send([_ | RestHist], UID)                                -> has_send(RestHist, UID).

has_rec([], _)                                             -> false;
has_rec([{rec, _Bs, _Es, _Stk, #msg{uid = UID}} | _], UID) -> true;
has_rec([_ | RestHist], UID)                               -> has_rec(RestHist, UID).

has_var(Bs, Name) -> cauder_eval:binding(Name, Bs) =/= unbound.


-spec fresh_pid() -> cauder_types:proc_id().

fresh_pid() ->
  NewPid = get(last_pid) + 1,
  put(last_pid, NewPid),
  NewPid.


-spec fresh_uid() -> cauder_types:msg_id().

fresh_uid() ->
  NewUid =
    case get(last_uid) of
      undefined -> 0;
      OldUid -> OldUid + 1
    end,
  put(last_uid, NewUid),
  NewUid.


fresh_variable_name(Name) ->
  Number = fresh_variable_number(),
  list_to_atom(atom_to_list(Name) ++ "_" ++ integer_to_list(Number)).

fresh_variable_number() ->
  NewVar =
    case get(last_var) of
      undefined -> 0;
      OldVar -> OldVar + 1
    end,
  put(last_var, NewVar),
  NewVar.


last_msg_rest(Mail) ->
  LastMsg = lists:last(Mail),
  RestMail = lists:droplast(Mail),
  {LastMsg, RestMail}.


gen_log_spawn(OtherPid) ->
  [["Roll spawn of ", pretty_print:pid(OtherPid)]].

gen_log_send(Pid, OtherPid, MsgValue, UID) ->
  [["Roll send from ", pretty_print:pid(Pid), " of ", pretty_print:to_string(MsgValue),
    " to ", pretty_print:pid(OtherPid), " (", integer_to_list(UID), ")"]].


clear_log(System) -> System#sys{roll = []}.

must_focus_log(System) ->
  Trace = System#sys.roll,
  case Trace of
    [] -> false;
    _ -> true
  end.


%% ==================== Load replay data ==================== %%


-spec load_replay_data(file:filename()) -> ok.

load_replay_data(Path) ->
  ResultFile = filename:join(Path, "trace_result.log"),
  {ok, FileHandler} = file:open(ResultFile, [read]),
  Lines = get_all_lines(FileHandler),
  file:close(FileHandler),
  #{call := Call, main_pid := Pid} = parse_lines(Lines),
  Data = #replay{log_path = Path, call = Call, main_pid = Pid},
  put(replay_data, Data),
  ok.


-spec get_all_lines(file:io_device()) -> [string()].

get_all_lines(File) ->
  case file:read_line(File) of
    eof -> [];
    {ok, Line} -> [string:chomp(Line) | get_all_lines(File)]
  end.


-spec parse_lines(Lines) -> Data when
  Lines :: [string()],
  Data :: #{call := Call, main_pid := Pid},
  Call :: {atom(), atom(), [cauder_types:abstract_expr()]},
  Pid :: cauder_types:proc_id().

parse_lines(Lines) -> parse_lines(Lines, #{call => undefined, main_pid => undefined}).


-type optional(T) :: T | undefined.


-spec parse_lines(Lines, Data) -> NewData when
  Lines :: [string()],
  Data :: #{call := optional(Call), main_pid := optional(Pid)},
  NewData :: #{call := Call, main_pid := Pid},
  Call :: {atom(), atom(), [cauder_types:abstract_expr()]},
  Pid :: cauder_types:proc_id().

parse_lines([], Data) -> Data;
parse_lines([Line | RestLines], Data) ->
  case hd(string:split(Line, " ")) of
    "call" ->
      {match, [Call]} = re:run(Line, "call \"(.+)\"", [{capture, [1], list}]),
      parse_lines(RestLines, Data#{call := parse_call(Call)});
    "main_pid" ->
      {match, [Pid]} = re:run(Line, "main_pid (\\d+)", [{capture, [1], list}]),
      parse_lines(RestLines, Data#{main_pid := list_to_integer(Pid)});
    _ ->
      parse_lines(RestLines, Data)
  end.


-spec parse_call(string()) -> {atom(), atom(), [cauder_types:abstract_expr()]}.

parse_call(Call) ->
  case erl_scan:string(Call ++ ".") of
    {ok, Tokens, _} ->
      case erl_parse:parse_exprs(Tokens) of
        {ok, Exprs} ->
          [{remote_call, _, M, F, As}] = cauder_syntax:expr_list(Exprs),
          {M, F, As};
        _Err -> error({parse_error, Call, Tokens})
      end;
    _Err -> error({parse_error, Call})
  end.


-spec get_log_data(file:filename(), pos_integer()) -> cauder_types:log().

get_log_data(Path, Pid) ->
  PidFile = filename:join(Path, "trace_" ++ integer_to_list(Pid) ++ ".log"),
  {ok, FileHandler} = file:open(PidFile, [read]),
  ReplayProcData = read_replay_proc_data(FileHandler, Pid),
  file:close(FileHandler),
  ReplayProcData.


-spec read_replay_proc_data(file:io_device(), pos_integer()) -> cauder_types:log().

read_replay_proc_data(FileHandler, Pid) -> read_replay_proc_data(FileHandler, Pid, []).


-spec read_replay_proc_data(file:io_device(), pos_integer(), cauder_types:log()) -> cauder_types:log().

read_replay_proc_data(FileHandler, Pid, Data) ->
  case file:read_line(FileHandler) of
    eof -> lists:reverse(Data);
    {ok, Line} ->
      Entry = parse_log_entry(string:chomp(Line), Pid),
      read_replay_proc_data(FileHandler, Pid, [Entry | Data])
  end.


-spec parse_log_entry(string(), pos_integer()) -> cauder_types:log_entry().

parse_log_entry(String, Pid) ->
  case erl_scan:string(String ++ ".") of
    {ok, Tokens, _} ->
      case erl_parse:parse_exprs(Tokens) of
        {ok, Exprs} ->
          [{value, _, {Pid, Action, Id}}] = cauder_syntax:expr_list(Exprs),
          {Action, Id};
        _Err -> error({parse_error, String, Tokens})
      end;
    _Err -> error({parse_error, String})
  end.


-spec current_line(Process) -> non_neg_integer() when
  Process :: cauder_types:process().

current_line(#proc{exprs = [E | _]}) -> element(2, E).
