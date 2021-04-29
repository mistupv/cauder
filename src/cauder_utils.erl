%%%-------------------------------------------------------------------
%%% @doc CauDEr utilities.
%%% @end
%%%-------------------------------------------------------------------

-module(cauder_utils).

-export([fundef_lookup/1]).
-export([find_spawn_log/2, find_spawn_parent/2, find_node_parent/2, find_msg_sender/2, find_msg_receiver/2]).
-export([find_process_with_future_reads/2,find_process_with_spawn/2, find_process_with_failed_spawn/2, find_process_with_start/2, find_process_on_node/2, find_process_with_failed_start/2, find_process_with_read/2, find_process_with_send/2, find_process_with_receive/2, find_process_with_variable/2, process_node/2]).
-export([merge_bindings/2]).
-export([checkNodeName/1]).
-export([stringToMFA/1, stringToExpressions/1]).
-export([filter_options/2]).
-export([fresh_pid/0]).
-export([temp_variable/1, is_temp_variable_name/1]).
-export([gen_log_nodes/1, gen_log_send/2, gen_log_spawn/1, gen_log_start/1, clear_log/1, must_focus_log/1]).
-export([load_replay_data/1]).
-export([current_line/1, is_dead/1]).
-export([is_conc_item/1]).

-include("cauder.hrl").


%%------------------------------------------------------------------------------
%% @doc Searches for the function definition that matches the given <i>MFA</i>.
%%
%% Returns a tuple is returned, where the first element is a boolean literal
%% that specifies whether the function is exported or not, and the second
%% element is the sequence of clauses of the matched function.
%% If a function is not loaded it tries to load it, if it fails the atom `error'
%% is returned.

-spec fundef_lookup(MFA) -> {Exported, Clauses} | error when
  MFA :: mfa(),
  Exported :: boolean(),
  Clauses :: cauder_types:af_clause_seq().

fundef_lookup({M, F, A}) ->
  case ets:match_object(?APP_DB, {{M, F, A, '_'}, '_'}) of
    [{{M, F, A, Exported}, Cs}] -> {Exported, Cs};
    [] ->
      Path = ets:lookup_element(?APP_DB, path, 2),
      File = filename:join(Path, atom_to_list(M) ++ ".erl"),
      case filelib:is_regular(File) of
        true ->
          {ok, M} = cauder_load:file(File),
          [{{M, F, A, Exported}, Cs}] = ets:match_object(?APP_DB, {{M, F, A, '_'}, '_'}),
          {Exported, Cs};
        false -> error
      end
  end.


%%------------------------------------------------------------------------------
%% @doc Searches for the given log entry in the given log map.
%% Returns `{value, Pid}' where `Pid' is the pid of the process whose log
%% contains the given entry, or `false' if the entry is not found.

-spec find_item(LMap, Entry) -> {value, Pid} | false when
  LMap :: cauder_types:log_map(),
  Entry :: cauder_types:log_entry_search(),
  Pid :: cauder_types:proc_id().

find_item(LMap, Entry) ->
  Pair = lists:search(fun({_Pid, Log}) -> compare(Log, Entry) end, maps:to_list(LMap)),
  case Pair of
    {value, {Pid, _}} -> {value, Pid};
    false -> false
  end.

%%------------------------------------------------------------------------------
%% @doc Given two log items returns true if the second one matches the first.
%% The peculiarity of this function is that allows to search for log items
%% while not fully specifying their form, indeed the second item can be
%% called using the wildcard '_' when we are not interested in an element.

-spec compare(LogItem, Entry) -> true | false when
  LogItem :: cauder_types:log(),
  Entry   :: cauder_types:log_entry_search().

compare([], _) -> false;
compare([H|_],H) -> true;
compare([{spawn, {_, _, Pid}}|_], {spawn, {_, _, Pid}}) -> true;
compare([{spawn, {Node, fail, _}}|_], {spawn, {Node, fail, _}}) -> true;
compare([_|T], Entry) -> compare(T,Entry).


%%------------------------------------------------------------------------------
%% @doc Searches for a process whose log has an entry with the information to
%% spawn the process with the given pid.
%% Returns `{value, Pid}' where `Pid' is the pid of the process whose log
%% contains the aforementioned entry, or `false' if the entry is not found.

-spec find_spawn_parent(LMap, Pid) -> {value, Parent} | false when
  LMap   :: cauder_types:log_map(),
  Pid    :: cauder_types:proc_id(),
  Parent :: cauder_types:proc_id().

find_spawn_parent(LMap, Pid) ->
  find_item(LMap, {spawn, {'_', '_', Pid}}).


%%---------------------------------------------------------------------------------
%% @doc Given a pid and a Log map retrieves the log about the spawning of such pid

-spec find_spawn_log(LMap, Pid) -> Log when
    LMap :: cauder_types:log_map(),
    Pid  :: cauder_types:proc_id(),
    Log  :: cauder_types:log_entry().

find_spawn_log(LMap, Pid) ->
  {value, Log} = lists:search(
                   fun
                     ({spawn, {_, _,SpawnPid}}) when Pid =:= SpawnPid -> true;
                     (_) -> false
                             end, lists:merge(maps:values(LMap))),
  Log.



%%------------------------------------------------------------------------------
%% @doc Searches for a process whose log has an entry with the information to
%% start the node with the given name.
%% Returns `{value, Pid}' where `Pid' is the pid of the process whose log
%% contains the aforementioned entry, or `false' if the entry is not found.

-spec find_node_parent(LMap, Node) -> {value, Parent} | false when
    LMap   :: cauder_types:log_map(),
    Node   :: cauder_types:net_node(),
    Parent :: cauder_types:proc_id().

find_node_parent(LMap, Node) ->
  find_item(LMap, {start, {succ, Node}}).


%%------------------------------------------------------------------------------
%% @doc Searches for a process whose log has an entry with the information to
%% send the message with the given uid.
%% Returns `{value, Pid}' where `Pid' is the pid of the process whose log
%% contains the aforementioned entry, or `false' if the entry is not found.

-spec find_msg_sender(LMap, Uid) -> {value, Pid} | false when
  LMap :: cauder_types:log_map(),
  Uid :: cauder_mailbox:uid(),
  Pid :: cauder_types:proc_id().

find_msg_sender(LMap, Uid) ->
  find_item(LMap, {send, Uid}).


%%------------------------------------------------------------------------------
%% @doc Searches for a process whose log has an entry with the information to
%% receive the message with the given uid.
%% Returns `{value, Pid}' where `Pid' is the pid of the process whose log
%% contains the aforementioned entry, or `false' if the entry is not found.

-spec find_msg_receiver(LMap, Uid) -> {value, Pid} | false when
  LMap :: cauder_types:log_map(),
  Uid :: cauder_mailbox:uid(),
  Pid :: cauder_types:proc_id().

find_msg_receiver(LMap, Uid) ->
  find_item(LMap, {'receive', Uid}).


%%------------------------------------------------------------------------------
%% @doc Searches for the process that spawned the process with the given pid, by
%% looking at its history.

-spec find_process_with_spawn(ProcessMap, Pid) -> {value, Process} | false when
  ProcessMap :: cauder_types:process_map(),
  Pid :: cauder_types:proc_id(),
  Process :: cauder_types:process().

find_process_with_spawn(PMap, Pid) ->
  lists:search(fun(#proc{hist = H}) -> has_spawn(H, Pid) end, maps:values(PMap)).


%%------------------------------------------------------------------------------
%% @doc Searches for the process that started the node with the given name, by
%% looking at its history.

-spec find_process_with_start(ProcessMap, Node) -> {value, Process} | false when
    ProcessMap :: cauder_types:process_map(),
    Node :: cauder_types:net_node(),
    Process :: cauder_types:process().

find_process_with_start(PMap, Node) ->
  lists:search(fun(#proc{hist = H}) -> has_start(H, Node) end, maps:values(PMap)).


%%------------------------------------------------------------------------------
%% @doc Searches for the process(es) that tried to start `Node' and failed because
%% this was already part of the network, by looking at its history.

-spec find_process_with_failed_start(ProcessMap, Node) -> {value, Process} | false when
  ProcessMap :: cauder_types:process_map(),
  Node :: cauder_types:net_node(),
  Process :: cauder_types:process().

find_process_with_failed_start(ProcessMap, Node) ->
  lists:search(fun(#proc{hist = H}) -> has_failed_start(H, Node) end, maps:values(ProcessMap)).

%%------------------------------------------------------------------------------
%% @doc Searches for the process(es) that will fail to spawn `Node' and failed because
%% this was already part of the network, by looking at its log

-spec find_process_with_failed_spawn(LMap, Node) -> {value, Process} | false when
    LMap :: cauder_types:log_map(),
    Node :: cauder_types:net_node(),
    Process :: cauder_types:proc_id().

find_process_with_failed_spawn(LMap, Node) ->
  find_item(LMap, {spawn, {Node, fail, '_'}}).

%%------------------------------------------------------------------------------
%% @doc Searches for the process(es) that will do a read while `Node' was not part of the network

-spec find_process_with_future_reads(LMap, Node) -> {value, Process} | false when
    LMap :: cauder_types:log_map(),
    Node :: cauder_types:net_node(),
    Process :: cauder_types:proc_id().

find_process_with_future_reads(LMap, Node) ->
  lists:search(fun(Key) ->
                   L = maps:get(Key, LMap),
                   not will_always_read(L, Node) /= false
               end
              , maps:keys(LMap)).


%%------------------------------------------------------------------------------
%% @doc Searches for process(es) running on `Node'

-spec find_process_on_node(ProcessMap, Node) -> {value, Process} | false when
    ProcessMap :: cauder_types:process_map(),
    Node :: cauder_types:net_node(),
    Process :: cauder_types:process().

find_process_on_node(ProcessMap, Node) ->
  lists:search(fun(#proc{node = ProcNode}) -> ProcNode =:= Node end, maps:values(ProcessMap)).


%%------------------------------------------------------------------------------
%% @doc Searches for process(es) that have performed a read of `Node' by means
%% of the function 'nodes()'

-spec find_process_with_read(ProcessMap, Node) -> {value, Process} | false when
    ProcessMap :: cauder_types:process_map(),
    Node :: cauder_types:net_node(),
    Process :: cauder_types:process().

find_process_with_read(ProcessMap, Node) ->
  lists:search(fun(#proc{hist = H}) -> has_read(H, Node) end, maps:values(ProcessMap)).

%%------------------------------------------------------------------------------
%% @doc Searches for the process that sent the message with the given uid, by
%% looking at its history.

-spec find_process_with_send(ProcessMap, Uid) -> {value, Process} | false when
  ProcessMap :: cauder_types:process_map(),
  Uid :: cauder_mailbox:uid(),
  Process :: cauder_types:process().

find_process_with_send(PMap, Uid) ->
  lists:search(fun(#proc{hist = H}) -> has_send(H, Uid) end, maps:values(PMap)).


%%------------------------------------------------------------------------------
%% @doc Searches for the process that received the message with the given uid,
%% by looking at its history.

-spec find_process_with_receive(ProcessMap, Uid) -> {value, Process} | false when
  ProcessMap :: cauder_types:process_map(),
  Uid :: cauder_mailbox:uid(),
  Process :: cauder_types:process().

find_process_with_receive(PMap, Uid) ->
  lists:search(fun(#proc{hist = H}) -> has_rec(H, Uid) end, maps:values(PMap)).


%%------------------------------------------------------------------------------
%% @doc Searches for the process that defined the variable with the given name,
%% by looking at its history.

-spec find_process_with_variable(ProcessMap, Name) -> {value, Process} | false when
  ProcessMap :: cauder_types:process_map(),
  Name :: atom(),
  Process :: cauder_types:process().

find_process_with_variable(PMap, Name) ->
  lists:search(fun(#proc{env = Bs}) -> maps:is_key(Name, Bs) end, maps:values(PMap)).


%%------------------------------------------------------------------------------
%% @doc Merges the given collections of bindings into a new one.
%% If the same variable is defined in both collections but with different
%% values, then an exception is thrown.

-spec merge_bindings(Bindings1, Bindings2) -> Bindings3 when
  Bindings1 :: cauder_types:environment(),
  Bindings2 :: cauder_types:environment(),
  Bindings3 :: cauder_types:environment().

merge_bindings(Bs1, Bs2) ->
  maps:fold(
    fun
      (K, V, Bs) when not is_map_key(K, Bs) -> Bs#{K => V};
      (K, V, Bs) when map_get(K, Bs) =:= V -> Bs
    end,
    Bs1,
    Bs2
  ).


%%------------------------------------------------------------------------------
%% @doc Converts the given string into a MFA tuple.

-spec stringToMFA(String) -> MFA when
  String :: string(),
  MFA :: mfa().

stringToMFA(String) ->
  [M, F, A] = string:lexemes(String, ":/"),
  {list_to_atom(M), list_to_atom(F), list_to_integer(A)}.

%%------------------------------------------------------------------------------
%% @doc Given an atom that represents a node checks that the format is correct.
%% Returns `error' if the format of the atom is not a valid Erlang node name.

-spec checkNodeName(NodeName) -> ok | error | not_provided when
    NodeName :: cauder_types:net_node().

checkNodeName([]) -> not_provided;
checkNodeName(NodeName) ->
  case string:split(NodeName, "@") of
    [Name, Host] when length(Name) > 0, length(Host) > 0 ->
      ok;
      _ -> error
  end.


%%------------------------------------------------------------------------------
%% @doc Converts the given string into a list of abstract expressions.
%% Returns `error' if the string does not represent a valid Erlang expression.

-spec stringToExpressions(String) -> Expressions | error when
  String :: string(),
  Expressions :: [cauder_types:abstract_expr()].

stringToExpressions([]) -> [];
stringToExpressions(String) ->
  case erl_scan:string(String ++ ".") of
    {ok, Tokens, _} ->
      case erl_parse:parse_exprs(Tokens) of
        {ok, Value} -> cauder_syntax:expr_list(Value);
        _ -> error
      end;
    _ -> error
  end.


%%------------------------------------------------------------------------------
%% @doc Returns a new list containing only the options whose pid, matches the
%% given pid.

-spec filter_options(Options1, Pid) -> Options2 when
  Options1 :: [cauder_types:option()],
  Pid :: cauder_types:proc_id(),
  Options2 :: [cauder_types:option()].

filter_options(Options, Pid) ->
  lists:filter(fun(Opt) -> Opt#opt.pid =:= Pid end, Options).


%%------------------------------------------------------------------------------
%% @doc Checks whether the given history contains a `spawn' entry for the
%% process with the given pid or not.

-spec has_spawn(History, Pid) -> Result when
  History :: cauder_types:history(),
  Pid :: cauder_types:proc_id(),
  Result :: boolean().

has_spawn([], _)                                          -> false;
has_spawn([{spawn, _Bs, _Es, _Stk, _Node, Pid} | _], Pid) -> true;
has_spawn([_ | RestHist], Pid)                            -> has_spawn(RestHist, Pid).


%%------------------------------------------------------------------------------
%% @doc Checks whether the given history contains a `start' entry for the
%% process with the given node or not.

-spec has_start(History, Node) -> Result when
    History :: cauder_types:history(),
    Node :: cauder_types:net_node(),
    Result :: boolean().

has_start([], _)                                              -> false;
has_start([{start, success, _Bs, _Es, _Stk, Node} | _], Node) -> true;
has_start([_ | RestHist], Node)                               -> has_start(RestHist, Node).


%%------------------------------------------------------------------------------
%% @doc Checks whether the given history contains a failed `start' entry for
%% the given node or not.

-spec has_failed_start(History, Node) -> Result when
    History :: cauder_types:history(),
    Node :: cauder_types:net_node(),
    Result :: boolean().

has_failed_start([], _)                                           -> false;
has_failed_start([{start, fail, _Bs, _Es, _Stk, Node} | _], Node) -> true;
has_failed_start([_ | RestHist], Node)                            -> has_failed_start(RestHist, Node).


%%------------------------------------------------------------------------------
%% @doc Checks whether the process will ever perform a read without `Node'

-spec will_always_read(Log, Node) -> Result when
    Log :: cauder_types:history(),
    Node :: cauder_types:net_node(),
    Result :: boolean().

will_always_read([], _)                           -> true;
will_always_read([{nodes, {Nodes}} | _], Node)    -> lists:member(Node, Nodes);
will_always_read([_ | RestLog], Node)             -> will_always_read(RestLog, Node).


%%------------------------------------------------------------------------------
%% @doc Checks whether the given history contains a read of `Node' by checking
%% the history item of the function 'nodes'

-spec has_read(History, Node) -> Result when
    History :: cauder_types:history(),
    Node :: cauder_types:net_node(),
    Result :: boolean().

has_read([], _)                                             -> false;
has_read([{nodes, _Bs, _Es, _Stk, Nodes} | RestHist], Node) ->
  case lists:member(Node, Nodes) of
    true -> true;
    false -> has_read(RestHist, Node)
  end;
has_read([_ | RestHist], Node)                              -> has_read(RestHist, Node).


%%------------------------------------------------------------------------------
%% @doc Checks whether the given history contains a `send' entry for the message
%% with the given uid or not.

-spec has_send(History, Uid) -> Result when
  History :: cauder_types:history(),
  Uid :: cauder_mailbox:uid(),
  Result :: boolean().

has_send([], _)                                                  -> false;
has_send([{send, _Bs, _Es, _Stk, #message{uid = Uid}} | _], Uid) -> true;
has_send([_ | RestHist], Uid)                                    -> has_send(RestHist, Uid).


%%------------------------------------------------------------------------------
%% @doc Checks whether the given history contains a `rec' entry for the message
%% with the given uid or not.

-spec has_rec(History, Uid) -> Result when
  History :: cauder_types:history(),
  Uid :: cauder_mailbox:uid(),
  Result :: boolean().

has_rec([], _)                                                 -> false;
has_rec([{rec, _Bs, _Es, _Stk, #message{uid = Uid}, _QPos} | _], Uid) -> true;
has_rec([_ | RestHist], Uid)                                   -> has_rec(RestHist, Uid).


%%------------------------------------------------------------------------------
%% @doc Returns a new and unique process identifier.

-spec fresh_pid() -> Pid when
  Pid :: cauder_types:proc_id().

fresh_pid() -> ets:update_counter(?APP_DB, last_pid, 1, {last_pid, -1}).


%%------------------------------------------------------------------------------
%% @doc Returns a new and unique number to use as a variable suffix.
%%
%% @see temp_variable/1
%% @todo Merge with temp_variable/1?

-spec fresh_variable_number() -> Number when
  Number :: non_neg_integer().

fresh_variable_number() -> ets:update_counter(?APP_DB, last_var, 1, {last_var, -1}).


%%------------------------------------------------------------------------------
%% @doc Creates a variable with the name `k_<num>' being `<num>' a unique non
%% negative number.
%%
%% This variable is intended to be used as a temporal variable in calls where a
%% resulting value is not known immediately, so when the value is calculated it
%% can replace this variable.
%%
%% @see replace_variable/3

-spec temp_variable(Line) -> Variable when
  Line :: cauder_types:line(),
  Variable :: cauder_types:af_variable().

temp_variable(Line) ->
  Number = fresh_variable_number(),
  Name = "k_" ++ integer_to_list(Number),
  {var, Line, list_to_atom(Name)}.


%%------------------------------------------------------------------------------
%% @doc Checks if a variable is a temporary variable created by CauDEr.
%%
%% @see temp_variable/1

-spec is_temp_variable_name(Name) -> IsTemporary when
  Name :: atom(),
  IsTemporary :: boolean().

is_temp_variable_name(Name) ->
  case atom_to_list(Name) of
    "k_" ++ _ -> true
    ;_ -> false
  end.


%%------------------------------------------------------------------------------
%% @doc Returns a roll log message about rolling the 'nodes()' of the process
%% with the given pid

-spec gen_log_nodes(Pid) -> [Log] when
  Pid :: cauder_types:proc_id(),
  Log :: [string()].

gen_log_nodes(Pid) ->
  [["Roll nodes of", cauder_pp:pid(Pid)]].

%%------------------------------------------------------------------------------
%% @doc Returns a roll log message about spawning a process with the given pid.

-spec gen_log_spawn(Pid) -> [Log] when
  Pid :: cauder_types:proc_id(),
  Log :: [string()].

gen_log_spawn(OtherPid) ->
  [["Roll spawn of ", cauder_pp:pid(OtherPid)]].

%%------------------------------------------------------------------------------
%% @doc Returns a roll log message about starting a node with the given name.

-spec gen_log_start(Node) -> [Log] when
  Node :: cauder_types:net_node(),
  Log :: [string()].

gen_log_start(Node) ->
  [["Roll start of ", cauder_pp:pp_node(Node)]].


%%------------------------------------------------------------------------------
%% @doc Returns a roll log message about sending a message with the given
%% information.

-spec gen_log_send(Pid, Message) -> [Log] when
  Pid :: cauder_types:proc_id(),
  Message :: cauder_mailbox:message(),
  Log :: [string()].

gen_log_send(Pid, #message{uid = Uid, value = Value, dest = Dest}) ->
  [["Roll send from ", cauder_pp:pid(Pid), " of ", cauder_pp:to_string(Value),
    " to ", cauder_pp:pid(Dest), " (", cauder_pp:to_string(Uid), ")"]].


%%------------------------------------------------------------------------------
%% @doc Returns the given system but with an empty roll log.

-spec clear_log(System) -> NewSystem when
  System :: cauder_types:system(),
  NewSystem :: cauder_types:system().

clear_log(System) -> System#sys{roll = []}.


%%------------------------------------------------------------------------------
%% @doc Returns whether the roll log tab must be shown or not.
%%
%% @todo Change name, move and refactor

-spec must_focus_log(System) -> FocusLog when
  System :: cauder_types:system(),
  FocusLog :: boolean().

must_focus_log(System) ->
  Trace = System#sys.roll,
  case Trace of
    [] -> false;
    _ -> true
  end.


%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Load the replay data in the given folder and returns it.

-spec load_replay_data(LogPath) -> ReplayData when
  LogPath :: file:filename(),
  ReplayData :: #replay{}.

load_replay_data(Path) ->
  ResultFile = filename:join(Path, "trace_result.log"),
  {ok, FileHandler} = file:open(ResultFile, [read]),
  Lines = read_lines(FileHandler),
  file:close(FileHandler),
  #{call := Call, main_pid := Pid, main_node := Node} = parse_lines(Lines),
  #replay{log_path = Path, call = Call, main_pid = Pid, main_node = Node}.


%%------------------------------------------------------------------------------
%% @doc Reads all the lines from the given `IoDevice' and returns them.
%% Any trailing `\n' or `\r\n' are removed from each line.

-spec read_lines(IoDevice) -> Lines when
  IoDevice :: file:io_device(),
  Lines :: [string()].

read_lines(File) ->
  case file:read_line(File) of
    eof -> [];
    {ok, Line} -> [string:chomp(Line) | read_lines(File)]
  end.


%%------------------------------------------------------------------------------
%% @doc Parses the given lines (list of strings) and returns the extracted
%% information as a map.

-spec parse_lines(Lines) -> Data when
  Lines :: [string()],
  Data :: #{call := Call, main_pid := Pid, main_node := Node},
  Call :: {module(), atom(), cauder_types:af_args()},
  Pid :: cauder_types:proc_id(),
  Node :: cauder_types:net_node(). 

parse_lines(Lines) ->
  #{call := _, main_pid := _} =
    lists:foldl(
      fun
        ("call" ++ _ = Line, Data) ->
          {match, [Call]} = re:run(Line, "call \"(.+)\"", [{capture, [1], list}]),
          Data#{call => parse_call(Call)};
        ("main_pid" ++ _ = Line, Data) ->
          {match, [Pid]} = re:run(Line, "main_pid (\\d+)", [{capture, [1], list}]),
          Data#{main_pid => list_to_integer(Pid)};
        ("main_node" ++ _ = Line, Data) ->
          {match, [Node]} = re:run(Line, "main_node (.+)", [{capture, [1], list}]),
          Data#{main_node => Node};
        (_, Data) -> Data % TODO result
      end,
      maps:new(),
      Lines
    ).


%%------------------------------------------------------------------------------
%% @doc Parses the given string as a function call.
%% Returns a tuple with the module name, the function name and the list of
%% arguments in abstract form.

-spec parse_call(String) -> Call when
  String :: string(),
  Call :: {module(), atom(), cauder_types:af_args()}.

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


%%------------------------------------------------------------------------------
%% @doc Returns the line of the current expression of the given process.

-spec current_line(Process) -> Line when
  Process :: cauder_types:process(),
  Line :: non_neg_integer().

current_line(#proc{exprs = [E | _]}) -> element(2, E).


%%------------------------------------------------------------------------------
%% @doc Checks whether a process has finished execution or not.

-spec is_dead(Process) -> IsDead when
  Process :: cauder_types:process(),
  IsDead :: boolean().

is_dead(#proc{exprs = [{value, _, _}], stack = []}) -> true;
is_dead(#proc{})                                    -> false.


%%------------------------------------------------------------------------------
%% @doc Returns the process node
-spec process_node(PMap, Pid) -> Result when
  PMap :: cauder_types:process_map(),
  Pid  :: cauder_types:proc_id(),
  Result :: cauder_types:net_node() | false.

process_node(PMap, Pid) ->
  case maps:get(Pid, PMap, false) of
    false -> false;
    Proc -> #proc{node = Node} = Proc,
             Node
  end.

-spec is_conc_item(HistoryEntry) -> IsConcurrent when
  HistoryEntry :: cauder_types:history_entry(),
  IsConcurrent :: boolean().

is_conc_item({tau, _Bs, _Es, _Stk})                   -> false;
is_conc_item({self, _Bs, _Es, _Stk})                  -> false;
is_conc_item({node, _Bs, _Es, _Stk})                  -> false;
is_conc_item({nodes, _Bs, _Es, _Stk, _Nodes})         -> true;
is_conc_item({start, success, _BS, _Es, _Stk, _Node}) -> true;
is_conc_item({start, fail, _BS, _Es, _Stk, _Node})    -> true;
is_conc_item({spawn, _Bs, _Es, _Stk, _Node, _Pid})    -> true;
is_conc_item({send, _Bs, _Es, _Stk, _Msg})            -> true;
is_conc_item({rec, _Bs, _Es, _Stk, _Msg, _QPos})      -> true.


