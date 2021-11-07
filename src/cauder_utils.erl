%%%-------------------------------------------------------------------
%%% @doc CauDEr utilities.
%%% @end
%%%-------------------------------------------------------------------

-module(cauder_utils).

-export([fundef_lookup/1]).
-export([find_spawn_log/2, find_spawn_parent/2, find_node_parent/2, find_msg_sender/2, find_msg_receiver/2]).
-export([find_process_with_future_reads/2, find_process_with_failed_spawn/2, process_node/2]).
-export([check_node_name/1]).
-export([string_to_expressions/1]).
-export([filter_options/2]).
-export([temp_variable/1, is_temp_variable_name/1]).
-export([gen_log_nodes/1, gen_log_send/2, gen_log_spawn/1, gen_log_start/1]).
-export([load_trace/1]).
-export([is_dead/1]).
-export([is_conc_item/1]).

-elvis([{elvis_style, god_modules, disable}]).

-include("cauder.hrl").
-include("cauder_process.hrl").
-include("cauder_history.hrl").

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
    Clauses :: cauder_syntax:af_clause_seq().

fundef_lookup({M, F, A}) ->
    case ets:match_object(?APP_DB, {{M, F, A, '_'}, '_'}) of
        [{{M, F, A, Exported}, Cs}] ->
            {Exported, Cs};
        [] ->
            Path = ets:lookup_element(?APP_DB, path, 2),
            File = filename:join(Path, atom_to_list(M) ++ ".erl"),
            case filelib:is_regular(File) of
                true ->
                    {ok, M} = cauder_load:file(File),
                    [{{M, F, A, Exported}, Cs}] = ets:match_object(?APP_DB, {{M, F, A, '_'}, '_'}),
                    {Exported, Cs};
                false ->
                    error
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Searches for the given log entry in the given log map.
%% Returns `{value, Pid}' where `Pid' is the pid of the process whose log
%% contains the given entry, or `false' if the entry is not found.

-spec find_item(LMap, Entry) -> {value, Pid} | false when
    LMap :: cauder_trace:trace(),
    Entry :: cauder_trace:trace_entry_search(),
    Pid :: cauder_process:id().

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
    LogItem :: [cauder_trace:trace_action()],
    Entry :: cauder_trace:trace_entry_search().

compare([], _) -> false;
compare([H | _], H) -> true;
compare([{spawn, {_, Pid}, _} | _], {spawn, {_, Pid}, _}) -> true;
compare([{spawn, {Node, _}, failure} | _], {spawn, {Node, _}, failure}) -> true;
compare([_ | T], Entry) -> compare(T, Entry).

%%------------------------------------------------------------------------------
%% @doc Searches for a process whose log has an entry with the information to
%% spawn the process with the given pid.
%% Returns `{value, Pid}' where `Pid' is the pid of the process whose log
%% contains the aforementioned entry, or `false' if the entry is not found.

-spec find_spawn_parent(LMap, Pid) -> {value, Parent} | false when
    LMap :: cauder_trace:trace(),
    Pid :: cauder_process:id(),
    Parent :: cauder_process:id().

find_spawn_parent(LMap, Pid) ->
    find_item(LMap, {spawn, {'_', Pid}, '_'}).

%%---------------------------------------------------------------------------------
%% @doc Given a pid and a Log map retrieves the log about the spawning of such pid

-spec find_spawn_log(LMap, Pid) -> Log when
    LMap :: cauder_trace:trace(),
    Pid :: cauder_process:id(),
    Log :: cauder_trace:trace_action().

find_spawn_log(LMap, Pid) ->
    {value, Log} = lists:search(
        fun
            ({spawn, {_, SpawnPid}, _}) when Pid =:= SpawnPid -> true;
            (_) -> false
        end,
        lists:merge(maps:values(LMap))
    ),
    Log.

%%------------------------------------------------------------------------------
%% @doc Searches for a process whose log has an entry with the information to
%% start the node with the given name.
%% Returns `{value, Pid}' where `Pid' is the pid of the process whose log
%% contains the aforementioned entry, or `false' if the entry is not found.

-spec find_node_parent(LMap, Node) -> {value, Parent} | false when
    LMap :: cauder_trace:trace(),
    Node :: node(),
    Parent :: cauder_process:id().

find_node_parent(LMap, Node) ->
    find_item(LMap, {start, Node, success}).

%%------------------------------------------------------------------------------
%% @doc Searches for a process whose log has an entry with the information to
%% send the message with the given uid.
%% Returns `{value, Pid}' where `Pid' is the pid of the process whose log
%% contains the aforementioned entry, or `false' if the entry is not found.

-spec find_msg_sender(LMap, Uid) -> {value, Pid} | false when
    LMap :: cauder_trace:trace(),
    Uid :: cauder_mailbox:uid(),
    Pid :: cauder_process:id().

find_msg_sender(LMap, Uid) ->
    find_item(LMap, {send, Uid}).

%%------------------------------------------------------------------------------
%% @doc Searches for a process whose log has an entry with the information to
%% receive the message with the given uid.
%% Returns `{value, Pid}' where `Pid' is the pid of the process whose log
%% contains the aforementioned entry, or `false' if the entry is not found.

-spec find_msg_receiver(LMap, Uid) -> {value, Pid} | false when
    LMap :: cauder_trace:trace(),
    Uid :: cauder_mailbox:uid(),
    Pid :: cauder_process:id().

find_msg_receiver(LMap, Uid) ->
    find_item(LMap, {'receive', Uid}).

%%------------------------------------------------------------------------------
%% @doc Searches for the process(es) that will fail to spawn `Node' and failed because
%% this was already part of the network, by looking at its log

-spec find_process_with_failed_spawn(LMap, Node) -> {value, Process} | false when
    LMap :: cauder_trace:trace(),
    Node :: node(),
    Process :: cauder_process:id().

find_process_with_failed_spawn(LMap, Node) ->
    find_item(LMap, {spawn, {Node, '_'}, failure}).

%%------------------------------------------------------------------------------
%% @doc Searches for the process(es) that will do a read while `Node' was not part of the network

-spec find_process_with_future_reads(LMap, Node) -> {value, Process} | false when
    LMap :: cauder_trace:trace(),
    Node :: node(),
    Process :: cauder_process:id().

find_process_with_future_reads(LMap, Node) ->
    lists:search(
        fun(Key) ->
            L = maps:get(Key, LMap),
            will_always_read(L, Node)
        end,
        maps:keys(LMap)
    ).

%%------------------------------------------------------------------------------
%% @doc Given an atom that represents a node checks that the format is correct.
%% Returns `error' if the format of the atom is not a valid Erlang node name.

-spec check_node_name(NodeName) -> ok | error | not_provided when
    NodeName :: string().

check_node_name([]) ->
    not_provided;
check_node_name(NodeName) ->
    case string:split(NodeName, "@") of
        [Name, Host] when length(Name) > 0, length(Host) > 0 ->
            ok;
        _ ->
            error
    end.

%%------------------------------------------------------------------------------
%% @doc Converts the given string into a list of abstract expressions.
%% Returns `error' if the string does not represent a valid Erlang expression.

-spec string_to_expressions(String) -> Expressions | error when
    String :: string(),
    Expressions :: [cauder_syntax:abstract_expr()].

string_to_expressions([]) ->
    [];
string_to_expressions(String) ->
    case erl_scan:string(String ++ ".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_exprs(Tokens) of
                {ok, Value} -> cauder_syntax:expr_list(Value);
                _ -> error
            end;
        _ ->
            error
    end.

%%------------------------------------------------------------------------------
%% @doc Returns a new list containing only the options whose pid, matches the
%% given pid.

-spec filter_options(Pid, Options1) -> Options2 when
    Pid :: cauder_process:id(),
    Options1 :: [cauder_types:option()],
    Options2 :: [cauder_types:option()].

filter_options(Pid, Options) ->
    lists:filter(fun(Opt) -> Opt#opt.pid =:= Pid end, Options).

%%------------------------------------------------------------------------------
%% @doc Checks whether the process will ever perform a read without `Node'

-spec will_always_read(Log, Node) -> Result when
    Log :: [cauder_trace:trace_action()],
    Node :: node(),
    Result :: boolean().

will_always_read([], _) -> true;
will_always_read([{nodes, Nodes} | _], Node) -> lists:member(Node, Nodes);
will_always_read([_ | RestLog], Node) -> will_always_read(RestLog, Node).

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
    Variable :: cauder_syntax:af_variable().

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
        "k_" ++ _ -> true;
        _ -> false
    end.

%%------------------------------------------------------------------------------
%% @doc Returns a roll log message about rolling the 'nodes()' of the process
%% with the given pid

-spec gen_log_nodes(Pid) -> [Log] when
    Pid :: cauder_process:id(),
    Log :: [string()].

gen_log_nodes(Pid) ->
    [["Roll nodes of", cauder_pp:pid(Pid)]].

%%------------------------------------------------------------------------------
%% @doc Returns a roll log message about spawning a process with the given pid.

-spec gen_log_spawn(Pid) -> [Log] when
    Pid :: cauder_process:id(),
    Log :: [string()].

gen_log_spawn(OtherPid) ->
    [["Roll spawn of ", cauder_pp:pid(OtherPid)]].

%%------------------------------------------------------------------------------
%% @doc Returns a roll log message about starting a node with the given name.

-spec gen_log_start(Node) -> [Log] when
    Node :: node(),
    Log :: [string()].

gen_log_start(Node) ->
    [["Roll start of ", cauder_pp:pp_node(Node)]].

%%------------------------------------------------------------------------------
%% @doc Returns a roll log message about sending a message with the given
%% information.

-spec gen_log_send(Pid, Message) -> [Log] when
    Pid :: cauder_process:id(),
    Message :: cauder_mailbox:message(),
    Log :: [string()].

gen_log_send(Pid, #message{uid = Uid, value = Value, dest = Dest}) ->
    [
        [
            "Roll send from ",
            cauder_pp:pid(Pid),
            " of ",
            cauder_pp:to_string(Value),
            " to ",
            cauder_pp:pid(Dest),
            " (",
            cauder_pp:to_string(Uid),
            ")"
        ]
    ].

%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Load the trace result from the given directory adn returns it.

-spec load_trace(TraceDir) -> TraceResult when
    TraceDir :: file:filename(),
    TraceResult :: cauder_types:trace_result().

load_trace(Dir) ->
    ResultFile = filename:join(Dir, "trace_result.log"),
    {ok, ResultTerms} = file:consult(ResultFile),
    #{
        node := InitialNode,
        pid := InitialPid,
        call := {Mod, Fun, Args},
        tracing := Tracing,
        return := ReturnValue,
        comp := CompTime,
        exec := ExecTime
    } = maps:from_list(ResultTerms),

    Traces =
        filelib:fold_files(
            Dir,
            "trace_\\d+\\.log",
            false,
            fun(File, Acc) ->
                "trace_" ++ StringPid = filename:basename(File, ".log"),
                Pid = list_to_integer(StringPid),
                {ok, Terms0} = file:consult(File),
                find_last_message_uid(Terms0),
                Acc#{Pid => Terms0}
            end,
            maps:new()
        ),

    #trace_result{
        node = InitialNode,
        pid = InitialPid,
        call = {Mod, Fun, Args},
        tracing = Tracing,
        return = ReturnValue,
        comp = CompTime,
        exec = ExecTime,
        traces = Traces
    }.

-spec find_last_message_uid(Terms) -> ok when
    Terms :: [cauder_trace:trace_action()].

find_last_message_uid(Terms) ->
    AllUids = lists:filtermap(
        fun
            ({send, Uid}) -> {true, Uid};
            (_) -> false
        end,
        Terms
    ),
    case AllUids of
        [] ->
            ok;
        _ ->
            true = ets:insert(?APP_DB, {last_uid, lists:max(AllUids)}),
            ok
    end.

%%------------------------------------------------------------------------------
%% @doc Checks whether a process has finished execution or not.

-spec is_dead(Process) -> IsDead when
    Process :: cauder_process:process(),
    IsDead :: boolean().

is_dead(#process{expr = [{value, _, _}], stack = Stk}) -> cauder_stack:is_empty(Stk);
is_dead(#process{}) -> false.

%%------------------------------------------------------------------------------
%% @doc Returns the process node
-spec process_node(Pid, Pool) -> Result when
    Pool :: cauder_pool:pool(),
    Pid :: cauder_process:id(),
    Result :: node() | false.

process_node(Pid, Pool) ->
    case cauder_pool:find(Pid, Pool) of
        {ok, #process{node = Node}} ->
            Node;
        error ->
            false
    end.

-spec is_conc_item(Entry) -> boolean() when
    Entry :: cauder_history:entry().

is_conc_item(#h_tau{}) -> false;
is_conc_item(#h_self{}) -> false;
is_conc_item(#h_node{}) -> false;
is_conc_item(#h_nodes{}) -> true;
is_conc_item(#h_start{}) -> true;
is_conc_item(#h_spawn{}) -> true;
is_conc_item(#h_send{}) -> true;
is_conc_item(#h_receive{}) -> true.
