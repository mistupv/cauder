%%%-------------------------------------------------------------------
%%% @doc CauDEr utilities.
%%% @end
%%%-------------------------------------------------------------------

-module(cauder_utils).

-export([fundef_lookup/1]).
-export([check_node_name/1]).
-export([string_to_expressions/1]).
-export([temp_variable/1, is_temp_variable_name/1]).
-export([gen_log_nodes/1, gen_log_send/2, gen_log_spawn/1, gen_log_start/1]).
-export([load_trace/1]).

-include("cauder.hrl").
-include("cauder_message.hrl").
-include("cauder_trace.hrl").
-include("cauder_tracer.hrl").

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
    Line :: cauder_syntax:line(),
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
    Message :: cauder_message:message(),
    Log :: [string()].

gen_log_send(Pid, #message{uid = Uid, dst = Dst, val = Val}) ->
    [
        [
            "Roll send from ",
            cauder_pp:pid(Pid),
            " of ",
            cauder_pp:to_string(Val),
            " to ",
            cauder_pp:pid(Dst),
            " (",
            cauder_pp:to_string(Uid),
            ")"
        ]
    ].

%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Load the trace result from the given directory adn returns it.

-spec load_trace(TraceDir) -> TraceInfo when
    TraceDir :: file:filename(),
    TraceInfo :: cauder_tracer:trace_info().

load_trace(Dir) ->
    ResultFile = filename:join(Dir, "trace_result.log"),
    {ok, ResultTerms} = file:consult(ResultFile),
    #{
        node := InitialNode,
        pid := InitialPid,
        call := {Mod, Fun, Args},
        tracing := Tracing,
        return := ReturnBinary,
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

    ReturnValue =
        case ReturnBinary of
            none -> none;
            _ -> {value, erlang:binary_to_term(ReturnBinary)}
        end,

    #trace_info{
        node = InitialNode,
        pid = InitialPid,
        call = {Mod, Fun, Args},
        tracing = Tracing,
        return = ReturnValue,
        comp = CompTime,
        exec = ExecTime,
        trace = Traces
    }.

-spec find_last_message_uid(Terms) -> ok when
    Terms :: [cauder_trace:action()].

find_last_message_uid(Terms) ->
    AllUids = lists:filtermap(
        fun
            (#trace_send{uid = Uid}) -> {true, Uid};
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
