-module(tracer_transform).

-export([parse_transform/2]).

-ignore_xref([parse_transform/2]).

-include("tracer.hrl").

-spec parse_transform(Forms, Options) -> NewForms when
    Forms :: [erl_parse:abstract_form() | erl_parse:form_info()],
    Options :: [compile:option()],
    NewForms :: [erl_parse:abstract_form() | erl_parse:form_info()].

% TODO: Treat correctly errors to be considered as a value
parse_transform(Forms, Opts) ->
    put(modules_to_instrument, proplists:get_value(inst_mod, Opts)),
    put(cur_dir, proplists:get_value(i, Opts)),
    put(atomic, atomics:new(1, [])),
    put(stamp_mode, proplists:get_value(stamp_mode, Opts)),

    [Module] = [M || {attribute, _, module, M} <- Forms],

    NForms = [erl_syntax_lib:map(fun instrument/1, Form) || Form <- Forms],

    RForms = erl_syntax:revert_forms(NForms),
    {ok, File} = file:open("./inst_" ++ atom_to_list(Module) ++ ".erl", [write]),
    [io:format(File, "~s", [erl_pp:form(RForm, [{linewidth, 120}])]) || RForm <- RForms],
    RForms.

-spec instrument(Tree) -> NewTree when
    NewTree :: erl_syntax:syntaxTree(),
    Tree :: erl_syntax:syntaxTree().

instrument(T) ->
    case erl_syntax:type(T) of
        function ->
            atomics:put(get(atomic), 1, 0),
            T;
        application ->
            Op = erl_syntax:application_operator(T),
            case erl_syntax:type(Op) of
                module_qualifier ->
                    Module = erl_syntax:atom_value(erl_syntax:module_qualifier_argument(Op)),
                    Function = erl_syntax:atom_value(erl_syntax:module_qualifier_body(Op)),
                    case {Module, Function} of
                        {erlang, send} ->
                            [Target, Message] = erl_syntax:application_arguments(T),
                            instrument_send(Target, Message);
                        _ ->
                            T
                    end;
                _ ->
                    T
            end;
        infix_expr ->
            Op = erl_syntax:operator_name(erl_syntax:infix_expr_operator(T)),
            case Op of
                '!' ->
                    Target = erl_syntax:infix_expr_left(T),
                    Message = erl_syntax:infix_expr_right(T),
                    instrument_send(Target, Message);
                _ ->
                    T
            end;
        receive_expr ->
            instrument_receive(T);
        _ ->
            T
    end.

-spec instrument_send(Target, Message) -> Expr when
    Target :: erl_syntax:syntaxTree(),
    Message :: erl_syntax:syntaxTree(),
    Expr :: erl_syntax:syntaxTree().

instrument_send(Target, Message) ->
    StampVar = temp_variable(),
    MessageVar = temp_variable(),

    SendSent = send_expr(?SEND_SENT),

    RecvStamp =
        erl_syntax:receive_expr([
            erl_syntax:clause(
                [erl_syntax:tuple([erl_syntax:atom(?RECV_STAMP), StampVar])],
                [],
                [erl_syntax:atom("ok")]
            )
        ]),

    SendWithStamp =
        erl_syntax:infix_expr(
            Target,
            erl_syntax:operator("!"),
            erl_syntax:tuple([
                erl_syntax:tuple([erl_syntax:atom(stamp), StampVar]),
                erl_syntax:match_expr(MessageVar, Message)
            ])
        ),

    erl_syntax:block_expr([SendSent, RecvStamp, SendWithStamp, MessageVar]).

-spec instrument_receive(ReceiveExpr) -> NewReceiveExpr when
    ReceiveExpr :: erl_syntax:syntaxTree(),
    NewReceiveExpr :: erl_syntax:syntaxTree().

instrument_receive(ReceiveExpr) ->
    Clauses = erl_syntax:receive_expr_clauses(ReceiveExpr),
    Timeout = erl_syntax:receive_expr_timeout(ReceiveExpr),
    Action = erl_syntax:receive_expr_action(ReceiveExpr),

    Clauses1 = lists:map(fun instrument_receive_clause/1, Clauses),

    erl_syntax:receive_expr(Clauses1, Timeout, Action).

-spec instrument_receive_clause(Clause) -> NewClause when
    Clause :: erl_syntax:syntaxTree(),
    NewClause :: erl_syntax:syntaxTree().

instrument_receive_clause(Clause) ->
    StampVar = temp_variable(),

    [Pattern] = erl_syntax:clause_patterns(Clause),
    Guard = erl_syntax:clause_guard(Clause),
    Body = erl_syntax:clause_body(Clause),

    ReceiveEvaluated = send_expr(?RECEIVE_EVALUATED, StampVar),

    erl_syntax:clause(
        [erl_syntax:tuple([(erl_syntax:tuple([erl_syntax:atom(stamp), StampVar])), Pattern])],
        Guard,
        [ReceiveEvaluated | Body]
    ).

-spec temp_variable() -> VarExpr when
    VarExpr :: erl_syntax:syntaxTree().

temp_variable() ->
    Atomic = atomics:add_get(get(atomic), 1, 1),
    erl_syntax:variable(io_lib:format("tmp$^~b", [Atomic])).

-spec send_expr(Key) -> SendExpr when
    Key :: atom(),
    SendExpr :: erl_syntax:syntaxTree().

send_expr(Key) -> send_expr(Key, erl_syntax:nil()).

-spec send_expr(Key, Value) -> SendExpr when
    Key :: atom(),
    Value :: erl_syntax:syntaxTree(),
    SendExpr :: erl_syntax:syntaxTree().
send_expr(Key, Value) ->
    erl_syntax:infix_expr(
        erl_syntax:tuple([
            erl_syntax:atom("undefined"),
            erl_syntax:atom(node())
        ]),
        erl_syntax:operator("!"),
        erl_syntax:tuple([erl_syntax:atom(Key), Value])
    ).
