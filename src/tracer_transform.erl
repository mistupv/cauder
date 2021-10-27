-module(tracer_transform).

%% API
-export([parse_transform/2]).

-ignore_xref([parse_transform/2]).

-spec parse_transform(Forms, Options) -> NewForms when
    Forms :: [erl_parse:abstract_form() | erl_parse:form_info()],
    Options :: [compile:option()],
    NewForms :: [erl_parse:abstract_form() | erl_parse:form_info()].

% TODO: Treat correctly errors to be considered as a value
parse_transform(Forms, Opts) ->
    put(atomic, atomics:new(1, [])),
    put(stamp_mode, proplists:get_value(stamp_mode, Opts)),

    XForms = erl_expand_records:module(Forms, Opts),
    IForms = [erl_syntax_lib:map(fun instrument/1, F) || F <- XForms],
    RForms = erl_syntax:revert_forms(IForms),

    [Module] = [M || {attribute, _, module, M} <- XForms],
    {ok, File} = file:open("./inst_" ++ atom_to_list(Module) ++ ".erl", [write]),
    [io:format(File, "~s", [erl_pp:form(F, [{linewidth, 120}])]) || F <- RForms],

    RForms.

-spec instrument(Node) -> erl_syntax:syntaxTree() when
    Node :: erl_syntax:syntaxTree().

instrument(Node) ->
    case erl_syntax:type(Node) of
        function ->
            atomics:put(get(atomic), 1, 0),
            Node;
        application ->
            Op = erl_syntax:application_operator(Node),
            Args = erl_syntax:application_arguments(Node),
            case erl_syntax:type(Op) of
                module_qualifier ->
                    Module = erl_syntax:atom_value(erl_syntax:module_qualifier_argument(Op)),
                    Function = erl_syntax:atom_value(erl_syntax:module_qualifier_body(Op)),
                    case {Module, Function} of
                        {erlang, send} ->
                            [Target, Message] = erl_syntax:application_arguments(Node),
                            instrument_send(Target, Message);
                        {erlang, nodes} when Args =:= [] ->
                            instrument_nodes();
                        _ ->
                            Node
                    end;
                _ ->
                    Node
            end;
        infix_expr ->
            Op = erl_syntax:operator_name(erl_syntax:infix_expr_operator(Node)),
            case Op of
                '!' ->
                    Target = erl_syntax:infix_expr_left(Node),
                    Message = erl_syntax:infix_expr_right(Node),
                    instrument_send(Target, Message);
                _ ->
                    Node
            end;
        receive_expr ->
            instrument_receive(Node);
        _ ->
            Node
    end.

-spec instrument_send(Destination, Message) -> erl_syntax:syntaxTree() when
    Destination :: erl_syntax:syntaxTree(),
    Message :: erl_syntax:syntaxTree().

instrument_send(Destination, Message) ->
    FunctionName =
        case get(stamp_mode) of
            distributed -> send_distributed;
            centralized -> send_centralized
        end,
    erl_syntax:application(
        erl_syntax:atom(tracer_erlang),
        erl_syntax:atom(FunctionName),
        [Destination, Message]
    ).

-spec instrument_receive(ReceiveExpr) -> erl_syntax:syntaxTree() when
    ReceiveExpr :: erl_syntax:syntaxTree().

instrument_receive(ReceiveExpr) ->
    Clauses = erl_syntax:receive_expr_clauses(ReceiveExpr),
    Timeout = erl_syntax:receive_expr_timeout(ReceiveExpr),
    Action = erl_syntax:receive_expr_action(ReceiveExpr),

    NewClauses = lists:map(fun(Clause) -> instrument_receive_clause(Clause) end, Clauses),

    erl_syntax:receive_expr(NewClauses, Timeout, Action).

-spec instrument_receive_clause(Clause) -> erl_syntax:syntaxTree() when
    Clause :: erl_syntax:syntaxTree().

instrument_receive_clause(Clause) ->
    StampVar = temp_variable(),

    [Pattern] = erl_syntax:clause_patterns(Clause),
    Guard = erl_syntax:clause_guard(Clause),
    Body = erl_syntax:clause_body(Clause),

    ReceiveEvaluated =
        erl_syntax:infix_expr(
            erl_syntax:tuple([
                erl_syntax:atom("undefined"),
                erl_syntax:atom(node())
            ]),
            erl_syntax:operator("!"),
            erl_syntax:tuple([
                erl_syntax:atom('receive'),
                StampVar
            ])
        ),

    erl_syntax:clause(
        [erl_syntax:tuple([erl_syntax:atom(send), StampVar, erl_syntax:underscore(), Pattern])],
        Guard,
        [ReceiveEvaluated | Body]
    ).

-spec instrument_nodes() -> erl_syntax:syntaxTree().

instrument_nodes() ->
    erl_syntax:application(
        erl_syntax:atom(tracer_erlang),
        erl_syntax:atom(nodes),
        []
    ).

-spec temp_variable() -> erl_syntax:syntaxTree().

temp_variable() ->
    Atomic = atomics:add_get(get(atomic), 1, 1),
    erl_syntax:variable(io_lib:format("tmp$^~b", [Atomic])).
