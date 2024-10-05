%%%-----------------------------------------------------------------------------
%%% @doc The CauDEr meta interpreter.
%%% This module provides an interpreter for Erlang expressions.
%%% The expressions are in the abstract syntax as returned by `cauder_syntax'.
%%% @see erl_eval
%%% @end
%%%-----------------------------------------------------------------------------

-module(cauder_eval).

%% API
-export([exprs/3]).
-export([is_value/1, is_reducible/2]).
-export([match_receive_pid/6, match_receive_uid/4]).

-include("cauder_message.hrl").
-include("cauder_stack.hrl").
-include("cauder_eval.hrl").

-export_type([result/0, label/0]).

-type result() :: #result{}.

-type label() ::
    label_tau()
    | label_self()
    | label_node()
    | label_nodes()
    | label_start()
    | label_spawn_fun()
    | label_spawn_mfa()
    | label_send()
    | label_receive().

-type label_tau() :: #label_tau{}.
-type label_self() :: #label_self{}.
-type label_node() :: #label_node{}.
-type label_nodes() :: #label_nodes{}.
-type label_start() :: #label_start{}.
-type label_spawn_fun() :: #label_spawn_fun{}.
-type label_spawn_mfa() :: #label_spawn_mfa{}.
-type label_send() :: #label_send{}.
-type label_receive() :: #label_receive{}.

-type expression() :: cauder_syntax:abstract_expr().
-type expressions() :: [cauder_syntax:abstract_expr()].
-type expression_list() :: [expression()].
-type clauses() :: [cauder_syntax:af_clause()].

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Evaluates the given sequence of expression.
%%
%% If the first expression in the sequence is reducible, then it is evaluated,
%% otherwise it is consumed, given an environment and a call stack.
%% Also if the first expression is not reducible, and there are no more
%% expressions in the sequence, then consumes the first element in the call
%% stack and retrieves the stored information.
%% Returns a record with the updated information and a label indicating the type
%% of step performed.
%%
%% @see is_reducible/2

-spec exprs(Expressions, Bindings, Stack) -> Result when
    Expressions :: expressions(),
    Bindings :: cauder_bindings:bindings(),
    Stack :: cauder_stack:stack(),
    Result :: cauder_eval:result().

exprs([E | Es], Bs, Stk) ->
    case is_reducible(E, Bs) of
        false ->
            case Es of
                [] ->
                    Line = element(2, E),
                    {Entry, Stk1} = cauder_stack:pop(Stk),
                    case Entry of
                        {value, #s_function{env = Bs1, expr = Es1, var = Var}} ->
                            Es2 = cauder_syntax:replace_variable(
                                Es1,
                                setelement(2, Var, Line),
                                cauder_syntax:concrete(E)
                            ),
                            #result{env = Bs1, expr = Es2, stack = Stk1};
                        {value, #s_block{expr = Es1, var = Var}} ->
                            Es2 = cauder_syntax:replace_variable(
                                Es1,
                                setelement(2, Var, Line),
                                cauder_syntax:concrete(E)
                            ),
                            #result{env = Bs, expr = Es2, stack = Stk1}
                    end;
                _ ->
                    #result{env = Bs, expr = Es, stack = Stk}
            end;
        true ->
            #result{env = Bs1, expr = Es1, stack = Stk1} = Result = expr(E, Bs, Stk),
            case cauder_stack:pop(Stk1) of
                {{value, #s_function{env = Bs2, expr = Es2} = Entry}, Stk} ->
                    Entry1 = Entry#s_function{env = Bs1, expr = Es1 ++ Es},
                    Result#result{
                        env = Bs2,
                        expr = Es2,
                        stack = cauder_stack:push(Entry1, Stk)
                    };
                {{value, #s_block{expr = Es2} = Entry}, Stk} ->
                    Entry1 = Entry#s_block{expr = Es1 ++ Es},
                    Result#result{
                        expr = Es2,
                        stack = cauder_stack:push(Entry1, Stk)
                    };
                {{value, _}, _} ->
                    Result#result{
                        expr = Es1 ++ Es
                    }
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Evaluates the given `Expression' and returns a tuple with an updated
%% environment, the expression that resulted from the evaluation, and a label.

-spec expr(Expression, Bindings, Stack) -> Result when
    Expression :: expression(),
    Bindings :: cauder_bindings:bindings(),
    Stack :: cauder_stack:stack(),
    Result :: cauder_eval:result().

expr({var, Line, Name}, Bs, Stk) ->
    {ok, Value} = cauder_bindings:find(Name, Bs),
    #result{env = Bs, expr = [{value, Line, Value}], stack = Stk};
expr(E = {cons, Line, H0, T0}, Bs, Stk) ->
    case is_reducible(H0, Bs) of
        true ->
            R = #result{expr = [H]} = expr(H0, Bs, Stk),
            case is_value(H) andalso is_value(T0) of
                true -> R#result{expr = [{value, Line, [cauder_syntax:concrete(H) | cauder_syntax:concrete(T0)]}]};
                false -> R#result{expr = [setelement(3, E, H)]}
            end;
        false ->
            case is_reducible(T0, Bs) of
                true ->
                    R = #result{expr = [T]} = expr(T0, Bs, Stk),
                    case is_value(H0) andalso is_value(T) of
                        true ->
                            R#result{expr = [{value, Line, [cauder_syntax:concrete(H0) | cauder_syntax:concrete(T)]}]};
                        false ->
                            R#result{expr = [setelement(4, E, T)]}
                    end;
                false ->
                    #result{
                        env = Bs,
                        expr = [{value, Line, [cauder_syntax:concrete(H0) | cauder_syntax:concrete(T0)]}],
                        stack = Stk
                    }
            end
    end;
expr(E = {tuple, Line, Es0}, Bs, Stk) ->
    R = #result{expr = Es} = expr_list(Es0, Bs, Stk),
    case is_value(Es) of
        true ->
            Tuple = list_to_tuple(lists:map(fun cauder_syntax:concrete/1, Es)),
            #result{env = Bs, expr = [{value, Line, Tuple}], stack = Stk};
        false ->
            R#result{expr = [setelement(3, E, Es)]}
    end;
expr({'if', Line, Cs}, Bs, Stk0) ->
    Body = if_clauses(Cs, Bs),
    Var = cauder_utils:temp_variable(Line),
    Entry = #s_block{type = 'if', expr = Body, var = Var},
    Stk = cauder_stack:push(Entry, Stk0),
    #result{env = Bs, expr = [Var], stack = Stk};
expr(E = {'case', Line, A, Cs}, Bs0, Stk0) ->
    case is_reducible(A, Bs0) of
        true ->
            eval_and_update(3, E, Bs0, Stk0);
        false ->
            {Body, Bs} = case_clauses(Cs, A, Bs0),
            Var = cauder_utils:temp_variable(Line),
            Entry = #s_block{type = 'case', expr = Body, var = Var},
            Stk = cauder_stack:push(Entry, Stk0),
            #result{env = Bs, expr = [Var], stack = Stk}
    end;
%% TODO Support receive with timeout
expr({'receive', Line, Cs}, Bs, Stk0) ->
    % TODO One of these variables is not necessary
    Var = cauder_utils:temp_variable(Line),
    VarBody = cauder_utils:temp_variable(Line),
    Entry = #s_block{type = 'receive', expr = [VarBody], var = Var},
    Stk = cauder_stack:push(Entry, Stk0),
    Label = #label_receive{var = VarBody, clauses = Cs},
    #result{env = Bs, expr = [Var], stack = Stk, label = Label};
% TODO Support fun() as entry point argument?
% TODO Handle calls to interpreted fun() from uninterpreted module
expr({'make_fun', Line, Name, Cs}, Bs, Stk0) ->
    {ok, M} = cauder_stack:current_module(Stk0),
    Arity = length(element(3, hd(Cs))),
    Info = {{M, Name}, Bs, Cs},
    Fun =
        case Arity of
            0 -> fun() -> {[], Info} end;
            1 -> fun(A) -> {[A], Info} end;
            2 -> fun(A, B) -> {[A, B], Info} end;
            3 -> fun(A, B, C) -> {[A, B, C], Info} end;
            4 -> fun(A, B, C, D) -> {[A, B, C, D], Info} end;
            5 -> fun(A, B, C, D, E) -> {[A, B, C, D, E], Info} end;
            % TODO Support more arities
            _ -> error({argument_limit, Arity})
        end,
    #result{env = Bs, expr = [{value, Line, Fun}], stack = Stk0};
expr(E = {bif, Line, M, F, As}, Bs, Stk) ->
    case is_reducible(As, Bs) of
        true ->
            eval_and_update(5, E, Bs, Stk);
        false ->
            Value = apply(M, F, lists:map(fun cauder_syntax:concrete/1, As)),
            #result{env = Bs, expr = [{value, Line, Value}], stack = Stk}
    end;
expr({self, Line}, Bs, Stk) ->
    Var = cauder_utils:temp_variable(Line),
    Label = #label_self{var = Var},
    #result{env = Bs, expr = [Var], stack = Stk, label = Label};
expr({node, Line}, Bs, Stk) ->
    Var = cauder_utils:temp_variable(Line),
    Label = #label_node{var = Var},
    #result{env = Bs, expr = [Var], stack = Stk, label = Label};
expr({nodes, Line}, Bs, Stk) ->
    Var = cauder_utils:temp_variable(Line),
    Label = #label_nodes{var = Var},
    #result{env = Bs, expr = [Var], stack = Stk, label = Label};
expr(E = {spawn, Line, Fun}, Bs, Stk) ->
    case is_reducible(Fun, Bs) of
        true ->
            eval_and_update(3, E, Bs, Stk);
        false ->
            Var = cauder_utils:temp_variable(Line),
            Label = #label_spawn_fun{var = Var, function = Fun},
            #result{env = Bs, expr = [Var], stack = Stk, label = Label}
    end;
expr(E = {spawn, Line, N, Fun}, Bs, Stk) ->
    case is_reducible(N, Bs) of
        true ->
            eval_and_update(3, E, Bs, Stk);
        false ->
            case is_reducible(Fun, Bs) of
                true ->
                    eval_and_update(4, E, Bs, Stk);
                false ->
                    Var = cauder_utils:temp_variable(Line),
                    Label = #label_spawn_fun{var = Var, node = cauder_syntax:concrete(N), function = Fun},
                    #result{env = Bs, expr = [Var], stack = Stk, label = Label}
            end
    end;
expr(E = {spawn, Line, M, F, As}, Bs, Stk) ->
    case is_reducible(M, Bs) of
        true ->
            eval_and_update(3, E, Bs, Stk);
        false ->
            case is_reducible(F, Bs) of
                true ->
                    eval_and_update(4, E, Bs, Stk);
                false ->
                    case is_reducible(As, Bs) of
                        true ->
                            eval_and_update(5, E, Bs, Stk);
                        false ->
                            Var = cauder_utils:temp_variable(Line),
                            Label = #label_spawn_mfa{
                                var = Var,
                                module = cauder_syntax:concrete(M),
                                function = cauder_syntax:concrete(F),
                                args = cauder_syntax:concrete(As)
                            },
                            #result{env = Bs, expr = [Var], stack = Stk, label = Label}
                    end
            end
    end;
expr(E = {spawn, Line, N, M, F, As}, Bs, Stk) ->
    case is_reducible(N, Bs) of
        true ->
            eval_and_update(3, E, Bs, Stk);
        false ->
            case is_reducible(M, Bs) of
                true ->
                    eval_and_update(4, E, Bs, Stk);
                false ->
                    case is_reducible(F, Bs) of
                        true ->
                            eval_and_update(5, E, Bs, Stk);
                        false ->
                            case is_reducible(As, Bs) of
                                true ->
                                    eval_and_update(6, E, Bs, Stk);
                                false ->
                                    Var = cauder_utils:temp_variable(Line),
                                    Label = #label_spawn_mfa{
                                        var = Var,
                                        node = cauder_syntax:concrete(N),
                                        module = cauder_syntax:concrete(M),
                                        function = cauder_syntax:concrete(F),
                                        args = cauder_syntax:concrete(As)
                                    },
                                    #result{env = Bs, expr = [Var], stack = Stk, label = Label}
                            end
                    end
            end
    end;
expr(E = {start, Line, N}, Bs, Stk) ->
    case is_reducible(N, Bs) of
        true ->
            eval_and_update(3, E, Bs, Stk);
        false ->
            Var = cauder_utils:temp_variable(Line),
            Label = #label_start{var = Var, name = cauder_syntax:concrete(N)},
            #result{env = Bs, expr = [Var], stack = Stk, label = Label}
    end;
expr(E = {start, Line, H, N}, Bs, Stk) ->
    case is_reducible(H, Bs) of
        true ->
            eval_and_update(3, E, Bs, Stk);
        false ->
            case is_reducible(N, Bs) of
                true ->
                    eval_and_update(4, E, Bs, Stk);
                false ->
                    Var = cauder_utils:temp_variable(Line),
                    Label = #label_start{var = Var, name = cauder_syntax:concrete(N), host = cauder_syntax:concrete(H)},
                    #result{env = Bs, expr = [Var], stack = Stk, label = Label}
            end
    end;
expr(E = {Send, _, L, R}, Bs, Stk) when Send =:= 'send' orelse Send =:= 'send_op' ->
    case is_reducible(L, Bs) of
        true ->
            eval_and_update(3, E, Bs, Stk);
        false ->
            case is_reducible(R, Bs) of
                true ->
                    eval_and_update(4, E, Bs, Stk);
                false ->
                    Label = #label_send{dst = cauder_syntax:concrete(L), val = cauder_syntax:concrete(R)},
                    #result{env = Bs, expr = [R], stack = Stk, label = Label}
            end
    end;
expr(E = {local_call, Line, F, As}, Bs0, Stk0) ->
    case is_reducible(As, Bs0) of
        true ->
            eval_and_update(4, E, Bs0, Stk0);
        false ->
            {ok, M} = cauder_stack:current_module(Stk0),
            A = length(As),
            {_, Cs} = cauder_utils:fundef_lookup({M, F, A}),
            {Body, Bs} = match_fun(Cs, As),
            Var = cauder_utils:temp_variable(Line),
            Entry = #s_function{mfa = {M, F, A}, env = Bs, expr = Body, var = Var},
            Stk = cauder_stack:push(Entry, Stk0),
            #result{env = Bs0, expr = [Var], stack = Stk}
    end;
expr(E = {remote_call, Line, M, F, As}, Bs0, Stk0) ->
    case is_reducible(As, Bs0) of
        true -> eval_and_update(5, E, Bs0, Stk0);
        false -> remote_call(M, F, As, Stk0, Line, Bs0)
    end;
% TODO Handle calls to self/0, spawn/1, spawn/3
expr(E = {apply, Line, M0, F0, As}, Bs0, Stk0) ->
    case is_reducible(M0, Bs0) of
        true ->
            eval_and_update(3, E, Bs0, Stk0);
        false ->
            case is_reducible(F0, Bs0) of
                true ->
                    eval_and_update(4, E, Bs0, Stk0);
                false ->
                    case is_reducible(As, Bs0) of
                        true ->
                            eval_and_update(5, E, Bs0, Stk0);
                        false ->
                            M = cauder_syntax:concrete(M0),
                            F = cauder_syntax:concrete(F0),
                            remote_call(M, F, As, Stk0, Line, Bs0)
                    end
            end
    end;
expr(E = {apply_fun, Line, Fun, As}, Bs0, Stk0) ->
    case is_reducible(Fun, Bs0) of
        true ->
            eval_and_update(3, E, Bs0, Stk0);
        false ->
            case is_reducible(As, Bs0) of
                true ->
                    eval_and_update(4, E, Bs0, Stk0);
                false ->
                    A = length(As),
                    {env, [{{M, F}, Bs1, Cs}]} = erlang:fun_info(cauder_syntax:concrete(Fun), env),
                    {Body, Bs2} = match_fun(Cs, As),
                    Var = cauder_utils:temp_variable(Line),
                    Bs = cauder_bindings:merge(Bs1, Bs2),
                    Entry = #s_function{mfa = {M, F, A}, env = Bs, expr = Body, var = Var},
                    Stk = cauder_stack:push(Entry, Stk0),
                    #result{env = Bs0, expr = [Var], stack = Stk}
            end
    end;
expr(E = {match, _, Lhs, Rhs}, Bs0, Stk) ->
    case is_reducible(Lhs, Bs0) of
        true ->
            eval_and_update(3, E, Bs0, Stk);
        false ->
            case is_reducible(Rhs, Bs0) of
                true ->
                    eval_and_update(4, E, Bs0, Stk);
                false ->
                    Val = cauder_syntax:concrete(Rhs),
                    case match(Lhs, Val, Bs0) of
                        {match, Bs} -> #result{env = Bs, expr = [Rhs], stack = Stk};
                        nomatch -> error({badmatch, Val})
                    end
            end
    end;
expr(E = {op, Line, Op, As}, Bs, Stk) ->
    case is_reducible(As, Bs) of
        true ->
            eval_and_update(4, E, Bs, Stk);
        false ->
            Value = apply(erlang, Op, lists:map(fun cauder_syntax:concrete/1, As)),
            #result{env = Bs, expr = [{value, Line, Value}], stack = Stk}
    end;
expr(E = {'andalso', Line, Lhs, Rhs}, Bs, Stk) ->
    case is_reducible(Lhs, Bs) of
        true ->
            eval_and_update(3, E, Bs, Stk);
        false ->
            case Lhs of
                {value, _, false} ->
                    #result{env = Bs, expr = [Lhs], stack = Stk};
                {value, _, true} ->
                    case is_reducible(Rhs, Bs) of
                        true ->
                            eval_and_update(4, E, Bs, Stk);
                        false ->
                            Value = apply(erlang, 'and', [cauder_syntax:concrete(Lhs), cauder_syntax:concrete(Rhs)]),
                            #result{env = Bs, expr = [{value, Line, Value}], stack = Stk}
                    end
            end
    end;
expr(E = {'orelse', Line, Lhs, Rhs}, Bs, Stk) ->
    case is_reducible(Lhs, Bs) of
        true ->
            eval_and_update(3, E, Bs, Stk);
        false ->
            case Lhs of
                {value, _, true} ->
                    #result{env = Bs, expr = [Lhs], stack = Stk};
                {value, _, false} ->
                    case is_reducible(Rhs, Bs) of
                        true ->
                            eval_and_update(4, E, Bs, Stk);
                        false ->
                            Value = apply(erlang, 'or', [cauder_syntax:concrete(Lhs), cauder_syntax:concrete(Rhs)]),
                            #result{env = Bs, expr = [{value, Line, Value}], stack = Stk}
                    end
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Evaluates the first reducible expression from the given list.
%%
%% Evaluates the first reducible expression from the given list of expressions,
%% given an environment and a call stack, then returns a record with the updated
%% information and a label indicating the type of step performed.
%%
%% @see is_reducible/2

-spec expr_list(ExpressionList, Bindings, Stack) -> Result when
    ExpressionList :: expression_list(),
    Bindings :: cauder_bindings:bindings(),
    Stack :: cauder_stack:stack(),
    Result :: cauder_eval:result().

expr_list([E | Es], Bs, Stk) ->
    case is_reducible(E, Bs) of
        true ->
            R = #result{expr = Es1} = expr(E, Bs, Stk),
            R#result{expr = Es1 ++ Es};
        false ->
            R = #result{expr = Es1} = expr_list(Es, Bs, Stk),
            R#result{expr = [E | Es1]}
    end.

remote_call(M, F, As, Stk0, Line, Bs0) ->
    A = length(As),
    case cauder_utils:fundef_lookup({M, F, A}) of
        {Exported, Cs} ->
            % Check if function is accessible
            case cauder_stack:current_module(Stk0) of
                {ok, M} -> ok;
                {ok, _} -> true = Exported;
                error when Stk0 =:= [] -> ok
            end,
            {Body, Bs} = match_fun(Cs, As),
            Var = cauder_utils:temp_variable(Line),
            Entry = #s_function{mfa = {M, F, A}, env = Bs, expr = Body, var = Var},
            Stk = cauder_stack:push(Entry, Stk0),
            #result{env = Bs0, expr = [Var], stack = Stk};
        error ->
            Value = apply(M, F, lists:map(fun cauder_syntax:concrete/1, As)),
            #result{env = Bs0, expr = [{value, Line, Value}], stack = Stk0}
    end.

%%%=============================================================================

-spec if_clauses(Clauses, Bindings) -> Body when
    Clauses :: clauses(),
    Bindings :: cauder_bindings:bindings(),
    Body :: cauder_syntax:af_body().

if_clauses([{'clause', _, [], G, B} | Cs], Bs) ->
    case guard_seq(G, Bs) of
        true ->
            B;
        false ->
            if_clauses(Cs, Bs)
    end;
if_clauses([], _Bs) ->
    erlang:error('if_clause').

-spec case_clauses(Clauses, Value, Bindings) -> {Body, ScopeBindings} | nomatch when
    Clauses :: clauses(),
    Value :: term(),
    Bindings :: cauder_bindings:bindings(),
    Body :: cauder_syntax:af_body(),
    ScopeBindings :: cauder_bindings:bindings().

case_clauses(Cs, Val, Bs) ->
    case match_clause(Cs, [Val], Bs) of
        {B, Bs1} ->
            {B, Bs1};
        nomatch ->
            erlang:error({'case_clause', Val})
    end.

-spec receive_clauses(Clauses, Message, Bindings) -> {Body, NewBindings} | nomatch when
    Clauses :: clauses(),
    Message :: cauder_message:message(),
    Bindings :: cauder_bindings:bindings(),
    Body :: cauder_syntax:af_body(),
    NewBindings :: cauder_bindings:bindings().

receive_clauses(Cs, #message{val = Val}, Bs) ->
    match_clause(Cs, [Val], Bs).

-spec match_fun(Clauses, ValueList) -> {Body, ScopeBindings} | nomatch when
    Clauses :: clauses(),
    ValueList :: [term()],
    Body :: cauder_syntax:af_body(),
    ScopeBindings :: cauder_bindings:bindings().

match_fun(Cs, Vals) ->
    match_clause(Cs, Vals, cauder_bindings:new()).

-spec match_receive_pid(Clauses, RecipientPid, Mail, Bindings, Sched, Sys) ->
    {Body, {Message, QueuePosition}, NewMail, NewBindings} | nomatch
when
    Clauses :: clauses(),
    RecipientPid :: cauder_process:id(),
    Mail :: cauder_mailbox:mailbox(),
    Bindings :: cauder_bindings:bindings(),
    Sched :: cauder_message:scheduler(),
    Sys :: cauder_system:system(),
    Body :: cauder_syntax:af_body(),
    Message :: cauder_message:message(),
    QueuePosition :: pos_integer(),
    NewMail :: cauder_mailbox:mailbox(),
    NewBindings :: cauder_bindings:bindings().

match_receive_pid(Cs, Pid, Mail, Bs, Sched, Sys) ->
    case cauder_mailbox:find_destination(Pid, Mail) of
        [] ->
            nomatch;
        QueueList ->
            case Sched of
                ?SCHEDULER_Manual ->
                    FoldQueue =
                        fun(Msg, Map1) ->
                            case receive_clauses(Cs, #message{uid = Uid} = Msg, Bs) of
                                {Body, Bs1} -> maps:put(Uid, {Bs1, Body, Msg}, Map1);
                                nomatch -> skip
                            end
                        end,
                    FoldQueueList = fun(Queue, Map0) -> lists:foldl(FoldQueue, Map0, queue:to_list(Queue)) end,
                    MatchingBranchesMap = lists:foldl(FoldQueueList, maps:new(), QueueList),
                    case maps:size(MatchingBranchesMap) of
                        0 ->
                            nomatch;
                        _ ->
                            MatchingMessages = lists:map(fun({_, _, Msg}) -> Msg end, maps:values(MatchingBranchesMap)),
                            case cauder:suspend_task(Pid, MatchingMessages, Sys) of
                                % TODO Use suspend time
                                {_SuspendTime, {resume, Uid}} ->
                                    cauder:resume_task(),
                                    {Bs1, Body, Msg} = maps:get(Uid, MatchingBranchesMap),
                                    {QPos, NewMail} = cauder_mailbox:remove(Msg, Mail),
                                    {Body, {Msg, QPos}, NewMail, Bs1};
                                % TODO Use suspend time
                                {_SuspendTime, cancel} ->
                                    throw(cancel)
                            end
                    end;
                ?SCHEDULER_Random ->
                    MatchingBranches = lists:filtermap(
                        fun(Queue) ->
                            {value, Msg} = queue:peek(Queue),
                            case receive_clauses(Cs, Msg, Bs) of
                                {Body, Bs1} -> {true, {Bs1, Body, Msg}};
                                nomatch -> false
                            end
                        end,
                        QueueList
                    ),
                    case length(MatchingBranches) of
                        0 ->
                            nomatch;
                        Length ->
                            {Bs1, Body, Msg} = lists:nth(rand:uniform(Length), MatchingBranches),
                            {QPos, NewMail} = cauder_mailbox:remove(Msg, Mail),
                            {Body, {Msg, QPos}, NewMail, Bs1}
                    end
            end
    end.

-spec match_receive_uid(Clauses, Uid, Mail, Bindings) ->
    {Body, {Message, QueuePosition}, NewMail, NewBindings} | nomatch
when
    Clauses :: clauses(),
    Uid :: cauder_message:uid(),
    Mail :: cauder_mailbox:mailbox(),
    Bindings :: cauder_bindings:bindings(),
    Body :: cauder_syntax:af_body(),
    Message :: cauder_message:message(),
    QueuePosition :: pos_integer(),
    NewMail :: cauder_mailbox:mailbox(),
    NewBindings :: cauder_bindings:bindings().

match_receive_uid(Cs, Uid, Mail0, Bs0) ->
    case cauder_mailbox:take(Uid, Mail0) of
        error ->
            nomatch;
        {{Msg, QPos}, Mail1} ->
            case match_clause(Cs, [Msg#message.val], Bs0) of
                {Body, Bs} -> {Body, {Msg, QPos}, Mail1, Bs};
                nomatch -> nomatch
            end
    end.

-spec match_clause(Clauses, ValueList, Bindings) -> {Body, ScopeBindings} | nomatch when
    Clauses :: clauses(),
    ValueList :: [term()],
    Bindings :: cauder_bindings:bindings(),
    Body :: cauder_syntax:af_body(),
    ScopeBindings :: cauder_bindings:bindings().

match_clause([{'clause', _, H, G, B} | Cs], Vals, Bs) ->
    case match_list(H, Vals, Bs) of
        {match, Bs1} ->
            case guard_seq(G, Bs1) of
                true -> {B, Bs1};
                false -> match_clause(Cs, Vals, Bs)
            end;
        nomatch ->
            match_clause(Cs, Vals, Bs)
    end;
match_clause([], _, _) ->
    nomatch.

%% Tries to match a list of values against a list of patterns using the given environment.
%% The list of values should have no variables.

-spec match(Pattern, Term, Bindings) -> {match, NewBindings} | nomatch when
    Pattern :: cauder_syntax:af_pattern(),
    Term :: term(),
    Bindings :: cauder_bindings:bindings(),
    NewBindings :: cauder_bindings:bindings().

match(Pat, Term, Bs0) ->
    case catch match1(Pat, Term, Bs0) of
        invalid ->
            % TODO Convert pattern to term
            erlang:error({illegal_pattern, Pat});
        Other ->
            Other
    end.

-spec match1(Pattern, Term, Bindings) -> {match, NewBindings} when
    Pattern :: cauder_syntax:af_pattern(),
    Term :: term(),
    Bindings :: cauder_bindings:bindings(),
    NewBindings :: cauder_bindings:bindings().

match1({value, _, T0}, T, Bs) ->
    case T of
        T0 -> {match, Bs};
        _ -> throw(nomatch)
    end;
match1({var, _, '_'}, _, Bs) ->
    {match, Bs};
match1({var, _, Name}, Term, Bs) ->
    case cauder_bindings:find(Name, Bs) of
        {ok, Term} ->
            {match, Bs};
        {ok, _} ->
            throw(nomatch);
        error ->
            {match, cauder_bindings:add(Name, Term, Bs)}
    end;
match1({match, _, Pat1, Pat2}, Term, Bs) ->
    {match, Bs1} = match1(Pat1, Term, Bs),
    match1(Pat2, Term, Bs1);
match1({cons, _, H, T}, [H1 | T1], Bs) ->
    {match, Bs1} = match1(H, H1, Bs),
    match1(T, T1, Bs1);
match1({cons, _, _, _}, _, _Bs) ->
    throw(nomatch);
match1({tuple, _, Es}, Tuple, Bs) when length(Es) =:= tuple_size(Tuple) ->
    match_tuple(Es, Tuple, 1, Bs);
match1(_, _, _) ->
    throw(invalid).

-spec match_tuple(Elements, Tuple, Index, Bindings) -> {match, NewBindings} | no_return() when
    Elements :: [cauder_syntax:af_pattern()],
    Tuple :: tuple(),
    Index :: pos_integer(),
    Bindings :: cauder_bindings:bindings(),
    NewBindings :: cauder_bindings:bindings().

match_tuple([E | Es], Tuple, I, Bs0) ->
    {match, Bs} = match1(E, element(I, Tuple), Bs0),
    match_tuple(Es, Tuple, I + 1, Bs);
match_tuple([], _, _, Bs) ->
    {match, Bs}.

-spec match_list(PatternList, TermList, Bindings) -> {match, NewBindings} | nomatch when
    PatternList :: [cauder_syntax:af_pattern()],
    TermList :: [term()],
    Bindings :: cauder_bindings:bindings(),
    NewBindings :: cauder_bindings:bindings().

match_list([P | Ps], [T | Ts], Bs) ->
    case match(P, T, Bs) of
        {match, Bs1} ->
            match_list(Ps, Ts, Bs1);
        nomatch ->
            nomatch
    end;
match_list([], [], Bs) ->
    {match, Bs};
match_list(_Ps, _Ts, _Bs) ->
    nomatch.

%%%=============================================================================

-spec guard_seq(Guards, Bindings) -> boolean() when
    Guards :: cauder_syntax:af_guard_seq(),
    Bindings :: cauder_bindings:bindings().

guard_seq([], _) ->
    true;
guard_seq(Gs, Bs) when is_list(Gs) ->
    % In a guard sequence, guards are evaluated until one is true. The remaining guards, if any, are not evaluated.
    % See: https://erlang.org/doc/reference_manual/expressions.html#guard-sequences
    lists:any(fun(G) -> guard(G, Bs) end, Gs).

-spec guard(Guard, Bindings) -> boolean() when
    Guard :: cauder_syntax:af_guard(),
    Bindings :: cauder_bindings:bindings().

guard(G, Bs) when is_list(G) ->
    lists:all(fun(Gt) -> guard_test(Gt, Bs) end, G).

-spec guard_test(GuardTest, Bindings) -> boolean() when
    GuardTest :: cauder_syntax:af_guard_test(),
    Bindings :: cauder_bindings:bindings().

guard_test(Gt, Bs) ->
    % TODO erl_lint:is_guard_test/1
    case is_reducible(Gt, Bs) of
        true ->
            #result{expr = [Gt1]} = expr(Gt, Bs, cauder_stack:new()),
            guard_test(Gt1, Bs);
        false ->
            cauder_syntax:concrete(Gt)
    end.

%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Checks if the given abstract expression (or list of expressions) can be
%% reduced any further or not, given an environment.

-spec is_reducible(Expression | [Expression], Bindings) -> IsReducible when
    Expression :: expression(),
    Bindings :: cauder_bindings:bindings(),
    IsReducible :: boolean().

is_reducible([], _) ->
    false;
is_reducible([E | Es], Bs) ->
    is_reducible(E, Bs) orelse is_reducible(Es, Bs);
is_reducible({value, _, _}, _) ->
    false;
is_reducible({var, _, '_'}, _) ->
    false;
is_reducible({var, _, Name}, Bs) ->
    not cauder_utils:is_temp_variable_name(Name) andalso cauder_bindings:is_bound(Name, Bs);
is_reducible({cons, _, H, T}, Bs) ->
    is_reducible(H, Bs) orelse is_reducible(T, Bs);
is_reducible({tuple, _, Es}, Bs) ->
    is_reducible(Es, Bs);
is_reducible(E, _) when is_tuple(E) ->
    true.

%%------------------------------------------------------------------------------
%% @doc Checks if the given abstract expression is a literal value.

-spec is_value(Expression | [Expression]) -> IsValue when
    Expression :: expression(),
    IsValue :: boolean().

is_value([]) -> true;
is_value([E | Es]) -> is_value(E) andalso is_value(Es);
is_value({value, _, _}) -> true;
is_value({cons, _, H, T}) -> is_value(H) andalso is_value(T);
is_value({tuple, _, Es}) -> is_value(Es);
is_value(E) when is_tuple(E) -> false.

%%%=============================================================================

-spec eval_and_update(Index, Expression, Bindings, Stack) -> Result when
    Index :: pos_integer(),
    Expression :: expression(),
    Bindings :: cauder_bindings:bindings(),
    Stack :: cauder_stack:stack(),
    Result :: cauder_eval:result().

eval_and_update(N, Expr, Bs, Stk) ->
    case element(N, Expr) of
        Es when is_list(Es) ->
            R = #result{expr = Es1} = expr_list(Es, Bs, Stk),
            R#result{expr = [setelement(N, Expr, Es1)]};
        E ->
            R = #result{expr = [E1]} = expr(E, Bs, Stk),
            R#result{expr = [setelement(N, Expr, E1)]}
    end.
