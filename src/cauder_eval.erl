-module(cauder_eval).

%% API
-export([exprs/3, abstract/1, concrete/1, is_value/1, is_reducible/2]).
-export([match_receive_pid/6, match_receive_uid/4]).
-export([clause_line/3]).

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
    Expressions :: [cauder_syntax:abstract_expr()],
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
                            Es2 = cauder_syntax:replace_variable(Es1, setelement(2, Var, Line), concrete(E)),
                            #result{env = Bs1, expr = Es2, stack = Stk1};
                        {value, #s_block{expr = Es1, var = Var}} ->
                            Es2 = cauder_syntax:replace_variable(Es1, setelement(2, Var, Line), concrete(E)),
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
    Expression :: cauder_syntax:abstract_expr(),
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
                true -> R#result{expr = [{value, Line, [concrete(H) | concrete(T0)]}]};
                false -> R#result{expr = [setelement(3, E, H)]}
            end;
        false ->
            case is_reducible(T0, Bs) of
                true ->
                    R = #result{expr = [T]} = expr(T0, Bs, Stk),
                    case is_value(H0) andalso is_value(T) of
                        true -> R#result{expr = [{value, Line, [concrete(H0) | concrete(T)]}]};
                        false -> R#result{expr = [setelement(4, E, T)]}
                    end;
                false ->
                    #result{env = Bs, expr = [{value, Line, [concrete(H0) | concrete(T0)]}], stack = Stk}
            end
    end;
expr(E = {tuple, Line, Es0}, Bs, Stk) ->
    R = #result{expr = Es} = expr_list(Es0, Bs, Stk),
    case is_value(Es) of
        true ->
            Tuple = list_to_tuple(lists:map(fun concrete/1, Es)),
            #result{env = Bs, expr = [{value, Line, Tuple}], stack = Stk};
        false ->
            R#result{expr = [setelement(3, E, Es)]}
    end;
expr({'if', Line, Cs}, Bs, Stk0) ->
    case match_if(Cs, Bs) of
        {match, Body} ->
            Var = cauder_utils:temp_variable(Line),
            Entry = #s_block{type = 'if', expr = Body, var = Var},
            Stk = cauder_stack:push(Entry, Stk0),
            #result{env = Bs, expr = [Var], stack = Stk};
        nomatch ->
            error(if_clause)
    end;
expr(E = {'case', Line, A, Cs}, Bs0, Stk0) ->
    case is_reducible(A, Bs0) of
        true ->
            eval_and_update(3, E, Bs0, Stk0);
        false ->
            case match_case(Cs, A, Bs0) of
                {match, Body, Bs} ->
                    Var = cauder_utils:temp_variable(Line),
                    Entry = #s_block{type = 'case', expr = Body, var = Var},
                    Stk = cauder_stack:push(Entry, Stk0),
                    #result{env = Bs, expr = [Var], stack = Stk};
                nomatch ->
                    error({case_clause, concrete(A)})
            end
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
            Value = apply(M, F, lists:map(fun concrete/1, As)),
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
                    Label = #label_spawn_fun{var = Var, node = concrete(N), function = Fun},
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
                                module = concrete(M),
                                function = concrete(F),
                                args = concrete(As)
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
                                        node = concrete(N),
                                        module = concrete(M),
                                        function = concrete(F),
                                        args = concrete(As)
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
            Label = #label_start{var = Var, name = concrete(N)},
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
                    Label = #label_start{var = Var, name = concrete(N), host = concrete(H)},
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
                    Label = #label_send{dst = concrete(L), val = concrete(R)},
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
            {match, Body, Bs} = match_fun(Cs, As),
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
                            M = concrete(M0),
                            F = concrete(F0),
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
                    {env, [{{M, F}, Bs1, Cs}]} = erlang:fun_info(concrete(Fun), env),
                    {match, Body, Bs2} = match_fun(Cs, As),
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
                    case match([Lhs], [Rhs], Bs0) of
                        {match, Bs} -> #result{env = Bs, expr = [Rhs], stack = Stk};
                        nomatch -> error({badmatch, concrete(Rhs)})
                    end
            end
    end;
expr(E = {op, Line, Op, As}, Bs, Stk) ->
    case is_reducible(As, Bs) of
        true ->
            eval_and_update(4, E, Bs, Stk);
        false ->
            Value = apply(erlang, Op, lists:map(fun concrete/1, As)),
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
                            Value = apply(erlang, 'and', [concrete(Lhs), concrete(Rhs)]),
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
                            Value = apply(erlang, 'or', [concrete(Lhs), concrete(Rhs)]),
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

-spec expr_list(Expressions, Bindings, Stack) -> Result when
    Expressions :: [cauder_syntax:abstract_expr()],
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
            {match, Body, Bs} = match_fun(Cs, As),
            Var = cauder_utils:temp_variable(Line),
            Entry = #s_function{mfa = {M, F, A}, env = Bs, expr = Body, var = Var},
            Stk = cauder_stack:push(Entry, Stk0),
            #result{env = Bs0, expr = [Var], stack = Stk};
        error ->
            Value = apply(M, F, lists:map(fun concrete/1, As)),
            #result{env = Bs0, expr = [{value, Line, Value}], stack = Stk0}
    end.

%%%=============================================================================

-spec match_if(Clauses, Bindings) -> {match, Body} | nomatch when
    Clauses :: cauder_syntax:af_clause_seq(),
    Bindings :: cauder_bindings:bindings(),
    Body :: cauder_syntax:af_body().

match_if([], _) ->
    nomatch;
match_if([{'clause', _, [], G, B} | Cs], Bs) ->
    case concrete(guard_seq(G, Bs)) of
        true -> {match, B};
        false -> match_if(Cs, Bs)
    end.

-spec match_case(Clauses, Argument, Bindings) -> {match, Body, ScopeBindings} | nomatch when
    Clauses :: cauder_syntax:af_clause_seq(),
    Argument :: cauder_syntax:af_literal(),
    Bindings :: cauder_bindings:bindings(),
    Body :: cauder_syntax:af_body(),
    ScopeBindings :: cauder_bindings:bindings().

match_case(Cs, V, Bs) -> match_clause(Cs, [V], Bs).

-spec match_fun(Clauses, Arguments) -> {match, Body, ScopeBindings} | nomatch when
    Clauses :: cauder_syntax:af_clause_seq(),
    Arguments :: [cauder_syntax:af_literal()],
    Body :: cauder_syntax:af_body(),
    ScopeBindings :: cauder_bindings:bindings().

match_fun(Cs, Vs) -> match_clause(Cs, Vs, cauder_bindings:new()).

-spec match_receive_pid(Clauses, RecipientPid, Mail, Bindings, Sched, Sys) ->
    {Body, {Message, QueuePosition}, NewMail, NewBindings} | nomatch
when
    Clauses :: cauder_syntax:af_clause_seq(),
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
                            case match_receive(Cs, #message{uid = Uid} = Msg, Bs) of
                                {match, Body, Bs1} -> maps:put(Uid, {Bs1, Body, Msg}, Map1);
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
                            case match_receive(Cs, Msg, Bs) of
                                {match, Body, Bs1} -> {true, {Bs1, Body, Msg}};
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

-spec match_receive(Clauses, Message, Bindings) -> {match, Body, NewBindings} | nomatch when
    Clauses :: cauder_syntax:af_clause_seq(),
    Message :: cauder_message:message(),
    Bindings :: cauder_bindings:bindings(),
    Body :: cauder_syntax:af_body(),
    NewBindings :: cauder_bindings:bindings().

match_receive(Cs, #message{val = Value}, Bs0) -> match_clause(Cs, [abstract(Value)], Bs0).

-spec match_receive_uid(Clauses, Uid, Mail, Bindings) ->
    {Body, {Message, QueuePosition}, NewMail, NewBindings} | nomatch
when
    Clauses :: cauder_syntax:af_clause_seq(),
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
            case match_clause(Cs, [abstract(Msg#message.val)], Bs0) of
                {match, Body, Bs} -> {Body, {Msg, QPos}, Mail1, Bs};
                nomatch -> nomatch
            end
    end.

-spec match_clause(Clauses, Arguments, Bindings) -> {match, Body, ScopeBindings} | nomatch when
    Clauses :: cauder_syntax:af_clause_seq(),
    Arguments :: [cauder_syntax:af_literal()],
    Bindings :: cauder_bindings:bindings(),
    Body :: cauder_syntax:af_body(),
    ScopeBindings :: cauder_bindings:bindings().

match_clause([], _, _) ->
    nomatch;
match_clause([{'clause', _, Ps, G, B} | Cs], Vs, Bs0) ->
    case match(Ps, Vs, Bs0) of
        {match, Bs} ->
            case concrete(guard_seq(G, Bs)) of
                true -> {match, B, Bs};
                false -> match_clause(Cs, Vs, Bs0)
            end;
        nomatch ->
            match_clause(Cs, Vs, Bs0)
    end.

-spec clause_line(Clauses, Arguments, Bindings) -> Line when
    Clauses :: cauder_syntax:af_clause_seq(),
    Arguments :: [cauder_syntax:af_literal()],
    Bindings :: cauder_bindings:bindings(),
    Line :: non_neg_integer().

clause_line([], _, _) ->
    -1;
clause_line([{'clause', Line, Ps, G, _} | Cs], Vs, Bs0) ->
    case match(Ps, Vs, Bs0) of
        {match, Bs} ->
            case concrete(guard_seq(G, Bs)) of
                true -> Line;
                false -> clause_line(Cs, Vs, Bs0)
            end;
        nomatch ->
            clause_line(Cs, Vs, Bs0)
    end.

%% Tries to match a list of values against a list of patterns using the given environment.
%% The list of values should have no variables.

-spec match(Patterns, Arguments, Bindings) -> {match, NewBindings} | nomatch when
    Patterns :: [cauder_syntax:af_pattern()],
    Arguments :: [cauder_syntax:af_literal()],
    Bindings :: cauder_bindings:bindings(),
    NewBindings :: cauder_bindings:bindings().

match([], [], Bs) ->
    {match, Bs};
match([Pat | Ps0], [{value, _, Val} | Vs0], Bs0) when length(Ps0) == length(Vs0) ->
    case catch match1(Pat, Val, Bs0) of
        {match, Bs} -> match(Ps0, Vs0, Bs);
        nomatch -> nomatch
    end;
match(_Bs, _Ps, _Vs) ->
    nomatch.

% TODO Organize arguments to be consistent
-spec match1(Pattern, Term, Bindings) -> {match, NewBindings} | no_return() when
    Pattern :: cauder_syntax:af_pattern(),
    Term :: term(),
    Bindings :: cauder_bindings:bindings(),
    NewBindings :: cauder_bindings:bindings().

match1({value, _, V}, V, Bs) ->
    {match, Bs};
match1({var, _, '_'}, _, Bs) ->
    {match, Bs};
match1({var, _, Name}, Value, Bs0) ->
    case cauder_bindings:find(Name, Bs0) of
        {ok, Value} ->
            {match, Bs0};
        {ok, _} ->
            throw(nomatch);
        error ->
            Bs1 = cauder_bindings:add(Name, Value, Bs0),
            {match, Bs1}
    end;
match1({match, _, Pat1, Pat2}, Term, Bs0) ->
    {match, Bs1} = match1(Pat1, Term, Bs0),
    match1(Pat2, Term, Bs1);
match1({cons, _, H, T}, [H1 | T1], Bs0) ->
    {match, Bs} = match1(H, H1, Bs0),
    match1(T, T1, Bs);
match1({tuple, _, Es}, Tuple, Bs) when length(Es) =:= tuple_size(Tuple) ->
    match_tuple(Es, Tuple, 1, Bs);
match1(_, _, _) ->
    throw(nomatch).

-spec match_tuple(Values, Tuple, Index, Bindings) -> {match, NewBindings} | no_return() when
    Values :: [cauder_syntax:af_literal()],
    Tuple :: tuple(),
    Index :: pos_integer(),
    Bindings :: cauder_bindings:bindings(),
    NewBindings :: cauder_bindings:bindings().

match_tuple([], _, _, Bs) ->
    {match, Bs};
match_tuple([E | Es], Tuple, I, Bs0) ->
    {match, Bs} = match1(E, element(I, Tuple), Bs0),
    match_tuple(Es, Tuple, I + 1, Bs).

%%%=============================================================================

-spec guard_seq(Guards, Bindings) -> Boolean when
    Guards :: cauder_syntax:af_guard_seq(),
    Bindings :: cauder_bindings:bindings(),
    Boolean :: cauder_syntax:af_boolean().

guard_seq([], _) ->
    abstract(true);
guard_seq(Gs, Bs) when is_list(Gs) ->
    % In a guard sequence, guards are evaluated until one is true. The remaining guards, if any, are not evaluated.
    % See: https://erlang.org/doc/reference_manual/expressions.html#guard-sequences
    abstract(lists:any(fun(G) -> concrete(guard(G, Bs)) end, Gs)).

-spec guard(Guard, Bindings) -> Boolean when
    Guard :: cauder_syntax:af_guard(),
    Bindings :: cauder_bindings:bindings(),
    Boolean :: cauder_syntax:af_boolean().

guard(G, Bs) when is_list(G) ->
    abstract(lists:all(fun(Gt) -> concrete(guard_test(Gt, Bs)) end, G)).

-spec guard_test(GuardTest, Bindings) -> GuardTest | Boolean when
    GuardTest :: cauder_syntax:af_guard_test(),
    Bindings :: cauder_bindings:bindings(),
    Boolean :: cauder_syntax:af_boolean().

guard_test(Gt, Bs) ->
    case is_reducible(Gt, Bs) of
        true ->
            #result{expr = [Gt1]} = expr(Gt, Bs, cauder_stack:new()),
            guard_test(Gt1, Bs);
        false ->
            Gt
    end.

%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Converts the given Erlang term into its abstract form.

-spec abstract(Term) -> Literal when
    Term :: term(),
    Literal :: cauder_syntax:af_literal().

abstract(Value) -> {value, 0, Value}.

%%------------------------------------------------------------------------------
%% @doc Converts the given abstract literal element into the Erlang term that it
%% represents.

-spec concrete(Literal) -> Term when
    Literal :: cauder_syntax:af_literal(),
    Term :: term().

concrete({value, _, Value}) -> Value;
concrete({cons, _, {value, _, H}, {value, _, T}}) -> [H | T].

%%------------------------------------------------------------------------------
%% @doc Checks if the given abstract expression (or list of expressions) can be
%% reduced any further or not, given an environment.

-spec is_reducible(Expression | [Expression], Bindings) -> IsReducible when
    Expression :: cauder_syntax:abstract_expr(),
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
    Expression :: cauder_syntax:abstract_expr(),
    IsValue :: boolean().

is_value([]) -> true;
is_value([E | Es]) -> is_value(E) andalso is_value(Es);
is_value({value, _, _}) -> true;
is_value({cons, _, H, T}) -> is_value(H) andalso is_value(T);
is_value({tuple, _, Es}) -> is_value(Es);
is_value(E) when is_tuple(E) -> false.

-spec eval_and_update(Index, Expression, Bindings, Stack) -> Result when
    Index :: pos_integer(),
    Expression :: cauder_syntax:abstract_expr(),
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
