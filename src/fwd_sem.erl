%%%-------------------------------------------------------------------
%%% @doc Some functions that implement the forward (reversible)
%%% semantics for Erlang. These can be divided into functions to get
%%% the evaluation options and functions to perform the evaluation
%%% @end
%%%-------------------------------------------------------------------

-module(fwd_sem).
-export([eval_step/2, eval_sched/2,
         eval_opts/1, eval_procs_opts/1, eval_sched_opts/1]).

-include("cauder.hrl").


% TODO Add other labels
% TODO Maybe use concrete values?
-type label() :: tau
| {spawn, {erl_parse:af_variable(), erl_parse:af_atom(), [erl_parse:abstract_expr()]}}
| {self, erl_parse:af_variable()}
| {send, erl_parse:af_integer(), erl_parse:abstract_expr()}
| any().


%% =====================================================================
%% @doc Evaluates the first non-literal expression for the given `Expressions`
%% list and returns a tuple with an updated environment, the list of expressions
%% that resulted from the evaluation, and a label.

-spec eval_expr_list(Environment, Expressions) -> {NewEnvironment, NewExpressions, Label} when
  Environment :: erl_eval:binding_struct(),
  Expressions :: [erl_parse:abstract_expr()],
  NewEnvironment :: erl_eval:binding_struct(),
  NewExpressions :: [erl_parse:abstract_expr()],
  Label :: label().

eval_expr_list(Env, [Expr | Exprs]) when is_tuple(Expr), is_list(Exprs) ->
  case is_expr(Expr) of
    true ->
      {NewEnv, NewExpr, Label} = eval_expr(Env, Expr),
      {NewEnv, [NewExpr | Exprs], Label};
    false ->
      {NewEnv, NewExpr, Label} = eval_expr_list(Env, Exprs),
      {NewEnv, [Expr | NewExpr], Label}
  end.


%% =====================================================================
%% @doc Evaluates the given `Expression` and returns a tuple with an updated
%% environment, the expression that resulted from the evaluation, and a label.

-spec eval_expr(Environment, Expression) -> {NewEnvironment, NewExpressions, Label} when
  Environment :: erl_eval:binding_struct(),
  Expression :: erl_parse:abstract_expr(),
  NewEnvironment :: erl_eval:binding_struct(),
  NewExpressions :: erl_parse:abstract_expr() | [erl_parse:abstract_expr()],
  Label :: label().

eval_expr(Env, Expr) when is_tuple(Expr) ->
  case is_expr(Expr) of
    false ->
      % If we find a literal we just consume it.
      {Env, [], tau};
    true ->
      case erl_syntax:type(Expr) of
        variable ->
          eval_variable(Env, Expr);
        match_expr ->
          eval_match_expr(Env, Expr);
        infix_expr ->
          eval_infix_expr(Env, Expr);
        prefix_expr ->
          eval_prefix_expr(Env, Expr);
        application ->
          eval_application(Env, Expr);
        list ->
          eval_list(Env, Expr);
        tuple ->
          eval_tuple(Env, Expr);



        'case' ->
          CaseArg = cerl:case_arg(Expr),
          case is_expr(CaseArg) of
            true ->
              {NewEnv, NewCaseArg, Label} = eval_expr(Env, CaseArg),
              NewExp = cerl:update_c_case(Expr,
                                          NewCaseArg,
                                          cerl:case_clauses(Expr)),
              {NewEnv, NewExp, Label};
            false ->
              %io:format("Env: ~p\n",[Env]),
              %io:format("CaseArg: ~p\n",[CaseArg]),
              CaseClauses = cerl:case_clauses(Expr),
              %io:format("CaseClauses: ~p\n",[CaseClauses]),
              CaseClauses2 = replace_guards(Env, CaseClauses),
              %io:format("CaseClauses2: ~p\n",[CaseClauses2]),
              %CaseClauses3 = init(CaseClauses2),
              CaseArgs =
              case cerl:type(CaseArg) of
                values ->
                  cerl:values_es(CaseArg);
                _ ->
                  [CaseArg]
              end,
              case cerl_clauses:reduce(CaseClauses2, CaseArgs) of
                {true, {Clause, Bindings}} ->
                  ClauseBody = cerl:clause_body(Clause),
                  NewEnv = utils:merge_env(Env, Bindings),
                  {NewEnv, ClauseBody, tau};
                {false, _} ->
                  io:fwrite("Error: No matching clause~n")
              end
          end;
        call ->
          CallArgs = cerl:call_args(Expr),
          CallModule = cerl:call_module(Expr),
          CallName = cerl:call_name(Expr),

          case is_expr(CallModule) of
            true ->
              {NewEnv, NewCallModule, Label} = eval_expr(Env, CallModule),
              NewExp = cerl:update_c_call(Expr,
                                          NewCallModule,
                                          CallName,
                                          CallArgs),
              {NewEnv, NewExp, Label};
            false ->
              case is_expr(CallName) of
                true ->
                  {NewEnv, NewCallName, Label} = eval_expr(Env, CallName),
                  NewExp = cerl:update_c_call(Expr,
                                              CallModule,
                                              NewCallName,
                                              CallArgs),
                  {NewEnv, NewExp, Label};
                false ->
                  case is_expr(CallArgs) of
                    true ->
                      {NewEnv, NewCallArgs, Label} = eval_expr(Env, CallArgs),
                      NewExp = cerl:update_c_call(Expr,
                                                  CallModule,
                                                  CallName,
                                                  NewCallArgs),
                      {NewEnv, NewExp, Label};
                    false ->
                      case {CallModule, CallName} of
                        {{c_literal, _, 'erlang'}, {c_literal, _, 'spawn'}} ->
                          Var = utils:temp_variable(),
                          FunName = lists:nth(2, CallArgs),
                          FunArgs = utils:list_from_core(lists:nth(3, CallArgs)),
                          {Env, Var, {spawn, {Var, FunName, FunArgs}}};
                        {{c_literal, _, 'erlang'}, {c_literal, _, 'self'}} ->
                          Var = utils:temp_variable(),
                          {Env, Var, {self, Var}};
                        {{c_literal, _, 'erlang'}, {c_literal, _, '!'}} ->
                          DestPid = lists:nth(1, CallArgs),
                          MsgValue = lists:nth(2, CallArgs),
                          {Env, MsgValue, {send, DestPid, MsgValue}};
                        {{c_literal, _, 'timer'}, {c_literal, _, 'sleep'}} ->
                          NewExp = cerl:c_atom('ok'),
                          {Env, NewExp, tau};
                        _ ->
                          ToggleOpts = utils_gui:toggle_opts(),
                          AddOptimize = proplists:get_value(?COMP_OPT, ToggleOpts),
                          CompOpts =
                          case AddOptimize of
                            true ->
                              [to_core, binary];
                            false ->
                              [to_core, binary, no_copt]
                          end,
                          Filename = cerl:concrete(CallModule),
                          Path = ets:lookup_element(?GUI_REF, ?LAST_PATH, 2),
                          File = filename:join(Path, Filename),
                          case compile:file(File, CompOpts) of
                            {ok, _, CoreForms} ->
                              NoAttsCoreForms = cerl:update_c_module(CoreForms,
                                                                     cerl:module_name(CoreForms),
                                                                     cerl:module_exports(CoreForms),
                                                                     [],
                                                                     cerl:module_defs(CoreForms)),
                              Stripper = fun(Tree) ->
                                cerl:set_ann(Tree, []) end,
                              CleanCoreForms = cerl_trees:map(Stripper, NoAttsCoreForms),
                              FunDefs = cerl:module_defs(CleanCoreForms),
                              ConcName = cerl:concrete(CallName),
                              %ConcArgs = [utils:toErlang(Arg) || Arg <- CallArgs],
                              %io:fwrite("---------------~n"),
                              %io:write(CallName),
                              %io:fwrite("~n---------------~n"),
                              %FunDef = utils:fundef_lookup(CallName, FunDefs),
                              FunDef = utils:fundef_lookup(cerl:c_var({ConcName, cerl:call_arity(Expr)}), FunDefs),
                              NewFunDef = utils:fundef_rename(FunDef),
                              FunBody = cerl:fun_body(NewFunDef),
                              FunArgs = cerl:fun_vars(NewFunDef),
                              % standard zip is used here (pretty-printer forces it)
                              NewEnv = utils:merge_env(Env, lists:zip(FunArgs, CallArgs)), %ApplyArgs
                              {NewEnv, FunBody, tau};
                            error -> %for builtin
                              ConcModule = cerl:concrete(CallModule),
                              ConcName = cerl:concrete(CallName),
                              ConcArgs = [utils:toErlang(Arg) || Arg <- CallArgs],
                              ConcExp = apply(ConcModule, ConcName, ConcArgs),
                              StrExp = lists:flatten(io_lib:format("~p", ([ConcExp]))) ++ ".",
                              {ok, ParsedExp, _} = erl_scan:string(StrExp),
                              {ok, TypedExp} = erl_parse:parse_exprs(ParsedExp),
                              CoreExp = hd([utils:toCore(Expr) || Expr <- TypedExp]),
                              NewExp = CoreExp,
                              {Env, NewExp, tau}
                          end
                      end
                  end
              end
          end;
        'receive' ->
          Var = utils:temp_variable(),
          % SubsExp = utils:substitute(Exp, Env),
          % {Env, Var, {rec, Var, cerl:receive_clauses(SubsExp)}}
          ReceiveClauses = cerl:receive_clauses(Expr),
          %%ReceiveClauses2 = replace_guards(Env,ReceiveClauses),
          {Env, Var, {rec, Var, ReceiveClauses}}
      end
  end.


-spec eval_variable(Environment, Variable) -> {NewEnvironment, NewExpression, Label} when
  Environment :: erl_eval:binding_struct(),
  Variable :: erl_parse:abstract_expr(), % erl_parse:af_variable()
  NewEnvironment :: erl_eval:binding_struct(),
  NewExpression :: erl_parse:abstract_expr(),
  Label :: tau.

eval_variable(Env, Var = {var, _, _}) ->
  Name = erl_syntax:variable_name(Var),
  Binding = orddict:fetch(Name, Env),
  NewValue = erl_syntax:revert(erl_syntax:abstract(Binding)),
  {Env, NewValue, tau}.


-spec eval_match_expr(Environment, MatchExpression) -> {NewEnvironment, NewExpression, Label} when
  Environment :: erl_eval:binding_struct(),
  MatchExpression :: erl_parse:abstract_expr(), % erl_parse:af_match(erl_parse:abstract_expr())
  NewEnvironment :: erl_eval:binding_struct(),
  NewExpression :: erl_parse:abstract_expr(),
  Label :: label().

eval_match_expr(Env, MatchExpr = {match, _, _, _}) ->
  Body = erl_syntax:match_expr_body(MatchExpr),
  case is_expr(Body) of
    true ->
      {NewEnv, NewBody, Label} = eval_expr(Env, Body),
      % Structure: {match, Pos, Pattern, Body}
      NewMatchExp = setelement(4, MatchExpr, NewBody),
      {NewEnv, NewMatchExp, Label};
    false ->
      % There should be no variables to evaluate so we pass no bindings
      {value, _, Bindings} = erl_eval:expr(MatchExpr, []),
      NewEnv = utils:merge_env(Env, Bindings),
      {NewEnv, [], tau} % TODO Review
  end.


-spec eval_infix_expr(Environment, InfixExpression) -> {NewEnvironment, NewExpression, Label} when
  Environment :: erl_eval:binding_struct(),
  InfixExpression :: erl_parse:abstract_expr(), % erl_parse:af_binary_op(erl_parse:abstract_expr())
  NewEnvironment :: erl_eval:binding_struct(),
  NewExpression :: erl_parse:abstract_expr(),
  Label :: label().

eval_infix_expr(Env, InfixExpr = {op, _, _, _, _}) ->
  % TODO Short-circuit expressions
  % TODO Send operator
  Left = erl_syntax:infix_expr_left(InfixExpr),
  case is_expr(Left) of
    true ->
      {NewEnv, NewLeft, Label} = eval_expr(Env, Left),
      % Structure: {op, Pos, Operator, Left, Right}
      NewOp = setelement(4, InfixExpr, NewLeft),
      {NewEnv, NewOp, Label};
    false ->
      Right = erl_syntax:infix_expr_right(InfixExpr),
      case is_expr(Right) of
        true ->
          {NewEnv, NewRight, Label} = eval_expr(Env, Right),
          % Structure: {op, Pos, Operator, Left, Right}
          NewOp = setelement(5, InfixExpr, NewRight),
          {NewEnv, NewOp, Label};
        false ->
          Op = erl_syntax:atom_value(erl_syntax:infix_expr_operator(InfixExpr)),
          case Op of
            '!' ->
              {Env, Right, {send, Left, Right}};
            _ ->
              % Infix operators are always built-in, so we just evaluate the expression
              % There should be no variables to evaluate so we pass no bindings
              {value, Value, _} = erl_eval:expr(InfixExpr, []),
              % Use `erl_syntax:abstract/1` and `erl_syntax:revert/1` instead
              % of `erl_parser:abstract/1` to avoid problems with lists being
              % represented as strings
              NewValue = erl_syntax:revert(erl_syntax:abstract(Value)),
              {Env, NewValue, tau}
          end
      end
  end.


-spec eval_prefix_expr(Environment, PrefixExpression) -> {NewEnvironment, NewExpression, Label} when
  Environment :: erl_eval:binding_struct(),
  PrefixExpression :: erl_parse:abstract_expr(), % erl_parse:af_unary_op(erl_parse:abstract_expr())
  NewEnvironment :: erl_eval:binding_struct(),
  NewExpression :: erl_parse:abstract_expr(),
  Label :: label().

eval_prefix_expr(Env, PrefixExpr = {op, _, _, _}) ->
  % FIXME The '-' prefix causes two steps the show the same expression,
  % however they have two different internal representations:
  %  - The number with the operator e.g -(42)
  %  - The negated number e.g (-42)
  Arg = erl_syntax:prefix_expr_argument(PrefixExpr),
  case is_expr(Arg) of
    true ->
      {NewEnv, NewArg, Label} = eval_expr(Env, Arg),
      % Structure: {op, Pos, Operator, Arg}
      NewOp = setelement(4, PrefixExpr, NewArg),
      {NewEnv, NewOp, Label};
    false ->
      % Prefix operators are always built-in, so we just evaluate the expression
      % There should be no variables to evaluate so we pass no bindings
      {value, Value, _} = erl_eval:expr(PrefixExpr, []),
      % Use `erl_syntax:abstract/1` and `erl_syntax:revert/1` instead
      % of `erl_parser:abstract/1` to avoid problems with lists being
      % represented as strings
      NewValue = erl_syntax:revert(erl_syntax:abstract(Value)),
      {Env, NewValue, tau}
  end.


-spec eval_application(Environment, Application) -> {NewEnvironment, NewExpression, Label} when
  Environment :: erl_eval:binding_struct(),
  Application :: erl_parse:abstract_expr(), % erl_parse:af_local_call() | erl_parse:af_remote_call()
  NewEnvironment :: erl_eval:binding_struct(),
  NewExpression :: erl_parse:abstract_expr(),
  Label :: label().

eval_application(Env, Expr = {call, _, _, _}) ->
  Op = erl_syntax:application_operator(Expr),
  case erl_syntax:type(Op) of
    module_qualifier ->
      Module = erl_syntax:module_qualifier_argument(Op),
      case is_expr(Module) of
        true ->
          {NewEnv, NewModule, Label} = eval_expr(Env, Module),
          % Structure: {remote, Pos, Module, Arg}
          NewOp = setelement(3, Op, NewModule),
          % Structure: {call, Pos, Operator, Args}
          NewExpr = setelement(3, Expr, NewOp),
          {NewEnv, NewExpr, Label};
        false ->
          Function = erl_syntax:module_qualifier_body(Op),
          case is_expr(Function) of
            true ->
              {NewEnv, NewName, Label} = eval_expr(Env, Function),
              % Structure: {remote, Pos, Module, Arg}
              NewOp = setelement(4, Op, NewName),
              % Structure: {call, Pos, Operator, Args}
              NewExpr = setelement(3, Expr, NewOp),
              {NewEnv, NewExpr, Label};
            false ->
              CallArgs = erl_syntax:application_arguments(Expr),
              case lists:any(fun is_expr/1, CallArgs) of
                true ->
                  {NewEnv, NewFunArgs, Label} = eval_expr_list(Env, CallArgs),
                  % Structure: {call, Pos, Operator, Args}
                  NewExpr = setelement(4, Expr, NewFunArgs),
                  {NewEnv, NewExpr, Label};
                false ->
                  case erl_syntax:atom_value(Module) of
                    'erlang' ->
                      Name = erl_syntax:atom_value(Function),
                      eval_bif(Name, Expr, Env);
                    _ ->
                      % TODO Check if module matches current one
                      % TODO Handle calls to functions in other modules
                      % There should be no variables to evaluate so we pass no bindings
                      {value, Value, _} = erl_eval:expr(Expr, []),
                      % Use `erl_syntax:abstract/1` and `erl_syntax:revert/1` instead
                      % of `erl_parser:abstract/1` to avoid problems with lists being
                      % represented as strings
                      NewExpr = erl_syntax:revert(erl_syntax:abstract(Value)),
                      {Env, NewExpr, tau}
                  end
              end
          end
      end;
    _ ->
      case is_expr(Op) of
        true ->
          {NewEnv, NewOp, Label} = eval_expr(Env, Op),
          % Structure: {call, Pos, Operator, Args}
          NewExpr = setelement(3, Expr, NewOp),
          {NewEnv, NewExpr, Label};
        false ->
          CallArgs = erl_syntax:application_arguments(Expr),
          case lists:any(fun is_expr/1, CallArgs) of
            true ->
              {NewEnv, NewFunArgs, Label} = eval_expr_list(Env, CallArgs),
              % Structure: {call, Pos, Operator, Args}
              NewExpr = setelement(4, Expr, NewFunArgs),
              {NewEnv, NewExpr, Label};
            false ->
              Name = erl_syntax:atom_value(Op),
              % Check if the function is in this file
              case utils:fundef_lookup(Name, length(CallArgs), ref_lookup(?FUN_DEFS)) of
                {value, FunDef} ->
                  FunClauses = erl_syntax:function_clauses(utils:fundef_rename(FunDef)),
                  % There should be no variables to evaluate so we pass no bindings
                  % TODO `match_clause` looks like an internal function, because it is not documented
                  {Body, Bindings} = erl_eval:match_clause(FunClauses, CallArgs, [], none),
                  % The environment stores the literal value but `match_clause`
                  % returns the bindings as `erl_parse` nodes so we convert them
                  ValueBindings = [{Name, erl_syntax:concrete(Value)} || {Name, Value} <- Bindings],
                  NewEnv = utils:merge_env(Env, ValueBindings),
                  {NewEnv, Body, tau};
                false ->
                  % TODO If the function name was a variable then BIF should not be evaluated
                  % TODO Look for function in other files in the same directory
                  eval_bif(Name, Env, Expr)
              end
          end
      end
  end.



-spec eval_bif(Name, Environment, Application) -> {NewEnvironment, NewExpression, Label} when
  Name :: atom(),
  Environment :: erl_eval:binding_struct(),
  Application :: erl_parse:abstract_expr(), % erl_parse:af_local_call() | erl_parse:af_remote_call()
  NewEnvironment :: erl_eval:binding_struct(),
  NewExpression :: erl_parse:abstract_expr(),
  Label :: label().

eval_bif(Name, Application = {call, _, _, _}, Env) when is_atom(Name) ->
  case Name of
    'spawn' ->
      TmpVar = utils:temp_variable(),
      case erl_syntax:application_arguments(Application) of
        % TODO erlang:spawn/1,2,4
        % erlang:spawn/3
        [_Module, Function, Args] ->
          % TODO Handle calls to functions in other modules
          {Env, TmpVar, {spawn, {TmpVar, Function, erl_syntax:list_elements(Args)}}}
      end;
    'self' ->
      TmpVar = utils:temp_variable(),
      {Env, TmpVar, {self, TmpVar}};
    _ ->
      % BIF so we just evaluate it
      % There should be no variables to evaluate so we pass no bindings
      {value, Value, _} = erl_eval:expr(Application, []),
      % Use `erl_syntax:abstract/1` and `erl_syntax:revert/1` instead
      % of `erl_parser:abstract/1` to avoid problems with lists being
      % represented as strings
      NewExpr = erl_syntax:revert(erl_syntax:abstract(Value)),
      {Env, NewExpr, tau}
  end.


-spec eval_list(Environment, List) -> {NewEnvironment, NewExpression, Label} when
  Environment :: erl_eval:binding_struct(),
  List :: erl_parse:abstract_expr(), % erl_parse:af_cons(erl_parse:abstract_expr())
  NewEnvironment :: erl_eval:binding_struct(),
  NewExpression :: erl_parse:abstract_expr(),
  Label :: label().

eval_list(Env, List = {cons, _, _, _}) ->
  Head = erl_syntax:list_head(List),
  case is_expr(Head) of
    true ->
      {NewEnv, NewHead, Label} = eval_expr(Env, Head),
      % Structure: {cons, Pos, Head, Tail}
      NewList = setelement(3, List, NewHead),
      {NewEnv, NewList, Label};
    false ->
      % `list_tail` returns a syntax tree but we want an `erl_parse` node
      Tail = erl_syntax:revert(erl_syntax:list_tail(List)),
      {NewEnv, NewTail, Label} = eval_expr(Env, Tail),
      % Structure: {cons, Pos, Head, Tail}
      NewList = setelement(4, List, NewTail),
      {NewEnv, NewList, Label}
  end.


-spec eval_tuple(Environment, Tuple) -> {NewEnvironment, NewExpression, Label} when
  Environment :: erl_eval:binding_struct(),
  Tuple :: erl_parse:abstract_expr(), % erl_parse:af_tuple(erl_parse:abstract_expr())
  NewEnvironment :: erl_eval:binding_struct(),
  NewExpression :: erl_parse:abstract_expr(),
  Label :: label().

eval_tuple(Env, Tuple = {tuple, _, _}) ->
  Elements = erl_syntax:tuple_elements(Tuple),
  {NewEnv, NewElements, Label} = eval_expr_list(Env, Elements),
  % Structure: {tuple, Pos, Elements}
  NewTuple = setelement(3, Tuple, NewElements),
  {NewEnv, NewTuple, Label}.


%init([_X]) -> [];
%nit([A|R]) -> [A|init(R)].

replace_guards(Bindings, Exps) ->
  lists:map(fun({c_clause, L, Pats, Guard, Exp}) ->
    Guard2 = utils:replace_all(Bindings, Guard),
    Guard3 = eval_guard(Guard2),
    {c_clause, L, Pats, Guard3, Exp}
            %case ReducedGuard of
            %    {value,true} -> {c_clause,L,Pats,true,Exp};
            %    _Other -> {c_clause,L,Pats,ReducedGuard,Exp}
            %end
            end, Exps).

eval_guard(Exp) ->
  case cerl:type(Exp) of
    call ->
      CallArgs = cerl:call_args(Exp),
      CallModule = cerl:call_module(Exp),
      CallName = cerl:call_name(Exp),
      ConcModule = cerl:concrete(CallModule),
      ConcName = cerl:concrete(CallName),
      ConcArgs = [utils:toErlang(Arg) || Arg <- CallArgs],
      ConcExp = apply(ConcModule, ConcName, ConcArgs),
      StrExp = lists:flatten(io_lib:format("~p", ([ConcExp]))) ++ ".",
      %%io:format("ConcModule: ~p\nConcName: ~p\nConcArgs: ~p\nConcExp: ~p\nStrExp: ~p\n",[ConcModule,ConcName,ConcArgs,ConcExp,StrExp]),
      {ok, ParsedExp, _} = erl_scan:string(StrExp),
      {ok, TypedExp} = erl_parse:parse_exprs(ParsedExp),
      hd([utils:toCore(Expr) || Expr <- TypedExp]);
    'let' ->
      %io:format("1)~w~n",[Exp]),
      LetArg = cerl:let_arg(Exp),
      case is_expr(LetArg) of
        true ->
          NewLetArg = eval_guard(LetArg),
          NewExp = cerl:update_c_let(Exp,
                                     cerl:let_vars(Exp),
                                     NewLetArg,
                                     cerl:let_body(Exp)),
          eval_guard(NewExp);
        false ->
          LetVars = cerl:let_vars(Exp),
          LetEnv =
          case cerl:let_arity(Exp) of
            1 ->
              lists:zip(LetVars, [LetArg]);
            _ ->
              FlatLetArg =
              case cerl:type(LetArg) of
                values ->
                  cerl:values_es(LetArg);
                _ ->
                  LetArg
              end,
              lists:zip(LetVars, FlatLetArg)
          end,
          NewExp = cerl:let_body(Exp),
          %io:format("2)~w~n",[NewExp]),
          %io:format("2e)~w~n",[LetEnv]),
          SubstExp = utils:replace_all(LetEnv, NewExp),
          %io:format("3)~w~n",[SubstExp]),
          %StrExp = lists:flatten(io_lib:format("~p", ([SubstExp]))) ++ ".",
          %%%io:format("ConcModule: ~p\nConcName: ~p\nConcArgs: ~p\nConcExp: ~p\nStrExp: ~p\n",[ConcModule,ConcName,ConcArgs,ConcExp,StrExp]),
          %{ok, ParsedExp, _} = erl_scan:string(StrExp),
          %{ok, TypedExp} = erl_parse:parse_exprs(ParsedExp),
          %TypExpr=hd([utils:toCore(Expr) || Expr <- TypedExp]),
          %FinalExp=eval_guard(TypExpr),
          %io:format("4)~w~n",[FinalExp]),
          eval_guard(SubstExp)
      end;
    _Other ->
      Exp
  end.

%%--------------------------------------------------------------------
%% @doc Performs an evaluation step in process Pid, given System
%% @end
%%--------------------------------------------------------------------
-spec eval_step(System, Pid) ->
  NewSystem when
  System :: #sys{},
  Pid :: erl_parse:af_integer(),
  NewSystem :: #sys{}.

eval_step(System, Pid) ->
  #sys{msgs = Msgs, procs = Procs, trace = Trace} = System,
  {Proc, RestProcs} = utils:select_proc(Procs, Pid),
  #proc{pid = Pid, hist = Hist, env = Env, exp = [Expr | RestExpr], mail = Mail} = Proc,
  {NewEnv, NewExpr, Label} = eval_expr(Env, Expr),
  NewSystem =
  case Label of
    tau ->
      NewHist = [{tau, Env, Expr} | Hist],
      NewProc = Proc#proc{hist = NewHist, env = NewEnv, exp = lists:flatten([NewExpr], RestExpr)}, % FIXME NewExp can be a list or not
      System#sys{msgs = Msgs, procs = [NewProc | RestProcs]};
    {self, Var} ->
      NewHist = [{self, Env, Expr} | Hist],
      RepExp = utils:replace_variable(Var, Pid, lists:flatten([NewExpr], RestExpr)), % FIXME NewExp can be a list or not
      NewProc = Proc#proc{hist = NewHist, env = NewEnv, exp = RepExp},
      System#sys{msgs = Msgs, procs = [NewProc | RestProcs]};
    {send, DestPid, MsgValue} ->
      Time = ref_lookup(?FRESH_TIME),
      ref_add(?FRESH_TIME, Time + 1),
      NewMsg = #msg{dest = DestPid, val = MsgValue, time = Time},
      NewMsgs = [NewMsg | Msgs],
      NewHist = [{send, Env, Expr, DestPid, {MsgValue, Time}} | Hist],
      NewProc = Proc#proc{hist = NewHist, env = NewEnv, exp = lists:flatten([NewExpr], RestExpr)}, % FIXME NewExp can be a list or not
      TraceItem = #trace{type = ?RULE_SEND, from = Pid, to = DestPid, val = MsgValue, time = Time},
      NewTrace = [TraceItem | Trace],
      System#sys{msgs = NewMsgs, procs = [NewProc | RestProcs], trace = NewTrace};
    {spawn, {Var, FunName, FunArgs}} ->
      SpawnPid = erl_parse:abstract(utils:fresh_pid()),
      SpawnProc = #proc{
        pid = SpawnPid,
        exp = [erl_syntax:revert(erl_syntax:application(FunName, FunArgs))],
        spf = {erl_syntax:atom_value(FunName), length(FunArgs)}
      },
      NewHist = [{spawn, Env, Expr, SpawnPid} | Hist],
      RepExp = utils:replace_variable(Var, SpawnPid, lists:flatten([NewExpr], RestExpr)), % FIXME NewExp can be a list or not
      NewProc = Proc#proc{hist = NewHist, env = NewEnv, exp = RepExp},
      TraceItem = #trace{type = ?RULE_SPAWN, from = Pid, to = SpawnPid},
      NewTrace = [TraceItem | Trace],
      System#sys{msgs = Msgs, procs = [NewProc | [SpawnProc | RestProcs]], trace = NewTrace};
    {rec, Var, ReceiveClauses} ->
      {Bindings, RecExp, ConsMsg, NewMail} = matchrec(ReceiveClauses, Mail, NewEnv),
      UpdatedEnv = utils:merge_env(NewEnv, Bindings),
      RepExp = utils:replace_variable(Var, RecExp, lists:flatten([NewExpr], RestExpr)), % FIXME NewExp can be a list or not
      NewHist = [{rec, Env, Expr, ConsMsg, Mail} | Hist],
      NewProc = Proc#proc{hist = NewHist, env = UpdatedEnv, exp = RepExp, mail = NewMail},
      {MsgValue, Time} = ConsMsg,
      TraceItem = #trace{type = ?RULE_RECEIVE, from = Pid, val = MsgValue, time = Time},
      NewTrace = [TraceItem | Trace],
      System#sys{msgs = Msgs, procs = [NewProc | RestProcs], trace = NewTrace}
  end,
  NewSystem.

%%--------------------------------------------------------------------
%% @doc Performs an evaluation step in message Id, given System
%% @end
%%--------------------------------------------------------------------
eval_sched(System, Id) ->
  Procs = System#sys.procs,
  Msgs = System#sys.msgs,
  {Msg, RestMsgs} = utils:select_msg(Msgs, Id),
  #msg{dest = DestPid, val = Value, time = Id} = Msg,
  {Proc, RestProcs} = utils:select_proc(Procs, DestPid),
  Mail = Proc#proc.mail,
  NewMail = Mail ++ [{Value, Id}],
  NewProc = Proc#proc{mail = NewMail},
  System#sys{msgs = RestMsgs, procs = [NewProc | RestProcs]}.

%% =====================================================================
%% @doc Checks if the given Expression can  is an expression or not
%% @end
%%--------------------------------------------------------------------

-spec is_expr(Expression :: erl_parse:abstract_expr()) ->
  boolean().

is_expr(Expr) when is_tuple(Expr) ->
  not erl_syntax:is_literal(Expr).



matchrec(Clauses, Mail, Env) ->
  matchrec(Clauses, Mail, [], Env).

matchrec(_, [], _, _) ->
  no_match;
matchrec(Clauses, [CurMsg | RestMsgs], AccMsgs, Env) ->
  {MsgValue, _MsgTime} = CurMsg,
  %io:format("matchrec (MsgValue): ~p~n",[MsgValue]),
  %io:format("matchrec (Clauses): ~p~n",[Clauses]),
  %%preprocessing is used to propagate matching bindings to guards
  NewClauses = preprocessing_clauses(Clauses, MsgValue, Env),
  %io:format("matchrec (NewClauses): ~p~n",[NewClauses]),
  case cerl_clauses:reduce(NewClauses, [MsgValue]) of
    {true, {Clause, Bindings}} ->
      ClauseBody = cerl:clause_body(Clause),
      NewMsgs = AccMsgs ++ RestMsgs,
      {Bindings, ClauseBody, CurMsg, NewMsgs};
    {false, []} ->
      matchrec(Clauses, RestMsgs, AccMsgs ++ [CurMsg], Env);
    {false, [Clause | OtherClauses]} ->
      io:format("CauDEr: Unsupported pattern, some behaviours may be missed ~n~w~n", [Clause]),
      matchrec(Clauses, RestMsgs, AccMsgs ++ [CurMsg], Env)
  end.

preprocessing_clauses(Clauses, Msg, Env) ->
  lists:map(fun({c_clause, L, Pats, Guard, Exp}) ->
    %io:format("Clauses: ~p~n",[Clauses]),
    %io:format("match (Pats/[Msg]) ~p~n~p~n",[Pats,[Msg]]),
    %io:format("--result: ~p~n",[cerl_clauses:match_list(Pats,[Msg])]),
    case cerl_clauses:match_list(Pats, [Msg]) of
      {true, Bindings} ->
        Guard2 = utils:replace_all(Bindings ++ Env, Guard),
        %io:format("calling eval_guard (Bindings/Guard/Guard2): ~n~p~n~p~n~p~n",[Bindings++Env,Guard,Guard2]),
        Guard3 = eval_guard(Guard2),
        {c_clause, L, Pats, Guard3, Exp};
      _ ->
        {c_clause, L, Pats, Guard, Exp}
    end
            end, Clauses).

%%--------------------------------------------------------------------
%% @doc Gets the evaluation options for a given System
%% @end
%%--------------------------------------------------------------------

-spec eval_opts(System) ->
  Options when
  System :: #sys{},
  Options :: [#opt{}].

eval_opts(System) ->
  SchedOpts = eval_sched_opts(System),
  ProcsOpts = eval_procs_opts(System),
  SchedOpts ++ ProcsOpts.


-spec eval_sched_opts(System) ->
  Options when
  System :: #sys{},
  Options :: [#opt{}].

eval_sched_opts(#sys{msgs = []}) ->
  [];
eval_sched_opts(#sys{msgs = [CurMsg | RestMsgs], procs = Procs}) ->
  DestPid = CurMsg#msg.dest,
  DestProcs = [P || P <- Procs, P#proc.pid == DestPid],
  case DestProcs of
    [] ->
      eval_sched_opts(#sys{msgs = RestMsgs, procs = Procs});
    _Other ->
      Time = CurMsg#msg.time,
      [#opt{sem = ?MODULE, type = ?TYPE_MSG, id = Time, rule = ?RULE_SCHED} | eval_sched_opts(#sys{msgs = RestMsgs, procs = Procs})]
  end.


-spec eval_procs_opts(System) ->
  Options when
  System :: #sys{},
  Options :: [#opt{}].

eval_procs_opts(#sys{procs = []}) ->
  [];
eval_procs_opts(#sys{procs = [CurProc | RestProcs]}) ->
  #proc{pid = Pid, env = Env, exp = Exprs, mail = Mail} = CurProc,
  case eval_expr_opt(Exprs, Env, Mail) of
    ?NOT_EXP ->
      eval_procs_opts(#sys{procs = RestProcs});
    Rule ->
      [#opt{sem = ?MODULE, type = ?TYPE_PROC, id = erl_syntax:integer_value(Pid), rule = Rule} | eval_procs_opts(#sys{procs = RestProcs})]
  end.


-spec eval_expr_opt(Expressions, Environment, Mail) ->
  Options when
  Expressions :: erl_parse:abstract_expr() | [erl_parse:abstract_expr()],
  Environment :: erl_eval:binding_struct(),
  Mail :: [#msg{}],
  Options :: ?NOT_EXP | ?RULE_SEQ | ?RULE_CHECK | ?RULE_SEND | ?RULE_RECEIVE | ?RULE_SPAWN | ?RULE_SELF.

eval_expr_opt(Expr, Env, Mail) when is_tuple(Expr) ->
  eval_expr_opt([Expr], Env, Mail);
eval_expr_opt([Expr | Exprs], Env, Mail) when is_tuple(Expr), is_list(Exprs) ->
  case is_expr(Expr) of
    false ->
      case Exprs of
        [] ->
          ?NOT_EXP;
        _ ->
          % If `Expr` is not an expression but there are still other expressions
          % to evaluate then it means we just found a literal in the middle of
          % the program, so we allow to continue.
          ?RULE_SEQ
      end;
    true ->
      case erl_syntax:type(Expr) of
        variable ->
          ?RULE_SEQ;
        match_expr ->
          Body = erl_syntax:match_expr_body(Expr),
          case is_expr(Body) of
            true ->
              eval_expr_opt(Body, Env, Mail);
            false ->
              ?RULE_SEQ
          end;
        infix_expr ->
          Left = erl_syntax:infix_expr_left(Expr),
          case is_expr(Left) of
            true ->
              eval_expr_opt(Left, Env, Mail);
            false ->
              Right = erl_syntax:infix_expr_right(Expr),
              case is_expr(Right) of
                true ->
                  eval_expr_opt(Right, Env, Mail);
                false ->
                  Op = erl_syntax:atom_value(erl_syntax:infix_expr_operator(Expr)),
                  case Op of
                    '!' ->
                      ?RULE_SEND;
                    _ ->
                      ?RULE_SEQ
                  end
              end
          end;
        prefix_expr ->
          Arg = erl_syntax:prefix_expr_argument(Expr),
          case is_expr(Arg) of
            true ->
              eval_expr_opt(Arg, Env, Mail);
            false ->
              ?RULE_SEQ
          end;
        application ->
          Args = erl_syntax:application_arguments(Expr),
          case lists:any(fun is_expr/1, Args) of
            true ->
              eval_expr_opt(Args, Env, Mail);
            false ->
              Op = erl_syntax:application_operator(Expr),
              case erl_syntax:type(Op) of
                module_qualifier ->
                  Module = erl_syntax:module_qualifier_argument(Op),
                  case is_expr(Module) of
                    true ->
                      eval_expr_opt(Module, Env, Mail);
                    false ->
                      case erl_syntax:atom_value(Module) of
                        'erlang' ->
                          Name = erl_syntax:module_qualifier_body(Op),
                          case is_expr(Name) of
                            true ->
                              eval_expr_opt(Name, Env, Mail);
                            false ->
                              case erl_syntax:atom_value(Name) of
                                'spawn' ->
                                  ?RULE_SPAWN;
                                'self' ->
                                  ?RULE_SELF;
                                _ ->
                                  ?RULE_SEQ
                              end
                          end;
                        _ ->
                          ?RULE_SEQ
                      end
                  end;
                _ ->
                  case is_expr(Op) of
                    true ->
                      eval_expr_opt(Op, Env, Mail);
                    false ->
                      % TODO Check for clashes with functions in the same file and/or directory
                      case erl_syntax:atom_value(Op) of
                        'spawn' ->
                          ?RULE_SPAWN;
                        'self' ->
                          ?RULE_SELF;
                        _ ->
                          ?RULE_SEQ
                      end
                  end
              end
          end;
        list ->
          eval_expr_opt(erl_syntax:list_elements(Expr), Env, Mail);
        tuple ->
          eval_expr_opt(erl_syntax:tuple_elements(Expr), Env, Mail);



        'case' ->
          CaseArg = cerl:case_arg(Expr),
          case is_expr(CaseArg) of
            true ->
              eval_expr_opt(CaseArg, Env, Mail);
            false ->
              ?RULE_SEQ
          end;
        'receive' ->
          % SubsExp = utils:substitute(Exp, Env),
          % ?LOG("Exp: " ++ ?TO_STRING(Exp) ++ "\n" ++
          %      "SUB: " ++ ?TO_STRING(SubsExp)),
          % ReceiveClauses = cerl:receive_clauses(SubsExp),
          ReceiveClauses = cerl:receive_clauses(Expr),
          case matchrec(ReceiveClauses, Mail, Env) of
            no_match ->
              ?NOT_EXP;
            _Other ->
              ?RULE_RECEIVE
          end
      end
  end.


ref_add(Id, Ref) ->
  ets:insert(?APP_REF, {Id, Ref}).

ref_lookup(Id) ->
  ets:lookup_element(?APP_REF, Id, 2).
