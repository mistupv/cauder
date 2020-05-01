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

-spec eval_seq(Env, Exp) -> {NewEnv, NewExp, Label} when
  Env :: erl_eval:binding_struct(),
  Exp :: erl_parse:abstract_expr(),
  NewEnv :: erl_eval:binding_struct(),
  NewExp :: Exp | [Exp], % FIXME Simplify
  Label :: tau | {spawn, {Var, FunName, FunArgs}} | {self, Var} | any(), % TODO Add other labels
  % Temporal variable
  Var :: erl_parse:af_variable(),
  % Spawn function name
  FunName :: erl_parse:af_atom(),
  % Spawn function arguments
  FunArgs :: [erl_parse:abstract_expr()].

eval_seq(Env, Exp) ->
  case is_list(Exp) of
    true -> eval_list(Env, Exp);
    false -> eval_seq_1(Env, Exp)
  end.


-spec eval_seq_1(Env, Exp) -> {NewEnv, NewExp, Label} when
  Env :: erl_eval:binding_struct(),
  Exp :: erl_parse:abstract_expr(),
  NewEnv :: erl_eval:binding_struct(),
  NewExp :: Exp | [Exp], % FIXME Simplify
  Label :: tau | {spawn, {Var, FunName, FunArgs}} | {self, Var} | {send, DestPid, MsgValue} | any(), % TODO Add other labels
  % Temporal variable
  Var :: erl_parse:af_variable(),
  % Spawn function name
  FunName :: erl_parse:af_atom(),
  % Spawn function arguments
  FunArgs :: [erl_parse:abstract_expr()],
  DestPid :: erl_parse:af_integer(),
  MsgValue :: erl_parse:abstract_expr().

eval_seq_1(Env, Exp) ->
  %io:format("eval_seq_1:\n\tEnv: ~p\n\tExp: ~p\n", [Env, Exp]),
  case erl_syntax:type(Exp) of
    variable ->
      Name = erl_syntax:variable_name(Exp),
      Binding = orddict:fetch(Name, Env),
      NewValue = erl_syntax:revert(erl_syntax:abstract(Binding)),
      {Env, NewValue, tau};
    match_expr ->
      Body = erl_syntax:match_expr_body(Exp),
      case is_expr(Body) of
        true ->
          {NewEnv, NewBody, Label} = eval_seq(Env, Body),
          % Structure: {match, Pos, Pattern, Body}
          NewMatchExp = setelement(4, Exp, NewBody),
          {NewEnv, NewMatchExp, Label};
        false ->
          % There should be no variables to evaluate so we pass no bindings
          {value, _, Bindings} = erl_eval:expr(Exp, []),
          NewEnv = utils:merge_env(Env, Bindings),
          {NewEnv, [], tau} % TODO Review
      end;
    infix_expr ->
      % TODO Short-circuit expressions
      % TODO Send operator
      Left = erl_syntax:infix_expr_left(Exp),
      case is_expr(Left) of
        true ->
          {NewEnv, NewLeft, Label} = eval_seq(Env, Left),
          % Structure: {op, Pos, Operator, Left, Right}
          NewOp = setelement(4, Exp, NewLeft),
          {NewEnv, NewOp, Label};
        false ->
          Right = erl_syntax:infix_expr_right(Exp),
          case is_expr(Right) of
            true ->
              {NewEnv, NewRight, Label} = eval_seq(Env, Right),
              % Structure: {op, Pos, Operator, Left, Right}
              NewOp = setelement(5, Exp, NewRight),
              {NewEnv, NewOp, Label};
            false ->
              Op = erl_syntax:atom_value(erl_syntax:infix_expr_operator(Exp)),
              case Op of
                '!' ->
                  {Env, Right, {send, Left, Right}};
                _ ->
                  % Infix operators are always built-in, so we just evaluate the expression
                  % There should be no variables to evaluate so we pass no bindings
                  {value, Value, _} = erl_eval:expr(Exp, []),
                  % Use `erl_syntax:abstract/1` and `erl_syntax:revert/1` instead
                  % of `erl_parser:abstract/1` to avoid problems with lists being
                  % represented as strings
                  NewValue = erl_syntax:revert(erl_syntax:abstract(Value)),
                  {Env, NewValue, tau}
              end
          end
      end;
    % FIXME The '-' prefix causes two steps the show the same expression,
    % however they have two different internal representations:
    %  - The number with the operator e.g -(42)
    %  - The negated number e.g (-42)
    prefix_expr ->
      Arg = erl_syntax:prefix_expr_argument(Exp),
      case is_expr(Arg) of
        true ->
          {NewEnv, NewArg, Label} = eval_seq(Env, Arg),
          % Structure: {op, Pos, Operator, Arg}
          NewOp = setelement(4, Exp, NewArg),
          {NewEnv, NewOp, Label};
        false ->
          % Prefix operators are always built-in, so we just evaluate the expression
          % There should be no variables to evaluate so we pass no bindings
          {value, Value, _} = erl_eval:expr(Exp, []),
          % Use `erl_syntax:abstract/1` and `erl_syntax:revert/1` instead
          % of `erl_parser:abstract/1` to avoid problems with lists being
          % represented as strings
          NewValue = erl_syntax:revert(erl_syntax:abstract(Value)),
          {Env, NewValue, tau}
      end;
    application ->
      CallArgs = erl_syntax:application_arguments(Exp),
      case is_expr(CallArgs) of
        true ->
          {NewEnv, NewFunArgs, Label} = eval_seq(Env, CallArgs),
          % Structure: {call, Pos, Operator, Args}
          NewExp = setelement(4, Exp, NewFunArgs),
          {NewEnv, NewExp, Label};
        false ->
          Op = erl_syntax:application_operator(Exp),
          case erl_syntax:type(Op) of
            module_qualifier ->
              Module = erl_syntax:concrete(erl_syntax:module_qualifier_argument(Op)),
              case Module of
                'erlang' ->
                  Name = erl_syntax:concrete(erl_syntax:module_qualifier_body(Op)),
                  eval_bif(Name, Exp, Env);
                % TODO Check if module matches current one
                % TODO Handle calls to functions in other modules
                _ ->
                  % BIF so we just evaluate it
                  % There should be no variables to evaluate so we pass no bindings
                  {value, Value, _} = erl_eval:expr(Exp, []),
                  % Use `erl_syntax:abstract/1` and `erl_syntax:revert/1` instead
                  % of `erl_parser:abstract/1` to avoid problems with lists being
                  % represented as strings
                  NewValue = erl_syntax:revert(erl_syntax:abstract(Value)),
                  {Env, NewValue, tau}
              end;
            atom ->
              Name = erl_syntax:concrete(Op),
              % Check if the function is in this file
              case utils:fundef_lookup(Name, length(CallArgs), ref_lookup(?FUN_DEFS)) of
                {value, FunDef} -> FunClauses = erl_syntax:function_clauses(utils:fundef_rename(FunDef)),
                  % There should be no variables to evaluate so we pass no bindings
                  % TODO `match_clause` looks like an internal function, because it is not documented
                  {Body, Bindings} = erl_eval:match_clause(FunClauses, CallArgs, [], none),
                  % The environment stores the literal value but `match_clause`
                  % returns the bindings as `erl_parse` nodes so we convert them
                  ValueBindings = [{Name, erl_syntax:concrete(Value)} || {Name, Value} <- Bindings],
                  NewEnv = utils:merge_env(Env, ValueBindings),
                  {NewEnv, Body, tau};
                false ->
                  % TODO Look for function in other files in the same directory
                  eval_bif(Name, Exp, Env)
              end
          end
      end;
    list ->
      Head = erl_syntax:list_head(Exp),
      case is_expr(Head) of
        true ->
          {NewEnv, NewHead, Label} = eval_seq(Env, Head),
          % Structure: {cons, Pos, Head, Tail}
          NewList = setelement(3, Exp, NewHead),
          {NewEnv, NewList, Label};
        false ->
          % `list_tail` returns a syntax tree but we want an `erl_parse` node
          Tail = erl_syntax:revert(erl_syntax:list_tail(Exp)),
          {NewEnv, NewTail, Label} = eval_seq(Env, Tail),
          % Structure: {cons, Pos, Head, Tail}
          NewList = setelement(4, Exp, NewTail),
          {NewEnv, NewList, Label}
      end;
    tuple ->
      Elements = erl_syntax:tuple_elements(Exp),
      {NewEnv, NewElements, Label} = eval_seq(Env, Elements),
      % Structure: {tuple, Pos, Elements}
      NewTuple = setelement(3, Exp, NewElements),
      {NewEnv, NewTuple, Label};



    'case' ->
      CaseArg = cerl:case_arg(Exp),
      case is_expr(CaseArg) of
        true ->
          {NewEnv,NewCaseArg,Label} = eval_seq(Env,CaseArg),
          NewExp = cerl:update_c_case(Exp,
                                      NewCaseArg,
                                      cerl:case_clauses(Exp)),
          {NewEnv,NewExp,Label};
        false ->
          %io:format("Env: ~p\n",[Env]),
          %io:format("CaseArg: ~p\n",[CaseArg]),
          CaseClauses = cerl:case_clauses(Exp),
          %io:format("CaseClauses: ~p\n",[CaseClauses]),
          CaseClauses2 = replace_guards(Env,CaseClauses),
          %io:format("CaseClauses2: ~p\n",[CaseClauses2]),
          %CaseClauses3 = init(CaseClauses2),
          CaseArgs =
            case cerl:type(CaseArg) of
              values -> cerl:values_es(CaseArg);
              _ -> [CaseArg]
          end,
          case cerl_clauses:reduce(CaseClauses2,CaseArgs) of
            {true,{Clause,Bindings}} ->
              ClauseBody = cerl:clause_body(Clause),
              NewEnv = utils:merge_env(Env, Bindings),
              {NewEnv,ClauseBody,tau};
            {false,_} ->
              io:fwrite("Error: No matching clause~n")
          end
      end;
    call ->
      CallArgs = cerl:call_args(Exp),
      CallModule = cerl:call_module(Exp),
      CallName = cerl:call_name(Exp),

      case is_expr(CallModule) of
        true ->
          {NewEnv,NewCallModule,Label} = eval_seq(Env,CallModule),
          NewExp = cerl:update_c_call(Exp,
                                      NewCallModule,
                                      CallName,
                                      CallArgs),
          {NewEnv,NewExp,Label};
        false ->
          case is_expr(CallName) of
            true ->
              {NewEnv,NewCallName,Label} = eval_seq(Env,CallName),
              NewExp = cerl:update_c_call(Exp,
                                          CallModule,
                                          NewCallName,
                                          CallArgs),
              {NewEnv,NewExp,Label};
            false ->
              case is_expr(CallArgs) of
                true ->
                  {NewEnv,NewCallArgs,Label} = eval_list(Env,CallArgs),
                  NewExp = cerl:update_c_call(Exp,
                                              CallModule,
                                              CallName,
                                              NewCallArgs),
                  {NewEnv,NewExp,Label};
                false ->
                  case {CallModule, CallName} of
                    {{c_literal,_,'erlang'},{c_literal,_,'spawn'}} ->
                      Var = utils:temp_variable(),
                      FunName = lists:nth(2,CallArgs),
                      FunArgs = utils:list_from_core(lists:nth(3,CallArgs)),
                      {Env,Var,{spawn,{Var,FunName,FunArgs}}};
                    {{c_literal,_,'erlang'},{c_literal, _, 'self'}} ->
                      Var = utils:temp_variable(),
                      {Env, Var, {self, Var}};
                    {{c_literal,_,'erlang'},{c_literal, _, '!'}} ->
                      DestPid = lists:nth(1, CallArgs),
                      MsgValue = lists:nth(2, CallArgs),
                      {Env, MsgValue, {send, DestPid, MsgValue}};
                    {{c_literal,_,'timer'},{c_literal,_,'sleep'}} ->
                      NewExp = cerl:c_atom('ok'),
                      {Env, NewExp, tau};
                    _ ->
			  ToggleOpts = utils_gui:toggle_opts(),
			  AddOptimize = proplists:get_value(?COMP_OPT, ToggleOpts),
			  CompOpts =
			      case AddOptimize of
				  true  -> [to_core,binary];
				  false -> [to_core,binary, no_copt]
			      end,
			  Filename = cerl:concrete(CallModule),
			  Path = ets:lookup_element(?GUI_REF,?LAST_PATH,2),
			  File = filename:join(Path,Filename),
			  case compile:file(File, CompOpts) of
			      {ok, _, CoreForms} ->
				  NoAttsCoreForms = cerl:update_c_module(CoreForms,
									 cerl:module_name(CoreForms),
									 cerl:module_exports(CoreForms),
									 [],
									 cerl:module_defs(CoreForms)),
				  Stripper = fun(Tree) -> cerl:set_ann(Tree, []) end,
				  CleanCoreForms = cerl_trees:map(Stripper, NoAttsCoreForms),
				  FunDefs = cerl:module_defs(CleanCoreForms),
				  ConcName = cerl:concrete(CallName),
				  %ConcArgs = [utils:toErlang(Arg) || Arg <- CallArgs],
				  %io:fwrite("---------------~n"),
				  %io:write(CallName),
				  %io:fwrite("~n---------------~n"),
				  %FunDef = utils:fundef_lookup(CallName, FunDefs),
				  FunDef = utils:fundef_lookup(cerl:c_var({ConcName,cerl:call_arity(Exp)}), FunDefs),
				  NewFunDef = utils:fundef_rename(FunDef),
				  FunBody = cerl:fun_body(NewFunDef),
				  FunArgs = cerl:fun_vars(NewFunDef),
						% standard zip is used here (pretty-printer forces it)
				  NewEnv = utils:merge_env(Env, lists:zip(FunArgs,CallArgs)), %ApplyArgs
				  {NewEnv,FunBody,tau};
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
    seq ->
      SeqArg = cerl:seq_arg(Exp),
      case is_expr(SeqArg) of
        true ->
          {NewEnv,NewSeqArg,Label} = eval_seq(Env,SeqArg),
          NewExp = cerl:update_c_seq(Exp,
                                     NewSeqArg,
                                     cerl:seq_body(Exp)),
          {NewEnv,NewExp,Label};
        false ->
          NewExp = cerl:seq_body(Exp),
          {Env,NewExp,tau}
      end;
    'receive' ->
        Var = utils:temp_variable(),
        % SubsExp = utils:substitute(Exp, Env),
        % {Env, Var, {rec, Var, cerl:receive_clauses(SubsExp)}}
        ReceiveClauses = cerl:receive_clauses(Exp),
        %%ReceiveClauses2 = replace_guards(Env,ReceiveClauses),
        {Env, Var, {rec, Var, ReceiveClauses}}
  end.

eval_bif(Name, Exp, Env) ->
  case Name of
    'spawn' ->
      TmpVar = utils:temp_variable(),
      case erl_syntax:application_arguments(Exp) of
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
      {value, Value, _} = erl_eval:expr(Exp, []),
      % Use `erl_syntax:abstract/1` and `erl_syntax:revert/1` instead
      % of `erl_parser:abstract/1` to avoid problems with lists being
      % represented as strings
      NewValue = erl_syntax:revert(erl_syntax:abstract(Value)),
      {Env, NewValue, tau}
  end.

%init([_X]) -> [];
%nit([A|R]) -> [A|init(R)].

replace_guards(Bindings,Exps) ->
  lists:map(fun({c_clause,L,Pats,Guard,Exp}) ->
          Guard2 = utils:replace_all(Bindings,Guard),
          Guard3 = eval_guard(Guard2),
          {c_clause,L,Pats,Guard3,Exp}
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
		    NewLetArg=eval_guard(LetArg),
		    NewExp = cerl:update_c_let(Exp,
					       cerl:let_vars(Exp),
					       NewLetArg,
					       cerl:let_body(Exp)),
		    eval_guard(NewExp);
		false ->
		    LetVars = cerl:let_vars(Exp),
		    LetEnv =
			case cerl:let_arity(Exp) of
			    1 -> lists:zip(LetVars,[LetArg]);
			    _ ->
				FlatLetArg =
				    case cerl:type(LetArg) of
					values ->
					    cerl:values_es(LetArg);
					_ -> LetArg
				    end,
				lists:zip(LetVars,FlatLetArg)
			end,
		    NewExp = cerl:let_body(Exp),
		    %io:format("2)~w~n",[NewExp]),
		    %io:format("2e)~w~n",[LetEnv]),
		    SubstExp=utils:replace_all(LetEnv,NewExp),
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
	_Other -> Exp
    end.

%%--------------------------------------------------------------------
%% @doc Performs an evaluation step in process Pid, given System
%% @end
%%--------------------------------------------------------------------
-spec eval_step(System, Pid) -> NewSystem when
  System :: #sys{},
  Pid :: erl_parse:af_integer(),
  NewSystem :: #sys{}.

eval_step(System, Pid) ->
  #sys{msgs = Msgs, procs = Procs, trace = Trace} = System,
  {Proc, RestProcs} = utils:select_proc(Procs, Pid),
  #proc{pid = Pid, hist = Hist, env = Env, exp = [Exp | RestExpr], mail = Mail} = Proc,
  {NewEnv, NewExp, Label} = eval_seq(Env, Exp),
  NewSystem =
    case Label of
      tau ->
        NewHist = [{tau, Env, Exp} | Hist],
        NewProc = Proc#proc{hist = NewHist, env = NewEnv, exp = lists:flatten([NewExp], RestExpr)}, % FIXME NewExp can be a list or not
        System#sys{msgs = Msgs, procs = [NewProc | RestProcs]};
      {self, Var} ->
        NewHist = [{self, Env, Exp}|Hist],
        RepExp = utils:replace_variable(Var, Pid, lists:flatten([NewExp], RestExpr)), % FIXME NewExp can be a list or not
        NewProc = Proc#proc{hist = NewHist, env = NewEnv, exp = RepExp},
        System#sys{msgs = Msgs, procs = [NewProc|RestProcs]};
      {send, DestPid, MsgValue} ->
        Time = ref_lookup(?FRESH_TIME),
        ref_add(?FRESH_TIME, Time + 1),
        NewMsg = #msg{dest = DestPid, val = MsgValue, time = Time},
        NewMsgs = [NewMsg|Msgs],
        NewHist = [{send, Env, Exp, DestPid, {MsgValue, Time}}|Hist],
        NewProc = Proc#proc{hist = NewHist, env = NewEnv, exp = lists:flatten([NewExp], RestExpr)}, % FIXME NewExp can be a list or not
        TraceItem = #trace{type = ?RULE_SEND, from = Pid, to = DestPid, val = MsgValue, time = Time},
        NewTrace = [TraceItem|Trace],
        System#sys{msgs = NewMsgs, procs = [NewProc|RestProcs], trace = NewTrace};
      {spawn, {Var, FunName, FunArgs}} ->
        SpawnPid = erl_parse:abstract(utils:fresh_pid()),
        SpawnProc = #proc{
          pid = SpawnPid,
          exp = [erl_syntax:revert(erl_syntax:application(FunName, FunArgs))],
          spf = {erl_syntax:atom_value(FunName), length(FunArgs)}
        },
        NewHist = [{spawn, Env, Exp, SpawnPid} | Hist],
        RepExp = utils:replace_variable(Var, SpawnPid, lists:flatten([NewExp], RestExpr)), % FIXME NewExp can be a list or not
        NewProc = Proc#proc{hist = NewHist, env = NewEnv, exp = RepExp},
        TraceItem = #trace{type = ?RULE_SPAWN, from = Pid, to = SpawnPid},
        NewTrace = [TraceItem|Trace],
        System#sys{msgs = Msgs, procs = [NewProc|[SpawnProc|RestProcs]], trace = NewTrace};
      {rec, Var, ReceiveClauses} ->
        {Bindings, RecExp, ConsMsg, NewMail} = matchrec(ReceiveClauses, Mail, NewEnv),
        UpdatedEnv = utils:merge_env(NewEnv, Bindings),
        RepExp = utils:replace_variable(Var, RecExp, lists:flatten([NewExp], RestExpr)), % FIXME NewExp can be a list or not
        NewHist = [{rec, Env, Exp, ConsMsg, Mail}|Hist],
        NewProc = Proc#proc{hist = NewHist, env = UpdatedEnv, exp = RepExp, mail = NewMail},
        {MsgValue, Time} = ConsMsg,
        TraceItem = #trace{type = ?RULE_RECEIVE, from = Pid, val = MsgValue, time = Time},
        NewTrace = [TraceItem|Trace],
        System#sys{msgs = Msgs, procs = [NewProc|RestProcs], trace = NewTrace}
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
  System#sys{msgs = RestMsgs, procs = [NewProc|RestProcs]}.

%%--------------------------------------------------------------------
%% @doc Checks if Expr is an expression or not
%% @end
%%--------------------------------------------------------------------

-spec is_expr(Expr) -> boolean() when
  Expr :: erl_parse:abstract_expr() | [erl_parse:abstract_expr()].

is_expr([]) ->
  false;
is_expr(Expr) when is_list(Expr) ->
  lists:any(fun is_expr/1, Expr);
is_expr(Expr) ->
  case erl_syntax:type(Expr) of
    list ->
      is_expr(erl_syntax:list_elements(Expr));
    tuple ->
      is_expr(erl_syntax:tuple_elements(Expr));
    _ ->
      not erl_syntax:is_literal(Expr)
  end.

eval_list(Env,[Exp|Exps]) ->
  case is_expr(Exp) of
    true ->
      {NewEnv,NewExp,Label} = eval_seq(Env,Exp),
      {NewEnv,[NewExp|Exps],Label};
    false ->
      {NewEnv,NewExp,Label} = eval_list(Env,Exps),
      {NewEnv,[Exp|NewExp],Label}
  end.

matchrec(Clauses, Mail,Env) ->
  matchrec(Clauses, Mail, [],Env).

matchrec(_, [], _, _) ->
  no_match;
matchrec(Clauses, [CurMsg|RestMsgs], AccMsgs, Env) ->
  {MsgValue, _MsgTime} = CurMsg,
  %io:format("matchrec (MsgValue): ~p~n",[MsgValue]),
  %io:format("matchrec (Clauses): ~p~n",[Clauses]),
  %%preprocessing is used to propagate matching bindings to guards
  NewClauses = preprocessing_clauses(Clauses,MsgValue,Env),
  %io:format("matchrec (NewClauses): ~p~n",[NewClauses]),
  case cerl_clauses:reduce(NewClauses, [MsgValue]) of
    {true, {Clause, Bindings}} ->
      ClauseBody = cerl:clause_body(Clause),
      NewMsgs =  AccMsgs ++ RestMsgs,
      {Bindings, ClauseBody, CurMsg, NewMsgs};
    {false, []} ->
	  matchrec(Clauses, RestMsgs, AccMsgs ++ [CurMsg],Env);
      {false, [Clause|OtherClauses]} -> io:format("CauDEr: Unsupported pattern, some behaviours may be missed ~n~w~n",[Clause]),
			matchrec(Clauses, RestMsgs, AccMsgs ++ [CurMsg],Env)
  end.

preprocessing_clauses(Clauses,Msg,Env) ->
  lists:map(fun({c_clause,L,Pats,Guard,Exp}) ->
  	%io:format("Clauses: ~p~n",[Clauses]),
  	%io:format("match (Pats/[Msg]) ~p~n~p~n",[Pats,[Msg]]),
  	%io:format("--result: ~p~n",[cerl_clauses:match_list(Pats,[Msg])]),
    case cerl_clauses:match_list(Pats,[Msg]) of
      {true,Bindings} -> Guard2 = utils:replace_all(Bindings++Env,Guard),
      					 %io:format("calling eval_guard (Bindings/Guard/Guard2): ~n~p~n~p~n~p~n",[Bindings++Env,Guard,Guard2]),
                         Guard3 = eval_guard(Guard2),
                         {c_clause,L,Pats,Guard3,Exp};
      _ -> {c_clause,L,Pats,Guard,Exp}
    end
  end, Clauses).

%%--------------------------------------------------------------------
%% @doc Gets the evaluation options for a given System
%% @end
%%--------------------------------------------------------------------

-spec eval_opts(System) -> Options when
  System::#sys{},
  Options::[#opt{}].

eval_opts(System) ->
  SchedOpts = eval_sched_opts(System),
  ProcsOpts = eval_procs_opts(System),
  SchedOpts ++ ProcsOpts.


-spec eval_sched_opts(System) -> Options when
  System::#sys{},
  Options::[#opt{}].

eval_sched_opts(#sys{msgs = []}) ->
  [];
eval_sched_opts(#sys{msgs = [CurMsg|RestMsgs], procs = Procs}) ->
  DestPid = CurMsg#msg.dest,
  DestProcs = [ P || P <- Procs, P#proc.pid == DestPid],
  case DestProcs of
    [] ->
      eval_sched_opts(#sys{msgs = RestMsgs, procs = Procs});
    _Other ->
      Time = CurMsg#msg.time,
      [#opt{sem = ?MODULE, type = ?TYPE_MSG, id = Time, rule = ?RULE_SCHED}|eval_sched_opts(#sys{msgs = RestMsgs, procs = Procs})]
  end.


-spec eval_procs_opts(System) -> Options when
  System::#sys{},
  Options::[#opt{}].

eval_procs_opts(#sys{procs = []}) ->
  [];
eval_procs_opts(#sys{procs = [CurProc | RestProcs]}) ->
  #proc{pid = Pid, env = Env, exp = [Expr | _], mail = Mail} = CurProc,
  case eval_expr_opt(Expr, Env, Mail) of
    ?NOT_EXP ->
      eval_procs_opts(#sys{procs = RestProcs});
    Opt ->
      [Opt#opt{sem = ?MODULE, type = ?TYPE_PROC, id = erl_syntax:concrete(Pid)} | eval_procs_opts(#sys{procs = RestProcs})]
  end.


-spec eval_expr_opt(Expr, Env, Mail) -> Opt when
  Expr :: [erl_parse:abstract_expr()],
  Env :: erl_eval:binding_struct(),
  Mail :: [#msg{}],
  Opt :: ?NOT_EXP | #opt{}.

eval_expr_opt(Expr, Env, Mail) ->
  case is_expr(Expr) of
    false ->
      % TODO Handle literals
      ?NOT_EXP;
    true ->
      case erl_syntax:type(Expr) of
        variable ->
          #opt{rule = ?RULE_SEQ};
        match_expr ->
          Body = erl_syntax:match_expr_body(Expr),
          case is_expr(Body) of
            true ->
              eval_expr_opt(Body, Env, Mail);
            false ->
              #opt{rule = ?RULE_SEQ}
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
                      #opt{rule = ?RULE_SEND};
                    _ ->
                      #opt{rule = ?RULE_SEQ}
                  end
              end
          end;
        prefix_expr ->
          Arg = erl_syntax:prefix_expr_argument(Expr),
          case is_expr(Arg) of
            true ->
              eval_expr_opt(Arg, Env, Mail);
            false ->
              #opt{rule = ?RULE_SEQ}
          end;
        application ->
          Args = erl_syntax:application_arguments(Expr),
          case is_expr(Args) of
            true ->
              eval_expr_list_opt(Args, Env, Mail);
            false ->
              Op = erl_syntax:application_operator(Expr),
              case erl_syntax:type(Op) of
                module_qualifier ->
                  Module = erl_syntax:concrete(erl_syntax:module_qualifier_argument(Op)),
                  case Module of
                    'erlang' ->
                      Name = erl_syntax:concrete(erl_syntax:module_qualifier_body(Op)),
                      case Name of
                        'spawn' ->
                          #opt{rule = ?RULE_SPAWN};
                        'self' ->
                          #opt{rule = ?RULE_SELF};
                        _ ->
                          #opt{rule = ?RULE_SEQ}
                      end;
                    _ ->
                      #opt{rule = ?RULE_SEQ}
                  end;
                atom ->
                  Name = erl_syntax:concrete(Op),
                  % TODO Check for clashes with functions in the same file and/or directory
                  case Name of
                    'spawn' ->
                      #opt{rule = ?RULE_SPAWN};
                    'self' ->
                      #opt{rule = ?RULE_SELF};
                    _ ->
                      #opt{rule = ?RULE_SEQ}
                  end
              end
          end;
        list ->
          eval_expr_list_opt(erl_syntax:list_elements(Expr), Env, Mail);
        tuple ->
          eval_expr_list_opt(erl_syntax:tuple_elements(Expr), Env, Mail);


        'let' ->
          LetArg = cerl:let_arg(Expr),
          case is_expr(LetArg) of
            true ->
              eval_expr_opt(LetArg, Env, Mail);
            false ->
              #opt{rule = ?RULE_SEQ}
          end;
        seq ->
          SeqArg = cerl:seq_arg(Expr),
          case is_expr(SeqArg) of
            true ->
              eval_expr_opt(SeqArg, Env, Mail);
            false ->
              #opt{rule = ?RULE_SEQ}
          end;
        'case' ->
          CaseArg = cerl:case_arg(Expr),
          case is_expr(CaseArg) of
            true ->
              eval_expr_opt(CaseArg, Env, Mail);
            false ->
              #opt{rule = ?RULE_SEQ}
          end;
        call ->
          CallModule = cerl:call_module(Expr),
          case is_expr(CallModule) of
            true ->
              eval_expr_opt(CallModule, Env, Mail);
            false ->
              CallName = cerl:call_name(Expr),
              case is_expr(CallName) of
                true ->
                  eval_expr_opt(CallName, Env, Mail);
                false ->
                  CallArgs = cerl:call_args(Expr),
                  case is_expr(CallArgs) of
                    true ->
                      eval_expr_list_opt(CallArgs, Env, Mail);
                    false ->
                      case {CallModule, CallName} of
                        {{c_literal, _, 'erlang'},{c_literal, _, 'spawn'}} -> #opt{rule = ?RULE_SPAWN};
                        {{c_literal, _, 'erlang'},{c_literal, _, 'self'}} -> #opt{rule = ?RULE_SELF};
                        {{c_literal, _, 'erlang'},{c_literal, _, '!'}} -> #opt{rule = ?RULE_SEND};
                        _ -> #opt{rule = ?RULE_SEQ}
                      end
                  end
              end
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
              #opt{rule = ?RULE_RECEIVE}
          end
      end
  end.

eval_expr_list_opt([], _, _) ->
  ?NOT_EXP;
eval_expr_list_opt([Expr | RestExpr], Env, Mail) ->
  case is_expr(Expr) of
    true -> eval_expr_opt(Expr, Env, Mail);
    false -> eval_expr_list_opt(RestExpr, Env, Mail)
  end.

ref_add(Id, Ref) ->
    ets:insert(?APP_REF, {Id, Ref}).

ref_lookup(Id) ->
    ets:lookup_element(?APP_REF, Id, 2).
