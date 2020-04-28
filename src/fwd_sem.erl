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
  Exp :: erl_syntax:syntaxTree(), % TODO Less generic type
  NewEnv :: erl_eval:binding_struct(),
  NewExp :: Exp | [Exp], % FIXME Simplify
  Label :: tau | any(). % TODO Add other labels

eval_seq(Env, Exp) ->
  case is_list(Exp) of
    true -> eval_list(Env, Exp);
    false -> eval_seq_1(Env, Exp)
  end.


-spec eval_seq_1(Env, Exp) -> {NewEnv, NewExp, Label} when
  Env :: erl_eval:binding_struct(),
  Exp :: erl_syntax:syntaxTree(), % TODO Less generic type
  NewEnv :: erl_eval:binding_struct(),
  NewExp :: Exp | [Exp], % FIXME Simplify
  Label :: tau | any(). % TODO Add other labels

eval_seq_1(Env, Exp) ->
  case erl_syntax:type(Exp) of
    variable ->
      Name = erl_syntax:variable_name(Exp),
      Binding = orddict:fetch(Name, Env),
      % Environment stores the position where variables were last assigned so,
      % to avoid problems, when we retrieve a value we change the position
      % of the new node to match the position of the variable node.
      %
      % We also assume that we have an `erl_parse` node with position
      % information in element 2.
      NewValue = setelement(2, Binding, erl_syntax:get_pos(Exp)),
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
          Pattern = erl_syntax:match_expr_pattern(Exp),
          %io:format("Body: ~p\n", [Body]),
          %io:format("Pattern: ~p\n", [Pattern]),
          Bindings =
            case erl_syntax:type(Pattern) of
              nil ->
                [];
              variable ->
                [{erl_syntax:variable_name(Pattern), Body}];
              tuple ->
                PatternElements = erl_syntax:tuple_elements(Pattern),
                BodyElements = erl_syntax:tuple_elements(Body),

                TupleBindings = lists:zipwith(
                  fun(Var, Val) -> {erl_syntax:variable_name(Var), Val} end,
                  PatternElements,
                  BodyElements
                ),

                [Binding || Binding = {VarName, _} <- TupleBindings, VarName =/= '_'];
              list ->
                PatternPrefix = erl_syntax:list_prefix(Pattern),
                PatternSuffix = erl_syntax:list_suffix(Pattern),
                {BodyPrefix, BodySuffix} = lists:split(length(PatternPrefix), erl_syntax:list_elements(Body)),

                PrefixBindings = lists:zipwith(
                  fun(Var, Val) -> {erl_syntax:variable_name(Var), Val} end,
                  PatternPrefix,
                  BodyPrefix
                ),

                SuffixBinding = {erl_syntax:variable_name(PatternSuffix), erl_syntax:revert(erl_syntax:list(BodySuffix))},

                [Binding || Binding = {VarName, _} <- PrefixBindings ++ [SuffixBinding], VarName =/= '_'];
              Other ->
                error(io_lib:format("Unsupported type: ~p", [Other])) % TODO Add more cases. See: 'erl_parse:af_pattern()'
            end,
          NewEnv = utils:merge_env(Env, Bindings),
          {NewEnv, [], tau} % TODO Review
      end;
    infix_expr ->
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
              Op = erl_syntax:operator_name(erl_syntax:infix_expr_operator(Exp)),
              io:format("Op: ~p\n\tLeft: ~p\n\tRight: ~p\n\t", [Op, Left, Right]),
              Result = apply(erlang, Op, [erl_syntax:concrete(Left), erl_syntax:concrete(Right)]),
              ResultStr = lists:flatten(io_lib:format("~p", [Result])) ++ ".",
              {ok, Tokens, _} = erl_scan:string(ResultStr),
              {ok, [NewOp | []]} = erl_parse:parse_exprs(Tokens),
              {Env, NewOp, tau}
          end
      end;
    application ->
      FunArgs = erl_syntax:application_arguments(Exp),
      case is_expr(FunArgs) of
        true ->
          {NewEnv, NewFunArgs, Label} = eval_seq(Env, FunArgs),
          % Structure: {call, Pos, Operator, Args}
          NewExp = setelement(4, Exp, NewFunArgs),
          {NewEnv, NewExp, Label};
        false ->
          FunName = erl_syntax:application_operator(Exp),
          FunDef = utils:fundef_lookup(erl_syntax:atom_value(FunName), length(FunArgs), ref_lookup(?FUN_DEFS)),
          FunClauses = erl_syntax:function_clauses(utils:fundef_rename(FunDef)),
          {Body, Bindings} = erl_eval:match_clause(FunClauses, FunArgs, erl_eval:new_bindings(), none), % TODO Review newBindings
          NewEnv = utils:merge_env(Env, Bindings),
          {NewEnv, Body, tau}
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
          % `revert` is required to get an `erl_parse` node
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
                      VarNum = ref_lookup(?FRESH_VAR),
                      ref_add(?FRESH_VAR, VarNum + 1),
                      Var = utils:build_var(VarNum),
                      FunName = lists:nth(2,CallArgs),
                      FunArgs = utils:list_from_core(lists:nth(3,CallArgs)),
                      {Env,Var,{spawn,{Var,FunName,FunArgs}}};
                    {{c_literal,_,'erlang'},{c_literal, _, 'self'}} ->
                      VarNum = ref_lookup(?FRESH_VAR),
                      ref_add(?FRESH_VAR, VarNum + 1),
                      Var = utils:build_var(VarNum),
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
        VarNum = ref_lookup(?FRESH_VAR),
        ref_add(?FRESH_VAR, VarNum + 1),
        Var = utils:build_var(VarNum),
        % SubsExp = utils:substitute(Exp, Env),
        % {Env, Var, {rec, Var, cerl:receive_clauses(SubsExp)}}
        ReceiveClauses = cerl:receive_clauses(Exp),
        %%ReceiveClauses2 = replace_guards(Env,ReceiveClauses),
        {Env, Var, {rec, Var, ReceiveClauses}}
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
  Pid :: erl_syntax:syntaxTree(), % TODO Less generic type
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
        RepExp = utils:replace(Var, Pid, NewExp),
        NewProc = Proc#proc{hist = NewHist, env = NewEnv, exp = RepExp},
        System#sys{msgs = Msgs, procs = [NewProc|RestProcs]};
      {send, DestPid, MsgValue} ->
        Time = ref_lookup(?FRESH_TIME),
        ref_add(?FRESH_TIME, Time + 1),
        NewMsg = #msg{dest = DestPid, val = MsgValue, time = Time},
        NewMsgs = [NewMsg|Msgs],
        NewHist = [{send, Env, Exp, DestPid, {MsgValue, Time}}|Hist],
        NewProc = Proc#proc{hist = NewHist, env = NewEnv, exp = NewExp},
        TraceItem = #trace{type = ?RULE_SEND, from = Pid, to = DestPid, val = MsgValue, time = Time},
        NewTrace = [TraceItem|Trace],
        System#sys{msgs = NewMsgs, procs = [NewProc|RestProcs], trace = NewTrace};
      {spawn, {Var, FunName, FunArgs}} ->
        PidNum = ref_lookup(?FRESH_PID),
        ref_add(?FRESH_PID, PidNum + 1),
        SpawnPid = cerl:c_int(PidNum),
        ArgsLen = length(FunArgs),
        FunCall = cerl:c_var({cerl:concrete(FunName), ArgsLen}),
        SpawnProc = #proc{pid = SpawnPid,
                          env = [],
                          exp = cerl:c_apply(FunCall,FunArgs),
                          spf = cerl:var_name(FunCall)},
        NewHist = [{spawn, Env, Exp, SpawnPid}|Hist],
        RepExp = utils:replace(Var, SpawnPid, NewExp),
        NewProc = Proc#proc{hist = NewHist, env = NewEnv, exp = RepExp},
        TraceItem = #trace{type = ?RULE_SPAWN, from = Pid, to = SpawnPid},
        NewTrace = [TraceItem|Trace],
        System#sys{msgs = Msgs, procs = [NewProc|[SpawnProc|RestProcs]], trace = NewTrace};
      {rec, Var, ReceiveClauses} ->
        {Bindings, RecExp, ConsMsg, NewMail} = matchrec(ReceiveClauses, Mail, NewEnv),
        UpdatedEnv = utils:merge_env(NewEnv, Bindings),
        RepExp = utils:replace(Var, RecExp, NewExp),
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
  Expr :: erl_syntax:syntaxTree() | [erl_syntax:syntaxTree()]. % TODO Less generic type

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
eval_opts(System) ->
  SchedOpts = eval_sched_opts(System),
  ProcsOpts = eval_procs_opts(System),
  SchedOpts ++ ProcsOpts.

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

eval_expr_opt(Expr, Env, Mail) ->
  case is_expr(Expr) of
    false ->
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
                  #opt{rule = ?RULE_SEQ}
              end
          end;
        application ->
          Args = erl_syntax:application_arguments(Expr),
          case is_expr(Args) of
            true ->
              eval_expr_list_opt(Args, Env, Mail);
            false ->
              #opt{rule = ?RULE_SEQ}
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
