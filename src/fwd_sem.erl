%%%-------------------------------------------------------------------
%%% @doc Some functions that implement the forward (reversible)
%%% semantics for Erlang. These can be divided into functions to get
%%% the evaluation options and functions to perform the evaluation
%%% @end
%%%-------------------------------------------------------------------

-module(fwd_sem).
-export([eval_step/2, eval_sched/2]).
-export([eval_opts/1, eval_procs_opts/1, eval_sched_opts/1]).

-include("cauder.hrl").

-export_type([af_function_decl/0, abstract_expr/0, af_integer/0, environment/0]).

%% Start of Abstract Format

-type anno() :: erl_anno:anno().

-type af_function_decl() :: {'function', anno(), function_name(), arity(), af_clause_seq()}.

-type abstract_expr() :: erl_parse:abstract_expr().

-type af_local_call() :: {'call', anno(), af_local_function(), af_args()}.

-type af_remote_call() :: {'call', anno(), af_remote_function(), af_args()}.

-type af_args() :: [abstract_expr()].

-type af_local_function() :: abstract_expr().

-type af_remote_function() :: {'remote', anno(), abstract_expr(), abstract_expr()}.

-type af_case() :: {'case', anno(), abstract_expr(), af_clause_seq()}.

-type af_clause_seq() :: [af_clause(), ...].

-type af_receive() :: {'receive', anno(), af_clause_seq()}
                    | {'receive', anno(), af_clause_seq(), abstract_expr(), af_body()}.

-type af_clause() :: {'clause', anno(), [af_pattern()], af_guard_seq(), af_body()}.

-type af_body() :: [abstract_expr(), ...].

-type af_guard_seq() :: [af_guard()].

-type af_guard() :: [af_guard_test(), ...].

-type af_guard_test() :: af_literal()
                       | af_variable()
                       | af_tuple(af_guard_test())
                       | af_nil()
                       | af_cons(af_guard_test())
                       | af_bin(af_guard_test())
                       | af_binary_op(af_guard_test())
                       | af_unary_op(af_guard_test())
                       | af_record_creation(af_guard_test())
                       | af_record_index()
                       | af_record_field_access(af_guard_test())
                       | af_map_creation(abstract_expr())
                       | af_map_update(abstract_expr())
                       | af_guard_call()
                       | af_remote_guard_call().

-type af_record_field_access(T) :: {'record_field', anno(), T, record_name(), af_field_name()}.

-type af_map_creation(T) :: {'map', anno(), [af_assoc(T)]}.

-type af_map_update(T) :: {'map', anno(), T, [af_assoc(T)]}.

-type af_assoc(T) :: {'map_field_assoc', anno(), T, T}
                   | af_assoc_exact(T).

-type af_assoc_exact(T) :: {'map_field_exact', anno(), T, T}.

-type af_guard_call() :: {'call', anno(), function_name(), [af_guard_test()]}.

-type af_remote_guard_call() :: {'call', anno(), {'remote', anno(), af_lit_atom('erlang'), af_atom()}, [af_guard_test()]}.

-type af_pattern() :: af_literal()
                    | af_match(af_pattern())
                    | af_variable()
                    | af_tuple(af_pattern())
                    | af_nil()
                    | af_cons(af_pattern())
                    | af_bin(af_pattern())
                    | af_binary_op(af_pattern())
                    | af_unary_op(af_pattern())
                    | af_record_creation(af_pattern())
                    | af_record_index()
                    | af_map_pattern().

-type af_record_index() :: {'record_index', anno(), record_name(), af_field_name()}.

-type af_record_creation(T) :: {'record', anno(), record_name(), [af_record_field(T)]}.

-type af_record_field(T) :: {'record_field', anno(), af_field_name(), T}.

-type af_map_pattern() :: {'map', anno(), [af_assoc_exact(abstract_expr())]}.

-type af_literal() :: af_atom()
                    | af_character()
                    | af_float()
                    | af_integer()
                    | af_string().

-type af_atom() :: af_lit_atom(atom()).

-type af_lit_atom(A) :: {'atom', anno(), A}.

-type af_character() :: {'char', anno(), char()}.

-type af_float() :: {'float', anno(), float()}.

-type af_integer() :: {'integer', anno(), non_neg_integer()}.

-type af_string() :: {'string', anno(), string()}.

-type af_match(T) :: {'match', anno(), af_pattern(), T}.

-type af_variable() :: {'var', anno(), atom()}.

-type af_tuple(T) :: {'tuple', anno(), [T]}.

-type af_nil() :: {'nil', anno()}.

-type af_cons(T) :: {'cons', anno(), T, T}.

-type af_bin(T) :: {'bin', anno(), [af_binelement(T)]}.

-type af_binelement(T) :: {'bin_element', anno(), T, af_binelement_size(), type_specifier_list()}.

-type af_binelement_size() :: 'default' | abstract_expr().

-type af_binary_op(T) :: {'op', anno(), binary_op(), T, T}.

-type binary_op() :: '/' | '*' | 'div' | 'rem' | 'band' | 'and' | '+' | '-'
                   | 'bor' | 'bxor' | 'bsl' | 'bsr' | 'or' | 'xor' | '++'
                   | '--' | '==' | '/=' | '=<' | '<'  | '>=' | '>' | '=:='
                   | '=/='.

-type af_unary_op(T) :: {'op', anno(), unary_op(), T}.

-type unary_op() :: '+' | '-' | 'bnot' | 'not'.

%% See also lib/stdlib/{src/erl_bits.erl,include/erl_bits.hrl}.
-type type_specifier_list() :: 'default' | [type_specifier(), ...].

-type type_specifier() :: type()
                        | signedness()
                        | endianness()
                        | unit().

-type type() :: 'integer'
              | 'float'
              | 'binary'
              | 'bytes'
              | 'bitstring'
              | 'bits'
              | 'utf8'
              | 'utf16'
              | 'utf32'.

-type signedness() :: 'signed' | 'unsigned'.

-type endianness() :: 'big' | 'little' | 'native'.

-type unit() :: {'unit', 1..256}.

-type record_name() :: atom().

-type af_field_name() :: af_atom().

-type function_name() :: atom().

%% End of Abstract Format

-type environment() :: orddict:orddict(atom(), term()).

-type result() :: {environment(), [abstract_expr()], label()}.

-type label() :: label_tau()
               | label_spawn()
               | label_self()
               | label_send()
               | label_rec().

-type label_tau() :: tau.
-type label_spawn() :: {spawn, {af_variable(), af_atom(), list(abstract_expr())}}.
-type label_self() :: {self, af_variable()}.
-type label_send() :: {send, af_integer(), abstract_expr()}.
-type label_rec() :: {rec, af_variable(), af_clause_seq()}.


%% =====================================================================
%% @doc Evaluates the first non-literal expression for the given `Expressions`
%% list and returns a tuple with an updated environment, the list of expressions
%% that resulted from the evaluation, and a label.

-spec eval_expr_list(environment(), [abstract_expr()]) -> result().

eval_expr_list(Env, [Expr | Exprs]) when is_tuple(Expr) ->
  case is_expr(Expr, Env) of
    true ->
      {NewEnv, NewExprs, Label} = eval_expr(Env, Expr),
      {NewEnv, NewExprs ++ Exprs, Label};
    false ->
      {NewEnv, NewExprs, Label} = eval_expr_list(Env, Exprs),
      {NewEnv, [Expr | NewExprs], Label}
  end.


%% =====================================================================
%% @doc Evaluates the given `Expression` and returns a tuple with an updated
%% environment, the expression that resulted from the evaluation, and a label.

-spec eval_expr(environment(), abstract_expr()) -> result().

eval_expr(Env, Expr) when is_tuple(Expr) ->
  case is_expr(Expr, Env) of
    false ->
      % If we find a literal we just consume it.
      {Env, [], tau};
    true ->
      case erl_syntax:type(Expr) of
        variable -> eval_variable(Env, Expr);
        match_expr -> eval_match_expr(Env, Expr);
        infix_expr -> eval_infix_expr(Env, Expr);
        prefix_expr -> eval_prefix_expr(Env, Expr);
        application -> eval_application(Env, Expr);
        list -> eval_list(Env, Expr);
        tuple -> eval_tuple(Env, Expr);
        case_expr -> eval_case_expr(Env, Expr);
        receive_expr -> eval_receive(Env, Expr)
      end
  end.


-spec eval_variable(environment(), af_variable()) -> result().

eval_variable(Env, {var, _, Name}) when is_atom(Name) ->
  Value = orddict:fetch(Name, Env),
  AbstractValue = abstract(Value),
  {Env, [AbstractValue], tau}.


-spec eval_match_expr(environment(), af_match(abstract_expr())) -> result().

eval_match_expr(Env, MatchExpr = {match, Line, Pattern, Body}) ->
  case is_expr(Pattern, Env) of
    true ->
      {NewEnv, [NewPattern], Label} = eval_expr(Env, Pattern),
      {NewEnv, [{match, Line, NewPattern, Body}], Label};
    false ->
      case is_expr(Body, Env) of
        true ->
          {NewEnv, [NewBody], Label} = eval_expr(Env, Body),
          {NewEnv, [{match, Line, Pattern, NewBody}], Label};
        false ->
          % There should be no variables to evaluate so we pass no bindings
          {value, Value, Bindings} = erl_eval:expr(MatchExpr, []),
          NewEnv = utils:merge_env(Env, Bindings),
          {NewEnv, [(abstract(Value))], tau}
      end
  end.


-spec eval_infix_expr(environment(), af_binary_op(abstract_expr())) -> result().

eval_infix_expr(Env, {op, Line, Operator, Left, Right}) when is_atom(Operator) ->
  case is_expr(Left, Env) of
    true ->
      {NewEnv, [NewLeft], Label} = eval_expr(Env, Left),
      {NewEnv, [{op, Line, Operator, NewLeft, Right}], Label};
    false ->
      case {erl_parse:normalise(Left), Operator} of
        {'false', 'andalso'} -> {Env, Left, tau};
        {'true', 'orelse'} -> {Env, Left, tau};
        _ ->
          case is_expr(Right, Env) of
            true ->
              {NewEnv, [NewRight], Label} = eval_expr(Env, Right),
              {NewEnv, [{op, Line, Operator, Left, NewRight}], Label};
            false ->
              case Operator of
                '!' -> {Env, [Right], {send, Left, Right}};
                _ ->
                  % Infix operators are always built-in, so we just evaluate the expression
                  Value = apply(erlang, Operator, [erl_parse:normalise(Left), erl_parse:normalise(Right)]),
                  {Env, [(abstract(Value))], tau}
              end
          end
      end
  end.


-spec eval_prefix_expr(environment(), af_unary_op(abstract_expr())) -> result().

eval_prefix_expr(Env, {op, Pos, Operator, Argument}) when is_atom(Operator) ->
  % FIXME The '-' prefix causes two steps the show the same expression,
  % however they have two different internal representations:
  %  - The number with the operator e.g -(42)
  %  - The negated number e.g (-42)
  case is_expr(Argument, Env) of
    true ->
      {NewEnv, [NewArgument], Label} = eval_expr(Env, Argument),
      {NewEnv, [{op, Pos, Operator, NewArgument}], Label};
    false ->
      % Prefix operators are always built-in, so we just evaluate the expression
      Value = apply(erlang, Operator, [erl_parse:normalise(Argument)]),
      {Env, [(abstract(Value))], tau}
  end.


-spec eval_application(environment(), af_local_call() | af_remote_call()) -> result().

eval_application(Env, RemoteCall = {call, CallPos, RemoteFun = {remote, RemotePos, Module, Name}, Arguments}) ->
  case is_expr(Module, Env) of
    true ->
      {NewEnv, [NewModule], Label} = eval_expr(Env, Module),
      {NewEnv, [{call, CallPos, {remote, RemotePos, NewModule, Name}, Arguments}], Label};
    false ->
      case is_expr(Name, Env) of
        true ->
          {NewEnv, [NewName], Label} = eval_expr(Env, Name),
          {NewEnv, [{call, CallPos, {remote, RemotePos, Module, NewName}, Arguments}], Label};
        false ->
          case lists:any(fun(Arg) -> is_expr(Arg, Env) end, Arguments) of
            true ->
              {NewEnv, NewArguments, Label} = eval_expr_list(Env, Arguments),
              {NewEnv, [{call, CallPos, RemoteFun, NewArguments}], Label};
            false ->
              case erl_syntax:atom_value(Module) of
                'erlang' -> eval_bif(Env, RemoteCall);
                _ ->
                  % TODO Check if module matches current one
                  % TODO Handle calls to functions in other modules
                  error(not_implemented)
              end
          end
      end
  end;
eval_application(Env, LocalCall = {call, Pos, LocalFun, Arguments}) ->
  case is_expr(LocalFun, Env) of
    true ->
      {NewEnv, [NewLocalFun], Label} = eval_expr(Env, LocalFun),
      {NewEnv, [{call, Pos, NewLocalFun, Arguments}], Label};
    false ->
      case lists:any(fun(Arg) -> is_expr(Arg, Env) end, Arguments) of
        true ->
          {NewEnv, NewArguments, Label} = eval_expr_list(Env, Arguments),
          {NewEnv, [{call, Pos, LocalFun, NewArguments}], Label};
        false ->
          Name = erl_syntax:atom_value(LocalFun),
          % Check if the function is in this file
          case utils:fundef_lookup({Name, length(Arguments)}, ref_lookup(?FUN_DEFS)) of
            {value, FunDef} ->
              FunClauses = erl_syntax:function_clauses(utils:fundef_rename(FunDef)),
              % There should be no variables to evaluate so we pass no bindings
              % TODO `match_clause` looks like an internal function, because it is not documented
              {Body, Bindings} = erl_eval:match_clause(FunClauses, Arguments, [], none),
              % The environment stores the literal value but `match_clause`
              % returns the bindings as `erl_parse` nodes so we convert them
              ValueBindings = [{Var, erl_syntax:concrete(Val)} || {Var, Val} <- Bindings],
              NewEnv = utils:merge_env(Env, ValueBindings),
              {NewEnv, Body, tau};
            false ->
              % TODO If the function name was a variable then BIF should not be evaluated
              % TODO Look for function in other files in the same directory
              eval_bif(Env, LocalCall)
          end
      end
  end.


-spec eval_bif(environment(), af_local_call() | af_remote_call()) -> result().

eval_bif(Env, {call, CallPos, {atom, NamePos, Name}, Arguments}) ->
  eval_bif(Env, {call, CallPos, {remote, NamePos, erlang, Name}, Arguments});
eval_bif(Env, {call, _, {remote, _, erlang, 'spawn'}, Arguments}) ->
  TmpVar = utils:temp_variable(),
  case Arguments of
    % TODO erlang:spawn/1,2,4
    % erlang:spawn/3
    [_SpawnModule, SpawnFunction, SpawnArgs] ->
      % TODO Handle calls to functions in other modules
      {Env, [TmpVar], {spawn, {TmpVar, SpawnFunction, erl_syntax:list_elements(SpawnArgs)}}}
  end;
eval_bif(Env, {call, _, {remote, _, erlang, 'self'}, {nil, _}}) ->
  TmpVar = utils:temp_variable(),
  {Env, [TmpVar], {self, TmpVar}};
eval_bif(Env, {call, _, {remote, _, erlang, FunName}, Arguments}) ->
  ConcreteArgs = lists:map(fun erl_syntax:concrete/1, Arguments),
  % BIF so we just evaluate it
  Value = apply(erlang, FunName, ConcreteArgs),
  {Env, [(abstract(Value))], tau}.


-spec eval_list(environment(), af_cons(abstract_expr())) -> result().

eval_list(Env, {cons, Pos, Head, Tail}) ->
  case is_expr(Head, Env) of
    true ->
      {NewEnv, [NewHead], Label} = eval_expr(Env, Head),
      {NewEnv, [{cons, Pos, NewHead, Tail}], Label};
    false ->
      {NewEnv, [NewTail], Label} = eval_expr(Env, Tail),
      {NewEnv, [{cons, Pos, Head, NewTail}], Label}
  end.


-spec eval_tuple(environment(), af_tuple(abstract_expr())) -> result().

eval_tuple(Env, {tuple, Pos, Elements}) when is_list(Elements) ->
  {NewEnv, NewElements, Label} = eval_expr_list(Env, Elements),
  {NewEnv, [{tuple, Pos, NewElements}], Label}.


-spec eval_case_expr(environment(), af_case()) -> result().

eval_case_expr(Env, {'case', Pos, Argument, Clauses}) ->
  case is_expr(Argument, Env) of
    true ->
      {NewEnv, [NewArgument], Label} = eval_expr(Env, Argument),
      {NewEnv, [{'case', Pos, NewArgument, Clauses}], Label};
    false ->
      {NewEnv, Body} = match_clause(Env, Clauses, Argument),
      {NewEnv, Body, tau}
  end.


-spec match_clause(environment(), af_clause_seq(), abstract_expr()) -> {environment(), [abstract_expr()]} | nomatch.

match_clause(Env, Clauses, Value) ->
  try
    lists:foreach(
      fun({'clause', _Pos, [Pattern], Guards, Body}) ->
        NewPattern = eval_pattern(Env, Pattern),
        MatchExpr = {match, erl_anno:new(0), NewPattern, Value},
        try erl_eval:expr(MatchExpr, []) of
          {value, _Value, Bindings} ->
            NewEnv = utils:merge_env(Env, Bindings),
            case Guards of
              [] -> throw({NewEnv, Body});
              Guard ->
                NewGuard = eval_guard_seq(NewEnv, Guard),
                case erl_syntax:atom_value(NewGuard) of
                  true -> throw({NewEnv, Body});
                  false -> continue
                end
            end
        catch
          % Pattern doesn't match
          error:{badmatch, _Rhs} -> continue
        end
      end,
      Clauses
    ),
    nomatch
  catch
    throw:Match -> Match
  end.


-spec eval_pattern(environment(), af_pattern()) -> af_pattern().

eval_pattern(Env, Pattern) ->
  case is_expr(Pattern, Env) of
    true ->
      {NewEnv, [NewPattern], tau} = eval_expr(Env, Pattern),
      eval_pattern(NewEnv, NewPattern);
    false -> Pattern
  end.


-spec eval_guard_seq(environment(), af_guard_seq()) -> af_lit_atom(true) | af_lit_atom(false).

eval_guard_seq(Env, GuardSeq) when is_list(GuardSeq) ->
  % In a guard sequence, guards are evaluated until one is true. The remaining guards, if any, are not evaluated.
  % See: https://erlang.org/doc/reference_manual/expressions.html#guard-sequences
  AnyTrue = lists:any(fun(Guard) -> erl_syntax:atom_value(eval_guard(Env, Guard)) end, GuardSeq),
  {atom, erl_anno:new(0), AnyTrue}.


-spec eval_guard(environment(), af_guard()) -> af_lit_atom(true) | af_lit_atom(false).

eval_guard(Env, Guard) when is_list(Guard) ->
  AllTrue = lists:all(fun(GuardTest) -> erl_syntax:atom_value(eval_guard_test(Env, GuardTest)) end, Guard),
  {atom, erl_anno:new(0), AllTrue}.


-spec eval_guard_test(environment(), af_guard_test()) -> af_guard_test() | af_lit_atom(true) | af_lit_atom(false).

eval_guard_test(Env, GuardTest) ->
  case erl_lint:is_guard_test(GuardTest) of
    true ->
      case is_expr(GuardTest, Env) of
        true ->
          % Environment should not change, and the label should be `tau`
          {Env, [NewGuardTest], tau} = eval_expr(Env, GuardTest),
          eval_guard_test(Env, NewGuardTest);
        false -> GuardTest
      end;
    false -> erlang:error(guard_expr) % TODO How to handle error in the interpreted code?
  end.


-spec eval_receive(environment(), af_receive()) -> result().

%% TODO Support receive with timeout
eval_receive(Env, {'receive', _, Clauses}) ->
  TmpVar = utils:temp_variable(),
  {Env, [TmpVar], {rec, TmpVar, Clauses}}.


-spec abstract(term()) -> abstract_expr().

abstract(Value) -> erl_syntax:revert(erl_syntax:abstract(Value)).


%% =====================================================================
%% @doc Performs an evaluation step in process Pid, given System

-spec eval_step(system(), af_integer()) -> system().

eval_step(System, Pid) ->
  #sys{msgs = Msgs, procs = Procs, trace = Trace} = System,
  {Proc, RestProcs} = utils:select_proc(Procs, Pid),
  #proc{pid = Pid, hist = Hist, env = Env, exprs = Exprs, mail = Mail} = Proc,
  [CurExpr | RestExpr] = Exprs,
  {NewEnv, NewExprs, Label} = eval_expr(Env, CurExpr),
  case Label of
    tau ->
      NewProc = Proc#proc{
        hist  = [{tau, Env, Exprs} | Hist],
        env   = NewEnv,
        exprs = NewExprs ++ RestExpr
      },
      System#sys{
        procs = [NewProc | RestProcs]
      };
    {self, TmpVar} ->
      RepExpr = utils:replace_variable(TmpVar, Pid, NewExprs),

      NewProc = Proc#proc{
        hist = [{self, Env, Exprs} | Hist],
        env  = NewEnv,
        exprs = RepExpr ++ RestExpr
      },
      System#sys{
        procs = [NewProc | RestProcs]
      };
    {send, DestPid, MsgValue} ->
      Time = utils:fresh_time(),
      NewMsg = #msg{dest = DestPid, val = MsgValue, time = Time},

      NewProc = Proc#proc{
        hist = [{send, Env, Exprs, DestPid, {MsgValue, Time}} | Hist],
        env  = NewEnv,
        exprs = NewExprs ++ RestExpr
      },
      TraceItem = #trace{
        type = ?RULE_SEND,
        from = Pid,
        to   = DestPid,
        val  = MsgValue,
        time = Time
      },
      System#sys{
        msgs  = [NewMsg | Msgs],
        procs = [NewProc | RestProcs],
        trace = [TraceItem | Trace]
      };
    {spawn, {TmpVar, FunName, FunArgs}} ->
      SpawnPid = abstract(utils:fresh_pid()),
      SpawnProc = #proc{
        pid   = SpawnPid,
        exprs = [{call, erl_anno:new(0), FunName, FunArgs}],
        spf   = {erl_syntax:atom_value(FunName), length(FunArgs)}
      },

      RepExpr = utils:replace_variable(TmpVar, SpawnPid, NewExprs),

      NewProc = Proc#proc{
        hist = [{spawn, Env, Exprs, SpawnPid} | Hist],
        env  = NewEnv,
        exprs = RepExpr ++ RestExpr
      },
      TraceItem = #trace{
        type = ?RULE_SPAWN,
        from = Pid,
        to   = SpawnPid
      },
      System#sys{
        procs = [NewProc | [SpawnProc | RestProcs]],
        trace = [TraceItem | Trace]
      };
    {rec, TmpVar, ReceiveClauses} ->
      {Bindings, RecExp, ConsMsg, NewMail} = matchrec(ReceiveClauses, Mail, NewEnv),
      {MsgValue, Time} = ConsMsg,

      RepExpr = utils:replace_variable(TmpVar, RecExp, NewExprs),

      NewProc = Proc#proc{
        hist  = [{rec, Env, Exprs, ConsMsg, Mail} | Hist],
        env   = utils:merge_env(NewEnv, Bindings),
        exprs = RepExpr ++ RestExpr,
        mail  = NewMail
      },
      TraceItem = #trace{
        type = ?RULE_RECEIVE,
        from = Pid,
        val  = MsgValue,
        time = Time
      },
      System#sys{
        procs = [NewProc | RestProcs],
        trace = [TraceItem | Trace]
      }
  end.


%% =====================================================================
%% @doc Performs an evaluation step in message Id, given System

-spec eval_sched(system(), non_neg_integer()) -> system().

eval_sched(System, Id) ->
  #sys{procs = Procs, msgs = Msgs} = System,
  {Msg, RestMsgs} = utils:select_msg(Msgs, Id),
  #msg{dest = DestPid, val = Value, time = Id} = Msg,
  {Proc, RestProcs} = utils:select_proc(Procs, DestPid),
  Mail = Proc#proc.mail,
  NewMail = Mail ++ [{Value, Id}],
  NewProc = Proc#proc{mail = NewMail},
  System#sys{msgs = RestMsgs, procs = [NewProc | RestProcs]}.


%% =====================================================================
%% @doc Checks if the given abstract expression can be reduced any further or not.

-spec is_expr(abstract_expr(), environment()) -> boolean().

is_expr({atom, _, _}, _)      -> false;
is_expr({char, _, _}, _)      -> false;
is_expr({float, _, _}, _)     -> false;
is_expr({integer, _, _}, _)   -> false;
is_expr({nil, _}, _)          -> false;
is_expr({string, _, _}, _)    -> false;
is_expr({var, _, '_'}, _)     -> false;
is_expr({var, _, Name}, Env)  -> erl_eval:binding(Name, Env) =/= unbound;
is_expr({cons, _, H, T}, Env) -> is_expr(H, Env) orelse is_expr(T, Env);
is_expr({tuple, _, Es}, Env)  -> lists:any(fun(E) -> is_expr(E, Env) end, Es);
is_expr(_, _)                 -> true.


%% =====================================================================
%% @doc Tries to match each message, in time order, in the mailbox against every
%% pattern from the `Clauses`, sequentially. If a match succeeds and the optional
%% guard sequence is `true`, a tuple with the following form is returned:
%% `{NewEnvironment, NewExpression, MatchedMessage, RestMessages}`
%% Otherwise, the atom `nomatch` is returned.

-spec matchrec(af_clause_seq(), [process_message()], environment()) -> {environment(), [abstract_expr()], process_message(), [process_message()]} | nomatch.

matchrec(Clauses, Mail, Env) -> matchrec(Clauses, Mail, [], Env).


-spec matchrec(af_clause_seq(), [process_message()], [process_message()], environment()) -> {environment(), [abstract_expr()], process_message(), [process_message()]} | nomatch.

matchrec(_Clauses, [], _CheckedMsgs, _Env) -> nomatch;
matchrec(Clauses, [CurMsg | RestMsgs], CheckedMsgs, Env) ->
  {MsgValue, _MsgTime} = CurMsg,
  case match_clause(Env, Clauses, MsgValue) of
    {NewEnv, Body} -> {NewEnv, Body, CurMsg, lists:reverse(CheckedMsgs, RestMsgs)};
    nomatch -> matchrec(Clauses, RestMsgs, [CurMsg | CheckedMsgs], Env)
  end.


%% =====================================================================
%% @doc Gets the evaluation options for a given System

-spec eval_opts(system()) -> [option()].

eval_opts(System) ->
  SchedOpts = eval_sched_opts(System),
  ProcsOpts = eval_procs_opts(System),
  lists:append(SchedOpts, ProcsOpts).


-spec eval_sched_opts(system()) -> [option()].

eval_sched_opts(#sys{msgs = []}) -> [];
eval_sched_opts(System = #sys{msgs = [CurMsg | RestMsgs], procs = Procs}) ->
  #msg{dest = DestPid, time = Time} = CurMsg,
  case lists:any(fun(P) -> P#proc.pid == DestPid end, Procs) of
    false -> eval_sched_opts(System#sys{msgs = RestMsgs});
    true ->
      Option = #opt{
        sem  = ?MODULE,
        type = ?TYPE_MSG,
        id   = Time,
        rule = ?RULE_SCHED
      },
      [Option | eval_sched_opts(System#sys{msgs = RestMsgs})]
  end.


-spec eval_procs_opts(system()) -> [option()].

eval_procs_opts(#sys{procs = []}) -> [];
eval_procs_opts(System = #sys{procs = [CurProc | RestProcs]}) ->
  #proc{pid = Pid, env = Env, exprs = Exprs, mail = Mail} = CurProc,
  case eval_expr_opt(Exprs, Env, Mail) of
    ?NOT_EXP -> eval_procs_opts(System#sys{procs = RestProcs});
    Rule ->
      Option = #opt{
        sem  = ?MODULE,
        type = ?TYPE_PROC,
        id   = erl_syntax:integer_value(Pid),
        rule = Rule
      },
      [Option | eval_procs_opts(System#sys{procs = RestProcs})]
  end.


-spec eval_expr_opt(Expressions, Environment, Mail) -> Options when
  Expressions :: abstract_expr() | [abstract_expr()],
  Environment :: environment(),
  Mail :: [#msg{}],
  Options :: ?NOT_EXP | ?RULE_SEQ | ?RULE_CHECK | ?RULE_SEND | ?RULE_RECEIVE | ?RULE_SPAWN | ?RULE_SELF.

eval_expr_opt(Expr, Env, Mail) when is_tuple(Expr) -> eval_expr_opt([Expr], Env, Mail);
eval_expr_opt([Expr | Exprs], Env, Mail) when is_tuple(Expr), is_list(Exprs) ->
  case is_expr(Expr, Env) of
    false ->
      case Exprs of
        [] -> ?NOT_EXP;
        _ ->
          % If `Expr` is not an expression but there are still other expressions
          % to evaluate then it means we just found a literal in the middle of
          % the program, so we allow to continue.
          ?RULE_SEQ
      end;
    true ->
      case erl_syntax:type(Expr) of
        variable -> ?RULE_SEQ;
        match_expr ->
          Pattern = erl_syntax:match_expr_pattern(Expr),
          case is_expr(Pattern, Env) of
            true -> eval_expr_opt(Pattern, Env, Mail);
            false ->
              Body = erl_syntax:match_expr_body(Expr),
              case is_expr(Body, Env) of
                true -> eval_expr_opt(Body, Env, Mail);
                false -> ?RULE_SEQ
              end
          end;
        infix_expr ->
          Left = erl_syntax:infix_expr_left(Expr),
          case is_expr(Left, Env) of
            true -> eval_expr_opt(Left, Env, Mail);
            false ->
              Right = erl_syntax:infix_expr_right(Expr),
              case is_expr(Right, Env) of
                true -> eval_expr_opt(Right, Env, Mail);
                false ->
                  Op = erl_syntax:atom_value(erl_syntax:infix_expr_operator(Expr)),
                  case Op of
                    '!' -> ?RULE_SEND;
                    _ -> ?RULE_SEQ
                  end
              end
          end;
        prefix_expr ->
          Arg = erl_syntax:prefix_expr_argument(Expr),
          case is_expr(Arg, Env) of
            true -> eval_expr_opt(Arg, Env, Mail);
            false -> ?RULE_SEQ
          end;
        application ->
          Args = erl_syntax:application_arguments(Expr),
          case lists:any(fun(Arg) -> is_expr(Arg, Env) end, Args) of
            true -> eval_expr_opt(Args, Env, Mail);
            false ->
              Op = erl_syntax:application_operator(Expr),
              case erl_syntax:type(Op) of
                module_qualifier ->
                  Module = erl_syntax:module_qualifier_argument(Op),
                  case is_expr(Module, Env) of
                    true -> eval_expr_opt(Module, Env, Mail);
                    false ->
                      case erl_syntax:atom_value(Module) of
                        'erlang' ->
                          Name = erl_syntax:module_qualifier_body(Op),
                          case is_expr(Name, Env) of
                            true -> eval_expr_opt(Name, Env, Mail);
                            false ->
                              case erl_syntax:atom_value(Name) of
                                'spawn' -> ?RULE_SPAWN;
                                'self' -> ?RULE_SELF;
                                _ -> ?RULE_SEQ
                              end
                          end;
                        _ -> ?RULE_SEQ
                      end
                  end;
                _ ->
                  case is_expr(Op, Env) of
                    true -> eval_expr_opt(Op, Env, Mail);
                    false ->
                      % TODO Check for clashes with functions in the same file and/or directory
                      case erl_syntax:atom_value(Op) of
                        'spawn' -> ?RULE_SPAWN;
                        'self' -> ?RULE_SELF;
                        _ -> ?RULE_SEQ
                      end
                  end
              end
          end;
        list -> eval_expr_opt(erl_syntax:list_elements(Expr), Env, Mail);
        tuple -> eval_expr_opt(erl_syntax:tuple_elements(Expr), Env, Mail);
        case_expr ->
          Arg = erl_syntax:case_expr_argument(Expr),
          case is_expr(Arg, Env) of
            true -> eval_expr_opt(Arg, Env, Mail);
            false -> ?RULE_SEQ
          end;
        receive_expr ->
          Clauses = erl_syntax:receive_expr_clauses(Expr),
          case matchrec(Clauses, Mail, Env) of
            nomatch -> ?NOT_EXP;
            _Other -> ?RULE_RECEIVE
          end
      end
  end.


ref_lookup(Id) -> ets:lookup_element(?APP_REF, Id, 2).
