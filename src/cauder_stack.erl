-module(cauder_stack).

%% API
-export([new/0, peek/1, pop/1, push/2, is_empty/1, to_list/1]).
-export([type/1]).
-export([block/3, block_type/1, block_expr/1, block_var/1]).
-export([function/4, function_mfa/1, function_env/1, function_expr/1, function_var/1]).
-export([current_module/1]).

% TODO Remove
-ignore_xref([peek/1]).

-export_type([stack/0]).

-record(entry_function, {
    mfa :: mfa(),
    env :: cauder_bindings:bindings(),
    expr :: [cauder_syntax:abstract_expr()],
    var :: cauder_syntax:af_variable()
}).
-record(entry_block, {
    type :: cauder_stack:block_type(),
    expr :: [cauder_syntax:abstract_expr()],
    var :: cauder_syntax:af_variable()
}).

-opaque stack() :: [cauder_stack:stack_entry()].
-type stack_entry() ::
    cauder_stack:entry_function()
    | cauder_stack:entry_block().

-type entry_function() :: #entry_function{}.
-type entry_block() :: #entry_block{}.

-type entry_type() :: 'function' | 'block'.
-type block_type() :: 'if' | 'case' | 'receive'.

%%%=============================================================================

-spec new() -> cauder_stack:stack().

new() -> [].

-spec peek(Stack) -> Entry when
    Stack :: cauder_stack:stack(),
    Entry :: cauder_stack:stack_entry().

peek([Entry | _]) -> Entry.

-spec pop(Stack1) -> {Entry, Stack2} when
    Stack1 :: cauder_stack:stack(),
    Entry :: cauder_stack:stack_entry(),
    Stack2 :: cauder_stack:stack().

pop([Entry | Stack]) -> {Entry, Stack}.

-spec push(Entry, Stack1) -> Stack2 when
    Stack1 :: cauder_stack:stack(),
    Entry :: cauder_stack:stack_entry(),
    Stack2 :: cauder_stack:stack().

push(Entry, Stack1) -> [Entry | Stack1].

-spec is_empty(Stack) -> boolean() when
    Stack :: cauder_stack:stack().

is_empty([]) -> true;
is_empty(_) -> false.

-spec to_list(Stack) -> [Entry] when
    Stack :: cauder_stack:stack(),
    Entry :: cauder_stack:stack_entry().

to_list(Stack) -> Stack.

%%%=============================================================================

-spec type(Entry) -> Type when
    Entry :: cauder_stack:stack_entry(),
    Type :: cauder_stack:entry_type().

type(#entry_function{}) -> 'function';
type(#entry_block{}) -> 'block'.

%%%=============================================================================

-spec block(Type, Expr, Var) -> Entry when
    Type :: cauder_stack:block_type(),
    Expr :: [cauder_syntax:abstract_expr()],
    Var :: cauder_syntax:af_variable(),
    Entry :: cauder_stack:entry_block().

block(Type, Expr, Var) ->
    #entry_block{type = Type, expr = Expr, var = Var}.

-spec block_type(Entry) -> Type when
    Entry :: cauder_stack:entry_block(),
    Type :: cauder_stack:block_type().

block_type(#entry_block{type = Type}) -> Type.

-spec block_expr(Entry) -> Expressions when
    Entry :: cauder_stack:entry_block(),
    Expressions :: [cauder_syntax:abstract_expr()].

block_expr(#entry_block{expr = Expr}) -> Expr.

-spec block_var(Entry) -> Variable when
    Entry :: cauder_stack:entry_block(),
    Variable :: cauder_syntax:af_variable().

block_var(#entry_block{var = Var}) -> Var.

%%%=============================================================================

-spec function(MFA, Bindings, Expressions, Variable) -> Entry when
    MFA :: mfa(),
    Bindings :: cauder_bindings:bindings(),
    Expressions :: [cauder_syntax:abstract_expr()],
    Variable :: cauder_syntax:af_variable(),
    Entry :: cauder_stack:entry_function().

function(MFA, Bs, Expr, Var) ->
    #entry_function{
        mfa = MFA,
        env = Bs,
        expr = Expr,
        var = Var
    }.

-spec function_mfa(Entry) -> MFA when
    Entry :: cauder_stack:entry_function(),
    MFA :: mfa().

function_mfa(#entry_function{mfa = MFA}) -> MFA.

-spec function_env(Entry) -> Bindings when
    Entry :: cauder_stack:entry_function(),
    Bindings :: cauder_bindings:bindings().

function_env(#entry_function{env = Bs}) -> Bs.

-spec function_expr(Entry) -> Expressions when
    Entry :: cauder_stack:entry_function(),
    Expressions :: [cauder_syntax:abstract_expr()].

function_expr(#entry_function{expr = Expr}) -> Expr.

-spec function_var(Entry) -> Variable when
    Entry :: cauder_stack:entry_function(),
    Variable :: cauder_syntax:af_variable().

function_var(#entry_function{var = Var}) -> Var.

%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Returns `{ok, Module}', where `Module' is the current module, or `error'
%% if the current module cannot be determined from the stack.

-spec current_module(Stack) -> {ok, Module} | error when
    Stack :: cauder_stack:stack(),
    Module :: module().

current_module([#entry_function{mfa = {M, _, _}} | _]) -> {ok, M};
current_module([_ | Stk]) -> current_module(Stk);
current_module([]) -> error.
