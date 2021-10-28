-module(cauder_stack).

%% API
-export([new/0, peek/1, pop/1, push/2, is_empty/1, to_list/1]).
-export([current_module/1]).

-include("cauder_stack.hrl").

% TODO Remove
-ignore_xref([peek/1]).

-export_type([stack/0]).

-opaque stack() :: [entry()].
-type entry() ::
    entry_function()
    | entry_block().

-type entry_function() :: #s_function{}.
-type entry_block() :: #s_block{}.

%%%=============================================================================

-spec new() -> cauder_stack:stack().

new() -> [].

-spec peek(Stack) -> {value, Entry} | empty when
    Stack :: cauder_stack:stack(),
    Entry :: cauder_stack:entry().

peek([Entry | _]) -> Entry.

-spec pop(Stack1) -> {{value, Entry}, Stack2} | {empty, Stack1} when
    Stack1 :: cauder_stack:stack(),
    Entry :: cauder_stack:entry(),
    Stack2 :: cauder_stack:stack().

pop([Entry | Stack]) -> {Entry, Stack}.

-spec push(Entry, Stack1) -> Stack2 when
    Stack1 :: cauder_stack:stack(),
    Entry :: cauder_stack:entry(),
    Stack2 :: cauder_stack:stack().

push(Entry, Stack1) -> [Entry | Stack1].

-spec is_empty(Stack) -> boolean() when
    Stack :: cauder_stack:stack().

is_empty([]) -> true;
is_empty(_) -> false.

-spec to_list(Stack) -> [Entry] when
    Stack :: cauder_stack:stack(),
    Entry :: cauder_stack:entry().

to_list(Stack) -> Stack.

%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Returns `{ok, Module}', where `Module' is the current module, or `error'
%% if the current module cannot be determined from the stack.

-spec current_module(Stack) -> {ok, Module} | error when
    Stack :: cauder_stack:stack(),
    Module :: module().

current_module([#s_function{mfa = {M, _, _}} | _]) -> {ok, M};
current_module([_ | Stk]) -> current_module(Stk);
current_module([]) -> error.
