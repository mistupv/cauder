-module(cauder_types).

-include("cauder.hrl").

-export_type([system/0, process/0, message/0, trace/0, option/0]).
-export_type([history/0, history_entry/0, environment/0, binding/0, stack/0, process_message/0]).
-export_type([print_option/0]).

% Abstract format types
-export_type([abstract_expr/0, af_literal/0, af_boolean/0, af_variable/0, af_clause_seq/0, af_clause/0, af_guard_seq/0, af_guard/0, af_guard_test/0, af_pattern/0]).

-export_type([result/0, label/0]).


%% Record types

-type system() :: #sys{}.
-type process() :: #proc{}.
-type message() :: #msg{}.
-type trace() :: #trace{}.
-type option() :: #opt{}.

-type history() :: [history_entry()].
-type history_entry() :: {tau, environment(), [abstract_expr()], stack()}
                       | {self, environment(), [abstract_expr()], stack()}
                       | {send, environment(), [abstract_expr()], stack(), pos_integer(), process_message()}
                       | {spawn, environment(), [abstract_expr()], stack(), pos_integer()}
                       | {rec, environment(), [abstract_expr()], stack(), process_message(), [process_message()]}.

-type environment() :: [binding()].
-type binding() :: {atom(), term()}.

-type stack() :: [stack_entry()].
-type stack_entry() :: {{atom(), atom(), arity()}, environment(), [abstract_expr()], af_variable()}
                     | {atom(), [abstract_expr()], af_variable()}.

-type process_message() :: {term(), non_neg_integer()}. % {Value, Id}


-type print_option() :: ?PRINT_MAIL
                      | ?PRINT_HIST
                      | ?PRINT_ENV
                      | ?PRINT_EXP
                      | ?PRINT_FULL
                      | ?COMP_OPT
                      | ?PRINT_FULL_ENV.


%% Custom of abstract format

-type line() :: non_neg_integer().

-type abstract_expr() :: af_literal()
                       | af_variable()
                       | af_variable()
                       | af_cons(abstract_expr())
                       | af_tuple(abstract_expr())
                       | af_if()
                       | af_case()
                       | af_receive()
                       | af_make_fun()
                       | af_bif_call()
                       | af_self_call()
                       | af_spawn_1_call()
                       | af_spawn_3_call()
                       | af_send_call()
                       | af_local_call()
                       | af_remote_call()
                       | af_apply()
                       | af_apply_fun()
                       | af_match(abstract_expr())
                       | af_op(abstract_expr())
                       | af_short_circuit_op(abstract_expr()).

-type af_args() :: [abstract_expr()].

-type af_literal() :: {value, line(), integer()}
                    | {value, line(), float()}
                    | {value, line(), atom()}
                    | {value, line(), string()}
                    | {value, line(), []}.

-type af_boolean() :: {value, line(), true | false}.

-type af_variable() :: {var, line(), atom()}.

-type af_cons(T) :: {cons, line(), T, T}.

-type af_tuple(T) :: {tuple, line(), [T]}.

-type af_if() :: {'if', line(), af_clause_seq()}.

-type af_case() :: {'case', line(), abstract_expr(), af_clause_seq()}.

-type af_receive() :: {'receive', line(), af_clause_seq()}.

-type af_make_fun() :: {make_fun, line(), atom(), af_clause_seq()}.

-type af_bif_call() :: {bif, line(), atom(), atom(), af_args()}.

-type af_self_call() :: {self, line()}.

-type af_spawn_1_call() :: {spawn, line(), abstract_expr()}.

-type af_spawn_3_call() :: {spawn, line(), abstract_expr(), abstract_expr(), abstract_expr()}.

-type af_send_call() :: {send, line(), abstract_expr(), abstract_expr()}.

-type af_local_call() :: {local_call, line(), atom(), af_args()}.

-type af_remote_call() :: {remote_call, line(), atom(), atom(), af_args()}.

-type af_apply() :: {apply, line(), abstract_expr(), abstract_expr(), af_args()}.

-type af_apply_fun() :: {apply_fun, line(), abstract_expr(), af_args()}.

-type af_match(T) :: {match, line(), af_pattern(), T}.

-type af_op(T) :: {op, line(), unary_op() | binary_op(), [T]}.

-type af_unary_arith_op(T) :: {op, line(), '+' | '-', [T]}.

-type af_short_circuit_op(T) :: {'andalso' | 'orelse', line(), T, T}.


%% Clauses

-type af_clause_seq() :: [af_clause(), ...].

-type af_clause() :: {'clause', line(), [af_pattern()], af_guard_seq(), af_body()}.

-type af_pattern() :: af_literal()
                    | af_variable()
                    | af_cons(af_pattern())
                    | af_tuple(af_pattern())
                    | af_match(af_pattern())
                    | af_unary_arith_op(af_pattern()).

-type af_guard_seq() :: [af_guard()].

-type af_guard() :: [af_guard_test(), ...].

-type af_guard_test() :: af_literal()
                       | af_variable()
                       | af_cons(af_guard_test())
                       | af_tuple(af_guard_test())
                       | af_unary_arith_op(af_guard_test())
                       | af_short_circuit_op(af_guard_test())
                       | af_guard_call()
                       | af_self_call().

-type af_guard_call() :: {'bif', line(), erlang, atom(), [af_guard_test()]}.

-type af_body() :: [abstract_expr(), ...].


%% Operators

-type binary_op() :: '/' | '*' | 'div' | 'rem' | 'band' | 'and' | '+' | '-'
                   | 'bor' | 'bxor' | 'bsl' | 'bsr' | 'or' | 'xor' | '++'
                   | '--' | '==' | '/=' | '=<' | '<'  | '>=' | '>' | '=:='
                   | '=/='.

-type unary_op() :: '+' | '-' | 'bnot' | 'not'.

%% End of custom abstract format


-type result() :: #result{}.

-type label() :: label_tau()
               | label_spawn_1()
               | label_spawn_3()
               | label_self()
               | label_send()
               | label_rec().

-type label_tau() :: tau.
-type label_spawn_1() :: {spawn, af_variable(), fun()}.
-type label_spawn_3() :: {spawn, af_variable(), atom(), atom(), [term()]}.
-type label_self() :: {self, af_variable()}.
-type label_send() :: {send, pos_integer(), term()}.
-type label_rec() :: {rec, af_variable(), af_clause_seq()}.
