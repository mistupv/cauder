-module(cauder_types).

-include("cauder.hrl").

-export_type([system/0, process/0, message/0, trace/0, option/0]).
-export_type([history/0, history_entry/0, environment/0, binding/0, process_message/0]).
-export_type([print_option/0]).

% Abstract Forms
-export_type([af_function_decl/0]).
% Abstract Form Expressions
-export_type([abstract_expr/0, af_atom/0, af_boolean/0, af_integer/0, af_string/0, af_match/1, af_variable/0, af_tuple/1, af_nil/0, af_cons/1, af_bin/1,
              af_binary_op/1, af_unary_op/1, af_local_call/0, af_remote_call/0, af_if/0, af_case/0, af_receive/0]).
-export_type([af_clause_seq/0, af_pattern/0, af_guard_seq/0, af_guard/0, af_guard_test/0]).

-export_type([eval_result/0]).


%% Record types

-type system() :: #sys{}.
-type process() :: #proc{}.
-type message() :: #msg{}.
-type trace() :: #trace{}.
-type option() :: #opt{}.

-type history() :: [history_entry()].
-type history_entry() :: {tau, environment(), [abstract_expr()]}
                       | {self, environment(), [abstract_expr()]}
                       | {send, environment(), [abstract_expr()], af_integer(), process_message()}
                       | {spawn, environment(), [abstract_expr()], af_integer()}
                       | {rec, environment(), [abstract_expr()], process_message(), [process_message()]}.

-type environment() :: [binding()].
-type binding() :: {atom(), term()}.

-type process_message() :: {abstract_expr(), non_neg_integer()}. % {Value, Id}


-type print_option() :: ?PRINT_MAIL
                      | ?PRINT_HIST
                      | ?PRINT_ENV
                      | ?PRINT_EXP
                      | ?PRINT_FULL
                      | ?COMP_OPT
                      | ?PRINT_FULL_ENV.


%% Type definitions copied from `erl_parse.erl`

%% Start of Abstract Format

-type anno() :: erl_anno:anno().

-type af_function_decl() :: {'function', anno(), function_name(), arity(), af_clause_seq()}.

-type abstract_expr() :: erl_parse:abstract_expr().

-type af_local_call() :: {'call', anno(), af_local_function(), af_args()}.

-type af_remote_call() :: {'call', anno(), af_remote_function(), af_args()}.

-type af_args() :: [abstract_expr()].

-type af_local_function() :: abstract_expr().

-type af_remote_function() :: {'remote', anno(), abstract_expr(), abstract_expr()}.

-type af_if() :: {'if', anno(), af_clause_seq()}.

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


%% Custom Abstract Format types

-type af_boolean() :: af_lit_atom(boolean()).


%% Eval types

-type eval_result() :: {cauder_types:environment(), [cauder_types:abstract_expr()], label()}.

-type label() :: label_tau()
               | label_spawn()
               | label_self()
               | label_send()
               | label_rec().

-type label_tau() :: tau.
-type label_spawn() :: {spawn, {cauder_types:af_variable(), cauder_types:af_atom(), list(cauder_types:abstract_expr())}}.
-type label_self() :: {self, cauder_types:af_variable()}.
-type label_send() :: {send, cauder_types:af_integer(), cauder_types:abstract_expr()}.
-type label_rec() :: {rec, cauder_types:af_variable(), cauder_types:af_clause_seq()}.
