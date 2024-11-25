-module(cauder_semantics).

%% API
-export([]).

-include("cauder_semantics.hrl").

-export_type([
    semantics/0,
    rule/0
]).

-type semantics() ::
    ?SEM_FWD
    | ?SEM_BWD.
-type rule() ::
    ?RULE_LOCAL
    | ?RULE_SELF
    | ?RULE_NODE
    | ?RULE_NODES
    | ?RULE_START
    | ?RULE_SPAWN
    | ?RULE_SEND
    | ?RULE_RECEIVE
    | ?RULE_READ
    | ?RULE_REG
    | ?RULE_DEL.
