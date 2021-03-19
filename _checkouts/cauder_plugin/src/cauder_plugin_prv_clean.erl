-module(cauder_plugin_prv_clean).

%% API
-export([init/1, do/1, format_error/1]).

-include_lib("kernel/include/file.hrl").

-define(PROVIDER, ?MODULE).
-define(DEPS, []).


%%%=============================================================================
%%% API
%%%=============================================================================


%% Called when rebar3 first boots, before even parsing the arguments
%% or commands to be run. Purely initiates the provider, and nothing
%% else should be done here.

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.

init(State) ->
  Provider = providers:create(
    [
      {name, ?PROVIDER},
      {module, ?MODULE},
      {bare, true},
      {deps, ?DEPS},
      {desc, "Creates a release of CauDEr using reltool"},
      {short_desc, "Creates a release of CauDEr using reltool"},
      {example, "rebar3 " ++ atom_to_list(?PROVIDER)},
      {opts, []}
    ]),
  {ok, rebar_state:add_provider(State, Provider)}.


%% Run the code for the plugin. The command line argument are parsed
%% and dependencies have been run.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.

do(State) ->
  case rebar_state:current_app(State) of
    undefined ->
      ok;
    AppInfo ->
      case rebar_app_info:name(AppInfo) of
        <<"cauder">> ->
          rebar_file_utils:rm_rf(rebar_dir:base_dir(State));
        _ ->
          ok
      end
  end,

  {ok, State}.


%% When an exception is raised or a value returned as
%% `{error, {?MODULE, Reason}}` will see the `format_error(Reason)`
%% function called for them, so a string can be formatted explaining
%% the issue.

-spec format_error(any()) -> iolist().

format_error(Reason) ->
  io_lib:format("~p", [Reason]).
