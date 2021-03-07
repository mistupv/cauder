-module(cauder_plugin_prv_reltool).

%% API
-export([init/1, do/1, format_error/1]).

-include_lib("kernel/include/file.hrl").

-define(PROVIDER, reltool).
-define(DEPS, [app_discovery, compile]).


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
  do_release(State, find_cauder_app(State)),
  {ok, State}.


%% When an exception is raised or a value returned as
%% `{error, {?MODULE, Reason}}` will see the `format_error(Reason)`
%% function called for them, so a string can be formatted explaining
%% the issue.

-spec format_error(any()) -> iolist().

format_error(Reason) ->
  io_lib:format("~p", [Reason]).


%%%=============================================================================
%%% Internal functions
%%%=============================================================================


find_cauder_app(State) ->
  Apps = rebar_state:project_apps(State),
  lists:keyfind(<<"cauder">>, 2, Apps).


do_release(_State, false) ->
  rebar_api:abort("Couldn't find CauDEr app", []);

do_release(State, App) ->
  ReleaseDir = release_dest_path(State),
  prepare_release_dir(ReleaseDir),

  AppVersion = rebar_app_info:original_vsn(App),
  rebar_api:info("Making release ~p~n", [AppVersion]),

  R = reltool:create_target(
    [{config, {sys, [
      {relocatable, true},
      {profile, standalone},
      {debug_info, strip},
      {rel, "cauder", AppVersion, rebar_app_info:applications(App) ++ [cauder]},
      {app, cauder, [{lib_dir, rebar_app_info:out_dir(App)}]},
      {boot_rel, "cauder"}
    ]}}], ReleaseDir),

  rebar_api:info("Reltool result: ~p~n", [R]),

  case os:type() of
    {win32, _} ->
      WinScript = "cauder.cmd",
      file:copy(filename:join("launcher", WinScript),
                filename:join(ReleaseDir, WinScript));
    {unix, _} ->
      UnixScript = "cauder.sh",
      file:copy(filename:join("launcher", UnixScript),
                filename:join(ReleaseDir, UnixScript)),

      set_executable_bit(filename:join(ReleaseDir, UnixScript))
  end,

  {ok, State}.


release_dest_path(State) ->
  filename:join([rebar_dir:base_dir(State), "reltool"]).


prepare_release_dir(Dir) ->
  rebar_file_utils:rm_rf(Dir),
  filelib:ensure_dir(filename:join(Dir, "dummy")).

set_executable_bit(Filename) ->
  {ok, #file_info{mode = OldMode}} = file:read_file_info(Filename),
  case OldMode bor 8#111 of
    OldMode -> ok;
    NewMode -> file:change_mode(Filename, NewMode)
  end.
