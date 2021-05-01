-module(cauder_plugin_prv_escript_link).

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
            {desc, "Creates a symbolic link to the escript"},
            {short_desc, "Creates a symbolic link to the escript"},
            {example, "rebar3 " ++ atom_to_list(?PROVIDER)},
            {opts, []}
        ]
    ),
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
                    Escript = filename:join([rebar_dir:base_dir(State), "bin", "cauder"]),
                    Link = filename:join(rebar_dir:root_dir(State), "cauder"),
                    case os:type() of
                        {win32, _} ->
                            file:delete(Link);
                        {unix, _} ->
                            case file:make_symlink(Escript, Link) of
                                ok ->
                                    rebar_api:info("Created symbolic link: ~p -> ~p~n", [Link, Escript]);
                                {error, eexist} ->
                                    rebar_api:info("Symbolic link already exists~n", [])
                            end
                    end;
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
