-module(cauder_wx_config).

%% API
-export([load/0, save/1]).

-define(CONFIG_FILE, "econfig.cfg").

-include("cauder.hrl").
-include("cauder_wx.hrl").

-type config() :: #config{}.

-export_type([config/0]).


%%%=============================================================================
%%% API
%%%=============================================================================


-spec load() -> Config when
  Config :: config().

load() ->
  FileName = filename:join(config_dir(), ?CONFIG_FILE),
  case file:consult(FileName) of
    {ok, [#config{} = Config]} -> config_check(Config);
    _ -> #config{}
  end.


-spec save(Config) -> 'ok' when
  Config :: config().

save(#config{} = Config) ->
  FileName = filename:join(config_dir(), ?CONFIG_FILE),
  case filelib:ensure_dir(FileName) of
    ok          -> file:write_file(FileName, io_lib:format("~p.\n", [Config]));
    {error, _}  -> ok
  end.


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec config_dir() -> File when
  File :: file:filename_all().

config_dir() -> filename:basedir(user_config, ?APP_NAME).


-spec config_check(Config) -> CheckedConfig when
  Config :: config(),
  CheckedConfig :: config().

config_check(#config{bindings_mode = BindingMode, history_mode = HistoryMode, mailbox_mode = MailboxMode} = Config) when
  is_boolean(Config#config.current_expression),
  is_boolean(Config#config.bindings),
  is_boolean(Config#config.stack),
  is_boolean(Config#config.log),
  is_boolean(Config#config.history),
  is_boolean(Config#config.mailbox),
  is_atom(BindingMode), BindingMode =:= all orelse BindingMode =:= relevant,
  is_atom(HistoryMode), HistoryMode =:= full orelse HistoryMode =:= concurrent,
  is_atom(MailboxMode), MailboxMode =:= all orelse MailboxMode =:= process,
  is_boolean(Config#config.status_bar) ->
  Config;

config_check(_) -> #config{}.
