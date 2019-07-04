-module(show_logs_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("macro_wrap.hrl").
-compile({parse_transform, lager_transform}).

all() -> [{group, works}, {group, fails}, skip].

groups() ->
    [{all, [], [error_logger, logger, sasl, ctpal, eunit, lager]},
     {works, [], [{group, all}]},
     {fails, [], [{group, all}]}].

init_per_group(fails, Config) ->
    [{fail, true} | Config];
init_per_group(works, Config) ->
    [{fail, false} | Config];
init_per_group(all, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(skip, _Config) ->
    {skip, manual};
init_per_testcase(logger, Config) ->
    case erlang:function_exported(logger, module_info, 0) of
        true -> Config;
        false -> {skip, not_supported}
    end;
init_per_testcase(lager, Config) ->
    {ok, Apps} = application:ensure_all_started(lager),
    [{apps, Apps} | Config];
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(lager, Config) ->
    [application:stop(App) || App <- lists:reverse(?config(apps, Config))],
    Config;
end_per_testcase(_, Config) ->
    Config.

error_logger(Config) ->
    error_logger:error_msg("error\n"),
    error_logger:warning_msg("warn\n"),
    error_logger:info_msg("info\n"),
    ?config(fail, Config) andalso error(fail).

logger(Config) ->
    logger:alert("alert\n"),
    logger:critical("critical\n"),
    ?LOG_ERROR("error\n"),
    ?LOG_WARNING("warn\n"),
    ?LOG_INFO("info\n"),
    ?config(fail, Config) andalso error(fail).


sasl(Config) ->
    application:start(sasl),
    application:start(tools),
    %ct:pal("~p~n",[sys:get_state(error_logger)]),
    application:stop(tools),
    application:stop(sasl),
    ?config(fail, Config) andalso error(fail).

ctpal(Config) ->
    ct:pal("ct:pal call"),
    ?config(fail, Config) andalso error(fail).

eunit(Config) ->
    ?assertMatch(false, ?config(fail, Config)),
    ok.

lager(Config) ->
    lager:error("error\n", []),
    lager:warning("warn\n", []),
    lager:info("info\n", []),
    ?config(fail, Config) andalso error(fail).

skip(_Config) ->
    ok.
