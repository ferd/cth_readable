-module(show_logs_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() -> [{group, works}, {group, fails}, skip].

groups() ->
    [{all, [], [error_logger, sasl, ctpal]},
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
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.

error_logger(Config) ->
    error_logger:error_msg("error\n"),
    error_logger:warning_msg("warn\n"),
    error_logger:info_msg("info\n"),
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

skip(_Config) ->
    ok.
