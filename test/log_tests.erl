-module(log_tests).
-include_lib("eunit/include/eunit.hrl").

%% run this to check interactions between lager and logger
%% by calling `rebar3 do eunit, ct'
start_test() ->
    {ok, Apps} = application:ensure_all_started(lager),
    io:format(user, "~p~n", [logger:get_handler_config()]),
    [application:stop(App) || App <- Apps].
