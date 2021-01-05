-module(failonly_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").


all() ->
  [pal].

%% run with rebar3 ct --readable false
%% and configure rebar.config with
%%     {ct_opts, [
%        {ct_hooks, [{cth_readable_failonly, [{max_events, 2}]}, cth_readable_shell]}
%      ]}.
pal() ->
  [ct:pal("Event ~p", [X]) || X <- lists:seq(0, 10)],
  error(crash).
