%% @doc Experimental
-module(cthr).
-include_lib("common_test/include/ct.hrl").
-export([pal/1, pal/2, pal/3, pal/4]).

pal(Format) ->
    pal(default, ?STD_IMPORTANCE, Format, []).

pal(X1,X2) ->
    {Category,Importance,Format,Args} =
    if is_atom(X1)    -> {X1,?STD_IMPORTANCE,X2,[]};
       is_integer(X1) -> {default,X1,X2,[]};
       is_list(X1)    -> {default,?STD_IMPORTANCE,X1,X2}
    end,
    pal(Category,Importance,Format,Args).

pal(X1,X2,X3) ->
    {Category,Importance,Format,Args} =
    if is_atom(X1), is_integer(X2) -> {X1,X2,X3,[]};
       is_atom(X1), is_list(X2)    -> {X1,?STD_IMPORTANCE,X2,X3};
       is_integer(X1)              -> {default,X1,X2,X3}
    end,
    pal(Category,Importance,Format,Args).

pal(Category,Importance,Format,Args) ->
    case whereis(cth_readable_failonly) of
        undefined -> % hook not running, passthrough
            ct_logs:tc_pal(Category,Importance,Format,Args);
        _ -> % hook running, take over
            %% Send to error_logger
            gen_event:sync(error_logger, {ct_pal, format(Category,Importance,Format,Args)}),
            %% Send to ct group leader
            ct_logs:tc_log(Category, Importance, Format, Args),
            ok
    end.

%%% Replicate CT stuff but don't output it
format(Category, Importance, Format, Args) ->
    VLvl = case ct_util:get_verbosity(Category) of
        undefined -> 
            ct_util:get_verbosity('$unspecified');
        {error,bad_invocation} ->
            ?MAX_VERBOSITY;
        {error,_Failure} ->
            ?MAX_VERBOSITY;
        Val ->
            Val
    end,
    if Importance >= (100-VLvl) ->
            Head = get_heading(Category),
            io_lib:format(lists:concat([Head,Format,"\n\n"]), Args);
        true ->
            ignore
    end.

get_heading(default) ->
    io_lib:format("\n-----------------------------"
                  "-----------------------\n~s\n",
                  [log_timestamp(os:timestamp())]);
get_heading(Category) ->
    io_lib:format("\n-----------------------------"
                  "-----------------------\n~s  ~w\n",
                  [log_timestamp(os:timestamp()),Category]).

log_timestamp({MS,S,US}) ->
    put(log_timestamp, {MS,S,US}),
    {{Year,Month,Day}, {Hour,Min,Sec}} =
                                         calendar:now_to_local_time({MS,S,US}),
    MilliSec = trunc(US/1000),
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B "
                                "~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B",
                                [Year,Month,Day,Hour,Min,Sec,MilliSec])).

