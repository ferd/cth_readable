-module(cth_readable_failonly).

-record(state, {id,
                sasl_reset,
                lager_reset,
                handlers=[],
                named}).
-record(eh_state, {buf = [], sasl=false}).

%% Callbacks
-export([id/1]).
-export([init/2]).

-export([pre_init_per_suite/3]).
-export([post_init_per_suite/4]).
-export([pre_end_per_suite/3]).
-export([post_end_per_suite/4]).

-export([pre_init_per_group/3]).
-export([post_init_per_group/4]).
-export([pre_end_per_group/3]).
-export([post_end_per_group/4]).

-export([pre_init_per_testcase/3]).
-export([post_end_per_testcase/4]).

-export([on_tc_fail/3]).
-export([on_tc_skip/3]).

-export([terminate/1]).

%% Handler API
-export([init/1,
         handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

-define(DEFAULT_LAGER_SINK, lager_event).
-define(DEFAULT_LAGER_HANDLER_CONF,
        [{lager_console_backend, info},
         {lager_file_backend,
            [{file, "log/error.log"}, {level, error},
             {size, 10485760}, {date, "$D0"}, {count, 5}]
         },
         {lager_file_backend,
            [{file, "log/console.log"}, {level, info},
             {size, 10485760}, {date, "$D0"}, {count, 5}]
         }
        ]).

%% @doc Return a unique id for this CTH.
id(_Opts) ->
    {?MODULE, make_ref()}.

%% @doc Always called before any other callback function. Use this to initiate
%% any common state.
init(Id, _Opts) ->
    %% ct:pal replacement needs to know if this hook is enabled -- we use a named proc for that
    Named = spawn_link(fun() -> timer:sleep(infinity) end),
    register(?MODULE, Named),
    error_logger:tty(false), % TODO check if on to begin with
    application:load(sasl), % TODO do this optionally?
    LagerReset = setup_lager(),
    case application:get_env(sasl, sasl_error_logger) of
        {ok, tty} ->
            ok = gen_event:add_handler(error_logger, ?MODULE, [sasl]),
            application:set_env(sasl, sasl_error_logger, false),
            {ok, #state{id=Id, sasl_reset={reset, tty}, lager_reset=LagerReset,
                        handlers=[?MODULE], named=Named}};
        _ ->
            ok = gen_event:add_handler(error_logger, ?MODULE, [nosasl]),
            {ok, #state{id=Id, lager_reset=LagerReset, handlers=[?MODULE], named=Named}}
    end.

%% @doc Called before init_per_suite is called. 
pre_init_per_suite(_Suite,Config,State) ->
    {Config, State}.

%% @doc Called after init_per_suite.
post_init_per_suite(_Suite,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before end_per_suite.
pre_end_per_suite(_Suite,Config,State) ->
    call_handlers(ignore, State#state.handlers),
    {Config, State}.

%% @doc Called after end_per_suite.
post_end_per_suite(_Suite,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before each init_per_group.
pre_init_per_group(_Group,Config,State) ->
    call_handlers(ignore, State#state.handlers),
    {Config, State}.

%% @doc Called after each init_per_group.
post_init_per_group(_Group,_Config,Return,State) ->
    {Return, State}.

%% @doc Called after each end_per_group.
pre_end_per_group(_Group,Config,State) ->
    {Config, State}.

%% @doc Called after each end_per_group.
post_end_per_group(_Group,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before each test case.
pre_init_per_testcase(_TC,Config,State) ->
    call_handlers(ignore, State#state.handlers),
    {Config, State}.

%% @doc Called after each test case.
post_end_per_testcase(_TC,_Config,ok,State=#state{}) ->
    {ok, State};
post_end_per_testcase(_TC,_Config,Error,State) ->
    {Error, State}.

%% @doc Called after post_init_per_suite, post_end_per_suite, post_init_per_group,
%% post_end_per_group and post_end_per_testcase if the suite, group or test case failed.
on_tc_fail({_TC,_Group}, _Reason, State=#state{}) ->
    call_handlers(flush, State#state.handlers),
    State;
on_tc_fail(_TC, _Reason, State=#state{}) ->
    call_handlers(flush, State#state.handlers),
    State.

%% @doc Called when a test case is skipped by either user action
%% or due to an init function failing.
on_tc_skip({_TC,_Group}, _Reason, State=#state{}) ->
    call_handlers(flush, State#state.handlers),
    State;
on_tc_skip(_TC, _Reason, State=#state{}) ->
    call_handlers(flush, State#state.handlers),
    State.

%% @doc Called when the scope of the CTH is done
terminate(_State=#state{handlers=Handlers, sasl_reset=SReset,
                        lager_reset=LReset, named=Pid}) ->
    _ = [gen_event:delete_handler(error_logger, Handler, shutdown)
         || Handler <- Handlers],
    case SReset of
        {reset, Val} -> application:set_env(sasl, sasl_error_logger, Val);
        undefined -> ok
    end,
    error_logger:tty(true),
    application:unload(sasl), % silently fails if running
    lager_reset(LReset),
    %% Kill the named process signifying this is running
    unlink(Pid),
    Ref = erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive
        {'DOWN', Ref, process, Pid, shutdown} -> ok
    end.

%%%%%%%%%%%%%

init([sasl]) ->
    {ok, #eh_state{sasl=true}};
init([nosasl]) ->
    {ok, #eh_state{sasl=false}}.

handle_event(Event, S=#eh_state{buf=Buf}) ->
    NewBuf = case parse_event(Event) of
        ignore -> Buf;
        sasl -> [{sasl, {calendar:local_time(), Event}}|Buf];
        error_logger -> [{error_logger, {erlang:universaltime(), Event}}|Buf]
    end,
    {ok, S#eh_state{buf=NewBuf}};
handle_event(_, S) ->
    {ok, S}.

handle_info(_, State) ->
    {ok, State}.

handle_call({lager, _} = Event, S=#eh_state{buf=Buf}) ->
    %% lager events come in from our fake handler, pre-filtered.
    {ok, ok, S#eh_state{buf=[Event | Buf]}};
handle_call({ct_pal, ignore}, S) ->
    {ok, ok, S};
handle_call({ct_pal, _}=Event, S=#eh_state{buf=Buf}) ->
    {ok, ok, S#eh_state{buf=[Event | Buf]}};
handle_call(ignore, State) ->
    {ok, ok, State#eh_state{buf=[]}};
handle_call(flush, S=#eh_state{buf=Buf}) ->
    ShowSASL = sasl_running() orelse sasl_ran(Buf) andalso S#eh_state.sasl,
    SASLType = get_sasl_error_logger_type(),
    _ = [case T of
            error_logger ->
                error_logger_tty_h:write_event(Event, io);
            sasl when ShowSASL ->
                sasl_report:write_report(standard_io, SASLType, Event);
            ct_pal ->
                io:format(user, Event, []);
            lager ->
                io:put_chars(user, Event);
            _ ->
                ignore
         end || {T, Event} <- lists:reverse(Buf)],
    {ok, ok, S#eh_state{buf=[]}}.


code_change(_, _, State) ->
    {ok, State}.

terminate(_, _) ->
    ok.

%%%%
sasl_running() ->
    length([1 || {sasl, _, _} <- application:which_applications()]) > 0.

get_sasl_error_logger_type() ->
    case application:get_env(sasl, errlog_type) of
        {ok, error} -> error;
        {ok, progress} -> progress;
        {ok, all} -> all;
        {ok, Bad} -> exit({bad_config, {sasl, {errlog_type, Bad}}});
        _ -> all
    end.

sasl_ran([]) -> false;
sasl_ran([{sasl, {_DateTime, {info_report,_,
            {_,progress, [{application,sasl},{started_at,_}|_]}}}}|_]) -> true;
sasl_ran([_|T]) -> sasl_ran(T).

call_handlers(Msg, Handlers) ->
    _ = [gen_event:call(error_logger, Handler, Msg, 300000)
         || Handler <- Handlers],
    ok.

parse_event({_, GL, _}) when node(GL) =/= node() -> ignore;
parse_event({info_report, _GL, {_Pid, progress, _Args}}) -> sasl;
parse_event({error_report, _GL, {_Pid, supervisor_report, _Args}}) -> sasl;
parse_event({error_report, _GL, {_Pid, crash_report, _Args}}) -> sasl;
parse_event({error, _GL, {_Pid, _Format, _Args}}) -> error_logger;
parse_event({info_msg, _GL, {_Pid, _Format, _Args}}) -> error_logger;
parse_event({warning_msg, _GL, {_Pid, _Format, _Args}}) -> error_logger;
parse_event({error_report, _GL, {_Pid, _Format, _Args}}) -> error_logger;
parse_event({info_report, _GL, {_Pid, _Format, _Args}}) -> error_logger;
parse_event({warning_report, _GL, {_Pid, _Format, _Args}}) -> error_logger;
parse_event(_) -> sasl. % sasl does its own filtering

setup_lager() ->
    case application:load(lager) of
        {error, {"no such file or directory", _}} ->
            %% app not available
            undefined;
        _ -> % it's show time
            %% Keep lager from throwing us out
            WhiteList = application:get_env(lager, error_logger_whitelist, []),
            application:set_env(lager, error_logger_whitelist, [?MODULE|WhiteList]),
            InitConf = application:get_env(lager, handlers, ?DEFAULT_LAGER_HANDLER_CONF),
            %% Add ourselves to the config
            NewConf = case proplists:get_value(lager_console_backend, InitConf) of
                undefined -> % no console backend running
                    InitConf;
                Opts ->
                    [{cth_readable_lager_backend, Opts}
                     | InitConf -- [{lager_console_backend, Opts}]]
            end,
            application:set_env(lager, handlers, NewConf),
            %% check if lager is running and override!
            case {whereis(lager_sup),
                  proplists:get_value(cth_readable_lager_backend, NewConf)} of
                {undefined, _} ->
                    InitConf;
                {_, undefined} ->
                    InitConf;
                {_, LOpts} ->
                    swap_lager_handlers(lager_console_backend,
                                        cth_readable_lager_backend, LOpts),
                    InitConf
            end
    end.

lager_reset(undefined) ->
    ok;
lager_reset(InitConf) ->
    %% Reset the whitelist
    WhiteList = application:get_env(lager, error_logger_whitelist, []),
    application:set_env(lager, error_logger_whitelist, WhiteList--[?MODULE]),
    %% Swap them handlers again
    Opts = poplists:get_value(lager_console_backend, InitConf),
    application:set_env(lager, handlers, InitConf),
    case {whereis(lager_sup), Opts} of
        {undefined, _} -> % not running
            ok;
        {_, undefined} -> % not scheduled
            ok;
        {_, _} ->
            swap_lager_handlers(cth_readable_lager_backend,
                                lager_console_backend, Opts)
     end.

swap_lager_handlers(Old, New, Opts) ->
    gen_event:delete_handler(?DEFAULT_LAGER_SINK, Old, shutdown),
    lager_app:start_handler(?DEFAULT_LAGER_SINK,
                            New, Opts).

