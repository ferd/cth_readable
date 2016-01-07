-module(cth_readable_shell).

-define(OKC, green).
-define(FAILC, red).
-define(SKIPC, magenta).

-define(OK(Suite, CasePat, CaseArgs),
        ?CASE(Suite, CasePat, ?OKC, "OK", CaseArgs)).
-define(SKIP(Suite, CasePat, CaseArgs, Reason),
        ?STACK(Suite, CasePat, CaseArgs, Reason, ?SKIPC, "SKIPPED")).
-define(FAIL(Suite, CasePat, CaseArgs, Reason),
        ?STACK(Suite, CasePat, CaseArgs, Reason, ?FAILC, "FAILED")).
-define(STACK(Suite, CasePat, CaseArgs, Reason, Color, Label),
        begin
         ?CASE(Suite, CasePat, Color, Label, CaseArgs),
         io:format(user, "%%% ~p ==> "++colorize(Color, maybe_eunit_format(Reason))++"~n", [Suite])
        end).
-define(CASE(Suite, CasePat, Color, Res, Args),
        io:format(user, "%%% ~p ==> "++CasePat++": "++colorize(Color, Res)++"~n", [Suite | Args])).

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

-record(state, {id, suite}).

%% @doc Return a unique id for this CTH.
id(_Opts) ->
    {?MODULE, make_ref()}.

%% @doc Always called before any other callback function. Use this to initiate
%% any common state.
init(Id, _Opts) ->
    {ok, #state{id=Id}}.

%% @doc Called before init_per_suite is called.
pre_init_per_suite(Suite,Config,State) ->
    {Config, State#state{suite=Suite}}.

%% @doc Called after init_per_suite.
post_init_per_suite(_Suite,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before end_per_suite.
pre_end_per_suite(_Suite,Config,State) ->
    {Config, State}.

%% @doc Called after end_per_suite.
post_end_per_suite(_Suite,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before each init_per_group.
pre_init_per_group(_Group,Config,State) ->
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
    {Config, State}.

%% @doc Called after each test case.
post_end_per_testcase(TC,_Config,ok,State=#state{suite=Suite}) ->
    ?OK(Suite, "~p", [TC]),
    {ok, State};
post_end_per_testcase(_TC,_Config,Error,State) ->
    {Error, State}.

%% @doc Called after post_init_per_suite, post_end_per_suite, post_init_per_group,
%% post_end_per_group and post_end_per_testcase if the suite, group or test case failed.
on_tc_fail({TC,Group}, Reason, State=#state{suite=Suite}) ->
    ?FAIL(Suite, "~p (group ~p)", [TC, Group], Reason),
    State;
on_tc_fail(TC, Reason, State=#state{suite=Suite}) ->
    ?FAIL(Suite, "~p", [TC], Reason),
    State.

%% @doc Called when a test case is skipped by either user action
%% or due to an init function failing.
on_tc_skip({TC,Group}, Reason, State=#state{suite=Suite}) ->
    ?SKIP(Suite, "~p (group ~p)", [TC, Group], Reason),
    State;
on_tc_skip(TC, Reason, State=#state{suite=Suite}) ->
    ?SKIP(Suite, "~p", [TC], Reason),
    State.

%% @doc Called when the scope of the CTH is done
terminate(_State) ->
    ok.

%%% Helpers
colorize(red, Txt) -> cf:format("~!r~s~!!", [Txt]);
colorize(green, Txt) -> cf:format("~!g~s~!!", [Txt]);
colorize(magenta, Txt) -> cf:format("~!m~s~!!",[Txt]).

maybe_eunit_format({failed, {{Type, Props}, _}}) when Type =:= assert_failed
                                                    ; Type =:= assert ->
    Keys = proplists:get_keys(Props),
    HasEUnitProps = ([expression, value, line] -- Keys) =:= [],
    HasHamcrestProps = ([expected, actual, matcher, line] -- Keys) =:= [],
    if
        HasEUnitProps ->
            [io_lib:format("~nFailure/Error: ?assert(~s)~n", [proplists:get_value(expression, Props)]),
             io_lib:format("  expected: true~n", []),
             case proplists:get_value(value, Props) of
                 false ->
                     io_lib:format("       got: false~n", []);
                 {not_a_boolean, V} ->
                     io_lib:format("       got: ~p~n", [V])
             end, io_lib:format("      line: ~p", [proplists:get_value(line, Props)])];
        HasHamcrestProps ->
            [io_lib:format("~nFailure/Error: ?assertThat(~p)~n", [proplists:get_value(matcher, Props)]),
             io_lib:format("  expected: ~p~n", [proplists:get_value(expected, Props)]),
             io_lib:format("       got: ~p~n", [proplists:get_value(actual, Props)]),
             io_lib:format("      line: ~p", [proplists:get_value(line, Props)])];
        true ->
            [io_lib:format("~nFailure/Error: unknown assert: ~p", [Props])]
    end;

maybe_eunit_format({failed, {{Type, Props}, _}}) when Type =:= assertMatch_failed
                                                    ; Type =:= assertMatch ->
    Expr = proplists:get_value(expression, Props),
    Pattern = proplists:get_value(pattern, Props),
    Value = proplists:get_value(value, Props),
    [io_lib:format("~nFailure/Error: ?assertMatch(~s, ~s)~n", [Pattern, Expr]),
     io_lib:format("  expected: = ~s~n", [Pattern]),
     io_lib:format("       got: ~p", [Value]),
     io_lib:format("      line: ~p", [proplists:get_value(line, Props)])];

maybe_eunit_format({failed, {{Type, Props}, _}}) when Type =:= assertNotMatch_failed
                                                    ; Type =:= assertNotMatch  ->
    Expr = proplists:get_value(expression, Props),
    Pattern = proplists:get_value(pattern, Props),
    Value = proplists:get_value(value, Props),
    [io_lib:format("~nFailure/Error: ?assertNotMatch(~s, ~s)~n", [Pattern, Expr]),
     io_lib:format("  expected not: = ~s~n", [Pattern]),
     io_lib:format("           got:   ~p", [Value]),
     io_lib:format("          line: ~p", [proplists:get_value(line, Props)])];

maybe_eunit_format({failed, {{Type, Props}, _}}) when Type =:= assertEqual_failed
                                                    ; Type =:= assertEqual  ->
    Expr = proplists:get_value(expression, Props),
    Expected = proplists:get_value(expected, Props),
    Value = proplists:get_value(value, Props),
    [io_lib:format("~nFailure/Error: ?assertEqual(~w, ~s)~n", [Expected,
                                                             Expr]),
     io_lib:format("  expected: ~p~n", [Expected]),
     io_lib:format("       got: ~p", [Value]),
     io_lib:format("      line: ~p", [proplists:get_value(line, Props)])];

maybe_eunit_format({failed, {{Type, Props}, _}}) when Type =:= assertNotEqual_failed
                                                    ; Type =:= assertNotEqual ->
    Expr = proplists:get_value(expression, Props),
    Value = proplists:get_value(value, Props),
    [io_lib:format("~nFailure/Error: ?assertNotEqual(~p, ~s)~n",
                   [Value, Expr]),
     io_lib:format("  expected not: == ~p~n", [Value]),
     io_lib:format("           got:    ~p", [Value]),
     io_lib:format("          line: ~p", [proplists:get_value(line, Props)])];

maybe_eunit_format({failed, {{Type, Props}, _}}) when Type =:= assertException_failed
                                                    ; Type =:= assertException ->
    Expr = proplists:get_value(expression, Props),
    Pattern = proplists:get_value(pattern, Props),
    {Class, Term} = extract_exception_pattern(Pattern), % I hate that we have to do this, why not just give DATA
    [io_lib:format("~nFailure/Error: ?assertException(~s, ~s, ~s)~n", [Class, Term, Expr]),
     case proplists:is_defined(unexpected_success, Props) of
         true ->
             [io_lib:format("  expected: exception ~s but nothing was raised~n", [Pattern]),
              io_lib:format("       got: value ~p", [proplists:get_value(unexpected_success, Props)]),
              io_lib:format("      line: ~p", [proplists:get_value(line, Props)])];
         false ->
             Ex = proplists:get_value(unexpected_exception, Props),
             [io_lib:format("  expected: exception ~s~n", [Pattern]),
              io_lib:format("       got: exception ~p", [Ex]),
              io_lib:format("      line: ~p", [proplists:get_value(line, Props)])]
     end];

maybe_eunit_format({failed, {{Type, Props}, _}}) when Type =:= assertNotException_failed
                                                    ; Type =:= assertNotException ->
    Expr = proplists:get_value(expression, Props),
    Pattern = proplists:get_value(pattern, Props),
    {Class, Term} = extract_exception_pattern(Pattern), % I hate that we have to do this, why not just give DAT
    Ex = proplists:get_value(unexpected_exception, Props),
    [io_lib:format("~nFailure/Error: ?assertNotException(~s, ~s, ~s)~n", [Class, Term, Expr]),
     io_lib:format("  expected not: exception ~s~n", [Pattern]),
     io_lib:format("           got: exception ~p", [Ex]),
     io_lib:format("          line: ~p", [proplists:get_value(line, Props)])];

maybe_eunit_format({failed, {{Type, Props}, _}}) when Type =:= command_failed
                                                    ; Type =:= command ->
    Cmd = proplists:get_value(command, Props),
    Expected = proplists:get_value(expected_status, Props),
    Status = proplists:get_value(status, Props),
    [io_lib:format("~nFailure/Error: ?cmdStatus(~p, ~p)~n", [Expected, Cmd]),
     io_lib:format("  expected: status ~p~n", [Expected]),
     io_lib:format("       got: status ~p", [Status]),
     io_lib:format("      line: ~p", [proplists:get_value(line, Props)])];

maybe_eunit_format({failed, {{Type, Props}, _}}) when Type =:= assertCmd_failed
                                                    ; Type =:= assertCmd ->
    Cmd = proplists:get_value(command, Props),
    Expected = proplists:get_value(expected_status, Props),
    Status = proplists:get_value(status, Props),
    [io_lib:format("~nFailure/Error: ?assertCmdStatus(~p, ~p)~n", [Expected, Cmd]),
     io_lib:format("  expected: status ~p~n", [Expected]),
     io_lib:format("       got: status ~p", [Status]),
     io_lib:format("      line: ~p", [proplists:get_value(line, Props)])];

maybe_eunit_format({failed, {{Type, Props}, _}}) when Type =:= assertCmdOutput_failed
                                                    ; Type =:= assertCmdOutput ->
    Cmd = proplists:get_value(command, Props),
    Expected = proplists:get_value(expected_output, Props),
    Output = proplists:get_value(output, Props),
    [io_lib:format("~nFailure/Error: ?assertCmdOutput(~p, ~p)~n", [Expected, Cmd]),
     io_lib:format("  expected: ~p~n", [Expected]),
     io_lib:format("       got: ~p", [Output]),
     io_lib:format("      line: ~p", [proplists:get_value(line, Props)])];

maybe_eunit_format(Reason) ->
    io_lib:format("~p", [Reason]).

extract_exception_pattern(Str) ->
    ["{", Class, Term|_] = string:tokens(Str, ", "),
    {Class, Term}.
