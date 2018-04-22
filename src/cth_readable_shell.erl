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
-export([on_tc_skip/3, on_tc_skip/4]).

-export([terminate/1]).

-record(state, {id, suite, groups}).

%% @doc Return a unique id for this CTH.
id(_Opts) ->
    {?MODULE, make_ref()}.

%% @doc Always called before any other callback function. Use this to initiate
%% any common state.
init(Id, _Opts) ->
    {ok, #state{id=Id}}.

%% @doc Called before init_per_suite is called.
pre_init_per_suite(Suite,Config,State) ->
    {Config, State#state{suite=Suite, groups=[]}}.

%% @doc Called after init_per_suite.
post_init_per_suite(_Suite,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before end_per_suite.
pre_end_per_suite(_Suite,Config,State) ->
    {Config, State}.

%% @doc Called after end_per_suite.
post_end_per_suite(_Suite,_Config,Return,State) ->
    {Return, State#state{suite=undefined, groups=[]}}.

%% @doc Called before each init_per_group.
pre_init_per_group(_Group,Config,State) ->
    {Config, State}.

%% @doc Called after each init_per_group.
post_init_per_group(Group,_Config,Return, State=#state{groups=Groups}) ->
    {Return, State#state{groups=[Group|Groups]}}.

%% @doc Called after each end_per_group.
pre_end_per_group(_Group,Config,State) ->
    {Config, State}.

%% @doc Called after each end_per_group.
post_end_per_group(_Group,_Config,Return, State=#state{groups=Groups}) ->
    {Return, State#state{groups=tl(Groups)}}.

%% @doc Called before each test case.
pre_init_per_testcase(_TC,Config,State) ->
    {Config, State}.

%% @doc Called after each test case.
post_end_per_testcase(TC,_Config,ok,State=#state{suite=Suite, groups=Groups}) ->
    ?OK(Suite, "~s", [format_path(TC,Groups)]),
    {ok, State};
post_end_per_testcase(TC,Config,Error,State=#state{suite=Suite, groups=Groups}) ->
    case lists:keyfind(tc_status, 1, Config) of
        {tc_status, ok} ->
            %% Test case passed, but we still ended in an error
            ?STACK(Suite, "~s", [format_path(TC,Groups)], Error, ?SKIPC, "end_per_testcase FAILED");
        _ ->
            %% Test case failed, in which case on_tc_fail already reports it
            ok
    end,
    {Error, State}.

%% @doc Called after post_init_per_suite, post_end_per_suite, post_init_per_group,
%% post_end_per_group and post_end_per_testcase if the suite, group or test case failed.
on_tc_fail({TC,_Group}, Reason, State=#state{suite=Suite, groups=Groups}) ->
    ?FAIL(Suite, "~s", [format_path(TC,Groups)], Reason),
    State;
on_tc_fail(TC, Reason, State=#state{suite=Suite, groups=Groups}) ->
    ?FAIL(Suite, "~s", [format_path(TC,Groups)], Reason),
    State.

%% @doc Called when a test case is skipped by either user action
%% or due to an init function failing. (>= 19.3)
on_tc_skip(Suite, {TC,_Group}, Reason, State=#state{groups=Groups}) ->
    ?SKIP(Suite, "~s", [format_path(TC,Groups)], Reason),
    State#state{suite=Suite};
on_tc_skip(Suite, TC, Reason, State=#state{groups=Groups}) ->
    ?SKIP(Suite, "~s", [format_path(TC,Groups)], Reason),
    State#state{suite=Suite}.

%% @doc Called when a test case is skipped by either user action
%% or due to an init function failing. (Pre-19.3)
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

maybe_eunit_format({failed, Reason}) ->
    maybe_eunit_format(Reason);

maybe_eunit_format({{Type, Props}, _}) when Type =:= assert_failed
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
     io_lib:format("       got: ~p~n", [Value]),
     io_lib:format("      line: ~p", [proplists:get_value(line, Props)])];

maybe_eunit_format({{Type, Props}, _}) when Type =:= assertNotMatch_failed
                                            ; Type =:= assertNotMatch  ->
    Expr = proplists:get_value(expression, Props),
    Pattern = proplists:get_value(pattern, Props),
    Value = proplists:get_value(value, Props),
    [io_lib:format("~nFailure/Error: ?assertNotMatch(~s, ~s)~n", [Pattern, Expr]),
     io_lib:format("  expected not: = ~s~n", [Pattern]),
     io_lib:format("           got:   ~p~n", [Value]),
     io_lib:format("          line: ~p", [proplists:get_value(line, Props)])];

maybe_eunit_format({{Type, Props}, _}) when Type =:= assertEqual_failed
                                            ; Type =:= assertEqual  ->
    Expr = proplists:get_value(expression, Props),
    Expected = proplists:get_value(expected, Props),
    Value = proplists:get_value(value, Props),
    [io_lib:format("~nFailure/Error: ?assertEqual(~w, ~s)~n", [Expected,
                                                             Expr]),
     io_lib:format("  expected: ~p~n", [Expected]),
     io_lib:format("       got: ~p~n", [Value]),
     io_lib:format("      line: ~p", [proplists:get_value(line, Props)])];

maybe_eunit_format({{Type, Props}, _}) when Type =:= assertNotEqual_failed
                                            ; Type =:= assertNotEqual ->
    Expr = proplists:get_value(expression, Props),
    Value = proplists:get_value(value, Props),
    [io_lib:format("~nFailure/Error: ?assertNotEqual(~p, ~s)~n",
                   [Value, Expr]),
     io_lib:format("  expected not: == ~p~n", [Value]),
     io_lib:format("           got:    ~p~n", [Value]),
     io_lib:format("          line: ~p", [proplists:get_value(line, Props)])];

maybe_eunit_format({{Type, Props}, _}) when Type =:= assertException_failed
                                            ; Type =:= assertException ->
    Expr = proplists:get_value(expression, Props),
    Pattern = proplists:get_value(pattern, Props),
    {Class, Term} = extract_exception_pattern(Pattern), % I hate that we have to do this, why not just give DATA
    [io_lib:format("~nFailure/Error: ?assertException(~s, ~s, ~s)~n", [Class, Term, Expr]),
     case proplists:is_defined(unexpected_success, Props) of
         true ->
             [io_lib:format("  expected: exception ~s but nothing was raised~n", [Pattern]),
              io_lib:format("       got: value ~p~n", [proplists:get_value(unexpected_success, Props)]),
              io_lib:format("      line: ~p", [proplists:get_value(line, Props)])];
         false ->
             Ex = proplists:get_value(unexpected_exception, Props),
             [io_lib:format("  expected: exception ~s~n", [Pattern]),
              io_lib:format("       got: exception ~p~n", [Ex]),
              io_lib:format("      line: ~p", [proplists:get_value(line, Props)])]
     end];

maybe_eunit_format({{Type, Props}, _}) when Type =:= assertNotException_failed
                                            ; Type =:= assertNotException ->
    Expr = proplists:get_value(expression, Props),
    Pattern = proplists:get_value(pattern, Props),
    {Class, Term} = extract_exception_pattern(Pattern), % I hate that we have to do this, why not just give DAT
    Ex = proplists:get_value(unexpected_exception, Props),
    [io_lib:format("~nFailure/Error: ?assertNotException(~s, ~s, ~s)~n", [Class, Term, Expr]),
     io_lib:format("  expected not: exception ~s~n", [Pattern]),
     io_lib:format("           got: exception ~p~n", [Ex]),
     io_lib:format("          line: ~p", [proplists:get_value(line, Props)])];

maybe_eunit_format({{Type, Props}, _}) when Type =:= command_failed
                                            ; Type =:= command ->
    Cmd = proplists:get_value(command, Props),
    Expected = proplists:get_value(expected_status, Props),
    Status = proplists:get_value(status, Props),
    [io_lib:format("~nFailure/Error: ?cmdStatus(~p, ~p)~n", [Expected, Cmd]),
     io_lib:format("  expected: status ~p~n", [Expected]),
     io_lib:format("       got: status ~p~n", [Status]),
     io_lib:format("      line: ~p", [proplists:get_value(line, Props)])];

maybe_eunit_format({{Type, Props}, _}) when Type =:= assertCmd_failed
                                            ; Type =:= assertCmd ->
    Cmd = proplists:get_value(command, Props),
    Expected = proplists:get_value(expected_status, Props),
    Status = proplists:get_value(status, Props),
    [io_lib:format("~nFailure/Error: ?assertCmdStatus(~p, ~p)~n", [Expected, Cmd]),
     io_lib:format("  expected: status ~p~n", [Expected]),
     io_lib:format("       got: status ~p~n", [Status]),
     io_lib:format("      line: ~p", [proplists:get_value(line, Props)])];

maybe_eunit_format({{Type, Props}, _}) when Type =:= assertCmdOutput_failed
                                            ; Type =:= assertCmdOutput ->
    Cmd = proplists:get_value(command, Props),
    Expected = proplists:get_value(expected_output, Props),
    Output = proplists:get_value(output, Props),
    [io_lib:format("~nFailure/Error: ?assertCmdOutput(~p, ~p)~n", [Expected, Cmd]),
     io_lib:format("  expected: ~p~n", [Expected]),
     io_lib:format("       got: ~p~n", [Output]),
     io_lib:format("      line: ~p", [proplists:get_value(line, Props)])];

maybe_eunit_format(Reason) ->
    io_lib:format("~p", [Reason]).

extract_exception_pattern(Str) ->
    ["{", Class, Term|_] = re:split(Str, "[, ]{1,2}", [unicode,{return,list}]),
    {Class, Term}.

format_path(TC, Groups) ->
    join([atom_to_list(P) || P <- lists:reverse([TC|Groups])], ".").

%% string:join/2 copy; string:join/2 is getting obsoleted
%% and replaced by lists:join/2, but lists:join/2 is too new
%% for version support (only appeared in 19.0) so it cannot be
%% used. Instead we just adopt join/2 locally and hope it works
%% for most unicode use cases anyway.
join([], Sep) when is_list(Sep) ->
    [];
join([H|T], Sep) ->
    H ++ lists:append([Sep ++ X || X <- T]).
