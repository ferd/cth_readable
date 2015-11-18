%% Copyright (c) 2011-2012, 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

%% @doc Console backend for lager that mutes logs to the shell when
%% CT runs succeed. Configured with a single option, the loglevel
%% desired.

-module(cth_readable_lager_backend).

-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2,
        code_change/3]).

-record(state, {level :: {'mask', integer()},
                formatter :: atom(),
                format_config :: any(),
                colors=[] :: list()}).

%-include("lager.hrl").
-define(TERSE_FORMAT,[time, " ", color, "[", severity,"] ", message]).

%% @private
init([Level]) when is_atom(Level) ->
    init(Level);
init([Level, true]) -> % for backwards compatibility
    init([Level,{lager_default_formatter,[{eol, eol()}]}]);
init([Level,false]) -> % for backwards compatibility
    init([Level,{lager_default_formatter,?TERSE_FORMAT ++ [eol()]}]);
init([Level,{Formatter,FormatterConfig}]) when is_atom(Formatter) ->
    Colors = case application:get_env(lager, colored) of
        {ok, true} ->
            {ok, LagerColors} = application:get_env(lager, colors),
            LagerColors;
        _ -> []
    end,

    %% edited out a bunch of console detection stuff, hopefully not breaking
    try lager_util:config_to_mask(Level) of
        Levels ->
            {ok, #state{level=Levels,
                        formatter=Formatter,
                        format_config=FormatterConfig,
                        colors=Colors}}
    catch
        _:_ ->
            {error, {fatal, bad_log_level}}
    end;
init(Level) ->
    init([Level,{lager_default_formatter,?TERSE_FORMAT ++ [eol()]}]).

%% @private
handle_call(get_loglevel, #state{level=Level} = State) ->
    {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
   try lager_util:config_to_mask(Level) of
        Levels ->
            {ok, ok, State#state{level=Levels}}
    catch
        _:_ ->
            {ok, {error, bad_log_level}, State}
    end;
handle_call(_Request, State) ->
    {ok, ok, State}.

%% @private
handle_event({log, Message},
    #state{level=L,formatter=Formatter,format_config=FormatConfig,colors=Colors} = State) ->
    case lager_util:is_loggable(Message, L, lager_console_backend) of
        true ->
            %% Handle multiple possible functions -- older lagers didn't
            %% support colors, and we depend on the currently running lib.
            Formatted = case erlang:function_exported(Formatter, format, 3) of
                true ->
                    Formatter:format(Message,FormatConfig,Colors);
                false ->
                    Formatter:format(Message,FormatConfig)
            end,
            %% lagger forwards in sync mode, and a call to error_logger makes
            %% everything deadlock, so we gotta go async on the logging call.
            %% We also need to do a call so that lager doesn't reforward the
            %% event in an infinite loop.
            spawn(fun() -> gen_event:call(error_logger, cth_readable_failonly, {lager, Formatted}) end),
            ct_logs:tc_log(default, Formatted, []),
            {ok, State};
        false ->
            {ok, State}
    end;
handle_event(_Event, State) ->
    {ok, State}.

%% @private
handle_info(_Info, State) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

eol() ->
    case application:get_env(lager, colored) of
        {ok, true}  ->
            "\e[0m\r\n";
        _ ->
            "\r\n"
    end.
