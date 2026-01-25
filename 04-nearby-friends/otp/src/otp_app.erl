%%%-------------------------------------------------------------------
%% @doc otp public API
%% @end
%%%-------------------------------------------------------------------

-module(otp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case otp_sup:start_link() of
        {ok, Pid} ->
            spawn(fun() -> otp_sup:spawn_friends() end),
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.

%% internal functions
