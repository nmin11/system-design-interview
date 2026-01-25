%%%-------------------------------------------------------------------
%% @doc otp public API
%% @end
%%%-------------------------------------------------------------------

-module(otp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    otp_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
