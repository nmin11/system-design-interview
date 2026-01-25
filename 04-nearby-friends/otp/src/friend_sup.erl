%%%-------------------------------------------------------------------
%%% @doc Supervisor for friend worker processes
%%% Uses simple_one_for_one for dynamic friend spawning
%%% @end
%%%-------------------------------------------------------------------
-module(friend_sup).
-behaviour(supervisor).

-export([start_link/0, start_friend/1, spawn_friends/2]).
-export([init/1]).

-define(SERVER, ?MODULE).
-define(TOTAL_FRIENDS, 400).
-define(SEOUL_FRIENDS, 100).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_friend(map()) -> {ok, pid()} | {error, term()}.
start_friend(Args) ->
    supervisor:start_child(?SERVER, [Args]).

-spec spawn_friends(integer(), integer()) -> ok.
spawn_friends(TotalCount, SeoulCount) ->
    KoreaCount = TotalCount - SeoulCount,

    lists:foreach(
        fun(N) ->
            Id = list_to_binary(io_lib:format("friend_~4..0B", [N])),
            start_friend(#{id => Id, in_seoul => false})
        end,
        lists:seq(1, KoreaCount)
    ),

    lists:foreach(
        fun(N) ->
            Id = list_to_binary(io_lib:format("friend_~4..0B", [KoreaCount + N])),
            start_friend(#{id => Id, in_seoul => true})
        end,
        lists:seq(1, SeoulCount)
    ),

    ok.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpec = #{
        id => friend_worker,
        start => {friend_worker, start_link, []},
        restart => transient,
        shutdown => 5000,
        type => worker,
        modules => [friend_worker]
    },

    {ok, {SupFlags, [ChildSpec]}}.
