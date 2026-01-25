%%%-------------------------------------------------------------------
%%% @doc otp top level supervisor.
%%% Starts location_registry, friend_sup, and Cowboy WebSocket listener
%%% @end
%%%-------------------------------------------------------------------
-module(otp_sup).
-behaviour(supervisor).

-export([start_link/0, spawn_friends/0]).
-export([init/1]).

-define(SERVER, ?MODULE).
-define(WS_PORT, 8080).
-define(TOTAL_FRIENDS, 400).
-define(SEOUL_FRIENDS, 100).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    start_cowboy(),

    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    LocationRegistry = #{
        id => location_registry,
        start => {location_registry, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [location_registry]
    },

    FriendSup = #{
        id => friend_sup,
        start => {friend_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [friend_sup]
    },

    ChildSpecs = [LocationRegistry, FriendSup],
    {ok, {SupFlags, ChildSpecs}}.

start_cowboy() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", ws_handler, []},
            {"/health", health_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        http_listener,
        [{port, ?WS_PORT}],
        #{env => #{dispatch => Dispatch}}
    ),
    io:format("WebSocket server started on port ~p~n", [?WS_PORT]),
    ok.

spawn_friends() ->
    timer:sleep(100),
    io:format("Spawning ~p friends (~p in Seoul, ~p elsewhere in Korea)...~n",
              [?TOTAL_FRIENDS, ?SEOUL_FRIENDS, ?TOTAL_FRIENDS - ?SEOUL_FRIENDS]),
    friend_sup:spawn_friends(?TOTAL_FRIENDS, ?SEOUL_FRIENDS),
    io:format("All friends spawned successfully!~n"),
    ok.
