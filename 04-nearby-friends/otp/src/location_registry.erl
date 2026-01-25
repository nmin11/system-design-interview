%%%-------------------------------------------------------------------
%%% @doc Location registry using ETS for fast location storage and queries
%%% @end
%%%-------------------------------------------------------------------
-module(location_registry).
-behaviour(gen_server).

-export([
    start_link/0,
    update_location/3,
    remove_location/1,
    get_nearby_friends/3,
    get_all_locations/0
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(LOCATIONS_TABLE, friend_locations).

-record(state, {
    locations_ets :: ets:tid()
}).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec update_location(binary(), float(), float()) -> ok.
update_location(FriendId, Lat, Lng) ->
    gen_server:cast(?SERVER, {update_location, FriendId, Lat, Lng}).

-spec remove_location(binary()) -> ok.
remove_location(FriendId) ->
    gen_server:cast(?SERVER, {remove_location, FriendId}).

-spec get_nearby_friends(float(), float(), float()) -> [{binary(), float(), float(), float()}].
get_nearby_friends(Lat, Lng, RadiusKm) ->
    gen_server:call(?SERVER, {get_nearby, Lat, Lng, RadiusKm}).

-spec get_all_locations() -> [{binary(), float(), float()}].
get_all_locations() ->
    gen_server:call(?SERVER, get_all).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    Tid = ets:new(?LOCATIONS_TABLE, [
        set,
        named_table,
        public,
        {read_concurrency, true}
    ]),
    {ok, #state{locations_ets = Tid}}.

handle_call({get_nearby, Lat, Lng, RadiusKm}, _From, State) ->
    NearbyFriends = find_nearby(Lat, Lng, RadiusKm),
    {reply, NearbyFriends, State};

handle_call(get_all, _From, State) ->
    All = ets:tab2list(?LOCATIONS_TABLE),
    Result = [{Id, FLat, FLng} || {Id, FLat, FLng, _Ts} <- All],
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({update_location, FriendId, Lat, Lng}, State) ->
    Timestamp = erlang:system_time(millisecond),
    ets:insert(?LOCATIONS_TABLE, {FriendId, Lat, Lng, Timestamp}),
    {noreply, State};

handle_cast({remove_location, FriendId}, State) ->
    ets:delete(?LOCATIONS_TABLE, FriendId),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

-spec find_nearby(float(), float(), float()) -> [{binary(), float(), float(), float()}].
find_nearby(CenterLat, CenterLng, RadiusKm) ->
    All = ets:tab2list(?LOCATIONS_TABLE),
    Filtered = lists:filtermap(
        fun({FriendId, FLat, FLng, _Ts}) ->
            Distance = geo_utils:haversine_distance(CenterLat, CenterLng, FLat, FLng),
            case Distance =< RadiusKm of
                true -> {true, {FriendId, FLat, FLng, Distance}};
                false -> false
            end
        end,
        All
    ),
    lists:sort(fun({_, _, _, D1}, {_, _, _, D2}) -> D1 =< D2 end, Filtered).
