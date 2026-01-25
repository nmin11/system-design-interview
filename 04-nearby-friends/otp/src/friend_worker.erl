%%%-------------------------------------------------------------------
%%% @doc Individual friend process that moves randomly
%%% Speed: 4-60 km/h, updates location every second, stays within Korea
%%% @end
%%%-------------------------------------------------------------------
-module(friend_worker).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(MOVE_INTERVAL_MS, 1000).
-define(MIN_SPEED_KMH, 4.0).
-define(MAX_SPEED_KMH, 60.0).
-define(DIRECTION_CHANGE_PROBABILITY, 0.1).

-record(state, {
    id :: binary(),
    lat :: float(),
    lng :: float(),
    speed_kmh :: float(),
    direction :: float(),
    in_seoul :: boolean()
}).

%%====================================================================
%% API
%%====================================================================

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(#{id := Id, in_seoul := InSeoul}) ->
    rand:seed(exsss, {erlang:unique_integer(), erlang:monotonic_time(), erlang:system_time()}),

    {Lat, Lng} = case InSeoul of
        true -> korea_boundaries:random_point_in_seoul();
        false -> korea_boundaries:random_point_in_korea()
    end,

    Speed = random_speed(),
    Direction = random_direction(),

    location_registry:update_location(Id, Lat, Lng),

    erlang:send_after(?MOVE_INTERVAL_MS, self(), move),

    {ok, #state{
        id = Id,
        lat = Lat,
        lng = Lng,
        speed_kmh = Speed,
        direction = Direction,
        in_seoul = InSeoul
    }}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(move, State = #state{id = Id, lat = Lat, lng = Lng, speed_kmh = Speed, direction = Dir}) ->
    DistanceKm = Speed / 3600.0,

    {NewLat, NewLng} = geo_utils:move_point(Lat, Lng, Dir, DistanceKm),

    {FinalLat, FinalLng, NewDir} =
        case korea_boundaries:is_in_korea(NewLat, NewLng) of
            true ->
                MaybeNewDir = maybe_change_direction(Dir),
                {NewLat, NewLng, MaybeNewDir};
            false ->
                ReversedDir = geo_utils:normalize_angle(Dir + math:pi() + random_offset()),
                {Lat, Lng, ReversedDir}
        end,

    NewSpeed = maybe_change_speed(Speed),

    location_registry:update_location(Id, FinalLat, FinalLng),

    erlang:send_after(?MOVE_INTERVAL_MS, self(), move),

    {noreply, State#state{
        lat = FinalLat,
        lng = FinalLng,
        speed_kmh = NewSpeed,
        direction = NewDir
    }};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{id = Id}) ->
    location_registry:remove_location(Id),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

-spec random_speed() -> float().
random_speed() ->
    ?MIN_SPEED_KMH + rand:uniform() * (?MAX_SPEED_KMH - ?MIN_SPEED_KMH).

-spec random_direction() -> float().
random_direction() ->
    rand:uniform() * 2 * math:pi().

-spec random_offset() -> float().
random_offset() ->
    (rand:uniform() - 0.5) * math:pi() / 2.

-spec maybe_change_direction(float()) -> float().
maybe_change_direction(CurrentDir) ->
    case rand:uniform() < ?DIRECTION_CHANGE_PROBABILITY of
        true ->
            Offset = (rand:uniform() - 0.5) * math:pi(),
            geo_utils:normalize_angle(CurrentDir + Offset);
        false ->
            CurrentDir
    end.

-spec maybe_change_speed(float()) -> float().
maybe_change_speed(CurrentSpeed) ->
    case rand:uniform() < 0.05 of
        true -> random_speed();
        false -> CurrentSpeed
    end.
