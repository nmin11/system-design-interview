%%%-------------------------------------------------------------------
%%% @doc Korea geographical boundaries and random point generation
%%% @end
%%%-------------------------------------------------------------------
-module(korea_boundaries).

-export([
    is_in_korea/2,
    is_in_seoul/2,
    random_point_in_korea/0,
    random_point_in_seoul/0,
    clamp_to_korea/2,
    korea_bounds/0,
    seoul_bounds/0
]).

-define(KOREA_MIN_LAT, 33.0).
-define(KOREA_MAX_LAT, 38.6).
-define(KOREA_MIN_LNG, 124.5).
-define(KOREA_MAX_LNG, 131.0).

-define(SEOUL_MIN_LAT, 37.413).
-define(SEOUL_MAX_LAT, 37.715).
-define(SEOUL_MIN_LNG, 126.734).
-define(SEOUL_MAX_LNG, 127.269).

-define(KOREA_POLYGON, [
    {38.6, 128.3},
    {38.3, 126.0},
    {37.7, 124.6},
    {36.0, 126.0},
    {34.5, 126.0},
    {34.0, 126.5},
    {33.1, 126.2},
    {33.1, 126.9},
    {34.3, 127.5},
    {34.7, 128.5},
    {35.1, 129.1},
    {35.5, 129.4},
    {36.0, 129.5},
    {37.0, 129.5},
    {37.5, 129.3},
    {38.6, 128.3}
]).

%%--------------------------------------------------------------------
%% @doc Check if a point is within South Korea boundaries
%% Uses simplified bounding box + polygon check
%%--------------------------------------------------------------------
-spec is_in_korea(float(), float()) -> boolean().
is_in_korea(Lat, Lng) ->
    InBoundingBox = Lat >= ?KOREA_MIN_LAT andalso Lat =< ?KOREA_MAX_LAT andalso
                    Lng >= ?KOREA_MIN_LNG andalso Lng =< ?KOREA_MAX_LNG,
    case InBoundingBox of
        false -> false;
        true -> point_in_polygon(Lat, Lng, ?KOREA_POLYGON)
    end.

%%--------------------------------------------------------------------
%% @doc Check if a point is within Seoul boundaries
%%--------------------------------------------------------------------
-spec is_in_seoul(float(), float()) -> boolean().
is_in_seoul(Lat, Lng) ->
    Lat >= ?SEOUL_MIN_LAT andalso Lat =< ?SEOUL_MAX_LAT andalso
    Lng >= ?SEOUL_MIN_LNG andalso Lng =< ?SEOUL_MAX_LNG.

%%--------------------------------------------------------------------
%% @doc Generate a random point within South Korea
%%--------------------------------------------------------------------
-spec random_point_in_korea() -> {float(), float()}.
random_point_in_korea() ->
    generate_random_point_in_korea(100).

generate_random_point_in_korea(0) ->
    {37.5665, 126.978};
generate_random_point_in_korea(Attempts) ->
    Lat = ?KOREA_MIN_LAT + rand:uniform() * (?KOREA_MAX_LAT - ?KOREA_MIN_LAT),
    Lng = ?KOREA_MIN_LNG + rand:uniform() * (?KOREA_MAX_LNG - ?KOREA_MIN_LNG),
    case is_in_korea(Lat, Lng) of
        true -> {Lat, Lng};
        false -> generate_random_point_in_korea(Attempts - 1)
    end.

%%--------------------------------------------------------------------
%% @doc Generate a random point within Seoul
%%--------------------------------------------------------------------
-spec random_point_in_seoul() -> {float(), float()}.
random_point_in_seoul() ->
    Lat = ?SEOUL_MIN_LAT + rand:uniform() * (?SEOUL_MAX_LAT - ?SEOUL_MIN_LAT),
    Lng = ?SEOUL_MIN_LNG + rand:uniform() * (?SEOUL_MAX_LNG - ?SEOUL_MIN_LNG),
    {Lat, Lng}.

%%--------------------------------------------------------------------
%% @doc Clamp coordinates to stay within Korea boundaries
%%--------------------------------------------------------------------
-spec clamp_to_korea(float(), float()) -> {float(), float()}.
clamp_to_korea(Lat, Lng) ->
    ClampedLat = max(?KOREA_MIN_LAT, min(?KOREA_MAX_LAT, Lat)),
    ClampedLng = max(?KOREA_MIN_LNG, min(?KOREA_MAX_LNG, Lng)),
    {ClampedLat, ClampedLng}.

%%--------------------------------------------------------------------
%% @doc Get Korea bounding box
%%--------------------------------------------------------------------
-spec korea_bounds() -> map().
korea_bounds() ->
    #{
        min_lat => ?KOREA_MIN_LAT,
        max_lat => ?KOREA_MAX_LAT,
        min_lng => ?KOREA_MIN_LNG,
        max_lng => ?KOREA_MAX_LNG
    }.

%%--------------------------------------------------------------------
%% @doc Get Seoul bounding box
%%--------------------------------------------------------------------
-spec seoul_bounds() -> map().
seoul_bounds() ->
    #{
        min_lat => ?SEOUL_MIN_LAT,
        max_lat => ?SEOUL_MAX_LAT,
        min_lng => ?SEOUL_MIN_LNG,
        max_lng => ?SEOUL_MAX_LNG
    }.

%%--------------------------------------------------------------------
%% Internal: Ray casting algorithm for point-in-polygon test
%%--------------------------------------------------------------------
-spec point_in_polygon(float(), float(), [{float(), float()}]) -> boolean().
point_in_polygon(Lat, Lng, Polygon) ->
    point_in_polygon(Lat, Lng, Polygon, Polygon, false).

point_in_polygon(_Lat, _Lng, _Original, [_], Inside) ->
    Inside;
point_in_polygon(Lat, Lng, Original, [{Y1, X1}, {Y2, X2} | Rest], Inside) ->
    Crosses = ((Y1 > Lat) =/= (Y2 > Lat)) andalso
              (Lng < (X2 - X1) * (Lat - Y1) / (Y2 - Y1) + X1),
    NewInside = case Crosses of
        true -> not Inside;
        false -> Inside
    end,
    point_in_polygon(Lat, Lng, Original, [{Y2, X2} | Rest], NewInside);
point_in_polygon(_Lat, _Lng, _Original, [], Inside) ->
    Inside.
