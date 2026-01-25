%%%-------------------------------------------------------------------
%%% @doc Geographic utility functions
%%% Haversine distance, coordinate movement, angle conversions
%%% @end
%%%-------------------------------------------------------------------
-module(geo_utils).

-export([
    haversine_distance/4,
    move_point/4,
    deg_to_rad/1,
    rad_to_deg/1,
    normalize_angle/1
]).

-define(EARTH_RADIUS_KM, 6371.0).

%%--------------------------------------------------------------------
%% @doc Calculate distance between two coordinates using Haversine formula
%% @param Lat1, Lng1 - First coordinate (degrees)
%% @param Lat2, Lng2 - Second coordinate (degrees)
%% @returns Distance in kilometers
%%--------------------------------------------------------------------
-spec haversine_distance(float(), float(), float(), float()) -> float().
haversine_distance(Lat1, Lng1, Lat2, Lng2) ->
    DLat = deg_to_rad(Lat2 - Lat1),
    DLng = deg_to_rad(Lng2 - Lng1),
    Lat1Rad = deg_to_rad(Lat1),
    Lat2Rad = deg_to_rad(Lat2),

    A = math:pow(math:sin(DLat / 2), 2) +
        math:cos(Lat1Rad) * math:cos(Lat2Rad) *
        math:pow(math:sin(DLng / 2), 2),
    C = 2 * math:atan2(math:sqrt(A), math:sqrt(1 - A)),
    ?EARTH_RADIUS_KM * C.

%%--------------------------------------------------------------------
%% @doc Move a point by given bearing and distance
%% @param Lat, Lng - Starting coordinate (degrees)
%% @param Bearing - Direction in radians (0 = North, PI/2 = East)
%% @param DistanceKm - Distance to move in kilometers
%% @returns {NewLat, NewLng} in degrees
%%--------------------------------------------------------------------
-spec move_point(float(), float(), float(), float()) -> {float(), float()}.
move_point(Lat, Lng, Bearing, DistanceKm) ->
    LatRad = deg_to_rad(Lat),
    LngRad = deg_to_rad(Lng),
    AngularDist = DistanceKm / ?EARTH_RADIUS_KM,

    NewLatRad = math:asin(
        math:sin(LatRad) * math:cos(AngularDist) +
        math:cos(LatRad) * math:sin(AngularDist) * math:cos(Bearing)
    ),
    NewLngRad = LngRad + math:atan2(
        math:sin(Bearing) * math:sin(AngularDist) * math:cos(LatRad),
        math:cos(AngularDist) - math:sin(LatRad) * math:sin(NewLatRad)
    ),

    {rad_to_deg(NewLatRad), rad_to_deg(NewLngRad)}.

%%--------------------------------------------------------------------
%% @doc Convert degrees to radians
%%--------------------------------------------------------------------
-spec deg_to_rad(float()) -> float().
deg_to_rad(Deg) ->
    Deg * math:pi() / 180.0.

%%--------------------------------------------------------------------
%% @doc Convert radians to degrees
%%--------------------------------------------------------------------
-spec rad_to_deg(float()) -> float().
rad_to_deg(Rad) ->
    Rad * 180.0 / math:pi().

%%--------------------------------------------------------------------
%% @doc Normalize angle to [0, 2*PI) range
%%--------------------------------------------------------------------
-spec normalize_angle(float()) -> float().
normalize_angle(Angle) ->
    TwoPi = 2 * math:pi(),
    Normalized = math:fmod(Angle, TwoPi),
    if
        Normalized < 0 -> Normalized + TwoPi;
        true -> Normalized
    end.
