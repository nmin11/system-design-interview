%%%-------------------------------------------------------------------
%%% @doc WebSocket handler for Cowboy
%%% Handles client connections and sends nearby friends updates
%%% @end
%%%-------------------------------------------------------------------
-module(ws_handler).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-define(UPDATE_INTERVAL_MS, 15000).
-define(SEARCH_RADIUS_KM, 8.0).

-record(state, {
    user_lat :: float() | undefined,
    user_lng :: float() | undefined,
    update_timer :: reference() | undefined
}).

%%====================================================================
%% Cowboy WebSocket callbacks
%%====================================================================

init(Req, _State) ->
    {cowboy_websocket, Req, #state{}, #{idle_timeout => 60000}}.

websocket_init(State) ->
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    case jsx:decode(Msg, [return_maps]) of
        #{<<"type">> := <<"search">>, <<"lat">> := Lat, <<"lng">> := Lng} ->
            handle_search(Lat, Lng, State);
        #{<<"type">> := <<"update_position">>, <<"lat">> := Lat, <<"lng">> := Lng} ->
            handle_update_position(Lat, Lng, State);
        _ ->
            ErrorMsg = jsx:encode(#{type => <<"error">>, message => <<"Unknown message type">>}),
            {reply, {text, ErrorMsg}, State}
    end;

websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(send_update, State = #state{user_lat = Lat, user_lng = Lng})
  when Lat =/= undefined, Lng =/= undefined ->
    NearbyFriends = location_registry:get_nearby_friends(Lat, Lng, ?SEARCH_RADIUS_KM),
    Response = format_friends_response(NearbyFriends),
    Timer = erlang:send_after(?UPDATE_INTERVAL_MS, self(), send_update),
    {reply, {text, Response}, State#state{update_timer = Timer}};

websocket_info(send_update, State) ->
    {ok, State};

websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, #state{update_timer = Timer}) ->
    case Timer of
        undefined -> ok;
        _ -> erlang:cancel_timer(Timer)
    end,
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

handle_search(Lat, Lng, State) ->
    case State#state.update_timer of
        undefined -> ok;
        OldTimer -> erlang:cancel_timer(OldTimer)
    end,

    NearbyFriends = location_registry:get_nearby_friends(Lat, Lng, ?SEARCH_RADIUS_KM),
    Response = format_friends_response(NearbyFriends),

    Timer = erlang:send_after(?UPDATE_INTERVAL_MS, self(), send_update),

    NewState = State#state{
        user_lat = Lat,
        user_lng = Lng,
        update_timer = Timer
    },

    {reply, {text, Response}, NewState}.

handle_update_position(Lat, Lng, State) ->
    NewState = State#state{user_lat = Lat, user_lng = Lng},
    {ok, NewState}.

format_friends_response(NearbyFriends) ->
    FriendsData = lists:map(
        fun({Id, Lat, Lng, Distance}) ->
            #{
                id => Id,
                lat => round_coord(Lat),
                lng => round_coord(Lng),
                distance => round_distance(Distance)
            }
        end,
        NearbyFriends
    ),
    jsx:encode(#{type => <<"friends">>, data => FriendsData}).

round_coord(Value) ->
    round(Value * 1000000) / 1000000.

round_distance(Value) ->
    round(Value * 100) / 100.
