%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(dm_voice_state).
-typing([eqwalizer]).

-export([handle_dm_disconnect/4, disconnect_voice_user/2]).
-export([resolve_call_region/1, resolve_call_region/2, voice_region_for_rpc/1]).
-export([normalize_session_id/1, resolve_effective_session_id/2]).
-export([validate_dm_viewer_stream_keys/3]).
-export([maybe_attach_voice_routing_metadata/3]).
-export([clear_dm_e2ee_room_key_if_channel_empty/3]).
-export([clear_dm_e2ee_room_keys_for_removed_voice_states/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export_type([
    dm_state/0,
    voice_state/0,
    voice_state_map/0
]).

-type dm_state() :: map().
-type voice_state() :: map().
-type voice_state_map() :: #{binary() => voice_state()}.

-spec handle_dm_disconnect(binary() | undefined, integer(), voice_state_map(), dm_state()) ->
    {reply, map(), dm_state()} | {reply, {error, atom(), atom()}, dm_state()}.
handle_dm_disconnect(undefined, _UserId, _VoiceStates, State) ->
    {reply, gateway_errors:error(voice_missing_connection_id), State};
handle_dm_disconnect(ConnectionId, _UserId, VoiceStates, State) ->
    case maps:get(ConnectionId, VoiceStates, undefined) of
        undefined -> {reply, #{success => true}, State};
        OldVoiceState -> do_dm_disconnect(ConnectionId, OldVoiceState, VoiceStates, State)
    end.

-spec do_dm_disconnect(binary(), voice_state(), voice_state_map(), dm_state()) ->
    {reply, map(), dm_state()}.
do_dm_disconnect(ConnectionId, OldVoiceState, VoiceStates, State) ->
    NewVoiceStates = maps:remove(ConnectionId, VoiceStates),
    NewState0 = State#{dm_voice_states => NewVoiceStates},
    OldChannelId = maps:get(<<"channel_id">>, OldVoiceState, null),
    NewState = clear_dm_e2ee_room_key_if_channel_empty(OldChannelId, NewVoiceStates, NewState0),
    DisconnectVS = OldVoiceState#{
        <<"channel_id">> => null, <<"connection_id">> => ConnectionId
    },
    SessionId = maps:get(id, State),
    leave_call_if_needed(OldChannelId, SessionId, ConnectionId, State),
    broadcast_disconnect_if_needed(OldChannelId, DisconnectVS, NewState),
    {reply, #{success => true}, NewState}.

-spec leave_call_if_needed(term(), term(), binary(), dm_state()) -> ok.
leave_call_if_needed(null, _SessionId, ConnectionId, _State) ->
    _ = voice_state_counts_cache:remove_connection(ConnectionId),
    ok;
leave_call_if_needed(ChannelId, SessionId, ConnectionId, State) ->
    case guild_voice_connection_normalize:normalize_positive_snowflake(ChannelId) of
        undefined ->
            _ = voice_state_counts_cache:remove_connection(ConnectionId),
            ok;
        ChannelIdInt ->
            do_leave_call(ChannelIdInt, SessionId, ConnectionId, State)
    end.

-spec do_leave_call(integer(), term(), binary(), dm_state()) -> ok.
do_leave_call(ChannelIdInt, SessionId, ConnectionId, State) ->
    SessionPid = maps:get(session_pid, State),
    gen_server:cast(SessionPid, {call_unmonitor, ChannelIdInt}),
    spawn(fun() -> leave_call_async(ChannelIdInt, SessionId, ConnectionId) end),
    ok.

-spec leave_call_async(integer(), term(), binary()) -> ok.
leave_call_async(ChannelIdInt, SessionId, ConnectionId) ->
    _ = shard_utils:safe_apply(fun() -> maybe_leave_call(ChannelIdInt, SessionId) end, ok),
    _ = voice_state_counts_cache:remove_connection(ConnectionId),
    ok.

-spec maybe_leave_call(integer(), term()) -> ok.
maybe_leave_call(ChannelIdInt, SessionId) ->
    case call_manager:lookup(ChannelIdInt) of
        {ok, CallPid} -> gen_server:call(CallPid, {leave, SessionId}, 5000);
        _ -> ok
    end.

-spec broadcast_disconnect_if_needed(term(), voice_state(), dm_state()) -> ok.
broadcast_disconnect_if_needed(null, _DisconnectVS, _State) ->
    ok;
broadcast_disconnect_if_needed(OldChannelId, DisconnectVS, State) ->
    case validation:validate_snowflake(<<"channel_id">>, OldChannelId) of
        {ok, OldChannelIdInt} ->
            dm_voice_ring:broadcast_voice_state_update(OldChannelIdInt, DisconnectVS, State);
        {error, _, _Reason} ->
            ok
    end.

-spec disconnect_voice_user(integer(), dm_state()) -> {reply, map(), dm_state()}.
disconnect_voice_user(UserId, State) ->
    VoiceStates = maps:get(dm_voice_states, State, #{}),
    SessionId = normalize_session_id(maps:get(id, State, undefined)),
    UserVoiceStates = maps:filter(
        fun(_ConnId, VS) ->
            snowflake_id:equal(UserId, maps:get(<<"user_id">>, VS, undefined)) andalso
                voice_state_session_matches(VS, SessionId)
        end,
        VoiceStates
    ),
    case maps:size(UserVoiceStates) of
        0 -> {reply, #{success => true}, State};
        _ -> do_disconnect_voice_user(UserVoiceStates, VoiceStates, State)
    end.

-spec voice_state_session_matches(voice_state(), binary() | undefined) -> boolean().
voice_state_session_matches(_VoiceState, undefined) ->
    true;
voice_state_session_matches(VoiceState, SessionId) ->
    case normalize_session_id(maps:get(<<"session_id">>, VoiceState, undefined)) of
        undefined -> true;
        SessionId -> true;
        _ -> false
    end.

-spec do_disconnect_voice_user(voice_state_map(), voice_state_map(), dm_state()) ->
    {reply, map(), dm_state()}.
do_disconnect_voice_user(UserVoiceStates, VoiceStates, State) ->
    NewVoiceStates = maps:fold(
        fun(ConnId, _VS, Acc) -> maps:remove(ConnId, Acc) end,
        VoiceStates,
        UserVoiceStates
    ),
    NewState0 = State#{dm_voice_states => NewVoiceStates},
    NewState = clear_dm_e2ee_room_keys_for_removed_voice_states(
        UserVoiceStates, NewVoiceStates, NewState0
    ),
    maps:foreach(
        fun(ConnId, _VS) ->
            _ = voice_state_counts_cache:remove_connection(ConnId),
            ok
        end,
        UserVoiceStates
    ),
    spawn_disconnect_broadcasts(UserVoiceStates, NewState),
    {reply, #{success => true}, NewState}.

-spec spawn_disconnect_broadcasts(voice_state_map(), dm_state()) -> ok.
spawn_disconnect_broadcasts(UserVoiceStates, NewState) ->
    spawn(fun() -> broadcast_user_disconnects(UserVoiceStates, NewState) end),
    ok.

-spec broadcast_user_disconnects(voice_state_map(), dm_state()) -> ok.
broadcast_user_disconnects(UserVoiceStates, NewState) ->
    maps:foreach(
        fun(ConnId, VS) -> broadcast_user_disconnect(ConnId, VS, NewState) end,
        UserVoiceStates
    ).

-spec broadcast_user_disconnect(binary(), voice_state(), dm_state()) -> ok.
broadcast_user_disconnect(ConnId, VoiceState, NewState) ->
    ChannelId = maps:get(<<"channel_id">>, VoiceState, null),
    DisconnectVS = VoiceState#{<<"channel_id">> => null, <<"connection_id">> => ConnId},
    broadcast_disconnect_if_needed(ChannelId, DisconnectVS, NewState).

-spec clear_dm_e2ee_room_key_if_channel_empty(term(), voice_state_map(), dm_state()) ->
    dm_state().
clear_dm_e2ee_room_key_if_channel_empty(ChannelId, VoiceStates, State) ->
    case guild_voice_connection_normalize:normalize_positive_snowflake(ChannelId) of
        undefined ->
            State;
        ChannelIdInt ->
            guild_voice_e2ee:forget_room_key_if_channel_empty_dm(
                ChannelIdInt, VoiceStates, State
            )
    end.

-spec clear_dm_e2ee_room_keys_for_removed_voice_states(
    voice_state_map(), voice_state_map(), dm_state()
) -> dm_state().
clear_dm_e2ee_room_keys_for_removed_voice_states(RemovedVS, NewVS, State) ->
    maps:fold(
        fun(_ConnId, VS, AccState) ->
            clear_dm_e2ee_room_key_if_channel_empty(
                maps:get(<<"channel_id">>, VS, null), NewVS, AccState
            )
        end,
        State,
        RemovedVS
    ).

-spec resolve_call_region(integer()) -> binary() | null.
resolve_call_region(ChannelId) ->
    case call_manager:lookup(ChannelId) of
        {ok, CallPid} -> call_region_from_pid(CallPid, 5000);
        _ -> null
    end.

-spec resolve_call_region(integer(), dm_state()) -> binary() | null.
resolve_call_region(ChannelId, State) ->
    Calls = maps:get(calls, State, #{}),
    case maps:get(ChannelId, Calls, undefined) of
        {CallPid, _Ref} when is_pid(CallPid) -> call_region_from_pid(CallPid, 250);
        _ -> null
    end.

-spec call_region_from_pid(pid(), timeout()) -> binary() | null.
call_region_from_pid(CallPid, Timeout) ->
    case gateway_rpc_call_lookup:safe_gen_server_call(CallPid, {get_state}, Timeout) of
        {ok, {ok, CallData}} when is_map(CallData) ->
            voice_region_for_rpc(maps:get(region, CallData, null));
        {ok, CallData} when is_map(CallData) ->
            voice_region_for_rpc(maps:get(region, CallData, null));
        _ ->
            null
    end.

-spec voice_region_for_rpc(term()) -> binary() | null.
voice_region_for_rpc(<<"automatic">>) -> null;
voice_region_for_rpc(Region) when is_binary(Region) -> Region;
voice_region_for_rpc(_) -> null.

-spec normalize_session_id(term()) -> binary() | undefined.
normalize_session_id(Value) ->
    guild_voice_connection_normalize:normalize_session_id(Value).

-spec resolve_effective_session_id(term(), term()) -> binary() | undefined.
resolve_effective_session_id(ExistingSessionId, RequestSessionId) ->
    ExistingNormalized = normalize_session_id(ExistingSessionId),
    RequestNormalized = normalize_session_id(RequestSessionId),
    case ExistingNormalized of
        undefined -> RequestNormalized;
        RequestNormalized -> RequestNormalized;
        _ -> ExistingNormalized
    end.

-spec validate_dm_viewer_stream_keys(term(), integer(), voice_state_map()) ->
    {ok, list()} | {error, atom()}.
validate_dm_viewer_stream_keys(RawKeys, _ChId, _VS) when
    RawKeys =:= undefined; RawKeys =:= null
->
    {ok, []};
validate_dm_viewer_stream_keys(RawKeys, _ChId, _VS) when not is_list(RawKeys) ->
    {error, voice_invalid_state};
validate_dm_viewer_stream_keys(Keys, ChId, VS) ->
    validate_keys_list(Keys, ChId, VS, []).

-spec validate_keys_list(list(), integer(), voice_state_map(), list()) ->
    {ok, list()} | {error, atom()}.
validate_keys_list([], _ChId, _VS, Acc) ->
    {ok, lists:reverse(Acc)};
validate_keys_list([Key | Rest], ChId, VS, Acc) ->
    case validate_single_key(Key, ChId, VS) of
        {ok, ValidKey} -> validate_keys_list(Rest, ChId, VS, [ValidKey | Acc]);
        {error, _} = Error -> Error
    end.

-spec validate_single_key(term(), integer(), voice_state_map()) ->
    {ok, binary()} | {error, atom()}.
validate_single_key(RawKey, _ChId, _VS) when not is_binary(RawKey) ->
    {error, voice_invalid_state};
validate_single_key(RawKey, ChannelIdValue, VoiceStates) ->
    case voice_state_utils:parse_stream_key(RawKey) of
        {ok, #{scope := dm, channel_id := ParsedChId, connection_id := ConnId}} when
            ParsedChId =:= ChannelIdValue
        ->
            check_stream_key_conn(ConnId, ChannelIdValue, VoiceStates, RawKey);
        _ ->
            {error, voice_invalid_state}
    end.

-spec check_stream_key_conn(binary(), integer(), voice_state_map(), binary()) ->
    {ok, binary()} | {error, atom()}.
check_stream_key_conn(ConnId, ChannelIdValue, VoiceStates, RawKey) ->
    case maps:get(ConnId, VoiceStates, undefined) of
        undefined ->
            {error, voice_connection_not_found};
        StreamVS ->
            validate_stream_channel(StreamVS, ChannelIdValue, RawKey)
    end.

-spec validate_stream_channel(voice_state(), integer(), binary()) ->
    {ok, binary()} | {error, atom()}.
validate_stream_channel(StreamVS, ChannelIdValue, RawKey) ->
    case voice_state_utils:voice_state_channel_id(StreamVS) of
        ChannelIdValue -> {ok, RawKey};
        _ -> {error, voice_invalid_state}
    end.

-spec maybe_attach_voice_routing_metadata(voice_state(), term(), term()) -> voice_state().
maybe_attach_voice_routing_metadata(VoiceState, RegionIdRaw, ServerIdRaw) ->
    RegionId = normalize_optional_binary(RegionIdRaw),
    ServerId = normalize_optional_binary(ServerIdRaw),
    VS1 = maybe_put_field(VoiceState, <<"region_id">>, RegionId),
    maybe_put_field(VS1, <<"server_id">>, ServerId).

-spec maybe_put_field(voice_state(), binary(), binary() | undefined) -> voice_state().
maybe_put_field(VoiceState, _Key, undefined) -> VoiceState;
maybe_put_field(VoiceState, Key, Value) -> VoiceState#{Key => Value}.

-spec normalize_optional_binary(term()) -> binary() | undefined.
normalize_optional_binary(Value) ->
    guild_voice_connection_normalize:normalize_optional_binary(Value).

-ifdef(TEST).

normalize_session_id_test() ->
    ?assertEqual(undefined, normalize_session_id(undefined)),
    ?assertEqual(<<"abc">>, normalize_session_id(<<"abc">>)),
    ?assertEqual(<<"123">>, normalize_session_id(123)),
    ?assertEqual(<<"test">>, normalize_session_id("test")).

validate_dm_viewer_stream_keys_null_test() ->
    ?assertEqual({ok, []}, validate_dm_viewer_stream_keys(undefined, 123, #{})),
    ?assertEqual({ok, []}, validate_dm_viewer_stream_keys(null, 123, #{})).

validate_dm_viewer_stream_keys_invalid_type_test() ->
    ?assertEqual({error, voice_invalid_state}, validate_dm_viewer_stream_keys(123, 456, #{})).

resolve_effective_session_id_test() ->
    ?assertEqual(<<"req">>, resolve_effective_session_id(undefined, <<"req">>)),
    ?assertEqual(<<"existing">>, resolve_effective_session_id(<<"existing">>, <<"req">>)),
    ?assertEqual(<<"same">>, resolve_effective_session_id(<<"same">>, <<"same">>)).

session_voice_state(UserId, ChannelId, ConnId, SessionId) ->
    #{
        <<"user_id">> => integer_to_binary(UserId),
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"connection_id">> => ConnId,
        <<"session_id">> => SessionId
    }.

disconnect_voice_user_keeps_other_session_connections_test() ->
    State = #{
        id => <<"session-1">>,
        user_id => 5,
        channels => #{},
        dm_voice_states => #{
            <<"conn-own">> => session_voice_state(5, 100, <<"conn-own">>, <<"session-1">>),
            <<"conn-other">> => session_voice_state(5, 100, <<"conn-other">>, <<"session-2">>),
            <<"conn-peer">> => session_voice_state(6, 100, <<"conn-peer">>, <<"session-3">>)
        }
    },
    {reply, #{success := true}, NewState} = disconnect_voice_user(5, State),
    VoiceStates = maps:get(dm_voice_states, NewState),
    ?assertNot(maps:is_key(<<"conn-own">>, VoiceStates)),
    ?assert(maps:is_key(<<"conn-other">>, VoiceStates)),
    ?assert(maps:is_key(<<"conn-peer">>, VoiceStates)).

disconnect_voice_user_without_session_id_test() ->
    State = #{
        user_id => 5,
        channels => #{},
        dm_voice_states => #{
            <<"conn-a">> => session_voice_state(5, 100, <<"conn-a">>, <<"session-1">>),
            <<"conn-b">> => session_voice_state(5, 100, <<"conn-b">>, <<"session-2">>)
        }
    },
    {reply, #{success := true}, NewState} = disconnect_voice_user(5, State),
    ?assertEqual(#{}, maps:get(dm_voice_states, NewState)).

disconnect_voice_user_keeps_unattributed_connections_test() ->
    State = #{
        id => <<"session-1">>,
        user_id => 5,
        channels => #{},
        dm_voice_states => #{
            <<"conn-legacy">> => #{
                <<"user_id">> => <<"5">>,
                <<"channel_id">> => <<"100">>,
                <<"connection_id">> => <<"conn-legacy">>
            },
            <<"conn-own">> => session_voice_state(5, 100, <<"conn-own">>, <<"session-1">>)
        }
    },
    {reply, #{success := true}, NewState} = disconnect_voice_user(5, State),
    ?assertEqual(#{}, maps:get(dm_voice_states, NewState)).

call_region_from_dead_pid_returns_null_test() ->
    DeadPid = spawn(fun() -> ok end),
    Ref = monitor(process, DeadPid),
    receive
        {'DOWN', Ref, process, DeadPid, _Reason} -> ok
    after 1000 ->
        ?assert(false, call_pid_did_not_exit)
    end,
    ?assertEqual(null, call_region_from_pid(DeadPid, 250)).

-endif.
