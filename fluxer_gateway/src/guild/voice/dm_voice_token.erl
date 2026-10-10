%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(dm_voice_token).
-typing([eqwalizer]).

-export([get_dm_voice_token_and_create_state/1, get_voice_token/6]).
-export([join_or_create_call/6]).
-export([maybe_spawn_join_call/6, dispatch_to_session/4]).

-export_type([
    dm_state/0,
    voice_state/0,
    token_request/0
]).

-type dm_state() :: map().
-type voice_state() :: map().
-type token_request() :: map().

-spec get_dm_voice_token_and_create_state(
    token_request()
) -> {reply, map(), dm_state()} | {reply, {error, atom(), atom()}, dm_state()}.
get_dm_voice_token_and_create_state(Req) ->
    UserId = maps:get(user_id, Req),
    ChannelId = maps:get(channel_id, Req),
    Latitude = maps:get(latitude, Req),
    Longitude = maps:get(longitude, Req),
    State = maps:get(state, Req),
    TokenReq = voice_utils:build_voice_token_rpc_request(
        null, ChannelId, UserId, null, Latitude, Longitude
    ),
    Region = dm_voice_state:resolve_call_region(ChannelId, State),
    ReqWithRegion = voice_utils:add_rtc_region_to_request(TokenReq, Region),
    logger:debug(
        "dm_voice_token_create_request: user_id=~p channel_id=~p rtc_region=~p",
        [UserId, ChannelId, Region]
    ),
    case dm_join_decision(Req) of
        {reject, ErrorAtom} ->
            {reply, gateway_errors:error(ErrorAtom), State};
        Decision ->
            request_dm_token(
                voice_p2p:add_to_token_request(
                    ReqWithRegion, Decision, maps:get(country_code, Req, undefined)
                ),
                Decision,
                Req
            )
    end.

-spec dm_join_decision(token_request()) -> voice_p2p:join_decision().
dm_join_decision(Req) ->
    UserId = maps:get(user_id, Req),
    voice_p2p:join_decision(
        voice_p2p:agreed(maps:get(p2p, Req, undefined), maps:get(bot, Req, false)),
        [
            VoiceState
         || VoiceState <- dm_voice_state:call_voice_states(maps:get(channel_id, Req)),
            not guild_voice_state:user_matches_voice_state(VoiceState, UserId)
        ],
        false,
        false
    ).

-spec request_dm_token(map(), voice_p2p:join_decision(), token_request()) ->
    {reply, map(), dm_state()} | {reply, {error, atom(), atom()}, dm_state()}.
request_dm_token(TokenReq, Decision, Req) ->
    UserId = maps:get(user_id, Req),
    ChannelId = maps:get(channel_id, Req),
    State = maps:get(state, Req),
    case rpc_client:call(TokenReq) of
        {ok, Data} ->
            handle_dm_token_success(Decision, Data, Req);
        {error, {rpc_error, _Status, Body}} ->
            handle_token_rpc_error(UserId, ChannelId, Body, State);
        {error, Reason} ->
            logger:warning(
                "dm_voice_token_create_error: user_id=~p channel_id=~p reason=~p",
                [UserId, ChannelId, Reason]
            ),
            {reply, gateway_errors:error(voice_token_failed), State}
    end.

-spec handle_token_rpc_error(integer(), integer(), term(), dm_state()) ->
    {reply, {error, atom(), atom()}, dm_state()}.
handle_token_rpc_error(UserId, ChannelId, Body, State) ->
    logger:warning(
        "dm_voice_token_create_rpc_error: user_id=~p channel_id=~p body=~p",
        [UserId, ChannelId, Body]
    ),
    case guild_voice_unclaimed_account_utils:parse_unclaimed_error(Body) of
        true -> {reply, gateway_errors:error(voice_unclaimed_account), State};
        false -> {reply, gateway_errors:error(voice_token_failed), State}
    end.

-spec handle_dm_token_success(voice_p2p:join_decision(), map(), token_request()) ->
    {reply, map(), dm_state()} | {reply, {error, atom(), atom()}, dm_state()}.
handle_dm_token_success(Decision, Data, Req) ->
    case voice_p2p:admit(Decision, voice_p2p:grant(Data)) of
        {reject, ErrorAtom} ->
            {reply, gateway_errors:error(ErrorAtom), maps:get(state, Req)};
        {p2p, ConnectionId, IceServers, MaxParticipants} ->
            handle_dm_p2p_grant(ConnectionId, IceServers, MaxParticipants, Req);
        sfu ->
            handle_dm_sfu_token(Data, Req)
    end.

-spec handle_dm_p2p_grant(binary(), [map()], pos_integer(), token_request()) ->
    {reply, map(), dm_state()}.
handle_dm_p2p_grant(ConnectionId, IceServers, MaxParticipants, Req) ->
    UserId = maps:get(user_id, Req),
    ChannelId = maps:get(channel_id, Req),
    SessionId = maps:get(session_id, Req),
    State = maps:get(state, Req),
    EffE2EE = maps:get(e2ee_capable, Req) andalso guild_voice_e2ee:is_e2ee_enabled_for_dm(),
    VoiceState = build_voice_state(Req, ConnectionId, EffE2EE, true),
    NewState = store_and_broadcast(ConnectionId, ChannelId, VoiceState, State),
    SessionPid = maps:get(session_pid, State),
    dispatch_to_session(
        SessionPid,
        voice_server_update,
        voice_p2p:voice_server_update(ConnectionId, ChannelId, null, IceServers),
        null
    ),
    spawn_join_call(ChannelId, UserId, VoiceState, SessionId, SessionPid, MaxParticipants),
    {reply, #{success => true, needs_token => false, connection_id => ConnectionId}, NewState}.

-spec handle_dm_sfu_token(map(), token_request()) -> {reply, map(), dm_state()}.
handle_dm_sfu_token(Data, Req) ->
    UserId = maps:get(user_id, Req),
    ChannelId = maps:get(channel_id, Req),
    SessionId = maps:get(session_id, Req),
    E2EECapable = maps:get(e2ee_capable, Req),
    State = maps:get(state, Req),
    Token = maps:get(<<"token">>, Data),
    Endpoint = maps:get(<<"endpoint">>, Data),
    ConnectionId = maps:get(<<"connectionId">>, Data),
    EffE2EE = E2EECapable andalso guild_voice_e2ee:is_e2ee_enabled_for_dm(),
    VoiceState0 = build_voice_state(Req, ConnectionId, EffE2EE, false),
    VoiceState = dm_voice_state:maybe_attach_voice_routing_metadata(
        VoiceState0,
        maps:get(<<"regionId">>, Data, undefined),
        maps:get(<<"serverId">>, Data, undefined)
    ),
    NewState0 = store_and_broadcast(ConnectionId, ChannelId, VoiceState, State),
    {NewState, E2EEKey} = maybe_get_e2ee_key(EffE2EE, ChannelId, NewState0),
    VSUpdate = build_voice_server_update(Token, Endpoint, ChannelId, ConnectionId, E2EEKey),
    SessionPid = maps:get(session_pid, State),
    dispatch_to_session(SessionPid, voice_server_update, VSUpdate, null),
    spawn_join_call(ChannelId, UserId, VoiceState, SessionId, SessionPid, undefined),
    {reply, #{success => true, needs_token => false, connection_id => ConnectionId}, NewState}.

-spec build_voice_state(token_request(), binary(), boolean(), boolean()) -> voice_state().
build_voice_state(Req, ConnectionId, EffE2EE, P2p) ->
    voice_state_utils:complete_voice_state(#{
        <<"guild_id">> => null,
        <<"user_id">> => integer_to_binary(maps:get(user_id, Req)),
        <<"channel_id">> => integer_to_binary(maps:get(channel_id, Req)),
        <<"connection_id">> => ConnectionId,
        <<"is_mobile">> => maps:get(is_mobile, Req),
        <<"session_id">> => maps:get(session_id, Req),
        <<"mute">> => false,
        <<"deaf">> => false,
        <<"self_mute">> => maps:get(self_mute, Req),
        <<"self_deaf">> => maps:get(self_deaf, Req),
        <<"self_video">> => maps:get(self_video, Req),
        <<"self_stream">> => maps:get(self_stream, Req),
        <<"suppress">> => false,
        <<"viewer_stream_keys">> => maps:get(viewer_stream_keys, Req),
        <<"e2ee_capable">> => EffE2EE,
        <<"p2p">> => P2p
    }).

-spec store_and_broadcast(binary(), integer(), voice_state(), dm_state()) -> dm_state().
store_and_broadcast(ConnectionId, ChannelId, VoiceState, State) ->
    VoiceStates = maps:get(dm_voice_states, State, #{}),
    NewVoiceStates = VoiceStates#{ConnectionId => VoiceState},
    NewState = State#{dm_voice_states => NewVoiceStates},
    _ = voice_state_counts_cache:upsert_voice_state(VoiceState),
    dm_voice_ring:broadcast_voice_state_update(ChannelId, VoiceState, NewState),
    NewState.

-spec maybe_get_e2ee_key(boolean(), integer(), dm_state()) ->
    {dm_state(), binary() | undefined}.
maybe_get_e2ee_key(true, ChannelId, State) ->
    {RoomKey, NextState} = guild_voice_e2ee:get_or_create_room_key_dm(ChannelId, State),
    {NextState, RoomKey};
maybe_get_e2ee_key(false, _ChannelId, State) ->
    {State, undefined}.

-spec build_voice_server_update(
    binary(), binary(), integer(), binary(), binary() | undefined
) -> map().
build_voice_server_update(Token, Endpoint, ChannelId, ConnectionId, undefined) ->
    #{
        <<"token">> => Token,
        <<"endpoint">> => Endpoint,
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"connection_id">> => ConnectionId
    };
build_voice_server_update(Token, Endpoint, ChannelId, ConnectionId, E2EEKey) ->
    #{
        <<"token">> => Token,
        <<"endpoint">> => Endpoint,
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"connection_id">> => ConnectionId,
        <<"e2ee_key">> => E2EEKey
    }.

-spec dispatch_to_session(pid(), atom(), term(), integer() | null) -> ok.
dispatch_to_session(SessionPid, Event, Payload, GuildId) when
    is_integer(GuildId), GuildId > 0
->
    case code:ensure_loaded(gateway_dispatch_relay) of
        {module, gateway_dispatch_relay} ->
            dispatch_to_session_with_relay(SessionPid, Event, Payload, GuildId);
        _ ->
            gen_server:cast(SessionPid, {dispatch, Event, Payload})
    end;
dispatch_to_session(SessionPid, Event, Payload, _GuildId) ->
    gen_server:cast(SessionPid, {dispatch, Event, Payload}).

-spec dispatch_to_session_with_relay(pid(), atom(), term(), integer()) -> ok.
dispatch_to_session_with_relay(SessionPid, Event, Payload, GuildId) ->
    try gateway_dispatch_relay:dispatch(SessionPid, Event, Payload, GuildId) of
        ok -> ok
    catch
        error:undef -> gen_server:cast(SessionPid, {dispatch, Event, Payload})
    end.

-spec spawn_join_call(
    integer(), integer(), voice_state(), binary(), pid(), pos_integer() | undefined
) -> ok.
spawn_join_call(ChannelId, UserId, VoiceState, SessionId, SessionPid, MaxParticipants) ->
    spawn(fun() ->
        safe_join_or_create_call(
            ChannelId, UserId, VoiceState, SessionId, SessionPid, MaxParticipants
        )
    end),
    ok.

-spec safe_join_or_create_call(
    integer(), integer(), voice_state(), binary(), pid(), pos_integer() | undefined
) -> ok.
safe_join_or_create_call(ChannelId, UserId, VoiceState, SessionId, SessionPid, MaxParticipants) ->
    _ = shard_utils:safe_apply(
        fun() ->
            join_or_create_call(
                ChannelId, UserId, VoiceState, SessionId, SessionPid, MaxParticipants
            )
        end,
        ok
    ),
    ok.

-spec maybe_spawn_join_call(
    boolean(), integer(), integer(), voice_state(), binary() | undefined, dm_state()
) -> ok.
maybe_spawn_join_call(false, _ChId, _UserId, _VS, _SessId, _State) ->
    ok;
maybe_spawn_join_call(true, ChannelId, UserId, VoiceState, SessionId, State) when
    is_binary(SessionId)
->
    case maps:get(session_pid, State, undefined) of
        Pid when is_pid(Pid) ->
            spawn_join_call(ChannelId, UserId, VoiceState, SessionId, Pid, undefined);
        _ ->
            ok
    end;
maybe_spawn_join_call(true, _ChId, _UserId, _VS, _SessId, _State) ->
    ok.

-spec get_voice_token(integer(), integer(), binary(), pid(), term(), term()) -> ok | error.
get_voice_token(ChannelId, UserId, _SessionId, SessionPid, Latitude, Longitude) ->
    Req = voice_utils:build_voice_token_rpc_request(
        null, ChannelId, UserId, null, Latitude, Longitude
    ),
    Region = dm_voice_state:resolve_call_region(ChannelId),
    ReqWithRegion = voice_utils:add_rtc_region_to_request(Req, Region),
    case rpc_client:call(ReqWithRegion) of
        {ok, Data} ->
            handle_get_voice_token_ok(Data, UserId, ChannelId, SessionPid);
        {error, {rpc_error, _Status, Body}} ->
            handle_get_token_rpc_error(UserId, ChannelId, Body, SessionPid);
        {error, Reason} ->
            logger:warning(
                "dm_voice_get_voice_token_error: user_id=~p channel_id=~p reason=~p",
                [UserId, ChannelId, Reason]
            ),
            SessionPid ! {voice_error, voice_token_failed},
            error
    end.

-spec handle_get_token_rpc_error(integer(), integer(), term(), pid()) -> error.
handle_get_token_rpc_error(UserId, ChannelId, Body, SessionPid) ->
    logger:warning(
        "dm_voice_get_voice_token_rpc_error: user_id=~p channel_id=~p body=~p",
        [UserId, ChannelId, Body]
    ),
    _ =
        case guild_voice_unclaimed_account_utils:parse_unclaimed_error(Body) of
            true -> _ = SessionPid ! {voice_error, voice_unclaimed_account};
            false -> _ = SessionPid ! {voice_error, voice_token_failed}
        end,
    error.

-spec handle_get_voice_token_ok(map(), integer(), integer(), pid()) -> ok.
handle_get_voice_token_ok(Data, UserId, ChannelId, SessionPid) ->
    Token = maps:get(<<"token">>, Data),
    Endpoint = maps:get(<<"endpoint">>, Data),
    ConnectionId = maps:get(<<"connectionId">>, Data),
    logger:debug(
        "dm_voice_get_voice_token_ok: user_id=~p channel_id=~p connection_id=~p endpoint=~p",
        [UserId, ChannelId, ConnectionId, Endpoint]
    ),
    SessionPid !
        {voice_server_update, #{
            channel_id => integer_to_binary(ChannelId),
            endpoint => Endpoint,
            token => Token,
            connection_id => ConnectionId
        }},
    ok.

-spec join_or_create_call(
    integer(), integer(), voice_state(), binary(), pid(), pos_integer() | undefined
) -> ok.
join_or_create_call(ChannelId, UserId, VoiceState, SessionId, SessionPid, MaxParticipants) ->
    join_or_create_call(
        ChannelId, UserId, VoiceState, SessionId, SessionPid, MaxParticipants, 10
    ).

-spec join_or_create_call(
    integer(),
    integer(),
    voice_state(),
    binary(),
    pid(),
    pos_integer() | undefined,
    non_neg_integer()
) -> ok.
join_or_create_call(ChannelId, UserId, VoiceState, _SessId, SessionPid, _MaxParticipants, 0) ->
    logger:warning(
        "dm_voice_join_or_create_call_exhausted_retries: user_id=~p channel_id=~p",
        [UserId, ChannelId]
    ),
    rollback_ghost_voice_state(ChannelId, VoiceState, SessionPid),
    ok;
join_or_create_call(
    ChannelId, UserId, VoiceState, SessionId, SessionPid, MaxParticipants, Retries
) ->
    ConnectionId = maps:get(<<"connection_id">>, VoiceState, undefined),
    case call_manager:lookup(ChannelId) of
        {ok, CallPid} ->
            do_join_call(
                CallPid,
                ChannelId,
                UserId,
                VoiceState,
                SessionId,
                SessionPid,
                ConnectionId,
                MaxParticipants,
                Retries
            );
        _ ->
            retry_join(
                ChannelId, UserId, VoiceState, SessionId, SessionPid, MaxParticipants, Retries
            )
    end.

-spec do_join_call(
    pid(),
    integer(),
    integer(),
    voice_state(),
    binary(),
    pid(),
    term(),
    pos_integer() | undefined,
    non_neg_integer()
) -> ok.
do_join_call(
    CallPid,
    ChannelId,
    UserId,
    VoiceState,
    SessionId,
    SessionPid,
    ConnectionId,
    MaxParticipants,
    Retries
) ->
    JoinMsg =
        case {ConnectionId, MaxParticipants} of
            {undefined, _} ->
                {join, UserId, VoiceState, SessionId, SessionPid};
            {_, undefined} ->
                {join, UserId, VoiceState, SessionId, SessionPid, ConnectionId};
            _ ->
                {join, UserId, VoiceState, SessionId, SessionPid, ConnectionId, MaxParticipants}
        end,
    case gateway_rpc_call_lookup:safe_gen_server_call(CallPid, JoinMsg, 5000) of
        {ok, ok} ->
            gen_server:cast(SessionPid, {call_monitor, ChannelId, CallPid}),
            logger:debug(
                "dm_voice_join_or_create_call_ok: user_id=~p channel_id=~p retries_left=~p",
                [UserId, ChannelId, Retries]
            ),
            ok;
        {ok, {error, ErrorAtom}} ->
            gen_server:cast(SessionPid, {voice_rejected, ErrorAtom}),
            rollback_ghost_voice_state(ChannelId, VoiceState, SessionPid);
        Error ->
            logger:warning(
                dm_join_call_failed_log_message(),
                [UserId, ChannelId, Retries, Error]
            ),
            retry_join(
                ChannelId, UserId, VoiceState, SessionId, SessionPid, MaxParticipants, Retries
            )
    end.

-spec dm_join_call_failed_log_message() -> string().
dm_join_call_failed_log_message() ->
    "dm_voice_join_or_create_call_failed: user_id=~p channel_id=~p "
    "retries_left=~p error=~p".

-spec retry_join(
    integer(),
    integer(),
    voice_state(),
    binary(),
    pid(),
    pos_integer() | undefined,
    non_neg_integer()
) -> ok.
retry_join(ChannelId, UserId, VoiceState, SessionId, SessionPid, MaxParticipants, Retries) ->
    logger:warning(
        "dm_voice_join_or_create_call_lookup_not_found: user_id=~p channel_id=~p retries_left=~p",
        [UserId, ChannelId, Retries]
    ),
    ok = gateway_retry_timer:wait(300),
    join_or_create_call(
        ChannelId, UserId, VoiceState, SessionId, SessionPid, MaxParticipants, Retries - 1
    ).

-spec rollback_ghost_voice_state(integer(), voice_state(), pid()) -> ok.
rollback_ghost_voice_state(ChannelId, VoiceState, SessionPid) ->
    ConnectionId = maps:get(<<"connection_id">>, VoiceState, undefined),
    _ = voice_state_counts_cache:remove_connection(ConnectionId),
    cast_force_disconnect(ChannelId, ConnectionId, SessionPid),
    ok.

-spec cast_force_disconnect(integer(), binary() | undefined, pid()) -> ok.
cast_force_disconnect(ChannelId, ConnectionId, SessionPid) when
    is_pid(SessionPid), is_binary(ConnectionId)
->
    gen_server:cast(SessionPid, {call_force_disconnect, ChannelId, ConnectionId});
cast_force_disconnect(ChannelId, _ConnectionId, SessionPid) when is_pid(SessionPid) ->
    gen_server:cast(SessionPid, {call_force_disconnect, ChannelId, undefined});
cast_force_disconnect(_ChannelId, _ConnectionId, _SessionPid) ->
    ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

do_join_call_dead_pid_exhausts_without_crash_test() ->
    DeadPid = spawn(fun() -> ok end),
    Ref = monitor(process, DeadPid),
    receive
        {'DOWN', Ref, process, DeadPid, _Reason} -> ok
    after 1000 ->
        ?assert(false, call_pid_did_not_exit)
    end,
    VoiceState = #{<<"connection_id">> => <<"connection">>},
    ?assertEqual(
        ok,
        do_join_call(
            DeadPid, 1234, 42, VoiceState, <<"session">>, self(), <<"connection">>, undefined, 1
        )
    ),
    receive
        {'$gen_cast', {call_monitor, 1234, DeadPid}} ->
            ?assert(false, stale_call_monitor_cast)
    after 50 ->
        ok
    end.

exhausted_retries_casts_force_disconnect_test() ->
    VoiceState = #{<<"connection_id">> => <<"conn-x">>},
    ?assertEqual(
        ok,
        join_or_create_call(1234, 42, VoiceState, <<"session">>, self(), undefined, 0)
    ),
    receive
        {'$gen_cast', {call_force_disconnect, 1234, <<"conn-x">>}} ->
            ok
    after 100 ->
        ?assert(false, missing_force_disconnect_cast)
    end.

exhausted_retries_without_connection_id_casts_undefined_test() ->
    ?assertEqual(
        ok,
        join_or_create_call(1234, 42, #{}, <<"session">>, self(), undefined, 0)
    ),
    receive
        {'$gen_cast', {call_force_disconnect, 1234, undefined}} ->
            ok
    after 100 ->
        ?assert(false, missing_force_disconnect_cast)
    end.

-endif.
