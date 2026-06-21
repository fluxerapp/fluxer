%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_manager_shard_drain).
-typing([eqwalizer]).

-export([
    broadcast_reconnect_drain/1,
    broadcast_transfer_to/2,
    broadcast_transfer_to_topology/2,
    handoff_to_topology/2,
    build_session_data/7,
    build_ready_data_for_session/1,
    filter_guild_ids_for_identify/2,
    validate_identify_sharding/3,
    check_identify_rate_limit/1,
    should_debounce_reactions/1,
    extract_e2ee_capable/1,
    fetch_rpc_data/2
]).

-export_type([handoff_result/0, identify_data/0, session_id/0, shard_identify_data/0, state/0]).

-define(IDENTIFY_FLAG_DEBOUNCE_MESSAGE_REACTIONS, 16#2).
-define(SESSION_RPC_RETRY_CONFIG, {1, 1000, 10000, 500}).

-type session_id() :: binary().
-type session_ref() :: {pid(), reference()}.
-type identify_data() :: #{
    token := binary(),
    properties := map(),
    shard => gateway_sharding:shard() | undefined,
    term() => term()
}.
-type shard_identify_data() :: #{
    shard => gateway_sharding:shard() | undefined, term() => term()
}.
-type state() :: #{sessions := #{session_id() => session_ref()}, _ => _}.
-type handoff_result() :: #{
    attempted := non_neg_integer(),
    handed_off := non_neg_integer()
}.

-spec broadcast_reconnect_drain(state()) -> non_neg_integer().
broadcast_reconnect_drain(State) ->
    Sessions = maps:get(sessions, State, #{}),
    maps:fold(fun maybe_drain_session/3, 0, Sessions).

-spec maybe_drain_session(session_id(), term(), non_neg_integer()) -> non_neg_integer().
maybe_drain_session(_SessionId, {Pid, _Ref}, Count) when is_pid(Pid) ->
    case process_liveness:is_alive(Pid) of
        true ->
            gen_server:cast(Pid, reconnect_drain),
            Count + 1;
        false ->
            Count
    end;
maybe_drain_session(_SessionId, _SessionRef, Count) ->
    Count.

-spec broadcast_transfer_to(node(), state()) -> non_neg_integer().
broadcast_transfer_to(TargetNode, State) ->
    Sessions = maps:get(sessions, State, #{}),
    maps:fold(
        fun(SId, SRef, Cnt) ->
            maybe_transfer(TargetNode, SId, SRef, Cnt)
        end,
        0,
        Sessions
    ).

-spec maybe_transfer(node(), session_id(), term(), non_neg_integer()) -> non_neg_integer().
maybe_transfer(TargetNode, SessionId, {Pid, _Ref}, Count) when is_pid(Pid) ->
    case process_liveness:is_alive(Pid) of
        true -> transfer_count(TargetNode, SessionId, Pid, Count);
        false -> Count
    end;
maybe_transfer(_, _, _, Count) ->
    Count.

-spec transfer_count(node(), session_id(), pid(), non_neg_integer()) -> non_neg_integer().
transfer_count(TargetNode, SessionId, Pid, Count) ->
    case push_and_drain_session(TargetNode, SessionId, Pid) of
        ok -> Count + 1;
        {error, _Reason} -> Count
    end.

-spec push_and_drain_session(node(), session_id(), pid()) -> ok | {error, term()}.
push_and_drain_session(TargetNode, SessionId, Pid) ->
    ExportResult =
        try
            gen_server:call(Pid, export_state, 2000)
        catch
            error:Reason -> {error, {error, Reason}};
            exit:Reason -> {error, {exit, Reason}}
        end,
    case ExportResult of
        {ok, SessionState} ->
            do_push_and_drain(TargetNode, SessionId, SessionState, Pid);
        Other ->
            {error, {export_failed, Other}}
    end.

-spec do_push_and_drain(node(), session_id(), map(), pid()) -> ok | {error, term()}.
do_push_and_drain(TargetNode, SessionId, SessionState, Pid) ->
    case session_state_transfer:push_state(TargetNode, SessionId, SessionState) of
        ok ->
            gen_server:cast(Pid, handoff_fence),
            ok;
        {error, Reason} = Error ->
            logger:warning(
                "session_state_push_failed: session_id=~ts target_node=~p reason=~p",
                [SessionId, TargetNode, Reason]
            ),
            Error
    end.

-spec broadcast_transfer_to_topology([node()], state()) -> non_neg_integer().
broadcast_transfer_to_topology(TargetNodes, State) ->
    maps:get(handed_off, handoff_to_topology(TargetNodes, State)).

-spec handoff_to_topology([node()], state()) -> handoff_result().
handoff_to_topology(TargetNodes, State) ->
    Sessions = maps:get(sessions, State, #{}),
    maps:fold(
        fun
            (SessionId, {Pid, _Ref}, Result) when is_pid(Pid) ->
                handoff_if_remote(SessionId, Pid, TargetNodes, Result);
            (_SessionId, _SessionRef, Result) ->
                Result
        end,
        empty_handoff_result(),
        Sessions
    ).

-spec empty_handoff_result() -> handoff_result().
empty_handoff_result() ->
    #{attempted => 0, handed_off => 0}.

-spec handoff_if_remote(session_id(), pid(), [node()], handoff_result()) -> handoff_result().
handoff_if_remote(SessionId, Pid, TargetNodes, Result) ->
    case process_liveness:is_alive(Pid) of
        false -> Result;
        true -> maybe_handoff_to_remote(SessionId, Pid, TargetNodes, Result)
    end.

-spec maybe_handoff_to_remote(session_id(), pid(), [node()], handoff_result()) ->
    handoff_result().
maybe_handoff_to_remote(SessionId, Pid, TargetNodes, Result) ->
    TargetNode = gateway_node_router:select_owner_node(SessionId, TargetNodes),
    case TargetNode =:= node() of
        true -> Result;
        false -> handoff_count(TargetNode, SessionId, Pid, Result)
    end.

-spec handoff_count(node(), session_id(), pid(), handoff_result()) -> handoff_result().
handoff_count(TargetNode, SessionId, Pid, #{attempted := Attempted, handed_off := HandedOff}) ->
    case push_and_drain_session(TargetNode, SessionId, Pid) of
        ok -> #{attempted => Attempted + 1, handed_off => HandedOff + 1};
        {error, _Reason} -> #{attempted => Attempted + 1, handed_off => HandedOff}
    end.

-spec build_session_data(
    map(),
    identify_data(),
    non_neg_integer(),
    pid(),
    session_id(),
    map(),
    integer()
) -> map().
build_session_data(Data, IdentifyData, Version, SocketPid, SessionId, UserDataMap, UserId) ->
    Properties = maps:get(properties, IdentifyData),
    Presence = map_utils:get_safe(IdentifyData, presence, null),
    IgnoredEvents = map_utils:get_safe(IdentifyData, ignored_events, []),
    Shard = maps:get(shard, IdentifyData, undefined),
    DetachedCustomStatus = detach_custom_status(resolve_custom_status(Data, Presence)),
    DetachedActivities = detach_activities(resolve_activities(Presence)),
    BaseFields = #{
        id => SessionId,
        user_id => UserId,
        version => Version,
        user_data => term_detach:detach(build_user_data(UserDataMap)),
        custom_status => DetachedCustomStatus,
        activities => DetachedActivities,
        token_hash => utils:hash_token(maps:get(token, IdentifyData)),
        auth_session_id_hash => extract_auth_session_id_hash(Data),
        properties => term_detach:detach(Properties),
        status => session_manager_shard_lookup:parse_presence(Data, IdentifyData),
        afk => extract_afk(Presence),
        mobile => extract_mobile(Presence, Properties),
        socket_pid => SocketPid,
        guilds => filter_guild_ids_for_identify(Data, IdentifyData),
        ready => build_ready_data_for_session(Data),
        bot => map_utils:get_safe(UserDataMap, <<"bot">>, false),
        e2ee_capable => extract_e2ee_capable(Properties),
        ignored_events => term_detach:detach(IgnoredEvents),
        initial_guild_id => map_utils:get_safe(IdentifyData, initial_guild_id, undefined),
        shard => Shard,
        debounce_reactions => should_debounce_reactions(IdentifyData)
    },
    BaseFields.

-spec filter_guild_ids_for_identify(map(), shard_identify_data()) -> [integer()].
filter_guild_ids_for_identify(Data, IdentifyData) ->
    GuildIds = session_manager_shard_lookup:parse_guild_ids(Data),
    Shard = maps:get(shard, IdentifyData, undefined),
    gateway_sharding:retain_guild_ids_for_shard(GuildIds, Shard).

-spec validate_identify_sharding(map(), shard_identify_data(), boolean()) ->
    ok | {error, sharding_required}.
validate_identify_sharding(_Data, _IdentifyData, false) ->
    ok;
validate_identify_sharding(Data, IdentifyData, true) ->
    GuildIds = session_manager_shard_lookup:parse_guild_ids(Data),
    Shard = maps:get(shard, IdentifyData, undefined),
    gateway_sharding:validate_session_guild_count(GuildIds, Shard).

-spec extract_auth_session_id_hash(map()) -> binary().
extract_auth_session_id_hash(Data) ->
    case maps:get(<<"auth_session_id_hash">>, Data, undefined) of
        undefined -> <<>>;
        null -> <<>>;
        Encoded -> base64url:decode(Encoded)
    end.

-spec build_user_data(map()) -> map().
build_user_data(UserDataMap) ->
    UserData0 = #{
        <<"id">> => maps:get(<<"id">>, UserDataMap),
        <<"username">> => maps:get(<<"username">>, UserDataMap),
        <<"discriminator">> => maps:get(<<"discriminator">>, UserDataMap),
        <<"global_name">> => maps:get(<<"global_name">>, UserDataMap, null),
        <<"avatar">> => maps:get(<<"avatar">>, UserDataMap),
        <<"avatar_color">> => map_utils:get_safe(UserDataMap, <<"avatar_color">>, undefined),
        <<"bot">> => map_utils:get_safe(UserDataMap, <<"bot">>, undefined),
        <<"system">> => map_utils:get_safe(UserDataMap, <<"system">>, undefined),
        <<"flags">> => maps:get(<<"flags">>, UserDataMap),
        <<"mention_flags">> => map_utils:get_safe(UserDataMap, <<"mention_flags">>, undefined)
    },
    Normalized = user_utils:normalize_user(UserData0),
    Normalized#{<<"is_staff">> => maps:get(<<"is_staff">>, UserDataMap, false)}.

-spec detach_custom_status(map() | null) -> map() | null.
detach_custom_status(null) ->
    null;
detach_custom_status(CustomStatus) when is_map(CustomStatus) ->
    Detached = term_detach:detach(CustomStatus),
    case Detached of
        M when is_map(M) -> M;
        _ -> null
    end.

-spec detach_activities([map()] | null) -> [map()] | null.
detach_activities(null) ->
    null;
detach_activities(Activities) when is_list(Activities) ->
    Detached = term_detach:detach(Activities),
    case Detached of
        List when is_list(List) -> [A || A <- List, is_map(A)];
        _ -> null
    end;
detach_activities(_) ->
    null.

-spec resolve_custom_status(map(), term()) -> map() | null.
resolve_custom_status(Data, Presence) ->
    UserSettingsMap = map_utils:get_safe(Data, <<"user_settings">>, #{}),
    case map_utils:get_safe(UserSettingsMap, <<"custom_status">>, null) of
        null -> session_manager_shard_lookup:get_presence_custom_status(Presence);
        CustomStatus when is_map(CustomStatus) -> CustomStatus;
        _ -> null
    end.

-spec resolve_activities(term()) -> [map()] | null.
resolve_activities(Presence) when is_map(Presence) ->
    case map_utils:get_safe(Presence, <<"activities">>, null) of
        List when is_list(List) -> [A || A <- List, is_map(A)];
        _ -> null
    end;
resolve_activities(_) ->
    null.

-spec extract_mobile(term(), map()) -> boolean().
extract_mobile(null, Properties) ->
    is_truthy(map_utils:get_safe(Properties, <<"mobile">>, false));
extract_mobile(P, _Properties) when is_map(P) ->
    is_truthy(map_utils:get_safe(P, <<"mobile">>, false));
extract_mobile(_, _Properties) ->
    false.

-spec extract_afk(term()) -> boolean().
extract_afk(null) ->
    false;
extract_afk(P) when is_map(P) ->
    is_truthy(map_utils:get_safe(P, <<"afk">>, false));
extract_afk(_) ->
    false.

-spec is_truthy(term()) -> boolean().
is_truthy(true) -> true;
is_truthy(_) -> false.

-spec build_ready_data_for_session(map()) -> map().
build_ready_data_for_session(Data) ->
    Detached = term_detach:detach(Data#{<<"guilds">> => []}),
    case Detached of
        M when is_map(M) -> M;
        _ -> #{}
    end.

-spec check_identify_rate_limit(list()) -> {ok, list()} | {error, rate_limited}.
check_identify_rate_limit(Attempts) ->
    case fluxer_gateway_env:get(identify_rate_limit_enabled) of
        true -> do_check_rate_limit(Attempts);
        _ -> {ok, Attempts}
    end.

-spec do_check_rate_limit(list()) -> {ok, list()} | {error, rate_limited}.
do_check_rate_limit(Attempts) ->
    Now = erlang:system_time(millisecond),
    AttemptsInWindow = [T || T <- Attempts, (Now - T) < 5000],
    case AttemptsInWindow =/= [] of
        true -> {error, rate_limited};
        false -> {ok, [Now | AttemptsInWindow]}
    end.

-spec should_debounce_reactions(map()) -> boolean().
should_debounce_reactions(IdentifyData) ->
    case map_utils:get_safe(IdentifyData, flags, 0) of
        Flags when is_integer(Flags), Flags >= 0 ->
            bitset:has(Flags, ?IDENTIFY_FLAG_DEBOUNCE_MESSAGE_REACTIONS);
        _ ->
            false
    end.

-spec extract_e2ee_capable(term()) -> boolean().
extract_e2ee_capable(Properties) when is_map(Properties) ->
    is_truthy(maps:get(<<"e2ee_capable">>, Properties, false));
extract_e2ee_capable(_) ->
    false.

-spec fetch_rpc_data(map(), term()) -> {ok, map()} | {error, term()}.
fetch_rpc_data(Request, PeerIP) ->
    StartTime = erlang:system_time(millisecond),
    Result = do_fetch_rpc_data(Request, PeerIP),
    LatencyMs = erlang:system_time(millisecond) - StartTime,
    log_rpc_result(Result, LatencyMs, PeerIP),
    Result.

-spec log_rpc_result(term(), integer(), term()) -> ok.
log_rpc_result({ok, _}, LatencyMs, PeerIP) ->
    logger:info("Session RPC succeeded", #{latency_ms => LatencyMs, peer_ip => PeerIP});
log_rpc_result({error, ErrorReason}, LatencyMs, PeerIP) ->
    logger:warning("Session RPC failed", #{
        latency_ms => LatencyMs, peer_ip => PeerIP, reason => ErrorReason
    }).

-spec do_fetch_rpc_data(map(), term()) -> {ok, map()} | {error, term()}.
do_fetch_rpc_data(Request, PeerIP) ->
    IdentifyData = maps:get(identify_data, Request),
    Properties = map_utils:get_safe(IdentifyData, properties, #{}),
    Lat = session_manager_shard_lookup:normalize_coordinate(
        map_utils:get_safe(Properties, <<"latitude">>, undefined)
    ),
    Lon = session_manager_shard_lookup:normalize_coordinate(
        map_utils:get_safe(Properties, <<"longitude">>, undefined)
    ),
    RpcRequest = #{
        <<"type">> => <<"session">>,
        <<"token">> => maps:get(token, IdentifyData),
        <<"version">> => maps:get(version, Request),
        <<"ip">> => PeerIP
    },
    RpcWithCoords = session_manager_shard_lookup:add_coordinates(RpcRequest, Lat, Lon),
    RpcResult = api_rpc_client:call_with_retry(RpcWithCoords, ?SESSION_RPC_RETRY_CONFIG),
    classify_rpc_result(RpcResult, PeerIP).

-spec classify_rpc_result(term(), term()) -> {ok, map()} | {error, term()}.
classify_rpc_result({ok, Data}, _PeerIP) when is_map(Data) -> {ok, Data};
classify_rpc_result({error, {rpc_error, 401, _}}, _PeerIP) ->
    {error, invalid_token};
classify_rpc_result({error, {rpc_error, 429, _}}, _PeerIP) ->
    {error, rate_limited};
classify_rpc_result({error, {retries_exhausted, {rpc_error, SC, _} = E}}, PeerIP) when
    SC >= 500
->
    logger:error("Session RPC retries exhausted (server error)", #{
        status => SC, peer_ip => PeerIP, last_error => E
    }),
    {error, {retries_exhausted, E}};
classify_rpc_result({error, {retries_exhausted, Reason}}, PeerIP) ->
    logger:error("Session RPC retries exhausted", #{peer_ip => PeerIP, last_error => Reason}),
    {error, {retries_exhausted, Reason}};
classify_rpc_result({error, Reason}, _PeerIP) ->
    {error, {network_error, Reason}}.
