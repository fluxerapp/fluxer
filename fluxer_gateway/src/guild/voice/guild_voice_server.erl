%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_server).
-typing([eqwalizer]).

-behaviour(gen_server).

-export([
    start_link/3,
    is_voice_server_pid/1,
    resolve/2,
    resolve_result/2,
    lookup/1,
    lookup_registered/1
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export_type([
    voice_state_map/0,
    server_state/0
]).

-define(REGISTRY_TABLE, guild_voice_registry).
-define(SWEEP_INTERVAL_MS, 10000).
-define(MAX_PENDING_CONNECTIONS, 1000).
-define(SEEDED_SESSION_CHECK_DELAY_MS, 60000).
-define(SESSION_LOOKUP_TIMEOUT_MS, 5000).

-type voice_state_map() :: #{binary() => map()}.
-type server_state() :: #{
    guild_id := integer(),
    guild_pid := pid(),
    voice_states := voice_state_map(),
    pending_voice_connections := map(),
    recently_disconnected_voice_states := map(),
    e2ee_room_keys := map()
}.

-spec start_link(integer(), pid(), voice_state_map()) -> gen_server:start_ret().
start_link(GuildId, GuildPid, InitialVoiceStates) ->
    gen_server:start_link(
        ?MODULE,
        #{
            guild_id => GuildId,
            guild_pid => GuildPid,
            initial_voice_states => voice_state_utils:ensure_voice_states(InitialVoiceStates)
        },
        []
    ).

-spec lookup(integer()) -> {ok, pid()} | {error, not_found}.
lookup(GuildId) ->
    guild_voice_server_sync:ensure_registry(),
    case ets:lookup(?REGISTRY_TABLE, GuildId) of
        [{_, Pid}] when is_pid(Pid) ->
            handle_registered_lookup(GuildId, Pid);
        _ ->
            guild_voice_server_lookup:recover(GuildId)
    end.

-spec handle_registered_lookup(integer(), pid()) -> {ok, pid()} | {error, not_found}.
handle_registered_lookup(GuildId, Pid) ->
    case process_liveness:is_alive(Pid) of
        true ->
            {ok, Pid};
        false ->
            ets:delete(?REGISTRY_TABLE, GuildId),
            guild_voice_server_lookup:recover(GuildId)
    end.

-spec lookup_registered(integer()) -> {ok, pid()} | {error, not_found}.
lookup_registered(GuildId) ->
    guild_voice_server_lookup:registered(GuildId).

-spec resolve(integer() | undefined, pid()) -> pid().
resolve(undefined, FallbackPid) ->
    FallbackPid;
resolve(GuildId, FallbackPid) ->
    case resolve_result(GuildId, FallbackPid) of
        {ok, VoicePid} -> VoicePid;
        {error, not_found} -> FallbackPid
    end.

-spec resolve_result(integer() | undefined, pid()) -> {ok, pid()} | {error, not_found}.
resolve_result(undefined, FallbackPid) ->
    {ok, FallbackPid};
resolve_result(GuildId, FallbackPid) ->
    case lookup(GuildId) of
        {ok, VoicePid} ->
            {ok, VoicePid};
        {error, not_found} ->
            guild_voice_server_lookup:after_miss(GuildId, FallbackPid)
    end.

-spec is_voice_server_pid(pid()) -> boolean().
is_voice_server_pid(Pid) when is_pid(Pid) ->
    try gen_server:call(Pid, get_voice_server_pid, 500) of
        {ok, Pid} -> true;
        _ -> false
    catch
        throw:_ -> false;
        error:_ -> false;
        exit:_ -> false
    end.

-spec init(map()) -> {ok, server_state()}.
init(#{guild_id := GuildId, guild_pid := GuildPid} = Args) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 0),
    guild_voice_server_sync:ensure_registry(),
    ets:insert(?REGISTRY_TABLE, {GuildId, self()}),
    erlang:send_after(?SWEEP_INTERVAL_MS, self(), sweep_pending_joins),
    erlang:send_after(?SEEDED_SESSION_CHECK_DELAY_MS, self(), check_p2p_sessions),
    InitialVoiceStates = voice_state_utils:ensure_voice_states(
        maps:get(initial_voice_states, Args, #{})
    ),
    _ = guild_voice_server_sync:sync_voice_state_counts(InitialVoiceStates),
    erlang:garbage_collect(),
    State = #{
        guild_id => GuildId,
        guild_pid => GuildPid,
        voice_states => InitialVoiceStates,
        pending_voice_connections => #{},
        recently_disconnected_voice_states => #{},
        e2ee_room_keys => #{}
    },
    ok = schedule_seeded_session_check(first, maps:keys(InitialVoiceStates)),
    {ok, State}.

-spec handle_call(term(), gen_server:from(), server_state()) -> {reply, term(), server_state()}.
handle_call({VoiceRequest, Req} = Msg, _From, S) ->
    handle_voice_request_call(voice_request_handler(VoiceRequest), Req, Msg, S);
handle_call(Msg, _From, State) ->
    handle_call_local(Msg, State).

-spec handle_voice_request_call(
    {ok, fun((map(), map()) -> {reply, term(), map()})} | error,
    term(),
    term(),
    server_state()
) -> {reply, term(), server_state()}.
handle_voice_request_call({ok, Fun}, Req, _Msg, S) when is_map(Req) ->
    delegate_voice_call(Fun, Req, S);
handle_voice_request_call({ok, _Fun}, _Req, _Msg, S) ->
    {reply, gateway_errors:error(validation_expected_map), S};
handle_voice_request_call(error, _Req, Msg, S) ->
    handle_call_local(Msg, S).

-spec voice_request_handler(term()) ->
    {ok, fun((map(), map()) -> {reply, term(), map()})} | error.
voice_request_handler(voice_state_update) ->
    {ok, fun guild_voice_connection:voice_state_update/2};
voice_request_handler(get_voice_state) ->
    {ok, fun guild_voice_state:get_voice_state/2};
voice_request_handler(update_member_voice) ->
    {ok, fun guild_voice_member:update_member_voice/2};
voice_request_handler(disconnect_voice_user) ->
    {ok, fun guild_voice_disconnect_user:disconnect_voice_user/2};
voice_request_handler(disconnect_voice_user_if_in_channel) ->
    {ok, fun guild_voice_disconnect_user:disconnect_voice_user_if_in_channel/2};
voice_request_handler(disconnect_all_voice_users_in_channel) ->
    {ok, fun guild_voice_disconnect_channel:disconnect_all_voice_users_in_channel/2};
voice_request_handler(confirm_voice_connection_from_livekit) ->
    {ok, fun guild_voice_connection_confirm:confirm_voice_connection_from_livekit/2};
voice_request_handler(move_member) ->
    {ok, fun guild_voice_move:move_member/2};
voice_request_handler(switch_voice_region) ->
    {ok, fun guild_voice_region:switch_voice_region_handler/2};
voice_request_handler(_) ->
    error.

-spec handle_call_local(term(), server_state()) -> {reply, term(), server_state()}.
handle_call_local({store_pending_connection, ConnectionId, Metadata}, State) ->
    Pending = maps:get(pending_voice_connections, State, #{}),
    NewPending = guild_voice_server_caps:bounded_put(
        ConnectionId, Metadata, Pending, ?MAX_PENDING_CONNECTIONS
    ),
    {reply, ok, State#{pending_voice_connections => NewPending}};
handle_call_local({get_voice_states_for_channel, ChIdBin}, State) ->
    {reply, channel_query(voice_states, ChIdBin, State), State};
handle_call_local({get_voice_states_map}, State) ->
    {reply, maps:get(voice_states, State, #{}), State};
handle_call_local(get_voice_server_pid, State) ->
    {reply, {ok, self()}, State};
handle_call_local(_, State) ->
    {reply, ok, State}.

-spec channel_query(voice_states, term(), server_state()) -> map().
channel_query(voice_states, ChIdBin, State) ->
    case guild_voice_server_state:parse_voice_channel_id(ChIdBin) of
        {ok, _, Bin} -> guild_voice_server_state:local_voice_states_for_channel(Bin, State);
        _ -> #{voice_states => []}
    end.

-spec handle_cast(term(), server_state()) -> {noreply, server_state()}.
handle_cast({store_pending_connection, ConnId, Meta}, State) ->
    Pending = maps:get(pending_voice_connections, State, #{}),
    NewPending = guild_voice_server_caps:bounded_put(
        ConnId, Meta, Pending, ?MAX_PENDING_CONNECTIONS
    ),
    {noreply, State#{pending_voice_connections => NewPending}};
handle_cast({disconnect_voice_user, Request}, State) when is_map(Request) ->
    {noreply,
        delegate_voice_cast(
            fun guild_voice_disconnect_user:disconnect_voice_user/2, Request, State
        )};
handle_cast({disconnect_all_voice_users_in_channel, Request}, State) when is_map(Request) ->
    {noreply,
        delegate_voice_cast(
            fun guild_voice_disconnect_channel:disconnect_all_voice_users_in_channel/2,
            Request,
            State
        )};
handle_cast({cleanup_virtual_access_for_user, UserId}, State) when is_integer(UserId) ->
    GS = guild_voice_server_state:build_guild_state(State),
    NewGS = guild_voice_disconnect_user:cleanup_virtual_channel_access_for_user(UserId, GS),
    {noreply, guild_voice_server_state:apply_guild_state(NewGS, State)};
handle_cast(_, State) ->
    {noreply, State}.

-spec handle_info(term(), server_state()) ->
    {noreply, server_state()} | {stop, normal, server_state()}.
handle_info(sweep_pending_joins, State) ->
    erlang:send_after(?SWEEP_INTERVAL_MS, self(), sweep_pending_joins),
    Swept = guild_voice_server_caps:sweep(State),
    case maps:size(maps:get(pending_voice_connections, Swept, #{})) of
        0 ->
            {noreply, Swept};
        _ ->
            GuildState = guild_voice_server_state:build_guild_state(Swept),
            NewGuildState = guild_voice_connection_pending:sweep_expired_pending_joins(
                GuildState
            ),
            {noreply, guild_voice_server_state:apply_guild_state(NewGuildState, Swept)}
    end;
handle_info(check_p2p_sessions, State) ->
    erlang:send_after(?SEEDED_SESSION_CHECK_DELAY_MS, self(), check_p2p_sessions),
    ok = spawn_seeded_session_check(
        first, seeded_sessions(p2p_connection_ids(State), State), State
    ),
    {noreply, State};
handle_info({check_seeded_sessions, Round, ConnectionIds}, State) ->
    Sessions = seeded_sessions(eqwalizer:dynamic_cast(ConnectionIds), State),
    ok = spawn_seeded_session_check(eqwalizer:dynamic_cast(Round), Sessions, State),
    {noreply, State};
handle_info({seeded_sessions_gone, first, Gone}, State) ->
    ok = schedule_seeded_session_check(second, maps:keys(eqwalizer:dynamic_cast(Gone))),
    {noreply, State};
handle_info({seeded_sessions_gone, second, Gone}, State) ->
    {noreply, remove_gone_seeded_connections(eqwalizer:dynamic_cast(Gone), State)};
handle_info({'EXIT', Pid, Reason}, #{guild_pid := GuildPid} = State) when Pid =:= GuildPid ->
    logger:info(
        "Voice server shutting down because guild process exited",
        #{guild_id => maps:get(guild_id, State), reason => Reason}
    ),
    {stop, normal, State};
handle_info(_, State) ->
    {noreply, State}.

-spec terminate(term(), server_state()) -> ok.
terminate(_Reason, #{guild_id := GuildId} = State) ->
    _ = guild_voice_server_sync:cleanup_voice_state_counts(maps:get(voice_states, State, #{})),
    try ets:delete(?REGISTRY_TABLE, GuildId) of
        _ -> ok
    catch
        throw:_ -> ok;
        error:_ -> ok;
        exit:_ -> ok
    end;
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), server_state(), term()) -> {ok, server_state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:process_flag(fullsweep_after, 0),
    erlang:garbage_collect(),
    {ok, State}.

-spec schedule_seeded_session_check(first | second, [binary()]) -> ok.
schedule_seeded_session_check(_Round, []) ->
    ok;
schedule_seeded_session_check(Round, ConnectionIds) ->
    _ = erlang:send_after(
        ?SEEDED_SESSION_CHECK_DELAY_MS, self(), {check_seeded_sessions, Round, ConnectionIds}
    ),
    ok.

-spec p2p_connection_ids(server_state()) -> [binary()].
p2p_connection_ids(State) ->
    maps:keys(
        maps:filter(
            fun(_ConnectionId, VoiceState) -> voice_p2p:is_p2p(VoiceState) end,
            maps:get(voice_states, State, #{})
        )
    ).

-spec seeded_sessions([binary()], server_state()) -> #{binary() => {integer(), binary()}}.
seeded_sessions(ConnectionIds, State) ->
    VoiceStates = maps:get(voice_states, State, #{}),
    maps:from_list([
        {ConnectionId, {UserId, SessionId}}
     || ConnectionId <- ConnectionIds,
        VoiceState <- [maps:get(ConnectionId, VoiceStates, undefined)],
        is_map(VoiceState),
        UserId <- [voice_state_utils:voice_state_user_id(VoiceState)],
        is_integer(UserId),
        SessionId <- [
            guild_voice_connection_normalize:normalize_session_id(
                maps:get(<<"session_id">>, VoiceState, undefined)
            )
        ],
        is_binary(SessionId)
    ]).

-spec spawn_seeded_session_check(
    first | second, #{binary() => {integer(), binary()}}, server_state()
) -> ok.
spawn_seeded_session_check(_Round, Seeded, _State) when map_size(Seeded) =:= 0 ->
    ok;
spawn_seeded_session_check(Round, Seeded, State) ->
    Self = self(),
    IsGone = session_gone_fun(State),
    _ = spawn(fun() -> Self ! {seeded_sessions_gone, Round, gone_seeded(Seeded, IsGone)} end),
    ok.

-spec gone_seeded(#{binary() => {integer(), binary()}}, fun((binary()) -> boolean())) ->
    #{binary() => {integer(), binary()}}.
gone_seeded(Seeded, IsGone) ->
    SessionIds = lists:usort([SessionId || {_UserId, SessionId} <- maps:values(Seeded)]),
    GoneSessions = [SessionId || SessionId <- SessionIds, IsGone(SessionId)],
    maps:filter(
        fun(_ConnectionId, {_UserId, SessionId}) -> lists:member(SessionId, GoneSessions) end,
        Seeded
    ).

-spec session_gone_fun(server_state()) -> fun((binary()) -> boolean()).
session_gone_fun(State) ->
    case maps:get(test_session_gone_fun, State, undefined) of
        Fun when is_function(Fun, 1) -> Fun;
        _ -> fun session_gone/1
    end.

-spec session_gone(binary()) -> boolean().
session_gone(SessionId) ->
    try
        session_manager_routing:call_owner_manager(
            SessionId, {lookup, SessionId}, ?SESSION_LOOKUP_TIMEOUT_MS
        )
    of
        {error, not_found} -> true;
        _ -> false
    catch
        _:_ -> false
    end.

-spec remove_gone_seeded_connections(#{binary() => {integer(), binary()}}, server_state()) ->
    server_state().
remove_gone_seeded_connections(Gone, State) ->
    maps:fold(
        fun(ConnectionId, {UserId, SessionId}, AccState) ->
            remove_gone_seeded_connection(ConnectionId, UserId, SessionId, AccState)
        end,
        State,
        Gone
    ).

-spec remove_gone_seeded_connection(binary(), integer(), binary(), server_state()) ->
    server_state().
remove_gone_seeded_connection(ConnectionId, UserId, SessionId, State) ->
    case seeded_sessions([ConnectionId], State) of
        #{ConnectionId := {UserId, SessionId}} ->
            logger:warning(
                "guild_voice_seeded_state_removed: guild_id=~p connection_id=~p"
                " user_id=~p session_id=~p",
                [maps:get(guild_id, State, undefined), ConnectionId, UserId, SessionId]
            ),
            delegate_voice_cast(
                fun guild_voice_disconnect_user:disconnect_voice_user/2,
                #{user_id => UserId, connection_id => ConnectionId},
                State
            );
        _ ->
            State
    end.

-spec delegate_voice_cast(fun((map(), map()) -> {reply, term(), map()}), map(), server_state()) ->
    server_state().
delegate_voice_cast(Fun, Request, State) ->
    GS = guild_voice_server_state:build_guild_state(State),
    {reply, _Reply, NewGS} = Fun(Request, GS),
    guild_voice_server_state:apply_guild_state(NewGS, State).

-spec delegate_voice_call(fun((map(), map()) -> {reply, term(), map()}), map(), server_state()) ->
    {reply, term(), server_state()}.
delegate_voice_call(Fun, Request, State) ->
    GS = guild_voice_server_state:build_guild_state(State),
    {reply, Reply, NewGS} = Fun(Request, GS),
    {reply, Reply, guild_voice_server_state:apply_guild_state(NewGS, State)}.
