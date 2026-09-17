%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_server).
-typing([eqwalizer]).

-behaviour(gen_server).

-export([
    start_link/2, start_link/3,
    is_voice_server_pid/1,
    resolve/2,
    resolve_result/2,
    stop/1,
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
-define(MAX_RECENT_DISCONNECTS, 500).
-define(MAX_E2EE_KEYS, 1000).
-define(PENDING_TTL_MS, 300000).
-define(RECENT_DISCONNECT_TTL_MS, 120000).
-define(E2EE_KEY_TTL_MS, 300000).
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

-spec start_link(integer(), pid()) -> gen_server:start_ret().
start_link(GuildId, GuildPid) -> start_link(GuildId, GuildPid, #{}).

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

-spec stop(pid()) -> ok.
stop(Pid) -> gen_server:stop(Pid, normal, 5000).

-spec lookup(integer()) -> {ok, pid()} | {error, not_found}.
lookup(GuildId) ->
    guild_voice_server_sync:ensure_registry(),
    case ets:lookup(?REGISTRY_TABLE, GuildId) of
        [{_, Pid}] when is_pid(Pid) ->
            handle_registered_lookup(GuildId, Pid);
        _ ->
            recover_from_guild(GuildId)
    end.

-spec handle_registered_lookup(integer(), pid()) -> {ok, pid()} | {error, not_found}.
handle_registered_lookup(GuildId, Pid) ->
    case process_liveness:is_alive(Pid) of
        true ->
            {ok, Pid};
        false ->
            ets:delete(?REGISTRY_TABLE, GuildId),
            recover_from_guild(GuildId)
    end.

-spec lookup_registered(integer()) -> {ok, pid()} | {error, not_found}.
lookup_registered(GuildId) ->
    Result = safe_lookup(fun() -> ets:lookup(?REGISTRY_TABLE, GuildId) end),
    case Result of
        [{_, Pid}] when is_pid(Pid) -> live_pid_result(Pid);
        _ -> {error, not_found}
    end.

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
            resolve_after_lookup_miss(GuildId, FallbackPid)
    end.

-spec resolve_after_lookup_miss(integer(), pid()) -> {ok, pid()} | {error, not_found}.
resolve_after_lookup_miss(GuildId, FallbackPid) ->
    case resolve_from_owner(GuildId) of
        {ok, VoicePid} -> {ok, VoicePid};
        {error, not_found} -> resolve_from_guild_pid(FallbackPid)
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
    {ok, fun guild_voice:voice_state_update/2};
voice_request_handler(get_voice_state) ->
    {ok, fun guild_voice:get_voice_state/2};
voice_request_handler(update_member_voice) ->
    {ok, fun guild_voice:update_member_voice/2};
voice_request_handler(disconnect_voice_user) ->
    {ok, fun guild_voice:disconnect_voice_user/2};
voice_request_handler(disconnect_voice_user_if_in_channel) ->
    {ok, fun guild_voice:disconnect_voice_user_if_in_channel/2};
voice_request_handler(disconnect_all_voice_users_in_channel) ->
    {ok, fun guild_voice:disconnect_all_voice_users_in_channel/2};
voice_request_handler(confirm_voice_connection_from_livekit) ->
    {ok, fun guild_voice:confirm_voice_connection_from_livekit/2};
voice_request_handler(move_member) ->
    {ok, fun guild_voice:move_member/2};
voice_request_handler(switch_voice_region) ->
    {ok, fun guild_voice:switch_voice_region_handler/2};
voice_request_handler(_) ->
    error.

-spec handle_call_local(term(), server_state()) -> {reply, term(), server_state()}.
handle_call_local({store_pending_connection, ConnectionId, Metadata}, State) ->
    Pending = maps:get(pending_voice_connections, State, #{}),
    NewPending = bounded_put(ConnectionId, Metadata, Pending, ?MAX_PENDING_CONNECTIONS),
    {reply, ok, State#{pending_voice_connections => NewPending}};
handle_call_local({get_voice_states_for_channel, ChIdBin}, State) ->
    {reply, channel_query(voice_states, ChIdBin, State), State};
handle_call_local({get_pending_joins_for_channel, ChIdBin}, State) ->
    {reply, channel_query(pending_joins, ChIdBin, State), State};
handle_call_local({get_voice_states_list}, State) ->
    {reply, maps:values(maps:get(voice_states, State, #{})), State};
handle_call_local({get_voice_states_map}, State) ->
    {reply, maps:get(voice_states, State, #{}), State};
handle_call_local(get_voice_server_pid, State) ->
    {reply, {ok, self()}, State};
handle_call_local({set_voice_states, VoiceStates}, State) when is_map(VoiceStates) ->
    {reply, ok, do_set_voice_states(VoiceStates, State)};
handle_call_local(_, State) ->
    {reply, ok, State}.

-spec channel_query(voice_states | pending_joins, term(), server_state()) -> map().
channel_query(Kind, ChIdBin, State) ->
    case guild_voice_server_state:parse_voice_channel_id(ChIdBin) of
        {ok, _, Bin} when Kind =:= voice_states ->
            guild_voice_server_state:local_voice_states_for_channel(Bin, State);
        {ok, _, Bin} when Kind =:= pending_joins ->
            guild_voice_server_state:local_pending_joins_for_channel(Bin, State);
        error when Kind =:= voice_states -> #{voice_states => []};
        error ->
            #{pending_joins => []}
    end.

-spec do_set_voice_states(map(), server_state()) -> server_state().
do_set_voice_states(VoiceStates, State) ->
    OldVS = maps:get(voice_states, State, #{}),
    NewVS = voice_state_utils:ensure_voice_states(VoiceStates),
    _ = guild_voice_server_sync:sync_replaced_voice_states(OldVS, NewVS),
    State#{voice_states => NewVS}.

-spec handle_cast(term(), server_state()) -> {noreply, server_state()}.
handle_cast({store_pending_connection, ConnId, Meta}, State) ->
    Pending = maps:get(pending_voice_connections, State, #{}),
    NewPending = bounded_put(ConnId, Meta, Pending, ?MAX_PENDING_CONNECTIONS),
    {noreply, State#{pending_voice_connections => NewPending}};
handle_cast({disconnect_voice_user, Request}, State) when is_map(Request) ->
    {noreply, delegate_voice_cast(fun guild_voice:disconnect_voice_user/2, Request, State)};
handle_cast({cleanup_virtual_access_for_user, UserId}, State) when is_integer(UserId) ->
    GS = guild_voice_server_state:build_guild_state(State),
    NewGS = guild_voice_disconnect:cleanup_virtual_channel_access_for_user(UserId, GS),
    {noreply, guild_voice_server_state:apply_guild_state(NewGS, State)};
handle_cast(_, State) ->
    {noreply, State}.

-spec handle_info(term(), server_state()) ->
    {noreply, server_state()} | {stop, normal, server_state()}.
handle_info(sweep_pending_joins, State) ->
    erlang:send_after(?SWEEP_INTERVAL_MS, self(), sweep_pending_joins),
    State1 = sweep_recently_disconnected(State),
    State2 = sweep_e2ee_room_keys(State1),
    case maps:size(maps:get(pending_voice_connections, State2, #{})) of
        0 ->
            {noreply, State2};
        _ ->
            GuildState = guild_voice_server_state:build_guild_state(State2),
            NewGuildState = guild_voice_connection:sweep_expired_pending_joins(GuildState),
            {noreply, guild_voice_server_state:apply_guild_state(NewGuildState, State2)}
    end;
handle_info({check_seeded_sessions, Round, ConnectionIds}, State) ->
    ok = spawn_seeded_session_check(Round, seeded_sessions(ConnectionIds, State), State),
    {noreply, State};
handle_info({seeded_sessions_gone, first, Gone}, State) ->
    ok = schedule_seeded_session_check(second, maps:keys(Gone)),
    {noreply, State};
handle_info({seeded_sessions_gone, second, Gone}, State) ->
    {noreply, remove_gone_seeded_connections(Gone, State)};
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

-spec safe_lookup(fun(() -> term())) -> term().
safe_lookup(Fun) ->
    try
        Fun()
    catch
        throw:_ -> {error, not_found};
        error:_ -> {error, not_found};
        exit:_ -> {error, not_found}
    end.

-spec safe_lookup_match(term()) -> {ok, pid()} | {error, not_found}.
safe_lookup_match({ok, Pid}) when is_pid(Pid) -> {ok, Pid};
safe_lookup_match(_) -> {error, not_found}.

-spec schedule_seeded_session_check(first | second, [binary()]) -> ok.
schedule_seeded_session_check(_Round, []) ->
    ok;
schedule_seeded_session_check(Round, ConnectionIds) ->
    _ = erlang:send_after(
        ?SEEDED_SESSION_CHECK_DELAY_MS, self(), {check_seeded_sessions, Round, ConnectionIds}
    ),
    ok.

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
            voice_state_utils:normalize_session_id(
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
    _ = spawn(fun() ->
        SessionIds = lists:usort([SessionId || {_UserId, SessionId} <- maps:values(Seeded)]),
        GoneSessions = [SessionId || SessionId <- SessionIds, IsGone(SessionId)],
        Gone = maps:filter(
            fun(_ConnectionId, {_UserId, SessionId}) ->
                lists:member(SessionId, GoneSessions)
            end,
            Seeded
        ),
        Self ! {seeded_sessions_gone, Round, Gone}
    end),
    ok.

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
                fun guild_voice:disconnect_voice_user/2,
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

-spec resolve_from_owner(integer()) -> {ok, pid()} | {error, not_found}.
resolve_from_owner(GuildId) ->
    Result = safe_lookup(fun() -> gateway_node_router:owner_node_result(GuildId, guilds) end),
    case Result of
        {ok, Owner} when is_atom(Owner), Owner =/= node() ->
            lookup_on_owner(Owner, GuildId);
        _ ->
            {error, not_found}
    end.

-spec lookup_on_owner(atom(), integer()) -> {ok, pid()} | {error, not_found}.
lookup_on_owner(Owner, GuildId) ->
    Result = safe_lookup(fun() -> rpc:call(Owner, ?MODULE, lookup, [GuildId], 1000) end),
    safe_lookup_match(Result).

-spec resolve_from_guild_pid(pid()) -> {ok, pid()} | {error, not_found}.
resolve_from_guild_pid(FallbackPid) ->
    Result = safe_lookup(fun() -> gen_server:call(FallbackPid, get_voice_server_pid, 500) end),
    case Result of
        {ok, VoicePid} when is_pid(VoicePid) -> live_pid_result(VoicePid);
        _ -> {error, not_found}
    end.

-spec recover_from_guild(integer()) -> {ok, pid()} | {error, not_found}.
recover_from_guild(GuildId) ->
    Result = safe_lookup(fun() -> ets:lookup(guild_pid_cache, GuildId) end),
    case Result of
        [{_, GuildPid}] when is_pid(GuildPid) ->
            ask_live_guild_for_voice_pid(GuildId, GuildPid);
        _ ->
            {error, not_found}
    end.

-spec ask_live_guild_for_voice_pid(integer(), pid()) -> {ok, pid()} | {error, not_found}.
ask_live_guild_for_voice_pid(GuildId, GuildPid) ->
    case process_liveness:is_alive(GuildPid) of
        true -> ask_guild_for_voice_pid(GuildId, GuildPid);
        false -> {error, not_found}
    end.

-spec ask_guild_for_voice_pid(integer(), pid()) -> {ok, pid()} | {error, not_found}.
ask_guild_for_voice_pid(GuildId, GuildPid) ->
    Result = safe_lookup(fun() -> gen_server:call(GuildPid, get_voice_server_pid, 500) end),
    case Result of
        {ok, VSP} when is_pid(VSP) ->
            cache_live_voice_pid(GuildId, VSP);
        _ ->
            {error, not_found}
    end.

-spec live_pid_result(pid()) -> {ok, pid()} | {error, not_found}.
live_pid_result(Pid) ->
    case process_liveness:is_alive(Pid) of
        true -> {ok, Pid};
        false -> {error, not_found}
    end.

-spec cache_live_voice_pid(integer(), pid()) -> {ok, pid()} | {error, not_found}.
cache_live_voice_pid(GuildId, VoicePid) ->
    case live_pid_result(VoicePid) of
        {ok, VoicePid} ->
            ets:insert(?REGISTRY_TABLE, {GuildId, VoicePid}),
            {ok, VoicePid};
        {error, not_found} ->
            {error, not_found}
    end.

-spec bounded_put(term(), term(), map(), pos_integer()) -> map().
bounded_put(Key, Value, Map, MaxSize) ->
    case maps:is_key(Key, Map) of
        true ->
            Map#{Key => Value};
        false ->
            bounded_put_new(Key, Value, Map, MaxSize)
    end.

-spec bounded_put_new(term(), term(), map(), pos_integer()) -> map().
bounded_put_new(Key, Value, Map, MaxSize) ->
    case maps:size(Map) >= MaxSize of
        true ->
            logger:warning("Voice server bounded map at capacity", #{max_size => MaxSize}),
            Map;
        false ->
            Map#{Key => Value}
    end.

-spec sweep_recently_disconnected(map()) -> map().
sweep_recently_disconnected(State) ->
    Cache = maps:get(recently_disconnected_voice_states, State, #{}),
    case maps:size(Cache) of
        0 ->
            State;
        _ ->
            Capped = evict_stale_disconnects(Cache),
            State#{recently_disconnected_voice_states => Capped}
    end.

-spec evict_stale_disconnects(map()) -> map().
evict_stale_disconnects(Cache) ->
    Now = erlang:system_time(millisecond),
    IsAlive = fun
        (_ConnId, #{disconnected_at := At}) -> (Now - At) < ?RECENT_DISCONNECT_TTL_MS;
        (_ConnId, _) -> false
    end,
    enforce_map_cap(maps:filter(IsAlive, Cache), ?MAX_RECENT_DISCONNECTS).

-spec sweep_e2ee_room_keys(server_state()) -> server_state().
sweep_e2ee_room_keys(State) ->
    Keys = maps:get(e2ee_room_keys, State, #{}),
    case maps:size(Keys) of
        0 ->
            State;
        Size when Size =< ?MAX_E2EE_KEYS ->
            State;
        _ ->
            State#{e2ee_room_keys => enforce_map_cap(Keys, ?MAX_E2EE_KEYS)}
    end.

-spec enforce_map_cap(map(), pos_integer()) -> map().
enforce_map_cap(Map, MaxSize) ->
    case maps:size(Map) =< MaxSize of
        true ->
            Map;
        false ->
            {Keep, _} = lists:split(MaxSize, maps:to_list(Map)),
            maps:from_list(Keep)
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

seeded_sessions_only_tracks_states_with_a_session_test() ->
    State = #{
        voice_states => #{
            <<"a">> => #{<<"user_id">> => <<"5">>, <<"session_id">> => <<"sess-a">>},
            <<"b">> => #{<<"user_id">> => <<"6">>},
            <<"c">> => #{<<"user_id">> => <<"7">>, <<"session_id">> => null}
        }
    },
    ?assertEqual(
        #{<<"a">> => {5, <<"sess-a">>}},
        seeded_sessions([<<"a">>, <<"b">>, <<"c">>, <<"missing">>], State)
    ).

seeded_session_check_reports_only_gone_sessions_test() ->
    State = #{
        voice_states => #{
            <<"a">> => #{<<"user_id">> => <<"5">>, <<"session_id">> => <<"dead">>},
            <<"b">> => #{<<"user_id">> => <<"6">>, <<"session_id">> => <<"alive">>}
        },
        test_session_gone_fun => fun(SessionId) -> SessionId =:= <<"dead">> end
    },
    ok = spawn_seeded_session_check(first, seeded_sessions([<<"a">>, <<"b">>], State), State),
    receive
        {seeded_sessions_gone, first, Gone} ->
            ?assertEqual(#{<<"a">> => {5, <<"dead">>}}, Gone)
    after 1000 -> ?assert(false)
    end.

gone_seeded_connection_is_removed_test() ->
    TestFun = fun(_, _, _, _) -> {ok, #{success => true}} end,
    GuildPid = spawn(fun() -> guild_state_reply_loop(TestFun) end),
    State = seeded_test_state(GuildPid),
    try
        {noreply, NewState} = handle_info(
            {seeded_sessions_gone, second, #{<<"ghost">> => {5, <<"dead">>}}}, State
        ),
        ?assertEqual([<<"live">>], maps:keys(maps:get(voice_states, NewState)))
    after
        exit(GuildPid, kill)
    end.

seeded_connection_that_changed_session_is_kept_test() ->
    TestFun = fun(_, _, _, _) -> {ok, #{success => true}} end,
    GuildPid = spawn(fun() -> guild_state_reply_loop(TestFun) end),
    State = seeded_test_state(GuildPid),
    try
        {noreply, NewState} = handle_info(
            {seeded_sessions_gone, second, #{<<"ghost">> => {5, <<"an-older-session">>}}}, State
        ),
        ?assertEqual(
            lists:sort([<<"ghost">>, <<"live">>]),
            lists:sort(maps:keys(maps:get(voice_states, NewState)))
        )
    after
        exit(GuildPid, kill)
    end.

seeded_test_state(GuildPid) ->
    #{
        guild_id => 42,
        guild_pid => GuildPid,
        voice_states => #{
            <<"ghost">> => #{
                <<"user_id">> => <<"5">>,
                <<"guild_id">> => <<"42">>,
                <<"channel_id">> => <<"20">>,
                <<"connection_id">> => <<"ghost">>,
                <<"session_id">> => <<"dead">>
            },
            <<"live">> => #{
                <<"user_id">> => <<"6">>,
                <<"guild_id">> => <<"42">>,
                <<"channel_id">> => <<"20">>,
                <<"connection_id">> => <<"live">>,
                <<"session_id">> => <<"alive">>
            }
        },
        pending_voice_connections => #{},
        recently_disconnected_voice_states => #{},
        e2ee_room_keys => #{}
    }.

disconnect_voice_user_cast_removes_the_voice_state_test() ->
    Self = self(),
    TestFun = fun(GId, ChId, UId, ConnId) ->
        Self ! {force_disconnect, GId, ChId, UId, ConnId},
        {ok, #{success => true}}
    end,
    GuildPid = spawn(fun() -> guild_state_reply_loop(TestFun) end),
    State = #{
        guild_id => 42,
        guild_pid => GuildPid,
        voice_states => #{
            <<"conn">> => #{
                <<"user_id">> => <<"5">>,
                <<"guild_id">> => <<"42">>,
                <<"channel_id">> => <<"20">>,
                <<"connection_id">> => <<"conn">>
            }
        },
        pending_voice_connections => #{},
        recently_disconnected_voice_states => #{},
        e2ee_room_keys => #{}
    },
    try
        {noreply, NewState} = handle_cast(
            {disconnect_voice_user, #{user_id => 5, connection_id => null}}, State
        ),
        ?assertEqual(#{}, maps:get(voice_states, NewState)),
        receive
            {force_disconnect, 42, 20, 5, <<"conn">>} -> ok
        after 200 -> ?assert(false)
        end
    after
        exit(GuildPid, kill)
    end.

guild_state_reply_loop(TestFun) ->
    GuildState = #{
        id => 42,
        data => #{<<"guild">> => #{<<"owner_id">> => <<"999">>}},
        sessions => #{},
        test_force_disconnect_fun => TestFun
    },
    receive
        {'$gen_call', From, {get_voice_guild_state}} ->
            gen_server:reply(From, GuildState),
            guild_state_reply_loop(TestFun);
        _ ->
            guild_state_reply_loop(TestFun)
    end.

apply_guild_state_preserves_e2ee_test() ->
    State = #{
        guild_id => 1,
        guild_pid => self(),
        voice_states => #{},
        pending_voice_connections => #{},
        recently_disconnected_voice_states => #{},
        e2ee_room_keys => #{10 => <<"old-key">>}
    },
    GuildState = State#{e2ee_room_keys => #{10 => <<"new-key">>}},
    NewState = guild_voice_server_state:merge_guild_state(GuildState, State),
    ?assertEqual(#{10 => <<"new-key">>}, maps:get(e2ee_room_keys, NewState)).

resolve_asks_fallback_test() ->
    GuildId = 987654321,
    VoicePid = spawn(fun voice_pid_loop/0),
    GuildPid = spawn(fun() -> voice_pid_reply_loop(VoicePid) end),
    try
        ?assertEqual(VoicePid, resolve(GuildId, GuildPid))
    after
        VoicePid ! stop,
        exit(GuildPid, kill)
    end.

resolve_rejects_dead_voice_pid_test() ->
    GuildId = 987654323,
    DeadVoicePid = spawn(fun() -> ok end),
    MonitorRef = erlang:monitor(process, DeadVoicePid),
    receive
        {'DOWN', MonitorRef, process, DeadVoicePid, _} -> ok
    after 1000 -> error(timeout)
    end,
    GuildPid = spawn(fun() ->
        voice_pid_reply_loop(DeadVoicePid)
    end),
    try
        ?assertEqual({error, not_found}, resolve_result(GuildId, GuildPid)),
        ?assertEqual(GuildPid, resolve(GuildId, GuildPid))
    after
        exit(GuildPid, kill)
    end.

parse_voice_channel_id_test() ->
    ?assertEqual({ok, 9, <<"9">>}, guild_voice_server_state:parse_voice_channel_id(<<"9">>)),
    ?assertEqual({ok, 9, <<"9">>}, guild_voice_server_state:parse_voice_channel_id(9)),
    ?assertEqual(error, guild_voice_server_state:parse_voice_channel_id(null)).

rpc_entry_formatters_test() ->
    VSEntries = guild_voice_server_state:voice_state_rpc_entries([
        not_a_map,
        #{
            <<"connection_id">> => <<"conn-a">>,
            <<"user_id">> => 42,
            <<"channel_id">> => 9,
            <<"region_id">> => <<"local">>,
            <<"server_id">> => <<"dev-1">>
        },
        #{<<"connection_id">> => null, <<"user_id">> => 43, <<"channel_id">> => 9}
    ]),
    ?assertEqual(
        [
            #{
                connection_id => <<"conn-a">>,
                user_id => <<"42">>,
                channel_id => <<"9">>,
                region_id => <<"local">>,
                server_id => <<"dev-1">>
            }
        ],
        VSEntries
    ),
    PJEntries = guild_voice_server_state:pending_join_rpc_entries([
        not_a_map,
        #{
            connection_id => <<"conn-a">>,
            user_id => 42,
            token_nonce => <<"nonce">>,
            expires_at => 123
        },
        #{connection_id => <<"missing-user">>, token_nonce => <<"nonce">>}
    ]),
    ?assertEqual(1, length(PJEntries)).

bounded_put_under_limit_test() ->
    Map = #{a => 1, b => 2},
    Result = bounded_put(c, 3, Map, 5),
    ?assertEqual(#{a => 1, b => 2, c => 3}, Result).

bounded_put_at_limit_drops_test() ->
    Map = #{a => 1, b => 2},
    Result = bounded_put(c, 3, Map, 2),
    ?assertEqual(#{a => 1, b => 2}, Result).

bounded_put_existing_key_updates_test() ->
    Map = #{a => 1, b => 2},
    Result = bounded_put(a, 99, Map, 2),
    ?assertEqual(#{a => 99, b => 2}, Result).

enforce_map_cap_under_test() ->
    Map = #{a => 1, b => 2},
    ?assertEqual(Map, enforce_map_cap(Map, 5)).

enforce_map_cap_over_test() ->
    Map = #{a => 1, b => 2, c => 3},
    Result = enforce_map_cap(Map, 2),
    ?assertEqual(2, maps:size(Result)).

sweep_recently_disconnected_empty_test() ->
    State = #{recently_disconnected_voice_states => #{}},
    ?assertEqual(State, sweep_recently_disconnected(State)).

sweep_recently_disconnected_evicts_old_test() ->
    Now = erlang:system_time(millisecond),
    Old = Now - ?RECENT_DISCONNECT_TTL_MS - 1000,
    Cache = #{
        <<"old-conn">> => #{disconnected_at => Old, voice_state => #{}},
        <<"new-conn">> => #{disconnected_at => Now, voice_state => #{}}
    },
    State = #{recently_disconnected_voice_states => Cache},
    Result = sweep_recently_disconnected(State),
    RD = maps:get(recently_disconnected_voice_states, Result),
    ?assertNot(maps:is_key(<<"old-conn">>, RD)),
    ?assert(maps:is_key(<<"new-conn">>, RD)).

voice_pid_loop() ->
    receive
        stop -> ok
    after 5000 ->
        ok
    end.

voice_pid_reply_loop(VoicePid) ->
    receive
        {'$gen_call', {From, Tag}, get_voice_server_pid} -> From ! {Tag, {ok, VoicePid}}
    after 5000 ->
        ok
    end.

-endif.
