%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_presence).
-typing([eqwalizer]).

-export([handle_bus_presence/3, send_cached_presence_to_session/3]).
-export([broadcast_presence_update/3]).
-export([sync_online_status/2]).
-export([build_broadcast_snapshot/1]).

-export_type([guild_state/0, user_id/0]).

-type guild_state() :: map().
-type member() :: map().
-type user_id() :: integer().

%% members_sorted_ids trims with the member map it indexes: a snapshot that kept it would
%% answer sorted_member_ids/2 with ids for members the snapshot no longer carries.
-define(HEAVY_MEMBER_DATA_KEYS, [
    <<"members">>, members_normalized, <<"member_role_index">>, members_sorted_ids
]).
-define(PRESENCE_SNAPSHOT_TRIM_MEMBER_THRESHOLD_DEFAULT, 5000).
-define(SESSIONS_PROJECTION_CACHE, presence_snapshot_sessions_cache).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec handle_bus_presence(user_id(), map(), guild_state()) -> {noreply, guild_state()}.
handle_bus_presence(UserId, Payload, State) ->
    case maps:get(<<"user_update">>, Payload, false) of
        true -> handle_user_update(UserId, Payload, State);
        false -> handle_presence_update(UserId, Payload, State)
    end.

-spec handle_user_update(user_id(), map(), guild_state()) -> {noreply, guild_state()}.
handle_user_update(UserId, Payload, State) ->
    UserData = maps:get(<<"user">>, Payload, #{}),
    UpdatedState = handle_user_data_update(UserId, UserData, State),
    {ok, NewState} = guild_member_list:broadcast_member_list_updates(
        UserId, State, UpdatedState
    ),
    {noreply, NewState}.

-spec handle_presence_update(user_id(), map(), guild_state()) -> {noreply, guild_state()}.
handle_presence_update(UserId, Payload, State) ->
    case find_member_by_user_id(UserId, State) of
        undefined -> {noreply, State};
        Member -> process_presence(UserId, Payload, Member, State)
    end.

-spec process_presence(user_id(), map(), member(), guild_state()) -> {noreply, guild_state()}.
process_presence(UserId, Payload, Member, State) ->
    PresenceMap = build_presence_map(Payload, Member),
    NormalizedStatus = normalize_presence_status(
        maps:get(<<"status">>, Payload, <<"offline">>)
    ),
    Status = ensure_atom(constants:status_type_atom(NormalizedStatus)),
    OldPresence = guild_state_member:lookup_presence(
        maps:get(member_presence, State),
        UserId
    ),
    process_presence_change(UserId, OldPresence, PresenceMap, Status, State).

-spec process_presence_change(user_id(), map(), map(), atom(), guild_state()) ->
    {noreply, guild_state()}.
process_presence_change(UserId, PresenceMap, PresenceMap, Status, State) ->
    {noreply, maybe_handle_unchanged_presence(Status, UserId, State)};
process_presence_change(UserId, OldPresence, PresenceMap, Status, State) ->
    StateWithPresence = store_member_presence(UserId, PresenceMap, State),
    ok = guild_presence_sync:sync_online_status(UserId, StateWithPresence),
    StateAfterBroadcast = spawn_presence_broadcast(
        UserId, OldPresence, PresenceMap, State, StateWithPresence
    ),
    StateAfterOffline = maybe_handle_offline(Status, UserId, StateAfterBroadcast),
    {noreply, StateAfterOffline}.

-spec maybe_handle_unchanged_presence(atom(), user_id(), guild_state()) -> guild_state().
maybe_handle_unchanged_presence(offline, UserId, State) ->
    maybe_handle_offline(offline, UserId, State);
maybe_handle_unchanged_presence(_Status, _UserId, State) ->
    State.

-spec build_presence_map(map(), member()) -> map().
build_presence_map(Payload, Member) ->
    StatusBin = maps:get(<<"status">>, Payload, <<"offline">>),
    NormalizedStatusBin = normalize_presence_status(StatusBin),
    Mobile = maps:get(<<"mobile">>, Payload, false),
    Afk = maps:get(<<"afk">>, Payload, false),
    MemberUser = maps:get(<<"user">>, Member, #{}),
    CustomStatus = maps:get(<<"custom_status">>, Payload, null),
    presence_payload:build(MemberUser, NormalizedStatusBin, Mobile, Afk, CustomStatus).

-spec maybe_handle_offline(atom(), user_id(), guild_state()) -> guild_state().
maybe_handle_offline(offline, UserId, State) ->
    guild_sessions:handle_user_offline(UserId, State);
maybe_handle_offline(_, _UserId, State) ->
    State.

-spec sync_online_status(user_id(), guild_state()) -> ok.
sync_online_status(UserId, State) ->
    guild_presence_sync:sync_online_status(UserId, State).

-spec spawn_presence_broadcast(
    user_id(),
    map(),
    map(),
    guild_state(),
    guild_state()
) -> guild_state().
spawn_presence_broadcast(UserId, OldPresence, PresenceMap, OldState, NewState) ->
    {ok, NewState1} = guild_member_list:broadcast_member_list_updates(
        UserId,
        OldState,
        NewState,
        OldPresence,
        PresenceMap
    ),
    {Pid, NewState2} = guild_broadcaster:ensure(NewState1),
    {NewSnap, NewState3} = build_broadcast_snapshot_cached(NewState2),
    guild_broadcaster:cast_presence(Pid, UserId, PresenceMap, #{}, NewSnap),
    NewState3.

-spec build_broadcast_snapshot(guild_state()) -> map().
build_broadcast_snapshot(State) ->
    {Snapshot, _State} = build_broadcast_snapshot_cached(State),
    Snapshot.

-spec build_broadcast_snapshot_cached(guild_state()) -> {map(), guild_state()}.
build_broadcast_snapshot_cached(State) ->
    maybe_trim_snapshot(build_base_snapshot(State), State).

-spec build_base_snapshot(guild_state()) -> map().
build_base_snapshot(State) ->
    Keys = [
        id,
        data,
        sessions,
        member_subscriptions,
        member_presence,
        role_overrides,
        permission_overwrites
    ],
    lists:foldl(
        fun(K, Acc) ->
            put_existing_key(K, State, Acc)
        end,
        #{},
        Keys
    ).

-spec maybe_trim_snapshot(map(), guild_state()) -> {map(), guild_state()}.
maybe_trim_snapshot(Snapshot, State) ->
    case should_trim_snapshot(State) of
        true -> trim_snapshot(Snapshot, State);
        false -> {Snapshot, State}
    end.

-spec should_trim_snapshot(guild_state()) -> boolean().
should_trim_snapshot(State) ->
    presence_snapshot_trim_enabled() andalso member_count_at_or_above_threshold(State).

-spec member_count_at_or_above_threshold(guild_state()) -> boolean().
member_count_at_or_above_threshold(State) ->
    case maps:get(member_count, State, undefined) of
        Count when is_integer(Count) ->
            Count >= presence_snapshot_trim_member_threshold();
        _ ->
            false
    end.

-spec presence_snapshot_trim_enabled() -> boolean().
presence_snapshot_trim_enabled() ->
    case application:get_env(fluxer_gateway, presence_snapshot_trim_enabled, true) of
        false -> false;
        _ -> true
    end.

-spec presence_snapshot_trim_member_threshold() -> pos_integer().
presence_snapshot_trim_member_threshold() ->
    case
        application:get_env(
            fluxer_gateway,
            presence_snapshot_trim_member_threshold,
            ?PRESENCE_SNAPSHOT_TRIM_MEMBER_THRESHOLD_DEFAULT
        )
    of
        N when is_integer(N), N > 0 -> N;
        _ -> ?PRESENCE_SNAPSHOT_TRIM_MEMBER_THRESHOLD_DEFAULT
    end.

-spec trim_snapshot(map(), guild_state()) -> {map(), guild_state()}.
trim_snapshot(Snapshot, State) ->
    trim_snapshot_sessions(trim_snapshot_data(Snapshot), State).

-spec trim_snapshot_data(map()) -> map().
trim_snapshot_data(#{data := Data} = Snapshot) when is_map(Data) ->
    Snapshot#{data => maps:without(?HEAVY_MEMBER_DATA_KEYS, Data)};
trim_snapshot_data(Snapshot) ->
    Snapshot.

-spec trim_snapshot_sessions(map(), guild_state()) -> {map(), guild_state()}.
trim_snapshot_sessions(#{sessions := Sessions} = Snapshot, State) when is_map(Sessions) ->
    {Projected, NewState} = cached_projected_sessions(Sessions, State),
    {Snapshot#{sessions => Projected}, NewState};
trim_snapshot_sessions(Snapshot, State) ->
    {Snapshot, State}.

%% Keyed by the sessions map itself, so every writer of that map invalidates the
%% projection without having to know this cache exists.
-spec cached_projected_sessions(map(), guild_state()) -> {map(), guild_state()}.
cached_projected_sessions(Sessions, State) ->
    case maps:get(?SESSIONS_PROJECTION_CACHE, State, undefined) of
        {Sessions, Projected} when is_map(Projected) -> {Projected, State};
        _ -> store_projected_sessions(Sessions, State)
    end.

-spec store_projected_sessions(map(), guild_state()) -> {map(), guild_state()}.
store_projected_sessions(Sessions, State) ->
    Projected = project_sessions(Sessions),
    {Projected, State#{?SESSIONS_PROJECTION_CACHE => {Sessions, Projected}}}.

-spec project_sessions(map()) -> map().
project_sessions(Sessions) ->
    maps:map(
        fun(_SessionId, SessionData) -> project_session(SessionData) end,
        Sessions
    ).

-spec project_session(term()) -> term().
project_session(SessionData) when is_map(SessionData) ->
    maps:with([user_id, pid, viewable_channels], SessionData);
project_session(SessionData) ->
    SessionData.

-spec put_existing_key(atom(), guild_state(), map()) -> map().
put_existing_key(Key, State, Acc) ->
    case maps:find(Key, State) of
        {ok, Value} -> Acc#{Key => Value};
        error -> Acc
    end.

-spec broadcast_presence_update(user_id(), map(), guild_state()) -> ok.
broadcast_presence_update(UserId, Payload, State) ->
    case find_member_by_user_id(UserId, State) of
        undefined -> ok;
        _Member -> broadcast_presence_update_impl(UserId, Payload, State)
    end.

-spec broadcast_presence_update_impl(user_id(), map(), guild_state()) -> ok.
broadcast_presence_update_impl(UserId, Payload, State) ->
    case guild_id(State) of
        GuildId when is_integer(GuildId), GuildId > 0 ->
            PresenceUpdate = Payload#{<<"guild_id">> => integer_to_binary(GuildId)},
            Sessions = maps:get(sessions, State, #{}),
            MemberSubs = maps:get(
                member_subscriptions, State, guild_subscriptions:init_state()
            ),
            SubscribedSessionIds = guild_subscriptions:get_subscribed_sessions(
                UserId, MemberSubs
            ),
            TargetChannelMap = guild_presence_sync:get_user_viewable_channel_map(
                UserId, Sessions, State
            ),
            {ValidSessionIds, InvalidSessionIds} =
                guild_presence_sync:partition_subscribed_sessions(
                    SubscribedSessionIds, Sessions, TargetChannelMap, UserId, State
                ),
            FinalState = guild_presence_sync:remove_invalid_subscriptions(
                InvalidSessionIds, UserId, State
            ),
            FinalSessions = maps:get(sessions, FinalState, #{}),
            guild_presence_sync:dispatch_to_valid_sessions(
                ValidSessionIds, FinalSessions, PresenceUpdate, GuildId
            );
        _ ->
            ok
    end.

-spec send_cached_presence_to_session(user_id(), binary(), guild_state()) -> guild_state().
send_cached_presence_to_session(UserId, SessionId, State) ->
    case safe_presence_cache_get(UserId) of
        {ok, Payload} -> send_presence_payload_to_session(UserId, SessionId, Payload, State);
        _ -> State
    end.

-spec send_presence_payload_to_session(user_id(), binary(), map(), guild_state()) ->
    guild_state().
send_presence_payload_to_session(UserId, SessionId, Payload, State) ->
    case guild_id(State) of
        GuildId when is_integer(GuildId), GuildId > 0 ->
            Sessions = maps:get(sessions, State, #{}),
            send_cached_presence_to_known_session(
                UserId, SessionId, Payload, GuildId, Sessions, State
            );
        _ ->
            State
    end.

-spec send_cached_presence_to_known_session(
    user_id(), binary(), map(), integer(), map(), guild_state()
) -> guild_state().
send_cached_presence_to_known_session(UserId, SessionId, Payload, GuildId, Sessions, State) ->
    case maps:get(SessionId, Sessions, undefined) of
        #{pid := SessionPid} when is_pid(SessionPid) ->
            dispatch_cached_presence(UserId, SessionPid, Payload, GuildId, State);
        _ ->
            State
    end.

-spec dispatch_cached_presence(user_id(), pid(), map(), integer(), guild_state()) ->
    guild_state().
dispatch_cached_presence(UserId, SessionPid, Payload, GuildId, State) ->
    case find_member_by_user_id(UserId, State) of
        undefined ->
            State;
        Member ->
            PresenceBase = build_presence_map(Payload, Member),
            PresenceUpdate = PresenceBase#{<<"guild_id">> => integer_to_binary(GuildId)},
            gateway_dispatch_relay:dispatch(
                SessionPid, presence_update, PresenceUpdate, GuildId
            ),
            State
    end.

-spec ensure_atom(atom() | binary()) -> atom().
ensure_atom(A) when is_atom(A) -> A;
ensure_atom(_) -> undefined.

-spec normalize_presence_status(binary() | term()) -> binary().
normalize_presence_status(<<"invisible">>) -> <<"offline">>;
normalize_presence_status(Status) when is_binary(Status) -> Status;
normalize_presence_status(_) -> <<"offline">>.

-spec safe_presence_cache_get(user_id()) -> {ok, map()} | not_found.
safe_presence_cache_get(UserId) ->
    try presence_cache:get(UserId) of
        {ok, Payload} when is_map(Payload) -> {ok, Payload};
        _ -> not_found
    catch
        _:_ -> not_found
    end.

-spec guild_id(guild_state()) -> integer() | undefined.
guild_id(State) ->
    snowflake_id:parse_optional(maps:get(id, State, undefined)).

-spec handle_user_data_update(user_id(), map(), guild_state()) -> guild_state().
handle_user_data_update(UserId, UserData, State) ->
    case find_member_by_user_id(UserId, State) of
        undefined -> State;
        Member -> apply_user_data_update(UserId, UserData, Member, State)
    end.

-spec apply_user_data_update(user_id(), map(), member(), guild_state()) -> guild_state().
apply_user_data_update(UserId, UserData, Member, State) ->
    CurrentUserData = maps:get(<<"user">>, Member, #{}),
    NormalizedUserData = user_utils:normalize_user(UserData),
    case utils:check_user_data_differs(CurrentUserData, NormalizedUserData) of
        false ->
            State;
        true ->
            UpdatedMember = Member#{<<"user">> => NormalizedUserData},
            Data = map_utils:ensure_map(map_utils:get_safe(State, data, #{})),
            UpdatedData = guild_data_index:put_member(UpdatedMember, Data),
            UpdatedState = State#{data => UpdatedData},
            guild_presence_sync:sync_member_data(UserId, UpdatedState),
            maybe_dispatch_member_update(UserId, UpdatedState),
            UpdatedState
    end.

-spec maybe_dispatch_member_update(user_id(), guild_state()) -> ok.
maybe_dispatch_member_update(UserId, State) ->
    case find_member_by_user_id(UserId, State) of
        undefined -> ok;
        Member -> dispatch_member_update(Member, State)
    end.

-spec dispatch_member_update(map(), guild_state()) -> ok.
dispatch_member_update(Member, State) ->
    case guild_id(State) of
        GuildId when is_integer(GuildId), GuildId > 0 ->
            MemberUpdate = Member#{<<"guild_id">> => integer_to_binary(GuildId)},
            gen_server:cast(
                self(), {dispatch, #{event => guild_member_update, data => MemberUpdate}}
            );
        _ ->
            ok
    end.

-spec find_member_by_user_id(user_id(), guild_state()) -> member() | undefined.
find_member_by_user_id(UserId, State) ->
    guild_permissions:find_member_by_user_id(UserId, State).

-spec store_member_presence(user_id(), map(), guild_state()) -> guild_state().
store_member_presence(UserId, PresenceMap, State) ->
    Tab = maps:get(member_presence, State),
    ets:insert(Tab, {UserId, PresenceMap}),
    State.

-ifdef(TEST).

handle_bus_presence_non_member_noop_test() ->
    Payload = #{<<"status">> => <<"online">>, <<"user">> => #{<<"id">> => <<"99">>}},
    State = #{data => #{<<"members">> => []}, sessions => #{}},
    {noreply, NewState} = handle_bus_presence(99, Payload, State),
    ?assertEqual(State, NewState).

handle_bus_presence_broadcasts_test() ->
    State = presence_test_state(),
    Payload = #{
        <<"status">> => <<"online">>,
        <<"mobile">> => true,
        <<"afk">> => false,
        <<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"Alpha">>}
    },
    {noreply, _NewState} = handle_bus_presence(1, Payload, State),
    ok.

handle_bus_presence_unchanged_payload_is_noop_test() ->
    State = presence_test_state(),
    Payload = #{
        <<"status">> => <<"online">>,
        <<"mobile">> => true,
        <<"afk">> => false,
        <<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"Alpha">>}
    },
    Member = #{} = guild_permissions:find_member_by_user_id(1, State),
    PresenceMap = build_presence_map(Payload, Member),
    ets:insert(maps:get(member_presence, State), {1, PresenceMap}),
    {noreply, NewState} = handle_bus_presence(1, Payload, State),
    ?assertEqual(State, NewState).

handle_bus_presence_user_update_test() ->
    State = presence_test_state(),
    UserData = #{<<"id">> => <<"1">>, <<"username">> => <<"Updated">>},
    Payload = #{<<"user">> => UserData, <<"user_update">> => true},
    {noreply, NewState} = handle_bus_presence(1, Payload, State),
    Data = maps:get(data, NewState),
    Member = maps:get(1, maps:get(<<"members">>, Data)),
    ?assertEqual(<<"Updated">>, maps:get(<<"username">>, maps:get(<<"user">>, Member))).

normalize_presence_status_test() ->
    ?assertEqual(<<"offline">>, normalize_presence_status(<<"invisible">>)),
    ?assertEqual(<<"online">>, normalize_presence_status(<<"online">>)),
    ?assertEqual(<<"idle">>, normalize_presence_status(<<"idle">>)),
    ?assertEqual(<<"offline">>, normalize_presence_status(undefined)).

presence_test_state() ->
    #{
        id => 42,
        data => #{
            <<"members">> => #{
                1 => #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"Alpha">>}}
            }
        },
        sessions => #{},
        member_presence => ets:new(test_member_presence, [set, public]),
        member_list_subscriptions => guild_member_list_subs:new()
    }.

snapshot_trim_test_state(Tab) ->
    #{
        id => 42,
        member_count => 10,
        data => #{
            members_ets => Tab,
            <<"members">> => #{1 => #{<<"user">> => #{<<"id">> => <<"1">>}}},
            members_normalized => #{1 => #{<<"user">> => #{<<"id">> => <<"1">>}}},
            <<"member_role_index">> => #{1 => []},
            members_sorted_ids => [1],
            <<"channels">> => [],
            <<"roles">> => []
        },
        sessions => #{
            <<"s1">> => #{
                user_id => 1,
                pid => self(),
                viewable_channels => #{100 => true},
                active_guilds => [42],
                pending_connect => false
            }
        }
    }.

build_broadcast_snapshot_trims_by_default_test() ->
    application:set_env(fluxer_gateway, presence_snapshot_trim_member_threshold, 2),
    Tab = ets:new(snapshot_trim_members, [set, public]),
    try
        Snap = build_broadcast_snapshot(snapshot_trim_test_state(Tab)),
        SnapData = maps:get(data, Snap),
        ?assertNot(maps:is_key(<<"members">>, SnapData)),
        ?assertNot(maps:is_key(members_normalized, SnapData)),
        ?assertNot(maps:is_key(<<"member_role_index">>, SnapData)),
        ?assertNot(maps:is_key(members_sorted_ids, SnapData)),
        ?assertEqual(Tab, maps:get(members_ets, SnapData)),
        ?assert(maps:is_key(<<"channels">>, SnapData)),
        SnapSession = maps:get(<<"s1">>, maps:get(sessions, Snap)),
        ?assertEqual([pid, user_id, viewable_channels], lists:sort(maps:keys(SnapSession))),
        ?assertNot(maps:is_key(voice_states, Snap)),
        ?assertNot(maps:is_key(member_count, Snap))
    after
        application:unset_env(fluxer_gateway, presence_snapshot_trim_member_threshold),
        ets:delete(Tab)
    end.

build_broadcast_snapshot_no_trim_when_disabled_test() ->
    application:set_env(fluxer_gateway, presence_snapshot_trim_enabled, false),
    Tab = ets:new(snapshot_notrim_members, [set, public]),
    try
        State = (snapshot_trim_test_state(Tab))#{member_count => 100000},
        Snap = build_broadcast_snapshot(State),
        ?assert(maps:is_key(<<"members">>, maps:get(data, Snap))),
        SnapSession = maps:get(<<"s1">>, maps:get(sessions, Snap)),
        ?assert(maps:is_key(active_guilds, SnapSession))
    after
        application:unset_env(fluxer_gateway, presence_snapshot_trim_enabled),
        ets:delete(Tab)
    end.

build_broadcast_snapshot_no_trim_below_threshold_test() ->
    application:set_env(fluxer_gateway, presence_snapshot_trim_member_threshold, 50000),
    Tab = ets:new(snapshot_below_members, [set, public]),
    try
        Snap = build_broadcast_snapshot(snapshot_trim_test_state(Tab)),
        ?assert(maps:is_key(<<"members">>, maps:get(data, Snap)))
    after
        application:unset_env(fluxer_gateway, presence_snapshot_trim_member_threshold),
        ets:delete(Tab)
    end.

reference_project_session(SessionData) when is_map(SessionData) ->
    maps:with([user_id, pid, viewable_channels], SessionData);
reference_project_session(SessionData) ->
    SessionData.

reference_project_sessions(Sessions) ->
    maps:map(
        fun(_SessionId, SessionData) -> reference_project_session(SessionData) end,
        Sessions
    ).

reference_trim_snapshot_sessions(#{sessions := Sessions} = Snapshot) when is_map(Sessions) ->
    Snapshot#{sessions => reference_project_sessions(Sessions)};
reference_trim_snapshot_sessions(Snapshot) ->
    Snapshot.

reference_maybe_trim_snapshot(true, Snapshot) ->
    reference_trim_snapshot_sessions(trim_snapshot_data(Snapshot));
reference_maybe_trim_snapshot(false, Snapshot) ->
    Snapshot.

reference_broadcast_snapshot(State) ->
    reference_maybe_trim_snapshot(should_trim_snapshot(State), build_base_snapshot(State)).

cache_test_session(UserId, ViewableChannels) ->
    #{
        user_id => UserId,
        pid => self(),
        viewable_channels => ViewableChannels,
        active_guilds => [42],
        pending_connect => false
    }.

cache_test_state(Sessions) ->
    #{
        id => 42,
        member_count => 10,
        data => #{<<"channels">> => [], <<"members">> => #{}},
        sessions => Sessions
    }.

assert_cached_snapshot_matches_reference(Sessions, State) ->
    NextState = State#{sessions => Sessions},
    {Snapshot, CachedState} = build_broadcast_snapshot_cached(NextState),
    ?assertEqual(reference_broadcast_snapshot(NextState), Snapshot),
    ?assertNot(maps:is_key(?SESSIONS_PROJECTION_CACHE, Snapshot)),
    CachedState.

session_projection_mutation_sequence() ->
    A = cache_test_session(1, #{100 => true}),
    B = cache_test_session(2, #{100 => true, 200 => true}),
    S1 = #{<<"a">> => A},
    S2 = S1#{<<"b">> => B},
    S3 = S2#{<<"a">> => cache_test_session(1, #{100 => true, 300 => true})},
    S4 = maps:remove(<<"b">>, S3),
    S5 = S4#{<<"a">> => (maps:get(<<"a">>, S4))#{pending_connect => true}},
    S6 = S5#{<<"c">> => not_a_map},
    [#{}, S1, S1, S2, S3, S4, S5, S6, S1, #{}].

cached_projection_matches_reference_across_mutations_test() ->
    application:set_env(fluxer_gateway, presence_snapshot_trim_member_threshold, 2),
    try
        lists:foldl(
            fun assert_cached_snapshot_matches_reference/2,
            cache_test_state(#{}),
            session_projection_mutation_sequence()
        )
    after
        application:unset_env(fluxer_gateway, presence_snapshot_trim_member_threshold)
    end.

cached_projection_is_reused_when_sessions_unchanged_test() ->
    application:set_env(fluxer_gateway, presence_snapshot_trim_member_threshold, 2),
    try
        Sessions = #{<<"a">> => cache_test_session(1, #{100 => true})},
        {Snap1, State1} = build_broadcast_snapshot_cached(cache_test_state(Sessions)),
        {Snap2, State2} = build_broadcast_snapshot_cached(State1),
        Projected1 = maps:get(sessions, Snap1),
        Projected2 = maps:get(sessions, Snap2),
        ?assertEqual(reference_project_sessions(Sessions), Projected1),
        ?assertEqual(Projected1, Projected2),
        ?assert(erts_debug:same(Projected1, Projected2)),
        ?assertEqual({Sessions, Projected1}, maps:get(?SESSIONS_PROJECTION_CACHE, State2))
    after
        application:unset_env(fluxer_gateway, presence_snapshot_trim_member_threshold)
    end.

cached_projection_ignores_foreign_cache_entry_test() ->
    application:set_env(fluxer_gateway, presence_snapshot_trim_member_threshold, 2),
    try
        Sessions = #{<<"a">> => cache_test_session(1, #{100 => true})},
        Stale = #{<<"z">> => cache_test_session(9, #{999 => true})},
        Foreign = {Stale, reference_project_sessions(Stale)},
        State = (cache_test_state(Sessions))#{?SESSIONS_PROJECTION_CACHE => Foreign},
        {Snapshot, _NewState} = build_broadcast_snapshot_cached(State),
        ?assertEqual(reference_broadcast_snapshot(State), Snapshot),
        ?assertEqual(reference_project_sessions(Sessions), maps:get(sessions, Snapshot))
    after
        application:unset_env(fluxer_gateway, presence_snapshot_trim_member_threshold)
    end.

cached_projection_ignores_corrupt_cache_entry_test() ->
    application:set_env(fluxer_gateway, presence_snapshot_trim_member_threshold, 2),
    try
        Sessions = #{<<"a">> => cache_test_session(1, #{100 => true})},
        State = (cache_test_state(Sessions))#{?SESSIONS_PROJECTION_CACHE => {Sessions, junk}},
        {Snapshot, _NewState} = build_broadcast_snapshot_cached(State),
        ?assertEqual(reference_project_sessions(Sessions), maps:get(sessions, Snapshot))
    after
        application:unset_env(fluxer_gateway, presence_snapshot_trim_member_threshold)
    end.

cached_projection_not_stored_when_trim_disabled_test() ->
    application:set_env(fluxer_gateway, presence_snapshot_trim_enabled, false),
    try
        Sessions = #{<<"a">> => cache_test_session(1, #{100 => true})},
        State = cache_test_state(Sessions),
        {Snapshot, NewState} = build_broadcast_snapshot_cached(State),
        ?assertEqual(reference_broadcast_snapshot(State), Snapshot),
        ?assertEqual(Sessions, maps:get(sessions, Snapshot)),
        ?assertNot(maps:is_key(?SESSIONS_PROJECTION_CACHE, NewState))
    after
        application:unset_env(fluxer_gateway, presence_snapshot_trim_enabled)
    end.

handle_bus_presence_threads_projection_cache_test() ->
    application:set_env(fluxer_gateway, presence_snapshot_trim_member_threshold, 2),
    Sessions = #{<<"s1">> => cache_test_session(1, #{100 => true})},
    State = (presence_test_state())#{member_count => 10, sessions => Sessions},
    Payload = #{
        <<"status">> => <<"online">>,
        <<"mobile">> => true,
        <<"afk">> => false,
        <<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"Alpha">>}
    },
    try
        {noreply, NewState} = handle_bus_presence(1, Payload, State),
        ?assertEqual(
            {Sessions, reference_project_sessions(Sessions)},
            maps:get(?SESSIONS_PROJECTION_CACHE, NewState)
        )
    after
        application:unset_env(fluxer_gateway, presence_snapshot_trim_member_threshold)
    end.

-endif.
