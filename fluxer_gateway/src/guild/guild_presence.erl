%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_presence).
-typing([eqwalizer]).

-export([handle_bus_presence/3, send_cached_presence_to_session/3]).
-export([cached_presences/1, send_presence_lookup_to_session/4]).
-export([sync_online_status/2]).

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
    ok = cast_presence_update(Pid, UserId, PresenceMap, NewState2),
    NewState2.

-spec cast_presence_update(pid() | undefined, user_id(), map(), guild_state()) -> ok.
cast_presence_update(BroadcasterPid, UserId, PresenceMap, State) when is_pid(BroadcasterPid) ->
    case safe_presence_update_recipients(UserId, presence_view(State)) of
        {GuildId, [_ | _] = Pids} ->
            PresenceUpdate = PresenceMap#{<<"guild_id">> => integer_to_binary(GuildId)},
            _ = guild_broadcaster:cast_event(
                BroadcasterPid, presence_update, PresenceUpdate, Pids
            ),
            ok;
        _ ->
            ok
    end;
cast_presence_update(_BroadcasterPid, _UserId, _PresenceMap, _State) ->
    ok.

-spec safe_presence_update_recipients(user_id(), guild_state()) -> {integer(), [pid()]} | none.
safe_presence_update_recipients(UserId, View) ->
    try
        presence_update_recipients(UserId, View)
    catch
        Class:Reason:Stack ->
            logger:warning(
                "guild presence_update recipients error: ~p:~p ~p",
                [Class, Reason, Stack]
            ),
            none
    end.

-spec presence_update_recipients(user_id(), guild_state()) -> {integer(), [pid()]} | none.
presence_update_recipients(UserId, View) ->
    case {find_member_by_user_id(UserId, View), guild_id(View)} of
        {undefined, _} ->
            none;
        {_Member, GuildId} when is_integer(GuildId), GuildId > 0 ->
            {GuildId, subscribed_session_pids(UserId, View)};
        _ ->
            none
    end.

-spec subscribed_session_pids(user_id(), guild_state()) -> [pid()].
subscribed_session_pids(UserId, View) ->
    MemberSubs = maps:get(member_subscriptions, View, guild_subscriptions:init_state()),
    case guild_subscriptions:get_subscribed_sessions(UserId, MemberSubs) of
        [] ->
            [];
        SubscribedSessionIds ->
            Sessions = maps:get(sessions, View, #{}),
            TargetChannelMap = guild_presence_sync:get_user_viewable_channel_map(
                UserId, Sessions, View
            ),
            {ValidSessionIds, _InvalidSessionIds} =
                guild_presence_sync:partition_subscribed_sessions(
                    SubscribedSessionIds, Sessions, TargetChannelMap, UserId, View
                ),
            guild_presence_sync:session_pids(ValidSessionIds, Sessions)
    end.

-spec presence_view(guild_state()) -> guild_state().
presence_view(State) ->
    View = maps:with(
        [
            id,
            data,
            sessions,
            member_subscriptions,
            member_presence,
            role_overrides,
            permission_overwrites
        ],
        State
    ),
    case should_trim_view(State) of
        true -> trim_view_data(View);
        false -> View
    end.

-spec should_trim_view(guild_state()) -> boolean().
should_trim_view(State) ->
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

-spec trim_view_data(guild_state()) -> guild_state().
trim_view_data(#{data := Data} = View) when is_map(Data) ->
    View#{data => maps:without(?HEAVY_MEMBER_DATA_KEYS, Data)};
trim_view_data(View) ->
    View.

-spec send_cached_presence_to_session(user_id(), binary(), guild_state()) -> guild_state().
send_cached_presence_to_session(UserId, SessionId, State) ->
    send_presence_lookup_to_session(UserId, SessionId, safe_presence_cache_get(UserId), State).

-spec send_presence_lookup_to_session(
    user_id(), binary(), {ok, map()} | not_found, guild_state()
) ->
    guild_state().
send_presence_lookup_to_session(UserId, SessionId, {ok, Payload}, State) ->
    send_presence_payload_to_session(UserId, SessionId, Payload, State);
send_presence_lookup_to_session(_UserId, _SessionId, not_found, State) ->
    State.

-spec cached_presences([user_id()]) -> #{user_id() => {ok, map()} | not_found}.
cached_presences([]) ->
    #{};
cached_presences(UserIds) ->
    Found = safe_presence_cache_bulk_get(UserIds),
    maps:from_list([{UserId, presence_lookup(UserId, Found)} || UserId <- UserIds]).

-spec presence_lookup(user_id(), #{integer() => map()}) -> {ok, map()} | not_found.
presence_lookup(UserId, Found) ->
    case maps:find(UserId, Found) of
        {ok, Payload} -> {ok, Payload};
        error -> not_found
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

-spec safe_presence_cache_bulk_get([user_id()]) -> #{integer() => map()}.
safe_presence_cache_bulk_get(UserIds) ->
    try
        presence_cache:bulk_get_map(UserIds)
    catch
        _:_ -> #{}
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
    ok = guild_member_list_read:note_presence_write(UserId),
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

online_payload() ->
    #{
        <<"status">> => <<"online">>,
        <<"mobile">> => true,
        <<"afk">> => false,
        <<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"Alpha">>}
    }.

idle_session() ->
    receive
        stop -> ok
    after 60000 -> ok
    end.

handle_bus_presence_casts_presence_update_to_broadcaster_test() ->
    Subscriber = spawn(fun idle_session/0),
    try
        MemberSubs = guild_subscriptions:subscribe(
            <<"s2">>, 1, guild_subscriptions:init_state()
        ),
        State = (presence_test_state())#{
            broadcaster_pid => self(),
            member_subscriptions => MemberSubs,
            sessions => #{
                <<"s1">> => #{user_id => 1, pid => self(), viewable_channels => #{100 => true}},
                <<"s2">> => #{
                    user_id => 2, pid => Subscriber, viewable_channels => #{100 => true}
                }
            }
        },
        {noreply, _NewState} = handle_bus_presence(1, online_payload(), State),
        receive
            {'$gen_cast', {event_broadcast, presence_update, Update, Pids}} ->
                ?assertEqual([Subscriber], Pids),
                ?assertEqual(<<"42">>, maps:get(<<"guild_id">>, Update)),
                ?assertEqual(<<"online">>, maps:get(<<"status">>, Update))
        after 1000 ->
            ?assert(false)
        end
    after
        exit(Subscriber, kill)
    end.

handle_bus_presence_skips_broadcaster_without_subscribers_test() ->
    State = (presence_test_state())#{broadcaster_pid => self()},
    {noreply, _NewState} = handle_bus_presence(1, online_payload(), State),
    receive
        {'$gen_cast', {event_broadcast, presence_update, _, _}} -> ?assert(false)
    after 100 ->
        ok
    end.

view_trim_test_state(Tab) ->
    #{
        id => 42,
        member_count => 10,
        voice_states => #{},
        virtual_channel_access => #{1 => sets:from_list([100])},
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

presence_view_trims_member_data_by_default_test() ->
    application:set_env(fluxer_gateway, presence_snapshot_trim_member_threshold, 2),
    Tab = ets:new(view_trim_members, [set, public]),
    try
        State = view_trim_test_state(Tab),
        View = presence_view(State),
        ViewData = maps:get(data, View),
        ?assertEqual(maps:without(?HEAVY_MEMBER_DATA_KEYS, maps:get(data, State)), ViewData),
        ?assertEqual(Tab, maps:get(members_ets, ViewData)),
        ?assertEqual(maps:get(sessions, State), maps:get(sessions, View)),
        ?assertEqual([data, id, sessions], lists:sort(maps:keys(View)))
    after
        application:unset_env(fluxer_gateway, presence_snapshot_trim_member_threshold),
        ets:delete(Tab)
    end.

presence_view_keeps_member_data_when_trim_disabled_test() ->
    application:set_env(fluxer_gateway, presence_snapshot_trim_enabled, false),
    Tab = ets:new(view_notrim_members, [set, public]),
    try
        State = (view_trim_test_state(Tab))#{member_count => 100000},
        ?assertEqual(maps:get(data, State), maps:get(data, presence_view(State)))
    after
        application:unset_env(fluxer_gateway, presence_snapshot_trim_enabled),
        ets:delete(Tab)
    end.

presence_view_keeps_member_data_below_threshold_test() ->
    application:set_env(fluxer_gateway, presence_snapshot_trim_member_threshold, 50000),
    Tab = ets:new(view_below_members, [set, public]),
    try
        State = view_trim_test_state(Tab),
        ?assertEqual(maps:get(data, State), maps:get(data, presence_view(State)))
    after
        application:unset_env(fluxer_gateway, presence_snapshot_trim_member_threshold),
        ets:delete(Tab)
    end.

reference_snapshot_keys() ->
    [
        id,
        data,
        sessions,
        member_subscriptions,
        member_presence,
        role_overrides,
        permission_overwrites
    ].

reference_project_session(SessionData) when is_map(SessionData) ->
    maps:with([user_id, pid, viewable_channels], SessionData);
reference_project_session(SessionData) ->
    SessionData.

reference_trim_snapshot(#{data := Data, sessions := Sessions} = Snapshot) ->
    Snapshot#{
        data => maps:without(?HEAVY_MEMBER_DATA_KEYS, Data),
        sessions => maps:map(fun(_, S) -> reference_project_session(S) end, Sessions)
    }.

reference_broadcast_snapshot(State) ->
    Base = lists:foldl(
        fun(K, Acc) ->
            case maps:find(K, State) of
                {ok, V} -> Acc#{K => V};
                error -> Acc
            end
        end,
        #{},
        reference_snapshot_keys()
    ),
    case should_trim_view(State) of
        true -> reference_trim_snapshot(Base);
        false -> Base
    end.

reference_broadcaster_recipients(UserId, State) ->
    Snapshot = reference_broadcast_snapshot(State),
    case {find_member_by_user_id(UserId, Snapshot), guild_id(Snapshot)} of
        {undefined, _} ->
            none;
        {_, GuildId} when is_integer(GuildId), GuildId > 0 ->
            Sessions = maps:get(sessions, Snapshot, #{}),
            MemberSubs = maps:get(
                member_subscriptions, Snapshot, guild_subscriptions:init_state()
            ),
            SubscribedSessionIds = guild_subscriptions:get_subscribed_sessions(
                UserId, MemberSubs
            ),
            TargetChannelMap = guild_presence_sync:get_user_viewable_channel_map(
                UserId, Sessions, Snapshot
            ),
            {Valid, _Invalid} = guild_presence_sync:partition_subscribed_sessions(
                SubscribedSessionIds, Sessions, TargetChannelMap, UserId, Snapshot
            ),
            {GuildId, [
                P
             || Sid <- Valid, #{pid := P} <- [maps:get(Sid, Sessions, #{})], is_pid(P)
            ]};
        _ ->
            none
    end.

-define(DIFF_ROLE_A, 201).
-define(DIFF_ROLE_B, 202).
-define(DIFF_CHANNELS, [500, 501, 502, 503, 504]).

diff_user_ids() ->
    lists:seq(10, 29).

diff_view() ->
    constants:view_channel_permission().

diff_overwrite(Id, Type) ->
    #{
        <<"id">> => integer_to_binary(Id),
        <<"type">> => Type,
        <<"allow">> => integer_to_binary(diff_view()),
        <<"deny">> => <<"0">>
    }.

diff_channel(Id, Overwrites) ->
    #{
        <<"id">> => integer_to_binary(Id),
        <<"type">> => 0,
        <<"permission_overwrites">> => Overwrites
    }.

diff_channels(Seed) ->
    Public =
        case Seed rem 2 of
            0 -> [];
            1 -> [diff_overwrite(42, 0)]
        end,
    [
        diff_channel(500, [diff_overwrite(?DIFF_ROLE_A, 0)]),
        diff_channel(501, [diff_overwrite(?DIFF_ROLE_B, 0)]),
        diff_channel(502, [diff_overwrite(?DIFF_ROLE_A, 0), diff_overwrite(?DIFF_ROLE_B, 0)]),
        diff_channel(503, [diff_overwrite(10 + Seed rem 20, 1)]),
        diff_channel(504, Public)
    ].

diff_role(Id) ->
    #{
        <<"id">> => integer_to_binary(Id),
        <<"name">> => integer_to_binary(Id),
        <<"position">> => Id - 42,
        <<"permissions">> => <<"0">>
    }.

diff_member(UserId) ->
    Roles = [integer_to_binary(R) || R <- [?DIFF_ROLE_A, ?DIFF_ROLE_B], rand:uniform(2) =:= 1],
    #{
        <<"user">> => #{
            <<"id">> => integer_to_binary(UserId),
            <<"username">> => integer_to_binary(UserId)
        },
        <<"roles">> => Roles
    }.

diff_data(Seed, WithEts, Tab) ->
    Data = guild_data_index:normalize_data(#{
        <<"guild">> => #{<<"id">> => <<"42">>, <<"owner_id">> => <<"1">>},
        <<"roles">> => [diff_role(42), diff_role(?DIFF_ROLE_A), diff_role(?DIFF_ROLE_B)],
        <<"members">> => [diff_member(U) || U <- diff_user_ids()],
        <<"channels">> => diff_channels(Seed)
    }),
    case WithEts of
        true ->
            true = ets:insert(Tab, maps:to_list(guild_data_index:member_map(Data))),
            Data#{members_ets => Tab};
        false ->
            Data
    end.

diff_random_user() ->
    lists:nth(rand:uniform(20), diff_user_ids()).

diff_session(Pids) ->
    Base = #{
        user_id => diff_random_user(),
        active_guilds => [42],
        pending_connect => false,
        user_roles => []
    },
    WithPid =
        case rand:uniform(8) of
            1 -> Base;
            _ -> Base#{pid => lists:nth(rand:uniform(length(Pids)), Pids)}
        end,
    case rand:uniform(3) of
        1 ->
            WithPid;
        _ ->
            Viewable = maps:from_list([{C, true} || C <- ?DIFF_CHANNELS, rand:uniform(3) =:= 1]),
            WithPid#{viewable_channels => Viewable}
    end.

diff_member_subscriptions(SessionIds) ->
    Candidates = [<<"gone">> | SessionIds],
    lists:foldl(
        fun(UserId, Subs) ->
            lists:foldl(
                fun(Sid, Acc) -> guild_subscriptions:subscribe(Sid, UserId, Acc) end,
                Subs,
                [Sid || Sid <- Candidates, rand:uniform(3) =:= 1]
            )
        end,
        guild_subscriptions:init_state(),
        diff_user_ids()
    ).

diff_state(Seed, WithEts, Tab, Pids) ->
    SessionIds = [<<"s", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 16)],
    Sessions = maps:from_list([{Sid, diff_session(Pids)} || Sid <- SessionIds]),
    #{
        id => 42,
        member_count => 20,
        data => diff_data(Seed, WithEts, Tab),
        sessions => Sessions,
        member_subscriptions => diff_member_subscriptions(SessionIds),
        presence_subscriptions => #{10 => 3, 11 => 1},
        virtual_channel_access => #{
            10 => sets:from_list([500, 501]), 11 => sets:from_list([504])
        },
        voice_states => #{}
    }.

with_trim(on, Fun) ->
    application:set_env(fluxer_gateway, presence_snapshot_trim_member_threshold, 2),
    try
        Fun()
    after
        application:unset_env(fluxer_gateway, presence_snapshot_trim_member_threshold)
    end;
with_trim(off, Fun) ->
    application:set_env(fluxer_gateway, presence_snapshot_trim_enabled, false),
    try
        Fun()
    after
        application:unset_env(fluxer_gateway, presence_snapshot_trim_enabled)
    end.

diff_compare_users(State) ->
    lists:map(
        fun(UserId) ->
            Expected = reference_broadcaster_recipients(UserId, State),
            ?assertEqual(
                {UserId, Expected},
                {UserId, presence_update_recipients(UserId, presence_view(State))}
            ),
            Subscribed = guild_subscriptions:get_subscribed_sessions(
                UserId, maps:get(member_subscriptions, State)
            ),
            {Expected, length(Subscribed)}
        end,
        [99 | diff_user_ids()]
    ).

diff_run(Seed, WithEts, Trim, Pids) ->
    Tab = ets:new(recipients_diff_members, [set, public]),
    try
        _ = rand:seed(exsss, {Seed, 7, 11}),
        State = diff_state(Seed, WithEts, Tab, Pids),
        with_trim(Trim, fun() -> diff_compare_users(State) end)
    after
        ets:delete(Tab)
    end.

presence_update_recipients_match_broadcaster_snapshot_path_test_() ->
    {timeout, 120, fun() ->
        Pids = [spawn(fun idle_session/0) || _ <- lists:seq(1, 6)],
        try
            Results = lists:append([
                diff_run(Seed, WithEts, Trim, Pids)
             || Seed <- lists:seq(1, 60), WithEts <- [true, false], Trim <- [on, off]
            ]),
            NonEmpty = [R || {{_, [_ | _]}, _} = R <- Results],
            Filtered = [R || {{_, Ps}, Subs} = R <- Results, length(Ps) < Subs],
            Unknown = [R || {none, _} = R <- Results],
            ?assert(length(NonEmpty) > 100),
            ?assert(length(Filtered) > 100),
            ?assert(length(Unknown) > 100)
        after
            [exit(P, kill) || P <- Pids]
        end
    end}.

-endif.
