%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_presence).

-typing([eqwalizer]).

-export([handle_bus_presence/3, send_cached_presence_to_session/3]).
-export([cached_presences/1, send_presence_lookup_to_session/4]).
-export([sync_online_status/2]).
-export([apply_connect_presences/2]).

-export_type([guild_state/0, user_id/0]).

-type guild_state() :: map().
-type member() :: map().
-type user_id() :: integer().
-type list_sync() :: immediate | deferred.

%% members_sorted_ids trims with the member map it indexes: a snapshot that kept it would
%% answer sorted_member_ids/2 with ids for members the snapshot no longer holds.
-define(HEAVY_MEMBER_DATA_KEYS, [
    <<"members">>, members_normalized, <<"member_role_index">>, members_sorted_ids
]).
-define(PRESENCE_SNAPSHOT_TRIM_MEMBER_THRESHOLD_DEFAULT, 5000).

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
    {ok, NewState} = guild_member_list_write:broadcast_member_list_updates(
        UserId, State, UpdatedState
    ),
    {noreply, NewState}.

-spec apply_connect_presences([user_id()], guild_state()) -> guild_state().
apply_connect_presences([], State) ->
    State;
apply_connect_presences(UserIds, State) ->
    Found = cached_presences(UserIds),
    lists:foldl(
        fun(UserId, Acc) ->
            apply_connect_presence(UserId, maps:get(UserId, Found, not_found), Acc)
        end,
        State,
        UserIds
    ).

-spec apply_connect_presence(user_id(), {ok, map()} | not_found, guild_state()) ->
    guild_state().
apply_connect_presence(UserId, {ok, Payload}, State) ->
    {noreply, NewState} =
        case maps:get(<<"user_update">>, Payload, false) of
            true -> handle_user_update(UserId, Payload, State);
            false -> handle_presence_update(UserId, Payload, deferred, State)
        end,
    NewState;
apply_connect_presence(_UserId, not_found, State) ->
    State.

-spec handle_presence_update(user_id(), map(), guild_state()) -> {noreply, guild_state()}.
handle_presence_update(UserId, Payload, State) ->
    handle_presence_update(UserId, Payload, immediate, State).

-spec handle_presence_update(user_id(), map(), list_sync(), guild_state()) ->
    {noreply, guild_state()}.
handle_presence_update(UserId, Payload, ListSync, State) ->
    case guild_permissions_check:find_member_by_user_id(UserId, State) of
        undefined -> {noreply, State};
        Member -> process_presence(UserId, Payload, Member, ListSync, State)
    end.

-spec process_presence(user_id(), map(), member(), list_sync(), guild_state()) ->
    {noreply, guild_state()}.
process_presence(UserId, Payload, Member, ListSync, State) ->
    PresenceMap = build_presence_map(Payload, Member),
    NormalizedStatus = normalize_presence_status(
        maps:get(<<"status">>, Payload, <<"offline">>)
    ),
    Status = ensure_atom(constants:status_type_atom(NormalizedStatus)),
    OldPresence = guild_state_member:lookup_presence(
        maps:get(member_presence, State),
        UserId
    ),
    process_presence_change(UserId, OldPresence, PresenceMap, Status, ListSync, State).

-spec process_presence_change(user_id(), map(), map(), atom(), list_sync(), guild_state()) ->
    {noreply, guild_state()}.
process_presence_change(UserId, PresenceMap, PresenceMap, Status, _ListSync, State) ->
    {noreply, maybe_handle_unchanged_presence(Status, UserId, State)};
process_presence_change(UserId, OldPresence, PresenceMap, Status, ListSync, State) ->
    StateWithPresence = store_member_presence(UserId, PresenceMap, State),
    ok = guild_presence_sync:sync_online_status(UserId, StateWithPresence),
    StateWithThreads = guild_thread_subscriptions:presence_changed(UserId, StateWithPresence),
    StateAfterBroadcast = spawn_presence_broadcast(
        UserId, OldPresence, PresenceMap, State, StateWithThreads, ListSync
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
    guild_sessions_presence:handle_user_offline(UserId, State);
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
    guild_state(),
    list_sync()
) -> guild_state().
spawn_presence_broadcast(UserId, OldPresence, PresenceMap, OldState, NewState, ListSync) ->
    {ok, NewState1} = member_list_presence_update(
        ListSync, UserId, OldState, NewState, OldPresence, PresenceMap
    ),
    {Pid, NewState2} = guild_broadcaster:ensure(NewState1),
    ok = cast_presence_update(Pid, UserId, PresenceMap, NewState2),
    NewState2.

-spec member_list_presence_update(
    list_sync(), user_id(), guild_state(), guild_state(), map(), map()
) -> {ok, guild_state()}.
member_list_presence_update(immediate, UserId, OldState, NewState, OldPresence, PresenceMap) ->
    guild_member_list_write:broadcast_member_list_updates(
        UserId, OldState, NewState, OldPresence, PresenceMap
    );
member_list_presence_update(deferred, UserId, OldState, NewState, OldPresence, PresenceMap) ->
    guild_member_list_write:queue_member_list_updates(
        UserId, OldState, NewState, OldPresence, PresenceMap
    ).

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
    case {guild_permissions_check:find_member_by_user_id(UserId, View), guild_id(View)} of
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
    case guild_permissions_check:find_member_by_user_id(UserId, State) of
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
    case guild_permissions_check:find_member_by_user_id(UserId, State) of
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
            UpdatedData = guild_data_index_members:put_member(UpdatedMember, Data),
            UpdatedState = State#{data => UpdatedData},
            guild_presence_sync:sync_member_data(UserId, UpdatedState),
            maybe_dispatch_member_update(UserId, UpdatedState),
            UpdatedState
    end.

-spec maybe_dispatch_member_update(user_id(), guild_state()) -> ok.
maybe_dispatch_member_update(UserId, State) ->
    case guild_permissions_check:find_member_by_user_id(UserId, State) of
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

-spec store_member_presence(user_id(), map(), guild_state()) -> guild_state().
store_member_presence(UserId, PresenceMap, State) ->
    Tab = maps:get(member_presence, State),
    ets:insert(Tab, {UserId, PresenceMap}),
    ok = guild_member_list_read:note_presence_write(UserId),
    State.
