%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_visibility_overwrites).
-typing([eqwalizer]).

-export([
    compute_and_dispatch_visibility_changes/2,
    compute_and_dispatch_visibility_changes_for_users/3,
    compute_and_dispatch_visibility_changes_for_channels/3
]).

-export_type([guild_state/0, user_id/0, channel_id/0]).

-type guild_state() :: map().
-type user_id() :: integer().
-type channel_id() :: integer().

-spec compute_and_dispatch_visibility_changes(guild_state(), guild_state()) -> guild_state().
compute_and_dispatch_visibility_changes(OldState, NewState) ->
    SessionEntries = guild_visibility_channels:filter_connected_session_entries(
        maps:get(sessions, NewState, #{})
    ),
    ConnectedVoiceByUser = guild_visibility_channels:connected_voice_channel_sets(NewState),
    dispatch_for_sessions(SessionEntries, OldState, NewState, ConnectedVoiceByUser).

-spec compute_and_dispatch_visibility_changes_for_channels(
    [channel_id()], guild_state(), guild_state()
) -> guild_state().
compute_and_dispatch_visibility_changes_for_channels(ChannelIds, OldState, NewState) ->
    ValidChannelIds = lists:usort([Id || Id <- ChannelIds, is_integer(Id), Id > 0]),
    case ValidChannelIds of
        [] ->
            compute_and_dispatch_visibility_changes(OldState, NewState);
        _ ->
            SessionEntries = guild_visibility_channels:filter_connected_session_entries(
                maps:get(sessions, NewState, #{})
            ),
            ConnectedVoiceByUser = guild_visibility_channels:connected_voice_channel_sets(
                NewState
            ),
            dispatch_channel_changes_for_sessions(
                SessionEntries,
                ValidChannelIds,
                OldState,
                NewState,
                ConnectedVoiceByUser,
                guild_visibility_memo:new(ValidChannelIds, OldState, NewState)
            )
    end.

-spec compute_and_dispatch_visibility_changes_for_users(
    [user_id()], guild_state(), guild_state()
) -> guild_state().
compute_and_dispatch_visibility_changes_for_users([], _OldState, NewState) ->
    NewState;
compute_and_dispatch_visibility_changes_for_users(UserIds, OldState, NewState) ->
    Sessions = maps:get(sessions, NewState, #{}),
    UserIdSet = sets:from_list(UserIds),
    TargetSessions = maps:fold(
        fun(SessionId, SessionData, Acc) ->
            collect_target_session(SessionId, SessionData, UserIdSet, Acc)
        end,
        [],
        Sessions
    ),
    ConnectedVoiceByUser = guild_visibility_channels:connected_voice_channel_sets(NewState),
    dispatch_for_sessions(TargetSessions, OldState, NewState, ConnectedVoiceByUser).

-spec collect_target_session(binary(), map(), sets:set(user_id()), [{binary(), map()}]) ->
    [{binary(), map()}].
collect_target_session(SessionId, SessionData, UserIdSet, Acc) ->
    case session_targets_user(SessionData, UserIdSet) of
        true -> [{SessionId, SessionData} | Acc];
        false -> Acc
    end.

-spec session_targets_user(map(), sets:set(user_id())) -> boolean().
session_targets_user(SessionData, UserIdSet) ->
    session_connected(SessionData) andalso session_user_in_set(SessionData, UserIdSet).

-spec session_connected(map()) -> boolean().
session_connected(SessionData) ->
    case maps:get(pending_connect, SessionData, false) of
        true -> false;
        _ -> true
    end.

-spec session_user_in_set(map(), sets:set(user_id())) -> boolean().
session_user_in_set(SessionData, UserIdSet) ->
    SessionUserId = maps:get(user_id, SessionData, undefined),
    is_integer(SessionUserId) andalso sets:is_element(SessionUserId, UserIdSet).

-spec dispatch_for_sessions(
    [{binary(), map()}], guild_state(), guild_state(), #{user_id() => sets:set(channel_id())}
) -> guild_state().
dispatch_for_sessions(SessionEntries, OldState, NewState, ConnectedVoiceByUser) ->
    case guild_id(NewState) of
        GuildId when is_integer(GuildId), GuildId > 0 ->
            dispatch_sessions_with_guild_id(
                SessionEntries, OldState, NewState, GuildId, ConnectedVoiceByUser
            );
        _ ->
            NewState
    end.

-spec dispatch_sessions_with_guild_id(
    [{binary(), map()}],
    guild_state(),
    guild_state(),
    integer(),
    #{user_id() => sets:set(channel_id())}
) -> guild_state().
dispatch_sessions_with_guild_id(
    SessionEntries, OldState, NewState, GuildId, ConnectedVoiceByUser
) ->
    lists:foldl(
        fun({SessionId, SessionData}, AccState) ->
            dispatch_session_changes(
                SessionId,
                SessionData,
                OldState,
                AccState,
                GuildId,
                ConnectedVoiceByUser
            )
        end,
        NewState,
        SessionEntries
    ).

-spec dispatch_session_changes(
    binary(),
    map(),
    guild_state(),
    guild_state(),
    integer(),
    #{user_id() => sets:set(channel_id())}
) -> guild_state().
dispatch_session_changes(
    SessionId, SessionData, OldState, AccState, GuildId, ConnectedVoiceByUser
) ->
    UserId = maps:get(user_id, SessionData),
    Pid = maps:get(pid, SessionData),
    ConnectedSet = maps:get(UserId, ConnectedVoiceByUser, sets:new()),
    {StateWithCache, Removed0, Added0} =
        compute_channel_diffs(SessionId, SessionData, UserId, OldState, AccState, ConnectedSet),
    Removed = gate_channel_set(SessionData, Removed0, OldState),
    Added = gate_channel_set(SessionData, Added0, StateWithCache),
    dispatch_removed_channels(Removed, Pid, OldState, GuildId),
    dispatch_added_channels(Added, SessionId, SessionData, StateWithCache, GuildId),
    ok = guild_thread_subscriptions:access_gained(
        SessionData, sets:to_list(Added), StateWithCache, GuildId
    ),
    StateWithCache.

-spec gate_channel_set(map(), sets:set(channel_id()), guild_state()) -> sets:set(channel_id()).
gate_channel_set(SessionData, ChannelIds, State) ->
    case guild_thread_gate:needs_variant(State) of
        false ->
            ChannelIds;
        true ->
            sets:filter(
                fun(ChannelId) ->
                    guild_thread_gate:channel_visible(SessionData, ChannelId, State)
                end,
                ChannelIds
            )
    end.

-spec compute_channel_diffs(
    binary(), map(), user_id(), guild_state(), guild_state(), sets:set(channel_id())
) ->
    {guild_state(), sets:set(channel_id()), sets:set(channel_id())}.
compute_channel_diffs(SessionId, SessionData, UserId, OldState, AccState, ConnectedSet) ->
    OldSet = guild_visibility_channels:cached_viewable_channel_set(
        SessionData, UserId, OldState
    ),
    NewSet = sets:from_list(
        guild_visibility_channels:get_user_viewable_channels(UserId, AccState)
    ),
    Removed0 = sets:subtract(OldSet, NewSet),
    {StateWithAccess, PreservedSet} =
        guild_visibility_channels:preserve_connected_channels(
            UserId, Removed0, ConnectedSet, AccState
        ),
    NewSet2 = sets:union(NewSet, PreservedSet),
    StateWithCache = guild_sessions:set_session_viewable_channels(
        SessionId, guild_visibility_channels:viewable_channel_map(NewSet2), StateWithAccess
    ),
    {StateWithCache, sets:subtract(OldSet, NewSet2), sets:subtract(NewSet2, OldSet)}.

-spec dispatch_removed_channels(sets:set(channel_id()), pid(), guild_state(), integer()) -> ok.
dispatch_removed_channels(Removed, Pid, OldState, GuildId) ->
    lists:foreach(
        fun(ChannelId) ->
            guild_visibility_roles:dispatch_channel_delete(ChannelId, Pid, OldState, GuildId)
        end,
        sets:to_list(Removed)
    ).

-spec dispatch_added_channels(
    sets:set(channel_id()), binary(), map(), guild_state(), integer()
) -> ok.
dispatch_added_channels(Added, SessionId, SessionData, StateWithCache, GuildId) ->
    lists:foreach(
        fun(ChannelId) ->
            guild_visibility_roles:dispatch_channel_create(
                ChannelId, SessionData, StateWithCache, GuildId
            ),
            guild_visibility_roles:send_member_list_sync(
                SessionId, SessionData, ChannelId, GuildId, StateWithCache
            )
        end,
        sets:to_list(Added)
    ).

-spec dispatch_channel_changes_for_sessions(
    [{binary(), map()}],
    [channel_id()],
    guild_state(),
    guild_state(),
    #{user_id() => sets:set(channel_id())},
    guild_visibility_memo:memo()
) -> guild_state().
dispatch_channel_changes_for_sessions(
    SessionEntries, ChannelIds, OldState, NewState, ConnectedVoiceByUser, Memo
) ->
    case guild_id(NewState) of
        GuildId when is_integer(GuildId), GuildId > 0 ->
            dispatch_channel_changes_with_guild_id(
                SessionEntries,
                ChannelIds,
                OldState,
                NewState,
                GuildId,
                ConnectedVoiceByUser,
                Memo
            );
        _ ->
            NewState
    end.

-spec dispatch_channel_changes_with_guild_id(
    [{binary(), map()}],
    [channel_id()],
    guild_state(),
    guild_state(),
    integer(),
    #{user_id() => sets:set(channel_id())},
    guild_visibility_memo:memo()
) -> guild_state().
dispatch_channel_changes_with_guild_id(
    SessionEntries, ChannelIds, OldState, NewState, GuildId, ConnectedVoiceByUser, Memo
) ->
    {FinalState, _Memo} = lists:foldl(
        fun({SessionId, SessionData}, {AccState, AccMemo}) ->
            dispatch_per_channel_changes(
                SessionId,
                SessionData,
                ChannelIds,
                OldState,
                AccState,
                GuildId,
                ConnectedVoiceByUser,
                AccMemo
            )
        end,
        {NewState, Memo},
        SessionEntries
    ),
    FinalState.

-spec dispatch_per_channel_changes(
    binary(),
    map(),
    [channel_id()],
    guild_state(),
    guild_state(),
    integer(),
    #{user_id() => sets:set(channel_id())},
    guild_visibility_memo:memo()
) -> {guild_state(), guild_visibility_memo:memo()}.
dispatch_per_channel_changes(
    SessionId, SessionData, ChannelIds, OldState, NewState, GuildId, ConnectedVoiceByUser, Memo
) ->
    UserId = maps:get(user_id, SessionData, undefined),
    case is_integer(UserId) of
        false ->
            {NewState, Memo};
        true ->
            Pid = maps:get(pid, SessionData, undefined),
            OldMember = guild_permissions:find_member_by_user_id(UserId, OldState),
            NewMember = guild_permissions:find_member_by_user_id(UserId, NewState),
            ConnectedSet = maps:get(UserId, ConnectedVoiceByUser, sets:new()),
            InitialViewableMap = guild_visibility_channels:ensure_viewable_channel_map(
                SessionData, UserId, OldState
            ),
            ChangeContext = #{
                user_id => UserId,
                pid => Pid,
                session_id => SessionId,
                session_data => SessionData,
                old_member => OldMember,
                new_member => NewMember,
                connected_set => ConnectedSet,
                old_state => OldState,
                guild_id => GuildId
            },
            {Memoised, Memo1} = guild_visibility_memo:pairs(
                ChannelIds, OldState, NewState, ChangeContext, Memo
            ),
            {FinalMap, StateAfter} = process_channel_list(
                ChannelIds, Memoised, InitialViewableMap, NewState, ChangeContext
            ),
            {
                guild_sessions:set_session_viewable_channels(SessionId, FinalMap, StateAfter),
                Memo1
            }
    end.

-spec process_channel_list(
    [channel_id()], guild_visibility_memo:pairs() | none, map(), guild_state(), map()
) -> {map(), guild_state()}.
process_channel_list(ChannelIds, none, InitialViewableMap, NewState, ChangeContext) ->
    lists:foldl(
        fun(ChannelId, Acc) ->
            apply_channel_change(ChannelId, Acc, ChangeContext)
        end,
        {InitialViewableMap, NewState},
        ChannelIds
    );
process_channel_list(ChannelIds, {Pairs, Settled}, InitialViewableMap, NewState, ChangeContext) ->
    #{connected_set := ConnectedSet} = ChangeContext,
    case {Settled, connected_channels(ChannelIds, ConnectedSet)} of
        {{Visible, Hidden}, []} ->
            {maps:merge(maps:without(Hidden, InitialViewableMap), Visible), NewState};
        {_, Connected} ->
            lists:foldl(
                fun(ChannelId, Acc) ->
                    case lists:member(ChannelId, Connected) of
                        true ->
                            apply_channel_change(ChannelId, Acc, ChangeContext);
                        false ->
                            {OldVisible, NewVisible} = maps:get(ChannelId, Pairs),
                            apply_visibility(
                                ChannelId, OldVisible, NewVisible, Acc, ChangeContext
                            )
                    end
                end,
                {InitialViewableMap, NewState},
                ChannelIds
            )
    end.

-spec connected_channels([channel_id()], sets:set(channel_id())) -> [channel_id()].
connected_channels(ChannelIds, ConnectedSet) ->
    case sets:is_empty(ConnectedSet) of
        true -> [];
        false -> [Id || Id <- ChannelIds, sets:is_element(Id, ConnectedSet)]
    end.

-spec apply_channel_change(
    channel_id(), {map(), guild_state()}, map()
) -> {map(), guild_state()}.
apply_channel_change(ChannelId, {ViewableMapAcc, StateAcc}, ChangeContext) ->
    #{
        user_id := UserId,
        old_member := OldMember,
        new_member := NewMember,
        connected_set := ConnectedSet,
        old_state := OldState
    } = ChangeContext,
    OldVisible = guild_visibility_channels:channel_is_visible(
        UserId, ChannelId, OldMember, OldState
    ),
    {StateAfterPreserve, NewVisible} =
        guild_visibility_channels:ensure_new_channel_visibility(
            UserId, ChannelId, ConnectedSet, NewMember, StateAcc
        ),
    apply_visibility(
        ChannelId, OldVisible, NewVisible, {ViewableMapAcc, StateAfterPreserve}, ChangeContext
    ).

-spec apply_visibility(
    channel_id(), boolean(), boolean(), {map(), guild_state()}, map()
) -> {map(), guild_state()}.
apply_visibility(ChannelId, OldVisible, NewVisible, {ViewableMapAcc, StateAcc}, ChangeContext) ->
    #{
        pid := Pid,
        session_id := SessionId,
        session_data := SessionData,
        old_state := OldState,
        guild_id := GuildId
    } = ChangeContext,
    UpdatedViewableMap = guild_visibility_channels:update_viewable_map_for_channel(
        ViewableMapAcc, ChannelId, NewVisible
    ),
    handle_visibility_transition(
        OldVisible,
        NewVisible,
        ChannelId,
        Pid,
        SessionId,
        SessionData,
        OldState,
        StateAcc,
        GuildId,
        UpdatedViewableMap
    ).

-spec handle_visibility_transition(
    boolean(),
    boolean(),
    channel_id(),
    pid() | term(),
    binary(),
    map(),
    guild_state(),
    guild_state(),
    integer(),
    map()
) -> {map(), guild_state()}.
handle_visibility_transition(true, false, ChId, Pid, _Sid, SD, Old, New, GId, VMap) when
    is_pid(Pid)
->
    case guild_thread_gate:channel_visible(SD, ChId, Old) of
        true -> guild_visibility_roles:dispatch_channel_delete(ChId, Pid, Old, GId);
        false -> ok
    end,
    {VMap, New};
handle_visibility_transition(false, true, ChId, Pid, Sid, SD, _Old, New, GId, VMap) when
    is_pid(Pid)
->
    case guild_thread_gate:channel_visible(SD, ChId, New) of
        true -> gated_channel_create(ChId, Sid, SD, New, GId, VMap);
        false -> {VMap, New}
    end;
handle_visibility_transition(_, _, _ChId, _Pid, _Sid, _SD, _Old, New, _GId, VMap) ->
    {VMap, New}.

-spec gated_channel_create(
    channel_id(), binary(), map(), guild_state(), integer(), map()
) -> {map(), guild_state()}.
gated_channel_create(ChId, Sid, SD, New, GId, VMap) ->
    guild_visibility_roles:dispatch_channel_create(ChId, SD, New, GId),
    ok = guild_thread_subscriptions:access_gained(SD, [ChId], New, GId),
    guild_visibility_roles:send_member_list_sync(Sid, SD, ChId, GId, New),
    FinalMap = guild_visibility_roles:maybe_ensure_parent_category_visible(
        ChId, VMap, New, SD, GId
    ),
    {FinalMap, New}.

-spec guild_id(guild_state()) -> integer() | undefined.
guild_id(State) ->
    snowflake_id:parse_optional(maps:get(id, State, undefined)).
