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
                SessionEntries, ValidChannelIds, OldState, NewState, ConnectedVoiceByUser
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
    {StateWithCache, Removed, Added} =
        compute_channel_diffs(SessionId, SessionData, UserId, OldState, AccState, ConnectedSet),
    dispatch_removed_channels(Removed, Pid, OldState, GuildId),
    dispatch_added_channels(Added, Pid, SessionId, SessionData, StateWithCache, GuildId),
    StateWithCache.

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
    sets:set(channel_id()), pid(), binary(), map(), guild_state(), integer()
) -> ok.
dispatch_added_channels(Added, Pid, SessionId, SessionData, StateWithCache, GuildId) ->
    lists:foreach(
        fun(ChannelId) ->
            guild_visibility_roles:dispatch_channel_create(
                ChannelId, Pid, StateWithCache, GuildId
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
    #{user_id() => sets:set(channel_id())}
) -> guild_state().
dispatch_channel_changes_for_sessions(
    SessionEntries, ChannelIds, OldState, NewState, ConnectedVoiceByUser
) ->
    case guild_id(NewState) of
        GuildId when is_integer(GuildId), GuildId > 0 ->
            dispatch_channel_changes_with_guild_id(
                SessionEntries, ChannelIds, OldState, NewState, GuildId, ConnectedVoiceByUser
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
    #{user_id() => sets:set(channel_id())}
) -> guild_state().
dispatch_channel_changes_with_guild_id(
    SessionEntries, ChannelIds, OldState, NewState, GuildId, ConnectedVoiceByUser
) ->
    lists:foldl(
        fun({SessionId, SessionData}, AccState) ->
            dispatch_per_channel_changes(
                SessionId,
                SessionData,
                ChannelIds,
                OldState,
                AccState,
                GuildId,
                ConnectedVoiceByUser
            )
        end,
        NewState,
        SessionEntries
    ).

-spec dispatch_per_channel_changes(
    binary(),
    map(),
    [channel_id()],
    guild_state(),
    guild_state(),
    integer(),
    #{user_id() => sets:set(channel_id())}
) -> guild_state().
dispatch_per_channel_changes(
    SessionId, SessionData, ChannelIds, OldState, NewState, GuildId, ConnectedVoiceByUser
) ->
    UserId = maps:get(user_id, SessionData, undefined),
    case is_integer(UserId) of
        false ->
            NewState;
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
            {FinalMap, StateAfter} = process_channel_list(
                ChannelIds, InitialViewableMap, NewState, ChangeContext
            ),
            guild_sessions:set_session_viewable_channels(
                SessionId, FinalMap, StateAfter
            )
    end.

-spec process_channel_list(
    [channel_id()], map(), guild_state(), map()
) -> {map(), guild_state()}.
process_channel_list(ChannelIds, InitialViewableMap, NewState, ChangeContext) ->
    lists:foldl(
        fun(ChannelId, Acc) ->
            apply_channel_change(ChannelId, Acc, ChangeContext)
        end,
        {InitialViewableMap, NewState},
        ChannelIds
    ).

-spec apply_channel_change(
    channel_id(), {map(), guild_state()}, map()
) -> {map(), guild_state()}.
apply_channel_change(ChannelId, {ViewableMapAcc, StateAcc}, ChangeContext) ->
    #{
        user_id := UserId,
        pid := Pid,
        session_id := SessionId,
        session_data := SessionData,
        old_member := OldMember,
        new_member := NewMember,
        connected_set := ConnectedSet,
        old_state := OldState,
        guild_id := GuildId
    } = ChangeContext,
    OldVisible = guild_visibility_channels:channel_is_visible(
        UserId, ChannelId, OldMember, OldState
    ),
    {StateAfterPreserve, NewVisible} =
        guild_visibility_channels:ensure_new_channel_visibility(
            UserId, ChannelId, ConnectedSet, NewMember, StateAcc
        ),
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
        StateAfterPreserve,
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
handle_visibility_transition(true, false, ChId, Pid, _Sid, _SD, Old, New, GId, VMap) when
    is_pid(Pid)
->
    guild_visibility_roles:dispatch_channel_delete(ChId, Pid, Old, GId),
    {VMap, New};
handle_visibility_transition(false, true, ChId, Pid, Sid, SD, _Old, New, GId, VMap) when
    is_pid(Pid)
->
    guild_visibility_roles:dispatch_channel_create(ChId, Pid, New, GId),
    guild_visibility_roles:send_member_list_sync(Sid, SD, ChId, GId, New),
    dispatch_thread_list_sync_for_channel(ChId, maps:get(user_id, SD, undefined), Pid, GId, New),
    FinalMap = guild_visibility_roles:maybe_ensure_parent_category_visible(
        ChId, VMap, New, Pid, GId
    ),
    {FinalMap, New};
handle_visibility_transition(_, _, _ChId, _Pid, _Sid, _SD, _Old, New, _GId, VMap) ->
    {VMap, New}.

-spec guild_id(guild_state()) -> integer() | undefined.
guild_id(State) ->
    snowflake_id:parse_optional(maps:get(id, State, undefined)).

-spec dispatch_thread_list_sync_for_channel(
    channel_id(), integer() | undefined, pid(), integer(), guild_state()
) -> ok.
dispatch_thread_list_sync_for_channel(_ChId, undefined, _Pid, _GuildId, _State) ->
    ok;
dispatch_thread_list_sync_for_channel(ChId, UserId, Pid, GuildId, State) ->
    Data = guild_data_index:ensure_data_map(State),
    AllThreads = guild_data_index:thread_list(Data),
    ChannelThreads = lists:filter(
        fun(Thread) ->
            SafeThread = map_utils:ensure_map(Thread),
            case snowflake_id:parse_optional(maps:get(<<"thread_parent_channel_id">>, SafeThread, undefined)) of
                ChId -> true;
                _ -> false
            end
        end,
        AllThreads
    ),
    case ChannelThreads of
        [] ->
            ok;
        _ ->
            EventData = #{
                <<"guild_id">> => integer_to_binary(GuildId),
                <<"threads">> => ChannelThreads,
                <<"members">> => [],
                <<"id">> => integer_to_binary(UserId)
            },
            gateway_dispatch_relay:dispatch(Pid, thread_list_sync, EventData, GuildId),
            ok
    end.
