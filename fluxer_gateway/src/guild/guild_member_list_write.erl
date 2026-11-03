%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_write).
-typing([eqwalizer]).

-export([
    broadcast_member_list_updates/3,
    broadcast_member_list_updates/5,
    queue_member_list_updates/5,
    broadcast_all_member_list_updates/1,
    broadcast_member_list_updates_for_channel/2,
    broadcast_channel_engine_connection_change/2,
    queue_synced_connection_change/2,
    presence_change_resyncs_lists/2,
    flush_pending_member_list_syncs/1,
    rebuild_channels_for_permission_change/2
]).

-type guild_state() :: map().
-type list_id() :: binary().
-type user_id() :: integer().
-type channel_id() :: integer().
-type absence() :: guild_member_list_absence:absence().
-type pending_mark() :: guild_member_list_sync_batch:pending_mark().
-type list_sync() :: immediate | deferred.

-define(MAX_MEMBER_LIST_SYNC_SKIPPED_ABSENT, 1000000000).

-export_type([guild_state/0, list_id/0, user_id/0, channel_id/0]).

-spec broadcast_member_list_updates(user_id() | undefined, guild_state(), guild_state()) ->
    {ok, guild_state()}.
broadcast_member_list_updates(undefined, _OldState, UpdatedState) ->
    {ok, UpdatedState};
broadcast_member_list_updates(UserId, OldState, UpdatedState) ->
    broadcast_member_list_updates(UserId, OldState, UpdatedState, undefined, undefined).

-spec broadcast_member_list_updates(
    user_id() | undefined,
    guild_state(),
    guild_state(),
    map() | undefined,
    map() | undefined
) -> {ok, guild_state()}.
broadcast_member_list_updates(
    undefined,
    _OldState,
    UpdatedState,
    _OldPresence,
    _NewPresence
) ->
    {ok, UpdatedState};
broadcast_member_list_updates(UserId, OldState, UpdatedState, OldPresence, NewPresence) ->
    member_list_updates(immediate, UserId, OldState, UpdatedState, OldPresence, NewPresence).

-spec queue_member_list_updates(
    user_id() | undefined,
    guild_state(),
    guild_state(),
    map() | undefined,
    map() | undefined
) -> {ok, guild_state()}.
queue_member_list_updates(undefined, _OldState, UpdatedState, _OldPresence, _NewPresence) ->
    {ok, UpdatedState};
queue_member_list_updates(UserId, OldState, UpdatedState, OldPresence, NewPresence) ->
    member_list_updates(deferred, UserId, OldState, UpdatedState, OldPresence, NewPresence).

-spec member_list_updates(
    list_sync(),
    user_id(),
    guild_state(),
    guild_state(),
    map() | undefined,
    map() | undefined
) -> {ok, guild_state()}.
member_list_updates(ListSync, UserId, OldState, UpdatedState, OldPresence, NewPresence) ->
    guild_member_list_write_context:with_guild_id(UpdatedState, fun(_GuildId) ->
        OldMember = find_member_in_state_data(UserId, OldState),
        NewMember = find_member_in_state_data(UserId, UpdatedState),
        State1 = dispatch_presence_delta(
            UserId, OldMember, NewMember, OldPresence, NewPresence, ListSync, UpdatedState
        ),
        {ok, State1}
    end).

-spec dispatch_presence_delta(
    user_id(),
    map() | undefined,
    map() | undefined,
    map() | undefined,
    map() | undefined,
    list_sync(),
    guild_state()
) -> guild_state().
dispatch_presence_delta(
    UserId, OldMember, NewMember, OldPresence, NewPresence, ListSync, State
) ->
    case presence_delta_is_inert(OldPresence, NewPresence, OldMember, NewMember) of
        true ->
            guild_member_list_sync_batch:invalidate_synced(UserId, State);
        false ->
            SubsTab = maps:get(member_list_subscriptions, State),
            dispatch_user_change_to_subscribed_lists(
                UserId, OldMember, NewMember, SubsTab, ListSync, State
            )
    end.

-spec presence_delta_is_inert(
    map() | undefined,
    map() | undefined,
    map() | undefined,
    map() | undefined
) -> boolean().
presence_delta_is_inert(OldPresence, NewPresence, Member, Member) when
    is_map(OldPresence), is_map(NewPresence), is_map(Member)
->
    not presence_change_resyncs_lists(OldPresence, NewPresence);
presence_delta_is_inert(_OldPresence, _NewPresence, _OldMember, _NewMember) ->
    false.

-spec presence_change_resyncs_lists(map(), map()) -> boolean().
presence_change_resyncs_lists(OldPresence, NewPresence) ->
    member_list_presence_fields(OldPresence) =/= member_list_presence_fields(NewPresence).

-spec member_list_presence_fields(map()) -> {binary(), term(), term()}.
member_list_presence_fields(Presence) ->
    {
        maps:get(<<"status">>, Presence, <<"offline">>),
        maps:get(<<"custom_status">>, Presence, null),
        maps:get(<<"mobile">>, Presence, false)
    }.

-spec find_member_in_state_data(user_id(), guild_state()) -> map() | undefined.
find_member_in_state_data(UserId, State) ->
    guild_data_index_members:get_member(UserId, maps:get(data, State, #{})).

-spec broadcast_all_member_list_updates(guild_state()) -> {ok, guild_state()}.
broadcast_all_member_list_updates(State) ->
    guild_member_list_write_context:with_guild_id(State, fun(_GuildId) ->
        SubsTab = maps:get(member_list_subscriptions, State),
        Rebuilt = rebuild_subscribed_channel_lists(State, SubsTab),
        {ok, guild_member_list_sync_batch:queue_subscribed_list_syncs(Rebuilt, SubsTab)}
    end).

-spec rebuild_channels_for_permission_change([channel_id()], guild_state()) -> guild_state().
rebuild_channels_for_permission_change(ChannelIds, State) ->
    guild_member_list_channel_engine:rebuild_channels(ChannelIds, State).

-spec broadcast_member_list_updates_for_channel(channel_id(), guild_state()) ->
    {ok, guild_state()}.
broadcast_member_list_updates_for_channel(ChannelId, State) when
    is_integer(ChannelId), ChannelId > 0
->
    guild_member_list_write_context:with_guild_id(State, fun(GuildId) ->
        broadcast_channel_with_guild_id(GuildId, ChannelId, State)
    end);
broadcast_member_list_updates_for_channel(_ChannelId, State) ->
    {ok, State}.

-spec broadcast_channel_with_guild_id(integer(), channel_id(), guild_state()) ->
    {ok, guild_state()}.
broadcast_channel_with_guild_id(GuildId, ChannelId, State) ->
    case guild_member_list:calculate_list_id(ChannelId, State) of
        undefined -> {ok, State};
        ListId -> broadcast_list_by_id(GuildId, ChannelId, ListId, State)
    end.

-spec broadcast_channel_engine_connection_change(user_id(), guild_state()) -> guild_state().
broadcast_channel_engine_connection_change(UserId, State) ->
    queue_connection_change(UserId, true, State).

-spec queue_synced_connection_change(user_id(), guild_state()) -> guild_state().
queue_synced_connection_change(UserId, State) ->
    queue_connection_change(UserId, synced, State).

-spec queue_connection_change(user_id(), pending_mark(), guild_state()) -> guild_state().
queue_connection_change(UserId, Mark, State) ->
    case maps:get(member_list_subscriptions, State, undefined) of
        undefined ->
            State;
        SubsTab ->
            queue_connection_change(UserId, Mark, State, SubsTab)
    end.

-spec queue_connection_change(user_id(), pending_mark(), guild_state(), ets:table()) ->
    guild_state().
queue_connection_change(UserId, Mark, State, SubsTab) ->
    {ok, NewState} = guild_member_list_write_context:with_guild_id(State, fun(GuildId) ->
        {ok, fold_connection_change_lists(GuildId, UserId, Mark, State, SubsTab)}
    end),
    NewState.

-spec fold_connection_change_lists(
    integer(), user_id(), pending_mark(), guild_state(), ets:table()
) -> guild_state().
fold_connection_change_lists(GuildId, UserId, Mark, State, SubsTab) ->
    Sessions = maps:get(sessions, State, #{}),
    lists:foldl(
        fun(ListId, AccState) ->
            sync_connection_change_for_subscribed_list(
                GuildId, UserId, ListId, Mark, Sessions, AccState
            )
        end,
        State,
        guild_member_list_subs:list_ids(SubsTab)
    ).

-spec dispatch_user_change_to_subscribed_lists(
    user_id(),
    map() | undefined,
    map() | undefined,
    ets:table(),
    list_sync(),
    guild_state()
) -> guild_state().
dispatch_user_change_to_subscribed_lists(
    UserId, OldMember, NewMember, SubsTab, ListSync, State
) ->
    guild_member_list_read:with_member_item_memo(fun() ->
        fold_user_change(
            UserId,
            {OldMember, NewMember},
            guild_member_list_subs:list_ids(SubsTab),
            ListSync,
            State
        )
    end).

-spec fold_user_change(
    user_id(), {map() | undefined, map() | undefined}, [list_id()], list_sync(), guild_state()
) -> guild_state().
fold_user_change(UserId, {OldMember, NewMember}, ListIds, ListSync, State) ->
    lists:foldl(
        fun(ListId, AccState) ->
            dispatch_user_change_to_subscribed_list(
                UserId, OldMember, NewMember, ListId, ListSync, AccState
            )
        end,
        State,
        ListIds
    ).

-spec dispatch_user_change_to_subscribed_list(
    user_id(),
    map() | undefined,
    map() | undefined,
    list_id(),
    list_sync(),
    guild_state()
) -> guild_state().
dispatch_user_change_to_subscribed_list(UserId, OldMember, NewMember, ListId, ListSync, State) ->
    Absence = user_change_absence(UserId, ListId, OldMember, NewMember, State),
    State1 = apply_user_change_to_channel_store(UserId, ListId, OldMember, NewMember, State),
    case guild_member_list_absence:sync_body_unchanged(UserId, ListId, Absence, State1) of
        true -> record_member_list_sync_skipped_absent(State1);
        false -> queue_user_change_sync(ListSync, ListId, State1)
    end.

-spec queue_user_change_sync(list_sync(), list_id(), guild_state()) -> guild_state().
queue_user_change_sync(immediate, ListId, State) ->
    guild_member_list_sync_batch:queue_list_sync(ListId, State);
queue_user_change_sync(deferred, ListId, State) ->
    guild_member_list_sync_batch:queue_connection_sync(ListId, true, State).

-spec user_change_absence(
    user_id(), list_id(), map() | undefined, map() | undefined, guild_state()
) -> absence().
user_change_absence(UserId, ListId, Member, Member, State) when is_map(Member) ->
    guild_member_list_absence:of_list(UserId, ListId, State);
user_change_absence(_UserId, _ListId, _OldMember, _NewMember, _State) ->
    present.

-spec apply_user_change_to_channel_store(
    user_id(), list_id(), map() | undefined, map() | undefined, guild_state()
) -> guild_state().
apply_user_change_to_channel_store(UserId, ListId, OldMember, NewMember, State) ->
    case guild_member_list_channel_engine:is_engine_list(ListId, State) of
        true ->
            State1 = guild_member_list_channel_engine:ensure(ListId, State),
            ok = apply_channel_member_change(UserId, ListId, OldMember, NewMember, State1),
            State1;
        false ->
            State
    end.

-spec apply_channel_member_change(
    user_id(), list_id(), map() | undefined, map() | undefined, guild_state()
) -> ok.
apply_channel_member_change(UserId, ListId, _OldMember, undefined, State) ->
    guild_member_list_channel_engine:remove_user(UserId, ListId, State);
apply_channel_member_change(UserId, ListId, OldMember, NewMember, State) when
    OldMember =/= NewMember
->
    guild_member_list_channel_engine:update_user(UserId, ListId, State);
apply_channel_member_change(_UserId, _ListId, _OldMember, _NewMember, _State) ->
    ok.

-spec sync_connection_change_for_subscribed_list(
    integer(), user_id(), list_id(), pending_mark(), map(), guild_state()
) -> guild_state().
sync_connection_change_for_subscribed_list(_GuildId, UserId, ListId, Mark, _Sessions, State) ->
    case guild_member_list_channel_engine:is_engine_list(ListId, State) of
        true ->
            queue_connection_list_sync_unless_absent(UserId, ListId, Mark, State);
        false ->
            State
    end.

-spec broadcast_list_by_id(integer(), channel_id(), list_id(), guild_state()) ->
    {ok, guild_state()}.
broadcast_list_by_id(_GuildId, _ChannelId, ListId, State) ->
    SubsTab = maps:get(member_list_subscriptions, State),
    ListSubs = guild_member_list_subs:get_list_subs(ListId, SubsTab),
    case map_size(ListSubs) of
        0 ->
            {ok, State};
        _ ->
            State1 = rebuild_channel_store(ListId, State),
            {ok, guild_member_list_sync_batch:queue_list_sync(ListId, State1)}
    end.

-spec rebuild_subscribed_channel_lists(guild_state(), ets:table()) -> guild_state().
rebuild_subscribed_channel_lists(State, SubsTab) ->
    lists:foldl(
        fun rebuild_channel_store/2,
        State,
        guild_member_list_subs:list_ids(SubsTab)
    ).

-spec rebuild_channel_store(list_id(), guild_state()) -> guild_state().
rebuild_channel_store(ListId, State) ->
    case guild_member_list_channel_engine:is_engine_list(ListId, State) of
        true -> guild_member_list_channel_engine:rebuild(ListId, State);
        false -> State
    end.

-spec queue_connection_list_sync_unless_absent(
    user_id(), list_id(), pending_mark(), guild_state()
) -> guild_state().
queue_connection_list_sync_unless_absent(UserId, ListId, Mark, State) ->
    case guild_member_list_absence:of_list(UserId, ListId, State) of
        {absent, _Ref} -> record_member_list_sync_skipped_absent(State);
        present -> guild_member_list_sync_batch:queue_connection_sync(ListId, Mark, State)
    end.

-spec record_member_list_sync_skipped_absent(guild_state()) -> guild_state().
record_member_list_sync_skipped_absent(State) ->
    case maps:get(member_list_sync_skipped_absent, State, 0) of
        Count when
            is_integer(Count), Count >= 0, Count < ?MAX_MEMBER_LIST_SYNC_SKIPPED_ABSENT
        ->
            State#{member_list_sync_skipped_absent => Count + 1};
        ?MAX_MEMBER_LIST_SYNC_SKIPPED_ABSENT ->
            State;
        Invalid ->
            erlang:error({invalid_member_list_sync_skipped_absent, Invalid})
    end.

-spec flush_pending_member_list_syncs(guild_state()) -> guild_state().
flush_pending_member_list_syncs(State) ->
    guild_member_list_sync_batch:flush_pending_syncs(State).
