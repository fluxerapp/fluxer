%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_write).
-typing([eqwalizer]).

-export([
    broadcast_member_list_updates/3,
    broadcast_member_list_updates/5,
    broadcast_all_member_list_updates/1,
    broadcast_member_list_updates_for_channel/2,
    broadcast_channel_engine_connection_change/2,
    flush_pending_member_list_syncs/1,
    resync_hoisted_member_lists/1,
    rebuild_channels_for_permission_change/2,
    resync_channels_for_permission_change/2,
    resync_all_channels_for_permission_change/1
]).

-type guild_state() :: map().
-type list_id() :: binary().
-type user_id() :: integer().
-type channel_id() :: integer().

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
    guild_member_list_write_context:with_guild_id(UpdatedState, fun(_GuildId) ->
        OldMember = find_member_in_state_data(UserId, OldState),
        NewMember = find_member_in_state_data(UserId, UpdatedState),
        State1 = dispatch_presence_delta(
            UserId, OldMember, NewMember, OldPresence, NewPresence, UpdatedState
        ),
        {ok, State1}
    end).

-spec dispatch_presence_delta(
    user_id(),
    map() | undefined,
    map() | undefined,
    map() | undefined,
    map() | undefined,
    guild_state()
) -> guild_state().
dispatch_presence_delta(UserId, OldMember, NewMember, OldPresence, NewPresence, State) ->
    case presence_delta_is_inert(OldPresence, NewPresence, OldMember, NewMember) of
        true ->
            State;
        false ->
            SubsTab = maps:get(member_list_subscriptions, State),
            dispatch_user_change_to_subscribed_lists(
                UserId, OldMember, NewMember, SubsTab, State
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
    member_list_presence_fields(OldPresence) =:= member_list_presence_fields(NewPresence);
presence_delta_is_inert(_OldPresence, _NewPresence, _OldMember, _NewMember) ->
    false.

-spec member_list_presence_fields(map()) -> {binary(), term()}.
member_list_presence_fields(Presence) ->
    {
        maps:get(<<"status">>, Presence, <<"offline">>),
        maps:get(<<"custom_status">>, Presence, null)
    }.

-spec find_member_in_state_data(user_id(), guild_state()) -> map() | undefined.
find_member_in_state_data(UserId, State) ->
    guild_data_index:get_member(UserId, maps:get(data, State, #{})).

-spec broadcast_all_member_list_updates(guild_state()) -> {ok, guild_state()}.
broadcast_all_member_list_updates(State) ->
    guild_member_list_write_context:with_guild_id(State, fun(_GuildId) ->
        SubsTab = maps:get(member_list_subscriptions, State),
        Rebuilt = rebuild_subscribed_channel_lists(State, SubsTab),
        {ok, guild_member_list_sync_batch:queue_subscribed_list_syncs(Rebuilt, SubsTab)}
    end).

-spec resync_channels_for_permission_change([channel_id()], guild_state()) -> guild_state().
resync_channels_for_permission_change(ChannelIds, State) ->
    State1 = rebuild_channels_for_permission_change(ChannelIds, State),
    lists:foldl(
        fun(ChannelId, Acc) ->
            {ok, Next} = broadcast_member_list_updates_for_channel(ChannelId, Acc),
            Next
        end,
        State1,
        ChannelIds
    ).

-spec rebuild_channels_for_permission_change([channel_id()], guild_state()) -> guild_state().
rebuild_channels_for_permission_change(ChannelIds, State) ->
    guild_member_list_channel_engine:rebuild_channels(ChannelIds, State).

-spec resync_all_channels_for_permission_change(guild_state()) -> guild_state().
resync_all_channels_for_permission_change(State) ->
    State1 = guild_member_list_channel_engine:rebuild_all(State),
    resync_hoisted_member_lists(State1).

-spec resync_hoisted_member_lists(guild_state()) -> guild_state().
resync_hoisted_member_lists(State) ->
    case maps:get(member_list_subscriptions, State, undefined) of
        undefined ->
            State;
        SubsTab ->
            resync_hoisted_member_lists(State, SubsTab)
    end.

-spec resync_hoisted_member_lists(guild_state(), ets:table()) -> guild_state().
resync_hoisted_member_lists(State, SubsTab) ->
    guild_member_list_sync_batch:queue_subscribed_list_syncs(State, SubsTab).

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
    case maps:get(member_list_subscriptions, State, undefined) of
        undefined ->
            State;
        SubsTab ->
            broadcast_channel_engine_connection_change(UserId, State, SubsTab)
    end.

-spec broadcast_channel_engine_connection_change(user_id(), guild_state(), ets:table()) ->
    guild_state().
broadcast_channel_engine_connection_change(UserId, State, SubsTab) ->
    {ok, NewState} = guild_member_list_write_context:with_guild_id(State, fun(GuildId) ->
        {ok, fold_connection_change_lists(GuildId, UserId, State, SubsTab)}
    end),
    NewState.

-spec fold_connection_change_lists(integer(), user_id(), guild_state(), ets:table()) ->
    guild_state().
fold_connection_change_lists(GuildId, UserId, State, SubsTab) ->
    Sessions = maps:get(sessions, State, #{}),
    guild_member_list_subs:fold_list_ids(
        fun(ListId, AccState) ->
            sync_connection_change_for_list(
                GuildId, UserId, ListId, SubsTab, Sessions, AccState
            )
        end,
        State,
        SubsTab
    ).

-spec dispatch_user_change_to_subscribed_lists(
    user_id(),
    map() | undefined,
    map() | undefined,
    ets:table(),
    guild_state()
) -> guild_state().
dispatch_user_change_to_subscribed_lists(
    UserId, OldMember, NewMember, SubsTab, State
) ->
    guild_member_list_subs:fold_list_ids(
        fun(ListId, AccState) ->
            dispatch_user_change_to_list(
                UserId, OldMember, NewMember, ListId, SubsTab, AccState
            )
        end,
        State,
        SubsTab
    ).

-spec dispatch_user_change_to_list(
    user_id(),
    map() | undefined,
    map() | undefined,
    list_id(),
    ets:table(),
    guild_state()
) -> guild_state().
dispatch_user_change_to_list(
    UserId, OldMember, NewMember, ListId, SubsTab, State
) ->
    State1 = apply_user_change_to_channel_store(UserId, ListId, OldMember, NewMember, State),
    queue_list_sync_if_subscribed(ListId, SubsTab, State1).

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

-spec sync_connection_change_for_list(
    integer(),
    user_id(),
    list_id(),
    ets:table(),
    map(),
    guild_state()
) -> guild_state().
sync_connection_change_for_list(_GuildId, _UserId, ListId, SubsTab, _Sessions, State) ->
    case guild_member_list_channel_engine:is_engine_list(ListId, State) of
        true ->
            queue_list_sync_if_subscribed(ListId, SubsTab, State);
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
    guild_member_list_subs:fold_lists(
        fun(ListId, _Subs, AccState) ->
            rebuild_channel_store(ListId, AccState)
        end,
        State,
        SubsTab
    ).

-spec rebuild_channel_store(list_id(), guild_state()) -> guild_state().
rebuild_channel_store(ListId, State) ->
    case guild_member_list_channel_engine:is_engine_list(ListId, State) of
        true -> guild_member_list_channel_engine:rebuild(ListId, State);
        false -> State
    end.

-spec queue_list_sync_if_subscribed(list_id(), ets:table(), guild_state()) -> guild_state().
queue_list_sync_if_subscribed(ListId, SubsTab, State) ->
    case guild_member_list_subs:has_subs(ListId, SubsTab) of
        false -> State;
        true -> guild_member_list_sync_batch:queue_list_sync(ListId, State)
    end.

-spec flush_pending_member_list_syncs(guild_state()) -> guild_state().
flush_pending_member_list_syncs(State) ->
    guild_member_list_sync_batch:flush_pending_syncs(State).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

apply_user_change_to_channel_store_updates_engine_test() ->
    Ref = guild_member_list_engine:new(),
    State = #{channel_member_list_engines => #{<<"123">> => Ref}, member_presence => #{}},
    Member = #{<<"user">> => #{<<"id">> => <<"2">>}, <<"roles">> => []},
    State1 = apply_user_change_to_channel_store(2, <<"123">>, undefined, Member, State),
    ?assert(maps:is_key(<<"123">>, maps:get(channel_member_list_engines, State1))).

sync_connection_change_ignores_zero_list_test() ->
    SubsTab = guild_member_list_subs:new(),
    ?assertEqual(
        #{},
        sync_connection_change_for_list(1, 2, <<"0">>, SubsTab, #{}, #{})
    ),
    guild_member_list_subs:destroy(SubsTab).

-endif.
