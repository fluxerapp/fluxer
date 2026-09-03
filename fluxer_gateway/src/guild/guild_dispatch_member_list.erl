%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_dispatch_member_list).
-typing([eqwalizer]).

-export([
    maybe_broadcast_member_list_update/4
]).

-type event() :: atom().
-type event_data() :: map().
-type guild_state() :: map().
-type user_id() :: integer().
-export_type([event/0, event_data/0, guild_state/0]).

-spec maybe_broadcast_member_list_update(event(), event_data(), guild_state(), guild_state()) ->
    guild_state().
maybe_broadcast_member_list_update(Event, EventData, OldState, UpdatedState) ->
    case guild_dispatch_config:is_member_list_updates_enabled(UpdatedState) of
        true ->
            dispatch_member_list_event(Event, EventData, OldState, UpdatedState);
        false ->
            UpdatedState
    end.

-spec dispatch_member_list_event(event(), event_data(), guild_state(), guild_state()) ->
    guild_state().
dispatch_member_list_event(guild_member_add, EventData, OldState, UpdatedState) ->
    broadcast_member_update(EventData, OldState, UpdatedState);
dispatch_member_list_event(guild_member_remove, EventData, OldState, UpdatedState) ->
    broadcast_member_update(EventData, OldState, UpdatedState);
dispatch_member_list_event(guild_member_update, EventData, OldState, UpdatedState) ->
    broadcast_member_update(EventData, OldState, UpdatedState);
dispatch_member_list_event(guild_role_create, _EventData, _OldState, UpdatedState) ->
    broadcast_all_updates(UpdatedState);
dispatch_member_list_event(guild_role_update, _EventData, _OldState, UpdatedState) ->
    broadcast_all_updates(UpdatedState);
dispatch_member_list_event(guild_role_update_bulk, _EventData, _OldState, UpdatedState) ->
    broadcast_all_updates(UpdatedState);
dispatch_member_list_event(guild_role_delete, _EventData, _OldState, UpdatedState) ->
    broadcast_all_updates(UpdatedState);
dispatch_member_list_event(channel_create, _EventData, _OldState, UpdatedState) ->
    broadcast_all_updates(UpdatedState);
dispatch_member_list_event(channel_delete, _EventData, _OldState, UpdatedState) ->
    broadcast_all_updates(UpdatedState);
dispatch_member_list_event(channel_update, EventData, OldState, UpdatedState) ->
    broadcast_channel_update(EventData, OldState, UpdatedState);
dispatch_member_list_event(channel_update_bulk, EventData, OldState, UpdatedState) ->
    Channels = maps:get(<<"channels">>, EventData, []),
    lists:foldl(
        fun(Channel, AccState) -> broadcast_channel_update(Channel, OldState, AccState) end,
        UpdatedState,
        Channels
    );
dispatch_member_list_event(_Event, _FinalData, _OldState, UpdatedState) ->
    UpdatedState.

-spec broadcast_member_update(event_data(), guild_state(), guild_state()) -> guild_state().
broadcast_member_update(EventData, OldState, UpdatedState) ->
    UserId = extract_user_id_from_event(EventData),
    case UserId of
        undefined ->
            UpdatedState;
        _ ->
            {ok, NewState} = guild_member_list:broadcast_member_list_updates(
                UserId, OldState, UpdatedState
            ),
            NewState
    end.

-spec broadcast_all_updates(guild_state()) -> guild_state().
broadcast_all_updates(UpdatedState) ->
    {ok, NewState} = guild_member_list:broadcast_all_member_list_updates(UpdatedState),
    NewState.

-spec broadcast_channel_update(event_data(), guild_state(), guild_state()) -> guild_state().
broadcast_channel_update(EventData, OldState, UpdatedState) ->
    ChannelIdBin = maps:get(<<"id">>, EventData, undefined),
    case guild_dispatch_decorate:parse_snowflake(<<"id">>, ChannelIdBin) of
        undefined ->
            UpdatedState;
        ChannelId ->
            broadcast_channel_update_for_id(ChannelId, OldState, UpdatedState)
    end.

%% guild_state:post_update_channel/2 has already rebuilt this channel's engine
%% earlier in the same dispatch, through
%% guild_member_list_write:rebuild_channels_for_permission_change/2. Rebuilding
%% again here is a second full O(members) build over the same state, so when the
%% engine already exists and nothing in the window can have changed what a
%% rebuild would see, queue the sync and skip the rebuild.
-spec broadcast_channel_update_for_id(integer(), guild_state(), guild_state()) -> guild_state().
broadcast_channel_update_for_id(ChannelId, OldState, UpdatedState) ->
    case rebuild_is_redundant(ChannelId, OldState, UpdatedState) of
        true ->
            sync_channel_list_without_rebuild(ChannelId, UpdatedState);
        false ->
            {ok, NewState} = guild_member_list:broadcast_member_list_updates_for_channel(
                ChannelId, UpdatedState
            ),
            NewState
    end.

%% The engine must already exist, because the earlier pass only rebuilds
%% ALREADY-LOADED engines while this pass would build a missing one. Virtual
%% channel access is compared across the whole dispatch, which is a conservative
%% superset of the window between the two rebuilds: guild_visibility can grant a
%% user virtual access to a channel they just lost permission for, and that does
%% change what a rebuild sees.
-spec rebuild_is_redundant(integer(), guild_state(), guild_state()) -> boolean().
rebuild_is_redundant(ChannelId, OldState, UpdatedState) ->
    guild_member_list_channel_engine:ref(integer_to_binary(ChannelId), UpdatedState) =/=
        undefined andalso
        maps:get(virtual_channel_access, OldState, #{}) =:=
            maps:get(virtual_channel_access, UpdatedState, #{}).

%% Mirrors guild_member_list_write:broadcast_list_by_id/4 with the rebuild
%% removed: a list nobody is subscribed to is left alone, otherwise the sync is
%% queued exactly as that function would have queued it.
-spec sync_channel_list_without_rebuild(integer(), guild_state()) -> guild_state().
sync_channel_list_without_rebuild(ChannelId, State) ->
    ListId = integer_to_binary(ChannelId),
    SubsTab = maps:get(member_list_subscriptions, State),
    case map_size(guild_member_list_subs:get_list_subs(ListId, SubsTab)) of
        0 -> State;
        _ -> guild_member_list_sync_batch:queue_list_sync(ListId, State)
    end.

-spec extract_user_id_from_event(event_data()) -> user_id() | undefined.
extract_user_id_from_event(EventData) ->
    MUser = maps:get(<<"user">>, EventData, #{}),
    guild_dispatch_decorate:parse_snowflake(
        <<"user.id">>, maps:get(<<"id">>, MUser, undefined)
    ).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

extract_user_id_from_event_test() ->
    EventData = #{<<"user">> => #{<<"id">> => <<"42">>}},
    ?assertEqual(42, extract_user_id_from_event(EventData)).

extract_user_id_from_event_missing_test() ->
    ?assertEqual(undefined, extract_user_id_from_event(#{})).

extract_user_id_from_event_invalid_test() ->
    EventData = #{<<"user">> => #{<<"id">> => <<"invalid">>}},
    ?assertEqual(undefined, extract_user_id_from_event(EventData)).

-endif.
