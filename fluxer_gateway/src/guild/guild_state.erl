%% Copyright (C) 2026 Fluxer Contributors
%%
%% This file is part of Fluxer.
%%
%% Fluxer is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Fluxer is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

-module(guild_state).

-export([update_state/3]).

-type guild_state() :: map().
-type guild_data() :: map().
-type event() :: atom().
-type event_data() :: map().
-type user_id() :: integer().
-type role_id() :: binary().

-spec update_state(event(), event_data(), guild_state()) -> guild_state().
update_state(Event, EventData, State) ->
    StateWithUpdatedUser0 = guild_user_data:maybe_update_cached_user_data(Event, EventData, State),
    Data0 = maps:get(data, StateWithUpdatedUser0),
    Data = guild_data_index:normalize_data(Data0),
    StateWithUpdatedUser = maps:put(data, Data, StateWithUpdatedUser0),
    UpdatedData = update_data_for_event(Event, EventData, Data, State),
    UpdatedState = maps:put(data, UpdatedData, StateWithUpdatedUser),
    handle_post_update(Event, EventData, StateWithUpdatedUser, UpdatedState).

-spec update_data_for_event(event(), event_data(), guild_data(), guild_state()) -> guild_data().
update_data_for_event(guild_update, EventData, Data, _State) ->
    handle_guild_update(EventData, Data);
update_data_for_event(guild_member_add, EventData, Data, _State) ->
    handle_member_add(EventData, Data);
update_data_for_event(guild_member_update, EventData, Data, _State) ->
    handle_member_update(EventData, Data);
update_data_for_event(guild_member_remove, EventData, Data, State) ->
    handle_member_remove(EventData, Data, State);
update_data_for_event(guild_role_create, EventData, Data, _State) ->
    handle_role_create(EventData, Data);
update_data_for_event(guild_role_update, EventData, Data, _State) ->
    handle_role_update(EventData, Data);
update_data_for_event(guild_role_update_bulk, EventData, Data, _State) ->
    handle_role_update_bulk(EventData, Data);
update_data_for_event(guild_role_delete, EventData, Data, _State) ->
    handle_role_delete(EventData, Data);
update_data_for_event(channel_create, EventData, Data, _State) ->
    handle_channel_create(EventData, Data);
update_data_for_event(channel_update, EventData, Data, _State) ->
    handle_channel_update(EventData, Data);
update_data_for_event(channel_update_bulk, EventData, Data, _State) ->
    handle_channel_update_bulk(EventData, Data);
update_data_for_event(channel_delete, EventData, Data, _State) ->
    handle_channel_delete(EventData, Data);
update_data_for_event(message_create, EventData, Data, _State) ->
    handle_message_create(EventData, Data);
update_data_for_event(channel_pins_update, EventData, Data, _State) ->
    handle_channel_pins_update(EventData, Data);
update_data_for_event(guild_emojis_update, EventData, Data, _State) ->
    handle_emojis_update(EventData, Data);
update_data_for_event(guild_stickers_update, EventData, Data, _State) ->
    handle_stickers_update(EventData, Data);
update_data_for_event(_Event, _EventData, Data, _State) ->
    Data.

-spec handle_post_update(event(), event_data(), guild_state(), guild_state()) -> guild_state().
handle_post_update(guild_update, _EventData, StateWithUpdatedUser, UpdatedState) ->
    guild_availability:handle_unavailability_transition(StateWithUpdatedUser, UpdatedState);
handle_post_update(guild_member_add, _EventData, _StateWithUpdatedUser, UpdatedState) ->
    guild:update_counts(UpdatedState);
handle_post_update(guild_member_update, EventData, StateWithUpdatedUser, UpdatedState) ->
    UserId = extract_user_id(EventData),
    case is_integer(UserId) andalso UserId > 0 of
        true ->
            guild_visibility:compute_and_dispatch_visibility_changes_for_users(
                [UserId],
                StateWithUpdatedUser,
                UpdatedState
            );
        false ->
            guild_visibility:compute_and_dispatch_visibility_changes(
                StateWithUpdatedUser,
                UpdatedState
            )
    end;
handle_post_update(guild_role_create, _EventData, _StateWithUpdatedUser, UpdatedState) ->
    UpdatedState;
handle_post_update(guild_role_update, EventData, StateWithUpdatedUser, UpdatedState) ->
    recompute_visibility_for_roles(
        extract_role_ids_from_role_update(EventData),
        StateWithUpdatedUser,
        UpdatedState
    );
handle_post_update(guild_role_update_bulk, EventData, StateWithUpdatedUser, UpdatedState) ->
    recompute_visibility_for_roles(
        extract_role_ids_from_role_update_bulk(EventData),
        StateWithUpdatedUser,
        UpdatedState
    );
handle_post_update(guild_role_delete, EventData, StateWithUpdatedUser, UpdatedState) ->
    recompute_visibility_for_roles(
        extract_role_ids_from_role_delete(EventData),
        StateWithUpdatedUser,
        UpdatedState
    );
handle_post_update(channel_update, EventData, StateWithUpdatedUser, UpdatedState) ->
    ChannelIds = extract_channel_ids_from_channel_update(EventData),
    guild_visibility:compute_and_dispatch_visibility_changes_for_channels(
        ChannelIds,
        StateWithUpdatedUser,
        UpdatedState
    );
handle_post_update(channel_update_bulk, EventData, StateWithUpdatedUser, UpdatedState) ->
    ChannelIds = extract_channel_ids_from_channel_update_bulk(EventData),
    guild_visibility:compute_and_dispatch_visibility_changes_for_channels(
        ChannelIds,
        StateWithUpdatedUser,
        UpdatedState
    );
handle_post_update(guild_member_remove, EventData, _StateWithUpdatedUser, UpdatedState) ->
    UserId = extract_user_id(EventData),
    State1 = cleanup_removed_member_sessions(UpdatedState),
    State2 = maybe_disconnect_removed_member(UserId, State1),
    guild:update_counts(State2);
handle_post_update(Event, _EventData, StateWithUpdatedUser, UpdatedState) ->
    case needs_visibility_check(Event) of
        true ->
            guild_visibility:compute_and_dispatch_visibility_changes(
                StateWithUpdatedUser, UpdatedState
            );
        false ->
            UpdatedState
    end.

-spec maybe_disconnect_removed_member(user_id(), guild_state()) -> guild_state().
maybe_disconnect_removed_member(UserId, State) when is_integer(UserId), UserId > 0 ->
    {reply, _Result, NewState} =
        guild_voice_disconnect:disconnect_voice_user(
            #{user_id => UserId, connection_id => null},
            State
        ),
    NewState;
maybe_disconnect_removed_member(_, State) ->
    State.

-spec recompute_visibility_for_roles([integer()], guild_state(), guild_state()) -> guild_state().
recompute_visibility_for_roles(RoleIds, StateWithUpdatedUser, UpdatedState) ->
    GuildId = maps:get(id, UpdatedState, 0),
    case lists:any(fun(RoleId) -> RoleId =:= GuildId end, RoleIds) of
        true ->
            guild_visibility:compute_and_dispatch_visibility_changes(
                StateWithUpdatedUser,
                UpdatedState
            );
        false ->
            UserIds = affected_user_ids_for_roles(RoleIds, StateWithUpdatedUser, UpdatedState),
            case UserIds of
                [] ->
                    UpdatedState;
                _ ->
                    guild_visibility:compute_and_dispatch_visibility_changes_for_users(
                        UserIds,
                        StateWithUpdatedUser,
                        UpdatedState
                    )
            end
    end.

-spec affected_user_ids_for_roles([integer()], guild_state(), guild_state()) -> [user_id()].
affected_user_ids_for_roles(RoleIds, StateWithUpdatedUser, UpdatedState) ->
    OldData = maps:get(data, StateWithUpdatedUser, #{}),
    NewData = maps:get(data, UpdatedState, #{}),
    OldMemberRoleIndex = guild_data_index:member_role_index(OldData),
    NewMemberRoleIndex = guild_data_index:member_role_index(NewData),
    UserIdSet = lists:foldl(
        fun(RoleId, AccSet) ->
            OldUsers = maps:keys(maps:get(RoleId, OldMemberRoleIndex, #{})),
            NewUsers = maps:keys(maps:get(RoleId, NewMemberRoleIndex, #{})),
            RoleUsers = OldUsers ++ NewUsers,
            lists:foldl(
                fun(UserId, InnerSet) -> sets:add_element(UserId, InnerSet) end,
                AccSet,
                RoleUsers
            )
        end,
        sets:new(),
        lists:usort(RoleIds)
    ),
    sets:to_list(UserIdSet).

-spec extract_role_ids_from_role_update(event_data()) -> [integer()].
extract_role_ids_from_role_update(EventData) ->
    RoleData = maps:get(<<"role">>, EventData, #{}),
    case type_conv:to_integer(maps:get(<<"id">>, RoleData, undefined)) of
        undefined -> [];
        RoleId -> [RoleId]
    end.

-spec extract_role_ids_from_role_update_bulk(event_data()) -> [integer()].
extract_role_ids_from_role_update_bulk(EventData) ->
    Roles = maps:get(<<"roles">>, EventData, []),
    lists:filtermap(
        fun(RoleData) ->
            case type_conv:to_integer(maps:get(<<"id">>, RoleData, undefined)) of
                undefined ->
                    false;
                RoleId ->
                    {true, RoleId}
            end
        end,
        Roles
    ).

-spec extract_role_ids_from_role_delete(event_data()) -> [integer()].
extract_role_ids_from_role_delete(EventData) ->
    case type_conv:to_integer(maps:get(<<"role_id">>, EventData, undefined)) of
        undefined -> [];
        RoleId -> [RoleId]
    end.

-spec extract_channel_ids_from_channel_update(event_data()) -> [integer()].
extract_channel_ids_from_channel_update(EventData) ->
    case type_conv:to_integer(maps:get(<<"id">>, EventData, undefined)) of
        undefined -> [];
        ChannelId -> [ChannelId]
    end.

-spec extract_channel_ids_from_channel_update_bulk(event_data()) -> [integer()].
extract_channel_ids_from_channel_update_bulk(EventData) ->
    Channels = maps:get(<<"channels">>, EventData, []),
    lists:filtermap(
        fun(ChannelData) ->
            case type_conv:to_integer(maps:get(<<"id">>, ChannelData, undefined)) of
                undefined ->
                    false;
                ChannelId ->
                    {true, ChannelId}
            end
        end,
        Channels
    ).

-spec needs_visibility_check(event()) -> boolean().
needs_visibility_check(guild_role_create) -> true;
needs_visibility_check(guild_role_update) -> true;
needs_visibility_check(guild_role_update_bulk) -> true;
needs_visibility_check(guild_role_delete) -> true;
needs_visibility_check(guild_member_update) -> true;
needs_visibility_check(channel_update) -> true;
needs_visibility_check(channel_update_bulk) -> true;
needs_visibility_check(_) -> false.

-spec handle_guild_update(event_data(), guild_data()) -> guild_data().
handle_guild_update(EventData, Data) ->
    Guild = maps:get(<<"guild">>, Data),
    UpdatedGuild = maps:merge(Guild, EventData),
    maps:put(<<"guild">>, UpdatedGuild, Data).

-spec handle_member_add(event_data(), guild_data()) -> guild_data().
handle_member_add(EventData, Data) ->
    guild_data_index:put_member(EventData, Data).

-spec handle_member_update(event_data(), guild_data()) -> guild_data().
handle_member_update(EventData, Data) ->
    UserId = extract_user_id(EventData),
    Members = guild_data_index:member_map(Data),
    case maps:is_key(UserId, Members) of
        true ->
            guild_data_index:put_member(EventData, Data);
        false ->
            Data
    end.

-spec handle_member_remove(event_data(), guild_data(), guild_state()) -> guild_data().
handle_member_remove(EventData, Data, _State) ->
    UserId = extract_user_id(EventData),
    guild_data_index:remove_member(UserId, Data).

-spec cleanup_removed_member_sessions(guild_state()) -> guild_state().
cleanup_removed_member_sessions(State) ->
    Data = maps:get(data, State),
    MemberUserIds = sets:from_list(guild_data_index:member_ids(Data)),
    Sessions = maps:get(sessions, State, #{}),
    FilteredSessions = maps:filter(
        fun(_K, S) ->
            UserId = maps:get(user_id, S),
            sets:is_element(UserId, MemberUserIds)
        end,
        Sessions
    ),
    maps:put(sessions, FilteredSessions, State).

-spec extract_user_id(event_data()) -> user_id().
extract_user_id(EventData) ->
    MUser = maps:get(<<"user">>, EventData, #{}),
    utils:binary_to_integer_safe(maps:get(<<"id">>, MUser, <<"0">>)).

-spec handle_role_create(event_data(), guild_data()) -> guild_data().
handle_role_create(EventData, Data) ->
    Roles = guild_data_index:role_list(Data),
    RoleData = maps:get(<<"role">>, EventData),
    guild_data_index:put_roles(Roles ++ [RoleData], Data).

-spec handle_role_update(event_data(), guild_data()) -> guild_data().
handle_role_update(EventData, Data) ->
    Roles = guild_data_index:role_list(Data),
    RoleData = maps:get(<<"role">>, EventData),
    RoleId = maps:get(<<"id">>, RoleData),
    UpdatedRoles = replace_item_by_id(Roles, RoleId, RoleData),
    guild_data_index:put_roles(UpdatedRoles, Data).

-spec handle_role_update_bulk(event_data(), guild_data()) -> guild_data().
handle_role_update_bulk(EventData, Data) ->
    Roles = guild_data_index:role_list(Data),
    BulkRoles = maps:get(<<"roles">>, EventData, []),
    UpdatedRoles = bulk_update_items(Roles, BulkRoles),
    guild_data_index:put_roles(UpdatedRoles, Data).

-spec handle_role_delete(event_data(), guild_data()) -> guild_data().
handle_role_delete(EventData, Data) ->
    Roles = guild_data_index:role_list(Data),
    RoleId = maps:get(<<"role_id">>, EventData),
    FilteredRoles = remove_item_by_id(Roles, RoleId),
    Data1 = guild_data_index:put_roles(FilteredRoles, Data),
    Data2 = strip_role_from_members(RoleId, Data1),
    strip_role_from_channel_overwrites(RoleId, Data2).

-spec strip_role_from_members(role_id(), guild_data()) -> guild_data().
strip_role_from_members(RoleId, Data) ->
    RoleIdInt = utils:binary_to_integer_safe(RoleId),
    MemberRoleIndex = guild_data_index:member_role_index(Data),
    AffectedUsers = maps:keys(maps:get(RoleIdInt, MemberRoleIndex, #{})),
    lists:foldl(
        fun(UserId, AccData) ->
            case guild_data_index:get_member(UserId, AccData) of
                undefined ->
                    AccData;
                Member ->
                    MemberRoles = maps:get(<<"roles">>, Member, []),
                    FilteredRoles = lists:filter(
                        fun(R) ->
                            RInt = utils:binary_to_integer_safe(R),
                            RInt =/= RoleIdInt
                        end,
                        MemberRoles
                    ),
                    guild_data_index:put_member(maps:put(<<"roles">>, FilteredRoles, Member), AccData)
            end
        end,
        Data,
        AffectedUsers
    ).

-spec strip_role_from_channel_overwrites(role_id(), guild_data()) -> guild_data().
strip_role_from_channel_overwrites(RoleId, Data) ->
    Channels = guild_data_index:channel_list(Data),
    RoleIdInt = utils:binary_to_integer_safe(RoleId),
    UpdatedChannels = lists:map(
        fun
            (Channel) when is_map(Channel) ->
                Overwrites = maps:get(<<"permission_overwrites">>, Channel, []),
                FilteredOverwrites = lists:filter(
                    fun
                        (Overwrite) when is_map(Overwrite) ->
                            OverwriteType = maps:get(<<"type">>, Overwrite, 0),
                            OverwriteId = utils:binary_to_integer_safe(
                                maps:get(<<"id">>, Overwrite, <<"0">>)
                            ),
                            not (OverwriteType =:= 0 andalso OverwriteId =:= RoleIdInt);
                        (_) ->
                            true
                    end,
                    Overwrites
                ),
                maps:put(<<"permission_overwrites">>, FilteredOverwrites, Channel);
            (Channel) ->
                Channel
        end,
        Channels
    ),
    guild_data_index:put_channels(UpdatedChannels, Data).

-spec handle_channel_create(event_data(), guild_data()) -> guild_data().
handle_channel_create(EventData, Data) ->
    Channels = guild_data_index:channel_list(Data),
    guild_data_index:put_channels(Channels ++ [EventData], Data).

-spec handle_channel_update(event_data(), guild_data()) -> guild_data().
handle_channel_update(EventData, Data) ->
    Channels = guild_data_index:channel_list(Data),
    ChannelId = maps:get(<<"id">>, EventData),
    UpdatedChannels = replace_item_by_id(Channels, ChannelId, EventData),
    guild_data_index:put_channels(UpdatedChannels, Data).

-spec handle_channel_update_bulk(event_data(), guild_data()) -> guild_data().
handle_channel_update_bulk(EventData, Data) ->
    Channels = guild_data_index:channel_list(Data),
    BulkChannels = maps:get(<<"channels">>, EventData, []),
    UpdatedChannels = bulk_update_items(Channels, BulkChannels),
    guild_data_index:put_channels(UpdatedChannels, Data).

-spec handle_channel_delete(event_data(), guild_data()) -> guild_data().
handle_channel_delete(EventData, Data) ->
    Channels = guild_data_index:channel_list(Data),
    ChannelId = maps:get(<<"id">>, EventData),
    FilteredChannels = remove_item_by_id(Channels, ChannelId),
    guild_data_index:put_channels(FilteredChannels, Data).

-spec handle_message_create(event_data(), guild_data()) -> guild_data().
handle_message_create(EventData, Data) ->
    Channels = guild_data_index:channel_list(Data),
    ChannelId = maps:get(<<"channel_id">>, EventData),
    MessageId = maps:get(<<"id">>, EventData),
    UpdatedChannels = update_channel_field(Channels, ChannelId, <<"last_message_id">>, MessageId),
    guild_data_index:put_channels(UpdatedChannels, Data).

-spec handle_channel_pins_update(event_data(), guild_data()) -> guild_data().
handle_channel_pins_update(EventData, Data) ->
    Channels = guild_data_index:channel_list(Data),
    ChannelId = maps:get(<<"channel_id">>, EventData),
    LastPin = maps:get(<<"last_pin_timestamp">>, EventData),
    UpdatedChannels = update_channel_field(Channels, ChannelId, <<"last_pin_timestamp">>, LastPin),
    guild_data_index:put_channels(UpdatedChannels, Data).

-spec update_channel_field([map()], binary(), binary(), term()) -> [map()].
update_channel_field(Channels, ChannelId, Field, Value) ->
    lists:map(
        fun(C) when is_map(C) ->
            case maps:get(<<"id">>, C) =:= ChannelId of
                true -> maps:put(Field, Value, C);
                false -> C
            end
        end,
        Channels
    ).

-spec handle_emojis_update(event_data(), guild_data()) -> guild_data().
handle_emojis_update(EventData, Data) ->
    maps:put(<<"emojis">>, maps:get(<<"emojis">>, EventData, []), Data).

-spec handle_stickers_update(event_data(), guild_data()) -> guild_data().
handle_stickers_update(EventData, Data) ->
    maps:put(<<"stickers">>, maps:get(<<"stickers">>, EventData, []), Data).

-spec replace_item_by_id([map()], binary(), map()) -> [map()].
replace_item_by_id(Items, Id, NewItem) ->
    lists:map(
        fun(Item) when is_map(Item) ->
            case maps:get(<<"id">>, Item) of
                Id -> NewItem;
                _ -> Item
            end
        end,
        Items
    ).

-spec remove_item_by_id([map()], binary()) -> [map()].
remove_item_by_id(Items, Id) ->
    lists:filter(
        fun(Item) when is_map(Item) ->
            maps:get(<<"id">>, Item) =/= Id
        end,
        Items
    ).

-spec bulk_update_items([map()], [map()]) -> [map()].
bulk_update_items(Items, BulkItems) ->
    BulkMap = lists:foldl(
        fun
            (Item, Acc) when is_map(Item) ->
                case maps:get(<<"id">>, Item, undefined) of
                    undefined -> Acc;
                    ItemId -> maps:put(ItemId, Item, Acc)
                end;
            (_, Acc) ->
                Acc
        end,
        #{},
        BulkItems
    ),
    lists:map(
        fun
            (Item) when is_map(Item) ->
                ItemId = maps:get(<<"id">>, Item, undefined),
                case maps:get(ItemId, BulkMap, undefined) of
                    undefined -> Item;
                    UpdatedItem -> UpdatedItem
                end;
            (Item) ->
                Item
        end,
        Items
    ).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

handle_role_delete_strips_from_members_test() ->
    RoleIdToDelete = <<"200">>,
    Data = #{
        <<"roles">> => [
            #{<<"id">> => <<"100">>, <<"name">> => <<"Admin">>},
            #{<<"id">> => <<"200">>, <<"name">> => <<"Moderator">>}
        ],
        <<"members">> => #{
            1 => #{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => [<<"100">>, <<"200">>]},
            2 => #{<<"user">> => #{<<"id">> => <<"2">>}, <<"roles">> => [<<"200">>]},
            3 => #{<<"user">> => #{<<"id">> => <<"3">>}, <<"roles">> => [<<"100">>]}
        },
        <<"channels">> => []
    },
    EventData = #{<<"role_id">> => RoleIdToDelete},
    Result = handle_role_delete(EventData, Data),
    Members = maps:get(<<"members">>, Result),
    M1 = maps:get(1, Members),
    M2 = maps:get(2, Members),
    M3 = maps:get(3, Members),
    ?assertEqual([<<"100">>], maps:get(<<"roles">>, M1)),
    ?assertEqual([], maps:get(<<"roles">>, M2)),
    ?assertEqual([<<"100">>], maps:get(<<"roles">>, M3)).

handle_role_delete_strips_from_channel_overwrites_test() ->
    RoleIdToDelete = <<"200">>,
    Data = #{
        <<"roles">> => [
            #{<<"id">> => <<"100">>, <<"name">> => <<"Everyone">>},
            #{<<"id">> => <<"200">>, <<"name">> => <<"Moderator">>}
        ],
        <<"members">> => [],
        <<"channels">> => [
            #{
                <<"id">> => <<"500">>,
                <<"permission_overwrites">> => [
                    #{
                        <<"id">> => <<"100">>,
                        <<"type">> => 0,
                        <<"allow">> => <<"0">>,
                        <<"deny">> => <<"1024">>
                    },
                    #{
                        <<"id">> => <<"200">>,
                        <<"type">> => 0,
                        <<"allow">> => <<"1024">>,
                        <<"deny">> => <<"0">>
                    },
                    #{
                        <<"id">> => <<"1">>,
                        <<"type">> => 1,
                        <<"allow">> => <<"2048">>,
                        <<"deny">> => <<"0">>
                    }
                ]
            }
        ]
    },
    EventData = #{<<"role_id">> => RoleIdToDelete},
    Result = handle_role_delete(EventData, Data),
    Channels = maps:get(<<"channels">>, Result),
    [Ch1] = Channels,
    Ch1Overwrites = maps:get(<<"permission_overwrites">>, Ch1),
    ?assertEqual(2, length(Ch1Overwrites)).

handle_member_add_test() ->
    Data = #{<<"members">> => #{1 => #{<<"user">> => #{<<"id">> => <<"1">>}}}},
    EventData = #{<<"user">> => #{<<"id">> => <<"2">>}},
    Result = handle_member_add(EventData, Data),
    Members = maps:get(<<"members">>, Result),
    ?assertEqual(2, map_size(Members)).

handle_member_update_test() ->
    Data = #{
        <<"members">> => #{
            1 => #{<<"user">> => #{<<"id">> => <<"1">>}, <<"nick">> => <<"OldNick">>}
        }
    },
    EventData = #{<<"user">> => #{<<"id">> => <<"1">>}, <<"nick">> => <<"NewNick">>},
    Result = handle_member_update(EventData, Data),
    Members = maps:get(<<"members">>, Result),
    Member = maps:get(1, Members),
    ?assertEqual(<<"NewNick">>, maps:get(<<"nick">>, Member)).

handle_channel_create_test() ->
    Data = #{<<"channels">> => []},
    EventData = #{<<"id">> => <<"100">>, <<"name">> => <<"general">>},
    Result = handle_channel_create(EventData, Data),
    Channels = maps:get(<<"channels">>, Result),
    ?assertEqual(1, length(Channels)).

bulk_update_items_test() ->
    Items = [
        #{<<"id">> => <<"1">>, <<"value">> => <<"old1">>},
        #{<<"id">> => <<"2">>, <<"value">> => <<"old2">>}
    ],
    BulkItems = [
        #{<<"id">> => <<"1">>, <<"value">> => <<"new1">>}
    ],
    Result = bulk_update_items(Items, BulkItems),
    [Item1, Item2] = Result,
    ?assertEqual(<<"new1">>, maps:get(<<"value">>, Item1)),
    ?assertEqual(<<"old2">>, maps:get(<<"value">>, Item2)).

needs_visibility_check_test() ->
    ?assertEqual(true, needs_visibility_check(guild_role_update)),
    ?assertEqual(true, needs_visibility_check(channel_update)),
    ?assertEqual(false, needs_visibility_check(message_create)),
    ?assertEqual(false, needs_visibility_check(unknown_event)).

extract_channel_ids_from_channel_update_test() ->
    ?assertEqual([42], extract_channel_ids_from_channel_update(#{<<"id">> => <<"42">>})),
    ?assertEqual([], extract_channel_ids_from_channel_update(#{})).

extract_channel_ids_from_channel_update_bulk_test() ->
    EventData = #{
        <<"channels">> => [
            #{<<"id">> => <<"10">>},
            #{<<"id">> => <<"11">>},
            #{<<"name">> => <<"missing_id">>}
        ]
    },
    ?assertEqual([10, 11], extract_channel_ids_from_channel_update_bulk(EventData)).

guild_member_remove_disconnects_voice_test() ->
    Self = self(),
    TestFun = fun(GuildId, ChannelId, UserId, ConnectionId) ->
        Self ! {force_disconnect, GuildId, ChannelId, UserId, ConnectionId},
        {ok, #{success => true}}
    end,
    GuildId = 42,
    UserId = 5,
    ChannelId = 20,
    Data = #{
        <<"guild">> => #{<<"owner_id">> => <<"999">>},
        <<"roles">> => [
            #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => <<"0">>}
        ],
        <<"members">> => #{
            UserId => #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}, <<"roles">> => []}
        },
        <<"channels">> => [#{<<"id">> => integer_to_binary(ChannelId)}]
    },
    VoiceStates = #{
        <<"conn">> => #{
            <<"user_id">> => integer_to_binary(UserId),
            <<"guild_id">> => integer_to_binary(GuildId),
            <<"channel_id">> => integer_to_binary(ChannelId),
            <<"connection_id">> => <<"conn">>
        }
    },
    Sessions = #{<<"s1">> => #{user_id => UserId, pid => self()}},
    State = #{
        id => GuildId,
        data => Data,
        voice_states => VoiceStates,
        sessions => Sessions,
        test_force_disconnect_fun => TestFun
    },
    EventData = #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}},
    UpdatedState = update_state(guild_member_remove, EventData, State),
    ?assertEqual(#{}, maps:get(voice_states, UpdatedState)),
    ?assertEqual(#{}, maps:get(sessions, UpdatedState, #{})),
    receive
        {force_disconnect, GuildId, ChannelId, UserId, <<"conn">>} -> ok
    after 200 ->
        ?assert(false)
    end.

guild_update_syncs_unavailability_cache_test() ->
    GuildId = 420042,
    CleanupState = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"features">> => []}
        }
    },
    _ = guild_availability:update_unavailability_cache_for_state(CleanupState),
    try
        State0 = #{
            id => GuildId,
            data => #{
                <<"guild">> => #{<<"features">> => []},
                <<"members">> => []
            },
            sessions => #{}
        },
        State1 = update_state(
            guild_update,
            #{<<"features">> => [<<"UNAVAILABLE_FOR_EVERYONE">>]},
            State0
        ),
        ?assertEqual(unavailable_for_everyone, guild_availability:get_cached_unavailability_mode(GuildId)),
        _State2 = update_state(guild_update, #{<<"features">> => []}, State1),
        ?assertEqual(available, guild_availability:get_cached_unavailability_mode(GuildId))
    after
        _ = guild_availability:update_unavailability_cache_for_state(CleanupState)
    end.

handle_guild_update_merges_fields_test() ->
    Data = #{
        <<"guild">> => #{<<"name">> => <<"Old">>, <<"icon">> => <<"abc">>},
        <<"roles">> => [],
        <<"members">> => [],
        <<"channels">> => []
    },
    EventData = #{<<"name">> => <<"New">>, <<"description">> => <<"desc">>},
    Result = handle_guild_update(EventData, Data),
    Guild = maps:get(<<"guild">>, Result),
    ?assertEqual(<<"New">>, maps:get(<<"name">>, Guild)),
    ?assertEqual(<<"abc">>, maps:get(<<"icon">>, Guild)),
    ?assertEqual(<<"desc">>, maps:get(<<"description">>, Guild)).

handle_member_update_ignores_non_member_test() ->
    Data = #{
        <<"members">> => #{
            1 => #{<<"user">> => #{<<"id">> => <<"1">>}, <<"nick">> => <<"nick">>}
        }
    },
    EventData = #{<<"user">> => #{<<"id">> => <<"999">>}, <<"nick">> => <<"new">>},
    Result = handle_member_update(EventData, Data),
    ?assertEqual(1, map_size(maps:get(<<"members">>, Result))),
    ?assertEqual(undefined, guild_data_index:get_member(999, Result)).

handle_role_create_test() ->
    Data = #{
        <<"roles">> => [#{<<"id">> => <<"1">>, <<"name">> => <<"Everyone">>}],
        <<"members">> => [],
        <<"channels">> => []
    },
    EventData = #{<<"role">> => #{<<"id">> => <<"2">>, <<"name">> => <<"New">>}},
    Result = handle_role_create(EventData, Data),
    Roles = guild_data_index:role_list(Result),
    ?assertEqual(2, length(Roles)),
    RoleIndex = guild_data_index:role_index(Result),
    ?assertMatch(#{2 := _}, RoleIndex).

handle_role_update_replaces_role_test() ->
    Data = #{
        <<"roles">> => [
            #{<<"id">> => <<"1">>, <<"name">> => <<"Old">>},
            #{<<"id">> => <<"2">>, <<"name">> => <<"Keep">>}
        ],
        <<"members">> => [],
        <<"channels">> => []
    },
    EventData = #{<<"role">> => #{<<"id">> => <<"1">>, <<"name">> => <<"Updated">>}},
    Result = handle_role_update(EventData, Data),
    Roles = guild_data_index:role_list(Result),
    [R1, R2] = Roles,
    ?assertEqual(<<"Updated">>, maps:get(<<"name">>, R1)),
    ?assertEqual(<<"Keep">>, maps:get(<<"name">>, R2)).

handle_role_update_bulk_test() ->
    Data = #{
        <<"roles">> => [
            #{<<"id">> => <<"1">>, <<"name">> => <<"A">>},
            #{<<"id">> => <<"2">>, <<"name">> => <<"B">>},
            #{<<"id">> => <<"3">>, <<"name">> => <<"C">>}
        ],
        <<"members">> => [],
        <<"channels">> => []
    },
    EventData = #{
        <<"roles">> => [
            #{<<"id">> => <<"1">>, <<"name">> => <<"A2">>},
            #{<<"id">> => <<"3">>, <<"name">> => <<"C2">>}
        ]
    },
    Result = handle_role_update_bulk(EventData, Data),
    Roles = guild_data_index:role_list(Result),
    [R1, R2, R3] = Roles,
    ?assertEqual(<<"A2">>, maps:get(<<"name">>, R1)),
    ?assertEqual(<<"B">>, maps:get(<<"name">>, R2)),
    ?assertEqual(<<"C2">>, maps:get(<<"name">>, R3)).

handle_role_delete_removes_role_from_list_test() ->
    Data = #{
        <<"roles">> => [
            #{<<"id">> => <<"1">>, <<"name">> => <<"Keep">>},
            #{<<"id">> => <<"2">>, <<"name">> => <<"Delete">>}
        ],
        <<"members">> => [],
        <<"channels">> => []
    },
    EventData = #{<<"role_id">> => <<"2">>},
    Result = handle_role_delete(EventData, Data),
    Roles = guild_data_index:role_list(Result),
    ?assertEqual(1, length(Roles)),
    ?assertEqual(<<"Keep">>, maps:get(<<"name">>, hd(Roles))).

handle_channel_update_test() ->
    Data = #{
        <<"channels">> => [
            #{<<"id">> => <<"100">>, <<"name">> => <<"old">>},
            #{<<"id">> => <<"101">>, <<"name">> => <<"keep">>}
        ]
    },
    EventData = #{<<"id">> => <<"100">>, <<"name">> => <<"updated">>},
    Result = handle_channel_update(EventData, Data),
    Channels = guild_data_index:channel_list(Result),
    [C1, C2] = Channels,
    ?assertEqual(<<"updated">>, maps:get(<<"name">>, C1)),
    ?assertEqual(<<"keep">>, maps:get(<<"name">>, C2)).

handle_channel_update_bulk_test() ->
    Data = #{
        <<"channels">> => [
            #{<<"id">> => <<"1">>, <<"name">> => <<"A">>},
            #{<<"id">> => <<"2">>, <<"name">> => <<"B">>}
        ]
    },
    EventData = #{
        <<"channels">> => [
            #{<<"id">> => <<"2">>, <<"name">> => <<"B2">>}
        ]
    },
    Result = handle_channel_update_bulk(EventData, Data),
    Channels = guild_data_index:channel_list(Result),
    [C1, C2] = Channels,
    ?assertEqual(<<"A">>, maps:get(<<"name">>, C1)),
    ?assertEqual(<<"B2">>, maps:get(<<"name">>, C2)).

handle_channel_delete_test() ->
    Data = #{
        <<"channels">> => [
            #{<<"id">> => <<"100">>, <<"name">> => <<"general">>},
            #{<<"id">> => <<"101">>, <<"name">> => <<"random">>}
        ]
    },
    EventData = #{<<"id">> => <<"100">>},
    Result = handle_channel_delete(EventData, Data),
    Channels = guild_data_index:channel_list(Result),
    ?assertEqual(1, length(Channels)),
    ?assertEqual(<<"random">>, maps:get(<<"name">>, hd(Channels))).

handle_message_create_updates_last_message_id_test() ->
    Data = #{
        <<"channels">> => [
            #{<<"id">> => <<"100">>, <<"last_message_id">> => <<"500">>},
            #{<<"id">> => <<"101">>, <<"last_message_id">> => <<"600">>}
        ]
    },
    EventData = #{<<"channel_id">> => <<"100">>, <<"id">> => <<"700">>},
    Result = handle_message_create(EventData, Data),
    Channels = guild_data_index:channel_list(Result),
    [C1, C2] = Channels,
    ?assertEqual(<<"700">>, maps:get(<<"last_message_id">>, C1)),
    ?assertEqual(<<"600">>, maps:get(<<"last_message_id">>, C2)).

handle_channel_pins_update_test() ->
    Data = #{
        <<"channels">> => [
            #{<<"id">> => <<"100">>}
        ]
    },
    EventData = #{<<"channel_id">> => <<"100">>, <<"last_pin_timestamp">> => <<"2024-01-01T00:00:00Z">>},
    Result = handle_channel_pins_update(EventData, Data),
    [Ch] = guild_data_index:channel_list(Result),
    ?assertEqual(<<"2024-01-01T00:00:00Z">>, maps:get(<<"last_pin_timestamp">>, Ch)).

handle_emojis_update_test() ->
    Data = #{<<"emojis">> => []},
    EventData = #{<<"emojis">> => [#{<<"id">> => <<"1">>}]},
    Result = handle_emojis_update(EventData, Data),
    ?assertEqual([#{<<"id">> => <<"1">>}], maps:get(<<"emojis">>, Result)).

handle_stickers_update_test() ->
    Data = #{<<"stickers">> => []},
    EventData = #{<<"stickers">> => [#{<<"id">> => <<"1">>}]},
    Result = handle_stickers_update(EventData, Data),
    ?assertEqual([#{<<"id">> => <<"1">>}], maps:get(<<"stickers">>, Result)).

replace_item_by_id_test() ->
    Items = [
        #{<<"id">> => <<"1">>, <<"v">> => <<"a">>},
        #{<<"id">> => <<"2">>, <<"v">> => <<"b">>}
    ],
    Result = replace_item_by_id(Items, <<"1">>, #{<<"id">> => <<"1">>, <<"v">> => <<"c">>}),
    [R1, R2] = Result,
    ?assertEqual(<<"c">>, maps:get(<<"v">>, R1)),
    ?assertEqual(<<"b">>, maps:get(<<"v">>, R2)).

replace_item_by_id_no_match_test() ->
    Items = [#{<<"id">> => <<"1">>, <<"v">> => <<"a">>}],
    Result = replace_item_by_id(Items, <<"999">>, #{<<"id">> => <<"999">>}),
    ?assertEqual(Items, Result).

remove_item_by_id_test() ->
    Items = [
        #{<<"id">> => <<"1">>},
        #{<<"id">> => <<"2">>},
        #{<<"id">> => <<"3">>}
    ],
    Result = remove_item_by_id(Items, <<"2">>),
    ?assertEqual(2, length(Result)),
    Ids = [maps:get(<<"id">>, I) || I <- Result],
    ?assertEqual([<<"1">>, <<"3">>], Ids).

remove_item_by_id_no_match_test() ->
    Items = [#{<<"id">> => <<"1">>}],
    ?assertEqual(Items, remove_item_by_id(Items, <<"999">>)).

bulk_update_items_no_updates_test() ->
    Items = [#{<<"id">> => <<"1">>, <<"v">> => <<"a">>}],
    ?assertEqual(Items, bulk_update_items(Items, [])).

bulk_update_items_missing_id_in_bulk_ignored_test() ->
    Items = [#{<<"id">> => <<"1">>, <<"v">> => <<"a">>}],
    BulkItems = [#{<<"v">> => <<"b">>}],
    ?assertEqual(Items, bulk_update_items(Items, BulkItems)).

extract_role_ids_from_role_update_test() ->
    EventData = #{<<"role">> => #{<<"id">> => <<"42">>}},
    ?assertEqual([42], extract_role_ids_from_role_update(EventData)).

extract_role_ids_from_role_update_missing_id_test() ->
    EventData = #{<<"role">> => #{}},
    ?assertEqual([], extract_role_ids_from_role_update(EventData)).

extract_role_ids_from_role_update_missing_role_test() ->
    ?assertEqual([], extract_role_ids_from_role_update(#{})).

extract_role_ids_from_role_update_bulk_test() ->
    EventData = #{<<"roles">> => [#{<<"id">> => <<"1">>}, #{<<"id">> => <<"2">>}]},
    ?assertEqual([1, 2], extract_role_ids_from_role_update_bulk(EventData)).

extract_role_ids_from_role_update_bulk_empty_test() ->
    ?assertEqual([], extract_role_ids_from_role_update_bulk(#{})).

extract_role_ids_from_role_delete_test() ->
    EventData = #{<<"role_id">> => <<"55">>},
    ?assertEqual([55], extract_role_ids_from_role_delete(EventData)).

extract_role_ids_from_role_delete_missing_test() ->
    ?assertEqual([], extract_role_ids_from_role_delete(#{})).

update_data_for_event_unknown_returns_data_unchanged_test() ->
    Data = #{<<"test">> => true},
    ?assertEqual(Data, update_data_for_event(unknown_event, #{}, Data, #{})).

strip_role_from_members_no_affected_users_test() ->
    Data = #{
        <<"roles">> => [],
        <<"members">> => #{
            1 => #{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => [<<"100">>]}
        },
        <<"channels">> => []
    },
    Result = strip_role_from_members(<<"999">>, Data),
    M1 = guild_data_index:get_member(1, Result),
    ?assertEqual([<<"100">>], maps:get(<<"roles">>, M1)).

strip_role_from_channel_overwrites_preserves_user_overwrites_test() ->
    Data = #{
        <<"channels">> => [
            #{
                <<"id">> => <<"500">>,
                <<"permission_overwrites">> => [
                    #{<<"id">> => <<"100">>, <<"type">> => 0, <<"allow">> => <<"0">>, <<"deny">> => <<"0">>},
                    #{<<"id">> => <<"1">>, <<"type">> => 1, <<"allow">> => <<"1024">>, <<"deny">> => <<"0">>}
                ]
            }
        ]
    },
    Result = strip_role_from_channel_overwrites(<<"100">>, Data),
    [Ch] = guild_data_index:channel_list(Result),
    Overwrites = maps:get(<<"permission_overwrites">>, Ch),
    ?assertEqual(1, length(Overwrites)),
    ?assertEqual(1, maps:get(<<"type">>, hd(Overwrites))).

cleanup_removed_member_sessions_removes_non_members_test() ->
    Data = #{
        <<"members">> => #{
            1 => #{<<"user">> => #{<<"id">> => <<"1">>}}
        }
    },
    Sessions = #{
        <<"s1">> => #{user_id => 1},
        <<"s2">> => #{user_id => 999}
    },
    State = #{data => Data, sessions => Sessions},
    Result = cleanup_removed_member_sessions(State),
    NewSessions = maps:get(sessions, Result),
    ?assertEqual(1, map_size(NewSessions)),
    ?assert(maps:is_key(<<"s1">>, NewSessions)).

-endif.
