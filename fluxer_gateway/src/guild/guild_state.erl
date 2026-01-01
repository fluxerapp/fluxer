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

-export([
    update_state/3
]).

-import(guild_user_data, [maybe_update_cached_user_data/3]).
-import(guild_availability, [handle_unavailability_transition/2]).
-import(guild_visibility, [compute_and_dispatch_visibility_changes/2]).
-import(guild, [update_counts/1]).

update_state(Event, EventData, State) ->
    StateWithUpdatedUser = maybe_update_cached_user_data(Event, EventData, State),
    Data = maps:get(data, StateWithUpdatedUser),

    UpdatedData = update_data_for_event(Event, EventData, Data, State),
    UpdatedState = maps:put(data, UpdatedData, StateWithUpdatedUser),

    handle_post_update(Event, StateWithUpdatedUser, UpdatedState).

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

handle_post_update(guild_update, StateWithUpdatedUser, UpdatedState) ->
    handle_unavailability_transition(StateWithUpdatedUser, UpdatedState),
    UpdatedState;
handle_post_update(guild_member_add, _StateWithUpdatedUser, UpdatedState) ->
    update_counts(UpdatedState);
handle_post_update(guild_member_remove, _StateWithUpdatedUser, UpdatedState) ->
    State1 = cleanup_removed_member_sessions(UpdatedState),
    update_counts(State1);
handle_post_update(Event, StateWithUpdatedUser, UpdatedState) ->
    case needs_visibility_check(Event) of
        true ->
            compute_and_dispatch_visibility_changes(StateWithUpdatedUser, UpdatedState),
            UpdatedState;
        false ->
            UpdatedState
    end.

needs_visibility_check(guild_role_create) -> true;
needs_visibility_check(guild_role_update) -> true;
needs_visibility_check(guild_role_update_bulk) -> true;
needs_visibility_check(guild_role_delete) -> true;
needs_visibility_check(guild_member_update) -> true;
needs_visibility_check(channel_update) -> true;
needs_visibility_check(channel_update_bulk) -> true;
needs_visibility_check(_) -> false.

handle_guild_update(EventData, Data) ->
    Guild = maps:get(<<"guild">>, Data),
    UpdatedGuild = maps:merge(Guild, EventData),
    maps:put(<<"guild">>, UpdatedGuild, Data).

handle_member_add(EventData, Data) ->
    Members = maps:get(<<"members">>, Data, []),
    UpdatedData = maps:put(<<"members">>, Members ++ [EventData], Data),
    UpdatedData.

handle_member_update(EventData, Data) ->
    Members = maps:get(<<"members">>, Data, []),
    UserId = extract_user_id(EventData),
    UpdatedMembers = replace_member_by_id(Members, UserId, EventData),
    maps:put(<<"members">>, UpdatedMembers, Data).

handle_member_remove(EventData, Data, _State) ->
    Members = maps:get(<<"members">>, Data, []),
    UserId = extract_user_id(EventData),
    FilteredMembers = remove_member_by_id(Members, UserId),
    maps:put(<<"members">>, FilteredMembers, Data).

cleanup_removed_member_sessions(State) ->
    Data = maps:get(data, State),
    Members = maps:get(<<"members">>, Data, []),
    MemberUserIds = sets:from_list([extract_user_id_from_member(M) || M <- Members]),

    Sessions = maps:get(sessions, State, #{}),
    FilteredSessions = maps:filter(
        fun(_K, S) ->
            UserId = maps:get(user_id, S),
            sets:is_element(UserId, MemberUserIds)
        end,
        Sessions
    ),

    Presences = maps:get(presences, State, #{}),
    FilteredPresences = maps:filter(
        fun(UserId, _V) ->
            sets:is_element(UserId, MemberUserIds)
        end,
        Presences
    ),

    State1 = maps:put(sessions, FilteredSessions, State),
    maps:put(presences, FilteredPresences, State1).

extract_user_id_from_member(Member) when is_map(Member) ->
    MUser = maps:get(<<"user">>, Member, #{}),
    utils:binary_to_integer_safe(maps:get(<<"id">>, MUser, <<"0">>));
extract_user_id_from_member(_) ->
    0.

extract_user_id(EventData) ->
    MUser = maps:get(<<"user">>, EventData, #{}),
    utils:binary_to_integer_safe(maps:get(<<"id">>, MUser, <<"0">>)).

replace_member_by_id(Members, UserId, NewMember) ->
    lists:map(
        fun(M) when is_map(M) ->
            MMUser = maps:get(<<"user">>, M, #{}),
            MUserId = utils:binary_to_integer_safe(maps:get(<<"id">>, MMUser, <<"0">>)),
            case MUserId =:= UserId of
                true -> NewMember;
                false -> M
            end
        end,
        Members
    ).

remove_member_by_id(Members, UserId) ->
    lists:filter(
        fun(M) when is_map(M) ->
            MMUser = maps:get(<<"user">>, M, #{}),
            MUserId = utils:binary_to_integer_safe(maps:get(<<"id">>, MMUser, <<"0">>)),
            MUserId =/= UserId
        end,
        Members
    ).

handle_role_create(EventData, Data) ->
    Roles = maps:get(<<"roles">>, Data, []),
    RoleData = maps:get(<<"role">>, EventData),
    maps:put(<<"roles">>, Roles ++ [RoleData], Data).

handle_role_update(EventData, Data) ->
    Roles = maps:get(<<"roles">>, Data, []),
    RoleData = maps:get(<<"role">>, EventData),
    RoleId = maps:get(<<"id">>, RoleData),
    UpdatedRoles = replace_item_by_id(Roles, RoleId, RoleData),
    maps:put(<<"roles">>, UpdatedRoles, Data).

handle_role_update_bulk(EventData, Data) ->
    Roles = maps:get(<<"roles">>, Data, []),
    BulkRoles = maps:get(<<"roles">>, EventData, []),
    UpdatedRoles = bulk_update_items(Roles, BulkRoles),
    maps:put(<<"roles">>, UpdatedRoles, Data).

handle_role_delete(EventData, Data) ->
    Roles = maps:get(<<"roles">>, Data, []),
    RoleId = maps:get(<<"role_id">>, EventData),
    FilteredRoles = remove_item_by_id(Roles, RoleId),
    Data1 = maps:put(<<"roles">>, FilteredRoles, Data),
    Data2 = strip_role_from_members(RoleId, Data1),
    strip_role_from_channel_overwrites(RoleId, Data2).

strip_role_from_members(RoleId, Data) ->
    Members = maps:get(<<"members">>, Data, []),
    UpdatedMembers = lists:map(
        fun(Member) when is_map(Member) ->
            MemberRoles = maps:get(<<"roles">>, Member, []),
            FilteredRoles = lists:filter(
                fun(R) ->
                    RoleIdInt = utils:binary_to_integer_safe(RoleId),
                    RInt = utils:binary_to_integer_safe(R),
                    RInt =/= RoleIdInt
                end,
                MemberRoles
            ),
            maps:put(<<"roles">>, FilteredRoles, Member);
        (Member) ->
            Member
        end,
        Members
    ),
    maps:put(<<"members">>, UpdatedMembers, Data).

strip_role_from_channel_overwrites(RoleId, Data) ->
    Channels = maps:get(<<"channels">>, Data, []),
    RoleIdInt = utils:binary_to_integer_safe(RoleId),
    UpdatedChannels = lists:map(
        fun(Channel) when is_map(Channel) ->
            Overwrites = maps:get(<<"permission_overwrites">>, Channel, []),
            FilteredOverwrites = lists:filter(
                fun(Overwrite) when is_map(Overwrite) ->
                    OverwriteType = maps:get(<<"type">>, Overwrite, 0),
                    OverwriteId = utils:binary_to_integer_safe(maps:get(<<"id">>, Overwrite, <<"0">>)),
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
    maps:put(<<"channels">>, UpdatedChannels, Data).

handle_channel_create(EventData, Data) ->
    Channels = maps:get(<<"channels">>, Data, []),
    maps:put(<<"channels">>, Channels ++ [EventData], Data).

handle_channel_update(EventData, Data) ->
    Channels = maps:get(<<"channels">>, Data, []),
    ChannelId = maps:get(<<"id">>, EventData),
    UpdatedChannels = replace_item_by_id(Channels, ChannelId, EventData),
    maps:put(<<"channels">>, UpdatedChannels, Data).

handle_channel_update_bulk(EventData, Data) ->
    Channels = maps:get(<<"channels">>, Data, []),
    BulkChannels = maps:get(<<"channels">>, EventData, []),
    UpdatedChannels = bulk_update_items(Channels, BulkChannels),
    maps:put(<<"channels">>, UpdatedChannels, Data).

handle_channel_delete(EventData, Data) ->
    Channels = maps:get(<<"channels">>, Data, []),
    ChannelId = maps:get(<<"id">>, EventData),
    FilteredChannels = remove_item_by_id(Channels, ChannelId),
    maps:put(<<"channels">>, FilteredChannels, Data).

handle_message_create(EventData, Data) ->
    Channels = maps:get(<<"channels">>, Data, []),
    ChannelId = maps:get(<<"channel_id">>, EventData),
    MessageId = maps:get(<<"id">>, EventData),
    UpdatedChannels = update_channel_field(Channels, ChannelId, <<"last_message_id">>, MessageId),
    maps:put(<<"channels">>, UpdatedChannels, Data).

handle_channel_pins_update(EventData, Data) ->
    Channels = maps:get(<<"channels">>, Data, []),
    ChannelId = maps:get(<<"channel_id">>, EventData),
    LastPin = maps:get(<<"last_pin_timestamp">>, EventData),
    UpdatedChannels = update_channel_field(Channels, ChannelId, <<"last_pin_timestamp">>, LastPin),
    maps:put(<<"channels">>, UpdatedChannels, Data).

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

handle_emojis_update(EventData, Data) ->
    maps:put(<<"emojis">>, maps:get(<<"emojis">>, EventData, []), Data).

handle_stickers_update(EventData, Data) ->
    maps:put(<<"stickers">>, maps:get(<<"stickers">>, EventData, []), Data).

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

remove_item_by_id(Items, Id) ->
    lists:filter(
        fun(Item) when is_map(Item) ->
            maps:get(<<"id">>, Item) =/= Id
        end,
        Items
    ).

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
        <<"members">> => [
            #{
                <<"user">> => #{<<"id">> => <<"1">>},
                <<"roles">> => [<<"100">>, <<"200">>]
            },
            #{
                <<"user">> => #{<<"id">> => <<"2">>},
                <<"roles">> => [<<"200">>]
            },
            #{
                <<"user">> => #{<<"id">> => <<"3">>},
                <<"roles">> => [<<"100">>]
            }
        ],
        <<"channels">> => []
    },
    EventData = #{<<"role_id">> => RoleIdToDelete},
    Result = handle_role_delete(EventData, Data),
    Members = maps:get(<<"members">>, Result),
    [M1, M2, M3] = Members,
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
                    #{<<"id">> => <<"100">>, <<"type">> => 0, <<"allow">> => <<"0">>, <<"deny">> => <<"1024">>},
                    #{<<"id">> => <<"200">>, <<"type">> => 0, <<"allow">> => <<"1024">>, <<"deny">> => <<"0">>},
                    #{<<"id">> => <<"1">>, <<"type">> => 1, <<"allow">> => <<"2048">>, <<"deny">> => <<"0">>}
                ]
            },
            #{
                <<"id">> => <<"501">>,
                <<"permission_overwrites">> => [
                    #{<<"id">> => <<"200">>, <<"type">> => 0, <<"allow">> => <<"1024">>, <<"deny">> => <<"0">>}
                ]
            }
        ]
    },
    EventData = #{<<"role_id">> => RoleIdToDelete},
    Result = handle_role_delete(EventData, Data),
    Channels = maps:get(<<"channels">>, Result),
    [Ch1, Ch2] = Channels,
    Ch1Overwrites = maps:get(<<"permission_overwrites">>, Ch1),
    Ch2Overwrites = maps:get(<<"permission_overwrites">>, Ch2),
    ?assertEqual(2, length(Ch1Overwrites)),
    ?assertEqual(0, length(Ch2Overwrites)),
    OverwriteIds = [maps:get(<<"id">>, O) || O <- Ch1Overwrites],
    ?assert(lists:member(<<"100">>, OverwriteIds)),
    ?assert(lists:member(<<"1">>, OverwriteIds)),
    ?assertNot(lists:member(<<"200">>, OverwriteIds)).

handle_role_delete_preserves_user_overwrites_test() ->
    RoleIdToDelete = <<"200">>,
    Data = #{
        <<"roles">> => [
            #{<<"id">> => <<"200">>, <<"name">> => <<"Moderator">>}
        ],
        <<"members">> => [],
        <<"channels">> => [
            #{
                <<"id">> => <<"500">>,
                <<"permission_overwrites">> => [
                    #{<<"id">> => <<"200">>, <<"type">> => 0, <<"allow">> => <<"1024">>, <<"deny">> => <<"0">>},
                    #{<<"id">> => <<"200">>, <<"type">> => 1, <<"allow">> => <<"2048">>, <<"deny">> => <<"0">>}
                ]
            }
        ]
    },
    EventData = #{<<"role_id">> => RoleIdToDelete},
    Result = handle_role_delete(EventData, Data),
    Channels = maps:get(<<"channels">>, Result),
    [Ch1] = Channels,
    Overwrites = maps:get(<<"permission_overwrites">>, Ch1),
    ?assertEqual(1, length(Overwrites)),
    [RemainingOverwrite] = Overwrites,
    ?assertEqual(1, maps:get(<<"type">>, RemainingOverwrite)).

handle_role_delete_removes_role_from_roles_list_test() ->
    RoleIdToDelete = <<"200">>,
    Data = #{
        <<"roles">> => [
            #{<<"id">> => <<"100">>, <<"name">> => <<"Admin">>},
            #{<<"id">> => <<"200">>, <<"name">> => <<"Moderator">>}
        ],
        <<"members">> => [],
        <<"channels">> => []
    },
    EventData = #{<<"role_id">> => RoleIdToDelete},
    Result = handle_role_delete(EventData, Data),
    Roles = maps:get(<<"roles">>, Result),
    ?assertEqual(1, length(Roles)),
    [RemainingRole] = Roles,
    ?assertEqual(<<"100">>, maps:get(<<"id">>, RemainingRole)).

-endif.
