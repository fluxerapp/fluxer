%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_dispatch_guild).
-typing([eqwalizer]).

-export([
    update_channels_map/3,
    update_dm_voice_states_map/3,
    update_relationships_map/3
]).

-export_type([session_state/0, event/0]).

-type session_state() :: session:session_state().
-type event() :: atom() | binary().
-type user_id() :: session:user_id().
-type channel_event() :: channel_create | channel_update.

-spec update_channels_map(event(), map() | list(), session_state()) -> session_state().
update_channels_map(channel_create, Data, State) when is_map(Data) ->
    maybe_add_dm_channel(channel_create, Data, State);
update_channels_map(channel_update, Data, State) when is_map(Data) ->
    maybe_add_dm_channel(channel_update, Data, State);
update_channels_map(channel_delete, Data, State) when is_map(Data) ->
    case maps:find(<<"id">>, Data) of
        error -> State;
        {ok, ChannelIdBin} -> remove_channel_from_state(ChannelIdBin, State)
    end;
update_channels_map(channel_recipient_add, Data, State) when is_map(Data) ->
    update_recipient_membership(add, Data, State);
update_channels_map(channel_recipient_remove, Data, State) when is_map(Data) ->
    update_recipient_membership(remove, Data, State);
update_channels_map(_Event, _Data, State) ->
    State.

-spec update_dm_voice_states_map(event(), map() | list(), session_state()) -> session_state().
update_dm_voice_states_map(voice_state_update, Data, State) when is_map(Data) ->
    update_dm_voice_state(Data, State);
update_dm_voice_states_map(call_create, Data, State) when is_map(Data) ->
    replace_dm_voice_states_for_call(Data, State);
update_dm_voice_states_map(call_update, Data, State) when is_map(Data) ->
    replace_dm_voice_states_for_call(Data, State);
update_dm_voice_states_map(call_delete, Data, State) when is_map(Data) ->
    remove_dm_voice_states_for_call(Data, State);
update_dm_voice_states_map(_, _, State) ->
    State.

-spec update_dm_voice_state(map(), session_state()) -> session_state().
update_dm_voice_state(Data, State) ->
    case {is_dm_voice_state(Data), normalize_binary_field(Data, <<"connection_id">>)} of
        {true, ConnId} when is_binary(ConnId) ->
            upsert_or_remove_dm_voice_state(ConnId, Data, State);
        _ ->
            State
    end.

-spec upsert_or_remove_dm_voice_state(binary(), map(), session_state()) -> session_state().
upsert_or_remove_dm_voice_state(ConnId, Data, State) ->
    VoiceStates = ensure_dm_voice_states(State),
    case normalize_optional_snowflake_field(Data, <<"channel_id">>) of
        undefined ->
            State#{dm_voice_states => maps:remove(ConnId, VoiceStates)};
        _ChannelId ->
            State#{dm_voice_states => VoiceStates#{ConnId => Data}}
    end.

-spec replace_dm_voice_states_for_call(map(), session_state()) -> session_state().
replace_dm_voice_states_for_call(Data, State) ->
    case
        {
            normalize_optional_snowflake_field(Data, <<"channel_id">>),
            get_field(Data, <<"voice_states">>)
        }
    of
        {ChannelId, VoiceStatesList} when is_integer(ChannelId), is_list(VoiceStatesList) ->
            Existing = remove_dm_voice_states_for_channel(
                ChannelId, ensure_dm_voice_states(State)
            ),
            State#{
                dm_voice_states => add_dm_voice_states_for_channel(
                    ChannelId, VoiceStatesList, Existing
                )
            };
        _ ->
            State
    end.

-spec remove_dm_voice_states_for_call(map(), session_state()) -> session_state().
remove_dm_voice_states_for_call(Data, State) ->
    case normalize_optional_snowflake_field(Data, <<"channel_id">>) of
        ChannelId when is_integer(ChannelId) ->
            State#{
                dm_voice_states => remove_dm_voice_states_for_channel(
                    ChannelId, ensure_dm_voice_states(State)
                )
            };
        _ ->
            State
    end.

-spec add_dm_voice_states_for_channel(integer(), [term()], map()) -> map().
add_dm_voice_states_for_channel(ChannelId, VoiceStates, Acc) ->
    lists:foldl(
        fun(VoiceState, InnerAcc) ->
            maybe_add_dm_voice_state_for_channel(ChannelId, VoiceState, InnerAcc)
        end,
        Acc,
        VoiceStates
    ).

-spec maybe_add_dm_voice_state_for_channel(integer(), term(), map()) -> map().
maybe_add_dm_voice_state_for_channel(ChannelId, VoiceState, Acc) when is_map(VoiceState) ->
    case
        {
            is_dm_voice_state(VoiceState),
            normalize_optional_snowflake_field(VoiceState, <<"channel_id">>),
            normalize_binary_field(VoiceState, <<"connection_id">>)
        }
    of
        {true, ChannelId, ConnId} when is_binary(ConnId) -> Acc#{ConnId => VoiceState};
        _ -> Acc
    end;
maybe_add_dm_voice_state_for_channel(_, _, Acc) ->
    Acc.

-spec remove_dm_voice_states_for_channel(integer(), map()) -> map().
remove_dm_voice_states_for_channel(ChannelId, VoiceStates) ->
    maps:filter(
        fun(_ConnId, VoiceState) ->
            normalize_optional_snowflake_field(VoiceState, <<"channel_id">>) =/= ChannelId
        end,
        VoiceStates
    ).

-spec ensure_dm_voice_states(session_state()) -> map().
ensure_dm_voice_states(State) ->
    case maps:get(dm_voice_states, State, #{}) of
        VoiceStates when is_map(VoiceStates) -> VoiceStates;
        _ -> #{}
    end.

-spec is_dm_voice_state(map()) -> boolean().
is_dm_voice_state(Data) ->
    case get_field(Data, <<"guild_id">>) of
        undefined -> true;
        null -> true;
        0 -> true;
        <<"0">> -> true;
        _ -> false
    end.

-spec normalize_optional_snowflake_field(map(), binary()) -> integer() | undefined.
normalize_optional_snowflake_field(Data, Key) ->
    case get_field(Data, Key) of
        undefined -> undefined;
        null -> undefined;
        Value -> guild_voice_connection_normalize:normalize_positive_snowflake(Value)
    end.

-spec normalize_binary_field(map(), binary()) -> binary() | undefined.
normalize_binary_field(Data, Key) ->
    guild_voice_connection_normalize:normalize_optional_binary(get_field(Data, Key)).

-spec get_field(map(), binary()) -> term().
get_field(Data, Key) ->
    case maps:find(Key, Data) of
        {ok, Value} -> Value;
        error -> get_atom_field(Data, Key)
    end.

-spec get_atom_field(map(), binary()) -> term().
get_atom_field(Data, <<"channel_id">>) ->
    maps:get(channel_id, Data, undefined);
get_atom_field(Data, <<"connection_id">>) ->
    maps:get(connection_id, Data, undefined);
get_atom_field(Data, <<"guild_id">>) ->
    maps:get(guild_id, Data, undefined);
get_atom_field(Data, <<"voice_states">>) ->
    maps:get(voice_states, Data, undefined);
get_atom_field(_Data, _Key) ->
    undefined.

-spec maybe_add_dm_channel(channel_event(), map(), session_state()) -> session_state().
maybe_add_dm_channel(Event, Data, State) ->
    case maps:get(<<"type">>, Data, undefined) of
        1 -> add_channel_to_state(Event, Data, State);
        3 -> add_channel_to_state(Event, Data, State);
        _ -> State
    end.

-spec add_channel_to_state(channel_event(), map(), session_state()) -> session_state().
add_channel_to_state(Event, Data, State) ->
    case maps:find(<<"id">>, Data) of
        error -> State;
        {ok, ChannelIdBin} -> upsert_channel_in_state(Event, ChannelIdBin, Data, State)
    end.

-spec upsert_channel_in_state(channel_event(), binary(), map(), session_state()) ->
    session_state().
upsert_channel_in_state(Event, ChannelIdBin, Data, State) ->
    case validation:validate_snowflake(<<"id">>, ChannelIdBin) of
        {ok, ChannelId} ->
            Channels = maps:get(channels, State, #{}),
            Existing = maps:get(ChannelId, Channels, undefined),
            StoredChannel = merge_channel_update(Event, Data, Existing),
            State#{channels => Channels#{ChannelId => StoredChannel}};
        {error, _, _} ->
            State
    end.

-spec merge_channel_update(channel_event(), map(), term()) -> map().
merge_channel_update(channel_create, Data, _Existing) ->
    Data;
merge_channel_update(channel_update, Data, Existing) when is_map(Existing) ->
    merge_channel_recipient_fields(Data, Existing, maps:merge(Existing, Data));
merge_channel_update(channel_update, Data, _Existing) ->
    Data.

-spec merge_channel_recipient_fields(map(), map(), map()) -> map().
merge_channel_recipient_fields(Data, Existing, Merged) ->
    HasRecipientIds = recipient_field_has_ids(Data, <<"recipient_ids">>),
    HasRecipients = recipient_field_has_ids(Data, <<"recipients">>),
    case {HasRecipientIds, HasRecipients} of
        {true, false} -> maps:remove(<<"recipients">>, Merged);
        {false, true} -> maps:remove(<<"recipient_ids">>, Merged);
        {true, true} -> Merged;
        {false, false} -> preserve_existing_recipient_fields(Existing, Merged)
    end.

-spec preserve_existing_recipient_fields(map(), map()) -> map().
preserve_existing_recipient_fields(Existing, Merged) ->
    lists:foldl(
        fun(Key, Acc) -> preserve_existing_recipient_field(Key, Existing, Acc) end,
        Merged,
        [<<"recipients">>, <<"recipient_ids">>]
    ).

-spec preserve_existing_recipient_field(binary(), map(), map()) -> map().
preserve_existing_recipient_field(Key, Existing, Merged) ->
    case recipient_field_has_ids(Existing, Key) of
        true -> Merged#{Key => maps:get(Key, Existing)};
        false -> Merged
    end.

-spec recipient_field_has_ids(map(), binary()) -> boolean().
recipient_field_has_ids(Data, Key) ->
    case maps:get(Key, Data, undefined) of
        Values when is_list(Values) -> lists:any(fun recipient_entry_has_id/1, Values);
        _ -> false
    end.

-spec recipient_entry_has_id(term()) -> boolean().
recipient_entry_has_id(Entry) when is_map(Entry) ->
    type_conv:extract_id(Entry, <<"id">>) =/= undefined;
recipient_entry_has_id(Entry) when is_binary(Entry) ->
    type_conv:extract_id(#{<<"id">> => Entry}, <<"id">>) =/= undefined;
recipient_entry_has_id(Entry) when is_integer(Entry) ->
    Entry > 0;
recipient_entry_has_id(_Entry) ->
    false.

-spec remove_channel_from_state(binary(), session_state()) -> session_state().
remove_channel_from_state(ChannelIdBin, State) ->
    case validation:validate_snowflake(<<"id">>, ChannelIdBin) of
        {ok, ChannelId} ->
            Channels = maps:get(channels, State, #{}),
            State#{channels => maps:remove(ChannelId, Channels)};
        {error, _, _} ->
            State
    end.

-spec update_recipient_membership(add | remove, map(), session_state()) -> session_state().
update_recipient_membership(Action, Data, State) ->
    ChannelIdBin = maps:get(<<"channel_id">>, Data, undefined),
    case validation:validate_snowflake(<<"channel_id">>, ChannelIdBin) of
        {ok, ChannelId} ->
            do_update_recipient(Action, Data, ChannelId, State);
        _ ->
            State
    end.

-spec do_update_recipient(add | remove, map(), non_neg_integer(), session_state()) ->
    session_state().
do_update_recipient(Action, Data, ChannelId, State) ->
    Channels = maps:get(channels, State, #{}),
    case find_group_dm_channel(ChannelId, Channels) of
        {ok, Channel} ->
            apply_recipient_update(Action, Data, Channel, ChannelId, State);
        _ ->
            State
    end.

-spec find_group_dm_channel(non_neg_integer(), map()) -> {ok, map()} | not_found.
find_group_dm_channel(ChannelId, Channels) ->
    case maps:find(ChannelId, Channels) of
        {ok, #{<<"type">> := 3} = Channel} -> {ok, Channel};
        _ -> not_found
    end.

-spec apply_recipient_update(add | remove, map(), map(), non_neg_integer(), session_state()) ->
    session_state().
apply_recipient_update(Action, Data, Channel, ChannelId, State) ->
    UserMap = maps:get(<<"user">>, Data, #{}),
    RecipientId = type_conv:extract_id(UserMap, <<"id">>),
    case RecipientId of
        undefined ->
            State;
        _ ->
            UpdatedChannel = update_channel_recipient(Channel, RecipientId, UserMap, Action),
            Channels = maps:get(channels, State, #{}),
            NewChannels = Channels#{ChannelId => UpdatedChannel},
            State#{channels => NewChannels}
    end.

-spec update_channel_recipient(map(), user_id(), map(), add | remove) -> map().
update_channel_recipient(Channel, RecipientId, UserMap, add) ->
    RecipientIds = maps:get(<<"recipient_ids">>, Channel, []),
    Recipients = maps:get(<<"recipients">>, Channel, []),
    NewRecipientIds = add_unique_id(RecipientId, RecipientIds),
    NewRecipients = add_unique_user(UserMap, Recipients),
    Channel#{<<"recipient_ids">> => NewRecipientIds, <<"recipients">> => NewRecipients};
update_channel_recipient(Channel, RecipientId, _UserMap, remove) ->
    RecipientIds = maps:get(<<"recipient_ids">>, Channel, []),
    Recipients = maps:get(<<"recipients">>, Channel, []),
    NewRecipientIds = lists:filter(
        fun(Id) -> not snowflake_id:equal(RecipientId, Id) end,
        RecipientIds
    ),
    NewRecipients = lists:filter(
        fun(R) ->
            type_conv:extract_id(R, <<"id">>) =/= RecipientId
        end,
        Recipients
    ),
    Channel#{<<"recipient_ids">> => NewRecipientIds, <<"recipients">> => NewRecipients}.

-spec add_unique_id(user_id(), [binary() | user_id()]) -> [binary() | user_id()].
add_unique_id(Id, List) ->
    case snowflake_id:member(Id, List) of
        true -> List;
        false -> [Id | List]
    end.

-spec add_unique_user(map(), [map()]) -> [map()].
add_unique_user(UserMap, List) when is_map(UserMap) ->
    case type_conv:extract_id(UserMap, <<"id">>) of
        undefined -> List;
        Id -> add_unique_user_by_id(UserMap, Id, List)
    end.

-spec add_unique_user_by_id(map(), user_id(), [map()]) -> [map()].
add_unique_user_by_id(UserMap, Id, List) ->
    HasUser = lists:any(
        fun(R) -> type_conv:extract_id(R, <<"id">>) =:= Id end,
        List
    ),
    case HasUser of
        true -> List;
        false -> [UserMap | List]
    end.

-spec update_relationships_map(event(), map() | list(), session_state()) -> session_state().
update_relationships_map(relationship_add, Data, State) ->
    upsert_relationship(Data, State);
update_relationships_map(relationship_update, Data, State) ->
    upsert_relationship(Data, State);
update_relationships_map(relationship_remove, Data, State) ->
    case type_conv:extract_id(Data, <<"id">>) of
        undefined ->
            State;
        UserId ->
            Relationships = maps:get(relationships, State, #{}),
            NewRelationships = maps:remove(UserId, Relationships),
            State#{relationships => NewRelationships}
    end;
update_relationships_map(_, _, State) ->
    State.

-spec upsert_relationship(map(), session_state()) -> session_state().
upsert_relationship(Data, State) ->
    UserId = type_conv:extract_id(Data, <<"id">>),
    Type = maps:get(<<"type">>, Data, undefined),
    case {UserId, Type} of
        {undefined, _} ->
            State;
        {_, T} when is_integer(T) ->
            Relationships = maps:get(relationships, State, #{}),
            State#{relationships => Relationships#{UserId => T}};
        _ ->
            State
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

add_unique_id_test() ->
    ?assertEqual([1, 2, 3], add_unique_id(1, [2, 3])),
    ?assertEqual([1, 2, 3], add_unique_id(1, [1, 2, 3])),
    ?assertEqual([<<"1">>, 2, 3], add_unique_id(1, [<<"1">>, 2, 3])),
    ok.

channel_create_inserts_test() ->
    State = #{channels => #{}},
    Data = #{<<"id">> => <<"100">>, <<"type">> => 1},
    State1 = update_channels_map(channel_create, Data, State),
    ?assertEqual(1, map_size(maps:get(channels, State1))).

channel_create_updates_existing_test() ->
    State = #{channels => #{100 => #{<<"type">> => 0}}},
    Data = #{<<"id">> => <<"100">>, <<"type">> => 1},
    State1 = update_channels_map(channel_create, Data, State),
    ?assertEqual(1, map_size(maps:get(channels, State1))).

channel_update_preserves_dm_recipients_when_payload_is_partial_test() ->
    Existing = #{
        <<"id">> => <<"100">>,
        <<"type">> => 3,
        <<"name">> => <<"old">>,
        <<"recipients">> => [#{<<"id">> => <<"2">>, <<"username">> => <<"two">>}],
        <<"recipient_ids">> => [<<"2">>]
    },
    State = #{channels => #{100 => Existing}},
    Data = #{<<"id">> => <<"100">>, <<"type">> => 3, <<"name">> => <<"new">>},
    State1 = update_channels_map(channel_update, Data, State),
    Channel = maps:get(100, maps:get(channels, State1)),
    ?assertEqual(<<"new">>, maps:get(<<"name">>, Channel)),
    ?assertEqual(maps:get(<<"recipients">>, Existing), maps:get(<<"recipients">>, Channel)),
    ?assertEqual(
        maps:get(<<"recipient_ids">>, Existing), maps:get(<<"recipient_ids">>, Channel)
    ).

channel_update_empty_recipient_payload_keeps_existing_targets_test() ->
    Existing = #{
        <<"id">> => <<"100">>,
        <<"type">> => 1,
        <<"recipients">> => [#{<<"id">> => <<"2">>, <<"username">> => <<"two">>}]
    },
    State = #{channels => #{100 => Existing}},
    Data = #{<<"id">> => <<"100">>, <<"type">> => 1, <<"recipients">> => []},
    State1 = update_channels_map(channel_update, Data, State),
    Channel = maps:get(100, maps:get(channels, State1)),
    ?assertEqual(maps:get(<<"recipients">>, Existing), maps:get(<<"recipients">>, Channel)).

channel_update_recipient_ids_replace_stale_recipient_maps_test() ->
    Existing = #{
        <<"id">> => <<"100">>,
        <<"type">> => 3,
        <<"recipients">> => [#{<<"id">> => <<"2">>, <<"username">> => <<"two">>}]
    },
    State = #{channels => #{100 => Existing}},
    Data = #{<<"id">> => <<"100">>, <<"type">> => 3, <<"recipient_ids">> => [<<"3">>]},
    State1 = update_channels_map(channel_update, Data, State),
    Channel = maps:get(100, maps:get(channels, State1)),
    ?assertNot(maps:is_key(<<"recipients">>, Channel)),
    ?assertEqual([<<"3">>], maps:get(<<"recipient_ids">>, Channel)).

relationship_add_inserts_test() ->
    State = #{relationships => #{}},
    Data = #{<<"id">> => <<"100">>, <<"type">> => 1},
    State1 = update_relationships_map(relationship_add, Data, State),
    ?assertEqual(1, map_size(maps:get(relationships, State1))).

relationship_add_updates_type_test() ->
    State = #{relationships => #{100 => 1}},
    Data = #{<<"id">> => <<"100">>, <<"type">> => 2},
    State1 = update_relationships_map(relationship_add, Data, State),
    ?assertEqual(2, maps:get(100, maps:get(relationships, State1))).

relationship_add_uses_top_level_relationship_id_test() ->
    State = #{relationships => #{}},
    Data = #{<<"id">> => <<"200">>, <<"user">> => #{<<"id">> => <<"200">>}, <<"type">> => 1},
    State1 = update_relationships_map(relationship_add, Data, State),
    ?assertEqual(1, maps:get(200, maps:get(relationships, State1))).

relationship_remove_uses_top_level_relationship_id_test() ->
    State = #{relationships => #{200 => 1}},
    Data = #{<<"id">> => <<"200">>, <<"user">> => #{<<"id">> => <<"200">>}},
    State1 = update_relationships_map(relationship_remove, Data, State),
    ?assertEqual(false, maps:is_key(200, maps:get(relationships, State1))).

voice_state_update_tracks_dm_voice_state_test() ->
    VoiceState = #{
        <<"guild_id">> => null,
        <<"channel_id">> => <<"100">>,
        <<"connection_id">> => <<"conn-a">>,
        <<"user_id">> => <<"10">>
    },
    State1 = update_dm_voice_states_map(voice_state_update, VoiceState, #{
        dm_voice_states => #{}
    }),
    ?assertEqual(VoiceState, maps:get(<<"conn-a">>, typed_dm_voice_states(State1))).

voice_state_update_removes_dm_voice_state_on_disconnect_test() ->
    Existing = #{
        <<"guild_id">> => null,
        <<"channel_id">> => <<"100">>,
        <<"connection_id">> => <<"conn-a">>,
        <<"user_id">> => <<"10">>
    },
    Disconnect = Existing#{<<"channel_id">> => null},
    State = #{dm_voice_states => #{<<"conn-a">> => Existing}},
    State1 = update_dm_voice_states_map(voice_state_update, Disconnect, State),
    ?assertEqual(#{}, maps:get(dm_voice_states, State1)).

call_update_replaces_dm_voice_states_for_channel_test() ->
    Stale = #{
        <<"guild_id">> => null,
        <<"channel_id">> => <<"100">>,
        <<"connection_id">> => <<"conn-stale">>,
        <<"user_id">> => <<"10">>
    },
    OtherChannel = #{
        <<"guild_id">> => null,
        <<"channel_id">> => <<"200">>,
        <<"connection_id">> => <<"conn-other">>,
        <<"user_id">> => <<"20">>
    },
    Current = #{
        <<"guild_id">> => null,
        <<"channel_id">> => <<"100">>,
        <<"connection_id">> => <<"conn-current">>,
        <<"user_id">> => <<"30">>,
        <<"viewer_stream_keys">> => [<<"dm:100:conn-stale">>]
    },
    State = #{
        dm_voice_states => #{<<"conn-stale">> => Stale, <<"conn-other">> => OtherChannel}
    },
    Data = #{channel_id => <<"100">>, voice_states => [Current]},
    State1 = update_dm_voice_states_map(call_update, Data, State),
    VoiceStates = typed_dm_voice_states(State1),
    ?assertNot(maps:is_key(<<"conn-stale">>, VoiceStates)),
    ?assertEqual(OtherChannel, maps:get(<<"conn-other">>, VoiceStates)),
    ?assertEqual(Current, maps:get(<<"conn-current">>, VoiceStates)).

-spec typed_dm_voice_states(map()) -> #{binary() => map()}.
typed_dm_voice_states(State) ->
    eqwalizer:dynamic_cast(maps:get(dm_voice_states, State, #{})).

-endif.
