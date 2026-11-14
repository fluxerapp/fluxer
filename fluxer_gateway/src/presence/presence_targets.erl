%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_targets).
-typing([eqwalizer]).

-export([
    friend_ids_from_state/1,
    group_dm_recipients_from_state/1,
    group_dm_channel_recipient_ids/2,
    direct_dm_partner_ids/1,
    dm_partner_presence_enabled/1,
    map_from_ids/1
]).

-export_type([user_id/0, channel_id/0, state/0]).

-type user_id() :: integer().
-type channel_id() :: integer().
-type state() :: map().

-spec friend_ids_from_state(state()) -> [user_id()].
friend_ids_from_state(State) ->
    Relationships = maps:get(relationships, State, #{}),
    maps:fold(fun accumulate_friend_id/3, [], Relationships).

-spec accumulate_friend_id(term(), term(), [user_id()]) -> [user_id()].
accumulate_friend_id(UserId, 1, Acc) when is_integer(UserId) ->
    [UserId | Acc];
accumulate_friend_id(_UserId, _Type, Acc) ->
    Acc.

-spec group_dm_recipients_from_state(state()) -> #{channel_id() => #{user_id() => true}}.
group_dm_recipients_from_state(State) ->
    UserId = maps:get(user_id, State, undefined),
    Eligible = eligible_dm_partner_ids(State),
    Channels = maps:get(channels, State, #{}),
    maps:fold(
        fun(ChannelId, Channel, Acc) ->
            accumulate_dm_channel(ChannelId, Channel, UserId, Eligible, Acc)
        end,
        #{},
        Channels
    ).

-spec accumulate_dm_channel(
    term(), term(), user_id() | undefined, #{user_id() => true}, map()
) -> map().
accumulate_dm_channel(ChannelId, Channel, UserId, Eligible, Acc) when
    is_integer(ChannelId), is_map(Channel)
->
    case maps:get(<<"type">>, Channel, 0) of
        3 ->
            Acc#{ChannelId => map_from_ids(other_recipient_ids(Channel, UserId))};
        1 ->
            accumulate_direct_dm(
                ChannelId, other_recipient_ids(Channel, UserId), Eligible, Acc
            );
        _ ->
            Acc
    end;
accumulate_dm_channel(_ChannelId, _Channel, _UserId, _Eligible, Acc) ->
    Acc.

-spec accumulate_direct_dm(channel_id(), [user_id()], #{user_id() => true}, map()) -> map().
accumulate_direct_dm(ChannelId, RecipientIds, Eligible, Acc) ->
    case [Rid || Rid <- RecipientIds, maps:is_key(Rid, Eligible)] of
        [] -> Acc;
        EligibleIds -> Acc#{ChannelId => map_from_ids(EligibleIds)}
    end.

-spec other_recipient_ids(map(), user_id() | undefined) -> [user_id()].
other_recipient_ids(Channel, UserId) ->
    [Rid || Rid <- extract_recipient_ids(Channel), Rid =/= UserId].

-spec eligible_dm_partner_ids(state()) -> #{user_id() => true}.
eligible_dm_partner_ids(State) ->
    case dm_partner_presence_enabled(maps:get(user_id, State, undefined)) of
        true -> connected_guild_dm_partners(State);
        false -> #{}
    end.

-spec connected_guild_dm_partners(state()) -> #{user_id() => true}.
connected_guild_dm_partners(State) ->
    Guilds = maps:get(guilds, State, #{}),
    maps:fold(
        fun(GuildId, PartnerIds, Acc) ->
            merge_connected(maps:get(GuildId, Guilds, undefined), PartnerIds, Acc)
        end,
        #{},
        maps:get(dm_mutual_by_guild, State, #{})
    ).

-spec merge_connected(term(), term(), #{user_id() => true}) -> #{user_id() => true}.
merge_connected({Pid, _Ref}, PartnerIds, Acc) when is_pid(Pid), is_map(PartnerIds) ->
    maps:merge(Acc, eqwalizer:dynamic_cast(PartnerIds));
merge_connected(_GuildRef, _PartnerIds, Acc) ->
    Acc.

-spec direct_dm_partner_ids(state()) -> [user_id()].
direct_dm_partner_ids(State) ->
    UserId = maps:get(user_id, State, undefined),
    PartnerIds = maps:fold(
        fun(_ChannelId, Channel, Acc) -> accumulate_direct_partner(Channel, UserId, Acc) end,
        [],
        maps:get(channels, State, #{})
    ),
    lists:usort(PartnerIds).

-spec accumulate_direct_partner(term(), user_id() | undefined, [user_id()]) -> [user_id()].
accumulate_direct_partner(#{<<"type">> := 1} = Channel, UserId, Acc) ->
    other_recipient_ids(Channel, UserId) ++ Acc;
accumulate_direct_partner(_Channel, _UserId, Acc) ->
    Acc.

-spec dm_partner_presence_enabled(term()) -> boolean().
dm_partner_presence_enabled(UserId) when is_integer(UserId) ->
    case application:get_env(fluxer_gateway, dm_presence_mutual_context, true) of
        true -> true;
        {users, UserIds} when is_list(UserIds) -> lists:member(UserId, UserIds);
        _ -> false
    end;
dm_partner_presence_enabled(_UserId) ->
    false.

-spec is_group_dm_channel_type(term()) -> boolean().
is_group_dm_channel_type(3) -> true;
is_group_dm_channel_type(_) -> false.

-spec group_dm_channel_recipient_ids(term(), user_id() | undefined) -> [user_id()].
group_dm_channel_recipient_ids(Channel, SelfUserId) when is_map(Channel) ->
    case is_group_dm_channel_type(maps:get(<<"type">>, Channel, 0)) of
        true ->
            [Rid || Rid <- extract_recipient_ids(Channel), Rid =/= SelfUserId];
        false ->
            []
    end;
group_dm_channel_recipient_ids(_Channel, _SelfUserId) ->
    [].

-spec extract_recipient_ids(map()) -> [user_id()].
extract_recipient_ids(Channel) ->
    Recipients = maps:get(
        <<"recipients">>, Channel, maps:get(<<"recipient_ids">>, Channel, [])
    ),
    Unique = lists:foldl(fun accumulate_unique_recipient/2, [], list_value(Recipients)),
    lists:reverse(Unique).

-spec accumulate_unique_recipient(term(), [user_id()]) -> [user_id()].
accumulate_unique_recipient(Entry, Acc) ->
    case extract_recipient_id(Entry) of
        undefined -> Acc;
        Value -> maybe_append_unique(Value, Acc)
    end.

-spec maybe_append_unique(user_id(), [user_id()]) -> [user_id()].
maybe_append_unique(Value, Acc) ->
    case lists:member(Value, Acc) of
        true -> Acc;
        false -> [Value | Acc]
    end.

-spec extract_recipient_id(term()) -> user_id() | undefined.
extract_recipient_id(Entry) when is_map(Entry) ->
    type_conv:extract_id(Entry, <<"id">>);
extract_recipient_id(Entry) ->
    case Entry of
        Bin when is_binary(Bin) ->
            type_conv:extract_id(#{<<"id">> => Bin}, <<"id">>);
        Int when is_integer(Int) ->
            Int;
        _ ->
            undefined
    end.

-spec map_from_ids([term()]) -> #{user_id() => true}.
map_from_ids(Ids) when is_list(Ids) ->
    maps:from_list([{Id, true} || Id <- Ids, is_integer(Id)]).

-spec list_value(term()) -> [term()].
list_value(Value) when is_list(Value) ->
    Value;
list_value(_) ->
    [].
