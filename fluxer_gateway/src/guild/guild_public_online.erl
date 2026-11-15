%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_public_online).

-typing([eqwalizer]).

-export([compute_count/1]).

-export_type([guild_state/0]).

-type guild_state() :: map().
-type channel() :: map().
-type user_id() :: integer().

-define(ALL_PERMS_VIEWER_KEY, public_online_all_perms_viewer).

-spec compute_count(guild_state()) -> non_neg_integer().
compute_count(State) ->
    case resolve_data(State) of
        undefined ->
            0;
        Data ->
            compute_count_for_data(Data, State)
    end.

-spec compute_count_for_data(map(), guild_state()) -> non_neg_integer().
compute_count_for_data(Data, State) ->
    Channels = [
        C
     || C <- ensure_list(maps:get(<<"channels">>, Data, [])),
        is_map(C),
        not channel_types:is_thread_only(maps:get(<<"type">>, C, undefined))
    ],
    case classify_everyone_viewable(Channels, State) of
        {open, _ChannelIds} -> guild_member_list_read:get_online_count(State);
        {restricted, ChannelIdSet} -> count_online_with_access(ChannelIdSet, State);
        empty -> 0
    end.

-spec resolve_data(guild_state()) -> map() | undefined.
resolve_data(State) ->
    case maps:get(data, State, undefined) of
        D when is_map(D) -> D;
        _ -> undefined
    end.

-spec classify_everyone_viewable([channel()], guild_state()) ->
    {open, [integer()]} | {restricted, sets:set(integer())} | empty.
classify_everyone_viewable(Channels, State) ->
    EveryoneMember = #{<<"user">> => #{<<"id">> => <<"0">>}, <<"roles">> => []},
    ViewBit = constants:view_channel_permission(),
    {OpenAcc, RestrictedAcc} = lists:foldl(
        fun(Channel, {OpenIds, RestrictedIds}) ->
            classify_channel_visibility(Channel, EveryoneMember, ViewBit, State, {
                OpenIds, RestrictedIds
            })
        end,
        {[], []},
        Channels
    ),
    case {OpenAcc, RestrictedAcc} of
        {[], []} -> empty;
        {[_ | _], _} -> {open, OpenAcc};
        {[], _} -> classify_restricted(RestrictedAcc)
    end.

-spec classify_restricted([integer()]) ->
    {open, [integer()]} | {restricted, sets:set(integer())}.
classify_restricted(RestrictedIds) ->
    case application:get_env(fluxer_gateway, ?ALL_PERMS_VIEWER_KEY, false) of
        true -> {open, RestrictedIds};
        _ -> {restricted, sets:from_list(RestrictedIds)}
    end.

-spec classify_channel_visibility(
    channel(), map(), integer(), guild_state(), {[integer()], [integer()]}
) -> {[integer()], [integer()]}.
classify_channel_visibility(Channel, EveryoneMember, ViewBit, State, {OpenIds, RestrictedIds}) ->
    case channel_id(Channel) of
        ChannelId when is_integer(ChannelId), ChannelId > 0 ->
            classify_valid_channel(ChannelId, Channel, EveryoneMember, ViewBit, State, {
                OpenIds, RestrictedIds
            });
        _ ->
            {OpenIds, RestrictedIds}
    end.

-spec classify_valid_channel(
    integer(), channel(), map(), integer(), guild_state(), {[integer()], [integer()]}
) -> {[integer()], [integer()]}.
classify_valid_channel(ChannelId, Channel, EveryoneMember, ViewBit, State, Acc) ->
    case
        guild_permissions_check:can_view_channel_by_permissions(
            0, ChannelId, EveryoneMember, State
        )
    of
        true -> classify_visible_channel(ChannelId, Channel, ViewBit, Acc);
        false -> Acc
    end.

-spec classify_visible_channel(integer(), channel(), integer(), {[integer()], [integer()]}) ->
    {[integer()], [integer()]}.
classify_visible_channel(ChannelId, Channel, ViewBit, {OpenIds, RestrictedIds}) ->
    case channel_has_view_restricting_overrides(Channel, ViewBit) of
        false -> {[ChannelId | OpenIds], RestrictedIds};
        true -> {OpenIds, [ChannelId | RestrictedIds]}
    end.

-spec channel_has_view_restricting_overrides(channel(), integer()) -> boolean().
channel_has_view_restricting_overrides(Channel, ViewBit) ->
    Overwrites =
        case maps:get(<<"permission_overwrites">>, Channel, []) of
            L when is_list(L) -> L;
            _ -> []
        end,
    lists:any(
        fun
            (OW) when is_map(OW) ->
                Deny = bitset:parse(maps:get(<<"deny">>, OW, undefined)),
                Type = map_utils:get_integer(OW, <<"type">>, undefined),
                is_integer(Deny) andalso bitset:has(Deny, ViewBit) andalso
                    (Type =:= 0 orelse Type =:= 1);
            (_) ->
                false
        end,
        Overwrites
    ).

-spec count_online_with_access(sets:set(integer()), guild_state()) -> non_neg_integer().
count_online_with_access(ChannelIdSet, State) ->
    TargetMap = maps:from_list([{Ch, true} || Ch <- sets:to_list(ChannelIdSet)]),
    Tab = maps:get(member_presence, State),
    sets:fold(
        fun(UserId, Acc) ->
            Presence = guild_state_member:lookup_presence(Tab, UserId),
            maybe_count_online_user(UserId, Presence, TargetMap, State, Acc)
        end,
        0,
        guild_member_list_connected:connected_session_user_ids(State)
    ).

-spec maybe_count_online_user(term(), term(), map(), guild_state(), non_neg_integer()) ->
    non_neg_integer().
maybe_count_online_user(UserId, Presence, TargetMap, State, Acc) when
    is_map(Presence), is_integer(UserId), UserId > 0
->
    case is_online(Presence) of
        true -> count_user_with_access(UserId, TargetMap, State, Acc);
        false -> Acc
    end;
maybe_count_online_user(_UserId, _Presence, _TargetMap, _State, Acc) ->
    Acc.

-spec count_user_with_access(user_id(), map(), guild_state(), non_neg_integer()) ->
    non_neg_integer().
count_user_with_access(UserId, TargetMap, State, Acc) ->
    case user_has_access(UserId, TargetMap, State) of
        true -> Acc + 1;
        false -> Acc
    end.

-spec user_has_access(user_id(), map(), guild_state()) -> boolean().
user_has_access(UserId, TargetMap, State) ->
    UserChannels = guild_visibility_channels:get_user_viewable_channels(UserId, State),
    lists:any(fun(Ch) -> maps:is_key(Ch, TargetMap) end, UserChannels).

-spec is_online(map()) -> boolean().
is_online(Presence) ->
    Status = maps:get(<<"status">>, Presence, <<"offline">>),
    Status =/= <<"offline">> andalso Status =/= <<"invisible">>.

-spec channel_id(map()) -> integer() | undefined.
channel_id(Channel) when is_map(Channel) ->
    snowflake_id:parse_optional(maps:get(<<"id">>, Channel, undefined));
channel_id(_) ->
    undefined.

-spec ensure_list(term()) -> [term()].
ensure_list(L) when is_list(L) -> L;
ensure_list(M) when is_map(M) -> maps:values(M);
ensure_list(_) -> [].
