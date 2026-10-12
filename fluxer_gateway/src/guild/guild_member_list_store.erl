%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_store).
-typing([eqwalizer]).

-export([prepare_member_tuples/2, prepare_hoisted_role_ids/2, extract_role_ids/1]).

-type store_ref() :: ets:table().
-type member_tuple() :: {integer(), binary(), [integer()], boolean()}.
-type guild_state() :: #{member_presence := ets:tid() | map(), term() => term()}.

-export_type([store_ref/0, member_tuple/0, guild_state/0]).

-spec prepare_member_tuples(map(), guild_state()) -> [member_tuple()].
prepare_member_tuples(MemberMap, State) ->
    ConnectedUserIds = guild_member_list_common:connected_session_user_ids(State),
    PresenceTab = maps:get(member_presence, State),
    maps:fold(
        fun(UserId, Member, Acc) ->
            member_tuple(UserId, Member, PresenceTab, ConnectedUserIds, Acc)
        end,
        [],
        MemberMap
    ).

-spec member_tuple(integer(), map(), ets:tid() | map(), sets:set(integer()), [member_tuple()]) ->
    [member_tuple()].
member_tuple(UserId, Member, PresenceTab, ConnectedUserIds, Acc) ->
    DisplayName = guild_member_list_common:get_member_display_name(Member),
    SortKey = guild_member_list_common:casefold_binary(DisplayName),
    RoleIds = extract_role_ids(Member),
    Presence = guild_state_member:lookup_presence(PresenceTab, UserId),
    Status = maps:get(<<"status">>, Presence, <<"offline">>),
    IsConnected = sets:is_element(UserId, ConnectedUserIds),
    IsOnline =
        IsConnected andalso Status =/= <<"offline">> andalso Status =/= <<"invisible">>,
    [{UserId, SortKey, RoleIds, IsOnline} | Acc].

-spec prepare_hoisted_role_ids([map()], integer() | undefined) -> [integer()].
prepare_hoisted_role_ids(Roles, GuildId) ->
    HoistedRoles = guild_member_list_groups:get_hoisted_roles_sorted(Roles, GuildId),
    [
        Id
     || Role <- HoistedRoles,
        Id <- [snowflake_id:parse_optional(maps:get(<<"id">>, Role, undefined))],
        is_integer(Id),
        Id > 0
    ].

-spec extract_role_ids(map()) -> [integer()].
extract_role_ids(Member) ->
    RawRoles = map_utils:ensure_list(
        maps:get(<<"roles">>, Member, [])
    ),
    lists:filtermap(fun filtermap_snowflake/1, RawRoles).

-spec filtermap_snowflake(term()) -> {true, integer()} | false.
filtermap_snowflake(RoleId) ->
    case snowflake_id:parse_optional(RoleId) of
        Id when is_integer(Id), Id > 0 -> {true, Id};
        _ -> false
    end.
