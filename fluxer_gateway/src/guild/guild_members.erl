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

-module(guild_members).

-export([get_users_to_mention_by_roles/2]).
-export([get_users_to_mention_by_user_ids/2]).
-export([get_all_users_to_mention/2]).
-export([resolve_all_mentions/2]).
-export([get_members_with_role/2]).
-export([can_manage_roles/2]).
-export([can_manage_role/2]).
-export([get_assignable_roles/2]).
-export([check_target_member/2]).
-export([get_viewable_channels/2]).

-type guild_state() :: map().
-type guild_reply(T) :: {reply, T, guild_state()}.
-type member() :: map().
-type role() :: map().
-type channel() :: map().
-type user_id() :: integer().
-type role_id() :: integer().
-type channel_id() :: integer().

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec get_users_to_mention_by_roles(map(), guild_state()) -> guild_reply(map()).
get_users_to_mention_by_roles(
    #{channel_id := ChannelId, role_ids := RoleIds, author_id := AuthorId}, State
) ->
    Members = guild_members(State),
    RoleIdSet = normalize_int_list(RoleIds),
    UserIds = collect_mentions(
        Members,
        AuthorId,
        ChannelId,
        State,
        fun(Member) -> member_has_any_role(Member, RoleIdSet) end
    ),
    {reply, #{user_ids => UserIds}, State}.

-spec get_users_to_mention_by_user_ids(map(), guild_state()) -> guild_reply(map()).
get_users_to_mention_by_user_ids(
    #{channel_id := ChannelId, user_ids := UserIdsReq, author_id := AuthorId}, State
) ->
    Members = guild_members(State),
    TargetIds = normalize_int_list(UserIdsReq),
    UserIds = collect_mentions(
        Members,
        AuthorId,
        ChannelId,
        State,
        fun(Member) ->
            case member_user_id(Member) of
                undefined -> false;
                Id -> lists:member(Id, TargetIds)
            end
        end
    ),
    {reply, #{user_ids => UserIds}, State}.

-spec get_all_users_to_mention(map(), guild_state()) -> guild_reply(map()).
get_all_users_to_mention(#{channel_id := ChannelId, author_id := AuthorId}, State) ->
    Members = guild_members(State),
    UserIds = collect_mentions(Members, AuthorId, ChannelId, State, fun(_) -> true end),
    {reply, #{user_ids => UserIds}, State}.

-spec resolve_all_mentions(map(), guild_state()) -> guild_reply(map()).
resolve_all_mentions(
    #{
        channel_id := ChannelId,
        author_id := AuthorId,
        mention_everyone := MentionEveryone,
        mention_here := MentionHere,
        role_ids := RoleIds,
        user_ids := DirectUserIds
    },
    State
) ->
    Members = guild_members(State),
    Sessions = maps:get(sessions, State, #{}),

    RoleIdSet = gb_sets:from_list(normalize_int_list(RoleIds)),
    DirectUserIdSet = gb_sets:from_list(normalize_int_list(DirectUserIds)),
    HasRoleMentions = not gb_sets:is_empty(RoleIdSet),
    HasDirectMentions = not gb_sets:is_empty(DirectUserIdSet),

    ConnectedUserIds =
        case MentionHere of
            true ->
                gb_sets:from_list([
                    maps:get(user_id, S)
                 || {_Sid, S} <- maps:to_list(Sessions)
                ]);
            false ->
                gb_sets:empty()
        end,

    UserIds = lists:filtermap(
        fun(Member) ->
            case member_user_id(Member) of
                undefined ->
                    false;
                UserId when UserId =:= AuthorId ->
                    false;
                UserId ->
                    case is_member_bot(Member) of
                        true ->
                            false;
                        false ->
                            ShouldMention =
                                MentionEveryone orelse
                                    (MentionHere andalso
                                        gb_sets:is_member(UserId, ConnectedUserIds)) orelse
                                    (HasRoleMentions andalso
                                        member_has_any_role_set(Member, RoleIdSet)) orelse
                                    (HasDirectMentions andalso
                                        gb_sets:is_member(UserId, DirectUserIdSet)),
                            case
                                ShouldMention andalso
                                    member_can_view_channel(UserId, ChannelId, Member, State)
                            of
                                true -> {true, UserId};
                                false -> false
                            end
                    end
            end
        end,
        Members
    ),
    {reply, #{user_ids => UserIds}, State}.

-spec get_members_with_role(map(), guild_state()) -> guild_reply(map()).
get_members_with_role(#{role_id := RoleId}, State) ->
    Members = guild_members(State),
    TargetRoles = [RoleId],
    UserIds = lists:filtermap(
        fun(Member) ->
            case member_user_id(Member) of
                undefined ->
                    false;
                UserId ->
                    case member_has_any_role(Member, TargetRoles) of
                        true -> {true, UserId};
                        false -> false
                    end
            end
        end,
        Members
    ),
    {reply, #{user_ids => UserIds}, State}.

-spec can_manage_roles(map(), guild_state()) -> guild_reply(map()).
can_manage_roles(#{user_id := UserId, role_id := RoleId}, State) ->
    Data = guild_data(State),
    OwnerId = owner_id(State),
    Reply =
        if
            UserId =:= OwnerId ->
                true;
            true ->
                UserPermissions = guild_permissions:get_member_permissions(
                    UserId, undefined, State
                ),
                case (UserPermissions band constants:manage_roles_permission()) =/= 0 of
                    false ->
                        false;
                    true ->
                        Roles = maps:get(<<"roles">>, Data, []),
                        case find_role_by_id(RoleId, Roles) of
                            undefined ->
                                false;
                            Role ->
                                UserMax = guild_permissions:get_max_role_position(UserId, State),
                                UserMax > role_position(Role)
                        end
                end
        end,
    {reply, #{can_manage => Reply}, State}.

-spec can_manage_role(map(), guild_state()) -> guild_reply(map()).
can_manage_role(#{user_id := UserId, role_id := RoleId}, State) ->
    Data = guild_data(State),
    Roles = maps:get(<<"roles">>, Data, []),
    Reply =
        case find_role_by_id(RoleId, Roles) of
            undefined ->
                false;
            Role ->
                UserMax = guild_permissions:get_max_role_position(UserId, State),
                RolePos = role_position(Role),
                UserMax > RolePos orelse
                    (UserMax =:= RolePos andalso
                        compare_role_ids_for_equal_position(UserId, RoleId, State))
        end,
    {reply, #{can_manage => Reply}, State}.

compare_role_ids_for_equal_position(UserId, TargetRoleId, State) ->
    case guild_permissions:find_member_by_user_id(UserId, State) of
        undefined ->
            false;
        Member ->
            MemberRoles = member_roles(Member),
            Data = guild_data(State),
            Roles = maps:get(<<"roles">>, Data, []),
            UserHighestRole = get_highest_role(MemberRoles, Roles),
            case UserHighestRole of
                undefined ->
                    false;
                HighestRole ->
                    HighestRoleId = map_utils:get_integer(HighestRole, <<"id">>, 0),
                    HighestRoleId < TargetRoleId
            end
    end.

get_highest_role(MemberRoleIds, Roles) ->
    lists:foldl(
        fun(RoleId, Acc) ->
            case find_role_by_id(RoleId, Roles) of
                undefined ->
                    Acc;
                Role ->
                    case Acc of
                        undefined ->
                            Role;
                        AccRole ->
                            AccPos = role_position(AccRole),
                            RolePos = role_position(Role),
                            if
                                RolePos > AccPos ->
                                    Role;
                                RolePos =:= AccPos ->
                                    AccId = map_utils:get_integer(AccRole, <<"id">>, 0),
                                    RId = map_utils:get_integer(Role, <<"id">>, 0),
                                    if
                                        RId < AccId -> Role;
                                        true -> AccRole
                                    end;
                                true ->
                                    AccRole
                            end
                    end
            end
        end,
        undefined,
        MemberRoleIds
    ).

-spec get_assignable_roles(map(), guild_state()) -> guild_reply(map()).
get_assignable_roles(#{user_id := UserId}, State) ->
    Roles = guild_roles(State),
    OwnerId = owner_id(State),
    RoleIds = get_assignable_role_ids(UserId, OwnerId, Roles, State),
    {reply, #{role_ids => RoleIds}, State}.

get_assignable_role_ids(OwnerId, OwnerId, Roles, _State) ->
    role_ids_from_roles(Roles);
get_assignable_role_ids(UserId, _OwnerId, Roles, State) ->
    UserMaxPosition = guild_permissions:get_max_role_position(UserId, State),
    lists:filtermap(
        fun(Role) -> filter_assignable_role(Role, UserMaxPosition) end,
        Roles
    ).

filter_assignable_role(Role, UserMaxPosition) ->
    case role_position(Role) < UserMaxPosition of
        true ->
            case map_utils:get_integer(Role, <<"id">>, undefined) of
                undefined -> false;
                RoleId -> {true, RoleId}
            end;
        false ->
            false
    end.

-spec check_target_member(map(), guild_state()) -> guild_reply(map()).
check_target_member(#{user_id := UserId, target_user_id := TargetUserId}, State) ->
    OwnerId = owner_id(State),
    CanManage =
        if
            UserId =:= OwnerId ->
                true;
            TargetUserId =:= OwnerId ->
                false;
            true ->
                UserMaxPos = guild_permissions:get_max_role_position(UserId, State),
                TargetMaxPos = guild_permissions:get_max_role_position(TargetUserId, State),
                UserMaxPos > TargetMaxPos
        end,
    {reply, #{can_manage => CanManage}, State}.

-spec get_viewable_channels(map(), guild_state()) -> guild_reply(map()).
get_viewable_channels(#{user_id := UserId}, State) ->
    Channels = guild_channels(State),
    case find_member_by_user_id(UserId, State) of
        undefined ->
            {reply, #{channel_ids => []}, State};
        Member ->
            ChannelIds = lists:filtermap(
                fun(Channel) ->
                    ChannelId = map_utils:get_integer(Channel, <<"id">>, undefined),
                    case ChannelId of
                        undefined ->
                            false;
                        _ ->
                            case
                                guild_permissions:can_view_channel(UserId, ChannelId, Member, State)
                            of
                                true -> {true, ChannelId};
                                false -> false
                            end
                    end
                end,
                Channels
            ),
            {reply, #{channel_ids => ChannelIds}, State}
    end.

find_member_by_user_id(UserId, State) ->
    guild_permissions:find_member_by_user_id(UserId, State).

find_role_by_id(RoleId, Roles) ->
    guild_permissions:find_role_by_id(RoleId, Roles).

-spec guild_data(guild_state()) -> map().
guild_data(State) ->
    map_utils:ensure_map(map_utils:get_safe(State, data, #{})).

-spec guild_members(guild_state()) -> [member()].
guild_members(State) ->
    map_utils:ensure_list(maps:get(<<"members">>, guild_data(State), [])).

-spec guild_roles(guild_state()) -> [role()].
guild_roles(State) ->
    map_utils:ensure_list(maps:get(<<"roles">>, guild_data(State), [])).

-spec guild_channels(guild_state()) -> [channel()].
guild_channels(State) ->
    map_utils:ensure_list(maps:get(<<"channels">>, guild_data(State), [])).

-spec owner_id(guild_state()) -> user_id().
owner_id(State) ->
    Guild = map_utils:ensure_map(maps:get(<<"guild">>, guild_data(State), #{})),
    map_utils:get_integer(Guild, <<"owner_id">>, 0).

-spec member_user_id(member()) -> user_id() | undefined.
member_user_id(Member) ->
    User = map_utils:ensure_map(maps:get(<<"user">>, Member, #{})),
    map_utils:get_integer(User, <<"id">>, undefined).

-spec member_roles(member()) -> [role_id()].
member_roles(Member) ->
    normalize_int_list(map_utils:ensure_list(maps:get(<<"roles">>, Member, []))).

-spec member_has_any_role(member(), [role_id()]) -> boolean().
member_has_any_role(Member, RoleIds) ->
    MemberRoles = member_roles(Member),
    lists:any(fun(RoleId) -> lists:member(RoleId, MemberRoles) end, RoleIds).

-spec member_has_any_role_set(member(), gb_sets:set(role_id())) -> boolean().
member_has_any_role_set(Member, RoleIdSet) ->
    MemberRoles = member_roles(Member),
    lists:any(fun(RoleId) -> gb_sets:is_member(RoleId, RoleIdSet) end, MemberRoles).

-spec is_member_bot(member()) -> boolean().
is_member_bot(Member) ->
    User = map_utils:ensure_map(maps:get(<<"user">>, Member, #{})),
    maps:get(<<"bot">>, User, false) =:= true.

-spec member_can_view_channel(user_id(), channel_id(), member(), guild_state()) -> boolean().
member_can_view_channel(UserId, ChannelId, Member, State) when is_integer(ChannelId) ->
    guild_permissions:can_view_channel(UserId, ChannelId, Member, State);
member_can_view_channel(_, _, _, _) ->
    false.

-spec collect_mentions([member()], user_id(), channel_id(), guild_state(), fun(
    (member()) -> boolean()
)) ->
    [user_id()].
collect_mentions(Members, AuthorId, ChannelId, State, Predicate) ->
    lists:filtermap(
        fun(Member) ->
            case member_user_id(Member) of
                undefined ->
                    false;
                UserId when UserId =:= AuthorId -> false;
                UserId ->
                    case
                        Predicate(Member) andalso
                            member_can_view_channel(UserId, ChannelId, Member, State)
                    of
                        true -> {true, UserId};
                        false -> false
                    end
            end
        end,
        Members
    ).

-spec normalize_int_list(list()) -> [integer()].
normalize_int_list(List) ->
    lists:reverse(
        lists:foldl(
            fun(Value, Acc) ->
                case type_conv:to_integer(Value) of
                    undefined -> Acc;
                    Int -> [Int | Acc]
                end
            end,
            [],
            map_utils:ensure_list(List)
        )
    ).

-spec role_ids_from_roles([role()]) -> [role_id()].
role_ids_from_roles(Roles) ->
    lists:filtermap(
        fun(Role) ->
            case map_utils:get_integer(Role, <<"id">>, undefined) of
                undefined -> false;
                RoleId -> {true, RoleId}
            end
        end,
        Roles
    ).

-spec role_position(role()) -> integer().
role_position(Role) ->
    maps:get(<<"position">>, Role, 0).

-ifdef(TEST).

get_users_to_mention_by_roles_basic_test() ->
    State = test_state(),
    ChannelId = 500,
    RoleMod = 200,
    Request = #{channel_id => ChannelId, role_ids => [RoleMod], author_id => 1},
    {reply, #{user_ids := UserIds}, _} = get_users_to_mention_by_roles(Request, State),
    ?assertEqual([2], UserIds).

get_assignable_roles_owner_test() ->
    State = test_state(),
    {reply, #{role_ids := RoleIds}, _} = get_assignable_roles(#{user_id => 1}, State),
    ?assertEqual(lists:sort([100, 200, 201]), lists:sort(RoleIds)).

get_assignable_roles_member_test() ->
    State = test_state(),
    {reply, #{role_ids := RoleIds}, _} = get_assignable_roles(#{user_id => 2}, State),
    ?assertEqual([100], RoleIds).

get_viewable_channels_filters_test() ->
    State = test_state(),
    {reply, #{channel_ids := ChannelIds}, _} = get_viewable_channels(#{user_id => 2}, State),
    ?assert(lists:member(500, ChannelIds)).

test_state() ->
    GuildId = 100,
    OwnerId = 1,
    MemberId = 2,
    OtherId = 3,
    ChannelId = 500,
    RoleMod = 200,
    RoleHigh = 201,
    ViewPerm = constants:view_channel_permission(),
    ManageRoles = constants:manage_roles_permission(),
    #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => integer_to_binary(OwnerId)},
            <<"roles">> => [
                #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"permissions">> => integer_to_binary(ViewPerm bor ManageRoles),
                    <<"position">> => 0
                },
                #{
                    <<"id">> => integer_to_binary(RoleMod),
                    <<"permissions">> => integer_to_binary(ViewPerm),
                    <<"position">> => 10
                },
                #{
                    <<"id">> => integer_to_binary(RoleHigh),
                    <<"permissions">> => integer_to_binary(ViewPerm),
                    <<"position">> => 20
                }
            ],
            <<"channels">> => [
                #{
                    <<"id">> => integer_to_binary(ChannelId),
                    <<"type">> => 0,
                    <<"permission_overwrites">> => []
                },
                #{
                    <<"id">> => integer_to_binary(ChannelId + 1),
                    <<"type">> => 2,
                    <<"permission_overwrites">> => []
                }
            ],
            <<"members">> => [
                #{
                    <<"user">> => #{<<"id">> => integer_to_binary(OwnerId)},
                    <<"roles">> => [integer_to_binary(GuildId)]
                },
                #{
                    <<"user">> => #{<<"id">> => integer_to_binary(MemberId)},
                    <<"roles">> => [integer_to_binary(RoleMod)],
                    <<"joined_at">> => <<"2024-01-01T00:00:00Z">>
                },
                #{
                    <<"user">> => #{<<"id">> => integer_to_binary(OtherId)},
                    <<"roles">> => [integer_to_binary(RoleHigh)],
                    <<"joined_at">> => <<"2024-01-02T00:00:00Z">>
                }
            ]
        }
    }.

-endif.
