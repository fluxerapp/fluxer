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

-export([
    get_users_to_mention_by_roles/2,
    get_users_to_mention_by_user_ids/2,
    get_all_users_to_mention/2,
    resolve_all_mentions/2,
    get_members_with_role/2,
    can_manage_roles/2,
    can_manage_role/2,
    get_assignable_roles/2,
    check_target_member/2,
    get_viewable_channels/2
]).

-type guild_state() :: map().
-type guild_reply(T) :: {reply, T, guild_state()}.
-type member() :: map().
-type role() :: map().
-type channel() :: map().
-type user_id() :: integer().
-type role_id() :: integer().
-type channel_id() :: integer().

-spec get_users_to_mention_by_roles(map(), guild_state()) -> guild_reply(map()).
get_users_to_mention_by_roles(
    #{channel_id := ChannelId, role_ids := RoleIds, author_id := AuthorId}, State
) ->
    RoleIdList = normalize_int_list(RoleIds),
    CandidateUserIds = user_ids_for_any_role(RoleIdList, State),
    UserIds = collect_mentions_for_user_ids(
        CandidateUserIds,
        AuthorId,
        ChannelId,
        State,
        fun(_UserId, _Member) -> true end
    ),
    {reply, #{user_ids => UserIds}, State}.

-spec get_users_to_mention_by_user_ids(map(), guild_state()) -> guild_reply(map()).
get_users_to_mention_by_user_ids(
    #{channel_id := ChannelId, user_ids := UserIdsReq, author_id := AuthorId}, State
) ->
    TargetIds = normalize_int_list(UserIdsReq),
    UserIds = collect_mentions_for_user_ids(
        TargetIds,
        AuthorId,
        ChannelId,
        State,
        fun(_UserId, _Member) -> true end
    ),
    {reply, #{user_ids => UserIds}, State}.

-spec get_all_users_to_mention(map(), guild_state()) -> guild_reply(map()).
get_all_users_to_mention(#{channel_id := ChannelId, author_id := AuthorId}, State) ->
    Members = guild_members(State),
    UserIds = collect_mentions(Members, AuthorId, ChannelId, State, fun(_) -> true end),
    {reply, #{user_ids => UserIds}, State}.

-spec resolve_all_mentions(map(), guild_state()) -> guild_reply(map()).
resolve_all_mentions(Request, State) ->
    #{
        channel_id := ChannelId,
        author_id := AuthorId,
        mention_everyone := MentionEveryone,
        mention_here := MentionHere,
        role_ids := RoleIds,
        user_ids := DirectUserIds
    } = Request,
    Members = guild_members(State),
    MemberMap = guild_data_index:member_map(guild_data(State)),
    Sessions = maps:get(sessions, State, #{}),
    RoleIdSet = gb_sets:from_list(normalize_int_list(RoleIds)),
    DirectUserIdSet = gb_sets:from_list(normalize_int_list(DirectUserIds)),
    HasRoleMentions = not gb_sets:is_empty(RoleIdSet),
    HasDirectMentions = not gb_sets:is_empty(DirectUserIdSet),
    ConnectedUserIds = build_connected_user_ids(MentionHere, Sessions),
    UserIds =
        case MentionEveryone of
            true ->
                resolve_mentions(
                    Members,
                    AuthorId,
                    ChannelId,
                    MentionEveryone,
                    MentionHere,
                    HasRoleMentions,
                    HasDirectMentions,
                    RoleIdSet,
                    DirectUserIdSet,
                    ConnectedUserIds,
                    State
                );
            false ->
                CandidateUserIds = candidate_user_ids_for_mentions(
                    MentionHere,
                    HasRoleMentions,
                    HasDirectMentions,
                    RoleIdSet,
                    DirectUserIdSet,
                    ConnectedUserIds,
                    State
                ),
                resolve_mentions_for_user_ids(
                    CandidateUserIds,
                    MemberMap,
                    AuthorId,
                    ChannelId,
                    MentionEveryone,
                    MentionHere,
                    HasRoleMentions,
                    HasDirectMentions,
                    RoleIdSet,
                    DirectUserIdSet,
                    ConnectedUserIds,
                    State
                )
        end,
    {reply, #{user_ids => UserIds}, State}.

-spec build_connected_user_ids(boolean(), map()) -> gb_sets:set(user_id()).
build_connected_user_ids(false, _Sessions) ->
    gb_sets:empty();
build_connected_user_ids(true, Sessions) ->
    gb_sets:from_list(
        lists:filtermap(
            fun({_Sid, SessionData}) ->
                case maps:get(user_id, SessionData, undefined) of
                    UserId when is_integer(UserId) -> {true, UserId};
                    _ -> false
                end
            end,
            maps:to_list(Sessions)
        )
    ).

-spec resolve_mentions(
    [member()],
    user_id(),
    channel_id(),
    boolean(),
    boolean(),
    boolean(),
    boolean(),
    gb_sets:set(),
    gb_sets:set(),
    gb_sets:set(),
    guild_state()
) -> [user_id()].
resolve_mentions(
    Members,
    AuthorId,
    ChannelId,
    MentionEveryone,
    MentionHere,
    HasRoleMentions,
    HasDirectMentions,
    RoleIdSet,
    DirectUserIdSet,
    ConnectedUserIds,
    State
) ->
    lists:filtermap(
        fun(Member) ->
            case member_user_id(Member) of
                undefined ->
                    false;
                UserId when UserId =:= AuthorId -> false;
                UserId ->
                    case is_member_bot(Member) of
                        true ->
                            false;
                        false ->
                            ShouldMention = check_should_mention(
                                UserId,
                                Member,
                                MentionEveryone,
                                MentionHere,
                                HasRoleMentions,
                                HasDirectMentions,
                                RoleIdSet,
                                DirectUserIdSet,
                                ConnectedUserIds
                            ),
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
    ).

-spec resolve_mentions_for_user_ids(
    [user_id()],
    #{user_id() => member()},
    user_id(),
    channel_id(),
    boolean(),
    boolean(),
    boolean(),
    boolean(),
    gb_sets:set(),
    gb_sets:set(),
    gb_sets:set(),
    guild_state()
) -> [user_id()].
resolve_mentions_for_user_ids(
    CandidateUserIds,
    MemberMap,
    AuthorId,
    ChannelId,
    MentionEveryone,
    MentionHere,
    HasRoleMentions,
    HasDirectMentions,
    RoleIdSet,
    DirectUserIdSet,
    ConnectedUserIds,
    State
) ->
    lists:filtermap(
        fun(UserId) ->
            case UserId =:= AuthorId of
                true ->
                    false;
                false ->
                    case maps:get(UserId, MemberMap, undefined) of
                        undefined ->
                            false;
                        Member ->
                            case is_member_bot(Member) of
                                true ->
                                    false;
                                false ->
                                    ShouldMention = check_should_mention(
                                        UserId,
                                        Member,
                                        MentionEveryone,
                                        MentionHere,
                                        HasRoleMentions,
                                        HasDirectMentions,
                                        RoleIdSet,
                                        DirectUserIdSet,
                                        ConnectedUserIds
                                    ),
                                    case
                                        ShouldMention andalso
                                            member_can_view_channel(UserId, ChannelId, Member, State)
                                    of
                                        true -> {true, UserId};
                                        false -> false
                                    end
                            end
                    end
            end
        end,
        lists:usort(CandidateUserIds)
    ).

-spec candidate_user_ids_for_mentions(
    boolean(),
    boolean(),
    boolean(),
    gb_sets:set(),
    gb_sets:set(),
    gb_sets:set(),
    guild_state()
) -> [user_id()].
candidate_user_ids_for_mentions(
    MentionHere,
    HasRoleMentions,
    HasDirectMentions,
    RoleIdSet,
    DirectUserIdSet,
    ConnectedUserIds,
    State
) ->
    HereSet =
        case MentionHere of
            true -> ConnectedUserIds;
            false -> gb_sets:empty()
        end,
    RoleUsersSet =
        case HasRoleMentions of
            true ->
                gb_sets:from_list(user_ids_for_any_role(gb_sets:to_list(RoleIdSet), State));
            false ->
                gb_sets:empty()
        end,
    DirectSet =
        case HasDirectMentions of
            true -> DirectUserIdSet;
            false -> gb_sets:empty()
        end,
    gb_sets:to_list(gb_sets:union(HereSet, gb_sets:union(RoleUsersSet, DirectSet))).

-spec user_ids_for_any_role([role_id()], guild_state()) -> [user_id()].
user_ids_for_any_role(RoleIds, State) ->
    Data = guild_data(State),
    MemberRoleIndex = guild_data_index:member_role_index(Data),
    RoleUserIds = lists:foldl(
        fun(RoleId, AccSet) ->
            case maps:get(RoleId, MemberRoleIndex, undefined) of
                undefined ->
                    AccSet;
                UserMap ->
                    lists:foldl(
                        fun(UserId, InnerSet) -> gb_sets:add(UserId, InnerSet) end,
                        AccSet,
                        maps:keys(UserMap)
                    )
            end
        end,
        gb_sets:empty(),
        RoleIds
    ),
    gb_sets:to_list(RoleUserIds).

-spec check_should_mention(
    user_id(),
    member(),
    boolean(),
    boolean(),
    boolean(),
    boolean(),
    gb_sets:set(),
    gb_sets:set(),
    gb_sets:set()
) -> boolean().
check_should_mention(
    UserId,
    Member,
    MentionEveryone,
    MentionHere,
    HasRoleMentions,
    HasDirectMentions,
    RoleIdSet,
    DirectUserIdSet,
    ConnectedUserIds
) ->
    MentionEveryone orelse
        (MentionHere andalso gb_sets:is_member(UserId, ConnectedUserIds)) orelse
        (HasRoleMentions andalso member_has_any_role_set(Member, RoleIdSet)) orelse
        (HasDirectMentions andalso gb_sets:is_member(UserId, DirectUserIdSet)).

-spec get_members_with_role(map(), guild_state()) -> guild_reply(map()).
get_members_with_role(#{role_id := RoleId}, State) ->
    Data = guild_data(State),
    MemberRoleIndex = guild_data_index:member_role_index(Data),
    TargetRoleId = type_conv:to_integer(RoleId),
    UserMap =
        case TargetRoleId of
            undefined ->
                #{};
            _ ->
                maps:get(TargetRoleId, MemberRoleIndex, #{})
        end,
    UserIds = lists:sort(maps:keys(UserMap)),
    {reply, #{user_ids => UserIds}, State}.

-spec can_manage_roles(map(), guild_state()) -> guild_reply(map()).
can_manage_roles(#{user_id := UserId, role_id := RoleId}, State) ->
    Data = guild_data(State),
    OwnerId = owner_id(State),
    Reply = check_can_manage_roles(UserId, RoleId, OwnerId, Data, State),
    {reply, #{can_manage => Reply}, State}.

-spec check_can_manage_roles(user_id(), role_id(), user_id(), map(), guild_state()) -> boolean().
check_can_manage_roles(UserId, _RoleId, UserId, _Data, _State) ->
    true;
check_can_manage_roles(UserId, RoleId, _OwnerId, Data, State) ->
    UserPermissions = guild_permissions:get_member_permissions(UserId, undefined, State),
    case (UserPermissions band constants:manage_roles_permission()) =/= 0 of
        false ->
            false;
        true ->
            Roles = guild_data_index:role_index(Data),
            case find_role_by_id(RoleId, Roles) of
                undefined ->
                    false;
                Role ->
                    UserMax = guild_permissions:get_max_role_position(UserId, State),
                    UserMax > role_position(Role)
            end
    end.

-spec can_manage_role(map(), guild_state()) -> guild_reply(map()).
can_manage_role(#{user_id := UserId, role_id := RoleId}, State) ->
    Data = guild_data(State),
    Roles = guild_data_index:role_index(Data),
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

-spec compare_role_ids_for_equal_position(user_id(), role_id(), guild_state()) -> boolean().
compare_role_ids_for_equal_position(UserId, TargetRoleId, State) ->
    case guild_permissions:find_member_by_user_id(UserId, State) of
        undefined ->
            false;
        Member ->
            MemberRoles = member_roles(Member),
            Data = guild_data(State),
            Roles = guild_data_index:role_index(Data),
            case get_highest_role(MemberRoles, Roles) of
                undefined ->
                    false;
                HighestRole ->
                    HighestRoleId = map_utils:get_integer(HighestRole, <<"id">>, 0),
                    HighestRoleId < TargetRoleId
            end
    end.

-spec get_highest_role([role_id()], [role()] | map()) -> role() | undefined.
get_highest_role(MemberRoleIds, Roles) ->
    lists:foldl(
        fun(RoleId, Acc) ->
            case find_role_by_id(RoleId, Roles) of
                undefined -> Acc;
                Role -> compare_roles(Role, Acc)
            end
        end,
        undefined,
        MemberRoleIds
    ).

-spec compare_roles(role(), role() | undefined) -> role().
compare_roles(Role, undefined) ->
    Role;
compare_roles(Role, AccRole) ->
    AccPos = role_position(AccRole),
    RolePos = role_position(Role),
    case RolePos > AccPos of
        true ->
            Role;
        false ->
            case RolePos =:= AccPos of
                true ->
                    AccId = map_utils:get_integer(AccRole, <<"id">>, 0),
                    RId = map_utils:get_integer(Role, <<"id">>, 0),
                    case RId < AccId of
                        true -> Role;
                        false -> AccRole
                    end;
                false ->
                    AccRole
            end
    end.

-spec get_assignable_roles(map(), guild_state()) -> guild_reply(map()).
get_assignable_roles(#{user_id := UserId}, State) ->
    Roles = guild_roles(State),
    OwnerId = owner_id(State),
    RoleIds = get_assignable_role_ids(UserId, OwnerId, Roles, State),
    {reply, #{role_ids => RoleIds}, State}.

-spec get_assignable_role_ids(user_id(), user_id(), [role()], guild_state()) -> [role_id()].
get_assignable_role_ids(OwnerId, OwnerId, Roles, _State) ->
    role_ids_from_roles(Roles);
get_assignable_role_ids(UserId, _OwnerId, Roles, State) ->
    UserMaxPosition = guild_permissions:get_max_role_position(UserId, State),
    lists:filtermap(
        fun(Role) -> filter_assignable_role(Role, UserMaxPosition) end,
        Roles
    ).

-spec filter_assignable_role(role(), integer()) -> {true, role_id()} | false.
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
    CanManage = check_can_manage_target(UserId, TargetUserId, OwnerId, State),
    {reply, #{can_manage => CanManage}, State}.

-spec check_can_manage_target(user_id(), user_id(), user_id(), guild_state()) -> boolean().
check_can_manage_target(UserId, _TargetUserId, UserId, _State) ->
    true;
check_can_manage_target(_UserId, OwnerId, OwnerId, _State) ->
    false;
check_can_manage_target(UserId, TargetUserId, _OwnerId, State) ->
    UserMaxPos = guild_permissions:get_max_role_position(UserId, State),
    TargetMaxPos = guild_permissions:get_max_role_position(TargetUserId, State),
    UserMaxPos > TargetMaxPos.

-spec get_viewable_channels(map(), guild_state()) -> guild_reply(map()).
get_viewable_channels(#{user_id := UserId}, State) ->
    Channels = guild_channels(State),
    case find_member_by_user_id(UserId, State) of
        undefined ->
            {reply, #{channel_ids => []}, State};
        Member ->
            ChannelIds = filter_viewable_channels(Channels, UserId, Member, State),
            {reply, #{channel_ids => ChannelIds}, State}
    end.

-spec filter_viewable_channels([channel()], user_id(), member(), guild_state()) -> [channel_id()].
filter_viewable_channels(Channels, UserId, Member, State) ->
    lists:filtermap(
        fun(Channel) ->
            ChannelId = map_utils:get_integer(Channel, <<"id">>, undefined),
            case ChannelId of
                undefined ->
                    false;
                _ ->
                    case guild_permissions:can_view_channel(UserId, ChannelId, Member, State) of
                        true -> {true, ChannelId};
                        false -> false
                    end
            end
        end,
        Channels
    ).

-spec find_member_by_user_id(user_id(), guild_state()) -> member() | undefined.
find_member_by_user_id(UserId, State) ->
    guild_permissions:find_member_by_user_id(UserId, State).

-spec find_role_by_id(role_id(), [role()] | map()) -> role() | undefined.
find_role_by_id(RoleId, Roles) ->
    guild_permissions:find_role_by_id(RoleId, Roles).

-spec guild_data(guild_state()) -> map().
guild_data(State) ->
    map_utils:ensure_map(map_utils:get_safe(State, data, #{})).

-spec guild_members(guild_state()) -> [member()].
guild_members(State) ->
    guild_data_index:member_values(guild_data(State)).

-spec guild_roles(guild_state()) -> [role()].
guild_roles(State) ->
    guild_data_index:role_list(guild_data(State)).

-spec guild_channels(guild_state()) -> [channel()].
guild_channels(State) ->
    guild_data_index:channel_list(guild_data(State)).

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

-spec collect_mentions_for_user_ids(
    [user_id()],
    user_id(),
    channel_id(),
    guild_state(),
    fun((user_id(), member()) -> boolean())
) ->
    [user_id()].
collect_mentions_for_user_ids(UserIds, AuthorId, ChannelId, State, Predicate) ->
    MemberMap = guild_data_index:member_map(guild_data(State)),
    lists:filtermap(
        fun(UserId) ->
            case UserId =:= AuthorId of
                true ->
                    false;
                false ->
                    case maps:get(UserId, MemberMap, undefined) of
                        undefined ->
                            false;
                        Member ->
                            case
                                Predicate(UserId, Member) andalso
                                    member_can_view_channel(UserId, ChannelId, Member, State)
                            of
                                true -> {true, UserId};
                                false -> false
                            end
                    end
            end
        end,
        lists:usort(UserIds)
    ).

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
-include_lib("eunit/include/eunit.hrl").

get_users_to_mention_by_roles_basic_test() ->
    State = test_state(),
    ChannelId = 500,
    RoleMod = 200,
    Request = #{channel_id => ChannelId, role_ids => [RoleMod], author_id => 1},
    {reply, #{user_ids := UserIds}, _} = get_users_to_mention_by_roles(Request, State),
    ?assertEqual([2], UserIds).

get_users_to_mention_by_user_ids_filters_unknown_members_test() ->
    State = test_state(),
    Request = #{channel_id => 500, user_ids => [2, 999], author_id => 1},
    {reply, #{user_ids := UserIds}, _} = get_users_to_mention_by_user_ids(Request, State),
    ?assertEqual([2], UserIds).

get_members_with_role_uses_member_role_index_test() ->
    State = test_state(),
    {reply, #{user_ids := UserIds}, _} = get_members_with_role(#{role_id => 200}, State),
    ?assertEqual([2], UserIds).

resolve_all_mentions_merges_role_and_direct_candidates_test() ->
    State = test_state(),
    Request = #{
        channel_id => 500,
        author_id => 1,
        mention_everyone => false,
        mention_here => false,
        role_ids => [200],
        user_ids => [3]
    },
    {reply, #{user_ids := UserIds}, _} = resolve_all_mentions(Request, State),
    ?assertEqual([2, 3], UserIds).

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

member_has_any_role_set_test() ->
    Member = #{<<"roles">> => [<<"100">>, <<"200">>]},
    RoleSet = gb_sets:from_list([200]),
    MissingRoleSet = gb_sets:from_list([999]),
    ?assertEqual(true, member_has_any_role_set(Member, RoleSet)),
    ?assertEqual(false, member_has_any_role_set(Member, MissingRoleSet)).

is_member_bot_test() ->
    BotMember = #{<<"user">> => #{<<"bot">> => true}},
    HumanMember = #{<<"user">> => #{<<"bot">> => false}},
    ?assertEqual(true, is_member_bot(BotMember)),
    ?assertEqual(false, is_member_bot(HumanMember)).

normalize_int_list_test() ->
    ?assertEqual([1, 2, 3], normalize_int_list([<<"1">>, <<"2">>, <<"3">>])),
    ?assertEqual([1, 2], normalize_int_list([1, 2])),
    ?assertEqual([], normalize_int_list([])).

role_ids_from_roles_test() ->
    Roles = [
        #{<<"id">> => <<"100">>},
        #{<<"id">> => <<"200">>},
        #{<<"name">> => <<"no_id">>}
    ],
    ?assertEqual([100, 200], role_ids_from_roles(Roles)).

check_target_member_owner_can_manage_anyone_test() ->
    State = test_state(),
    {reply, #{can_manage := CanManage}, _} =
        check_target_member(#{user_id => 1, target_user_id => 2}, State),
    ?assertEqual(true, CanManage).

check_target_member_cannot_manage_owner_test() ->
    State = test_state(),
    {reply, #{can_manage := CanManage}, _} =
        check_target_member(#{user_id => 2, target_user_id => 1}, State),
    ?assertEqual(false, CanManage).

check_target_member_higher_role_can_manage_lower_test() ->
    State = test_state(),
    {reply, #{can_manage := CanManage}, _} =
        check_target_member(#{user_id => 3, target_user_id => 2}, State),
    ?assertEqual(true, CanManage).

check_target_member_lower_role_cannot_manage_higher_test() ->
    State = test_state(),
    {reply, #{can_manage := CanManage}, _} =
        check_target_member(#{user_id => 2, target_user_id => 3}, State),
    ?assertEqual(false, CanManage).

can_manage_roles_owner_always_true_test() ->
    State = test_state(),
    {reply, #{can_manage := CanManage}, _} =
        can_manage_roles(#{user_id => 1, role_id => 201}, State),
    ?assertEqual(true, CanManage).

can_manage_roles_member_lower_role_test() ->
    State = test_state(),
    {reply, #{can_manage := CanManage}, _} =
        can_manage_roles(#{user_id => 2, role_id => 201}, State),
    ?assertEqual(false, CanManage).

can_manage_roles_unknown_role_test() ->
    State = test_state(),
    {reply, #{can_manage := CanManage}, _} =
        can_manage_roles(#{user_id => 2, role_id => 999}, State),
    ?assertEqual(false, CanManage).

get_viewable_channels_unknown_user_test() ->
    State = test_state(),
    {reply, #{channel_ids := ChannelIds}, _} =
        get_viewable_channels(#{user_id => 999}, State),
    ?assertEqual([], ChannelIds).

get_viewable_channels_owner_sees_all_test() ->
    State = test_state(),
    {reply, #{channel_ids := ChannelIds}, _} =
        get_viewable_channels(#{user_id => 1}, State),
    ?assert(length(ChannelIds) >= 2).

get_viewable_channels_with_deny_overwrite_test() ->
    ViewPerm = constants:view_channel_permission(),
    ManageRoles = constants:manage_roles_permission(),
    State = #{
        id => 100,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"1">>},
            <<"roles">> => [
                #{<<"id">> => <<"100">>, <<"permissions">> => integer_to_binary(ViewPerm bor ManageRoles), <<"position">> => 0}
            ],
            <<"channels">> => [
                #{
                    <<"id">> => <<"500">>,
                    <<"type">> => 0,
                    <<"permission_overwrites">> => [
                        #{<<"id">> => <<"100">>, <<"type">> => 0, <<"allow">> => <<"0">>, <<"deny">> => integer_to_binary(ViewPerm)}
                    ]
                },
                #{<<"id">> => <<"501">>, <<"type">> => 0, <<"permission_overwrites">> => []}
            ],
            <<"members">> => [
                #{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => [<<"100">>]},
                #{<<"user">> => #{<<"id">> => <<"2">>}, <<"roles">> => [<<"100">>]}
            ]
        }
    },
    {reply, #{channel_ids := OwnerChannels}, _} =
        get_viewable_channels(#{user_id => 1}, State),
    ?assert(lists:member(500, OwnerChannels)),
    {reply, #{channel_ids := MemberChannels}, _} =
        get_viewable_channels(#{user_id => 2}, State),
    ?assertNot(lists:member(500, MemberChannels)),
    ?assert(lists:member(501, MemberChannels)).

resolve_all_mentions_mention_everyone_test() ->
    State = test_state(),
    Request = #{
        channel_id => 500,
        author_id => 1,
        mention_everyone => true,
        mention_here => false,
        role_ids => [],
        user_ids => []
    },
    {reply, #{user_ids := UserIds}, _} = resolve_all_mentions(Request, State),
    ?assert(lists:member(2, UserIds)),
    ?assert(lists:member(3, UserIds)),
    ?assertNot(lists:member(1, UserIds)).

resolve_all_mentions_mention_here_only_connected_test() ->
    State0 = test_state(),
    State = State0#{
        sessions => #{
            <<"sess_2">> => #{user_id => 2}
        }
    },
    Request = #{
        channel_id => 500,
        author_id => 1,
        mention_everyone => false,
        mention_here => true,
        role_ids => [],
        user_ids => []
    },
    {reply, #{user_ids := UserIds}, _} = resolve_all_mentions(Request, State),
    ?assert(lists:member(2, UserIds)),
    ?assertNot(lists:member(3, UserIds)).

resolve_all_mentions_excludes_bots_test() ->
    ViewPerm = constants:view_channel_permission(),
    ManageRoles = constants:manage_roles_permission(),
    State = #{
        id => 100,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"1">>},
            <<"roles">> => [
                #{<<"id">> => <<"100">>, <<"permissions">> => integer_to_binary(ViewPerm bor ManageRoles), <<"position">> => 0}
            ],
            <<"channels">> => [
                #{<<"id">> => <<"500">>, <<"type">> => 0, <<"permission_overwrites">> => []}
            ],
            <<"members">> => [
                #{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => [<<"100">>]},
                #{<<"user">> => #{<<"id">> => <<"2">>, <<"bot">> => true}, <<"roles">> => [<<"100">>]},
                #{<<"user">> => #{<<"id">> => <<"3">>}, <<"roles">> => [<<"100">>]}
            ]
        },
        sessions => #{}
    },
    Request = #{
        channel_id => 500,
        author_id => 1,
        mention_everyone => true,
        mention_here => false,
        role_ids => [],
        user_ids => []
    },
    {reply, #{user_ids := UserIds}, _} = resolve_all_mentions(Request, State),
    ?assertNot(lists:member(2, UserIds)),
    ?assert(lists:member(3, UserIds)).

resolve_all_mentions_author_always_excluded_test() ->
    State = test_state(),
    Request = #{
        channel_id => 500,
        author_id => 2,
        mention_everyone => false,
        mention_here => false,
        role_ids => [],
        user_ids => [2]
    },
    {reply, #{user_ids := UserIds}, _} = resolve_all_mentions(Request, State),
    ?assertNot(lists:member(2, UserIds)).

get_all_users_to_mention_excludes_author_test() ->
    State = test_state(),
    {reply, #{user_ids := UserIds}, _} =
        get_all_users_to_mention(#{channel_id => 500, author_id => 1}, State),
    ?assertNot(lists:member(1, UserIds)),
    ?assert(lists:member(2, UserIds)),
    ?assert(lists:member(3, UserIds)).

get_members_with_role_undefined_role_id_test() ->
    State = test_state(),
    {reply, #{user_ids := UserIds}, _} =
        get_members_with_role(#{role_id => undefined}, State),
    ?assertEqual([], UserIds).

get_members_with_role_nonexistent_role_test() ->
    State = test_state(),
    {reply, #{user_ids := UserIds}, _} =
        get_members_with_role(#{role_id => 999}, State),
    ?assertEqual([], UserIds).

get_assignable_roles_non_member_test() ->
    State = test_state(),
    {reply, #{role_ids := RoleIds}, _} =
        get_assignable_roles(#{user_id => 999}, State),
    ?assertEqual([], RoleIds).

member_user_id_missing_user_test() ->
    ?assertEqual(undefined, member_user_id(#{})).

member_user_id_missing_id_test() ->
    ?assertEqual(undefined, member_user_id(#{<<"user">> => #{}})).

member_user_id_valid_test() ->
    ?assertEqual(42, member_user_id(#{<<"user">> => #{<<"id">> => <<"42">>}})).

member_roles_empty_test() ->
    ?assertEqual([], member_roles(#{})).

member_roles_binary_ids_test() ->
    Member = #{<<"roles">> => [<<"100">>, <<"200">>]},
    ?assertEqual([100, 200], member_roles(Member)).

is_member_bot_missing_user_test() ->
    ?assertEqual(false, is_member_bot(#{})).

is_member_bot_missing_bot_field_test() ->
    ?assertEqual(false, is_member_bot(#{<<"user">> => #{}})).

role_position_default_test() ->
    ?assertEqual(0, role_position(#{})).

role_position_explicit_test() ->
    ?assertEqual(5, role_position(#{<<"position">> => 5})).

role_ids_from_roles_empty_test() ->
    ?assertEqual([], role_ids_from_roles([])).

role_ids_from_roles_skips_undefined_ids_test() ->
    Roles = [#{}, #{<<"id">> => <<"100">>}],
    ?assertEqual([100], role_ids_from_roles(Roles)).

normalize_int_list_mixed_types_test() ->
    ?assertEqual([1, 2], normalize_int_list([<<"1">>, <<"invalid">>, 2])).

normalize_int_list_non_list_input_test() ->
    ?assertEqual([], normalize_int_list(not_a_list)).

member_has_any_role_set_empty_roles_test() ->
    Member = #{<<"roles">> => []},
    RoleSet = gb_sets:from_list([100]),
    ?assertEqual(false, member_has_any_role_set(Member, RoleSet)).

member_has_any_role_set_empty_set_test() ->
    Member = #{<<"roles">> => [<<"100">>]},
    RoleSet = gb_sets:empty(),
    ?assertEqual(false, member_has_any_role_set(Member, RoleSet)).

compare_roles_first_undefined_test() ->
    Role = #{<<"position">> => 5, <<"id">> => <<"10">>},
    ?assertEqual(Role, compare_roles(Role, undefined)).

compare_roles_higher_position_wins_test() ->
    RoleA = #{<<"position">> => 5, <<"id">> => <<"10">>},
    RoleB = #{<<"position">> => 10, <<"id">> => <<"20">>},
    ?assertEqual(RoleB, compare_roles(RoleB, RoleA)).

compare_roles_same_position_lower_id_wins_test() ->
    RoleA = #{<<"position">> => 5, <<"id">> => <<"20">>},
    RoleB = #{<<"position">> => 5, <<"id">> => <<"10">>},
    ?assertEqual(RoleB, compare_roles(RoleB, RoleA)).

compare_roles_same_position_higher_id_loses_test() ->
    RoleA = #{<<"position">> => 5, <<"id">> => <<"10">>},
    RoleB = #{<<"position">> => 5, <<"id">> => <<"20">>},
    ?assertEqual(RoleA, compare_roles(RoleB, RoleA)).

get_highest_role_empty_roles_test() ->
    ?assertEqual(undefined, get_highest_role([], #{})).

get_highest_role_no_matching_roles_test() ->
    Roles = #{999 => #{<<"id">> => <<"999">>, <<"position">> => 5}},
    ?assertEqual(undefined, get_highest_role([100], Roles)).

get_highest_role_picks_highest_position_test() ->
    RoleLow = #{<<"id">> => <<"100">>, <<"position">> => 5},
    RoleHigh = #{<<"id">> => <<"200">>, <<"position">> => 10},
    Roles = #{100 => RoleLow, 200 => RoleHigh},
    ?assertEqual(RoleHigh, get_highest_role([100, 200], Roles)).

build_connected_user_ids_false_returns_empty_test() ->
    Sessions = #{<<"s1">> => #{user_id => 1}},
    ?assertEqual(gb_sets:empty(), build_connected_user_ids(false, Sessions)).

build_connected_user_ids_true_collects_user_ids_test() ->
    Sessions = #{
        <<"s1">> => #{user_id => 1},
        <<"s2">> => #{user_id => 2},
        <<"s3">> => #{other => data}
    },
    Result = build_connected_user_ids(true, Sessions),
    ?assert(gb_sets:is_member(1, Result)),
    ?assert(gb_sets:is_member(2, Result)),
    ?assertEqual(2, gb_sets:size(Result)).

build_connected_user_ids_empty_sessions_test() ->
    Result = build_connected_user_ids(true, #{}),
    ?assertEqual(gb_sets:empty(), Result).

check_should_mention_everyone_true_test() ->
    Member = #{<<"roles">> => []},
    ?assertEqual(true, check_should_mention(
        1, Member, true, false, false, false,
        gb_sets:empty(), gb_sets:empty(), gb_sets:empty()
    )).

check_should_mention_here_connected_test() ->
    Member = #{<<"roles">> => []},
    Connected = gb_sets:from_list([1]),
    ?assertEqual(true, check_should_mention(
        1, Member, false, true, false, false,
        gb_sets:empty(), gb_sets:empty(), Connected
    )).

check_should_mention_here_not_connected_test() ->
    Member = #{<<"roles">> => []},
    Connected = gb_sets:from_list([2]),
    ?assertEqual(false, check_should_mention(
        1, Member, false, true, false, false,
        gb_sets:empty(), gb_sets:empty(), Connected
    )).

check_should_mention_role_match_test() ->
    Member = #{<<"roles">> => [<<"100">>]},
    RoleSet = gb_sets:from_list([100]),
    ?assertEqual(true, check_should_mention(
        1, Member, false, false, true, false,
        RoleSet, gb_sets:empty(), gb_sets:empty()
    )).

check_should_mention_direct_id_match_test() ->
    Member = #{<<"roles">> => []},
    DirectSet = gb_sets:from_list([1]),
    ?assertEqual(true, check_should_mention(
        1, Member, false, false, false, true,
        gb_sets:empty(), DirectSet, gb_sets:empty()
    )).

check_should_mention_nothing_matches_test() ->
    Member = #{<<"roles">> => []},
    ?assertEqual(false, check_should_mention(
        1, Member, false, false, false, false,
        gb_sets:empty(), gb_sets:empty(), gb_sets:empty()
    )).

member_can_view_channel_non_integer_channel_id_test() ->
    ?assertEqual(false, member_can_view_channel(1, undefined, #{}, #{})).

collect_mentions_excludes_author_test() ->
    ViewPerm = constants:view_channel_permission(),
    State = #{
        id => 100,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"1">>},
            <<"roles">> => [
                #{<<"id">> => <<"100">>, <<"permissions">> => integer_to_binary(ViewPerm), <<"position">> => 0}
            ],
            <<"channels">> => [
                #{<<"id">> => <<"500">>, <<"type">> => 0, <<"permission_overwrites">> => []}
            ],
            <<"members">> => [
                #{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => [<<"100">>]},
                #{<<"user">> => #{<<"id">> => <<"2">>}, <<"roles">> => [<<"100">>]}
            ]
        }
    },
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => [<<"100">>]},
        #{<<"user">> => #{<<"id">> => <<"2">>}, <<"roles">> => [<<"100">>]}
    ],
    UserIds = collect_mentions(Members, 1, 500, State, fun(_) -> true end),
    ?assertNot(lists:member(1, UserIds)),
    ?assert(lists:member(2, UserIds)).

collect_mentions_skips_members_without_user_id_test() ->
    ViewPerm = constants:view_channel_permission(),
    State = #{
        id => 100,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"1">>},
            <<"roles">> => [
                #{<<"id">> => <<"100">>, <<"permissions">> => integer_to_binary(ViewPerm), <<"position">> => 0}
            ],
            <<"channels">> => [
                #{<<"id">> => <<"500">>, <<"type">> => 0, <<"permission_overwrites">> => []}
            ],
            <<"members">> => []
        }
    },
    Members = [#{}, #{<<"user">> => #{}}],
    UserIds = collect_mentions(Members, 1, 500, State, fun(_) -> true end),
    ?assertEqual([], UserIds).

filter_assignable_role_below_position_test() ->
    Role = #{<<"id">> => <<"100">>, <<"position">> => 5},
    ?assertEqual({true, 100}, filter_assignable_role(Role, 10)).

filter_assignable_role_at_position_test() ->
    Role = #{<<"id">> => <<"100">>, <<"position">> => 10},
    ?assertEqual(false, filter_assignable_role(Role, 10)).

filter_assignable_role_above_position_test() ->
    Role = #{<<"id">> => <<"100">>, <<"position">> => 15},
    ?assertEqual(false, filter_assignable_role(Role, 10)).

filter_assignable_role_no_id_test() ->
    Role = #{<<"position">> => 5},
    ?assertEqual(false, filter_assignable_role(Role, 10)).

user_ids_for_any_role_empty_roles_test() ->
    State = test_state(),
    ?assertEqual([], user_ids_for_any_role([], State)).

user_ids_for_any_role_nonexistent_role_test() ->
    State = test_state(),
    ?assertEqual([], user_ids_for_any_role([999], State)).

user_ids_for_any_role_multiple_roles_test() ->
    State = test_state(),
    UserIds = lists:sort(user_ids_for_any_role([200, 201], State)),
    ?assertEqual([2, 3], UserIds).

owner_id_valid_test() ->
    State = test_state(),
    ?assertEqual(1, owner_id(State)).

owner_id_missing_guild_test() ->
    State = #{data => #{}},
    ?assertEqual(0, owner_id(State)).

guild_data_missing_data_test() ->
    State = #{},
    ?assertEqual(#{}, guild_data(State)).

guild_members_empty_test() ->
    State = #{data => #{<<"members">> => []}},
    ?assertEqual([], guild_members(State)).

guild_roles_empty_test() ->
    State = #{data => #{<<"roles">> => []}},
    ?assertEqual([], guild_roles(State)).

guild_channels_empty_test() ->
    State = #{data => #{<<"channels">> => []}},
    ?assertEqual([], guild_channels(State)).

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
