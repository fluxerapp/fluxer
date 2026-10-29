%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_members_roles_tests).
-typing([eqwalizer]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(GUILD_ID, 100).
-define(OWNER_ID, 1).
-define(BOT_ID, 2).
-define(LOW_ID, 3).
-define(HIGH_ID, 4).
-define(PLAIN_ID, 5).
-define(TIE_FIRST_ID, 6).
-define(TIE_SECOND_ID, 7).
-define(ROLE_R, 200).
-define(ROLE_LOW, 300).
-define(ROLE_BOT, 400).
-define(ROLE_HIGH, 500).
-define(ROLE_TIE_FIRST, 600).
-define(ROLE_TIE_SECOND, 700).

manager_can_give_a_role_below_them_test() ->
    ?assert(can_manage_roles(?BOT_ID, ?ROLE_R)).

manager_cannot_give_a_role_above_them_test() ->
    ?assertNot(can_manage_roles(?LOW_ID, ?ROLE_R)),
    ?assertNot(can_manage_roles(?BOT_ID, ?ROLE_HIGH)).

manager_can_give_a_role_to_a_member_they_cannot_moderate_test() ->
    ?assert(can_manage_roles(?BOT_ID, ?ROLE_R)),
    ?assertNot(check_target_member(?BOT_ID, ?HIGH_ID)).

owner_stays_out_of_reach_for_member_actions_test() ->
    ?assertNot(check_target_member(?BOT_ID, ?OWNER_ID)),
    ?assert(check_target_member(?BOT_ID, ?PLAIN_ID)).

equal_position_roles_rank_the_lower_id_higher_test() ->
    ?assert(can_manage_roles(?TIE_FIRST_ID, ?ROLE_TIE_SECOND)),
    ?assertNot(can_manage_roles(?TIE_SECOND_ID, ?ROLE_TIE_FIRST)),
    ?assert(can_manage_role(?TIE_FIRST_ID, ?ROLE_TIE_SECOND)),
    ?assertNot(can_manage_role(?TIE_SECOND_ID, ?ROLE_TIE_FIRST)),
    ?assert(check_target_member(?TIE_FIRST_ID, ?TIE_SECOND_ID)),
    ?assertNot(check_target_member(?TIE_SECOND_ID, ?TIE_FIRST_ID)).

can_manage_roles(UserId, RoleId) ->
    {reply, #{can_manage := CanManage}, _} = guild_members_roles:can_manage_roles(
        #{user_id => UserId, role_id => RoleId}, test_state()
    ),
    CanManage.

can_manage_role(UserId, RoleId) ->
    {reply, #{can_manage := CanManage}, _} = guild_members_roles:can_manage_role(
        #{user_id => UserId, role_id => RoleId}, test_state()
    ),
    CanManage.

check_target_member(UserId, TargetUserId) ->
    {reply, #{can_manage := CanManage}, _} = guild_members_roles:check_target_member(
        #{user_id => UserId, target_user_id => TargetUserId}, test_state()
    ),
    CanManage.

test_state() ->
    ViewPerm = constants:view_channel_permission(),
    ManageRoles = constants:manage_roles_permission(),
    #{
        id => ?GUILD_ID,
        data => #{
            <<"guild">> => #{<<"owner_id">> => integer_to_binary(?OWNER_ID)},
            <<"roles">> => [
                role(?GUILD_ID, 0, ViewPerm),
                role(?ROLE_R, 10, ViewPerm),
                role(?ROLE_LOW, 5, ViewPerm bor ManageRoles),
                role(?ROLE_BOT, 30, ViewPerm bor ManageRoles),
                role(?ROLE_HIGH, 40, ViewPerm),
                role(?ROLE_TIE_FIRST, 50, ViewPerm bor ManageRoles),
                role(?ROLE_TIE_SECOND, 50, ViewPerm bor ManageRoles)
            ],
            <<"channels">> => [],
            <<"members">> => [
                member(?OWNER_ID, []),
                member(?BOT_ID, [?ROLE_BOT]),
                member(?LOW_ID, [?ROLE_LOW]),
                member(?HIGH_ID, [?ROLE_HIGH]),
                member(?PLAIN_ID, []),
                member(?TIE_FIRST_ID, [?ROLE_TIE_FIRST]),
                member(?TIE_SECOND_ID, [?ROLE_TIE_SECOND])
            ]
        }
    }.

role(Id, Position, Permissions) ->
    #{
        <<"id">> => integer_to_binary(Id),
        <<"permissions">> => integer_to_binary(Permissions),
        <<"position">> => Position
    }.

member(UserId, RoleIds) ->
    #{
        <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
        <<"roles">> => [integer_to_binary(RoleId) || RoleId <- RoleIds]
    }.

-endif.
