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
-define(ROLE_R, 200).
-define(ROLE_LOW, 300).
-define(ROLE_BOT, 400).
-define(ROLE_HIGH, 500).

manager_above_role_can_update_owner_roles_test() ->
    ?assert(can_manage_roles(?BOT_ID, ?OWNER_ID, ?ROLE_R)).

manager_below_role_cannot_update_owner_roles_test() ->
    ?assertNot(can_manage_roles(?LOW_ID, ?OWNER_ID, ?ROLE_R)).

manager_cannot_update_roles_of_member_above_them_test() ->
    ?assertNot(can_manage_roles(?BOT_ID, ?HIGH_ID, ?ROLE_R)),
    ?assert(can_manage_roles(?BOT_ID, ?PLAIN_ID, ?ROLE_R)).

owner_stays_out_of_reach_for_member_actions_test() ->
    ?assertNot(check_target_member(?BOT_ID, ?OWNER_ID)),
    ?assert(check_target_member(?BOT_ID, ?PLAIN_ID)).

can_manage_roles(UserId, TargetUserId, RoleId) ->
    {reply, #{can_manage := CanManage}, _} = guild_members_roles:can_manage_roles(
        #{user_id => UserId, target_user_id => TargetUserId, role_id => RoleId}, test_state()
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
                role(?ROLE_HIGH, 40, ViewPerm)
            ],
            <<"channels">> => [],
            <<"members">> => [
                member(?OWNER_ID, []),
                member(?BOT_ID, [?ROLE_BOT]),
                member(?LOW_ID, [?ROLE_LOW]),
                member(?HIGH_ID, [?ROLE_HIGH]),
                member(?PLAIN_ID, [])
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
