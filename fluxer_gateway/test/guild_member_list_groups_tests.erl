%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_groups_tests).

-include_lib("eunit/include/eunit.hrl").

-define(GUILD_ID, 100).

role(Id, Position, HoistPosition) ->
    #{
        <<"id">> => integer_to_binary(Id),
        <<"hoist">> => true,
        <<"position">> => Position,
        <<"hoist_position">> => HoistPosition
    }.

sorted_ids(Roles) ->
    [
        binary_to_integer(maps:get(<<"id">>, Role))
     || Role <- guild_member_list_groups:get_hoisted_roles_sorted(Roles, ?GUILD_ID)
    ].

tied_positions_sort_by_lower_id_first_test() ->
    Roles = [role(201, 1, null), role(202, 1, null), role(203, 1, null)],
    ?assertEqual([201, 202, 203], sorted_ids(Roles)),
    ?assertEqual([201, 202, 203], sorted_ids(lists:reverse(Roles))).

higher_position_first_test() ->
    Roles = [role(201, 1, null), role(202, 3, null), role(203, 2, null)],
    ?assertEqual([202, 203, 201], sorted_ids(Roles)).

hoist_position_overrides_position_test() ->
    Roles = [role(201, 5, 1), role(202, 1, 2)],
    ?assertEqual([202, 201], sorted_ids(Roles)).
