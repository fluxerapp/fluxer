%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_engine_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

-define(ONLINE_IDX, 16#F0000000).
-define(OFFLINE_IDX, 16#F0000001).

new_returns_reference_test() ->
    Ref = guild_member_list_engine:new(),
    ?assert(is_reference(Ref)),
    guild_member_list_engine:destroy(Ref).

new_empty_counts_test() ->
    Ref = guild_member_list_engine:new(),
    ?assertEqual({0, 0}, guild_member_list_engine:get_counts(Ref)),
    guild_member_list_engine:destroy(Ref).

new_empty_groups_test() ->
    Ref = guild_member_list_engine:new(),
    assert_groups(default_groups(0, 0), Ref),
    guild_member_list_engine:destroy(Ref).

new_empty_items_test() ->
    Ref = guild_member_list_engine:new(),
    ?assertEqual(
        [],
        guild_member_list_engine:get_items(Ref, 0, 100)
    ),
    guild_member_list_engine:destroy(Ref).

destroy_is_idempotent_test() ->
    Ref = guild_member_list_engine:new(),
    ?assertEqual(ok, guild_member_list_engine:destroy(Ref)),
    ?assertEqual(ok, guild_member_list_engine:destroy(Ref)).

add_offline_member_test() ->
    Ref = guild_member_list_engine:new(),
    ok = guild_member_list_engine:add_member(Ref, 1, <<"alice">>, [], false),
    ?assertEqual({1, 0}, guild_member_list_engine:get_counts(Ref)),
    assert_groups(default_groups(0, 1), Ref),
    guild_member_list_engine:destroy(Ref).

add_online_member_test() ->
    Ref = guild_member_list_engine:new(),
    ok = guild_member_list_engine:add_member(Ref, 1, <<"alice">>, [], true),
    ?assertEqual({1, 1}, guild_member_list_engine:get_counts(Ref)),
    assert_groups(default_groups(1, 0), Ref),
    guild_member_list_engine:destroy(Ref).

add_rejects_invalid_user_id_test() ->
    Ref = guild_member_list_engine:new(),
    ok = guild_member_list_engine:add_member(Ref, 0, <<"zero">>, [], true),
    ok = guild_member_list_engine:add_member(Ref, -1, <<"neg">>, [], true),
    ?assertEqual({0, 0}, guild_member_list_engine:get_counts(Ref)),
    guild_member_list_engine:destroy(Ref).

add_replaces_existing_member_test() ->
    Ref = guild_member_list_engine:new(),
    ok = guild_member_list_engine:add_member(Ref, 1, <<"alice">>, [], false),
    ok = guild_member_list_engine:add_member(Ref, 1, <<"alice_new">>, [], true),
    ?assertEqual({1, 1}, guild_member_list_engine:get_counts(Ref)),
    guild_member_list_engine:destroy(Ref).

remove_member_test() ->
    Ref = guild_member_list_engine:new(),
    ok = guild_member_list_engine:add_member(Ref, 1, <<"alice">>, [], true),
    ok = guild_member_list_engine:remove_member(Ref, 1),
    ?assertEqual({0, 0}, guild_member_list_engine:get_counts(Ref)),
    guild_member_list_engine:destroy(Ref).

remove_nonexistent_is_noop_test() ->
    Ref = guild_member_list_engine:new(),
    ok = guild_member_list_engine:remove_member(Ref, 999),
    ?assertEqual({0, 0}, guild_member_list_engine:get_counts(Ref)),
    guild_member_list_engine:destroy(Ref).

set_online_moves_to_online_section_test() ->
    Ref = guild_member_list_engine:new(),
    ok = guild_member_list_engine:add_member(Ref, 1, <<"alice">>, [], false),
    ?assertEqual({1, 0}, guild_member_list_engine:get_counts(Ref)),
    ok = guild_member_list_engine:set_online(Ref, 1, true),
    ?assertEqual({1, 1}, guild_member_list_engine:get_counts(Ref)),
    assert_groups(default_groups(1, 0), Ref),
    guild_member_list_engine:destroy(Ref).

set_online_same_state_is_noop_test() ->
    Ref = guild_member_list_engine:new(),
    ok = guild_member_list_engine:add_member(Ref, 1, <<"alice">>, [], true),
    ok = guild_member_list_engine:set_online(Ref, 1, true),
    ?assertEqual({1, 1}, guild_member_list_engine:get_counts(Ref)),
    guild_member_list_engine:destroy(Ref).

set_online_nonexistent_is_noop_test() ->
    Ref = guild_member_list_engine:new(),
    ok = guild_member_list_engine:set_online(Ref, 999, true),
    ?assertEqual({0, 0}, guild_member_list_engine:get_counts(Ref)),
    guild_member_list_engine:destroy(Ref).

bulk_load_test() ->
    Ref = guild_member_list_engine:new(),
    Members = [
        {1, <<"alice">>, [], true},
        {2, <<"bob">>, [], false},
        {3, <<"charlie">>, [], true}
    ],
    ok = guild_member_list_engine:bulk_load(Ref, Members, []),
    ?assertEqual({3, 2}, guild_member_list_engine:get_counts(Ref)),
    assert_groups(default_groups(2, 1), Ref),
    guild_member_list_engine:destroy(Ref).

bulk_load_with_hoisted_roles_test() ->
    Ref = guild_member_list_engine:new(),
    RoleA = 100,
    Members = [
        {1, <<"alice">>, [RoleA], true},
        {2, <<"bob">>, [], true},
        {3, <<"charlie">>, [RoleA], false}
    ],
    ok = guild_member_list_engine:bulk_load(Ref, Members, [RoleA]),
    ?assertEqual({3, 2}, guild_member_list_engine:get_counts(Ref)),
    Groups = guild_member_list_engine:get_groups(Ref),
    ?assertEqual([{<<"100">>, 1}, {<<"online">>, 1}, {<<"offline">>, 1}], Groups),
    guild_member_list_engine:destroy(Ref).

bulk_load_filters_invalid_ids_test() ->
    Ref = guild_member_list_engine:new(),
    Members = [
        {0, <<"zero">>, [], true},
        {-1, <<"neg">>, [], true},
        {1, <<"valid">>, [], true}
    ],
    ok = guild_member_list_engine:bulk_load(Ref, Members, []),
    ?assertEqual({1, 1}, guild_member_list_engine:get_counts(Ref)),
    guild_member_list_engine:destroy(Ref).

get_items_full_range_test() ->
    Ref = guild_member_list_engine:new(),
    Members = alice_online_bob_offline(),
    ok = guild_member_list_engine:bulk_load(Ref, Members, []),
    Items = guild_member_list_engine:get_items(Ref, 0, 100),
    ?assertEqual(
        [
            {group, <<"online">>, 1},
            {member, 1},
            {group, <<"offline">>, 1},
            {member, 2}
        ],
        Items
    ),
    guild_member_list_engine:destroy(Ref).

get_items_header_only_test() ->
    Ref = guild_member_list_engine:new(),
    ok = guild_member_list_engine:add_member(Ref, 1, <<"alice">>, [], true),
    Items = guild_member_list_engine:get_items(Ref, 0, 0),
    ?assertEqual([{group, <<"online">>, 1}], Items),
    guild_member_list_engine:destroy(Ref).

get_items_partial_range_test() ->
    Ref = guild_member_list_engine:new(),
    Members = [
        {1, <<"alice">>, [], true},
        {2, <<"bob">>, [], true},
        {3, <<"charlie">>, [], true},
        {4, <<"dave">>, [], false},
        {5, <<"eve">>, [], false}
    ],
    ok = guild_member_list_engine:bulk_load(Ref, Members, []),
    Items = guild_member_list_engine:get_items(Ref, 2, 5),
    ?assertEqual(
        [
            {member, 2},
            {member, 3},
            {group, <<"offline">>, 2},
            {member, 4}
        ],
        Items
    ),
    guild_member_list_engine:destroy(Ref).

get_items_start_past_end_test() ->
    Ref = guild_member_list_engine:new(),
    ok = guild_member_list_engine:add_member(Ref, 1, <<"alice">>, [], true),
    ?assertEqual([], guild_member_list_engine:get_items(Ref, 5, 3)),
    guild_member_list_engine:destroy(Ref).

get_items_beyond_total_test() ->
    Ref = guild_member_list_engine:new(),
    ok = guild_member_list_engine:add_member(Ref, 1, <<"alice">>, [], true),
    Items = guild_member_list_engine:get_items(Ref, 0, 100),
    ?assertEqual(
        [
            {group, <<"online">>, 1},
            {member, 1}
        ],
        Items
    ),
    guild_member_list_engine:destroy(Ref).

get_items_with_hoisted_roles_test() ->
    Ref = guild_member_list_engine:new(),
    RoleA = 100,
    RoleB = 200,
    Members = [
        {1, <<"alice">>, [RoleA], true},
        {2, <<"bob">>, [RoleB], true},
        {3, <<"charlie">>, [], true},
        {4, <<"dave">>, [], false}
    ],
    ok = guild_member_list_engine:bulk_load(Ref, Members, [RoleA, RoleB]),
    Items = guild_member_list_engine:get_items(Ref, 0, 100),
    ?assertEqual(
        [
            {group, <<"100">>, 1},
            {member, 1},
            {group, <<"200">>, 1},
            {member, 2},
            {group, <<"online">>, 1},
            {member, 3},
            {group, <<"offline">>, 1},
            {member, 4}
        ],
        Items
    ),
    guild_member_list_engine:destroy(Ref).

get_items_sort_order_test() ->
    Ref = guild_member_list_engine:new(),
    Members = [
        {3, <<"charlie">>, [], true},
        {1, <<"alice">>, [], true},
        {2, <<"bob">>, [], true}
    ],
    ok = guild_member_list_engine:bulk_load(Ref, Members, []),
    Items = guild_member_list_engine:get_items(Ref, 1, 3),
    ?assertEqual([{member, 1}, {member, 2}, {member, 3}], Items),
    guild_member_list_engine:destroy(Ref).

get_items_empty_sections_test() ->
    Ref = guild_member_list_engine:new(),
    ok = guild_member_list_engine:bulk_load(Ref, [], [100]),
    Items = guild_member_list_engine:get_items(Ref, 0, 100),
    ?assertEqual(
        [],
        Items
    ),
    guild_member_list_engine:destroy(Ref).

set_hoisted_roles_assigns_sections_test() ->
    Ref = guild_member_list_engine:new(),
    Members = [
        {1, <<"alice">>, [100], true},
        {2, <<"bob">>, [200], true},
        {3, <<"charlie">>, [], true}
    ],
    ok = guild_member_list_engine:bulk_load(Ref, Members, []),
    assert_groups(default_groups(3, 0), Ref),
    _ = guild_member_list_engine:set_hoisted_roles(Ref, [100, 200]),
    assert_groups(role_groups([{100, 1}, {200, 1}], 1, 0), Ref),
    ?assertEqual({3, 3}, guild_member_list_engine:get_counts(Ref)),
    guild_member_list_engine:destroy(Ref).

set_hoisted_roles_offline_stays_offline_test() ->
    Ref = guild_member_list_engine:new(),
    Members = [
        {1, <<"alice">>, [100], false},
        {2, <<"bob">>, [100], true}
    ],
    ok = guild_member_list_engine:bulk_load(Ref, Members, [100]),
    Groups = guild_member_list_engine:get_groups(Ref),
    ?assertEqual([{<<"100">>, 1}, {<<"online">>, 0}, {<<"offline">>, 1}], Groups),
    guild_member_list_engine:destroy(Ref).

set_hoisted_roles_same_is_noop_test() ->
    Ref = guild_member_list_engine:new(),
    ok = guild_member_list_engine:bulk_load(Ref, [{1, <<"a">>, [100], true}], [100]),
    _ = guild_member_list_engine:set_hoisted_roles(Ref, [100]),
    ?assertEqual({1, 1}, guild_member_list_engine:get_counts(Ref)),
    guild_member_list_engine:destroy(Ref).

set_hoisted_roles_reports_changed_unchanged_test() ->
    Ref = guild_member_list_engine:new(),
    ok = guild_member_list_engine:bulk_load(Ref, [], []),
    ?assertEqual(changed, guild_member_list_engine:set_hoisted_roles(Ref, [100, 200])),
    ?assertEqual(unchanged, guild_member_list_engine:set_hoisted_roles(Ref, [100, 200])),
    ?assertEqual(changed, guild_member_list_engine:set_hoisted_roles(Ref, [200, 100])),
    ?assertEqual(changed, guild_member_list_engine:set_hoisted_roles(Ref, [200])),
    ?assertEqual(unchanged, guild_member_list_engine:set_hoisted_roles(Ref, [200])),
    guild_member_list_engine:destroy(Ref).

set_hoisted_roles_remove_roles_test() ->
    Ref = guild_member_list_engine:new(),
    Members = [{1, <<"a">>, [100], true}, {2, <<"b">>, [], true}],
    ok = guild_member_list_engine:bulk_load(Ref, Members, [100]),
    assert_groups(role_groups([{100, 1}], 1, 0), Ref),
    _ = guild_member_list_engine:set_hoisted_roles(Ref, []),
    assert_groups(default_groups(2, 0), Ref),
    guild_member_list_engine:destroy(Ref).

set_hoisted_roles_picks_highest_priority_test() ->
    Ref = guild_member_list_engine:new(),
    ok = guild_member_list_engine:bulk_load(Ref, [{1, <<"a">>, [200, 100], true}], [100, 200]),
    Groups = guild_member_list_engine:get_groups(Ref),
    ?assertEqual(
        [{<<"100">>, 1}, {<<"200">>, 0}, {<<"online">>, 0}, {<<"offline">>, 0}], Groups
    ),
    guild_member_list_engine:destroy(Ref).

get_sorted_user_ids_test() ->
    Ref = guild_member_list_engine:new(),
    Members = [
        {3, <<"charlie">>, [], true},
        {1, <<"alice">>, [], false},
        {2, <<"bob">>, [], true}
    ],
    ok = guild_member_list_engine:bulk_load(Ref, Members, []),
    Ids = guild_member_list_engine:get_sorted_user_ids(Ref),
    ?assertEqual([2, 3, 1], Ids),
    guild_member_list_engine:destroy(Ref).

get_all_item_keys_test() ->
    Ref = guild_member_list_engine:new(),
    Members = alice_online_bob_offline(),
    ok = guild_member_list_engine:bulk_load(Ref, Members, []),
    Keys = guild_member_list_engine:get_all_item_keys(Ref),
    ?assertEqual(
        [
            {group, <<"online">>, 0},
            {member, 1},
            {group, <<"offline">>, 0},
            {member, 2}
        ],
        Keys
    ),
    guild_member_list_engine:destroy(Ref).

ordering_10k_test() ->
    Ref = guild_member_list_engine:new(),
    N = 10000,
    Members = [
        {I, iolist_to_binary(io_lib:format("user_~6..0B", [I])), [], I rem 3 =/= 0}
     || I <- lists:seq(1, N)
    ],
    ok = guild_member_list_engine:bulk_load(Ref, Members, []),
    {Total, Online} = guild_member_list_engine:get_counts(Ref),
    ?assertEqual(N, Total),
    ExpectedOnline = length([1 || I <- lists:seq(1, N), I rem 3 =/= 0]),
    ?assertEqual(ExpectedOnline, Online),
    Items = guild_member_list_engine:get_items(Ref, 0, 20),
    [{group, <<"online">>, OnlineCount} | MemberItems] = Items,
    ?assertEqual(ExpectedOnline, OnlineCount),
    MemberIds = [Id || {member, Id} <- MemberItems],
    ?assertEqual(MemberIds, lists:sort(MemberIds)),
    guild_member_list_engine:destroy(Ref).

ordering_preserves_sort_key_order_test() ->
    Ref = guild_member_list_engine:new(),
    Members = [
        {5, <<"eve">>, [], true},
        {1, <<"alice">>, [], true},
        {4, <<"dave">>, [], true},
        {2, <<"bob">>, [], true},
        {3, <<"charlie">>, [], true}
    ],
    ok = guild_member_list_engine:bulk_load(Ref, Members, []),
    Items = guild_member_list_engine:get_items(Ref, 1, 5),
    Ids = [Id || {member, Id} <- Items],
    ?assertEqual([1, 2, 3, 4, 5], Ids),
    guild_member_list_engine:destroy(Ref).

rapid_add_remove_test() ->
    Ref = guild_member_list_engine:new(),
    lists:foreach(
        fun(I) ->
            guild_member_list_engine:add_member(Ref, I, <<"u">>, [], true)
        end,
        lists:seq(1, 100)
    ),
    ?assertEqual({100, 100}, guild_member_list_engine:get_counts(Ref)),
    lists:foreach(
        fun(I) ->
            guild_member_list_engine:remove_member(Ref, I)
        end,
        lists:seq(1, 50)
    ),
    ?assertEqual({50, 50}, guild_member_list_engine:get_counts(Ref)),
    guild_member_list_engine:destroy(Ref).

rapid_online_toggle_test() ->
    Ref = guild_member_list_engine:new(),
    ok = guild_member_list_engine:add_member(Ref, 1, <<"a">>, [], false),
    lists:foreach(
        fun(I) ->
            guild_member_list_engine:set_online(Ref, 1, I rem 2 =:= 0)
        end,
        lists:seq(1, 1000)
    ),
    {1, _} = guild_member_list_engine:get_counts(Ref),
    guild_member_list_engine:destroy(Ref).

set_hoisted_roles_preserves_all_indexed_members_test() ->
    Ref = guild_member_list_engine:new(),
    try
        Members = [
            {UserId, engine_sort_key(UserId), engine_roles_for_test_member(UserId), true}
         || UserId <- lists:seq(1, 1000)
        ],
        ok = guild_member_list_engine:bulk_load(Ref, Members, [10]),
        ?assertEqual(
            lists:seq(1, 1000), lists:sort(guild_member_list_engine:get_sorted_user_ids(Ref))
        ),
        ?assertEqual(changed, guild_member_list_engine:set_hoisted_roles(Ref, [20, 10])),
        ?assertEqual(
            lists:seq(1, 1000), lists:sort(guild_member_list_engine:get_sorted_user_ids(Ref))
        ),
        ?assertEqual({1000, 1000}, guild_member_list_engine:get_counts(Ref))
    after
        guild_member_list_engine:destroy(Ref)
    end.

update_member_preserves_bulk_loaded_members_test() ->
    Ref = guild_member_list_engine:new(),
    try
        InitialMembers = [
            {100000 + I, engine_sort_key(I), engine_roles_for_test_member(I), false}
         || I <- lists:seq(1, 480)
        ],
        ok = guild_member_list_engine:bulk_load(Ref, InitialMembers, [10]),
        lists:foreach(
            fun(I) ->
                UserId = 200000 + I,
                ok = guild_member_list_engine:update_member(
                    Ref, UserId, engine_sort_key(I), engine_roles_for_test_member(I), false
                )
            end,
            lists:seq(1, 60)
        ),
        Expected = lists:seq(100001, 100480) ++ lists:seq(200001, 200060),
        ?assertEqual(Expected, lists:sort(guild_member_list_engine:get_sorted_user_ids(Ref))),
        ?assertEqual({540, 0}, guild_member_list_engine:get_counts(Ref))
    after
        guild_member_list_engine:destroy(Ref)
    end.

default_groups(OnlineCount, OfflineCount) ->
    [{<<"online">>, OnlineCount}, {<<"offline">>, OfflineCount}].

role_groups(RoleCounts, OnlineCount, OfflineCount) ->
    RoleGroups = [{integer_to_binary(RoleId), Count} || {RoleId, Count} <- RoleCounts],
    RoleGroups ++ default_groups(OnlineCount, OfflineCount).

alice_online_bob_offline() ->
    [
        {1, <<"alice">>, [], true},
        {2, <<"bob">>, [], false}
    ].

assert_groups(Expected, Ref) ->
    ?assertEqual(Expected, guild_member_list_engine:get_groups(Ref)).

info_reports_memory_test() ->
    Ref = guild_member_list_engine:new(),
    ok = guild_member_list_engine:bulk_load(
        Ref,
        [{I, <<"user">>, [], false} || I <- lists:seq(1, 100)],
        []
    ),
    Info = guild_member_list_engine:info(Ref),
    ?assertEqual(100, maps:get(total, Info)),
    ?assert(maps:get(total_bytes, Info) > 0),
    guild_member_list_engine:destroy(Ref).

index_of_matches_display_position_test() ->
    Ref = guild_member_list_engine:new(),
    Hoisted = [100, 200],
    Members = [
        {1, <<"alice">>, [100], true},
        {2, <<"bob">>, [], true},
        {3, <<"carol">>, [200], true},
        {4, <<"dave">>, [], false},
        {5, <<"eve">>, [100], false},
        {6, <<"frank">>, [], true},
        {7, <<"grace">>, [200], true}
    ],
    ok = guild_member_list_engine:bulk_load(Ref, Members, Hoisted),
    assert_index_of_matches(Ref, Members),
    ?assertEqual(not_found, guild_member_list_engine:index_of(Ref, 9999)),
    guild_member_list_engine:destroy(Ref).

index_of_random_test_() ->
    {timeout, 60, fun() ->
        rand:seed(exsss, {51, 52, 53}),
        lists:foreach(fun(_) -> run_index_of_random() end, lists:seq(1, 5))
    end}.

run_index_of_random() ->
    Ref = guild_member_list_engine:new(),
    Hoisted = [100, 200, 300],
    N = 200 + rand:uniform(300),
    Members = [
        {Id, idx_sort_key(Id), idx_rand_roles(), rand:uniform(3) =/= 1}
     || Id <- lists:seq(1, N)
    ],
    ok = guild_member_list_engine:bulk_load(Ref, Members, Hoisted),
    assert_index_of_matches(Ref, Members),
    guild_member_list_engine:destroy(Ref).

assert_index_of_matches(Ref, Members) ->
    Items = guild_member_list_engine:get_items(Ref, 0, 1000000),
    Positions = item_positions(Items),
    lists:foreach(
        fun({UserId, _, _, _}) ->
            Exp = maps:get(UserId, Positions, not_in_list),
            ?assertEqual(
                {UserId, Exp}, {UserId, guild_member_list_engine:index_of(Ref, UserId)}
            )
        end,
        Members
    ).

item_positions(Items) ->
    {_, Map} = lists:foldl(
        fun
            ({member, UserId}, {I, Acc}) -> {I + 1, Acc#{UserId => I}};
            ({group, _, _}, {I, Acc}) -> {I + 1, Acc}
        end,
        {0, #{}},
        Items
    ),
    Map.

idx_sort_key(Id) ->
    integer_to_binary(rand:uniform(1000000) * 100 + (Id rem 100)).

idx_rand_roles() ->
    case rand:uniform(4) of
        1 -> [100];
        2 -> [200];
        3 -> [300];
        4 -> []
    end.

engine_sort_key(UserId) ->
    iolist_to_binary(io_lib:format("member_~4..0B", [UserId])).

engine_roles_for_test_member(UserId) when UserId rem 3 =:= 0 ->
    [10];
engine_roles_for_test_member(UserId) when UserId rem 5 =:= 0 ->
    [20];
engine_roles_for_test_member(_UserId) ->
    [].

version_bumps_on_every_mutation_test() ->
    Ref = guild_member_list_engine:new(),
    try
        V0 = guild_member_list_engine:version(Ref),
        ok = guild_member_list_engine:bulk_load(Ref, [{1, <<"a">>, [], true}], []),
        V1 = guild_member_list_engine:version(Ref),
        ok = guild_member_list_engine:add_member(Ref, 2, <<"b">>, [], false),
        V2 = guild_member_list_engine:version(Ref),
        ok = guild_member_list_engine:update_member(Ref, 2, <<"c">>, [], false),
        V3 = guild_member_list_engine:version(Ref),
        ok = guild_member_list_engine:set_online(Ref, 2, true),
        V4 = guild_member_list_engine:version(Ref),
        changed = guild_member_list_engine:set_hoisted_roles(Ref, [7]),
        V5 = guild_member_list_engine:version(Ref),
        ok = guild_member_list_engine:remove_member(Ref, 1),
        V6 = guild_member_list_engine:version(Ref),
        Versions = [V0, V1, V2, V3, V4, V5, V6],
        ?assertEqual(Versions, lists:usort(Versions)),
        ?assertEqual(7, length(lists:usort(Versions)))
    after
        guild_member_list_engine:destroy(Ref)
    end.

version_is_stable_for_noop_mutations_test() ->
    Ref = guild_member_list_engine:new(),
    try
        ok = guild_member_list_engine:add_member(Ref, 1, <<"a">>, [], true),
        V = guild_member_list_engine:version(Ref),
        ok = guild_member_list_engine:set_online(Ref, 1, true),
        ok = guild_member_list_engine:set_online(Ref, 99, false),
        ok = guild_member_list_engine:remove_member(Ref, 99),
        unchanged = guild_member_list_engine:set_hoisted_roles(Ref, []),
        _ = guild_member_list_engine:get_items(Ref, 0, 10),
        ?assertEqual(V, guild_member_list_engine:version(Ref))
    after
        guild_member_list_engine:destroy(Ref)
    end.

version_survives_tables_without_a_version_row_test() ->
    Ref = guild_member_list_engine:new(),
    try
        true = ets:delete(Ref, version),
        ?assertEqual(undefined, guild_member_list_engine:version(Ref)),
        ok = guild_member_list_engine:add_member(Ref, 1, <<"a">>, [], true),
        ?assertEqual(1, guild_member_list_engine:version(Ref))
    after
        guild_member_list_engine:destroy(Ref)
    end,
    ?assertEqual(undefined, guild_member_list_engine:version(Ref)).
