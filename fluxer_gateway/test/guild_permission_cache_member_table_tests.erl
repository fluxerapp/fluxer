%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_permission_cache_member_table_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

-define(MEMO, guild_permission_cache_stripped_members).
-define(CHANNEL_ID, 500).

snapshot_points_at_the_member_table_test() ->
    with_guild(908, [set, public], fun(GuildId, Tab, _Data) ->
        {ok, Snapshot} = guild_permission_cache:get_snapshot(GuildId),
        SnapshotData = maps:get(data, Snapshot),
        ?assertEqual(Tab, maps:get(members_ets, SnapshotData)),
        ?assertEqual(#{}, maps:get(<<"members">>, SnapshotData)),
        ?assertEqual(undefined, erlang:get(?MEMO))
    end).

member_reads_match_the_stripped_members_snapshot_test() ->
    Data = member_data(),
    GuildId = 909,
    memo_reset(),
    Tab = member_table(Data, [set, public]),
    try
        ok = guild_permission_cache:put_normalized_data(GuildId, Data),
        Expected = member_reads(GuildId),
        ok = guild_permission_cache:put_normalized_data(GuildId, Data#{members_ets => Tab}),
        ?assertEqual(Expected, member_reads(GuildId))
    after
        cleanup(GuildId, Tab)
    end.

deleted_member_table_reports_not_found_test() ->
    with_guild(910, [set, public], fun(GuildId, Tab, _Data) ->
        true = ets:delete(Tab),
        ?assertEqual({error, not_found}, guild_permission_cache:get_snapshot(GuildId)),
        ?assertEqual(
            {error, not_found}, guild_permission_cache:get_permissions(GuildId, 1, ?CHANNEL_ID)
        ),
        ?assertEqual({error, not_found}, guild_permission_cache:has_member(GuildId, 1)),
        ?assertEqual({error, not_found}, guild_permission_cache:get_member(GuildId, 1))
    end).

private_member_table_keeps_the_stripped_members_test() ->
    with_guild(911, [set, private], fun(GuildId, _Tab, Data) ->
        {ok, Snapshot} = guild_permission_cache:get_snapshot(GuildId),
        SnapshotData = maps:get(data, Snapshot),
        ?assertNot(maps:is_key(members_ets, SnapshotData)),
        ?assertEqual(
            map_size(maps:get(<<"members">>, Data)),
            map_size(maps:get(<<"members">>, SnapshotData))
        )
    end).

disabled_flag_keeps_the_stripped_members_test() ->
    application:set_env(fluxer_gateway, permission_cache_members_ets, false),
    try
        with_guild(912, [set, public], fun(GuildId, _Tab, Data) ->
            {ok, Snapshot} = guild_permission_cache:get_snapshot(GuildId),
            SnapshotData = maps:get(data, Snapshot),
            ?assertNot(maps:is_key(members_ets, SnapshotData)),
            ?assertEqual(
                map_size(maps:get(<<"members">>, Data)),
                map_size(maps:get(<<"members">>, SnapshotData))
            )
        end)
    after
        application:unset_env(fluxer_gateway, permission_cache_members_ets)
    end.

with_guild(GuildId, Options, Fun) ->
    memo_reset(),
    Data = member_data(),
    Tab = member_table(Data, Options),
    try
        ok = guild_permission_cache:put_normalized_data(GuildId, Data#{members_ets => Tab}),
        Fun(GuildId, Tab, Data)
    after
        cleanup(GuildId, Tab)
    end.

cleanup(GuildId, Tab) ->
    try
        ets:delete(Tab)
    catch
        error:badarg -> true
    end,
    ok = guild_permission_cache:delete(GuildId),
    memo_reset().

member_reads(GuildId) ->
    [
        {UserId, guild_permission_cache:get_permissions(GuildId, UserId, ?CHANNEL_ID),
            guild_permission_cache:has_member(GuildId, UserId),
            guild_permission_cache:get_member(GuildId, UserId)}
     || UserId <- [1, 2, 3]
    ].

member_data() ->
    guild_data_index:normalize_data(#{
        <<"guild">> => #{<<"owner_id">> => <<"1">>},
        <<"roles">> => [#{<<"id">> => <<"42">>, <<"permissions">> => <<"1024">>}],
        <<"members">> => [member(1, [<<"42">>]), member(2, [])],
        <<"channels">> => [
            #{<<"id">> => integer_to_binary(?CHANNEL_ID), <<"permission_overwrites">> => []}
        ]
    }).

member(UserId, Roles) ->
    #{
        <<"user">> => #{<<"id">> => integer_to_binary(UserId), <<"username">> => <<"u">>},
        <<"roles">> => Roles,
        <<"nick">> => <<"n">>
    }.

member_table(Data, Options) ->
    Tab = ets:new(guild_permission_cache_member_table_tests, Options),
    maps:foreach(
        fun(UserId, Member) -> true = ets:insert(Tab, {UserId, Member}) end,
        maps:get(<<"members">>, Data)
    ),
    Tab.

memo_reset() ->
    _ = erlang:erase(?MEMO),
    ok.
