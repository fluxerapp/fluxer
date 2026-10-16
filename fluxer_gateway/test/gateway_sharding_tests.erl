%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_sharding_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

parse_identify_shard_accepts_absent_and_null_test() ->
    ?assertEqual({ok, undefined}, gateway_sharding:parse_identify_shard(undefined)),
    ?assertEqual({ok, undefined}, gateway_sharding:parse_identify_shard(null)).

parse_identify_shard_accepts_valid_pair_test() ->
    ?assertEqual({ok, {3, 8}}, gateway_sharding:parse_identify_shard([3, 8])).

parse_identify_shard_rejects_invalid_shape_and_range_test() ->
    ?assertEqual({error, invalid_shard}, gateway_sharding:parse_identify_shard([1])),
    ?assertEqual({error, invalid_shard}, gateway_sharding:parse_identify_shard([1, 1])),
    ?assertEqual({error, invalid_shard}, gateway_sharding:parse_identify_shard([-1, 2])),
    ?assertEqual({error, invalid_shard}, gateway_sharding:parse_identify_shard([0, 0])),
    ?assertEqual({error, invalid_shard}, gateway_sharding:parse_identify_shard([0, 16385])).

retain_guild_ids_for_shard_uses_reference_snowflake_formula_test() ->
    Guild0 = guild_id_for_shard(0, 4, 0),
    Guild1 = guild_id_for_shard(1, 4, 0),
    Guild2 = guild_id_for_shard(2, 4, 0),
    Guild5 = guild_id_for_shard(1, 4, 1),
    ?assertEqual(
        [Guild1, Guild5],
        gateway_sharding:retain_guild_ids_for_shard(
            [Guild0, Guild1, Guild2, Guild5], {1, 4}
        )
    ).

validate_session_guild_count_uses_filtered_shard_count_test() ->
    GuildIds = [guild_id_for_shard(0, 1, I) || I <- lists:seq(0, 2500)],
    ?assertEqual(
        {error, sharding_required},
        gateway_sharding:validate_session_guild_count(GuildIds, undefined)
    ),
    ?assertEqual(ok, gateway_sharding:validate_session_guild_count(GuildIds, {1, 2})).

guild_id_for_shard(ShardId, NumShards, Offset) ->
    ((Offset * NumShards + ShardId) bsl 22) + 1.
