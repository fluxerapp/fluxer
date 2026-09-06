%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_manager_shard_start_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

build_test_state() ->
    #{
        sessions => #{},
        identify_attempts => [],
        pending_identifies => #{},
        identify_workers => #{},
        shard_index => 0
    }.

guild_wire(GuildId) ->
    #{<<"id">> => integer_to_binary(GuildId)}.

guild_id_for_shard(ShardId, NumShards, Offset) ->
    ((Offset * NumShards + ShardId) bsl 22) + 1.

bot_identify_data(GuildCount) ->
    Guilds = [guild_wire(guild_id_for_shard(0, 1, I)) || I <- lists:seq(1, GuildCount)],
    #{
        <<"user">> => #{
            <<"id">> => <<"123">>,
            <<"username">> => <<"bot">>,
            <<"discriminator">> => <<"0001">>,
            <<"avatar">> => null,
            <<"flags">> => 0,
            <<"bot">> => true
        },
        <<"guilds">> => Guilds
    }.

start_session(Data, IdentifyData) ->
    session_manager_shard_start:build_and_start_session(
        Data, IdentifyData, 1, self(), <<"session-id">>, #{}, build_test_state()
    ).

build_and_start_session_rejects_oversized_bot_with_sharding_required_test() ->
    Data = bot_identify_data(2501),
    ?assertMatch(
        {reply, {error, sharding_required}, _},
        start_session(Data, #{properties => #{}, token => <<"token">>, presence => null})
    ).
