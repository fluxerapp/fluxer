%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(channel_threads_config_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

-define(KEY, channel_threads_config).
-define(PULLED_KEY, {channel_threads_config, pulled}).

config_fails_closed_when_nothing_is_stored_test() ->
    with_stored(undefined, fun() ->
        ?assertEqual(channel_threads_config:default_config(), channel_threads_config:config()),
        ?assertEqual(0, channel_threads_config:version()),
        ?assertNot(channel_threads_config:ever_enabled()),
        ?assertNot(channel_threads_config:guild_active(1)),
        ?assertNot(channel_threads_config:user_active(1)),
        ?assertNot(channel_threads_config:user_excluded(1))
    end).

validate_accepts_a_full_wire_config_test() ->
    {ok, Config} = channel_threads_config:validate_config(full_wire()),
    ?assertMatch(
        #{
            enabled := true,
            config_version := 7,
            ever_enabled := true,
            guild_basis_points := 250,
            guild_salt := <<"guild salt">>,
            user_basis_points := 10000,
            user_salt := <<"user-salt">>
        },
        Config
    ),
    ?assertEqual(#{<<"10">> => true, <<"11">> => true}, maps:get(enabled_guilds, Config)),
    ?assertEqual(#{<<"12">> => true}, maps:get(disabled_guilds, Config)),
    ?assertEqual(#{<<"20">> => true}, maps:get(included_users, Config)),
    ?assertEqual(#{<<"21">> => true}, maps:get(excluded_users, Config)).

validate_fills_defaults_and_ignores_unknown_keys_test() ->
    {ok, Config} = channel_threads_config:validate_config(#{
        <<"enabled">> => true, <<"future_field">> => 1
    }),
    Default = channel_threads_config:default_config(),
    ?assertEqual(Default#{enabled := true}, Config),
    ?assertEqual(<<"channel-threads-guild-v1">>, maps:get(guild_salt, Config)),
    ?assertEqual(<<"channel-threads-user-v1">>, maps:get(user_salt, Config)).

validate_rejects_every_malformed_field_test() ->
    Invalid = [
        {<<"enabled">>, <<"true">>},
        {<<"ever_enabled">>, 1},
        {<<"config_version">>, -1},
        {<<"config_version">>, 1.5},
        {<<"guild_basis_points">>, 10001},
        {<<"user_basis_points">>, -1},
        {<<"guild_salt">>, <<>>},
        {<<"user_salt">>, binary:copy(<<"a">>, 65)},
        {<<"guild_salt">>, <<"caf", 16#c3, 16#a9>>},
        {<<"user_salt">>, <<"tab\there">>},
        {<<"enabled_guild_ids">>, <<"1">>},
        {<<"disabled_guild_ids">>, [1]},
        {<<"included_user_ids">>, [<<"12a">>]},
        {<<"excluded_user_ids">>, [<<>>]},
        {<<"included_user_ids">>, [binary:copy(<<"1">>, 21)]},
        {<<"enabled_guild_ids">>, [integer_to_binary(N) || N <- lists:seq(1, 1001)]}
    ],
    lists:foreach(
        fun({Key, Value} = Case) ->
            ?assertMatch(
                {Case, {error, {invalid_field, Key, _}}},
                {Case, channel_threads_config:validate_config(#{Key => Value})}
            )
        end,
        Invalid
    ).

validate_accepts_the_boundaries_test() ->
    {ok, _} = channel_threads_config:validate_config(#{
        <<"guild_basis_points">> => 10000,
        <<"user_basis_points">> => 0,
        <<"guild_salt">> => binary:copy(<<"~">>, 64),
        <<"included_user_ids">> => [<<"18446744073709551615">>],
        <<"disabled_guild_ids">> => [integer_to_binary(N) || N <- lists:seq(1, 1000)]
    }).

guild_precedence_is_excluded_then_included_then_bucket_test() ->
    Config = config(#{
        <<"guild_basis_points">> => 10000,
        <<"enabled_guild_ids">> => [<<"5">>],
        <<"disabled_guild_ids">> => [<<"5">>, <<"6">>]
    }),
    ?assertNot(channel_threads_config:guild_active(Config, 5)),
    ?assertNot(channel_threads_config:guild_active(Config, 6)),
    ?assert(channel_threads_config:guild_active(Config, 7)),
    Included = config(#{<<"enabled_guild_ids">> => [<<"8">>]}),
    ?assert(channel_threads_config:guild_active(Included, 8)),
    ?assertNot(channel_threads_config:guild_active(Included, 9)).

guild_bucket_threshold_matches_the_shared_vectors_test() ->
    ?assertNot(
        channel_threads_config:guild_active(config(#{<<"guild_basis_points">> => 6741}), 1)
    ),
    ?assert(
        channel_threads_config:guild_active(config(#{<<"guild_basis_points">> => 6742}), 1)
    ),
    ?assertNot(
        channel_threads_config:guild_active(config(#{<<"guild_basis_points">> => 9122}), 0)
    ),
    ?assert(
        channel_threads_config:guild_active(config(#{<<"guild_basis_points">> => 9123}), 0)
    ).

user_bucket_threshold_matches_the_shared_vectors_test() ->
    ?assertNot(
        channel_threads_config:user_active(config(#{<<"user_basis_points">> => 3403}), 1)
    ),
    ?assert(channel_threads_config:user_active(config(#{<<"user_basis_points">> => 3404}), 1)).

user_precedence_and_exclusion_test() ->
    Config = config(#{
        <<"user_basis_points">> => 10000,
        <<"included_user_ids">> => [<<"1">>],
        <<"excluded_user_ids">> => [<<"1">>, <<"2">>]
    }),
    ?assertNot(channel_threads_config:user_active(Config, 1)),
    ?assertNot(channel_threads_config:user_active(Config, 2)),
    ?assert(channel_threads_config:user_active(Config, 3)),
    ?assert(channel_threads_config:user_excluded(Config, 2)),
    ?assertNot(channel_threads_config:user_excluded(Config, 3)).

disabled_config_is_inactive_for_everyone_test() ->
    {ok, Config} = channel_threads_config:validate_config(
        (full_wire())#{<<"enabled">> => false}
    ),
    ?assertNot(channel_threads_config:guild_active(Config, 10)),
    ?assertNot(channel_threads_config:user_active(Config, 20)),
    ?assert(channel_threads_config:user_excluded(Config, 21)).

zero_basis_points_never_buckets_in_test() ->
    Config = config(#{}),
    ?assertEqual([], [
        Id
     || Id <- lists:seq(1, 2000), channel_threads_config:guild_active(Config, Id)
    ]),
    ?assertEqual([], [
        Id
     || Id <- lists:seq(1, 2000), channel_threads_config:user_active(Config, Id)
    ]).

store_keeps_the_version_monotonic_test() ->
    with_stored(undefined, fun() ->
        V5 = config(#{<<"config_version">> => 5}),
        V4 = config(#{<<"config_version">> => 4, <<"enabled">> => false}),
        Default = channel_threads_config:default_config(),
        ?assertEqual(updated, channel_threads_config:store_validated_config(Default, V5, nats)),
        ?assertEqual(5, channel_threads_config:version()),
        ?assertEqual(stale, channel_threads_config:store_validated_config(V5, V4, api)),
        ?assertEqual(5, channel_threads_config:version()),
        ?assert(maps:get(enabled, channel_threads_config:config())),
        ?assertEqual(unchanged, channel_threads_config:store_validated_config(V5, V5, api))
    end).

store_keeps_ever_enabled_sticky_test() ->
    with_stored(undefined, fun() ->
        Enabled = config(#{<<"config_version">> => 1, <<"ever_enabled">> => true}),
        Killed = config(#{
            <<"config_version">> => 2, <<"enabled">> => false, <<"ever_enabled">> => false
        }),
        Default = channel_threads_config:default_config(),
        updated = channel_threads_config:store_validated_config(Default, Enabled, api),
        updated = channel_threads_config:store_validated_config(Enabled, Killed, api),
        ?assert(channel_threads_config:ever_enabled()),
        ?assertNot(maps:get(enabled, channel_threads_config:config())),
        ?assertEqual(
            unchanged,
            channel_threads_config:store_validated_config(
                channel_threads_config:config(), Killed, api
            )
        )
    end).

nats_payload_is_validated_before_it_is_stored_test() ->
    with_stored(undefined, fun() ->
        Payload = iolist_to_binary(
            json:encode(#{
                <<"type">> => <<"channel_threads_config">>,
                <<"config">> => #{<<"enabled">> => true, <<"config_version">> => 3}
            })
        ),
        ?assertEqual(updated, channel_threads_config:apply_nats_payload(Payload)),
        ?assertEqual(3, channel_threads_config:version()),
        Bad = iolist_to_binary(
            json:encode(#{
                <<"config">> => #{<<"enabled">> => <<"yes">>, <<"config_version">> => 9}
            })
        ),
        ?assertEqual(rejected, channel_threads_config:apply_nats_payload(Bad)),
        ?assertEqual(rejected, channel_threads_config:apply_nats_payload(<<"not json">>)),
        ?assertEqual(rejected, channel_threads_config:apply_nats_payload(<<"[1]">>)),
        ?assertEqual(3, channel_threads_config:version())
    end).

a_default_pull_counts_as_loaded_test() ->
    with_stored(undefined, fun() ->
        ?assertNot(channel_threads_config:loaded()),
        Default = iolist_to_binary(
            json:encode(#{<<"config">> => #{<<"enabled">> => false, <<"config_version">> => 0}})
        ),
        ?assertEqual(unchanged, channel_threads_config:apply_nats_payload(Default)),
        ?assertEqual(channel_threads_config:default_config(), channel_threads_config:config()),
        ?assert(channel_threads_config:loaded()),
        ?assertNot(channel_threads_config:enabled())
    end).

a_rejected_pull_is_not_loaded_test() ->
    with_stored(undefined, fun() ->
        ?assertEqual(rejected, channel_threads_config:apply_nats_payload(<<"not json">>)),
        ?assertNot(channel_threads_config:loaded())
    end).

fields_changed_are_split_by_dimension_test() ->
    Base = config(#{}),
    GuildOnly = config(#{<<"enabled_guild_ids">> => [<<"1">>]}),
    UserOnly = config(#{<<"user_basis_points">> => 5}),
    VersionOnly = config(#{<<"config_version">> => 9}),
    Disabled = config(#{<<"enabled">> => false}),
    ?assert(channel_threads_config:guild_fields_changed(Base, GuildOnly)),
    ?assertNot(channel_threads_config:user_fields_changed(Base, GuildOnly)),
    ?assert(channel_threads_config:user_fields_changed(Base, UserOnly)),
    ?assertNot(channel_threads_config:guild_fields_changed(Base, UserOnly)),
    ?assertNot(channel_threads_config:guild_fields_changed(Base, VersionOnly)),
    ?assertNot(channel_threads_config:user_fields_changed(Base, VersionOnly)),
    ?assert(channel_threads_config:guild_fields_changed(Base, Disabled)),
    ?assert(channel_threads_config:user_fields_changed(Base, Disabled)).

config(Overrides) ->
    {ok, Config} = channel_threads_config:validate_config(
        maps:merge(#{<<"enabled">> => true}, Overrides)
    ),
    Config.

full_wire() ->
    #{
        <<"enabled">> => true,
        <<"config_version">> => 7,
        <<"ever_enabled">> => true,
        <<"guild_basis_points">> => 250,
        <<"guild_salt">> => <<"guild salt">>,
        <<"enabled_guild_ids">> => [<<"10">>, <<"11">>],
        <<"disabled_guild_ids">> => [<<"12">>],
        <<"user_basis_points">> => 10000,
        <<"user_salt">> => <<"user-salt">>,
        <<"included_user_ids">> => [<<"20">>],
        <<"excluded_user_ids">> => [<<"21">>]
    }.

with_stored(Value, Fun) ->
    Previous = persistent_term:get(?KEY, undefined),
    PreviousPulled = persistent_term:get(?PULLED_KEY, undefined),
    restore(?KEY, Value),
    restore(?PULLED_KEY, undefined),
    try
        Fun()
    after
        restore(?KEY, Previous),
        restore(?PULLED_KEY, PreviousPulled)
    end.

restore(Key, undefined) ->
    _ = persistent_term:erase(Key),
    ok;
restore(Key, Value) ->
    persistent_term:put(Key, Value).
