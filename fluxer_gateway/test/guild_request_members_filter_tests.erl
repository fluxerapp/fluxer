%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_request_members_filter_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

-define(REQUEST_MEMBERS_RATE_LIMIT_TABLE, guild_request_members_rate_limit).
-define(REQUEST_MEMBERS_RATE_LIMIT_MAX_EVENTS, 12).
-define(REQUEST_MEMBERS_GUILD_RATE_LIMIT_TABLE, guild_request_members_guild_rate_limit).
-define(REQUEST_MEMBERS_GUILD_RATE_LIMIT_MAX_EVENTS, 40).
-define(FULL_LIST_BOT_RATE_LIMIT_TABLE, guild_request_members_bot_full_list_rate_limit).
-define(FULL_LIST_BOT_RATE_LIMIT_WINDOW_MS, 30000).

is_full_member_list_true_test() ->
    ?assertEqual(true, guild_request_members_filter:is_full_member_list(<<>>, 0, [])).

is_full_member_list_query_set_test() ->
    ?assertEqual(false, guild_request_members_filter:is_full_member_list(<<"abc">>, 0, [])).

is_full_member_list_limit_set_test() ->
    ?assertEqual(false, guild_request_members_filter:is_full_member_list(<<>>, 10, [])).

is_full_member_list_user_ids_set_test() ->
    ?assertEqual(false, guild_request_members_filter:is_full_member_list(<<>>, 0, [1])).

check_permission_non_full_list_allows_user_test() ->
    ?assertEqual(
        ok,
        guild_request_members_filter:check_permission(1, false, 123, false, #{guilds => #{}})
    ).

check_permission_bot_full_list_allowed_test() ->
    ?assertEqual(
        ok, guild_request_members_filter:check_permission(1, true, 123, true, #{guilds => #{}})
    ).

check_permission_user_full_list_missing_guild_test() ->
    ?assertEqual(
        {error, guild_not_found},
        guild_request_members_filter:check_permission(1, false, 123, true, #{guilds => #{}})
    ).

check_full_list_bot_rate_limit_non_full_list_test() ->
    ?assertEqual(
        ok, guild_request_members_filter:check_full_list_bot_rate_limit(false, true, 1, 2)
    ).

check_full_list_bot_rate_limit_non_bot_test() ->
    ?assertEqual(
        ok, guild_request_members_filter:check_full_list_bot_rate_limit(true, false, 1, 2)
    ).

check_full_list_bot_rate_limit_allows_first_test() ->
    UserId = 111111001,
    GuildId = 111111002,
    clear_full_list_bot_rate_limit(UserId, GuildId),
    ?assertEqual(
        ok,
        guild_request_members_filter:check_full_list_bot_rate_limit(true, true, UserId, GuildId)
    ),
    clear_full_list_bot_rate_limit(UserId, GuildId).

check_full_list_bot_rate_limit_blocks_second_within_window_test() ->
    with_rate_limits_enabled(fun() ->
        UserId = 111111003,
        GuildId = 111111004,
        clear_full_list_bot_rate_limit(UserId, GuildId),
        ?assertEqual(
            ok,
            guild_request_members_filter:check_full_list_bot_rate_limit(
                true, true, UserId, GuildId
            )
        ),
        case
            guild_request_members_filter:check_full_list_bot_rate_limit(
                true, true, UserId, GuildId
            )
        of
            {rate_limited, RetryAfter} ->
                ?assert(RetryAfter > 0),
                ?assert(RetryAfter =< ?FULL_LIST_BOT_RATE_LIMIT_WINDOW_MS);
            Other ->
                ?assertEqual({rate_limited, expected}, Other)
        end,
        clear_full_list_bot_rate_limit(UserId, GuildId)
    end).

check_full_list_bot_rate_limit_disabled_by_env_test() ->
    OldValue = os:getenv("FLUXER_DISABLE_RATE_LIMITS"),
    os:putenv("FLUXER_DISABLE_RATE_LIMITS", "true"),
    try
        UserId = 111111011,
        GuildId = 111111012,
        clear_full_list_bot_rate_limit(UserId, GuildId),
        ?assertEqual(
            ok,
            guild_request_members_filter:check_full_list_bot_rate_limit(
                true, true, UserId, GuildId
            )
        ),
        ?assertEqual(
            ok,
            guild_request_members_filter:check_full_list_bot_rate_limit(
                true, true, UserId, GuildId
            )
        ),
        clear_full_list_bot_rate_limit(UserId, GuildId)
    after
        restore_env("FLUXER_DISABLE_RATE_LIMITS", OldValue)
    end.

check_full_list_bot_rate_limit_per_guild_isolation_test() ->
    UserId = 111111005,
    GuildA = 111111006,
    GuildB = 111111007,
    clear_full_list_bot_rate_limit(UserId, GuildA),
    clear_full_list_bot_rate_limit(UserId, GuildB),
    ?assertEqual(
        ok,
        guild_request_members_filter:check_full_list_bot_rate_limit(true, true, UserId, GuildA)
    ),
    ?assertEqual(
        ok,
        guild_request_members_filter:check_full_list_bot_rate_limit(true, true, UserId, GuildB)
    ),
    clear_full_list_bot_rate_limit(UserId, GuildA),
    clear_full_list_bot_rate_limit(UserId, GuildB).

check_full_list_bot_rate_limit_per_bot_isolation_test() ->
    UserA = 111111008,
    UserB = 111111009,
    GuildId = 111111010,
    clear_full_list_bot_rate_limit(UserA, GuildId),
    clear_full_list_bot_rate_limit(UserB, GuildId),
    ?assertEqual(
        ok,
        guild_request_members_filter:check_full_list_bot_rate_limit(true, true, UserA, GuildId)
    ),
    ?assertEqual(
        ok,
        guild_request_members_filter:check_full_list_bot_rate_limit(true, true, UserB, GuildId)
    ),
    clear_full_list_bot_rate_limit(UserA, GuildId),
    clear_full_list_bot_rate_limit(UserB, GuildId).

enforce_single_guild_for_bots_user_multi_test() ->
    ?assertEqual(
        ok, guild_request_members_filter:enforce_single_guild_for_bots(false, [1, 2, 3])
    ).

enforce_single_guild_for_bots_bot_single_test() ->
    ?assertEqual(ok, guild_request_members_filter:enforce_single_guild_for_bots(true, [1])).

enforce_single_guild_for_bots_bot_multi_rejected_test() ->
    ?assertEqual(
        {error, too_many_guild_ids},
        guild_request_members_filter:enforce_single_guild_for_bots(true, [1, 2])
    ).

enforce_single_guild_for_bots_bot_empty_test() ->
    ?assertEqual(ok, guild_request_members_filter:enforce_single_guild_for_bots(true, [])).

check_request_rate_limit_allows_initial_request_test() ->
    UserId = 987654321,
    clear_request_rate_limit(UserId),
    ?assertEqual(ok, guild_request_members_filter:check_request_rate_limit(UserId)),
    clear_request_rate_limit(UserId).

check_request_rate_limit_blocks_burst_test() ->
    UserId = 987654322,
    clear_request_rate_limit(UserId),
    ensure_ets_table(?REQUEST_MEMBERS_RATE_LIMIT_TABLE),
    Now = erlang:system_time(millisecond),
    Timestamps = lists:duplicate(?REQUEST_MEMBERS_RATE_LIMIT_MAX_EVENTS, Now - 1000),
    ets:insert(?REQUEST_MEMBERS_RATE_LIMIT_TABLE, {UserId, Timestamps}),
    ?assertEqual(
        {error, rate_limited}, guild_request_members_filter:check_request_rate_limit(UserId)
    ),
    clear_request_rate_limit(UserId).

check_request_rate_limit_invalid_user_test() ->
    ?assertEqual(
        {error, invalid_session},
        guild_request_members_filter:check_request_rate_limit(undefined)
    ).

check_guild_request_rate_limit_allows_initial_request_test() ->
    GuildId = 87654321,
    clear_guild_request_rate_limit(GuildId),
    ?assertEqual(ok, guild_request_members_filter:check_guild_request_rate_limit(GuildId)),
    clear_guild_request_rate_limit(GuildId).

check_guild_request_rate_limit_blocks_burst_test() ->
    GuildId = 87654322,
    clear_guild_request_rate_limit(GuildId),
    ensure_ets_table(?REQUEST_MEMBERS_GUILD_RATE_LIMIT_TABLE),
    Now = erlang:system_time(millisecond),
    Timestamps = lists:duplicate(?REQUEST_MEMBERS_GUILD_RATE_LIMIT_MAX_EVENTS, Now - 1000),
    ets:insert(?REQUEST_MEMBERS_GUILD_RATE_LIMIT_TABLE, {GuildId, Timestamps}),
    ?assertEqual(
        {error, rate_limited},
        guild_request_members_filter:check_guild_request_rate_limit(GuildId)
    ),
    clear_guild_request_rate_limit(GuildId).

check_guild_request_rate_limit_invalid_guild_test() ->
    ?assertEqual(
        {error, invalid_guild_id},
        guild_request_members_filter:check_guild_request_rate_limit(invalid_guild_id())
    ).

restore_env(Key, false) ->
    os:unsetenv(Key);
restore_env(Key, Value) ->
    os:putenv(Key, Value).

with_rate_limits_enabled(Fun) ->
    OldValue = os:getenv("FLUXER_DISABLE_RATE_LIMITS"),
    os:unsetenv("FLUXER_DISABLE_RATE_LIMITS"),
    try
        Fun()
    after
        restore_env("FLUXER_DISABLE_RATE_LIMITS", OldValue)
    end.

clear_full_list_bot_rate_limit(UserId, GuildId) ->
    ensure_ets_table(?FULL_LIST_BOT_RATE_LIMIT_TABLE),
    ets:delete(?FULL_LIST_BOT_RATE_LIMIT_TABLE, {UserId, GuildId}).

invalid_guild_id() ->
    eqwalizer:dynamic_cast(undefined).

clear_request_rate_limit(UserId) ->
    ensure_ets_table(?REQUEST_MEMBERS_RATE_LIMIT_TABLE),
    ets:delete(?REQUEST_MEMBERS_RATE_LIMIT_TABLE, UserId).

clear_guild_request_rate_limit(GuildId) ->
    ensure_ets_table(?REQUEST_MEMBERS_GUILD_RATE_LIMIT_TABLE),
    ets:delete(?REQUEST_MEMBERS_GUILD_RATE_LIMIT_TABLE, GuildId).

ensure_ets_table(Name) ->
    case ets:whereis(Name) of
        undefined ->
            try
                ets:new(Name, [
                    named_table,
                    public,
                    set,
                    {read_concurrency, true},
                    {write_concurrency, true}
                ]),
                ok
            catch
                error:badarg -> ok
            end;
        _ ->
            ok
    end.
