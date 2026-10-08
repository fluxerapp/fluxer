%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rpc_guild_lifecycle_tests).

-include_lib("eunit/include/eunit.hrl").

with_guild_pid(Fun) ->
    meck:new(gateway_rpc_guild_infra, [passthrough, no_link]),
    meck:expect(gateway_rpc_guild_infra, with_guild_unchecked, fun(_GuildId, GuildFun) ->
        GuildFun(self())
    end),
    try
        Fun()
    after
        meck:unload(gateway_rpc_guild_infra)
    end.

dispatch_many_casts_every_event_in_order_test() ->
    with_guild_pid(fun() ->
        Params = #{
            <<"guild_id">> => <<"1">>,
            <<"events">> => [
                #{<<"event">> => <<"THREAD_UPDATE">>, <<"data">> => #{<<"id">> => <<"5">>}},
                #{
                    <<"event">> => <<"THREAD_MEMBER_UPDATE">>,
                    <<"data">> => #{<<"id">> => <<"5">>}
                },
                #{<<"event">> => <<"MESSAGE_CREATE">>, <<"data">> => #{<<"id">> => <<"6">>}}
            ]
        },
        ?assertEqual(
            true, gateway_rpc_guild_lifecycle:handle(<<"guild.dispatch_many">>, Params)
        ),
        receive
            {'$gen_cast', {dispatch_many, Requests}} ->
                ?assertEqual(
                    [
                        #{event => thread_update, data => #{<<"id">> => <<"5">>}},
                        #{event => thread_member_update, data => #{<<"id">> => <<"5">>}},
                        #{event => message_create, data => #{<<"id">> => <<"6">>}}
                    ],
                    Requests
                )
        after 1000 -> ?assert(false)
        end
    end).

dispatch_many_rejects_malformed_payloads_without_casting_test() ->
    with_guild_pid(fun() ->
        Malformed = [
            #{
                <<"guild_id">> => <<"1">>,
                <<"events">> => [#{<<"event">> => <<"THREAD_UPDATE">>}]
            },
            #{
                <<"guild_id">> => <<"1">>,
                <<"events">> => [
                    #{<<"event">> => <<"THREAD_UPDATE">>, <<"data">> => #{}},
                    #{<<"event">> => 7, <<"data">> => #{}}
                ]
            },
            #{<<"guild_id">> => <<"1">>, <<"events">> => #{}},
            #{<<"guild_id">> => <<"1">>}
        ],
        lists:foreach(
            fun(Params) ->
                ?assertError(
                    {gateway_rpc_error, validation_invalid_params},
                    gateway_rpc_guild_lifecycle:handle(<<"guild.dispatch_many">>, Params)
                )
            end,
            Malformed
        ),
        ?assertError(
            {validation, validation_invalid_snowflake},
            gateway_rpc_guild_lifecycle:handle(
                <<"guild.dispatch_many">>, #{<<"guild_id">> => <<"0">>, <<"events">> => []}
            )
        ),
        receive
            {'$gen_cast', {dispatch_many, _}} -> ?assert(false)
        after 50 -> ok
        end
    end).

dispatch_many_reaches_an_overloaded_guild_test() ->
    GuildId = 987654321,
    guild_manager_cache:ensure_guild_pid_cache(),
    ok = guild_ets_owner:ensure_table(guild_health_status, [named_table, public, set]),
    Pending = {make_ref(), erlang:monotonic_time(millisecond), 2500},
    true = ets:insert(guild_pid_cache, {GuildId, self()}),
    true = ets:insert(guild_health_status, {self(), GuildId, true, undefined, Pending}),
    try
        ?assert(guild_health:is_overloaded(self())),
        Params = #{
            <<"guild_id">> => integer_to_binary(GuildId),
            <<"events">> => [
                #{<<"event">> => <<"THREAD_CREATE">>, <<"data">> => #{<<"id">> => <<"5">>}},
                #{
                    <<"event">> => <<"THREAD_MEMBER_UPDATE">>,
                    <<"data">> => #{<<"id">> => <<"5">>}
                }
            ]
        },
        ?assertEqual(
            true, gateway_rpc_guild_lifecycle:handle(<<"guild.dispatch_many">>, Params)
        ),
        receive
            {'$gen_cast', {dispatch_many, Requests}} -> ?assertEqual(2, length(Requests))
        after 1000 -> ?assert(false)
        end
    after
        ets:delete(guild_health_status, self()),
        ets:delete(guild_pid_cache, GuildId)
    end.
