%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_endpoint_guard_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

-define(ENDPOINT, <<"https://push.example.com/wpush/v2/abc">>).
-define(VERDICT_TABLE, push_endpoint_verdicts).
-define(MAX_VERDICTS, 2048).

resolves_to(Addresses) ->
    fun(_Host) -> {ok, Addresses} end.

fails_with(Reason) ->
    fun(_Host) -> {error, Reason} end.

never_resolves() ->
    fun(_Host) -> erlang:error(resolver_called) end.

link_local_metadata_address_is_refused_test() ->
    ?assertEqual(
        {error, endpoint_blocked},
        push_endpoint_guard:check(?ENDPOINT, resolves_to([{169, 254, 169, 254}]))
    ).

private_address_is_refused_test() ->
    ?assertEqual(
        {error, endpoint_blocked},
        push_endpoint_guard:check(?ENDPOINT, resolves_to([{10, 0, 0, 1}]))
    ).

loopback_address_is_refused_test() ->
    ?assertEqual(
        {error, endpoint_blocked},
        push_endpoint_guard:check(?ENDPOINT, resolves_to([{127, 0, 0, 1}]))
    ).

every_reserved_ipv4_range_is_refused_test() ->
    Blocked = [
        {0, 0, 0, 1},
        {10, 1, 2, 3},
        {100, 64, 0, 1},
        {127, 0, 0, 1},
        {169, 254, 169, 254},
        {172, 16, 0, 1},
        {172, 31, 255, 254},
        {192, 0, 0, 1},
        {192, 0, 2, 1},
        {192, 88, 99, 1},
        {192, 168, 1, 1},
        {198, 18, 0, 1},
        {198, 51, 100, 1},
        {203, 0, 113, 1},
        {224, 0, 0, 1},
        {240, 0, 0, 1},
        {255, 255, 255, 255}
    ],
    lists:foreach(
        fun(Address) ->
            ?assertEqual(
                {error, endpoint_blocked},
                push_endpoint_guard:check(?ENDPOINT, resolves_to([Address]))
            )
        end,
        Blocked
    ).

every_reserved_ipv6_range_is_refused_test() ->
    Blocked = [
        {0, 0, 0, 0, 0, 0, 0, 0},
        {0, 0, 0, 0, 0, 0, 0, 1},
        {16#2001, 16#0db8, 0, 0, 0, 0, 0, 1},
        {16#fd00, 0, 0, 0, 0, 0, 0, 1},
        {16#fe80, 0, 0, 0, 0, 0, 0, 1},
        {16#ff02, 0, 0, 0, 0, 0, 0, 1}
    ],
    lists:foreach(
        fun(Address) ->
            ?assertEqual(
                {error, endpoint_blocked},
                push_endpoint_guard:check(?ENDPOINT, resolves_to([Address]))
            )
        end,
        Blocked
    ).

ipv4_mapped_form_of_a_private_address_is_refused_test() ->
    Mapped = {0, 0, 0, 0, 0, 16#ffff, 16#0a00, 16#0001},
    ?assertEqual(
        {error, endpoint_blocked},
        push_endpoint_guard:check(?ENDPOINT, resolves_to([Mapped]))
    ).

ipv4_compatible_and_nat64_and_sixtofour_forms_are_refused_test() ->
    Compatible = {0, 0, 0, 0, 0, 0, 16#a9fe, 16#a9fe},
    Nat64 = {16#0064, 16#ff9b, 0, 0, 0, 0, 16#0a00, 16#0001},
    SixToFour = {16#2002, 16#0a00, 16#0001, 0, 0, 0, 0, 0},
    lists:foreach(
        fun(Address) ->
            ?assertEqual(
                {error, endpoint_blocked},
                push_endpoint_guard:check(?ENDPOINT, resolves_to([Address]))
            )
        end,
        [Compatible, Nat64, SixToFour]
    ).

one_private_address_refuses_the_whole_set_test() ->
    Mixed = [{93, 184, 216, 34}, {10, 0, 0, 1}],
    ?assertEqual(
        {error, endpoint_blocked},
        push_endpoint_guard:check(?ENDPOINT, resolves_to(Mixed))
    ),
    ?assertEqual(
        {error, endpoint_blocked},
        push_endpoint_guard:check(?ENDPOINT, resolves_to(lists:reverse(Mixed)))
    ).

one_private_ipv6_address_refuses_the_whole_set_test() ->
    Mixed = [
        {93, 184, 216, 34},
        {16#2606, 16#4700, 16#4700, 0, 0, 0, 0, 16#1111},
        {16#fd00, 0, 0, 0, 0, 0, 0, 1}
    ],
    ?assertEqual(
        {error, endpoint_blocked},
        push_endpoint_guard:check(?ENDPOINT, resolves_to(Mixed))
    ).

public_addresses_are_allowed_test() ->
    Public = [
        {93, 184, 216, 34},
        {16#2606, 16#4700, 16#4700, 0, 0, 0, 0, 16#1111},
        {0, 0, 0, 0, 0, 16#ffff, 16#5db8, 16#d822}
    ],
    ?assertEqual(ok, push_endpoint_guard:check(?ENDPOINT, resolves_to(Public))).

a_host_that_resolves_to_nothing_is_refused_test() ->
    ?assertEqual({error, nxdomain}, push_endpoint_guard:check(?ENDPOINT, resolves_to([]))).

an_unresolvable_host_reports_the_resolver_error_test() ->
    ?assertEqual({error, nxdomain}, push_endpoint_guard:check(?ENDPOINT, fails_with(nxdomain))),
    ?assertEqual({error, timeout}, push_endpoint_guard:check(?ENDPOINT, fails_with(timeout))).

an_unresolvable_host_fails_cleanly_against_the_real_resolver_test() ->
    ?assertMatch({error, _}, push_endpoint_guard:check(<<"https://push.invalid/sub">>)).

a_public_host_is_allowed_by_the_real_resolver_test() ->
    case inet:getaddrs("one.one.one.one", inet, 3000) of
        {ok, [_ | _]} ->
            ?assertEqual(ok, push_endpoint_guard:check(<<"https://one.one.one.one/sub">>));
        _ ->
            ok
    end.

plain_http_is_refused_test() ->
    ?assertEqual(
        {error, endpoint_rejected},
        push_endpoint_guard:check(
            <<"http://push.example.com/sub">>, resolves_to([{1, 1, 1, 1}])
        )
    ).

non_standard_ports_are_refused_test() ->
    ?assertEqual(
        {error, endpoint_rejected},
        push_endpoint_guard:check(
            <<"https://push.example.com:8080/sub">>, resolves_to([{1, 1, 1, 1}])
        )
    ),
    ?assertEqual(
        ok,
        push_endpoint_guard:check(
            <<"https://push.example.com:443/sub">>, resolves_to([{1, 1, 1, 1}])
        )
    ).

userinfo_is_refused_test() ->
    ?assertEqual(
        {error, endpoint_rejected},
        push_endpoint_guard:check(
            <<"https://push.example.com@169.254.169.254/sub">>, resolves_to([{1, 1, 1, 1}])
        )
    ),
    ?assertEqual(
        {error, endpoint_rejected},
        push_endpoint_guard:check(
            <<"https://user:secret@push.example.com/sub">>, resolves_to([{1, 1, 1, 1}])
        )
    ).

a_repeat_lookup_for_the_same_host_does_not_resolve_again_test() ->
    with_verdict_cache(fun() ->
        Counter = counters:new(1, []),
        Endpoint = endpoint("cache-repeat.example.com"),
        Resolver = counting_resolver(Counter, [{93, 184, 216, 34}]),
        ?assertEqual(ok, push_endpoint_guard:check(Endpoint, Resolver, cached)),
        ?assertEqual(ok, push_endpoint_guard:check(Endpoint, Resolver, cached)),
        ?assertEqual(ok, push_endpoint_guard:check(Endpoint, Resolver, cached)),
        ?assertEqual(1, counters:get(Counter, 1))
    end).

a_cache_hit_returns_the_verdict_the_uncached_path_returns_test() ->
    with_verdict_cache(fun() ->
        lists:foreach(
            fun assert_cached_matches_uncached/1,
            [
                {"cache-allow.example.com", resolves_to([{93, 184, 216, 34}]), ok},
                {"cache-block.example.com", resolves_to([{10, 0, 0, 1}]),
                    {error, endpoint_blocked}},
                {"cache-empty.example.com", resolves_to([]), {error, nxdomain}},
                {"cache-timeout.example.com", fails_with(timeout), {error, timeout}}
            ]
        )
    end).

assert_cached_matches_uncached({Host, Resolver, Expected}) ->
    Endpoint = endpoint(Host),
    ?assertEqual(Expected, push_endpoint_guard:check(Endpoint, Resolver)),
    ?assertEqual(Expected, push_endpoint_guard:check(Endpoint, Resolver, cached)),
    ?assertEqual(Expected, push_endpoint_guard:check(Endpoint, never_resolves(), cached)).

an_uncached_check_never_writes_the_cache_test() ->
    with_verdict_cache(fun() ->
        Endpoint = endpoint("cache-bypass.example.com"),
        ?assertEqual(ok, push_endpoint_guard:check(Endpoint, resolves_to([{1, 1, 1, 1}]))),
        ?assertEqual(0, push_ets_cache:table_size(?VERDICT_TABLE))
    end).

an_ip_literal_is_never_cached_test() ->
    with_verdict_cache(fun() ->
        ?assertEqual(
            ok, push_endpoint_guard:check(<<"https://93.184.216.34/sub">>, never_resolves())
        ),
        ?assertEqual(0, push_ets_cache:table_size(?VERDICT_TABLE))
    end).

a_cached_verdict_expires_test() ->
    with_verdict_cache(fun() ->
        Counter = counters:new(1, []),
        Endpoint = endpoint("cache-expiry.example.com"),
        Resolver = counting_resolver(Counter, [{93, 184, 216, 34}]),
        ?assertEqual(ok, push_endpoint_guard:check(Endpoint, Resolver, cached)),
        ?assertEqual(ok, push_endpoint_guard:check(Endpoint, Resolver, cached)),
        ?assertEqual(1, counters:get(Counter, 1)),
        expire_verdict(<<"cache-expiry.example.com">>),
        ?assertEqual(ok, push_endpoint_guard:check(Endpoint, Resolver, cached)),
        ?assertEqual(2, counters:get(Counter, 1))
    end).

a_refused_verdict_expires_sooner_than_an_allowed_one_test() ->
    with_verdict_cache(fun() ->
        Allowed = endpoint("cache-ttl-allowed.example.com"),
        Refused = endpoint("cache-ttl-refused.example.com"),
        ?assertEqual(
            ok, push_endpoint_guard:check(Allowed, resolves_to([{93, 184, 216, 34}]), cached)
        ),
        ?assertEqual(
            {error, timeout}, push_endpoint_guard:check(Refused, fails_with(timeout), cached)
        ),
        AllowedExpiry = expires_at(<<"cache-ttl-allowed.example.com">>),
        RefusedExpiry = expires_at(<<"cache-ttl-refused.example.com">>),
        ?assert(RefusedExpiry < AllowedExpiry)
    end).

many_distinct_hosts_cannot_grow_the_cache_without_bound_test() ->
    with_verdict_cache(fun() ->
        Resolver = resolves_to([{93, 184, 216, 34}]),
        lists:foreach(
            fun(N) -> flood_one_host(N, Resolver) end,
            lists:seq(1, 20000)
        ),
        Size = push_ets_cache:table_size(?VERDICT_TABLE),
        ?assert(Size >= 1500),
        ?assert(Size =< ?MAX_VERDICTS),
        ?assert(verdict_table_bytes() =< 4 * 1024 * 1024)
    end).

flood_one_host(N, Resolver) ->
    Endpoint = endpoint("flood-" ++ integer_to_list(N) ++ ".example.com"),
    ?assertEqual(ok, push_endpoint_guard:check(Endpoint, Resolver, cached)),
    ?assert(push_ets_cache:table_size(?VERDICT_TABLE) =< ?MAX_VERDICTS).

the_guard_is_enabled_by_default_test() ->
    ?assertEqual(
        {error, endpoint_blocked},
        push_endpoint_guard:check(?ENDPOINT, resolves_to([{10, 0, 0, 1}]))
    ).

a_disabled_guard_passes_everything_through_without_resolving_test() ->
    with_verdict_cache(fun() ->
        with_guard_disabled(fun() ->
            Never = never_resolves(),
            ?assertEqual(ok, push_endpoint_guard:check(?ENDPOINT, Never)),
            ?assertEqual(ok, push_endpoint_guard:check(?ENDPOINT, Never, cached)),
            ?assertEqual(
                ok, push_endpoint_guard:check(<<"https://169.254.169.254/latest">>, Never)
            ),
            ?assertEqual(
                ok, push_endpoint_guard:check(<<"http://push.example.com/sub">>, Never)
            ),
            ?assertEqual(ok, push_endpoint_guard:check(<<"not-a-url">>, Never)),
            ?assertEqual(0, push_ets_cache:table_size(?VERDICT_TABLE))
        end)
    end).

counting_resolver(Counter, Addresses) ->
    fun(_Host) ->
        counters:add(Counter, 1, 1),
        {ok, Addresses}
    end.

endpoint(Host) ->
    list_to_binary("https://" ++ Host ++ "/sub").

expires_at(Host) ->
    [{Host, _Verdict, ExpiresAt}] = ets:lookup(?VERDICT_TABLE, Host),
    ExpiresAt.

expire_verdict(Host) ->
    [{Host, Verdict, _ExpiresAt}] = ets:lookup(?VERDICT_TABLE, Host),
    true = ets:insert(?VERDICT_TABLE, {Host, Verdict, erlang:system_time(second) - 1}),
    ok.

verdict_table_bytes() ->
    ets:info(?VERDICT_TABLE, memory) * erlang:system_info(wordsize).

with_verdict_cache(Fun) ->
    ok = push_ets_cache:init(),
    true = ets:delete_all_objects(?VERDICT_TABLE),
    try
        Fun()
    after
        ets:delete_all_objects(?VERDICT_TABLE)
    end.

with_guard_disabled(Fun) ->
    Original = fluxer_gateway_env:get(push_endpoint_guard_enabled),
    _ = fluxer_gateway_env:patch(#{push_endpoint_guard_enabled => false}),
    try
        Fun()
    after
        _ = fluxer_gateway_env:patch(#{push_endpoint_guard_enabled => Original})
    end.

ip_literals_skip_dns_and_are_screened_directly_test() ->
    Never = never_resolves(),
    ?assertEqual(
        {error, endpoint_blocked},
        push_endpoint_guard:check(<<"https://169.254.169.254/latest">>, Never)
    ),
    ?assertEqual(
        {error, endpoint_blocked}, push_endpoint_guard:check(<<"https://127.0.0.1/sub">>, Never)
    ),
    ?assertEqual(
        {error, endpoint_blocked}, push_endpoint_guard:check(<<"https://[::1]/sub">>, Never)
    ),
    ?assertEqual(
        {error, endpoint_blocked},
        push_endpoint_guard:check(<<"https://[::ffff:10.0.0.1]/sub">>, Never)
    ),
    ?assertEqual(ok, push_endpoint_guard:check(<<"https://93.184.216.34/sub">>, Never)).

malformed_and_non_fqdn_hosts_are_refused_test() ->
    Never = never_resolves(),
    lists:foreach(
        fun(Endpoint) ->
            ?assertEqual(
                {error, endpoint_rejected}, push_endpoint_guard:check(Endpoint, Never)
            )
        end,
        [
            <<"not-a-url">>,
            <<>>,
            <<"https://localhost/sub">>,
            <<"https://metadata/sub">>,
            <<"https://push.example.123/sub">>,
            <<"https://-push.example.com/sub">>,
            <<"ftp://push.example.com/sub">>
        ]
    ).

uppercase_hosts_are_normalised_test() ->
    ?assertEqual(
        {error, endpoint_blocked},
        push_endpoint_guard:check(
            <<"https://PUSH.EXAMPLE.COM/sub">>, resolves_to([{10, 0, 0, 1}])
        )
    ).
