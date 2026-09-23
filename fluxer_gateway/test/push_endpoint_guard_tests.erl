%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_endpoint_guard_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

-define(ENDPOINT, <<"https://push.example.com/wpush/v2/abc">>).

resolves_to(Addresses) ->
    fun(_Host) -> {ok, Addresses} end.

fails_with(Reason) ->
    fun(_Host) -> {error, Reason} end.

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

ip_literals_skip_dns_and_are_screened_directly_test() ->
    Never = fun(_Host) -> erlang:error(resolver_called) end,
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
    Never = fun(_Host) -> erlang:error(resolver_called) end,
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
