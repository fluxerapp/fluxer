%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_public_endpoint_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

-define(VECTORS_PATH, "../fluxer_common/src/testdata/public_endpoint_vectors.json").

normalize_default_https_install_test() ->
    ?assertEqual(
        <<"https://fluxer.example/media">>,
        normalize(<<"https://fluxer.example/media">>, <<"fluxer.example">>, <<"https">>, 443)
    ),
    ?assertEqual(
        <<"https://fluxer.example">>,
        normalize(<<"https://fluxer.example">>, <<"fluxer.example">>, <<"https">>, 443)
    ),
    ?assertEqual(
        <<"wss://fluxer.example/gateway">>,
        normalize(<<"wss://fluxer.example/gateway">>, <<"fluxer.example">>, <<"https">>, 443)
    ).

normalize_default_http_install_test() ->
    ?assertEqual(
        <<"http://fluxer.example/media">>,
        normalize(<<"http://fluxer.example/media">>, <<"fluxer.example">>, <<"http">>, 80)
    ),
    ?assertEqual(
        <<"ws://fluxer.example/gateway">>,
        normalize(<<"ws://fluxer.example/gateway">>, <<"fluxer.example">>, <<"http">>, 80)
    ).

normalize_inserts_non_default_port_test() ->
    ?assertEqual(
        <<"https://fluxer.example:8443/media">>,
        normalize(<<"https://fluxer.example/media">>, <<"fluxer.example">>, <<"https">>, 8443)
    ),
    ?assertEqual(
        <<"https://fluxer.example:8443">>,
        normalize(<<"https://fluxer.example">>, <<"fluxer.example">>, <<"https">>, 8443)
    ),
    ?assertEqual(
        <<"http://fluxer.example:8080/media">>,
        normalize(<<"http://fluxer.example/media">>, <<"fluxer.example">>, <<"http">>, 8080)
    ),
    ?assertEqual(
        <<"http://fluxer.example:443/media">>,
        normalize(<<"http://fluxer.example/media">>, <<"fluxer.example">>, <<"http">>, 443)
    ).

normalize_preserves_url_parts_test() ->
    ?assertEqual(
        <<"https://fluxer.example:8443/">>,
        normalize(<<"https://fluxer.example/">>, <<"fluxer.example">>, <<"https">>, 8443)
    ),
    ?assertEqual(
        <<"https://fluxer.example:8443/Media/Path?q=A%20b#Frag">>,
        normalize(
            <<"https://fluxer.example/Media/Path?q=A%20b#Frag">>,
            <<"fluxer.example">>,
            <<"https">>,
            8443
        )
    ),
    ?assertEqual(
        <<"https://user@fluxer.example:8443/media">>,
        normalize(
            <<"https://user@fluxer.example/media">>, <<"fluxer.example">>, <<"https">>, 8443
        )
    ).

normalize_host_matching_test() ->
    ?assertEqual(
        <<"https://Fluxer.EXAMPLE.:8443/media">>,
        normalize(<<"https://Fluxer.EXAMPLE./media">>, <<"fluxer.example">>, <<"https">>, 8443)
    ),
    ?assertEqual(
        <<"https://cdn.othercdn.net/assets">>,
        normalize(
            <<"https://cdn.othercdn.net/assets">>, <<"fluxer.example">>, <<"https">>, 8443
        )
    ),
    ?assertEqual(
        <<"https://media.fluxer.example/media">>,
        normalize(
            <<"https://media.fluxer.example/media">>, <<"fluxer.example">>, <<"https">>, 8443
        )
    ).

normalize_keeps_explicit_port_test() ->
    ?assertEqual(
        <<"https://fluxer.example:9443/media">>,
        normalize(
            <<"https://fluxer.example:9443/media">>, <<"fluxer.example">>, <<"https">>, 8443
        )
    ),
    ?assertEqual(
        <<"https://fluxer.example:8443/media">>,
        normalize(
            <<"https://fluxer.example:8443/media">>, <<"fluxer.example">>, <<"https">>, 8443
        )
    ),
    ?assertEqual(
        <<"https://fluxer.example:/media">>,
        normalize(<<"https://fluxer.example:/media">>, <<"fluxer.example">>, <<"https">>, 8443)
    ).

normalize_missing_inputs_test() ->
    ?assertEqual(
        <<"https://fluxer.example/media">>,
        normalize(
            <<"https://fluxer.example/media">>, <<"fluxer.example">>, <<"https">>, undefined
        )
    ),
    ?assertEqual(
        <<"https://fluxer.example/media">>,
        normalize(<<"https://fluxer.example/media">>, undefined, <<"https">>, 8443)
    ),
    ?assertEqual(
        <<"https://fluxer.example/media">>,
        normalize(<<"https://fluxer.example/media">>, <<>>, <<"https">>, 8443)
    ).

normalize_unparsable_url_test() ->
    ?assertEqual(
        <<"not a url">>,
        normalize(<<"not a url">>, <<"fluxer.example">>, <<"https">>, 8443)
    ),
    ?assertEqual(<<>>, normalize(<<>>, <<"fluxer.example">>, <<"https">>, 8443)),
    ?assertEqual(
        <<"//fluxer.example/media">>,
        normalize(<<"//fluxer.example/media">>, <<"fluxer.example">>, <<"https">>, 8443)
    ).

normalize_ipv6_host_test() ->
    ?assertEqual(
        <<"http://[::1]:19080/media">>,
        normalize(<<"http://[::1]/media">>, <<"[::1]">>, <<"http">>, 19080)
    ),
    ?assertEqual(
        <<"http://[2001:DB8::1]:19080/media">>,
        normalize(<<"http://[2001:DB8::1]/media">>, <<"[2001:db8::1]">>, <<"http">>, 19080)
    ),
    ?assertEqual(
        <<"http://[::1]:8080/media">>,
        normalize(<<"http://[::1]:8080/media">>, <<"[::1]">>, <<"http">>, 19080)
    ).

normalize_userinfo_at_sign_test() ->
    ?assertEqual(
        <<"http://user:p@ss@fluxer.example:19080/media">>,
        normalize(
            <<"http://user:p@ss@fluxer.example/media">>, <<"fluxer.example">>, <<"http">>, 19080
        )
    ),
    ?assertEqual(
        <<"http://user:p@ss@fluxer.example:19080/media">>,
        normalize(
            <<"http://user:p@ss@fluxer.example:19080/media">>,
            <<"fluxer.example">>,
            <<"http">>,
            19080
        )
    ).

normalize_backslash_authority_test() ->
    ?assertEqual(
        <<"http://fluxer.example:19080\\evil">>,
        normalize(<<"http://fluxer.example\\evil">>, <<"fluxer.example">>, <<"http">>, 19080)
    ).

normalize_zero_port_test() ->
    ?assertEqual(
        <<"http://fluxer.example/media">>,
        normalize(<<"http://fluxer.example/media">>, <<"fluxer.example">>, <<"http">>, 0)
    ).

normalize_single_root_dot_test() ->
    ?assertEqual(
        <<"http://fluxer.example.:19080/media">>,
        normalize(<<"http://fluxer.example./media">>, <<"fluxer.example.">>, <<"http">>, 19080)
    ),
    ?assertEqual(
        <<"http://fluxer.example../media">>,
        normalize(<<"http://fluxer.example../media">>, <<"fluxer.example">>, <<"http">>, 19080)
    ),
    ?assertEqual(
        <<"http://fluxer.example/media">>,
        normalize(<<"http://fluxer.example/media">>, <<"fluxer.example..">>, <<"http">>, 19080)
    ).

normalize_unsupported_scheme_test() ->
    ?assertEqual(
        <<"file:///media">>,
        normalize(<<"file:///media">>, <<"fluxer.example">>, <<"http">>, 19080)
    ),
    ?assertEqual(
        <<"mailto:a@fluxer.example">>,
        normalize(<<"mailto:a@fluxer.example">>, <<"fluxer.example">>, <<"http">>, 19080)
    ).

normalize_matches_shared_vectors_test() ->
    Vectors = read_vectors(),
    ?assertMatch([_ | _], Vectors),
    lists:foreach(fun run_vector/1, Vectors).

read_vectors() ->
    case file:read_file(?VECTORS_PATH) of
        {ok, Contents} ->
            decode_vectors(Contents);
        {error, Reason} ->
            erlang:error({public_endpoint_vectors_unreadable, ?VECTORS_PATH, Reason})
    end.

decode_vectors(Contents) ->
    case json:decode(Contents) of
        [_ | _] = Vectors -> Vectors;
        _ -> erlang:error({public_endpoint_vectors_empty, ?VECTORS_PATH})
    end.

run_vector(
    #{<<"url">> := Url, <<"base_domain">> := BaseDomain, <<"normalized">> := Expected} = Vector
) when is_binary(Url), is_binary(BaseDomain), is_binary(Expected) ->
    Port = vector_port(Vector),
    ?assertEqual(
        {Url, BaseDomain, Port, Expected},
        {Url, BaseDomain, Port, normalize(Url, BaseDomain, undefined, Port)}
    );
run_vector(Vector) ->
    erlang:error({public_endpoint_vector_malformed, Vector}).

vector_port(#{<<"public_port">> := null}) ->
    undefined;
vector_port(#{<<"public_port">> := Port}) when is_integer(Port) ->
    Port;
vector_port(Vector) ->
    erlang:error({public_endpoint_vector_malformed, Vector}).

normalize_is_idempotent_test() ->
    Once = normalize(
        <<"https://fluxer.example/media">>, <<"fluxer.example">>, <<"https">>, 8443
    ),
    Twice = normalize(Once, <<"fluxer.example">>, <<"https">>, 8443),
    ?assertEqual(<<"https://fluxer.example:8443/media">>, Once),
    ?assertEqual(Once, Twice).

normalize(Url, BaseDomain, Scheme, Port) ->
    gateway_public_endpoint:normalize(Url, BaseDomain, Scheme, Port).
