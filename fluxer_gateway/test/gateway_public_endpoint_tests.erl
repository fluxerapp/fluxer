%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_public_endpoint_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

-define(VECTORS_PATH, "../fluxer_common/src/testdata/public_endpoint_vectors.json").

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

normalize(Url, BaseDomain, Scheme, Port) ->
    gateway_public_endpoint:normalize(Url, BaseDomain, Scheme, Port).
